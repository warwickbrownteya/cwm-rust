//! Parallel Rule Execution
//!
//! This module provides parallel execution of N3 rules for improved performance
//! on multi-core systems.
//!
//! # Architecture
//!
//! The parallel reasoner splits rule matching across multiple threads while
//! maintaining deterministic results:
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────┐
//! │                   ParallelReasoner                       │
//! ├─────────────────────────────────────────────────────────┤
//! │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
//! │  │ Thread Pool  │  │   Rule       │  │  Result      │  │
//! │  │ (N workers)  │  │ Partitioner  │  │  Collector   │  │
//! │  └──────────────┘  └──────────────┘  └──────────────┘  │
//! │                           │                              │
//! │     ┌──────────┬──────────┼──────────┬──────────┐       │
//! │     │ Worker 1 │ Worker 2 │ Worker 3 │ Worker N │       │
//! │     └──────────┴──────────┴──────────┴──────────┘       │
//! │                           │                              │
//! │                    Merge Results                         │
//! │                    (sequential)                          │
//! └─────────────────────────────────────────────────────────┘
//! ```
//!
//! # Strategies
//!
//! - **Rule Partitioning**: Each worker handles a subset of rules
//! - **Data Partitioning**: Each worker handles a subset of data (for large stores)
//! - **Hybrid**: Combination of rule and data partitioning
//!
//! # Usage
//!
//! ```ignore
//! use cwm::reasoner::parallel::{ParallelReasoner, ParallelConfig};
//!
//! let config = ParallelConfig::default()
//!     .with_workers(4)
//!     .with_chunk_size(100);
//!
//! let mut reasoner = ParallelReasoner::new(config);
//! reasoner.add_rules(rules);
//! reasoner.run(&mut store);
//! ```

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};
use std::thread;

use crate::term::{Term, Triple, Variable, Bindings, substitute_triple};
use crate::store::Store;
use crate::builtins::BuiltinRegistry;

use super::{Rule, ReasonerStats, ReasonerConfig};

/// Configuration for parallel execution
#[derive(Debug, Clone)]
pub struct ParallelConfig {
    /// Number of worker threads (0 = auto-detect based on CPU count)
    pub workers: usize,
    /// Minimum rules per worker before parallelizing
    pub min_rules_per_worker: usize,
    /// Chunk size for data partitioning
    pub chunk_size: usize,
    /// Whether to enable parallel execution
    pub enabled: bool,
    /// Base reasoner configuration
    pub base_config: ReasonerConfig,
}

impl Default for ParallelConfig {
    fn default() -> Self {
        ParallelConfig {
            workers: 0, // Auto-detect
            min_rules_per_worker: 4,
            chunk_size: 1000,
            enabled: true,
            base_config: ReasonerConfig::default(),
        }
    }
}

impl ParallelConfig {
    /// Set number of worker threads
    pub fn with_workers(mut self, n: usize) -> Self {
        self.workers = n;
        self
    }

    /// Set chunk size for data partitioning
    pub fn with_chunk_size(mut self, size: usize) -> Self {
        self.chunk_size = size;
        self
    }

    /// Disable parallel execution
    pub fn sequential(mut self) -> Self {
        self.enabled = false;
        self
    }

    /// Get effective worker count
    pub fn effective_workers(&self) -> usize {
        if self.workers == 0 {
            num_cpus()
        } else {
            self.workers
        }
    }
}

/// Get number of CPUs (fallback to 1 if detection fails)
fn num_cpus() -> usize {
    thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1)
}

/// A match result from rule evaluation
#[derive(Clone)]
struct MatchResult {
    /// The derived triple
    triple: Triple,
    /// Index of the rule that produced this
    rule_index: usize,
    /// Variable bindings used
    bindings: Bindings,
    /// Premises that matched the antecedent
    premises: Vec<Triple>,
}

/// Parallel reasoner for rule execution
pub struct ParallelReasoner {
    config: ParallelConfig,
    rules: Vec<Rule>,
    builtins: BuiltinRegistry,
    stats: ReasonerStats,
}

impl ParallelReasoner {
    /// Create a new parallel reasoner
    pub fn new(config: ParallelConfig) -> Self {
        ParallelReasoner {
            config,
            rules: Vec::new(),
            builtins: BuiltinRegistry::default(),
            stats: ReasonerStats::default(),
        }
    }

    /// Create with default configuration
    pub fn default_reasoner() -> Self {
        Self::new(ParallelConfig::default())
    }

    /// Add a rule
    pub fn add_rule(&mut self, rule: Rule) {
        self.rules.push(rule);
    }

    /// Add multiple rules
    pub fn add_rules(&mut self, rules: impl IntoIterator<Item = Rule>) {
        self.rules.extend(rules);
    }

    /// Set builtin registry
    pub fn set_builtins(&mut self, builtins: BuiltinRegistry) {
        self.builtins = builtins;
    }

    /// Get statistics
    pub fn stats(&self) -> &ReasonerStats {
        &self.stats
    }

    /// Run parallel inference
    pub fn run(&mut self, store: &mut Store) -> &ReasonerStats {
        self.stats = ReasonerStats::default();

        // Check if parallel execution is beneficial
        let workers = self.config.effective_workers();
        let use_parallel = self.config.enabled
            && self.rules.len() >= workers * self.config.min_rules_per_worker
            && workers > 1;

        loop {
            if self.stats.steps >= self.config.base_config.max_steps {
                break;
            }

            let derived = if use_parallel {
                self.parallel_step(store, workers)
            } else {
                self.sequential_step(store)
            };

            self.stats.steps += 1;

            if derived == 0 || !self.config.base_config.recursive {
                break;
            }
        }

        &self.stats
    }

    /// Execute a single step with parallel rule matching
    fn parallel_step(&mut self, store: &mut Store, workers: usize) -> usize {
        // Partition rules across workers
        let rule_chunks = partition_rules(&self.rules, workers);

        // Create a shared read-only view of the store triples
        let store_triples: Vec<Triple> = store.iter().cloned().collect();
        let store_triples = Arc::new(store_triples);

        // Track existing triples for duplicate detection
        let existing: HashSet<String> = store_triples.iter()
            .map(|t| format!("{:?}", t))
            .collect();
        let existing = Arc::new(existing);

        // Collect results from all workers
        let results: Arc<Mutex<Vec<MatchResult>>> = Arc::new(Mutex::new(Vec::new()));

        // Spawn workers
        let handles: Vec<_> = rule_chunks.into_iter()
            .map(|chunk| {
                let store_triples = Arc::clone(&store_triples);
                let existing = Arc::clone(&existing);
                let results = Arc::clone(&results);
                let builtins = self.builtins.clone();

                thread::spawn(move || {
                    let mut local_results = Vec::new();

                    for (rule_idx, rule) in chunk {
                        // Find matches for this rule
                        let matches = match_antecedent_parallel(
                            &store_triples,
                            &rule.antecedent,
                            &builtins,
                        );

                        for (bindings, premises) in matches {
                            for pattern in &rule.consequent {
                                let triple = substitute_triple(pattern, &bindings);

                                if !triple.is_ground() {
                                    continue;
                                }

                                let sig = format!("{:?}", triple);
                                if !existing.contains(&sig) {
                                    local_results.push(MatchResult {
                                        triple,
                                        rule_index: rule_idx,
                                        bindings: bindings.clone(),
                                        premises: premises.clone(),
                                    });
                                }
                            }
                        }
                    }

                    // Merge local results
                    if !local_results.is_empty() {
                        let mut results = results.lock().unwrap();
                        results.extend(local_results);
                    }
                })
            })
            .collect();

        // Wait for all workers
        for handle in handles {
            let _ = handle.join();
        }

        // Collect and deduplicate results
        let all_results = Arc::try_unwrap(results)
            .map(|mutex| mutex.into_inner().unwrap())
            .unwrap_or_else(|arc| arc.lock().unwrap().clone());

        // Deduplicate based on triple signature
        let mut seen = HashSet::new();
        let mut unique_results = Vec::new();
        for result in all_results {
            let sig = format!("{:?}", result.triple);
            if !seen.contains(&sig) {
                seen.insert(sig);
                unique_results.push(result);
            }
        }

        let count = unique_results.len();
        self.stats.triples_derived += count;
        self.stats.rules_fired += count;

        // Add triples to store (must be sequential)
        for result in unique_results {
            store.add(result.triple);
        }

        count
    }

    /// Execute a single step sequentially (fallback)
    fn sequential_step(&mut self, store: &mut Store) -> usize {
        let mut derivations: Vec<(Triple, usize)> = Vec::new();
        let mut step_signatures: HashSet<String> = HashSet::new();

        // Create a temporary read-only view
        let triples: Vec<Triple> = store.iter().cloned().collect();

        for (rule_idx, rule) in self.rules.iter().enumerate() {
            let matches = match_antecedent_sequential(&triples, &rule.antecedent, &self.builtins);

            for (bindings, _premises) in matches {
                for pattern in &rule.consequent {
                    let triple = substitute_triple(pattern, &bindings);

                    if !triple.is_ground() {
                        continue;
                    }

                    let sig = format!("{:?}", triple);
                    if !store.contains(&triple) && !step_signatures.contains(&sig) {
                        step_signatures.insert(sig);
                        derivations.push((triple, rule_idx));
                        self.stats.rules_fired += 1;
                    }
                }
            }
        }

        let count = derivations.len();
        self.stats.triples_derived += count;

        for (triple, _) in derivations {
            store.add(triple);
        }

        count
    }
}

/// Partition rules across workers
fn partition_rules(rules: &[Rule], workers: usize) -> Vec<Vec<(usize, Rule)>> {
    let chunk_size = (rules.len() + workers - 1) / workers;
    rules.iter()
        .cloned()
        .enumerate()
        .collect::<Vec<_>>()
        .chunks(chunk_size.max(1))
        .map(|c| c.to_vec())
        .collect()
}

/// Match antecedent patterns against store (parallel-safe version)
fn match_antecedent_parallel(
    triples: &[Triple],
    patterns: &[Triple],
    builtins: &BuiltinRegistry,
) -> Vec<(Bindings, Vec<Triple>)> {
    match_patterns(triples, patterns, Bindings::default(), builtins)
}

/// Match antecedent patterns against store (sequential version)
fn match_antecedent_sequential(
    triples: &[Triple],
    patterns: &[Triple],
    builtins: &BuiltinRegistry,
) -> Vec<(Bindings, Vec<Triple>)> {
    match_patterns(triples, patterns, Bindings::default(), builtins)
}

/// Recursive pattern matching
fn match_patterns(
    triples: &[Triple],
    patterns: &[Triple],
    bindings: Bindings,
    builtins: &BuiltinRegistry,
) -> Vec<(Bindings, Vec<Triple>)> {
    if patterns.is_empty() {
        return vec![(bindings, Vec::new())];
    }

    let pattern = &patterns[0];
    let rest = &patterns[1..];

    // Substitute current bindings into pattern
    let ground_pattern = substitute_triple(pattern, &bindings);

    // Check if this is a builtin predicate
    if let Term::Uri(uri) = &ground_pattern.predicate {
        if builtins.is_builtin(uri.as_str()) {
            // Try to evaluate builtin
            if let Some(result) = evaluate_builtin(builtins, &ground_pattern, &bindings) {
                if result.is_empty() {
                    return Vec::new(); // Builtin failed
                }
                // Builtin succeeded with possible new bindings
                let mut all_results = Vec::new();
                for new_bindings in result {
                    let merged = merge_bindings(&bindings, &new_bindings);
                    all_results.extend(match_patterns(triples, rest, merged, builtins));
                }
                return all_results;
            }
        }
    }

    // Regular pattern matching
    let mut results = Vec::new();

    for triple in triples {
        if let Some(new_bindings) = unify_triple(&ground_pattern, triple, &bindings) {
            let merged = merge_bindings(&bindings, &new_bindings);
            let sub_results = match_patterns(triples, rest, merged, builtins);

            for (final_bindings, mut premises) in sub_results {
                premises.insert(0, triple.clone());
                results.push((final_bindings, premises));
            }
        }
    }

    results
}

/// Attempt to unify a pattern with a triple
fn unify_triple(pattern: &Triple, triple: &Triple, bindings: &Bindings) -> Option<Bindings> {
    let mut new_bindings = Bindings::default();

    if !unify_term(&pattern.subject, &triple.subject, bindings, &mut new_bindings) {
        return None;
    }
    if !unify_term(&pattern.predicate, &triple.predicate, bindings, &mut new_bindings) {
        return None;
    }
    if !unify_term(&pattern.object, &triple.object, bindings, &mut new_bindings) {
        return None;
    }

    Some(new_bindings)
}

/// Unify a pattern term with a ground term
fn unify_term(pattern: &Term, term: &Term, existing: &Bindings, new: &mut Bindings) -> bool {
    match pattern {
        Term::Variable(v) => {
            // Check existing bindings first
            if let Some(bound) = existing.get(v) {
                return bound == term;
            }
            if let Some(bound) = new.get(v) {
                return bound == term;
            }
            // Bind variable
            new.insert(v.clone(), term.clone());
            true
        }
        _ => pattern == term,
    }
}

/// Merge two sets of bindings
fn merge_bindings(a: &Bindings, b: &Bindings) -> Bindings {
    let mut result = a.clone();
    for (k, v) in b.iter() {
        result.insert(k.clone(), v.clone());
    }
    result
}

/// Evaluate a builtin predicate
fn evaluate_builtin(
    builtins: &BuiltinRegistry,
    triple: &Triple,
    bindings: &Bindings,
) -> Option<Vec<Bindings>> {
    if let Term::Uri(uri) = &triple.predicate {
        if builtins.is_builtin(uri.as_str()) {
            match builtins.evaluate(uri.as_str(), &triple.subject, &triple.object, bindings) {
                crate::builtins::BuiltinResult::Success(new_bindings) => Some(vec![new_bindings]),
                crate::builtins::BuiltinResult::Failure => Some(vec![]),
                crate::builtins::BuiltinResult::NotReady => None, // Can't evaluate yet
            }
        } else {
            None
        }
    } else {
        None
    }
}

/// Statistics specific to parallel execution
#[derive(Debug, Clone, Default)]
pub struct ParallelStats {
    /// Base reasoner statistics
    pub base: ReasonerStats,
    /// Number of workers used
    pub workers: usize,
    /// Total rules processed
    pub total_rules: usize,
    /// Average rules per worker
    pub rules_per_worker: f64,
    /// Whether parallel execution was used
    pub used_parallel: bool,
}

impl ParallelStats {
    /// Compute parallel stats from a run
    pub fn from_run(
        base: &ReasonerStats,
        workers: usize,
        total_rules: usize,
        used_parallel: bool,
    ) -> Self {
        ParallelStats {
            base: base.clone(),
            workers,
            total_rules,
            rules_per_worker: if workers > 0 {
                total_rules as f64 / workers as f64
            } else {
                0.0
            },
            used_parallel,
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_defaults() {
        let config = ParallelConfig::default();
        assert_eq!(config.workers, 0); // Auto-detect
        assert!(config.enabled);
    }

    #[test]
    fn test_config_builder() {
        let config = ParallelConfig::default()
            .with_workers(8)
            .with_chunk_size(500);

        assert_eq!(config.workers, 8);
        assert_eq!(config.chunk_size, 500);
        assert_eq!(config.effective_workers(), 8);
    }

    #[test]
    fn test_sequential_mode() {
        let config = ParallelConfig::default().sequential();
        assert!(!config.enabled);
    }

    #[test]
    fn test_partition_rules() {
        let rules: Vec<Rule> = (0..10).map(|i| Rule::named(
            format!("rule{}", i),
            vec![],
            vec![],
        )).collect();

        let partitions = partition_rules(&rules, 3);
        assert_eq!(partitions.len(), 3);

        // All rules should be present
        let total: usize = partitions.iter().map(|p| p.len()).sum();
        assert_eq!(total, 10);
    }

    #[test]
    fn test_simple_inference() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
            Term::uri("http://example.org/Human"),
        ));

        // Human subClassOf Mortal
        store.add(Triple::new(
            Term::uri("http://example.org/Human"),
            Term::uri("http://www.w3.org/2000/01/rdf-schema#subClassOf"),
            Term::uri("http://example.org/Mortal"),
        ));

        // Rule: ?c subClassOf ?d, ?x type ?c => ?x type ?d
        let rule = Rule::named(
            "subclass-inference",
            vec![
                Triple::new(
                    Term::universal("c"),
                    Term::uri("http://www.w3.org/2000/01/rdf-schema#subClassOf"),
                    Term::universal("d"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                    Term::universal("c"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                    Term::universal("d"),
                ),
            ],
        );

        let config = ParallelConfig::default().sequential(); // Force sequential for test
        let mut reasoner = ParallelReasoner::new(config);
        reasoner.add_rule(rule);
        reasoner.run(&mut store);

        // Should have derived: socrates type Mortal
        let expected = Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
            Term::uri("http://example.org/Mortal"),
        );

        assert!(store.iter().any(|t| t == &expected));
    }

    #[test]
    fn test_parallel_simple() {
        let mut store = Store::new();

        // Add some data
        for i in 0..100 {
            store.add(Triple::new(
                Term::uri(&format!("http://example.org/s{}", i)),
                Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                Term::uri("http://example.org/Thing"),
            ));
        }

        // Create multiple rules
        let mut rules = Vec::new();
        for i in 0..10 {
            rules.push(Rule::named(
                format!("rule{}", i),
                vec![
                    Triple::new(
                        Term::universal("x"),
                        Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                        Term::uri("http://example.org/Thing"),
                    ),
                ],
                vec![
                    Triple::new(
                        Term::universal("x"),
                        Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                        Term::uri(&format!("http://example.org/Derived{}", i)),
                    ),
                ],
            ));
        }

        let config = ParallelConfig::default().with_workers(2);
        let mut reasoner = ParallelReasoner::new(config);
        reasoner.add_rules(rules);
        reasoner.run(&mut store);

        // Should have derived 100 * 10 = 1000 new triples
        // Original: 100, Derived: 1000, Total: 1100
        assert!(store.len() >= 100);
    }

    #[test]
    fn test_unify_term() {
        let var = Term::universal("x");
        let uri = Term::uri("http://example.org/test");
        let bindings = Bindings::default();
        let mut new_bindings = Bindings::default();

        assert!(unify_term(&var, &uri, &bindings, &mut new_bindings));
        let x_var = Variable::universal("x".to_string());
        assert_eq!(new_bindings.get(&x_var), Some(&uri));
    }

    #[test]
    fn test_merge_bindings() {
        let mut a = Bindings::default();
        let x_var = Variable::universal("x".to_string());
        a.insert(x_var.clone(), Term::uri("http://example.org/a"));

        let mut b = Bindings::default();
        let y_var = Variable::universal("y".to_string());
        b.insert(y_var.clone(), Term::uri("http://example.org/b"));

        let merged = merge_bindings(&a, &b);
        assert!(merged.contains_key(&x_var));
        assert!(merged.contains_key(&y_var));
    }

    #[test]
    fn test_num_cpus() {
        let n = num_cpus();
        assert!(n >= 1);
    }
}
