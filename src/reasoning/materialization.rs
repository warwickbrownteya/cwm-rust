//! Materialization strategies for inference
//!
//! This module provides different strategies for when and how to materialize
//! (pre-compute) inferences from rules.
//!
//! # Strategies
//!
//! - **Eager**: Compute all inferences upfront at load time
//! - **Lazy**: Compute inferences on-demand when queried
//! - **Partial**: Materialize frequently-used or low-cost patterns
//! - **Threshold**: Materialize based on cost/benefit analysis
//!
//! # Example
//!
//! ```ignore
//! use cwm::{MaterializationEngine, MaterializationStrategy, Store, Rule};
//!
//! let engine = MaterializationEngine::new(MaterializationStrategy::Partial {
//!     max_materialized: 10000,
//!     selectivity_threshold: 0.1,
//! });
//!
//! engine.materialize(&mut store, &rules);
//! ```

use std::collections::{HashMap, HashSet};
use std::time::Instant;
use crate::term::{Term, Triple, Bindings, substitute_triple};
use crate::store::Store;
use crate::builtins::{BuiltinRegistry, BuiltinResult};
use crate::reasoner::Rule;

/// Materialization strategy configuration
#[derive(Clone, Debug)]
pub enum MaterializationStrategy {
    /// Compute all inferences upfront
    Eager {
        /// Maximum number of inference steps
        max_steps: usize,
    },
    /// Compute inferences on-demand
    Lazy,
    /// Materialize frequently-used patterns
    Partial {
        /// Maximum triples to materialize
        max_materialized: usize,
        /// Only materialize patterns with selectivity below this threshold
        selectivity_threshold: f64,
    },
    /// Cost-benefit based materialization
    Threshold {
        /// Minimum benefit ratio to materialize (benefit / cost)
        min_benefit_ratio: f64,
        /// Maximum storage overhead as percentage of base data
        max_overhead_percent: f64,
    },
    /// Hybrid: eager for some predicates, lazy for others
    Hybrid {
        /// Predicates to eagerly materialize
        eager_predicates: HashSet<String>,
        /// Maximum steps for eager materialization
        max_eager_steps: usize,
    },
}

impl Default for MaterializationStrategy {
    fn default() -> Self {
        MaterializationStrategy::Eager { max_steps: 10000 }
    }
}

/// Statistics about materialization
#[derive(Clone, Debug, Default)]
pub struct MaterializationStats {
    /// Number of triples materialized
    pub triples_materialized: usize,
    /// Number of rules applied
    pub rules_applied: usize,
    /// Number of inference steps
    pub inference_steps: usize,
    /// Time spent materializing (milliseconds)
    pub time_ms: u64,
    /// Number of patterns skipped (not materialized)
    pub patterns_skipped: usize,
    /// Overhead percentage (materialized / base * 100)
    pub overhead_percent: f64,
}

/// Pattern usage tracking for partial materialization
#[derive(Clone, Debug, Default)]
pub struct PatternUsage {
    /// Query count per predicate
    predicate_queries: HashMap<String, usize>,
    /// Total queries
    total_queries: usize,
}

impl PatternUsage {
    /// Record a query on a predicate
    pub fn record_query(&mut self, predicate: &str) {
        *self.predicate_queries.entry(predicate.to_string()).or_insert(0) += 1;
        self.total_queries += 1;
    }

    /// Get query frequency for a predicate
    pub fn frequency(&self, predicate: &str) -> f64 {
        if self.total_queries == 0 {
            return 0.0;
        }
        let count = self.predicate_queries.get(predicate).copied().unwrap_or(0);
        count as f64 / self.total_queries as f64
    }

    /// Get most frequently queried predicates
    pub fn top_predicates(&self, n: usize) -> Vec<(String, usize)> {
        let mut sorted: Vec<_> = self.predicate_queries.iter().collect();
        sorted.sort_by(|a, b| b.1.cmp(a.1));
        sorted.into_iter().take(n).map(|(k, v)| (k.clone(), *v)).collect()
    }
}

/// Cost estimation for materialization decisions
#[derive(Clone, Debug)]
pub struct MaterializationCost {
    /// Estimated number of triples that would be derived
    pub estimated_triples: usize,
    /// Estimated storage cost (bytes)
    pub storage_cost: usize,
    /// Estimated computation cost (relative units)
    pub computation_cost: f64,
    /// Estimated query speedup factor
    pub query_speedup: f64,
}

impl MaterializationCost {
    /// Calculate benefit ratio
    pub fn benefit_ratio(&self) -> f64 {
        if self.computation_cost <= 0.0 || self.storage_cost == 0 {
            return 0.0;
        }
        self.query_speedup / (self.computation_cost + self.storage_cost as f64 / 1000.0)
    }
}

/// Materialized view for lazy evaluation
#[derive(Clone, Debug)]
pub struct MaterializedView {
    /// The pattern this view covers
    pub pattern_signature: String,
    /// Materialized triples
    pub triples: Vec<Triple>,
    /// Whether the view is up-to-date
    pub valid: bool,
    /// Last update time
    pub last_updated: Option<std::time::SystemTime>,
    /// Query count
    pub query_count: usize,
}

impl MaterializedView {
    /// Create a new materialized view
    pub fn new(pattern_signature: String) -> Self {
        MaterializedView {
            pattern_signature,
            triples: Vec::new(),
            valid: false,
            last_updated: None,
            query_count: 0,
        }
    }

    /// Invalidate the view
    pub fn invalidate(&mut self) {
        self.valid = false;
    }

    /// Update the view with new triples
    pub fn update(&mut self, triples: Vec<Triple>) {
        self.triples = triples;
        self.valid = true;
        self.last_updated = Some(std::time::SystemTime::now());
    }

    /// Record a query
    pub fn record_query(&mut self) {
        self.query_count += 1;
    }
}

/// Engine for managing materialization
pub struct MaterializationEngine {
    /// Current strategy
    strategy: MaterializationStrategy,
    /// Builtin registry
    builtins: BuiltinRegistry,
    /// Pattern usage statistics
    usage: PatternUsage,
    /// Materialized views (for lazy/partial strategies)
    views: HashMap<String, MaterializedView>,
    /// Statistics
    stats: MaterializationStats,
}

impl MaterializationEngine {
    /// Create a new materialization engine
    pub fn new(strategy: MaterializationStrategy) -> Self {
        MaterializationEngine {
            strategy,
            builtins: BuiltinRegistry::new(),
            usage: PatternUsage::default(),
            views: HashMap::new(),
            stats: MaterializationStats::default(),
        }
    }

    /// Get the current strategy
    pub fn strategy(&self) -> &MaterializationStrategy {
        &self.strategy
    }

    /// Set the strategy
    pub fn set_strategy(&mut self, strategy: MaterializationStrategy) {
        self.strategy = strategy;
    }

    /// Get statistics
    pub fn stats(&self) -> &MaterializationStats {
        &self.stats
    }

    /// Get pattern usage
    pub fn usage(&self) -> &PatternUsage {
        &self.usage
    }

    /// Materialize inferences according to the current strategy
    pub fn materialize(&mut self, store: &mut Store, rules: &[Rule]) -> &MaterializationStats {
        let start = Instant::now();
        let base_size = store.len();

        self.stats = MaterializationStats::default();

        // Clone strategy parameters to avoid borrow issues
        let strategy = self.strategy.clone();

        match strategy {
            MaterializationStrategy::Eager { max_steps } => {
                self.materialize_eager(store, rules, max_steps);
            }
            MaterializationStrategy::Lazy => {
                // No upfront materialization
                self.stats.patterns_skipped = rules.len();
            }
            MaterializationStrategy::Partial { max_materialized, selectivity_threshold } => {
                self.materialize_partial(store, rules, max_materialized, selectivity_threshold);
            }
            MaterializationStrategy::Threshold { min_benefit_ratio, max_overhead_percent } => {
                self.materialize_threshold(store, rules, min_benefit_ratio, max_overhead_percent, base_size);
            }
            MaterializationStrategy::Hybrid { eager_predicates, max_eager_steps } => {
                self.materialize_hybrid(store, rules, &eager_predicates, max_eager_steps);
            }
        }

        self.stats.time_ms = start.elapsed().as_millis() as u64;
        self.stats.overhead_percent = if base_size > 0 {
            (self.stats.triples_materialized as f64 / base_size as f64) * 100.0
        } else {
            0.0
        };

        &self.stats
    }

    /// Eager materialization - compute all inferences
    fn materialize_eager(&mut self, store: &mut Store, rules: &[Rule], max_steps: usize) {
        let mut derived_signatures: HashSet<String> = HashSet::new();

        // Initialize with existing triples
        for triple in store.iter() {
            derived_signatures.insert(format!("{:?}", triple));
        }

        for step in 0..max_steps {
            let mut new_triples = Vec::new();

            for rule in rules {
                let matches = self.match_antecedent(store, &rule.antecedent);

                for bindings in matches {
                    for pattern in &rule.consequent {
                        let triple = substitute_triple(pattern, &bindings);

                        if !triple.is_ground() {
                            continue;
                        }

                        let sig = format!("{:?}", triple);
                        if !derived_signatures.contains(&sig) {
                            derived_signatures.insert(sig);
                            new_triples.push(triple);
                            self.stats.rules_applied += 1;
                        }
                    }
                }
            }

            if new_triples.is_empty() {
                break;
            }

            self.stats.triples_materialized += new_triples.len();
            for triple in new_triples {
                store.add(triple);
            }

            self.stats.inference_steps = step + 1;
        }
    }

    /// Partial materialization based on selectivity
    fn materialize_partial(
        &mut self,
        store: &mut Store,
        rules: &[Rule],
        max_materialized: usize,
        selectivity_threshold: f64,
    ) {
        // Estimate selectivity for each rule
        let rule_selectivities: Vec<(usize, f64)> = rules
            .iter()
            .enumerate()
            .map(|(i, rule)| {
                let selectivity = self.estimate_rule_selectivity(store, rule);
                (i, selectivity)
            })
            .collect();

        // Sort by selectivity (most selective first)
        let mut sorted = rule_selectivities;
        sorted.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());

        // Materialize rules below threshold until limit reached
        let mut derived_signatures: HashSet<String> = store.iter()
            .map(|t| format!("{:?}", t))
            .collect();

        for (rule_idx, selectivity) in sorted {
            if selectivity > selectivity_threshold {
                self.stats.patterns_skipped += 1;
                continue;
            }

            if self.stats.triples_materialized >= max_materialized {
                self.stats.patterns_skipped += 1;
                continue;
            }

            let rule = &rules[rule_idx];
            let matches = self.match_antecedent(store, &rule.antecedent);

            for bindings in matches {
                if self.stats.triples_materialized >= max_materialized {
                    break;
                }

                for pattern in &rule.consequent {
                    let triple = substitute_triple(pattern, &bindings);

                    if !triple.is_ground() {
                        continue;
                    }

                    let sig = format!("{:?}", triple);
                    if !derived_signatures.contains(&sig) {
                        derived_signatures.insert(sig);
                        store.add(triple);
                        self.stats.triples_materialized += 1;
                        self.stats.rules_applied += 1;
                    }
                }
            }
        }
    }

    /// Threshold-based materialization
    fn materialize_threshold(
        &mut self,
        store: &mut Store,
        rules: &[Rule],
        min_benefit_ratio: f64,
        max_overhead_percent: f64,
        base_size: usize,
    ) {
        let max_overhead = ((base_size as f64 * max_overhead_percent) / 100.0) as usize;
        let mut derived_signatures: HashSet<String> = store.iter()
            .map(|t| format!("{:?}", t))
            .collect();

        for rule in rules {
            let cost = self.estimate_materialization_cost(store, rule);

            if cost.benefit_ratio() < min_benefit_ratio {
                self.stats.patterns_skipped += 1;
                continue;
            }

            if self.stats.triples_materialized + cost.estimated_triples > max_overhead {
                self.stats.patterns_skipped += 1;
                continue;
            }

            let matches = self.match_antecedent(store, &rule.antecedent);

            for bindings in matches {
                for pattern in &rule.consequent {
                    let triple = substitute_triple(pattern, &bindings);

                    if !triple.is_ground() {
                        continue;
                    }

                    let sig = format!("{:?}", triple);
                    if !derived_signatures.contains(&sig) {
                        derived_signatures.insert(sig);
                        store.add(triple);
                        self.stats.triples_materialized += 1;
                        self.stats.rules_applied += 1;
                    }
                }
            }
        }
    }

    /// Hybrid materialization
    fn materialize_hybrid(
        &mut self,
        store: &mut Store,
        rules: &[Rule],
        eager_predicates: &HashSet<String>,
        max_eager_steps: usize,
    ) {
        // Filter rules by predicate
        let eager_rules: Vec<&Rule> = rules
            .iter()
            .filter(|rule| {
                rule.consequent.iter().any(|t| {
                    if let Term::Uri(u) = &t.predicate {
                        eager_predicates.contains(u.as_str())
                    } else {
                        false
                    }
                })
            })
            .collect();

        let eager_rules: Vec<Rule> = eager_rules.into_iter().cloned().collect();

        // Materialize eager rules
        let skipped_count = rules.len() - eager_rules.len();
        self.materialize_eager(store, &eager_rules, max_eager_steps);
        self.stats.patterns_skipped += skipped_count;
    }

    /// Match antecedent patterns against the store
    fn match_antecedent(&self, store: &Store, patterns: &[Triple]) -> Vec<Bindings> {
        if patterns.is_empty() {
            return vec![Bindings::default()];
        }

        let first = &patterns[0];
        let rest = &patterns[1..];

        // Check for builtin
        if let Some(results) = self.try_builtin(first, &Bindings::default()) {
            let mut all_results = Vec::new();
            for bindings in results {
                let remaining = self.match_with_bindings(store, rest, bindings);
                all_results.extend(remaining);
            }
            return all_results;
        }

        let first_matches = store.match_pattern(first);
        let mut results = Vec::new();

        for bindings in first_matches {
            let remaining = self.match_with_bindings(store, rest, bindings);
            results.extend(remaining);
        }

        results
    }

    /// Continue matching with partial bindings
    fn match_with_bindings(
        &self,
        store: &Store,
        patterns: &[Triple],
        bindings: Bindings,
    ) -> Vec<Bindings> {
        if patterns.is_empty() {
            return vec![bindings];
        }

        let pattern = substitute_triple(&patterns[0], &bindings);
        let rest = &patterns[1..];

        // Check for builtin
        if let Some(results) = self.try_builtin(&pattern, &bindings) {
            let mut all_results = Vec::new();
            for new_bindings in results {
                let mut merged = bindings.clone();
                for (var, term) in new_bindings {
                    merged.insert(var, term);
                }
                let remaining = self.match_with_bindings(store, rest, merged);
                all_results.extend(remaining);
            }
            return all_results;
        }

        let matches = store.match_pattern(&pattern);
        let mut results = Vec::new();

        for new_bindings in matches {
            let mut merged = bindings.clone();
            for (var, term) in new_bindings {
                merged.insert(var, term);
            }
            let remaining = self.match_with_bindings(store, rest, merged);
            results.extend(remaining);
        }

        results
    }

    /// Try to evaluate a pattern as a builtin
    fn try_builtin(&self, pattern: &Triple, bindings: &Bindings) -> Option<Vec<Bindings>> {
        if let Term::Uri(uri) = &pattern.predicate {
            if self.builtins.is_builtin(uri.as_str()) {
                match self.builtins.evaluate(uri.as_str(), &pattern.subject, &pattern.object, bindings) {
                    BuiltinResult::Success(result) => Some(vec![result]),
                    BuiltinResult::Failure => Some(vec![]),
                    BuiltinResult::NotReady => None,
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Estimate selectivity of a rule
    fn estimate_rule_selectivity(&self, store: &Store, rule: &Rule) -> f64 {
        if store.len() == 0 {
            return 1.0;
        }

        // Estimate based on bound terms in antecedent
        let mut total_bound = 0;
        let mut total_terms = 0;

        for pattern in &rule.antecedent {
            if !matches!(pattern.subject, Term::Variable(_)) {
                total_bound += 1;
            }
            if !matches!(pattern.predicate, Term::Variable(_)) {
                total_bound += 1;
            }
            if !matches!(pattern.object, Term::Variable(_)) {
                total_bound += 1;
            }
            total_terms += 3;
        }

        if total_terms == 0 {
            return 1.0;
        }

        1.0 - (total_bound as f64 / total_terms as f64)
    }

    /// Estimate cost of materializing a rule
    fn estimate_materialization_cost(&self, store: &Store, rule: &Rule) -> MaterializationCost {
        let selectivity = self.estimate_rule_selectivity(store, rule);
        let estimated_triples = (store.len() as f64 * selectivity) as usize;

        // Rough estimates
        let storage_cost = estimated_triples * 100; // ~100 bytes per triple
        let computation_cost = estimated_triples as f64 * rule.antecedent.len() as f64;
        let query_speedup = if selectivity < 0.1 { 10.0 } else { 1.0 / selectivity };

        MaterializationCost {
            estimated_triples,
            storage_cost,
            computation_cost,
            query_speedup,
        }
    }

    /// Get or create a materialized view
    pub fn get_or_create_view(&mut self, pattern_signature: &str) -> &mut MaterializedView {
        self.views
            .entry(pattern_signature.to_string())
            .or_insert_with(|| MaterializedView::new(pattern_signature.to_string()))
    }

    /// Invalidate all views (called when base data changes)
    pub fn invalidate_all_views(&mut self) {
        for view in self.views.values_mut() {
            view.invalidate();
        }
    }

    /// Record a pattern query (for usage-based optimization)
    pub fn record_pattern_query(&mut self, predicate: &str) {
        self.usage.record_query(predicate);
    }
}

impl Default for MaterializationEngine {
    fn default() -> Self {
        MaterializationEngine::new(MaterializationStrategy::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_rule(antecedent: Vec<Triple>, consequent: Vec<Triple>) -> Rule {
        Rule::new(antecedent, consequent)
    }

    #[test]
    fn test_eager_materialization() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        ));

        let rule = make_rule(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Human"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Mortal"),
            )],
        );

        let mut engine = MaterializationEngine::new(MaterializationStrategy::Eager {
            max_steps: 100,
        });
        engine.materialize(&mut store, &[rule]);

        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Mortal"),
        )));
        assert_eq!(engine.stats().triples_materialized, 1);
    }

    #[test]
    fn test_lazy_materialization() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        ));

        let rules = vec![make_rule(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/p"),
                Term::universal("y"),
            )],
            vec![Triple::new(
                Term::universal("y"),
                Term::uri("http://example.org/q"),
                Term::universal("x"),
            )],
        )];

        let mut engine = MaterializationEngine::new(MaterializationStrategy::Lazy);
        engine.materialize(&mut store, &rules);

        // No triples should be materialized
        assert_eq!(engine.stats().triples_materialized, 0);
        assert_eq!(engine.stats().patterns_skipped, 1);
    }

    #[test]
    fn test_partial_materialization() {
        let mut store = Store::new();
        for i in 0..100 {
            store.add(Triple::new(
                Term::uri(&format!("http://example.org/s{}", i)),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Thing"),
            ));
        }

        let rule = make_rule(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::universal("t"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/category"),
                Term::literal("general"),
            )],
        );

        let mut engine = MaterializationEngine::new(MaterializationStrategy::Partial {
            max_materialized: 50,
            selectivity_threshold: 0.5,
        });
        engine.materialize(&mut store, &[rule]);

        // Should be capped at max_materialized
        assert!(engine.stats().triples_materialized <= 50);
    }

    #[test]
    fn test_hybrid_materialization() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        ));

        let eager_rule = make_rule(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/p"),
                Term::universal("y"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/eager"),
                Term::universal("y"),
            )],
        );

        let lazy_rule = make_rule(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/p"),
                Term::universal("y"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/lazy"),
                Term::universal("y"),
            )],
        );

        let eager_predicates: HashSet<String> =
            vec!["http://example.org/eager".to_string()].into_iter().collect();

        let mut engine = MaterializationEngine::new(MaterializationStrategy::Hybrid {
            eager_predicates,
            max_eager_steps: 100,
        });
        engine.materialize(&mut store, &[eager_rule, lazy_rule]);

        // Should only materialize the eager predicate
        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/eager"),
            Term::uri("http://example.org/b"),
        )));
    }

    #[test]
    fn test_pattern_usage_tracking() {
        let mut usage = PatternUsage::default();

        usage.record_query("http://example.org/type");
        usage.record_query("http://example.org/type");
        usage.record_query("http://example.org/name");

        assert_eq!(usage.frequency("http://example.org/type"), 2.0 / 3.0);
        assert_eq!(usage.frequency("http://example.org/name"), 1.0 / 3.0);

        let top = usage.top_predicates(2);
        assert_eq!(top[0].0, "http://example.org/type");
    }

    #[test]
    fn test_cost_estimation() {
        let mut store = Store::new();
        for i in 0..100 {
            store.add(Triple::new(
                Term::uri(&format!("http://example.org/s{}", i)),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Thing"),
            ));
        }

        let engine = MaterializationEngine::default();
        let rule = make_rule(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Thing"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/derived"),
                Term::literal("yes"),
            )],
        );

        let cost = engine.estimate_materialization_cost(&store, &rule);

        assert!(cost.estimated_triples > 0);
        assert!(cost.benefit_ratio() > 0.0);
    }
}
