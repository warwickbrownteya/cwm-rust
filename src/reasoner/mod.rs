//! Forward-chaining reasoner for N3 rules
//!
//! Implements the core inference algorithm of cwm:
//! - Pattern matching with unification
//! - Forward chaining over rules
//! - Built-in predicate evaluation
//! - Proof generation for inference tracking
//! - Tabling/memoization for cycle detection and performance
//! - Incremental reasoning with dependency tracking
//! - Human-readable explanation generation

pub mod proof;
pub mod parallel;
pub mod incremental;
pub mod explanation;

use crate::term::{Term, Triple, Variable, Bindings, substitute_triple};
use crate::store::Store;
use crate::builtins::BuiltinRegistry;
use std::collections::{HashSet, HashMap};

/// Tabling state for memoization and cycle detection
#[derive(Clone, Debug, Default)]
pub struct TablingState {
    /// Patterns currently being computed (for cycle detection)
    /// Note: Used by backward chaining; forward chaining uses its own mechanism
    #[allow(dead_code)]
    computing: HashSet<String>,
    /// Cached pattern match results
    /// Note: Used by backward chaining; forward chaining uses its own mechanism
    #[allow(dead_code)]
    cache: HashMap<String, Vec<Bindings>>,
    /// Number of cache hits
    pub cache_hits: usize,
    /// Number of cache misses
    pub cache_misses: usize,
    /// Number of cycles detected
    pub cycles_detected: usize,
}

/// A single step in a proof chain
#[derive(Clone, Debug)]
pub struct ProofStep {
    /// The derived triple
    pub conclusion: Triple,
    /// Index of the rule that produced this conclusion
    pub rule_index: usize,
    /// The rule name (if any)
    pub rule_name: Option<String>,
    /// The antecedent patterns from the rule
    pub rule_antecedent: Vec<Triple>,
    /// The consequent patterns from the rule
    pub rule_consequent: Vec<Triple>,
    /// The variable bindings used
    pub bindings: Vec<(String, Term)>,
    /// The ground triples that matched the antecedent
    pub premises: Vec<Triple>,
    /// Step number in the inference sequence
    pub step_number: usize,
}

/// A complete proof trace for an inference session
#[derive(Clone, Debug, Default)]
pub struct Proof {
    /// All inference steps in order
    pub steps: Vec<ProofStep>,
    /// The original (asserted) triples
    pub assertions: Vec<Triple>,
}

/// A rule in N3 (antecedent => consequent)
#[derive(Clone, Debug)]
pub struct Rule {
    /// Name/identifier for the rule (optional)
    pub name: Option<String>,
    /// Antecedent patterns (body)
    pub antecedent: Vec<Triple>,
    /// Consequent patterns (head)
    pub consequent: Vec<Triple>,
    /// Variables in scope
    pub variables: Vec<Variable>,
}

impl Rule {
    /// Create a new rule
    pub fn new(antecedent: Vec<Triple>, consequent: Vec<Triple>) -> Self {
        // Collect all variables
        let mut variables = Vec::new();
        for triple in antecedent.iter().chain(consequent.iter()) {
            Self::collect_variables(&triple.subject, &mut variables);
            Self::collect_variables(&triple.predicate, &mut variables);
            Self::collect_variables(&triple.object, &mut variables);
        }

        Rule {
            name: None,
            antecedent,
            consequent,
            variables,
        }
    }

    fn collect_variables(term: &Term, vars: &mut Vec<Variable>) {
        if let Term::Variable(v) = term {
            if !vars.contains(v) {
                vars.push(v.clone());
            }
        }
    }

    /// Create a named rule
    pub fn named(name: impl Into<String>, antecedent: Vec<Triple>, consequent: Vec<Triple>) -> Self {
        let mut rule = Self::new(antecedent, consequent);
        rule.name = Some(name.into());
        rule
    }
}

/// Configuration for the reasoner
#[derive(Clone, Debug)]
pub struct ReasonerConfig {
    /// Maximum number of inference steps
    pub max_steps: usize,
    /// Whether to apply rules recursively
    pub recursive: bool,
    /// Whether to filter derivations (think mode)
    pub filter: bool,
    /// Whether to generate proof traces
    pub generate_proof: bool,
    /// Whether to enable tabling/memoization for cycle detection
    pub enable_tabling: bool,
    /// Whether to enable crypto builtins (requires --crypto flag)
    pub enable_crypto: bool,
}

impl Default for ReasonerConfig {
    fn default() -> Self {
        ReasonerConfig {
            max_steps: 10000,
            recursive: true,
            filter: false,
            generate_proof: false,
            enable_tabling: true, // Enable by default for safety
            enable_crypto: false, // Disabled by default, requires --crypto flag
        }
    }
}

/// The forward-chaining reasoner
pub struct Reasoner {
    /// Configuration
    config: ReasonerConfig,
    /// Built-in predicate registry
    builtins: BuiltinRegistry,
    /// Rules to apply
    rules: Vec<Rule>,
    /// Statistics
    stats: ReasonerStats,
    /// Proof trace (if proof generation is enabled)
    proof: Option<Proof>,
    /// Tabling state for memoization
    tabling: TablingState,
    /// Set of derived triple signatures for duplicate detection
    derived_signatures: HashSet<String>,
}

/// Statistics about reasoning
#[derive(Clone, Debug, Default)]
pub struct ReasonerStats {
    pub steps: usize,
    pub rules_fired: usize,
    pub triples_derived: usize,
    pub builtins_evaluated: usize,
}

impl Reasoner {
    /// Create a new reasoner with default configuration
    pub fn new() -> Self {
        Reasoner {
            config: ReasonerConfig::default(),
            builtins: BuiltinRegistry::new(),
            rules: Vec::new(),
            stats: ReasonerStats::default(),
            proof: None,
            tabling: TablingState::default(),
            derived_signatures: HashSet::new(),
        }
    }

    /// Create a reasoner with custom configuration
    pub fn with_config(config: ReasonerConfig) -> Self {
        let generate_proof = config.generate_proof;
        let enable_crypto = config.enable_crypto;
        Reasoner {
            config,
            builtins: BuiltinRegistry::with_options(enable_crypto),
            rules: Vec::new(),
            stats: ReasonerStats::default(),
            proof: if generate_proof { Some(Proof::default()) } else { None },
            tabling: TablingState::default(),
            derived_signatures: HashSet::new(),
        }
    }

    /// Enable proof generation
    pub fn enable_proof(&mut self) {
        self.config.generate_proof = true;
        self.proof = Some(Proof::default());
    }

    /// Get the proof trace (if proof generation was enabled)
    pub fn proof(&self) -> Option<&Proof> {
        self.proof.as_ref()
    }

    /// Take ownership of the proof trace
    pub fn take_proof(&mut self) -> Option<Proof> {
        self.proof.take()
    }

    /// Get tabling statistics
    pub fn tabling_stats(&self) -> &TablingState {
        &self.tabling
    }

    /// Reset tabling state (useful between runs)
    pub fn reset_tabling(&mut self) {
        self.tabling = TablingState::default();
        self.derived_signatures.clear();
    }

    /// Add a rule to the reasoner
    pub fn add_rule(&mut self, rule: Rule) {
        self.rules.push(rule);
    }

    /// Add multiple rules
    pub fn add_rules(&mut self, rules: impl IntoIterator<Item = Rule>) {
        self.rules.extend(rules);
    }

    /// Get the built-in registry
    pub fn builtins(&self) -> &BuiltinRegistry {
        &self.builtins
    }

    /// Get mutable access to the built-in registry
    pub fn builtins_mut(&mut self) -> &mut BuiltinRegistry {
        &mut self.builtins
    }

    /// Run forward chaining inference on a store
    pub fn run(&mut self, store: &mut Store) -> &ReasonerStats {
        self.stats = ReasonerStats::default();

        // Reset tabling state for fresh run
        if self.config.enable_tabling {
            self.reset_tabling();
            // Pre-populate derived signatures with existing triples
            for triple in store.iter() {
                self.derived_signatures.insert(format!("{:?}", triple));
            }
        }

        // Capture original assertions for proof
        if let Some(ref mut proof) = self.proof {
            proof.assertions = store.iter().cloned().collect();
        }

        loop {
            if self.stats.steps >= self.config.max_steps {
                break;
            }

            let derived = self.step(store);
            self.stats.steps += 1;

            if derived == 0 || !self.config.recursive {
                break;
            }
        }

        &self.stats
    }

    /// Execute a single inference step
    fn step(&mut self, store: &mut Store) -> usize {
        // Collect derivations with proof info: (triple, rule_index, bindings, premises)
        let mut derivations: Vec<(Triple, usize, Bindings, Vec<Triple>)> = Vec::new();
        // Track signatures of triples being derived in this step
        let mut step_signatures: HashSet<String> = HashSet::new();

        for (rule_idx, rule) in self.rules.iter().enumerate() {
            // Find all matches for the antecedent, tracking premises
            let matches = self.match_antecedent_with_premises(store, &rule.antecedent);

            for (bindings, premises) in matches {
                // Generate consequent triples
                for pattern in &rule.consequent {
                    let triple = substitute_triple(pattern, &bindings);

                    // Only process ground triples
                    if !triple.is_ground() {
                        continue;
                    }

                    let sig = format!("{:?}", triple);

                    // Use tabling for fast duplicate detection if enabled
                    let already_exists = if self.config.enable_tabling {
                        self.derived_signatures.contains(&sig)
                    } else {
                        store.contains(&triple)
                    };

                    if !already_exists && !step_signatures.contains(&sig) {
                        step_signatures.insert(sig);
                        derivations.push((triple, rule_idx, bindings.clone(), premises.clone()));
                        self.stats.rules_fired += 1;
                    }
                }
            }
        }

        let count = derivations.len();
        self.stats.triples_derived += count;

        // Add triples to store and record proof steps
        let proof_enabled = self.proof.is_some();
        let step_base = if let Some(ref proof) = self.proof {
            proof.steps.len()
        } else {
            0
        };

        for (i, (triple, rule_idx, bindings, premises)) in derivations.into_iter().enumerate() {
            // Update tabling state
            if self.config.enable_tabling {
                self.derived_signatures.insert(format!("{:?}", triple));
            }

            store.add(triple.clone());

            // Record proof step if proof generation is enabled
            if proof_enabled {
                let rule = &self.rules[rule_idx];
                let proof_step = ProofStep {
                    conclusion: triple,
                    rule_index: rule_idx,
                    rule_name: rule.name.clone(),
                    rule_antecedent: rule.antecedent.clone(),
                    rule_consequent: rule.consequent.clone(),
                    bindings: bindings.iter().map(|(v, t)| (v.name().to_string(), t.clone())).collect(),
                    premises,
                    step_number: step_base + i + 1,
                };
                if let Some(ref mut proof) = self.proof {
                    proof.steps.push(proof_step);
                }
            }
        }

        count
    }

    /// Match antecedent patterns and track which triples matched (premises)
    fn match_antecedent_with_premises(&self, store: &Store, patterns: &[Triple]) -> Vec<(Bindings, Vec<Triple>)> {
        if patterns.is_empty() {
            return vec![(Bindings::default(), Vec::new())];
        }

        let first = &patterns[0];
        let rest = &patterns[1..];

        // Check if this is a built-in predicate
        if let Some(builtin_results) = self.try_builtin_with_bindings(store, first, &Bindings::default()) {
            let mut results = Vec::new();
            for bindings in builtin_results {
                // Built-in matched - no ground premise for builtins
                let remaining = self.match_with_bindings_and_premises(store, rest, bindings, Vec::new());
                results.extend(remaining);
            }
            return results;
        }

        // Regular pattern matching
        let first_matches = store.match_pattern(first);

        let mut results = Vec::new();
        for bindings in first_matches {
            // Get the ground triple that matched this pattern
            let ground_triple = substitute_triple(first, &bindings);
            let remaining = self.match_with_bindings_and_premises(store, rest, bindings, vec![ground_triple]);
            results.extend(remaining);
        }

        results
    }

    /// Continue matching with partial bindings, tracking premises
    fn match_with_bindings_and_premises(
        &self,
        store: &Store,
        patterns: &[Triple],
        bindings: Bindings,
        premises: Vec<Triple>,
    ) -> Vec<(Bindings, Vec<Triple>)> {
        if patterns.is_empty() {
            return vec![(bindings, premises)];
        }

        let pattern = substitute_triple(&patterns[0], &bindings);
        let rest = &patterns[1..];

        // Check for built-in
        if let Some(builtin_results) = self.try_builtin_with_bindings(store, &pattern, &bindings) {
            let mut results = Vec::new();
            for new_bindings in builtin_results {
                let mut merged = bindings.clone();
                for (var, term) in new_bindings {
                    merged.insert(var, term);
                }
                // Built-in doesn't add to premises
                let remaining = self.match_with_bindings_and_premises(store, rest, merged, premises.clone());
                results.extend(remaining);
            }
            return results;
        }

        // Regular matching
        let matches = store.match_pattern(&pattern);

        let mut results = Vec::new();
        for new_bindings in matches {
            let mut merged = bindings.clone();
            for (var, term) in new_bindings {
                merged.insert(var, term);
            }
            // Add the ground triple to premises
            let ground_triple = substitute_triple(&patterns[0], &merged);
            let mut new_premises = premises.clone();
            new_premises.push(ground_triple);
            let remaining = self.match_with_bindings_and_premises(store, rest, merged, new_premises);
            results.extend(remaining);
        }

        results
    }

    /// Try to evaluate a pattern as a built-in predicate
    fn try_builtin_with_bindings(&self, _store: &Store, pattern: &Triple, current_bindings: &Bindings) -> Option<Vec<Bindings>> {
        // Check if the predicate is a built-in
        if let Term::Uri(uri) = &pattern.predicate {
            if self.builtins.is_builtin(uri.as_str()) {
                // Evaluate the built-in predicate with current bindings
                use crate::builtins::BuiltinResult;

                match self.builtins.evaluate(uri.as_str(), &pattern.subject, &pattern.object, current_bindings) {
                    BuiltinResult::Success(result_bindings) => {
                        return Some(vec![result_bindings]);
                    }
                    BuiltinResult::Failure => {
                        return Some(vec![]); // No matches
                    }
                    BuiltinResult::NotReady => {
                        // Built-in needs more bound values, fall through to regular matching
                        return None;
                    }
                }
            }
        }
        None
    }

    /// Get statistics
    pub fn stats(&self) -> &ReasonerStats {
        &self.stats
    }
}

/// Stratification analysis for logic programs
/// Determines if rules can be evaluated in a well-defined order
#[derive(Clone, Debug)]
pub struct StratificationResult {
    /// Whether the program is stratified (has no cycles through negation)
    pub is_stratified: bool,
    /// Rule indices grouped by stratum (first stratum = 0)
    pub strata: Vec<Vec<usize>>,
    /// Warnings about potential issues
    pub warnings: Vec<String>,
}

impl Reasoner {
    /// Analyze rules for stratification
    /// Returns information about rule ordering and any stratification issues
    pub fn analyze_stratification(&self) -> StratificationResult {
        let log_not_includes = "http://www.w3.org/2000/10/swap/log#notIncludes";

        // Extract predicates from each rule
        let mut rule_body_preds: Vec<HashSet<String>> = Vec::new();
        let mut rule_head_preds: Vec<HashSet<String>> = Vec::new();
        let mut rule_negated_preds: Vec<HashSet<String>> = Vec::new();

        for rule in &self.rules {
            let mut body = HashSet::new();
            let mut head = HashSet::new();
            let mut negated = HashSet::new();

            // Extract predicates from antecedent
            for triple in &rule.antecedent {
                if let Term::Uri(uri) = &triple.predicate {
                    let pred = uri.as_str().to_string();
                    if pred == log_not_includes {
                        // This is a negation - extract predicates from the formula argument
                        if let Term::Formula(formula) = &triple.object {
                            for inner in formula.triples() {
                                if let Term::Uri(inner_uri) = &inner.predicate {
                                    negated.insert(inner_uri.as_str().to_string());
                                }
                            }
                        }
                    } else {
                        body.insert(pred);
                    }
                }
            }

            // Extract predicates from consequent
            for triple in &rule.consequent {
                if let Term::Uri(uri) = &triple.predicate {
                    head.insert(uri.as_str().to_string());
                }
            }

            rule_body_preds.push(body);
            rule_head_preds.push(head);
            rule_negated_preds.push(negated);
        }

        // Build dependency graph
        // A rule R1 depends on R2 if R1's body mentions a predicate defined in R2's head
        let mut dependencies: Vec<HashSet<usize>> = vec![HashSet::new(); self.rules.len()];
        let mut neg_dependencies: Vec<HashSet<usize>> = vec![HashSet::new(); self.rules.len()];

        for (i, body) in rule_body_preds.iter().enumerate() {
            for (j, head) in rule_head_preds.iter().enumerate() {
                if i != j {
                    // Check if rule i depends on predicates defined by rule j
                    if !body.is_disjoint(head) {
                        dependencies[i].insert(j);
                    }
                }
            }
        }

        for (i, negated) in rule_negated_preds.iter().enumerate() {
            for (j, head) in rule_head_preds.iter().enumerate() {
                if i != j {
                    // Check if rule i negatively depends on predicates defined by rule j
                    if !negated.is_disjoint(head) {
                        neg_dependencies[i].insert(j);
                    }
                }
            }
        }

        // Check for cycles through negation (non-stratified)
        let mut warnings = Vec::new();
        let mut is_stratified = true;

        // Simple cycle detection through negative dependencies
        for (i, neg_deps) in neg_dependencies.iter().enumerate() {
            for &j in neg_deps {
                // Check if j depends (transitively) on i
                let mut visited = HashSet::new();
                let mut stack = vec![j];
                while let Some(current) = stack.pop() {
                    if current == i {
                        is_stratified = false;
                        warnings.push(format!(
                            "Non-stratified negation detected: rule {} negatively depends on rule {} which depends back on rule {}",
                            i, j, i
                        ));
                        break;
                    }
                    if !visited.contains(&current) {
                        visited.insert(current);
                        stack.extend(&dependencies[current]);
                    }
                }
            }
        }

        // Compute strata using topological sort
        let mut strata: Vec<Vec<usize>> = Vec::new();
        let mut assigned: HashSet<usize> = HashSet::new();

        // First stratum: rules with no dependencies
        let current_stratum: Vec<usize> = (0..self.rules.len())
            .filter(|&i| dependencies[i].is_empty() && neg_dependencies[i].is_empty())
            .collect();

        if !current_stratum.is_empty() {
            assigned.extend(&current_stratum);
            strata.push(current_stratum);
        }

        // Subsequent strata: rules whose dependencies are all in previous strata
        let max_iterations = self.rules.len() + 1;
        for _ in 0..max_iterations {
            if assigned.len() == self.rules.len() {
                break;
            }

            let mut next_stratum: Vec<usize> = Vec::new();
            for i in 0..self.rules.len() {
                if assigned.contains(&i) {
                    continue;
                }
                // All positive dependencies must be assigned
                let pos_satisfied = dependencies[i].iter().all(|d| assigned.contains(d));
                // All negative dependencies must be assigned
                let neg_satisfied = neg_dependencies[i].iter().all(|d| assigned.contains(d));

                if pos_satisfied && neg_satisfied {
                    next_stratum.push(i);
                }
            }

            if next_stratum.is_empty() {
                // Remaining rules form a cycle
                let remaining: Vec<usize> = (0..self.rules.len())
                    .filter(|i| !assigned.contains(i))
                    .collect();
                warnings.push(format!(
                    "Cyclic dependencies detected among rules: {:?}",
                    remaining
                ));
                // Add them all to final stratum anyway
                strata.push(remaining.clone());
                assigned.extend(remaining);
            } else {
                assigned.extend(&next_stratum);
                strata.push(next_stratum);
            }
        }

        StratificationResult {
            is_stratified,
            strata,
            warnings,
        }
    }

    /// Reorder rules according to stratification analysis
    /// Returns true if rules were reordered
    pub fn stratify_rules(&mut self) -> bool {
        let analysis = self.analyze_stratification();

        if analysis.strata.len() <= 1 {
            return false; // No reordering needed
        }

        // Collect rules in stratified order
        let mut new_order: Vec<Rule> = Vec::with_capacity(self.rules.len());
        for stratum in &analysis.strata {
            for &idx in stratum {
                new_order.push(self.rules[idx].clone());
            }
        }

        self.rules = new_order;
        true
    }
}

impl Default for Reasoner {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// ReasonerBuilder - Fluent API for constructing reasoners
// ============================================================================

/// Builder for constructing reasoners with custom configuration
///
/// Provides a fluent API for composing reasoners:
///
/// # Example
///
/// ```ignore
/// use cwm::{ReasonerBuilder, Store};
///
/// let reasoner = ReasonerBuilder::new()
///     .with_max_steps(1000)
///     .with_tabling(true)
///     .with_proof_generation(true)
///     .with_rule(rule)
///     .build();
///
/// reasoner.run(&mut store);
/// ```
#[derive(Clone, Debug, Default)]
pub struct ReasonerBuilder {
    config: ReasonerConfig,
    rules: Vec<Rule>,
    custom_builtins: Option<BuiltinRegistry>,
}

impl ReasonerBuilder {
    /// Create a new builder with default configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Set maximum inference steps
    pub fn with_max_steps(mut self, max_steps: usize) -> Self {
        self.config.max_steps = max_steps;
        self
    }

    /// Enable or disable tabling (memoization and cycle detection)
    pub fn with_tabling(mut self, enabled: bool) -> Self {
        self.config.enable_tabling = enabled;
        self
    }

    /// Enable or disable proof generation
    pub fn with_proof_generation(mut self, enabled: bool) -> Self {
        self.config.generate_proof = enabled;
        self
    }

    /// Enable or disable recursive rule application
    pub fn with_recursive(mut self, recursive: bool) -> Self {
        self.config.recursive = recursive;
        self
    }

    /// Enable or disable filter mode (think mode)
    pub fn with_filter(mut self, filter: bool) -> Self {
        self.config.filter = filter;
        self
    }

    /// Set the complete configuration
    pub fn with_config(mut self, config: ReasonerConfig) -> Self {
        self.config = config;
        self
    }

    /// Add a single rule
    pub fn with_rule(mut self, rule: Rule) -> Self {
        self.rules.push(rule);
        self
    }

    /// Add multiple rules
    pub fn with_rules(mut self, rules: impl IntoIterator<Item = Rule>) -> Self {
        self.rules.extend(rules);
        self
    }

    /// Use a custom builtin registry
    pub fn with_builtins(mut self, builtins: BuiltinRegistry) -> Self {
        self.custom_builtins = Some(builtins);
        self
    }

    /// Enable or disable crypto builtins
    pub fn with_crypto(mut self, enabled: bool) -> Self {
        self.config.enable_crypto = enabled;
        self
    }

    /// Build the reasoner with all configured options
    pub fn build(self) -> Reasoner {
        let generate_proof = self.config.generate_proof;
        let enable_crypto = self.config.enable_crypto;
        Reasoner {
            config: self.config,
            builtins: self.custom_builtins.unwrap_or_else(|| BuiltinRegistry::with_options(enable_crypto)),
            rules: self.rules,
            stats: ReasonerStats::default(),
            proof: if generate_proof { Some(Proof::default()) } else { None },
            tabling: TablingState::default(),
            derived_signatures: HashSet::new(),
        }
    }

    /// Build and run the reasoner on a store
    pub fn run(self, store: &mut Store) -> ReasonerStats {
        let mut reasoner = self.build();
        reasoner.run(store);
        reasoner.stats
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_rule() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        ));

        // Rule: Human(?x) => Mortal(?x)
        let rule = Rule::new(
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

        let mut reasoner = Reasoner::new();
        reasoner.add_rule(rule);
        reasoner.run(&mut store);

        // Check that socrates is now mortal
        let mortal = Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Mortal"),
        );
        assert!(store.contains(&mortal));
    }

    #[test]
    fn test_transitive_closure() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/subClassOf"),
            Term::uri("http://example.org/b"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/b"),
            Term::uri("http://example.org/subClassOf"),
            Term::uri("http://example.org/c"),
        ));

        // Rule: subClassOf(?x, ?y) ^ subClassOf(?y, ?z) => subClassOf(?x, ?z)
        let rule = Rule::new(
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri("http://example.org/subClassOf"),
                    Term::universal("y"),
                ),
                Triple::new(
                    Term::universal("y"),
                    Term::uri("http://example.org/subClassOf"),
                    Term::universal("z"),
                ),
            ],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/subClassOf"),
                Term::universal("z"),
            )],
        );

        let mut reasoner = Reasoner::new();
        reasoner.add_rule(rule);
        reasoner.run(&mut store);

        // Check transitive inference
        let result = Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/subClassOf"),
            Term::uri("http://example.org/c"),
        );
        assert!(store.contains(&result));
    }
}
