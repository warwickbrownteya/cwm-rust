//! Forward-chaining reasoner for N3 rules
//!
//! Implements the core inference algorithm of cwm:
//! - Pattern matching with unification
//! - Forward chaining over rules
//! - Built-in predicate evaluation

use crate::term::{Term, Triple, Variable, Bindings, substitute_triple};
use crate::store::Store;
use crate::builtins::BuiltinRegistry;

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
}

impl Default for ReasonerConfig {
    fn default() -> Self {
        ReasonerConfig {
            max_steps: 10000,
            recursive: true,
            filter: false,
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
        }
    }

    /// Create a reasoner with custom configuration
    pub fn with_config(config: ReasonerConfig) -> Self {
        Reasoner {
            config,
            builtins: BuiltinRegistry::new(),
            rules: Vec::new(),
            stats: ReasonerStats::default(),
        }
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
        let mut new_triples = Vec::new();

        for rule in &self.rules {
            // Find all matches for the antecedent
            let bindings_list = self.match_antecedent(store, &rule.antecedent);

            for bindings in bindings_list {
                // Generate consequent triples
                for pattern in &rule.consequent {
                    let triple = substitute_triple(pattern, &bindings);

                    // Only add ground triples that aren't already in the store
                    if triple.is_ground() && !store.contains(&triple) {
                        new_triples.push(triple);
                        self.stats.rules_fired += 1;
                    }
                }
            }
        }

        let count = new_triples.len();
        self.stats.triples_derived += count;

        for triple in new_triples {
            store.add(triple);
        }

        count
    }

    /// Match all antecedent patterns against the store
    fn match_antecedent(&self, store: &Store, patterns: &[Triple]) -> Vec<Bindings> {
        if patterns.is_empty() {
            return vec![Bindings::default()];
        }

        let first = &patterns[0];
        let rest = &patterns[1..];

        // Check if this is a built-in predicate
        if let Some(builtin_results) = self.try_builtin_with_bindings(store, first, &Bindings::default()) {
            // Handle built-in evaluation
            let mut results = Vec::new();
            for bindings in builtin_results {
                // Continue matching with remaining patterns
                let remaining = self.match_with_bindings(store, rest, bindings);
                results.extend(remaining);
            }
            return results;
        }

        // Regular pattern matching
        let first_matches = store.match_pattern(first);

        let mut results = Vec::new();
        for bindings in first_matches {
            // Apply bindings to remaining patterns and continue matching
            let remaining = self.match_with_bindings(store, rest, bindings);
            results.extend(remaining);
        }

        results
    }

    /// Continue matching with partial bindings
    fn match_with_bindings(&self, store: &Store, patterns: &[Triple], bindings: Bindings) -> Vec<Bindings> {
        if patterns.is_empty() {
            return vec![bindings];
        }

        // Substitute bindings into the first pattern
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
                let remaining = self.match_with_bindings(store, rest, merged);
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
            let remaining = self.match_with_bindings(store, rest, merged);
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

impl Default for Reasoner {
    fn default() -> Self {
        Self::new()
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
