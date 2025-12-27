//! Forward chaining reasoning strategy
//!
//! Implements bottom-up, data-driven inference:
//! 1. Match rule antecedents against store
//! 2. Evaluate built-in predicates
//! 3. Derive new triples from rule consequents
//! 4. Repeat until fixed point or max steps

use std::collections::HashSet;

use crate::term::{Term, Triple, Bindings, substitute_triple};
use crate::store::Store;
use crate::builtins::{BuiltinRegistry, BuiltinResult};
use crate::reasoner::Rule;
use super::strategy::{ReasoningStrategy, StrategyConfig, InferenceStats};

/// Forward chaining inference strategy
///
/// This is the default strategy, implementing bottom-up reasoning
/// where rules fire based on available data.
#[derive(Debug, Default)]
pub struct ForwardChaining {
    /// Set of derived triple signatures for duplicate detection
    derived_signatures: HashSet<String>,
    /// Statistics for the current run
    stats: InferenceStats,
}

impl ForwardChaining {
    /// Create a new forward chaining strategy
    pub fn new() -> Self {
        Self::default()
    }

    /// Generate a signature for a triple (for duplicate detection)
    fn triple_signature(triple: &Triple) -> String {
        format!("{:?}", triple)
    }

    /// Try to evaluate a builtin predicate, returning updated bindings if successful
    fn try_builtin(
        &mut self,
        triple: &Triple,
        bindings: &Bindings,
        builtins: &BuiltinRegistry,
    ) -> Option<Bindings> {
        // Check if predicate is a builtin
        if let Term::Uri(uri) = &triple.predicate {
            let uri_str = uri.as_str();
            if builtins.is_builtin(uri_str) {
                self.stats.builtins_evaluated += 1;

                // Apply current bindings to subject and object
                let subject = Self::apply_bindings(&triple.subject, bindings);
                let object = Self::apply_bindings(&triple.object, bindings);

                match builtins.evaluate(uri_str, &subject, &object, bindings) {
                    BuiltinResult::Success(new_bindings) => return Some(new_bindings),
                    BuiltinResult::Failure => return None,
                    BuiltinResult::NotReady => {
                        // Try again later when more bindings are available
                        return Some(bindings.clone());
                    }
                }
            }
        }
        None
    }

    /// Apply bindings to a term
    fn apply_bindings(term: &Term, bindings: &Bindings) -> Term {
        match term {
            Term::Variable(var) => {
                bindings.get(var).cloned().unwrap_or_else(|| term.clone())
            }
            _ => term.clone(),
        }
    }

    /// Match rule antecedent patterns against the store
    fn match_antecedent(
        &mut self,
        store: &Store,
        patterns: &[Triple],
        builtins: &BuiltinRegistry,
    ) -> Vec<Bindings> {
        if patterns.is_empty() {
            return vec![Bindings::default()];
        }

        // Start with matches for the first pattern
        let first = &patterns[0];

        // Check if first pattern is a builtin
        let mut results = if let Term::Uri(uri) = &first.predicate {
            if builtins.is_builtin(uri.as_str()) {
                // For builtins, start with empty bindings and try to evaluate
                let empty = Bindings::default();
                match self.try_builtin(first, &empty, builtins) {
                    Some(bindings) => vec![bindings],
                    None => return vec![],
                }
            } else {
                store.match_pattern(first)
            }
        } else {
            store.match_pattern(first)
        };

        // Match remaining patterns
        for pattern in &patterns[1..] {
            let mut new_results = Vec::new();

            for bindings in results {
                // Check if this is a builtin
                if let Term::Uri(uri) = &pattern.predicate {
                    if builtins.is_builtin(uri.as_str()) {
                        if let Some(new_bindings) = self.try_builtin(pattern, &bindings, builtins) {
                            new_results.push(new_bindings);
                        }
                        continue;
                    }
                }

                // Regular pattern matching
                let substituted = substitute_triple(pattern, &bindings);
                for new_bindings in store.match_pattern(&substituted) {
                    let mut merged = bindings.clone();
                    for (var, term) in new_bindings {
                        merged.insert(var, term);
                    }
                    new_results.push(merged);
                }
            }

            results = new_results;
        }

        results
    }

    /// Apply a single rule, returning derived triples
    fn apply_rule(
        &mut self,
        store: &Store,
        rule: &Rule,
        builtins: &BuiltinRegistry,
    ) -> Vec<Triple> {
        let mut derived = Vec::new();

        // Find all bindings that satisfy the antecedent
        let bindings_list = self.match_antecedent(store, &rule.antecedent, builtins);

        for bindings in bindings_list {
            // Generate consequent triples
            for pattern in &rule.consequent {
                let triple = substitute_triple(pattern, &bindings);

                // Only add if ground and new
                if triple.is_ground() {
                    let sig = Self::triple_signature(&triple);
                    if !self.derived_signatures.contains(&sig) && !store.contains(&triple) {
                        self.derived_signatures.insert(sig);
                        derived.push(triple);
                        self.stats.rules_fired += 1;
                    }
                }
            }
        }

        derived
    }

    /// Run one inference step, returning number of new triples
    fn step(
        &mut self,
        store: &mut Store,
        rules: &[Rule],
        builtins: &BuiltinRegistry,
    ) -> usize {
        let mut all_derived = Vec::new();

        for rule in rules {
            let derived = self.apply_rule(store, rule, builtins);
            all_derived.extend(derived);
        }

        let count = all_derived.len();
        for triple in all_derived {
            store.add(triple);
        }

        self.stats.triples_derived += count;
        count
    }
}

impl ReasoningStrategy for ForwardChaining {
    fn name(&self) -> &str {
        "forward-chaining"
    }

    fn infer(
        &mut self,
        store: &mut Store,
        rules: &[Rule],
        builtins: &BuiltinRegistry,
        config: &StrategyConfig,
    ) -> InferenceStats {
        // Reset state
        self.derived_signatures.clear();
        self.stats = InferenceStats::default();

        // Main inference loop
        loop {
            self.stats.steps += 1;

            let derived = self.step(store, rules, builtins);

            if derived == 0 {
                self.stats.converged = true;
                break;
            }

            if config.max_steps > 0 && self.stats.steps >= config.max_steps {
                break;
            }

            // In non-recursive mode, only do one step
            if !config.recursive {
                break;
            }
        }

        self.stats.clone()
    }

    fn reset(&mut self) {
        self.derived_signatures.clear();
        self.stats = InferenceStats::default();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::term::Term;

    #[test]
    fn test_forward_chaining_basic() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        ));

        // Human(?x) => Mortal(?x)
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

        let builtins = BuiltinRegistry::new();
        let config = StrategyConfig::default();

        let mut strategy = ForwardChaining::new();
        let stats = strategy.infer(&mut store, &[rule], &builtins, &config);

        assert!(stats.converged);
        assert!(stats.triples_derived > 0);

        // Check that Socrates is mortal
        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Mortal"),
        )));
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

        // Transitive rule
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

        let builtins = BuiltinRegistry::new();
        let config = StrategyConfig::default();

        let mut strategy = ForwardChaining::new();
        let stats = strategy.infer(&mut store, &[rule], &builtins, &config);

        assert!(stats.converged);

        // Check transitive closure
        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/subClassOf"),
            Term::uri("http://example.org/c"),
        )));
    }
}
