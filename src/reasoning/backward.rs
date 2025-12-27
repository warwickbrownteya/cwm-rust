//! Backward chaining reasoning strategy
//!
//! Implements top-down, goal-directed query answering:
//! 1. Start with goal patterns
//! 2. Try to match goals against store facts
//! 3. If no match, try to derive via rule consequents
//! 4. Recursively solve antecedent subgoals
//!
//! This is essentially SLD resolution with tabling to prevent infinite loops.

use std::collections::{HashSet, HashMap};

use crate::term::{Term, Triple, Bindings, substitute_triple};
use crate::store::Store;
use crate::builtins::{BuiltinRegistry, BuiltinResult};
use crate::reasoner::Rule;
use super::strategy::{ReasoningStrategy, StrategyConfig, InferenceStats};

/// A goal to be solved
#[derive(Clone, Debug)]
struct Goal {
    /// The pattern to match
    pattern: Triple,
    /// Current variable bindings
    bindings: Bindings,
    /// Depth in the search tree (for limiting)
    depth: usize,
}

/// Backward chaining inference strategy
///
/// This strategy answers queries by working backwards from goals
/// to find facts that satisfy them, using rules when needed.
#[derive(Debug)]
pub struct BackwardChaining {
    /// Maximum recursion depth
    max_depth: usize,
    /// Tabling: memoized goal results
    memo: HashMap<String, Vec<Bindings>>,
    /// Goals currently being computed (cycle detection)
    computing: HashSet<String>,
    /// Statistics
    stats: InferenceStats,
}

impl Default for BackwardChaining {
    fn default() -> Self {
        Self::new()
    }
}

impl BackwardChaining {
    /// Create a new backward chaining strategy
    pub fn new() -> Self {
        BackwardChaining {
            max_depth: 100,
            memo: HashMap::new(),
            computing: HashSet::new(),
            stats: InferenceStats::default(),
        }
    }

    /// Create with custom max depth
    pub fn with_max_depth(max_depth: usize) -> Self {
        BackwardChaining {
            max_depth,
            ..Self::new()
        }
    }

    /// Generate a signature for a goal (for memoization)
    fn goal_signature(pattern: &Triple, bindings: &Bindings) -> String {
        let substituted = substitute_triple(pattern, bindings);
        format!("{:?}", substituted)
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

    /// Resolve a term through bindings chain
    fn resolve(term: &Term, bindings: &Bindings) -> Term {
        match term {
            Term::Variable(var) => {
                if let Some(bound) = bindings.get(var) {
                    Self::resolve(bound, bindings)
                } else {
                    term.clone()
                }
            }
            _ => term.clone(),
        }
    }

    /// Try to unify two terms, extending bindings
    fn unify(pattern: &Term, ground: &Term, bindings: &mut Bindings) -> bool {
        // Resolve both terms through current bindings
        let p = Self::resolve(pattern, bindings);
        let g = Self::resolve(ground, bindings);

        match (&p, &g) {
            // Both are the same variable - already unified
            (Term::Variable(v1), Term::Variable(v2)) if v1 == v2 => true,
            // Pattern is a variable - bind it
            (Term::Variable(var), _) => {
                bindings.insert(var.clone(), g.clone());
                true
            }
            // Ground is a variable - bind it to pattern
            (_, Term::Variable(var)) => {
                bindings.insert(var.clone(), p.clone());
                true
            }
            // Neither is a variable - must be equal
            _ => p == g,
        }
    }

    /// Try to unify a pattern with a triple
    fn unify_triple(pattern: &Triple, triple: &Triple, bindings: &Bindings) -> Option<Bindings> {
        let mut new_bindings = bindings.clone();

        if !Self::unify(&pattern.subject, &triple.subject, &mut new_bindings) {
            return None;
        }
        if !Self::unify(&pattern.predicate, &triple.predicate, &mut new_bindings) {
            return None;
        }
        if !Self::unify(&pattern.object, &triple.object, &mut new_bindings) {
            return None;
        }

        Some(new_bindings)
    }

    /// Try to evaluate a builtin predicate
    fn try_builtin(
        &mut self,
        pattern: &Triple,
        bindings: &Bindings,
        builtins: &BuiltinRegistry,
    ) -> Option<Vec<Bindings>> {
        if let Term::Uri(uri) = &pattern.predicate {
            let uri_str = uri.as_str();
            if builtins.is_builtin(uri_str) {
                self.stats.builtins_evaluated += 1;

                let subject = Self::apply_bindings(&pattern.subject, bindings);
                let object = Self::apply_bindings(&pattern.object, bindings);

                match builtins.evaluate(uri_str, &subject, &object, bindings) {
                    BuiltinResult::Success(new_bindings) => {
                        return Some(vec![new_bindings]);
                    }
                    BuiltinResult::Failure => return Some(vec![]),
                    BuiltinResult::NotReady => return None, // Can't evaluate yet
                }
            }
        }
        None
    }

    /// Solve a single goal, returning all possible bindings
    fn solve_goal(
        &mut self,
        goal: &Goal,
        store: &Store,
        rules: &[Rule],
        builtins: &BuiltinRegistry,
    ) -> Vec<Bindings> {
        // Check depth limit
        if goal.depth > self.max_depth {
            return vec![];
        }

        // Generate signature for memoization
        let sig = Self::goal_signature(&goal.pattern, &goal.bindings);

        // Check memo
        if let Some(results) = self.memo.get(&sig) {
            self.stats.cache_hits += 1;
            return results.clone();
        }

        // Cycle detection
        if self.computing.contains(&sig) {
            self.stats.cycles_detected += 1;
            return vec![];
        }

        self.computing.insert(sig.clone());

        let mut results = Vec::new();

        // Apply current bindings to the pattern
        let pattern = substitute_triple(&goal.pattern, &goal.bindings);

        // Try builtin first
        if let Some(builtin_results) = self.try_builtin(&pattern, &goal.bindings, builtins) {
            results.extend(builtin_results);
        } else {
            // Try matching against store facts
            for triple in store.iter() {
                if let Some(new_bindings) = Self::unify_triple(&pattern, triple, &goal.bindings) {
                    results.push(new_bindings);
                }
            }

            // Try deriving via rules
            for rule in rules {
                // Rename rule variables to avoid capture
                let renamed_rule = self.rename_variables(rule, goal.depth);

                // Try to unify goal with each consequent pattern
                for consequent in &renamed_rule.consequent {
                    if let Some(new_bindings) = Self::unify_triple(&pattern, consequent, &goal.bindings) {
                        // Solve antecedent subgoals
                        let subgoal_results = self.solve_antecedent(
                            &renamed_rule.antecedent,
                            new_bindings,
                            goal.depth + 1,
                            store,
                            rules,
                            builtins,
                        );
                        results.extend(subgoal_results);
                    }
                }
            }
        }

        // Memoize results
        self.memo.insert(sig.clone(), results.clone());
        self.computing.remove(&sig);

        results
    }

    /// Rename variables in a rule to avoid capture
    fn rename_variables(&self, rule: &Rule, depth: usize) -> Rule {
        let suffix = format!("_{}", depth);

        let rename_term = |term: &Term| -> Term {
            match term {
                Term::Variable(var) => {
                    let new_name = format!("{}{}", var.name(), suffix);
                    if var.is_universal() {
                        Term::universal(new_name)
                    } else {
                        Term::existential(new_name)
                    }
                }
                _ => term.clone(),
            }
        };

        let rename_triple = |t: &Triple| -> Triple {
            Triple::new(
                rename_term(&t.subject),
                rename_term(&t.predicate),
                rename_term(&t.object),
            )
        };

        Rule::new(
            rule.antecedent.iter().map(rename_triple).collect(),
            rule.consequent.iter().map(rename_triple).collect(),
        )
    }

    /// Solve a conjunction of antecedent patterns
    fn solve_antecedent(
        &mut self,
        patterns: &[Triple],
        bindings: Bindings,
        depth: usize,
        store: &Store,
        rules: &[Rule],
        builtins: &BuiltinRegistry,
    ) -> Vec<Bindings> {
        if patterns.is_empty() {
            return vec![bindings];
        }

        let first = &patterns[0];
        let rest = &patterns[1..];

        let goal = Goal {
            pattern: first.clone(),
            bindings: bindings.clone(),
            depth,
        };

        let first_results = self.solve_goal(&goal, store, rules, builtins);

        let mut results = Vec::new();
        for new_bindings in first_results {
            let sub_results = self.solve_antecedent(rest, new_bindings, depth, store, rules, builtins);
            results.extend(sub_results);
        }

        results
    }

    /// Answer a query by finding all satisfying bindings
    pub fn answer_query(
        &mut self,
        goals: &[Triple],
        store: &Store,
        rules: &[Rule],
        builtins: &BuiltinRegistry,
    ) -> Vec<Bindings> {
        // Reset state
        self.memo.clear();
        self.computing.clear();
        self.stats = InferenceStats::default();

        // Solve the conjunction of goals
        self.solve_antecedent(goals, Bindings::default(), 0, store, rules, builtins)
    }
}

impl ReasoningStrategy for BackwardChaining {
    fn name(&self) -> &str {
        "backward-chaining"
    }

    fn infer(
        &mut self,
        _store: &mut Store,
        _rules: &[Rule],
        _builtins: &BuiltinRegistry,
        _config: &StrategyConfig,
    ) -> InferenceStats {
        // Backward chaining doesn't derive new triples by default
        // Use query() method instead
        self.stats.clone()
    }

    fn query(
        &self,
        store: &Store,
        goals: &[Triple],
        rules: &[Rule],
        builtins: &BuiltinRegistry,
    ) -> Vec<Bindings> {
        // Create a mutable copy for querying
        let mut bc = BackwardChaining {
            max_depth: self.max_depth,
            memo: HashMap::new(),
            computing: HashSet::new(),
            stats: InferenceStats::default(),
        };
        bc.answer_query(goals, store, rules, builtins)
    }

    fn supports_query(&self) -> bool {
        true
    }

    fn reset(&mut self) {
        self.memo.clear();
        self.computing.clear();
        self.stats = InferenceStats::default();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::term::Term;

    #[test]
    fn test_backward_chaining_facts() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/plato"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        ));

        let builtins = BuiltinRegistry::new();
        let mut bc = BackwardChaining::new();

        // Query: who is Human?
        let goals = vec![Triple::new(
            Term::universal("x"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        )];

        let results = bc.answer_query(&goals, &store, &[], &builtins);

        assert_eq!(results.len(), 2);
    }

    #[test]
    fn test_backward_chaining_rules() {
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
        let mut bc = BackwardChaining::new();

        // Query: who is Mortal? (requires rule application)
        let goals = vec![Triple::new(
            Term::universal("x"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Mortal"),
        )];

        let results = bc.answer_query(&goals, &store, &[rule], &builtins);

        assert_eq!(results.len(), 1);
        // Check that x resolves to socrates (may need to follow binding chain)
        let x_term = Term::universal("x");
        let resolved = BackwardChaining::resolve(&x_term, &results[0]);
        assert_eq!(resolved, Term::uri("http://example.org/socrates"));
    }

    #[test]
    fn test_transitive_query() {
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
        let mut bc = BackwardChaining::new();

        // Query: is a subClassOf c?
        let goals = vec![Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/subClassOf"),
            Term::uri("http://example.org/c"),
        )];

        let results = bc.answer_query(&goals, &store, &[rule], &builtins);

        assert!(!results.is_empty(), "Should find a is subClassOf c");
    }
}
