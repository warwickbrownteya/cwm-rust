//! Core trait abstractions for cwm-rust extensibility
//!
//! This module defines the fundamental traits that enable:
//! - Pluggable builtin predicates
//! - Swappable triple store backends
//! - Alternative reasoning strategies
//! - Observable inference hooks
//!
//! # Design Principles
//!
//! 1. **Dependency Inversion**: High-level modules depend on abstractions, not concretions
//! 2. **Interface Segregation**: Focused, single-purpose traits
//! 3. **Open/Closed**: Extend via implementation, not modification
//! 4. **Composition**: Combine simple traits to build complex behavior

use crate::term::{Term, Triple, Bindings, Variable};
use std::fmt::Debug;

// ============================================================================
// Builtin Predicate Evaluation
// ============================================================================

/// Result of evaluating a built-in predicate
///
/// Built-ins can succeed (binding variables), fail (no match), need more bindings,
/// or produce multiple solutions.
#[derive(Clone, Debug)]
pub enum EvalResult {
    /// Predicate succeeded with extended/verified bindings
    Success(Bindings),
    /// Predicate definitively failed (no possible match)
    Failure,
    /// Predicate cannot be evaluated yet (needs more ground terms)
    NotReady,
    /// Predicate produced multiple solutions (for generators like list:member)
    MultiSuccess(Vec<Bindings>),
}

impl EvalResult {
    /// Check if this result is a success (single or multi)
    pub fn is_success(&self) -> bool {
        matches!(self, EvalResult::Success(_) | EvalResult::MultiSuccess(_))
    }

    /// Check if this result needs more bindings
    pub fn is_not_ready(&self) -> bool {
        matches!(self, EvalResult::NotReady)
    }

    /// Check if this result is a failure
    pub fn is_failure(&self) -> bool {
        matches!(self, EvalResult::Failure)
    }

    /// Get bindings if this is a single success
    pub fn bindings(&self) -> Option<&Bindings> {
        match self {
            EvalResult::Success(b) => Some(b),
            _ => None,
        }
    }

    /// Get all bindings (works for both single and multi success)
    pub fn all_bindings(&self) -> Vec<Bindings> {
        match self {
            EvalResult::Success(b) => vec![b.clone()],
            EvalResult::MultiSuccess(bs) => bs.clone(),
            _ => vec![],
        }
    }
}

/// A built-in predicate evaluator
///
/// Implementations provide the evaluation logic for specific predicate URIs.
/// Predicates receive the subject, object, and current variable bindings,
/// returning an `EvalResult` indicating success, failure, or need for more bindings.
///
/// # Example
///
/// ```ignore
/// struct MathSum;
///
/// impl BuiltinPredicate for MathSum {
///     fn uri(&self) -> &str { "http://www.w3.org/2000/10/swap/math#sum" }
///
///     fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
///         // (a b) math:sum c means a + b = c
///         // Can solve for any one unknown
///     }
///
///     fn is_bidirectional(&self) -> bool { true }
/// }
/// ```
pub trait BuiltinPredicate: Send + Sync + Debug {
    /// The full URI of this predicate (e.g., "http://www.w3.org/2000/10/swap/math#sum")
    fn uri(&self) -> &str;

    /// Evaluate the predicate with given subject, object, and current bindings
    ///
    /// Returns:
    /// - `Success(bindings)` if the predicate holds, possibly with new bindings
    /// - `Failure` if the predicate definitely does not hold
    /// - `NotReady` if evaluation needs more ground terms
    /// - `MultiSuccess(bindings_list)` for predicates that generate multiple solutions
    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult;

    /// Whether this predicate supports inverse/backward evaluation
    ///
    /// Bidirectional predicates can solve for unknown variables in either
    /// the subject or object position. For example, math:sum can compute
    /// missing operands given the result.
    fn is_bidirectional(&self) -> bool {
        false
    }

    /// Whether this predicate is a generator (can produce multiple bindings)
    ///
    /// Generator predicates may return MultiSuccess with multiple binding sets.
    /// Examples include list:member, string:matches, etc.
    fn is_generator(&self) -> bool {
        false
    }

    /// Minimum number of ground terms required for evaluation
    ///
    /// 0 = can evaluate with all variables (rare)
    /// 1 = needs at least one ground term
    /// 2 = needs both subject and object ground
    fn required_groundedness(&self) -> u8 {
        1
    }

    /// Human-readable description of the predicate's semantics
    fn description(&self) -> &str {
        ""
    }

    /// Short local name (e.g., "sum" from math:sum)
    fn local_name(&self) -> &str {
        self.uri()
            .rsplit_once('#')
            .or_else(|| self.uri().rsplit_once('/'))
            .map(|(_, name)| name)
            .unwrap_or(self.uri())
    }
}

/// Registry of built-in predicates
///
/// Provides lookup and evaluation of registered predicates by URI.
pub trait PredicateRegistry: Send + Sync {
    /// Check if a predicate URI is registered
    fn is_registered(&self, uri: &str) -> bool;

    /// Get a predicate by URI
    fn get(&self, uri: &str) -> Option<&dyn BuiltinPredicate>;

    /// Evaluate a predicate by URI (convenience method)
    fn evaluate(&self, uri: &str, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        match self.get(uri) {
            Some(pred) => pred.evaluate(subject, object, bindings),
            None => EvalResult::NotReady,
        }
    }

    /// List all registered predicate URIs
    fn predicates(&self) -> Vec<&str>;

    /// Count of registered predicates
    fn len(&self) -> usize {
        self.predicates().len()
    }

    /// Check if registry is empty
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// Mutable registry for adding predicates
pub trait MutableRegistry: PredicateRegistry {
    /// Register a predicate
    fn register(&mut self, predicate: Box<dyn BuiltinPredicate>);

    /// Unregister a predicate by URI
    fn unregister(&mut self, uri: &str) -> bool;
}

/// A namespace of related predicates
///
/// Implementations group predicates by their namespace (e.g., math:, string:).
pub trait BuiltinNamespace: Send + Sync {
    /// Namespace URI prefix (e.g., "http://www.w3.org/2000/10/swap/math#")
    fn prefix(&self) -> &str;

    /// Human-readable name for this namespace
    fn name(&self) -> &str;

    /// Register all predicates from this namespace into a registry
    fn register(&self, registry: &mut dyn MutableRegistry);

    /// Number of predicates in this namespace
    fn predicate_count(&self) -> usize;
}

// ============================================================================
// Triple Store Abstraction
// ============================================================================

/// A triple store abstraction for storing and querying RDF triples
///
/// Implementations may vary in indexing strategy, persistence, or query optimization.
/// This trait is dyn-compatible for use with trait objects.
pub trait TripleStore: Send + Sync {
    /// Add a triple to the store
    fn add(&mut self, triple: Triple);

    /// Add multiple triples to the store (boxed iterator for dyn-compatibility)
    fn add_all(&mut self, triples: Box<dyn Iterator<Item = Triple> + '_>) {
        for triple in triples {
            self.add(triple);
        }
    }

    /// Remove a triple from the store
    fn remove(&mut self, triple: &Triple) -> bool;

    /// Check if a triple exists in the store
    fn contains(&self, triple: &Triple) -> bool;

    /// Get the number of triples
    fn len(&self) -> usize;

    /// Check if the store is empty
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Iterate over all triples
    fn iter(&self) -> Box<dyn Iterator<Item = &Triple> + '_>;

    /// Clear all triples
    fn clear(&mut self);

    /// Clone all triples to a Vec
    fn to_vec(&self) -> Vec<Triple> {
        self.iter().cloned().collect()
    }
}

/// Extension trait for convenience methods on TripleStore
///
/// This provides ergonomic methods that can't be in the main trait
/// due to dyn-compatibility requirements.
pub trait TripleStoreExt: TripleStore {
    /// Add multiple triples from any iterator
    fn add_iter<I: IntoIterator<Item = Triple>>(&mut self, triples: I) {
        for triple in triples {
            self.add(triple);
        }
    }

    /// Add triples from a slice
    fn add_slice(&mut self, triples: &[Triple]) {
        for triple in triples {
            self.add(triple.clone());
        }
    }
}

// Blanket implementation for all TripleStore implementors
impl<T: TripleStore + ?Sized> TripleStoreExt for T {}

/// Pattern matching capability for triple stores
///
/// Separated from TripleStore to allow different matching implementations.
pub trait PatternMatcher: Send + Sync {
    /// Match a pattern against a store, returning all valid bindings
    ///
    /// Variables in the pattern are matched against concrete terms in the store.
    fn match_pattern(&self, store: &dyn TripleStore, pattern: &Triple) -> Vec<Bindings>;

    /// Match multiple patterns conjunctively (AND)
    ///
    /// Returns bindings that satisfy all patterns simultaneously.
    fn match_patterns(&self, store: &dyn TripleStore, patterns: &[Triple]) -> Vec<Bindings>;

    /// Match patterns with initial bindings
    fn match_patterns_with(
        &self,
        store: &dyn TripleStore,
        patterns: &[Triple],
        initial: &Bindings,
    ) -> Vec<Bindings>;
}

// ============================================================================
// Inference Engine Abstraction
// ============================================================================

/// Result of running inference
#[derive(Clone, Debug, Default)]
pub struct InferenceResult {
    /// Number of inference steps executed
    pub steps: usize,
    /// Number of rule firings
    pub rules_fired: usize,
    /// Number of new triples derived
    pub triples_derived: usize,
    /// Whether inference reached a fixed point
    pub converged: bool,
    /// Whether inference was stopped early (max steps, etc.)
    pub stopped_early: bool,
}

/// Configuration for inference
#[derive(Clone, Debug)]
pub struct InferenceConfig {
    /// Maximum inference steps (0 = unlimited)
    pub max_steps: usize,
    /// Enable tabling/memoization to prevent infinite loops
    pub enable_tabling: bool,
    /// Enable stratification analysis for negation
    pub enable_stratification: bool,
    /// Generate proof trace
    pub generate_proof: bool,
    /// Apply rule recursively in each step
    pub recursive: bool,
}

impl Default for InferenceConfig {
    fn default() -> Self {
        InferenceConfig {
            max_steps: 100,
            enable_tabling: true,
            enable_stratification: true,
            generate_proof: false,
            recursive: true,
        }
    }
}

/// An inference engine abstraction
///
/// Implementations provide different reasoning strategies (forward, backward, hybrid).
pub trait InferenceEngine: Send + Sync {
    /// Name of this inference strategy
    fn name(&self) -> &str;

    /// Run inference on a store with given rules
    fn infer(
        &mut self,
        store: &mut dyn TripleStore,
        rules: &[crate::reasoner::Rule],
        registry: &dyn PredicateRegistry,
        config: &InferenceConfig,
    ) -> InferenceResult;

    /// Query mode: find bindings satisfying a goal pattern without modifying store
    fn query(
        &self,
        _store: &dyn TripleStore,
        _goal: &[Triple],
        _registry: &dyn PredicateRegistry,
    ) -> Vec<Bindings> {
        // Default: no query support
        vec![]
    }

    /// Check if this engine supports query mode
    fn supports_query(&self) -> bool {
        false
    }
}

// ============================================================================
// Reasoner Hooks (Observer Pattern)
// ============================================================================

/// Hook for observing and potentially modifying reasoning events
///
/// Hooks receive callbacks during inference for logging, debugging, or extension.
pub trait ReasonerHook: Send + Sync {
    /// Called when a new triple is derived
    fn on_triple_derived(&self, _triple: &Triple, _rule_index: usize) {}

    /// Called when a rule fires with specific bindings
    fn on_rule_fired(&self, _rule_index: usize, _bindings: &Bindings) {}

    /// Called when a builtin predicate is evaluated
    fn on_builtin_evaluated(&self, _uri: &str, _result: &EvalResult) {}

    /// Called at the end of each inference step
    fn on_step_complete(&self, _step: usize, _derived_count: usize) {}

    /// Called when inference completes
    fn on_inference_complete(&self, _result: &InferenceResult) {}

    /// Called when a cycle is detected (with tabling)
    fn on_cycle_detected(&self, _signature: &str) {}
}

// ============================================================================
// Unification
// ============================================================================

/// Unification result
#[derive(Clone, Debug)]
pub enum UnifyResult {
    /// Terms unify with the given bindings
    Success(Bindings),
    /// Terms do not unify
    Failure,
}

/// Trait for unifying terms
pub trait Unifier: Send + Sync {
    /// Attempt to unify two terms, extending the given bindings
    fn unify(&self, t1: &Term, t2: &Term, bindings: &Bindings) -> UnifyResult;

    /// Attempt to unify two triples
    fn unify_triple(&self, t1: &Triple, t2: &Triple, bindings: &Bindings) -> UnifyResult;
}

// ============================================================================
// Variable Handling
// ============================================================================

/// Extract variables from a term
pub fn term_variables(term: &Term) -> Vec<Variable> {
    match term {
        Term::Variable(v) => vec![v.clone()],
        Term::List(l) => l.to_vec().iter().flat_map(term_variables).collect(),
        Term::Formula(f) => f
            .triples()
            .iter()
            .flat_map(triple_variables)
            .collect(),
        _ => vec![],
    }
}

/// Extract variables from a triple
pub fn triple_variables(triple: &Triple) -> Vec<Variable> {
    let mut vars = term_variables(&triple.subject);
    vars.extend(term_variables(&triple.predicate));
    vars.extend(term_variables(&triple.object));
    vars
}

// ============================================================================
// Convenience Implementations
// ============================================================================

impl From<bool> for EvalResult {
    fn from(b: bool) -> Self {
        if b {
            EvalResult::Success(Bindings::default())
        } else {
            EvalResult::Failure
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_result_helpers() {
        let success = EvalResult::Success(Bindings::default());
        assert!(success.is_success());
        assert!(!success.is_failure());
        assert!(!success.is_not_ready());

        let failure = EvalResult::Failure;
        assert!(failure.is_failure());
        assert!(!failure.is_success());

        let not_ready = EvalResult::NotReady;
        assert!(not_ready.is_not_ready());
    }

    #[test]
    fn test_eval_result_from_bool() {
        let t: EvalResult = true.into();
        assert!(t.is_success());

        let f: EvalResult = false.into();
        assert!(f.is_failure());
    }

    #[test]
    fn test_inference_config_default() {
        let config = InferenceConfig::default();
        assert_eq!(config.max_steps, 100);
        assert!(config.enable_tabling);
        assert!(config.enable_stratification);
        assert!(!config.generate_proof);
    }
}
