//! Reasoning strategy trait and configuration

use crate::term::{Triple, Bindings};
use crate::store::Store;
use crate::builtins::BuiltinRegistry;
use crate::reasoner::Rule;

/// Configuration for reasoning strategies
#[derive(Clone, Debug)]
pub struct StrategyConfig {
    /// Maximum number of inference steps (0 = unlimited)
    pub max_steps: usize,
    /// Enable tabling/memoization for cycle detection
    pub enable_tabling: bool,
    /// Generate proof trace
    pub generate_proof: bool,
    /// Apply rules recursively within each step
    pub recursive: bool,
    /// Filter mode (only show derived triples matching query patterns)
    pub filter: bool,
}

impl Default for StrategyConfig {
    fn default() -> Self {
        StrategyConfig {
            max_steps: 10000,
            enable_tabling: true,
            generate_proof: false,
            recursive: true,
            filter: false,
        }
    }
}

/// Result of running inference
#[derive(Clone, Debug, Default)]
pub struct InferenceStats {
    /// Number of inference steps executed
    pub steps: usize,
    /// Number of rule firings
    pub rules_fired: usize,
    /// Number of new triples derived
    pub triples_derived: usize,
    /// Number of builtin evaluations
    pub builtins_evaluated: usize,
    /// Whether inference reached a fixed point
    pub converged: bool,
    /// Number of tabling cache hits
    pub cache_hits: usize,
    /// Number of cycles detected
    pub cycles_detected: usize,
}

/// Abstract reasoning strategy
///
/// Implementations provide different inference algorithms:
/// - Forward chaining: bottom-up, data-driven
/// - Backward chaining: top-down, goal-directed
/// - Hybrid: combining both approaches
pub trait ReasoningStrategy: Send + Sync {
    /// Name of this strategy
    fn name(&self) -> &str;

    /// Run inference on a store with given rules
    ///
    /// # Arguments
    /// * `store` - The triple store to reason over
    /// * `rules` - The inference rules to apply
    /// * `builtins` - Registry of built-in predicates
    /// * `config` - Strategy configuration
    ///
    /// # Returns
    /// Statistics about the inference run
    fn infer(
        &mut self,
        store: &mut Store,
        rules: &[Rule],
        builtins: &BuiltinRegistry,
        config: &StrategyConfig,
    ) -> InferenceStats;

    /// Query mode: find bindings satisfying goal patterns
    ///
    /// Default implementation returns empty (no query support).
    /// Backward chaining strategies override this.
    fn query(
        &self,
        _store: &Store,
        _goals: &[Triple],
        _rules: &[Rule],
        _builtins: &BuiltinRegistry,
    ) -> Vec<Bindings> {
        vec![]
    }

    /// Check if this strategy supports query mode
    fn supports_query(&self) -> bool {
        false
    }

    /// Reset any internal state (e.g., caches)
    fn reset(&mut self) {}
}
