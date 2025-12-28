//! Native theorem prover engines
//!
//! This module implements various theorem proving algorithms for different logics:
//!
//! ## Resolution-based (First-Order Logic)
//! - [`OtterProver`] - Otter's given-clause algorithm
//! - [`Prover9`] - Prover9 with LPO and hints
//! - [`SuperpositionProver`] - Superposition calculus (Vampire/E style)
//!
//! ## Connection-based (First-Order Logic)
//! - [`LeanCop`] - Lean connection prover (clausal)
//! - [`NanoCop`] - Non-clausal connection prover
//!
//! ## Tableau-based
//! - [`TableauProver`] - Analytic tableaux for FOL
//! - [`DlTableau`] - Description Logic tableau (OWL reasoning)
//!
//! ## SAT/SMT Solvers
//! - [`DpllSolver`] - Classic DPLL SAT solver
//! - [`CdclSolver`] - Modern CDCL SAT solver
//! - [`SmtSolver`] - DPLL(T) SMT solver
//!
//! ## Equational Reasoning
//! - [`KnuthBendix`] - Knuth-Bendix completion

// Core components
pub mod term;
pub mod unify;
pub mod clause;
pub mod resolution;

// Resolution-based provers
pub mod otter;
pub mod prover9;
pub mod superposition;

// Connection-based provers
pub mod leancop;
pub mod nanocop;

// Tableau-based provers
pub mod tableau;
pub mod dl_tableau;

// SAT/SMT solvers
pub mod dpll;
pub mod cdcl;
pub mod smt;

// Equational reasoning
pub mod knuth_bendix;

// Re-export core types
pub use term::{FolTerm, Function, Variable as FolVariable, Atom, Predicate};
pub use clause::{Literal, Clause, ClauseSet};
pub use unify::{Substitution, unify, apply_substitution, match_term};
pub use resolution::{resolve, factor};

// Re-export resolution provers
pub use otter::OtterProver;
pub use prover9::{Prover9, Prover9Config};
pub use superposition::{SuperpositionProver, SuperpositionConfig, KboOrdering};

// Re-export connection provers
pub use leancop::{LeanCop, LeanCopConfig, LeanCopResult};
pub use nanocop::{NanoCop, NanoCopConfig, NanoCopResult, NcFormula};

// Re-export tableau provers
pub use tableau::{TableauProver, TableauConfig, TableauResult, Formula as TableauFormula};
pub use dl_tableau::{DlTableau, DlTableauConfig, DlResult, Concept, Individual};

// Re-export SAT/SMT solvers
pub use dpll::{DpllSolver, DpllConfig, Var, Lit, SatClause, SatResult};
pub use cdcl::{CdclSolver, CdclConfig};
pub use smt::{SmtSolver, SmtConfig, SmtResult, SmtTerm, Sort, TheorySolver, EufSolver, LiaSolver};

// Re-export equational reasoning
pub use knuth_bendix::{KnuthBendix, KbConfig, KbResult, RewriteRule, Equation, LpoOrdering};

/// Result of a proof attempt
#[derive(Debug, Clone)]
pub enum ProofResult {
    /// Proof found - the empty clause was derived
    Proved {
        /// The proof steps
        proof: Vec<ProofStep>,
        /// Number of clauses generated
        clauses_generated: usize,
        /// Number of resolution steps
        resolution_steps: usize,
    },
    /// No proof found within resource limits
    Unknown {
        reason: String,
    },
    /// The clause set is satisfiable (model found for finite domain)
    Satisfiable,
}

/// A step in a proof
#[derive(Debug, Clone)]
pub struct ProofStep {
    /// The clause derived
    pub clause: Clause,
    /// How it was derived
    pub derivation: Derivation,
    /// Step number
    pub step_id: usize,
}

/// How a clause was derived
#[derive(Debug, Clone)]
pub enum Derivation {
    /// Input clause (axiom or negated goal)
    Input,
    /// Resolution of two clauses
    Resolution {
        clause1_id: usize,
        clause2_id: usize,
        literal1_idx: usize,
        literal2_idx: usize,
    },
    /// Factoring within a clause
    Factor {
        clause_id: usize,
        literal1_idx: usize,
        literal2_idx: usize,
    },
    /// Paramodulation (equality reasoning)
    Paramodulation {
        from_clause: usize,
        into_clause: usize,
    },
    /// Demodulation (rewriting with equalities)
    Demodulation {
        clause_id: usize,
        demodulator_id: usize,
    },
}

/// Configuration for the prover
#[derive(Debug, Clone)]
pub struct ProverConfig {
    /// Maximum number of clauses to generate
    pub max_clauses: usize,
    /// Maximum number of seconds to run
    pub max_seconds: u64,
    /// Maximum clause weight to keep
    pub max_weight: usize,
    /// Use set of support strategy
    pub set_of_support: bool,
    /// Use ordered resolution
    pub ordered_resolution: bool,
    /// Use hyperresolution
    pub hyperresolution: bool,
    /// Use paramodulation for equality
    pub paramodulation: bool,
    /// Use demodulation
    pub demodulation: bool,
    /// Verbosity level
    pub verbose: bool,
}

impl Default for ProverConfig {
    fn default() -> Self {
        ProverConfig {
            max_clauses: 100000,
            max_seconds: 60,
            max_weight: 100,
            set_of_support: true,
            ordered_resolution: true,
            hyperresolution: false,
            paramodulation: true,
            demodulation: true,
            verbose: false,
        }
    }
}
