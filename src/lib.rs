//! CWM - Closed World Machine
//!
//! A Rust implementation of the cwm N3 reasoner and RDF processor.
//!
//! # Architecture
//!
//! The crate is organized around core trait abstractions that enable extensibility:
//!
//! - [`core::BuiltinPredicate`] - Interface for built-in predicate implementations
//! - [`core::PredicateRegistry`] - Registry of built-in predicates
//! - [`core::TripleStore`] - Abstraction for triple storage backends
//! - [`core::InferenceEngine`] - Interface for reasoning strategies
//!
//! # Features
//!
//! - Parse RDF/N3 (Notation3) syntax
//! - Store and query RDF triples
//! - Forward-chaining inference with N3 rules
//! - 266+ built-in predicates for math, string, list, crypto, time operations
//! - Bidirectional constraint solving
//! - Tabling/memoization for cycle prevention
//! - Stratification analysis for negation
//! - Proof generation with W3C SWAP reason vocabulary
//!
//! # Example
//!
//! ```rust,ignore
//! use cwm::{Store, Reasoner, Rule, Term, Triple};
//!
//! let mut store = Store::new();
//! store.add(Triple::new(
//!     Term::uri("http://example.org/socrates"),
//!     Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
//!     Term::uri("http://example.org/Human"),
//! ));
//!
//! let rule = Rule::new(
//!     vec![Triple::new(
//!         Term::universal("x"),
//!         Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
//!         Term::uri("http://example.org/Human"),
//!     )],
//!     vec![Triple::new(
//!         Term::universal("x"),
//!         Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
//!         Term::uri("http://example.org/Mortal"),
//!     )],
//! );
//!
//! let mut reasoner = Reasoner::new();
//! reasoner.add_rule(rule);
//! reasoner.run(&mut store);
//! ```

// Core abstractions (Phase 1 of refactoring)
pub mod core;
pub mod term;
pub mod parser;
pub mod store;
pub mod reasoner;
pub mod reasoning;
pub mod builtins;
pub mod sparql;

// Re-export core traits and types
pub use crate::core::{
    EvalResult, BuiltinPredicate, PredicateRegistry, MutableRegistry, BuiltinNamespace,
    TripleStore, TripleStoreExt, PatternMatcher, InferenceEngine, InferenceResult, InferenceConfig,
    ReasonerHook, Unifier, UnifyResult,
    term_variables, triple_variables,
    ns,
};

// Re-export term types
pub use term::{Term, Triple, Bindings, Uri, Literal, Datatype, BlankNode, Variable, List, FormulaRef};

// Re-export parser types
pub use parser::{parse, ParseResult, ParseError, ParserState, Formula, N3Parser};

// Re-export store types
pub use store::Store;

// Re-export reasoner types
pub use reasoner::{Reasoner, ReasonerBuilder, Rule, ReasonerConfig, ReasonerStats, Proof, ProofStep, TablingState, StratificationResult};

// Re-export builtins (legacy compatibility)
pub use builtins::{BuiltinRegistry, BuiltinResult};

// Re-export SPARQL types
pub use sparql::{execute_sparql, QueryResult, format_results_xml, format_results_json};

// Re-export reasoning strategies
pub use reasoning::{ReasoningStrategy, StrategyConfig, ForwardChaining};
