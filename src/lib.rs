//! CWM - Closed World Machine
//!
//! A Rust implementation of the cwm N3 reasoner and RDF processor.
//!
//! # Features
//!
//! - Parse RDF/N3 (Notation3) syntax
//! - Store and query RDF triples
//! - Forward-chaining inference with N3 rules
//! - Built-in predicates for math, string, list operations
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

pub mod term;
pub mod parser;
pub mod store;
pub mod reasoner;
pub mod builtins;
pub mod sparql;

// Re-export main types
pub use term::{Term, Triple, Bindings, Uri, Literal, Datatype, BlankNode, Variable, List, FormulaRef};
pub use parser::{parse, ParseResult, ParseError, ParserState, Formula, N3Parser};
pub use store::Store;
pub use reasoner::{Reasoner, Rule, ReasonerConfig, ReasonerStats, Proof, ProofStep, TablingState};
pub use builtins::{BuiltinRegistry, BuiltinResult};
pub use sparql::{execute_sparql, QueryResult, format_results_xml, format_results_json};
