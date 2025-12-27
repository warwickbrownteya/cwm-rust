//! Core abstractions and common definitions for cwm-rust
//!
//! This module provides:
//! - `traits`: Core trait abstractions for extensibility (BuiltinPredicate, TripleStore, etc.)
//! - `namespaces`: Standard RDF/N3 namespace URI constants

pub mod traits;
pub mod namespaces;

pub use traits::*;
pub use namespaces::ns;
