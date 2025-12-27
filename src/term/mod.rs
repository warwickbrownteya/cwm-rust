//! RDF/N3 Term representations
//!
//! This module defines the core data types for representing RDF and N3 terms:
//! - URIs (named nodes)
//! - Literals (with optional datatype or language tag)
//! - Blank nodes (anonymous nodes)
//! - Lists (RDF collections)
//! - Variables (universally or existentially quantified)
//! - Formulas (quoted graphs / N3 cited formulas)

use std::fmt;
use std::hash::Hash;
use std::sync::Arc;
use fnv::FnvHashMap;

pub mod uri;
mod literal;
mod blank;
mod variable;
mod list;

pub use uri::Uri;
pub use literal::{Literal, Datatype};
pub use blank::BlankNode;
pub use variable::{Variable, Quantification};
pub use list::List;

/// A term in RDF/N3
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Term {
    /// A URI reference (named node)
    Uri(Arc<Uri>),
    /// A literal value
    Literal(Arc<Literal>),
    /// A blank node (anonymous)
    BlankNode(BlankNode),
    /// A variable (for patterns/rules)
    Variable(Variable),
    /// An RDF list
    List(Arc<List>),
    /// A quoted formula (N3 extension)
    Formula(FormulaRef),
}

/// A formula (quoted graph) containing triples
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FormulaRef {
    /// Unique ID for this formula
    pub(crate) id: u64,
    /// Triples contained in this formula
    pub(crate) triples: Arc<Vec<Triple>>,
}

impl FormulaRef {
    /// Create a new formula with the given ID and triples
    pub fn new(id: u64, triples: Vec<Triple>) -> Self {
        FormulaRef { id, triples: Arc::new(triples) }
    }

    /// Get the formula ID
    pub fn id(&self) -> u64 {
        self.id
    }

    /// Get the triples in this formula
    pub fn triples(&self) -> &[Triple] {
        &self.triples
    }
}

impl Term {
    /// Create a URI term
    pub fn uri(s: impl Into<String>) -> Self {
        Term::Uri(Arc::new(Uri::new(s.into())))
    }

    /// Create a plain literal
    pub fn literal(s: impl Into<String>) -> Self {
        Term::Literal(Arc::new(Literal::plain(s.into())))
    }

    /// Create a typed literal
    pub fn typed_literal(value: impl Into<String>, datatype: impl Into<String>) -> Self {
        Term::Literal(Arc::new(Literal::typed(value.into(), datatype.into())))
    }

    /// Create a language-tagged literal
    pub fn lang_literal(value: impl Into<String>, lang: impl Into<String>) -> Self {
        Term::Literal(Arc::new(Literal::with_language(value.into(), lang.into())))
    }

    /// Create a blank node with a label
    pub fn blank(label: impl Into<String>) -> Self {
        Term::BlankNode(BlankNode::labeled(label.into()))
    }

    /// Create a fresh blank node
    pub fn fresh_blank() -> Self {
        Term::BlankNode(BlankNode::fresh())
    }

    /// Create a universal variable
    pub fn universal(name: impl Into<String>) -> Self {
        Term::Variable(Variable::universal(name.into()))
    }

    /// Create an existential variable
    pub fn existential(name: impl Into<String>) -> Self {
        Term::Variable(Variable::existential(name.into()))
    }

    /// Create an empty list
    pub fn nil() -> Self {
        Term::List(Arc::new(List::Nil))
    }

    /// Create a list from terms
    pub fn list(items: Vec<Term>) -> Self {
        Term::List(Arc::new(List::from_vec(items)))
    }

    /// Check if this term is a variable
    pub fn is_variable(&self) -> bool {
        matches!(self, Term::Variable(_))
    }

    /// Check if this term is ground (contains no variables)
    pub fn is_ground(&self) -> bool {
        match self {
            Term::Uri(_) | Term::Literal(_) | Term::BlankNode(_) => true,
            Term::Variable(_) => false,
            Term::List(l) => l.is_ground(),
            Term::Formula(_) => true, // Formulas are treated as ground for matching
        }
    }

    /// Get the URI if this is a URI term
    pub fn as_uri(&self) -> Option<&Uri> {
        match self {
            Term::Uri(u) => Some(u),
            _ => None,
        }
    }

    /// Get the literal if this is a literal term
    pub fn as_literal(&self) -> Option<&Literal> {
        match self {
            Term::Literal(l) => Some(l),
            _ => None,
        }
    }
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Uri(u) => write!(f, "<{}>", u.as_str()),
            Term::Literal(l) => write!(f, "{:?}", l),
            Term::BlankNode(b) => write!(f, "{:?}", b),
            Term::Variable(v) => write!(f, "{:?}", v),
            Term::List(l) => write!(f, "{:?}", l),
            Term::Formula(r) => write!(f, "{{formula:{}}}", r.id),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Uri(u) => write!(f, "<{}>", u.as_str()),
            Term::Literal(l) => write!(f, "{}", l),
            Term::BlankNode(b) => write!(f, "{}", b),
            Term::Variable(v) => write!(f, "{}", v),
            Term::List(l) => write!(f, "{}", l),
            Term::Formula(r) => {
                write!(f, "{{ ")?;
                for (i, triple) in r.triples.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{} {} {}", triple.subject, triple.predicate, triple.object)?;
                    if i < r.triples.len() - 1 {
                        write!(f, " .")?;
                    }
                }
                write!(f, " }}")
            }
        }
    }
}

/// A triple (statement) in RDF
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Triple {
    pub subject: Term,
    pub predicate: Term,
    pub object: Term,
}

impl Triple {
    pub fn new(subject: Term, predicate: Term, object: Term) -> Self {
        Triple { subject, predicate, object }
    }

    /// Check if this triple contains any variables
    pub fn has_variables(&self) -> bool {
        self.subject.is_variable() || self.predicate.is_variable() || self.object.is_variable()
    }

    /// Check if this triple is ground (no variables)
    pub fn is_ground(&self) -> bool {
        self.subject.is_ground() && self.predicate.is_ground() && self.object.is_ground()
    }
}

impl fmt::Debug for Triple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {:?} {:?} .", self.subject, self.predicate, self.object)
    }
}

impl fmt::Display for Triple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {} .", self.subject, self.predicate, self.object)
    }
}

/// Bindings from variables to terms
pub type Bindings = FnvHashMap<Variable, Term>;

/// Apply bindings to a term, substituting variables
pub fn substitute(term: &Term, bindings: &Bindings) -> Term {
    match term {
        Term::Variable(v) => {
            bindings.get(v).cloned().unwrap_or_else(|| term.clone())
        }
        Term::List(l) => {
            Term::List(Arc::new(l.substitute(bindings)))
        }
        _ => term.clone(),
    }
}

/// Apply bindings to a triple
pub fn substitute_triple(triple: &Triple, bindings: &Bindings) -> Triple {
    Triple {
        subject: substitute(&triple.subject, bindings),
        predicate: substitute(&triple.predicate, bindings),
        object: substitute(&triple.object, bindings),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_term_creation() {
        let uri = Term::uri("http://example.org/foo");
        assert!(matches!(uri, Term::Uri(_)));

        let lit = Term::literal("hello");
        assert!(matches!(lit, Term::Literal(_)));

        let blank = Term::blank("b1");
        assert!(matches!(blank, Term::BlankNode(_)));

        let var = Term::universal("x");
        assert!(var.is_variable());
    }

    #[test]
    fn test_ground_check() {
        assert!(Term::uri("http://example.org/").is_ground());
        assert!(Term::literal("hello").is_ground());
        assert!(!Term::universal("x").is_ground());
    }

    #[test]
    fn test_triple() {
        let t = Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::literal("o"),
        );
        assert!(t.is_ground());
    }
}
