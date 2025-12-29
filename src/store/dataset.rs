//! RDF Dataset Implementation
//!
//! An RDF Dataset consists of a default graph and zero or more named graphs.
//! This module provides the `Dataset` struct for managing multiple named graphs.
//!
//! # SPARQL 1.1 Dataset
//!
//! According to SPARQL 1.1, a dataset consists of:
//! - One default graph (unnamed)
//! - Zero or more named graphs (each identified by an IRI)
//!
//! # Example
//!
//! ```ignore
//! use cwm::store::{Dataset, Store};
//! use cwm::term::{Term, Triple};
//!
//! let mut dataset = Dataset::new();
//!
//! // Add to default graph
//! dataset.default_graph_mut().add(triple1);
//!
//! // Add to named graph
//! let graph_name = Term::uri("http://example.org/graph1");
//! dataset.named_graph_mut(&graph_name).add(triple2);
//! ```

use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::fmt;

use crate::term::{Term, Triple};
use super::Store;

/// A quad is a triple with an associated graph name
#[derive(Clone, PartialEq, Eq)]
pub struct Quad {
    pub subject: Term,
    pub predicate: Term,
    pub object: Term,
    /// The graph this quad belongs to. None means the default graph.
    pub graph: Option<Term>,
}

impl Quad {
    /// Create a new quad with an explicit graph
    pub fn new(subject: Term, predicate: Term, object: Term, graph: Option<Term>) -> Self {
        Quad { subject, predicate, object, graph }
    }

    /// Create a quad in the default graph
    pub fn in_default(subject: Term, predicate: Term, object: Term) -> Self {
        Quad { subject, predicate, object, graph: None }
    }

    /// Create a quad in a named graph
    pub fn in_graph(subject: Term, predicate: Term, object: Term, graph: Term) -> Self {
        Quad { subject, predicate, object, graph: Some(graph) }
    }

    /// Convert to a triple (dropping graph information)
    pub fn to_triple(&self) -> Triple {
        Triple::new(self.subject.clone(), self.predicate.clone(), self.object.clone())
    }

    /// Create a quad from a triple in the default graph
    pub fn from_triple(triple: Triple) -> Self {
        Quad {
            subject: triple.subject,
            predicate: triple.predicate,
            object: triple.object,
            graph: None,
        }
    }

    /// Create a quad from a triple in a named graph
    pub fn from_triple_in_graph(triple: Triple, graph: Term) -> Self {
        Quad {
            subject: triple.subject,
            predicate: triple.predicate,
            object: triple.object,
            graph: Some(graph),
        }
    }

    /// Check if this quad is in the default graph
    pub fn is_default_graph(&self) -> bool {
        self.graph.is_none()
    }

    /// Check if this quad contains any variables
    pub fn has_variables(&self) -> bool {
        self.subject.is_variable()
            || self.predicate.is_variable()
            || self.object.is_variable()
            || self.graph.as_ref().map(|g| g.is_variable()).unwrap_or(false)
    }

    /// Check if this quad is ground (no variables)
    pub fn is_ground(&self) -> bool {
        !self.has_variables()
    }
}

impl Hash for Quad {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.subject.hash(state);
        self.predicate.hash(state);
        self.object.hash(state);
        self.graph.hash(state);
    }
}

impl fmt::Debug for Quad {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.graph {
            Some(g) => write!(f, "{:?} {:?} {:?} {:?} .", self.subject, self.predicate, self.object, g),
            None => write!(f, "{:?} {:?} {:?} .", self.subject, self.predicate, self.object),
        }
    }
}

impl fmt::Display for Quad {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.graph {
            Some(g) => write!(f, "{} {} {} {} .", self.subject, self.predicate, self.object, g),
            None => write!(f, "{} {} {} .", self.subject, self.predicate, self.object),
        }
    }
}

/// Compute a hash for a graph name (for indexing)
fn graph_hash(graph: &Option<Term>) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    let mut hasher = DefaultHasher::new();
    graph.hash(&mut hasher);
    hasher.finish()
}

/// An RDF Dataset containing a default graph and named graphs
///
/// This implements the SPARQL 1.1 dataset model where:
/// - There is exactly one default graph (possibly empty)
/// - There are zero or more named graphs, each identified by an IRI
#[derive(Clone, Default)]
pub struct Dataset {
    /// The default (unnamed) graph
    default_graph: Store,
    /// Named graphs, keyed by graph IRI hash
    named_graphs: HashMap<u64, (Term, Store)>,
}

impl Dataset {
    /// Create a new empty dataset
    pub fn new() -> Self {
        Dataset {
            default_graph: Store::new(),
            named_graphs: HashMap::new(),
        }
    }

    /// Create a dataset with the given default graph
    pub fn with_default(default_graph: Store) -> Self {
        Dataset {
            default_graph,
            named_graphs: HashMap::new(),
        }
    }

    /// Get a reference to the default graph
    pub fn default_graph(&self) -> &Store {
        &self.default_graph
    }

    /// Get a mutable reference to the default graph
    pub fn default_graph_mut(&mut self) -> &mut Store {
        &mut self.default_graph
    }

    /// Get a reference to a named graph, if it exists
    pub fn named_graph(&self, name: &Term) -> Option<&Store> {
        let hash = graph_hash(&Some(name.clone()));
        self.named_graphs.get(&hash).map(|(_, store)| store)
    }

    /// Get a mutable reference to a named graph, creating it if it doesn't exist
    pub fn named_graph_mut(&mut self, name: &Term) -> &mut Store {
        let hash = graph_hash(&Some(name.clone()));
        &mut self.named_graphs
            .entry(hash)
            .or_insert_with(|| (name.clone(), Store::new()))
            .1
    }

    /// Get or create a named graph
    pub fn get_or_create_graph(&mut self, name: &Term) -> &mut Store {
        self.named_graph_mut(name)
    }

    /// Check if a named graph exists
    pub fn has_graph(&self, name: &Term) -> bool {
        let hash = graph_hash(&Some(name.clone()));
        self.named_graphs.contains_key(&hash)
    }

    /// Remove a named graph
    pub fn remove_graph(&mut self, name: &Term) -> Option<Store> {
        let hash = graph_hash(&Some(name.clone()));
        self.named_graphs.remove(&hash).map(|(_, store)| store)
    }

    /// Get all graph names
    pub fn graph_names(&self) -> Vec<Term> {
        self.named_graphs.values().map(|(name, _)| name.clone()).collect()
    }

    /// Get the number of named graphs
    pub fn named_graph_count(&self) -> usize {
        self.named_graphs.len()
    }

    /// Add a quad to the dataset
    pub fn add_quad(&mut self, quad: Quad) {
        let triple = quad.to_triple();
        match quad.graph {
            None => self.default_graph.add(triple),
            Some(ref graph) => self.named_graph_mut(graph).add(triple),
        }
    }

    /// Add a triple to the default graph
    pub fn add(&mut self, triple: Triple) {
        self.default_graph.add(triple);
    }

    /// Add a triple to a named graph
    pub fn add_to_graph(&mut self, triple: Triple, graph: &Term) {
        self.named_graph_mut(graph).add(triple);
    }

    /// Get total triple count across all graphs
    pub fn total_triple_count(&self) -> usize {
        let default_count = self.default_graph.len();
        let named_count: usize = self.named_graphs.values().map(|(_, s)| s.len()).sum();
        default_count + named_count
    }

    /// Iterate over all quads in the dataset
    pub fn iter_quads(&self) -> impl Iterator<Item = Quad> + '_ {
        // Default graph quads
        let default_quads = self.default_graph.iter().map(|t| Quad::from_triple(t.clone()));

        // Named graph quads
        let named_quads = self.named_graphs.values().flat_map(|(name, store)| {
            let name = name.clone();
            store.iter().map(move |t| Quad::from_triple_in_graph(t.clone(), name.clone()))
        });

        default_quads.chain(named_quads)
    }

    /// Query a specific graph (or default if None)
    pub fn query_graph(&self, graph: Option<&Term>) -> Option<&Store> {
        match graph {
            None => Some(&self.default_graph),
            Some(name) => self.named_graph(name),
        }
    }

    /// Match a quad pattern across specified graphs
    ///
    /// If `graph_pattern` is:
    /// - `None` - match only in the default graph
    /// - `Some(term)` where term is ground - match in that named graph
    /// - `Some(term)` where term is variable - match across all graphs
    pub fn match_quad_pattern(
        &self,
        subject: Option<&Term>,
        predicate: Option<&Term>,
        object: Option<&Term>,
        graph_pattern: Option<&Term>,
    ) -> Vec<(Quad, Option<Term>)> {
        let mut results = Vec::new();

        match graph_pattern {
            None => {
                // Match only default graph
                for triple in self.default_graph.iter() {
                    if Self::matches_triple(triple, subject, predicate, object) {
                        results.push((Quad::from_triple(triple.clone()), None));
                    }
                }
            }
            Some(g) if g.is_ground() => {
                // Match specific named graph
                if let Some(store) = self.named_graph(g) {
                    for triple in store.iter() {
                        if Self::matches_triple(triple, subject, predicate, object) {
                            results.push((Quad::from_triple_in_graph(triple.clone(), g.clone()), Some(g.clone())));
                        }
                    }
                }
            }
            Some(_) => {
                // Graph is variable - match across all named graphs
                for (name, store) in self.named_graphs.values() {
                    for triple in store.iter() {
                        if Self::matches_triple(triple, subject, predicate, object) {
                            results.push((Quad::from_triple_in_graph(triple.clone(), name.clone()), Some(name.clone())));
                        }
                    }
                }
            }
        }

        results
    }

    /// Check if a triple matches the given pattern
    fn matches_triple(
        triple: &Triple,
        subject: Option<&Term>,
        predicate: Option<&Term>,
        object: Option<&Term>,
    ) -> bool {
        let subj_match = subject.map(|s| &triple.subject == s).unwrap_or(true);
        let pred_match = predicate.map(|p| &triple.predicate == p).unwrap_or(true);
        let obj_match = object.map(|o| &triple.object == o).unwrap_or(true);
        subj_match && pred_match && obj_match
    }

    /// Clear all graphs (default and named)
    pub fn clear(&mut self) {
        self.default_graph = Store::new();
        self.named_graphs.clear();
    }

    /// Clear only the default graph
    pub fn clear_default(&mut self) {
        self.default_graph = Store::new();
    }

    /// Clear a specific named graph
    pub fn clear_graph(&mut self, name: &Term) {
        let hash = graph_hash(&Some(name.clone()));
        if let Some((_, store)) = self.named_graphs.get_mut(&hash) {
            *store = Store::new();
        }
    }

    /// Merge another dataset into this one
    pub fn merge(&mut self, other: Dataset) {
        // Merge default graphs
        for triple in other.default_graph.iter() {
            self.default_graph.add(triple.clone());
        }

        // Merge named graphs
        for (_, (name, store)) in other.named_graphs {
            for triple in store.iter() {
                self.add_to_graph(triple.clone(), &name);
            }
        }
    }
}

impl fmt::Debug for Dataset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Dataset {{ default_graph: {} triples, named_graphs: {} }}",
               self.default_graph.len(),
               self.named_graph_count())
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn test_triple(s: &str, p: &str, o: &str) -> Triple {
        Triple::new(
            Term::uri(s),
            Term::uri(p),
            Term::uri(o),
        )
    }

    #[test]
    fn test_quad_creation() {
        let quad = Quad::in_default(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/o"),
        );
        assert!(quad.is_default_graph());
        assert!(quad.is_ground());

        let named_quad = Quad::in_graph(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/o"),
            Term::uri("http://example.org/graph1"),
        );
        assert!(!named_quad.is_default_graph());
        assert!(named_quad.is_ground());
    }

    #[test]
    fn test_dataset_default_graph() {
        let mut dataset = Dataset::new();
        let triple = test_triple("http://ex.org/s", "http://ex.org/p", "http://ex.org/o");

        dataset.add(triple.clone());
        assert_eq!(dataset.default_graph().len(), 1);
        assert_eq!(dataset.total_triple_count(), 1);
    }

    #[test]
    fn test_dataset_named_graph() {
        let mut dataset = Dataset::new();
        let graph_name = Term::uri("http://example.org/graph1");
        let triple = test_triple("http://ex.org/s", "http://ex.org/p", "http://ex.org/o");

        dataset.add_to_graph(triple.clone(), &graph_name);

        assert!(dataset.has_graph(&graph_name));
        assert_eq!(dataset.named_graph(&graph_name).unwrap().len(), 1);
        assert_eq!(dataset.named_graph_count(), 1);
        assert_eq!(dataset.default_graph().len(), 0);
        assert_eq!(dataset.total_triple_count(), 1);
    }

    #[test]
    fn test_dataset_multiple_graphs() {
        let mut dataset = Dataset::new();
        let graph1 = Term::uri("http://example.org/graph1");
        let graph2 = Term::uri("http://example.org/graph2");

        dataset.add(test_triple("http://ex.org/s1", "http://ex.org/p", "http://ex.org/o1"));
        dataset.add_to_graph(test_triple("http://ex.org/s2", "http://ex.org/p", "http://ex.org/o2"), &graph1);
        dataset.add_to_graph(test_triple("http://ex.org/s3", "http://ex.org/p", "http://ex.org/o3"), &graph2);

        assert_eq!(dataset.default_graph().len(), 1);
        assert_eq!(dataset.named_graph_count(), 2);
        assert_eq!(dataset.total_triple_count(), 3);

        let names = dataset.graph_names();
        assert_eq!(names.len(), 2);
    }

    #[test]
    fn test_dataset_add_quad() {
        let mut dataset = Dataset::new();

        let default_quad = Quad::in_default(
            Term::uri("http://ex.org/s1"),
            Term::uri("http://ex.org/p"),
            Term::uri("http://ex.org/o1"),
        );
        dataset.add_quad(default_quad);

        let named_quad = Quad::in_graph(
            Term::uri("http://ex.org/s2"),
            Term::uri("http://ex.org/p"),
            Term::uri("http://ex.org/o2"),
            Term::uri("http://example.org/graph1"),
        );
        dataset.add_quad(named_quad);

        assert_eq!(dataset.default_graph().len(), 1);
        assert_eq!(dataset.named_graph_count(), 1);
        assert_eq!(dataset.total_triple_count(), 2);
    }

    #[test]
    fn test_dataset_iter_quads() {
        let mut dataset = Dataset::new();
        let graph1 = Term::uri("http://example.org/graph1");

        dataset.add(test_triple("http://ex.org/s1", "http://ex.org/p", "http://ex.org/o1"));
        dataset.add_to_graph(test_triple("http://ex.org/s2", "http://ex.org/p", "http://ex.org/o2"), &graph1);

        let quads: Vec<_> = dataset.iter_quads().collect();
        assert_eq!(quads.len(), 2);

        let default_quads: Vec<_> = quads.iter().filter(|q| q.is_default_graph()).collect();
        let named_quads: Vec<_> = quads.iter().filter(|q| !q.is_default_graph()).collect();
        assert_eq!(default_quads.len(), 1);
        assert_eq!(named_quads.len(), 1);
    }

    #[test]
    fn test_dataset_match_quad_pattern() {
        let mut dataset = Dataset::new();
        let graph1 = Term::uri("http://example.org/graph1");
        let pred = Term::uri("http://ex.org/knows");

        dataset.add(Triple::new(
            Term::uri("http://ex.org/alice"),
            pred.clone(),
            Term::uri("http://ex.org/bob"),
        ));
        dataset.add_to_graph(Triple::new(
            Term::uri("http://ex.org/alice"),
            pred.clone(),
            Term::uri("http://ex.org/carol"),
        ), &graph1);

        // Match in default graph only
        let default_matches = dataset.match_quad_pattern(None, Some(&pred), None, None);
        assert_eq!(default_matches.len(), 1);

        // Match in named graph
        let named_matches = dataset.match_quad_pattern(None, Some(&pred), None, Some(&graph1));
        assert_eq!(named_matches.len(), 1);

        // Match across all named graphs (variable graph)
        let var_graph = Term::universal("g");
        let all_named_matches = dataset.match_quad_pattern(None, Some(&pred), None, Some(&var_graph));
        assert_eq!(all_named_matches.len(), 1); // Only named graph matches
    }

    #[test]
    fn test_dataset_remove_graph() {
        let mut dataset = Dataset::new();
        let graph1 = Term::uri("http://example.org/graph1");

        dataset.add_to_graph(test_triple("http://ex.org/s", "http://ex.org/p", "http://ex.org/o"), &graph1);
        assert!(dataset.has_graph(&graph1));

        let removed = dataset.remove_graph(&graph1);
        assert!(removed.is_some());
        assert!(!dataset.has_graph(&graph1));
    }

    #[test]
    fn test_dataset_clear() {
        let mut dataset = Dataset::new();
        let graph1 = Term::uri("http://example.org/graph1");

        dataset.add(test_triple("http://ex.org/s1", "http://ex.org/p", "http://ex.org/o1"));
        dataset.add_to_graph(test_triple("http://ex.org/s2", "http://ex.org/p", "http://ex.org/o2"), &graph1);

        dataset.clear();
        assert_eq!(dataset.total_triple_count(), 0);
        assert_eq!(dataset.named_graph_count(), 0);
    }

    #[test]
    fn test_dataset_merge() {
        let mut dataset1 = Dataset::new();
        let mut dataset2 = Dataset::new();
        let graph1 = Term::uri("http://example.org/graph1");

        dataset1.add(test_triple("http://ex.org/s1", "http://ex.org/p", "http://ex.org/o1"));
        dataset2.add(test_triple("http://ex.org/s2", "http://ex.org/p", "http://ex.org/o2"));
        dataset2.add_to_graph(test_triple("http://ex.org/s3", "http://ex.org/p", "http://ex.org/o3"), &graph1);

        dataset1.merge(dataset2);
        assert_eq!(dataset1.default_graph().len(), 2);
        assert_eq!(dataset1.named_graph_count(), 1);
        assert_eq!(dataset1.total_triple_count(), 3);
    }

    #[test]
    fn test_quad_from_triple() {
        let triple = test_triple("http://ex.org/s", "http://ex.org/p", "http://ex.org/o");
        let quad = Quad::from_triple(triple.clone());

        assert!(quad.is_default_graph());
        assert_eq!(quad.to_triple(), triple);
    }
}
