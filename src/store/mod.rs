//! RDF/N3 Store (Formula) implementation
//!
//! A store holds a set of triples and supports pattern matching.
//!
//! # Indexing Strategy
//!
//! The store uses SPO (Subject-Predicate-Object) indexes for efficient pattern matching:
//! - O(1) lookup for exact triples
//! - O(log n) to O(1) lookup for patterns with bound terms
//! - Falls back to O(n) scan only when all positions are variables

use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use indexmap::IndexMap;

use crate::term::{Term, Triple, Bindings, FormulaRef, substitute_triple};
use crate::core::TripleStore;

/// Compute a hash for a term (for indexing)
fn term_hash(term: &Term) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    let mut hasher = DefaultHasher::new();
    term.hash(&mut hasher);
    hasher.finish()
}

/// Compute a hash for a triple (for duplicate detection)
fn triple_hash(triple: &Triple) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    let mut hasher = DefaultHasher::new();
    triple.subject.hash(&mut hasher);
    triple.predicate.hash(&mut hasher);
    triple.object.hash(&mut hasher);
    hasher.finish()
}

/// A store of RDF triples (also called a formula or graph)
///
/// Implements the `TripleStore` trait with O(1) duplicate detection
/// and efficient pattern matching using SPO indexes.
#[derive(Clone, Default)]
pub struct Store {
    /// The triples in this store
    triples: Vec<Triple>,
    /// Hash set for O(1) duplicate detection
    triple_hashes: HashSet<u64>,
    /// Index by subject hash -> triple indices
    by_subject: HashMap<u64, Vec<usize>>,
    /// Index by predicate hash -> triple indices
    by_predicate: HashMap<u64, Vec<usize>>,
    /// Index by object hash -> triple indices
    by_object: HashMap<u64, Vec<usize>>,
    /// Next formula ID
    next_formula_id: u64,
    /// Nested formulas
    formulas: IndexMap<u64, Store>,
}

impl Store {
    /// Create a new empty store
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a triple to the store with O(1) duplicate detection and index updates
    pub fn add(&mut self, triple: Triple) {
        let hash = triple_hash(&triple);

        // O(1) duplicate check
        if self.triple_hashes.contains(&hash) {
            // Hash collision possible - verify with exact check
            if self.triples.iter().any(|t| t == &triple) {
                return;
            }
        }

        // Insert into hash set
        self.triple_hashes.insert(hash);

        // Get the index for this triple
        let idx = self.triples.len();

        // Update SPO indexes
        let subj_hash = term_hash(&triple.subject);
        let pred_hash = term_hash(&triple.predicate);
        let obj_hash = term_hash(&triple.object);

        self.by_subject.entry(subj_hash).or_default().push(idx);
        self.by_predicate.entry(pred_hash).or_default().push(idx);
        self.by_object.entry(obj_hash).or_default().push(idx);

        // Add the triple
        self.triples.push(triple);
    }

    /// Add multiple triples
    pub fn add_all(&mut self, triples: impl IntoIterator<Item = Triple>) {
        for triple in triples {
            self.add(triple);
        }
    }

    /// Check if the store contains a triple (O(1) average case)
    pub fn contains(&self, triple: &Triple) -> bool {
        let hash = triple_hash(triple);
        if !self.triple_hashes.contains(&hash) {
            return false;
        }
        // Hash collision possible - verify with exact check
        self.triples.iter().any(|t| t == triple)
    }

    /// Get all triples
    pub fn triples(&self) -> &[Triple] {
        &self.triples
    }

    /// Get the number of triples
    pub fn len(&self) -> usize {
        self.triples.len()
    }

    /// Check if the store is empty
    pub fn is_empty(&self) -> bool {
        self.triples.is_empty()
    }

    /// Match a pattern against the store, returning all bindings
    ///
    /// Uses indexes for efficient lookup when pattern has bound terms:
    /// - If subject is bound, uses by_subject index
    /// - If predicate is bound, uses by_predicate index
    /// - If object is bound, uses by_object index
    /// - Chooses the most selective (smallest candidate set)
    pub fn match_pattern(&self, pattern: &Triple) -> Vec<Bindings> {
        // Get candidate indices from the most selective index
        let candidates = self.get_candidates(pattern);

        let mut results = Vec::new();
        for &idx in &candidates {
            if let Some(triple) = self.triples.get(idx) {
                if let Some(bindings) = self.unify_triple(pattern, triple) {
                    results.push(bindings);
                }
            }
        }

        results
    }

    /// Get candidate triple indices using the most selective index
    fn get_candidates(&self, pattern: &Triple) -> Vec<usize> {
        let subj_bound = !matches!(pattern.subject, Term::Variable(_));
        let pred_bound = !matches!(pattern.predicate, Term::Variable(_));
        let obj_bound = !matches!(pattern.object, Term::Variable(_));

        // If nothing is bound, scan all triples
        if !subj_bound && !pred_bound && !obj_bound {
            return (0..self.triples.len()).collect();
        }

        // Get candidates from each bound index
        let mut candidate_sets: Vec<Option<&Vec<usize>>> = Vec::new();

        if subj_bound {
            let hash = term_hash(&pattern.subject);
            candidate_sets.push(self.by_subject.get(&hash));
        }
        if pred_bound {
            let hash = term_hash(&pattern.predicate);
            candidate_sets.push(self.by_predicate.get(&hash));
        }
        if obj_bound {
            let hash = term_hash(&pattern.object);
            candidate_sets.push(self.by_object.get(&hash));
        }

        // Find the smallest non-empty candidate set
        let smallest = candidate_sets
            .into_iter()
            .flatten()
            .min_by_key(|v| v.len());

        match smallest {
            Some(indices) => indices.clone(),
            None => Vec::new(), // No matches possible
        }
    }

    /// Try to unify a pattern triple with a ground triple
    fn unify_triple(&self, pattern: &Triple, ground: &Triple) -> Option<Bindings> {
        let mut bindings = Bindings::default();

        if !self.unify_term(&pattern.subject, &ground.subject, &mut bindings) {
            return None;
        }
        if !self.unify_term(&pattern.predicate, &ground.predicate, &mut bindings) {
            return None;
        }
        if !self.unify_term(&pattern.object, &ground.object, &mut bindings) {
            return None;
        }

        Some(bindings)
    }

    /// Try to unify a pattern term with a ground term
    fn unify_term(&self, pattern: &Term, ground: &Term, bindings: &mut Bindings) -> bool {
        match pattern {
            Term::Variable(var) => {
                if let Some(existing) = bindings.get(var) {
                    // Variable already bound - check consistency
                    existing == ground
                } else {
                    // Bind the variable
                    bindings.insert(var.clone(), ground.clone());
                    true
                }
            }
            _ => pattern == ground,
        }
    }

    /// Query with multiple patterns (conjunctive query)
    pub fn query(&self, patterns: &[Triple]) -> Vec<Bindings> {
        if patterns.is_empty() {
            return vec![Bindings::default()];
        }

        let mut results = self.match_pattern(&patterns[0]);

        for pattern in &patterns[1..] {
            let mut new_results = Vec::new();

            for bindings in results {
                // Apply current bindings to pattern
                let substituted = substitute_triple(pattern, &bindings);

                // Match against store
                for new_bindings in self.match_pattern(&substituted) {
                    // Merge bindings
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

    /// Create a new nested formula and return its reference
    pub fn new_formula(&mut self) -> FormulaRef {
        let id = self.next_formula_id;
        self.next_formula_id += 1;
        self.formulas.insert(id, Store::new());
        FormulaRef::new(id, Vec::new())
    }

    /// Get a nested formula by reference
    pub fn get_formula(&self, r: &FormulaRef) -> Option<&Store> {
        self.formulas.get(&r.id)
    }

    /// Get a mutable reference to a nested formula
    pub fn get_formula_mut(&mut self, r: &FormulaRef) -> Option<&mut Store> {
        self.formulas.get_mut(&r.id)
    }

    /// Remove a triple from the store
    ///
    /// Note: This is O(n) as it requires rebuilding indexes.
    /// For frequent removals, consider using a different data structure.
    pub fn remove(&mut self, triple: &Triple) -> bool {
        if let Some(pos) = self.triples.iter().position(|t| t == triple) {
            // Remove from hash set
            let hash = triple_hash(triple);
            self.triple_hashes.remove(&hash);

            // Remove the triple
            self.triples.remove(pos);

            // Rebuild indexes (simpler than trying to update in place)
            self.rebuild_indexes();

            true
        } else {
            false
        }
    }

    /// Rebuild all indexes from scratch
    fn rebuild_indexes(&mut self) {
        self.by_subject.clear();
        self.by_predicate.clear();
        self.by_object.clear();

        for (idx, triple) in self.triples.iter().enumerate() {
            let subj_hash = term_hash(&triple.subject);
            let pred_hash = term_hash(&triple.predicate);
            let obj_hash = term_hash(&triple.object);

            self.by_subject.entry(subj_hash).or_default().push(idx);
            self.by_predicate.entry(pred_hash).or_default().push(idx);
            self.by_object.entry(obj_hash).or_default().push(idx);
        }
    }

    /// Clear all triples and indexes
    pub fn clear(&mut self) {
        self.triples.clear();
        self.triple_hashes.clear();
        self.by_subject.clear();
        self.by_predicate.clear();
        self.by_object.clear();
    }

    /// Iterate over all triples
    pub fn iter(&self) -> impl Iterator<Item = &Triple> {
        self.triples.iter()
    }
}

// Implement the TripleStore trait for Store
impl TripleStore for Store {
    fn add(&mut self, triple: Triple) {
        Store::add(self, triple)
    }

    fn remove(&mut self, triple: &Triple) -> bool {
        Store::remove(self, triple)
    }

    fn contains(&self, triple: &Triple) -> bool {
        Store::contains(self, triple)
    }

    fn len(&self) -> usize {
        Store::len(self)
    }

    fn iter(&self) -> Box<dyn Iterator<Item = &Triple> + '_> {
        Box::new(self.triples.iter())
    }

    fn clear(&mut self) {
        Store::clear(self)
    }
}

impl std::fmt::Debug for Store {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Store {{")?;
        for triple in &self.triples {
            writeln!(f, "  {:?}", triple)?;
        }
        write!(f, "}}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_and_contains() {
        let mut store = Store::new();
        let triple = Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::literal("o"),
        );

        store.add(triple.clone());
        assert!(store.contains(&triple));
        assert_eq!(store.len(), 1);
    }

    #[test]
    fn test_no_duplicates() {
        let mut store = Store::new();
        let triple = Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::literal("o"),
        );

        store.add(triple.clone());
        store.add(triple.clone());
        assert_eq!(store.len(), 1);
    }

    #[test]
    fn test_match_pattern() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/bob"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/charlie"),
        ));

        // Match all things alice knows
        let pattern = Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::universal("x"),
        );

        let results = store.match_pattern(&pattern);
        assert_eq!(results.len(), 2);
    }

    #[test]
    fn test_query() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/bob"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/bob"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/charlie"),
        ));

        // Find X where alice knows X and X knows Y
        let patterns = vec![
            Triple::new(
                Term::uri("http://example.org/alice"),
                Term::uri("http://example.org/knows"),
                Term::universal("x"),
            ),
            Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/knows"),
                Term::universal("y"),
            ),
        ];

        let results = store.query(&patterns);
        assert_eq!(results.len(), 1);
    }
}
