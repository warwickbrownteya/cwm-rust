//! RDF/N3 Store (Formula) implementation
//!
//! A store holds a set of triples and supports pattern matching.

use indexmap::IndexMap;

use crate::term::{Term, Triple, Bindings, FormulaRef, substitute_triple};

/// A store of RDF triples (also called a formula or graph)
#[derive(Clone, Default)]
pub struct Store {
    /// The triples in this store
    triples: Vec<Triple>,
    /// Index by subject
    by_subject: IndexMap<u64, Vec<usize>>,
    /// Index by predicate
    by_predicate: IndexMap<u64, Vec<usize>>,
    /// Index by object
    by_object: IndexMap<u64, Vec<usize>>,
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

    /// Add a triple to the store
    pub fn add(&mut self, triple: Triple) {
        // Check for duplicates
        if self.contains(&triple) {
            return;
        }

        self.triples.push(triple);
        // TODO: Update indexes for efficient lookups
    }

    /// Add multiple triples
    pub fn add_all(&mut self, triples: impl IntoIterator<Item = Triple>) {
        for triple in triples {
            self.add(triple);
        }
    }

    /// Check if the store contains a triple
    pub fn contains(&self, triple: &Triple) -> bool {
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
    pub fn match_pattern(&self, pattern: &Triple) -> Vec<Bindings> {
        let mut results = Vec::new();

        for triple in &self.triples {
            if let Some(bindings) = self.unify_triple(pattern, triple) {
                results.push(bindings);
            }
        }

        results
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
    pub fn remove(&mut self, triple: &Triple) -> bool {
        if let Some(pos) = self.triples.iter().position(|t| t == triple) {
            self.triples.remove(pos);
            true
        } else {
            false
        }
    }

    /// Clear all triples
    pub fn clear(&mut self) {
        self.triples.clear();
        self.by_subject.clear();
        self.by_predicate.clear();
        self.by_object.clear();
    }

    /// Iterate over all triples
    pub fn iter(&self) -> impl Iterator<Item = &Triple> {
        self.triples.iter()
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
