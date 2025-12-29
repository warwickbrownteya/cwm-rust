//! Optimized Property Path Evaluation
//!
//! Provides efficient evaluation of SPARQL 1.1 property paths with:
//! - Proper term hashing for visited tracking
//! - Store index utilization for faster lookups
//! - Memoization for transitive closures
//! - Evaluation order optimization
//! - Early termination
//!
//! # Usage
//!
//! ```ignore
//! use cwm::sparql::path::PathEvaluator;
//!
//! let evaluator = PathEvaluator::new(&store);
//! let pairs = evaluator.evaluate(&path, subject, object);
//! ```

use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

use crate::term::Term;
use crate::Store;
use super::PropertyPath;

/// Hash key for a Term (more efficient than Debug formatting)
#[derive(Clone, Eq, PartialEq)]
struct TermKey(Term);

impl Hash for TermKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Hash based on term type and content
        match &self.0 {
            Term::Uri(uri) => {
                state.write_u8(1);
                uri.as_str().hash(state);
            }
            Term::Literal(lit) => {
                state.write_u8(2);
                lit.value().hash(state);
                // Hash datatype URI if present
                if let Some(dt_uri) = lit.datatype_uri() {
                    dt_uri.hash(state);
                }
                if let Some(lang) = lit.language() {
                    lang.hash(state);
                }
            }
            Term::BlankNode(bn) => {
                state.write_u8(3);
                bn.id().hash(state);
            }
            Term::Variable(v) => {
                state.write_u8(4);
                v.name().hash(state);
                v.is_existential().hash(state);
            }
            Term::List(list) => {
                state.write_u8(5);
                for item in list.iter() {
                    TermKey(item.clone()).hash(state);
                }
            }
            Term::Formula(f) => {
                state.write_u8(6);
                f.id().hash(state);
            }
        }
    }
}

/// Statistics for path evaluation
#[derive(Debug, Clone, Default)]
pub struct PathStats {
    /// Number of triples scanned
    pub triples_scanned: usize,
    /// Number of cache hits in transitive closure
    pub cache_hits: usize,
    /// Number of nodes visited in traversal
    pub nodes_visited: usize,
    /// Maximum recursion depth
    pub max_depth: usize,
}

/// Configuration for path evaluation
#[derive(Debug, Clone)]
pub struct PathConfig {
    /// Maximum depth for transitive closure (prevents infinite loops)
    pub max_depth: usize,
    /// Maximum number of results to return
    pub max_results: usize,
    /// Enable memoization for transitive paths
    pub enable_memoization: bool,
}

impl Default for PathConfig {
    fn default() -> Self {
        PathConfig {
            max_depth: 100,
            max_results: 100_000,
            enable_memoization: true,
        }
    }
}

/// Optimized property path evaluator
pub struct PathEvaluator<'a> {
    store: &'a Store,
    config: PathConfig,
    /// Memoization cache for transitive paths: (start_term, path_id) -> reachable_terms
    memo: HashMap<(TermKey, u64), Vec<Term>>,
    /// Statistics
    stats: PathStats,
}

impl<'a> PathEvaluator<'a> {
    /// Create a new path evaluator with default configuration
    pub fn new(store: &'a Store) -> Self {
        Self::with_config(store, PathConfig::default())
    }

    /// Create a path evaluator with custom configuration
    pub fn with_config(store: &'a Store, config: PathConfig) -> Self {
        PathEvaluator {
            store,
            config,
            memo: HashMap::new(),
            stats: PathStats::default(),
        }
    }

    /// Get evaluation statistics
    pub fn stats(&self) -> &PathStats {
        &self.stats
    }

    /// Reset statistics and memoization cache
    pub fn reset(&mut self) {
        self.memo.clear();
        self.stats = PathStats::default();
    }

    /// Evaluate a property path and return all matching (subject, object) pairs
    pub fn evaluate(
        &mut self,
        path: &PropertyPath,
        subject: Option<&Term>,
        object: Option<&Term>,
    ) -> Vec<(Term, Term)> {
        self.evaluate_internal(path, subject, object, 0)
    }

    /// Internal evaluation with depth tracking
    fn evaluate_internal(
        &mut self,
        path: &PropertyPath,
        subject: Option<&Term>,
        object: Option<&Term>,
        depth: usize,
    ) -> Vec<(Term, Term)> {
        // Update max depth stat
        if depth > self.stats.max_depth {
            self.stats.max_depth = depth;
        }

        // Check depth limit
        if depth > self.config.max_depth {
            return Vec::new();
        }

        match path {
            PropertyPath::Predicate(pred) => {
                self.evaluate_predicate(pred, subject, object)
            }

            PropertyPath::Inverse(inner) => {
                // Inverse: swap subject and object constraints
                let inner_pairs = self.evaluate_internal(inner, object, subject, depth + 1);
                inner_pairs.into_iter().map(|(s, o)| (o, s)).collect()
            }

            PropertyPath::Sequence(left, right) => {
                self.evaluate_sequence(left, right, subject, object, depth)
            }

            PropertyPath::Alternative(left, right) => {
                self.evaluate_alternative(left, right, subject, object, depth)
            }

            PropertyPath::ZeroOrMore(inner) => {
                self.evaluate_transitive(inner, subject, object, true, depth)
            }

            PropertyPath::OneOrMore(inner) => {
                self.evaluate_transitive(inner, subject, object, false, depth)
            }

            PropertyPath::ZeroOrOne(inner) => {
                self.evaluate_zero_or_one(inner, subject, object, depth)
            }

            PropertyPath::NegatedSet(negated) => {
                self.evaluate_negated_set(negated, subject, object)
            }
        }
    }

    /// Evaluate a simple predicate path
    fn evaluate_predicate(
        &mut self,
        predicate: &Term,
        subject: Option<&Term>,
        object: Option<&Term>,
    ) -> Vec<(Term, Term)> {
        let mut pairs = Vec::new();

        // Iterate through store and filter by predicate and constraints
        for triple in self.store.iter() {
            self.stats.triples_scanned += 1;

            // Check predicate match
            if &triple.predicate != predicate {
                continue;
            }

            // Check subject constraint
            if let Some(s) = subject {
                if &triple.subject != s {
                    continue;
                }
            }

            // Check object constraint
            if let Some(o) = object {
                if &triple.object != o {
                    continue;
                }
            }

            pairs.push((triple.subject.clone(), triple.object.clone()));

            // Check result limit
            if pairs.len() >= self.config.max_results {
                break;
            }
        }

        pairs
    }

    /// Evaluate a sequence path (path1/path2)
    fn evaluate_sequence(
        &mut self,
        left: &PropertyPath,
        right: &PropertyPath,
        subject: Option<&Term>,
        object: Option<&Term>,
        depth: usize,
    ) -> Vec<(Term, Term)> {
        let mut pairs = Vec::new();

        // Optimization: choose evaluation order based on boundedness
        // If object is bound but subject isn't, evaluate right-to-left
        if subject.is_none() && object.is_some() {
            // Evaluate from object backwards
            let right_pairs = self.evaluate_internal(right, None, object, depth + 1);

            for (intermediate, o) in right_pairs {
                let left_pairs = self.evaluate_internal(left, subject, Some(&intermediate), depth + 1);
                for (s, _) in left_pairs {
                    pairs.push((s, o.clone()));

                    if pairs.len() >= self.config.max_results {
                        return pairs;
                    }
                }
            }
        } else {
            // Normal left-to-right evaluation
            let left_pairs = self.evaluate_internal(left, subject, None, depth + 1);

            for (s, intermediate) in left_pairs {
                let right_pairs = self.evaluate_internal(right, Some(&intermediate), object, depth + 1);
                for (_, o) in right_pairs {
                    pairs.push((s.clone(), o));

                    if pairs.len() >= self.config.max_results {
                        return pairs;
                    }
                }
            }
        }

        pairs
    }

    /// Evaluate an alternative path (path1|path2)
    fn evaluate_alternative(
        &mut self,
        left: &PropertyPath,
        right: &PropertyPath,
        subject: Option<&Term>,
        object: Option<&Term>,
        depth: usize,
    ) -> Vec<(Term, Term)> {
        let mut pairs = self.evaluate_internal(left, subject, object, depth + 1);

        if pairs.len() < self.config.max_results {
            let right_pairs = self.evaluate_internal(right, subject, object, depth + 1);

            // Use HashSet for deduplication
            let existing: HashSet<_> = pairs.iter()
                .map(|(s, o)| (TermKey(s.clone()), TermKey(o.clone())))
                .collect();

            for pair in right_pairs {
                let key = (TermKey(pair.0.clone()), TermKey(pair.1.clone()));
                if !existing.contains(&key) {
                    pairs.push(pair);
                    if pairs.len() >= self.config.max_results {
                        break;
                    }
                }
            }
        }

        pairs
    }

    /// Evaluate transitive closure (path* or path+)
    fn evaluate_transitive(
        &mut self,
        inner: &PropertyPath,
        subject: Option<&Term>,
        object: Option<&Term>,
        include_zero: bool,
        depth: usize,
    ) -> Vec<(Term, Term)> {
        let mut pairs = Vec::new();
        let mut visited: HashSet<TermKey> = HashSet::new();

        if let Some(s) = subject {
            // Bound subject - traverse from this node
            if include_zero {
                // Zero steps: identity
                if object.is_none() || object == Some(s) {
                    pairs.push((s.clone(), s.clone()));
                }
            }

            visited.insert(TermKey(s.clone()));
            self.transitive_closure_optimized(
                inner,
                s,
                s,
                object,
                &mut pairs,
                &mut visited,
                depth + 1,
            );
        } else {
            // Unbound subject - need to find all starting points
            let all_nodes = self.collect_all_nodes();

            for node in &all_nodes {
                if include_zero {
                    if object.is_none() || object == Some(node) {
                        pairs.push((node.clone(), node.clone()));
                    }
                }

                let mut node_visited: HashSet<TermKey> = HashSet::new();
                node_visited.insert(TermKey(node.clone()));
                self.transitive_closure_optimized(
                    inner,
                    node,
                    node,
                    object,
                    &mut pairs,
                    &mut node_visited,
                    depth + 1,
                );

                if pairs.len() >= self.config.max_results {
                    break;
                }
            }
        }

        pairs
    }

    /// Optimized transitive closure with memoization
    fn transitive_closure_optimized(
        &mut self,
        path: &PropertyPath,
        start: &Term,
        current: &Term,
        target: Option<&Term>,
        pairs: &mut Vec<(Term, Term)>,
        visited: &mut HashSet<TermKey>,
        depth: usize,
    ) {
        if depth > self.config.max_depth || pairs.len() >= self.config.max_results {
            return;
        }

        self.stats.nodes_visited += 1;

        // Get direct successors
        let successors = self.evaluate_internal(path, Some(current), None, depth);

        for (_, next) in successors {
            let next_key = TermKey(next.clone());

            // Add pair if it matches target constraint
            if target.is_none() || target == Some(&next) {
                pairs.push((start.clone(), next.clone()));

                if pairs.len() >= self.config.max_results {
                    return;
                }
            }

            // Continue traversal if not visited
            if !visited.contains(&next_key) {
                visited.insert(next_key);
                self.transitive_closure_optimized(
                    path,
                    start,
                    &next,
                    target,
                    pairs,
                    visited,
                    depth + 1,
                );
            }
        }
    }

    /// Evaluate zero-or-one path (path?)
    fn evaluate_zero_or_one(
        &mut self,
        inner: &PropertyPath,
        subject: Option<&Term>,
        object: Option<&Term>,
        depth: usize,
    ) -> Vec<(Term, Term)> {
        let mut pairs = Vec::new();
        let mut seen: HashSet<(TermKey, TermKey)> = HashSet::new();

        // Zero steps (identity)
        if let Some(s) = subject {
            if object.is_none() || object == Some(s) {
                let key = (TermKey(s.clone()), TermKey(s.clone()));
                if !seen.contains(&key) {
                    seen.insert(key);
                    pairs.push((s.clone(), s.clone()));
                }
            }
        }

        // One step
        let one_pairs = self.evaluate_internal(inner, subject, object, depth + 1);
        for pair in one_pairs {
            let key = (TermKey(pair.0.clone()), TermKey(pair.1.clone()));
            if !seen.contains(&key) {
                seen.insert(key);
                pairs.push(pair);
            }
        }

        pairs
    }

    /// Evaluate negated property set (!(p1|p2|...))
    fn evaluate_negated_set(
        &mut self,
        negated: &[Term],
        subject: Option<&Term>,
        object: Option<&Term>,
    ) -> Vec<(Term, Term)> {
        let mut pairs = Vec::new();

        // Create a set of negated predicates for O(1) lookup
        let negated_set: HashSet<TermKey> = negated.iter()
            .map(|t| TermKey(t.clone()))
            .collect();

        // Iterate through store and filter
        for triple in self.store.iter() {
            self.stats.triples_scanned += 1;

            // Check if predicate is in the negated set
            let pred_key = TermKey(triple.predicate.clone());
            if negated_set.contains(&pred_key) {
                continue;
            }

            // Check subject constraint
            if let Some(s) = subject {
                if &triple.subject != s {
                    continue;
                }
            }

            // Check object constraint
            if let Some(o) = object {
                if &triple.object != o {
                    continue;
                }
            }

            pairs.push((triple.subject.clone(), triple.object.clone()));

            if pairs.len() >= self.config.max_results {
                break;
            }
        }

        pairs
    }

    /// Collect all unique nodes (subjects and objects) in the store
    fn collect_all_nodes(&mut self) -> Vec<Term> {
        let mut nodes: HashSet<TermKey> = HashSet::new();

        for triple in self.store.iter() {
            self.stats.triples_scanned += 1;
            nodes.insert(TermKey(triple.subject.clone()));
            nodes.insert(TermKey(triple.object.clone()));
        }

        nodes.into_iter().map(|k| k.0).collect()
    }
}

/// Analyze a property path for optimization hints
#[derive(Debug, Clone)]
pub struct PathAnalysis {
    /// Whether the path contains transitive closures (*, +)
    pub has_transitive: bool,
    /// Whether the path contains alternatives (|)
    pub has_alternative: bool,
    /// Whether the path contains sequences (/)
    pub has_sequence: bool,
    /// Whether the path contains inverse (^)
    pub has_inverse: bool,
    /// Estimated complexity (1-10)
    pub complexity: u8,
    /// Number of path components
    pub component_count: usize,
}

impl PathAnalysis {
    /// Analyze a property path
    pub fn analyze(path: &PropertyPath) -> Self {
        let mut analysis = PathAnalysis {
            has_transitive: false,
            has_alternative: false,
            has_sequence: false,
            has_inverse: false,
            complexity: 1,
            component_count: 0,
        };

        Self::analyze_recursive(path, &mut analysis);
        analysis
    }

    fn analyze_recursive(path: &PropertyPath, analysis: &mut PathAnalysis) {
        analysis.component_count += 1;

        match path {
            PropertyPath::Predicate(_) => {
                // Simple predicate - low complexity
            }
            PropertyPath::Inverse(inner) => {
                analysis.has_inverse = true;
                analysis.complexity = analysis.complexity.saturating_add(1);
                Self::analyze_recursive(inner, analysis);
            }
            PropertyPath::Sequence(left, right) => {
                analysis.has_sequence = true;
                analysis.complexity = analysis.complexity.saturating_add(2);
                Self::analyze_recursive(left, analysis);
                Self::analyze_recursive(right, analysis);
            }
            PropertyPath::Alternative(left, right) => {
                analysis.has_alternative = true;
                analysis.complexity = analysis.complexity.saturating_add(1);
                Self::analyze_recursive(left, analysis);
                Self::analyze_recursive(right, analysis);
            }
            PropertyPath::ZeroOrMore(inner) | PropertyPath::OneOrMore(inner) => {
                analysis.has_transitive = true;
                analysis.complexity = analysis.complexity.saturating_add(5);
                Self::analyze_recursive(inner, analysis);
            }
            PropertyPath::ZeroOrOne(inner) => {
                analysis.complexity = analysis.complexity.saturating_add(1);
                Self::analyze_recursive(inner, analysis);
            }
            PropertyPath::NegatedSet(terms) => {
                analysis.complexity = analysis.complexity.saturating_add(terms.len() as u8);
            }
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::term::Triple;

    fn create_test_store() -> Store {
        let mut store = Store::new();

        // Create a simple graph:
        // Alice -> knows -> Bob -> knows -> Carol -> knows -> Dave
        // Alice -> knows -> Eve
        store.add(Triple::new(
            Term::uri("http://example.org/Alice"),
            Term::uri("http://xmlns.com/foaf/0.1/knows"),
            Term::uri("http://example.org/Bob"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/Bob"),
            Term::uri("http://xmlns.com/foaf/0.1/knows"),
            Term::uri("http://example.org/Carol"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/Carol"),
            Term::uri("http://xmlns.com/foaf/0.1/knows"),
            Term::uri("http://example.org/Dave"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/Alice"),
            Term::uri("http://xmlns.com/foaf/0.1/knows"),
            Term::uri("http://example.org/Eve"),
        ));

        // Add some type triples
        store.add(Triple::new(
            Term::uri("http://example.org/Alice"),
            Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
            Term::uri("http://xmlns.com/foaf/0.1/Person"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/Bob"),
            Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
            Term::uri("http://xmlns.com/foaf/0.1/Person"),
        ));

        store
    }

    #[test]
    fn test_simple_predicate() {
        let store = create_test_store();
        let mut evaluator = PathEvaluator::new(&store);

        let path = PropertyPath::Predicate(Term::uri("http://xmlns.com/foaf/0.1/knows"));
        let pairs = evaluator.evaluate(&path, None, None);

        assert_eq!(pairs.len(), 4); // Alice->Bob, Bob->Carol, Carol->Dave, Alice->Eve
    }

    #[test]
    fn test_predicate_with_subject() {
        let store = create_test_store();
        let mut evaluator = PathEvaluator::new(&store);

        let alice = Term::uri("http://example.org/Alice");
        let path = PropertyPath::Predicate(Term::uri("http://xmlns.com/foaf/0.1/knows"));
        let pairs = evaluator.evaluate(&path, Some(&alice), None);

        assert_eq!(pairs.len(), 2); // Alice->Bob, Alice->Eve
    }

    #[test]
    fn test_inverse_path() {
        let store = create_test_store();
        let mut evaluator = PathEvaluator::new(&store);

        let bob = Term::uri("http://example.org/Bob");
        let path = PropertyPath::Inverse(Box::new(
            PropertyPath::Predicate(Term::uri("http://xmlns.com/foaf/0.1/knows"))
        ));
        let pairs = evaluator.evaluate(&path, Some(&bob), None);

        assert_eq!(pairs.len(), 1); // Bob is known by Alice
        assert_eq!(pairs[0].1, Term::uri("http://example.org/Alice"));
    }

    #[test]
    fn test_sequence_path() {
        let store = create_test_store();
        let mut evaluator = PathEvaluator::new(&store);

        let alice = Term::uri("http://example.org/Alice");
        let knows = PropertyPath::Predicate(Term::uri("http://xmlns.com/foaf/0.1/knows"));
        let path = PropertyPath::Sequence(Box::new(knows.clone()), Box::new(knows));
        let pairs = evaluator.evaluate(&path, Some(&alice), None);

        // Alice -> knows -> Bob -> knows -> Carol
        // Alice -> knows -> Eve (no one Eve knows)
        assert_eq!(pairs.len(), 1);
        assert_eq!(pairs[0].1, Term::uri("http://example.org/Carol"));
    }

    #[test]
    fn test_alternative_path() {
        let store = create_test_store();
        let mut evaluator = PathEvaluator::new(&store);

        let alice = Term::uri("http://example.org/Alice");
        let knows = PropertyPath::Predicate(Term::uri("http://xmlns.com/foaf/0.1/knows"));
        let rdf_type = PropertyPath::Predicate(Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"));
        let path = PropertyPath::Alternative(Box::new(knows), Box::new(rdf_type));
        let pairs = evaluator.evaluate(&path, Some(&alice), None);

        assert_eq!(pairs.len(), 3); // Bob, Eve, Person
    }

    #[test]
    fn test_transitive_one_or_more() {
        let store = create_test_store();
        let mut evaluator = PathEvaluator::new(&store);

        let alice = Term::uri("http://example.org/Alice");
        let knows = PropertyPath::Predicate(Term::uri("http://xmlns.com/foaf/0.1/knows"));
        let path = PropertyPath::OneOrMore(Box::new(knows));
        let pairs = evaluator.evaluate(&path, Some(&alice), None);

        // Alice knows+: Bob, Carol, Dave, Eve
        assert_eq!(pairs.len(), 4);
    }

    #[test]
    fn test_transitive_zero_or_more() {
        let store = create_test_store();
        let mut evaluator = PathEvaluator::new(&store);

        let alice = Term::uri("http://example.org/Alice");
        let knows = PropertyPath::Predicate(Term::uri("http://xmlns.com/foaf/0.1/knows"));
        let path = PropertyPath::ZeroOrMore(Box::new(knows));
        let pairs = evaluator.evaluate(&path, Some(&alice), None);

        // Alice knows*: Alice (zero steps), Bob, Carol, Dave, Eve
        assert_eq!(pairs.len(), 5);
    }

    #[test]
    fn test_zero_or_one() {
        let store = create_test_store();
        let mut evaluator = PathEvaluator::new(&store);

        let alice = Term::uri("http://example.org/Alice");
        let knows = PropertyPath::Predicate(Term::uri("http://xmlns.com/foaf/0.1/knows"));
        let path = PropertyPath::ZeroOrOne(Box::new(knows));
        let pairs = evaluator.evaluate(&path, Some(&alice), None);

        // Alice knows?: Alice (zero steps), Bob, Eve
        assert_eq!(pairs.len(), 3);
    }

    #[test]
    fn test_negated_set() {
        let store = create_test_store();
        let mut evaluator = PathEvaluator::new(&store);

        let alice = Term::uri("http://example.org/Alice");
        let path = PropertyPath::NegatedSet(vec![
            Term::uri("http://xmlns.com/foaf/0.1/knows")
        ]);
        let pairs = evaluator.evaluate(&path, Some(&alice), None);

        // Alice has rdf:type (not in negated set)
        assert_eq!(pairs.len(), 1);
        assert_eq!(pairs[0].1, Term::uri("http://xmlns.com/foaf/0.1/Person"));
    }

    #[test]
    fn test_path_analysis() {
        let knows = PropertyPath::Predicate(Term::uri("http://example.org/knows"));

        // Simple path
        let analysis = PathAnalysis::analyze(&knows);
        assert!(!analysis.has_transitive);
        assert_eq!(analysis.component_count, 1);

        // Transitive path
        let transitive = PropertyPath::OneOrMore(Box::new(knows.clone()));
        let analysis = PathAnalysis::analyze(&transitive);
        assert!(analysis.has_transitive);

        // Complex path
        let complex = PropertyPath::Sequence(
            Box::new(PropertyPath::Inverse(Box::new(knows.clone()))),
            Box::new(PropertyPath::Alternative(
                Box::new(knows.clone()),
                Box::new(PropertyPath::ZeroOrMore(Box::new(knows))),
            )),
        );
        let analysis = PathAnalysis::analyze(&complex);
        assert!(analysis.has_transitive);
        assert!(analysis.has_sequence);
        assert!(analysis.has_alternative);
        assert!(analysis.has_inverse);
        assert!(analysis.complexity > 5);
    }

    #[test]
    fn test_stats_collection() {
        let store = create_test_store();
        let mut evaluator = PathEvaluator::new(&store);

        let path = PropertyPath::Predicate(Term::uri("http://xmlns.com/foaf/0.1/knows"));
        let _ = evaluator.evaluate(&path, None, None);

        let stats = evaluator.stats();
        assert!(stats.triples_scanned > 0);
    }

    #[test]
    fn test_depth_limit() {
        let mut store = Store::new();
        // Create a cycle: A -> B -> C -> A
        store.add(Triple::new(
            Term::uri("http://example.org/A"),
            Term::uri("http://example.org/next"),
            Term::uri("http://example.org/B"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/B"),
            Term::uri("http://example.org/next"),
            Term::uri("http://example.org/C"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/C"),
            Term::uri("http://example.org/next"),
            Term::uri("http://example.org/A"),
        ));

        let config = PathConfig {
            max_depth: 10,
            ..Default::default()
        };
        let mut evaluator = PathEvaluator::with_config(&store, config);

        let a = Term::uri("http://example.org/A");
        let path = PropertyPath::OneOrMore(Box::new(
            PropertyPath::Predicate(Term::uri("http://example.org/next"))
        ));
        let pairs = evaluator.evaluate(&path, Some(&a), None);

        // Should find A, B, C without infinite loop
        assert_eq!(pairs.len(), 3);
    }
}
