//! Incremental reasoning with dependency tracking
//!
//! This module provides incremental reasoning capabilities that allow for efficient
//! updates when data changes, avoiding full re-computation of inferences.
//!
//! # Features
//!
//! - **Dependency Tracking**: Tracks which triples were used to derive other triples
//! - **Delta Reasoning**: Processes additions and deletions incrementally
//! - **Minimal Re-derivation**: Only re-computes affected inferences when data changes
//!
//! # Example
//!
//! ```ignore
//! use cwm::{IncrementalReasoner, Store, Rule};
//!
//! let mut reasoner = IncrementalReasoner::new();
//! reasoner.add_rule(rule);
//!
//! // Initial reasoning
//! reasoner.materialize(&mut store);
//!
//! // Add a triple - only affected inferences are recomputed
//! reasoner.add_triple(&mut store, new_triple);
//!
//! // Remove a triple - dependent inferences are retracted
//! reasoner.remove_triple(&mut store, old_triple);
//! ```

use std::collections::{HashMap, HashSet, VecDeque};
use crate::term::{Term, Triple, Bindings, substitute_triple};
use crate::store::Store;
use crate::builtins::{BuiltinRegistry, BuiltinResult};
use super::{Rule, ReasonerConfig, ReasonerStats};

/// Unique identifier for a triple in the dependency graph
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TripleId(u64);

impl TripleId {
    /// Create a new TripleId from a hash
    pub fn new(id: u64) -> Self {
        TripleId(id)
    }

    /// Get the raw id value
    pub fn id(&self) -> u64 {
        self.0
    }
}

/// Origin of a triple - how it came to exist in the store
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TripleOrigin {
    /// Asserted by user (base fact)
    Asserted,
    /// Derived by rule application
    Derived {
        /// Index of the rule that derived this triple
        rule_index: usize,
        /// IDs of the premise triples used in the derivation
        premises: Vec<TripleId>,
        /// Variable bindings used in the derivation
        bindings: Vec<(String, String)>, // Serialized for comparison
    },
}

/// Entry in the dependency graph tracking a triple's origins and dependents
#[derive(Clone, Debug)]
pub struct DependencyEntry {
    /// The triple itself
    pub triple: Triple,
    /// All ways this triple can be derived (may have multiple derivations)
    pub origins: Vec<TripleOrigin>,
    /// IDs of triples that depend on this triple
    pub dependents: HashSet<TripleId>,
    /// Whether this triple is currently valid (not retracted)
    pub valid: bool,
}

/// Dependency graph tracking triple derivations and dependencies
#[derive(Clone, Debug, Default)]
pub struct DependencyGraph {
    /// Map from triple ID to dependency entry
    entries: HashMap<TripleId, DependencyEntry>,
    /// Map from triple signature to ID for quick lookup
    triple_to_id: HashMap<String, TripleId>,
    /// Next available ID
    next_id: u64,
}

impl DependencyGraph {
    /// Create a new empty dependency graph
    pub fn new() -> Self {
        Self::default()
    }

    /// Get or create an ID for a triple
    pub fn get_or_create_id(&mut self, triple: &Triple) -> TripleId {
        let sig = format!("{:?}", triple);
        if let Some(&id) = self.triple_to_id.get(&sig) {
            return id;
        }

        let id = TripleId::new(self.next_id);
        self.next_id += 1;
        self.triple_to_id.insert(sig, id);
        id
    }

    /// Get the ID for a triple if it exists
    pub fn get_id(&self, triple: &Triple) -> Option<TripleId> {
        let sig = format!("{:?}", triple);
        self.triple_to_id.get(&sig).copied()
    }

    /// Add an asserted triple to the graph
    pub fn add_asserted(&mut self, triple: Triple) -> TripleId {
        let id = self.get_or_create_id(&triple);

        if let Some(entry) = self.entries.get_mut(&id) {
            // Add assertion origin if not already present
            if !entry.origins.contains(&TripleOrigin::Asserted) {
                entry.origins.push(TripleOrigin::Asserted);
            }
            entry.valid = true;
        } else {
            self.entries.insert(id, DependencyEntry {
                triple,
                origins: vec![TripleOrigin::Asserted],
                dependents: HashSet::new(),
                valid: true,
            });
        }

        id
    }

    /// Add a derived triple to the graph
    pub fn add_derived(
        &mut self,
        triple: Triple,
        rule_index: usize,
        premise_ids: Vec<TripleId>,
        bindings: &Bindings,
    ) -> TripleId {
        let id = self.get_or_create_id(&triple);

        // Serialize bindings for comparison
        let serialized_bindings: Vec<(String, String)> = bindings
            .iter()
            .map(|(v, t)| (v.name().to_string(), format!("{:?}", t)))
            .collect();

        let origin = TripleOrigin::Derived {
            rule_index,
            premises: premise_ids.clone(),
            bindings: serialized_bindings,
        };

        if let Some(entry) = self.entries.get_mut(&id) {
            // Add derivation origin if not already present
            if !entry.origins.contains(&origin) {
                entry.origins.push(origin);
            }
            entry.valid = true;
        } else {
            self.entries.insert(id, DependencyEntry {
                triple,
                origins: vec![origin],
                dependents: HashSet::new(),
                valid: true,
            });
        }

        // Update dependents for premises
        for premise_id in premise_ids {
            if let Some(premise_entry) = self.entries.get_mut(&premise_id) {
                premise_entry.dependents.insert(id);
            }
        }

        id
    }

    /// Get a dependency entry by ID
    pub fn get(&self, id: TripleId) -> Option<&DependencyEntry> {
        self.entries.get(&id)
    }

    /// Get a mutable dependency entry by ID
    pub fn get_mut(&mut self, id: TripleId) -> Option<&mut DependencyEntry> {
        self.entries.get_mut(&id)
    }

    /// Check if a triple is asserted (base fact)
    pub fn is_asserted(&self, id: TripleId) -> bool {
        self.entries.get(&id)
            .map(|e| e.origins.contains(&TripleOrigin::Asserted))
            .unwrap_or(false)
    }

    /// Check if a triple has any valid derivation
    pub fn has_valid_derivation(&self, id: TripleId) -> bool {
        if let Some(entry) = self.entries.get(&id) {
            for origin in &entry.origins {
                match origin {
                    TripleOrigin::Asserted => return true,
                    TripleOrigin::Derived { premises, .. } => {
                        // Check if all premises are still valid
                        if premises.iter().all(|p| {
                            self.entries.get(p).map(|e| e.valid).unwrap_or(false)
                        }) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    /// Get all triples that depend on the given triple
    pub fn get_dependents(&self, id: TripleId) -> HashSet<TripleId> {
        self.entries.get(&id)
            .map(|e| e.dependents.clone())
            .unwrap_or_default()
    }

    /// Invalidate a triple and return all affected dependents
    pub fn invalidate(&mut self, id: TripleId) -> Vec<TripleId> {
        let mut affected = Vec::new();
        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();
        queue.push_back(id);
        visited.insert(id);

        while let Some(current_id) = queue.pop_front() {
            // First, collect dependents in an immutable borrow
            let dependents = {
                if let Some(entry) = self.entries.get(&current_id) {
                    entry.dependents.clone()
                } else {
                    HashSet::new()
                }
            };

            // Check if entry is currently valid and should be invalidated
            let was_valid = self.entries.get(&current_id)
                .map(|e| e.valid)
                .unwrap_or(false);

            if was_valid {
                // Invalidate this entry
                if let Some(entry) = self.entries.get_mut(&current_id) {
                    entry.valid = false;
                }
                affected.push(current_id);
            }

            // Check dependents for invalidation (always check, even if already invalid)
            for dep_id in dependents {
                if !visited.contains(&dep_id) && !self.has_valid_derivation(dep_id) {
                    visited.insert(dep_id);
                    queue.push_back(dep_id);
                }
            }
        }

        affected
    }

    /// Remove an assertion origin from a triple
    pub fn remove_assertion(&mut self, id: TripleId) -> bool {
        // First, remove the assertion origin
        let origins_empty = if let Some(entry) = self.entries.get_mut(&id) {
            entry.origins.retain(|o| o != &TripleOrigin::Asserted);
            entry.origins.is_empty()
        } else {
            return false;
        };

        // If no origins remain, invalidate
        if origins_empty {
            if let Some(entry) = self.entries.get_mut(&id) {
                entry.valid = false;
            }
            return true;
        }

        // Check if any remaining derivations are valid (separate borrow)
        let has_valid = self.has_valid_derivation(id);
        if !has_valid {
            if let Some(entry) = self.entries.get_mut(&id) {
                entry.valid = false;
            }
            return true;
        }

        false
    }

    /// Get all valid triple IDs
    pub fn valid_ids(&self) -> Vec<TripleId> {
        self.entries
            .iter()
            .filter(|(_, e)| e.valid)
            .map(|(&id, _)| id)
            .collect()
    }

    /// Get statistics about the dependency graph
    pub fn stats(&self) -> DependencyStats {
        let total = self.entries.len();
        let valid = self.entries.values().filter(|e| e.valid).count();
        let asserted = self.entries.values()
            .filter(|e| e.origins.contains(&TripleOrigin::Asserted))
            .count();
        let derived = self.entries.values()
            .filter(|e| e.origins.iter().any(|o| matches!(o, TripleOrigin::Derived { .. })))
            .count();
        let max_dependents = self.entries.values()
            .map(|e| e.dependents.len())
            .max()
            .unwrap_or(0);

        DependencyStats {
            total_triples: total,
            valid_triples: valid,
            asserted_triples: asserted,
            derived_triples: derived,
            max_dependents,
        }
    }
}

/// Statistics about the dependency graph
#[derive(Clone, Debug, Default)]
pub struct DependencyStats {
    /// Total triples in the graph
    pub total_triples: usize,
    /// Currently valid triples
    pub valid_triples: usize,
    /// Asserted (base) triples
    pub asserted_triples: usize,
    /// Derived triples (may overlap with asserted)
    pub derived_triples: usize,
    /// Maximum number of dependents for any triple
    pub max_dependents: usize,
}

/// Delta representing changes to the store
#[derive(Clone, Debug, Default)]
pub struct Delta {
    /// Triples to add
    pub additions: Vec<Triple>,
    /// Triples to remove
    pub deletions: Vec<Triple>,
}

impl Delta {
    /// Create a new empty delta
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a delta with additions
    pub fn additions(triples: Vec<Triple>) -> Self {
        Delta {
            additions: triples,
            deletions: Vec::new(),
        }
    }

    /// Create a delta with deletions
    pub fn deletions(triples: Vec<Triple>) -> Self {
        Delta {
            additions: Vec::new(),
            deletions: triples,
        }
    }

    /// Check if the delta is empty
    pub fn is_empty(&self) -> bool {
        self.additions.is_empty() && self.deletions.is_empty()
    }

    /// Merge another delta into this one
    pub fn merge(&mut self, other: Delta) {
        self.additions.extend(other.additions);
        self.deletions.extend(other.deletions);
    }
}

/// Statistics about incremental reasoning operations
#[derive(Clone, Debug, Default)]
pub struct IncrementalStats {
    /// Base reasoner statistics
    pub base: ReasonerStats,
    /// Number of triples added incrementally
    pub triples_added: usize,
    /// Number of triples removed
    pub triples_removed: usize,
    /// Number of triples re-derived after changes
    pub triples_rederived: usize,
    /// Number of triples retracted due to dependency invalidation
    pub triples_retracted: usize,
}

/// Incremental reasoner with dependency tracking
pub struct IncrementalReasoner {
    /// Configuration
    config: ReasonerConfig,
    /// Built-in predicate registry
    builtins: BuiltinRegistry,
    /// Rules to apply
    rules: Vec<Rule>,
    /// Dependency graph
    dependencies: DependencyGraph,
    /// Statistics
    stats: IncrementalStats,
    /// Set of derived triple signatures for duplicate detection
    derived_signatures: HashSet<String>,
}

impl IncrementalReasoner {
    /// Create a new incremental reasoner
    pub fn new() -> Self {
        IncrementalReasoner {
            config: ReasonerConfig::default(),
            builtins: BuiltinRegistry::new(),
            rules: Vec::new(),
            dependencies: DependencyGraph::new(),
            stats: IncrementalStats::default(),
            derived_signatures: HashSet::new(),
        }
    }

    /// Create with custom configuration
    pub fn with_config(config: ReasonerConfig) -> Self {
        let enable_crypto = config.enable_crypto;
        IncrementalReasoner {
            config,
            builtins: BuiltinRegistry::with_options(enable_crypto),
            rules: Vec::new(),
            dependencies: DependencyGraph::new(),
            stats: IncrementalStats::default(),
            derived_signatures: HashSet::new(),
        }
    }

    /// Add a rule
    pub fn add_rule(&mut self, rule: Rule) {
        self.rules.push(rule);
    }

    /// Add multiple rules
    pub fn add_rules(&mut self, rules: impl IntoIterator<Item = Rule>) {
        self.rules.extend(rules);
    }

    /// Get the rules
    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }

    /// Get the dependency graph
    pub fn dependencies(&self) -> &DependencyGraph {
        &self.dependencies
    }

    /// Get statistics
    pub fn stats(&self) -> &IncrementalStats {
        &self.stats
    }

    /// Initial materialization - compute all inferences from scratch
    pub fn materialize(&mut self, store: &mut Store) -> &IncrementalStats {
        self.stats = IncrementalStats::default();
        self.dependencies = DependencyGraph::new();
        self.derived_signatures.clear();

        // Register all existing triples as asserted
        for triple in store.iter() {
            self.dependencies.add_asserted(triple.clone());
            self.derived_signatures.insert(format!("{:?}", triple));
        }

        // Forward chain until fixpoint
        loop {
            let delta = self.forward_step(store);

            if delta.is_empty() {
                break;
            }

            self.stats.base.steps += 1;

            if self.stats.base.steps >= self.config.max_steps {
                break;
            }
        }

        &self.stats
    }

    /// Perform one forward chaining step
    fn forward_step(&mut self, store: &mut Store) -> Delta {
        let mut delta = Delta::new();

        for (rule_idx, rule) in self.rules.iter().enumerate() {
            // Find all matches for the rule's antecedent
            let matches = self.match_antecedent_with_ids(store, &rule.antecedent);

            for (bindings, premise_ids) in matches {
                // Generate consequent triples
                for pattern in &rule.consequent {
                    let triple = substitute_triple(pattern, &bindings);

                    // Only process ground triples
                    if !triple.is_ground() {
                        continue;
                    }

                    let sig = format!("{:?}", triple);

                    // Check for duplicates
                    if self.derived_signatures.contains(&sig) {
                        // Triple already exists - but we may want to record additional derivation
                        if let Some(id) = self.dependencies.get_id(&triple) {
                            // Add this derivation as an alternative origin
                            self.dependencies.add_derived(
                                triple.clone(),
                                rule_idx,
                                premise_ids.clone(),
                                &bindings,
                            );
                        }
                        continue;
                    }

                    // New triple - add to store and dependency graph
                    self.derived_signatures.insert(sig);
                    store.add(triple.clone());

                    self.dependencies.add_derived(
                        triple.clone(),
                        rule_idx,
                        premise_ids.clone(),
                        &bindings,
                    );

                    delta.additions.push(triple);
                    self.stats.base.triples_derived += 1;
                    self.stats.base.rules_fired += 1;
                }
            }
        }

        delta
    }

    /// Match antecedent patterns and return IDs of matched triples
    fn match_antecedent_with_ids(
        &self,
        store: &Store,
        patterns: &[Triple],
    ) -> Vec<(Bindings, Vec<TripleId>)> {
        if patterns.is_empty() {
            return vec![(Bindings::default(), Vec::new())];
        }

        let first = &patterns[0];
        let rest = &patterns[1..];

        // Check if this is a built-in predicate
        if let Some(builtin_results) = self.try_builtin(first, &Bindings::default()) {
            let mut results = Vec::new();
            for bindings in builtin_results {
                let remaining = self.match_with_bindings_and_ids(store, rest, bindings, Vec::new());
                results.extend(remaining);
            }
            return results;
        }

        // Regular pattern matching
        let first_matches = store.match_pattern(first);

        let mut results = Vec::new();
        for bindings in first_matches {
            let ground_triple = substitute_triple(first, &bindings);
            let premise_id = self.dependencies.get_id(&ground_triple);

            if let Some(id) = premise_id {
                let remaining = self.match_with_bindings_and_ids(store, rest, bindings, vec![id]);
                results.extend(remaining);
            }
        }

        results
    }

    /// Continue matching with partial bindings and premise IDs
    fn match_with_bindings_and_ids(
        &self,
        store: &Store,
        patterns: &[Triple],
        bindings: Bindings,
        premise_ids: Vec<TripleId>,
    ) -> Vec<(Bindings, Vec<TripleId>)> {
        if patterns.is_empty() {
            return vec![(bindings, premise_ids)];
        }

        let pattern = substitute_triple(&patterns[0], &bindings);
        let rest = &patterns[1..];

        // Check for built-in
        if let Some(builtin_results) = self.try_builtin(&pattern, &bindings) {
            let mut results = Vec::new();
            for new_bindings in builtin_results {
                let mut merged = bindings.clone();
                for (var, term) in new_bindings {
                    merged.insert(var, term);
                }
                let remaining = self.match_with_bindings_and_ids(store, rest, merged, premise_ids.clone());
                results.extend(remaining);
            }
            return results;
        }

        // Regular matching
        let matches = store.match_pattern(&pattern);

        let mut results = Vec::new();
        for new_bindings in matches {
            let mut merged = bindings.clone();
            for (var, term) in new_bindings {
                merged.insert(var, term);
            }

            let ground_triple = substitute_triple(&patterns[0], &merged);
            if let Some(id) = self.dependencies.get_id(&ground_triple) {
                let mut new_ids = premise_ids.clone();
                new_ids.push(id);
                let remaining = self.match_with_bindings_and_ids(store, rest, merged, new_ids);
                results.extend(remaining);
            }
        }

        results
    }

    /// Try to evaluate a pattern as a built-in predicate
    fn try_builtin(&self, pattern: &Triple, current_bindings: &Bindings) -> Option<Vec<Bindings>> {
        if let Term::Uri(uri) = &pattern.predicate {
            if self.builtins.is_builtin(uri.as_str()) {
                match self.builtins.evaluate(uri.as_str(), &pattern.subject, &pattern.object, current_bindings) {
                    BuiltinResult::Success(result_bindings) => {
                        return Some(vec![result_bindings]);
                    }
                    BuiltinResult::Failure => {
                        return Some(vec![]);
                    }
                    BuiltinResult::NotReady => {
                        return None;
                    }
                }
            }
        }
        None
    }

    /// Add a triple incrementally
    pub fn add_triple(&mut self, store: &mut Store, triple: Triple) -> Delta {
        let sig = format!("{:?}", triple);

        // Add to store if not already present
        if !self.derived_signatures.contains(&sig) {
            store.add(triple.clone());
            self.derived_signatures.insert(sig);
            self.stats.triples_added += 1;
        }

        // Register as asserted
        self.dependencies.add_asserted(triple.clone());

        // Forward chain from this new triple
        let mut total_delta = Delta::additions(vec![triple]);

        loop {
            let delta = self.forward_step(store);

            if delta.is_empty() {
                break;
            }

            total_delta.merge(delta);
            self.stats.base.steps += 1;

            if self.stats.base.steps >= self.config.max_steps {
                break;
            }
        }

        total_delta
    }

    /// Remove a triple incrementally
    pub fn remove_triple(&mut self, store: &mut Store, triple: &Triple) -> Delta {
        let mut delta = Delta::new();

        // Get the ID for this triple
        let id = match self.dependencies.get_id(triple) {
            Some(id) => id,
            None => return delta, // Triple not in dependency graph
        };

        // Remove the assertion origin
        if !self.dependencies.remove_assertion(id) {
            // Triple still has valid derivations, don't remove
            return delta;
        }

        // Get all affected triples through dependency propagation
        let affected = self.dependencies.invalidate(id);

        // Remove affected triples from store
        for affected_id in affected {
            if let Some(entry) = self.dependencies.get(affected_id) {
                store.remove(&entry.triple);
                self.derived_signatures.remove(&format!("{:?}", entry.triple));
                delta.deletions.push(entry.triple.clone());
                self.stats.triples_retracted += 1;
            }
        }

        // Also remove the original triple
        store.remove(triple);
        self.derived_signatures.remove(&format!("{:?}", triple));
        delta.deletions.push(triple.clone());
        self.stats.triples_removed += 1;

        // Try to re-derive any affected triples through alternative paths
        let rederived = self.try_rederive(store, &delta.deletions);

        // Remove successfully re-derived triples from deletions
        let rederived_sigs: HashSet<String> = rederived.iter()
            .map(|t| format!("{:?}", t))
            .collect();

        delta.deletions.retain(|t| !rederived_sigs.contains(&format!("{:?}", t)));
        delta.additions.extend(rederived);

        delta
    }

    /// Try to re-derive triples that were retracted
    fn try_rederive(&mut self, store: &mut Store, retracted: &[Triple]) -> Vec<Triple> {
        let mut rederived = Vec::new();

        for triple in retracted {
            // Try each rule to see if the triple can be re-derived
            for (rule_idx, rule) in self.rules.iter().enumerate() {
                // Check if this triple matches any consequent pattern
                for pattern in &rule.consequent {
                    if let Some(bindings) = self.unify_triple(pattern, triple) {
                        // Apply bindings to antecedent and check if it matches
                        let antecedent: Vec<Triple> = rule.antecedent
                            .iter()
                            .map(|p| substitute_triple(p, &bindings))
                            .collect();

                        // Check if all antecedent patterns match valid triples
                        let matches = self.match_antecedent_with_ids(store, &antecedent);

                        if !matches.is_empty() {
                            let (_, premise_ids) = &matches[0];

                            // Re-add the triple
                            store.add(triple.clone());
                            self.derived_signatures.insert(format!("{:?}", triple));
                            self.dependencies.add_derived(
                                triple.clone(),
                                rule_idx,
                                premise_ids.clone(),
                                &bindings,
                            );

                            rederived.push(triple.clone());
                            self.stats.triples_rederived += 1;
                            break;
                        }
                    }
                }
            }
        }

        rederived
    }

    /// Unify a pattern with a ground triple
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

    /// Unify a pattern term with a ground term
    fn unify_term(&self, pattern: &Term, ground: &Term, bindings: &mut Bindings) -> bool {
        match pattern {
            Term::Variable(var) => {
                if let Some(existing) = bindings.get(var) {
                    existing == ground
                } else {
                    bindings.insert(var.clone(), ground.clone());
                    true
                }
            }
            _ => pattern == ground,
        }
    }

    /// Apply a delta to the store incrementally
    pub fn apply_delta(&mut self, store: &mut Store, delta: Delta) -> Delta {
        let mut result = Delta::new();

        // Process deletions first
        for triple in delta.deletions {
            let sub_delta = self.remove_triple(store, &triple);
            result.merge(sub_delta);
        }

        // Then process additions
        for triple in delta.additions {
            let sub_delta = self.add_triple(store, triple);
            result.merge(sub_delta);
        }

        result
    }

    /// Get dependency statistics
    pub fn dependency_stats(&self) -> DependencyStats {
        self.dependencies.stats()
    }

    /// Get the builtin registry
    pub fn builtins(&self) -> &BuiltinRegistry {
        &self.builtins
    }
}

impl Default for IncrementalReasoner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_rule(antecedent: Vec<Triple>, consequent: Vec<Triple>) -> Rule {
        Rule::new(antecedent, consequent)
    }

    #[test]
    fn test_initial_materialization() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        ));

        // Human(?x) => Mortal(?x)
        let rule = make_rule(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Human"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Mortal"),
            )],
        );

        let mut reasoner = IncrementalReasoner::new();
        reasoner.add_rule(rule);
        reasoner.materialize(&mut store);

        let mortal = Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Mortal"),
        );
        assert!(store.contains(&mortal));
        assert_eq!(reasoner.stats().base.triples_derived, 1);
    }

    #[test]
    fn test_incremental_addition() {
        let mut store = Store::new();

        // Human(?x) => Mortal(?x)
        let rule = make_rule(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Human"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Mortal"),
            )],
        );

        let mut reasoner = IncrementalReasoner::new();
        reasoner.add_rule(rule);
        reasoner.materialize(&mut store);

        // Initially empty
        assert_eq!(store.len(), 0);

        // Add a human incrementally
        let human = Triple::new(
            Term::uri("http://example.org/plato"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        );

        let delta = reasoner.add_triple(&mut store, human);

        // Should have derived mortal
        assert!(delta.additions.len() >= 1);

        let mortal = Triple::new(
            Term::uri("http://example.org/plato"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Mortal"),
        );
        assert!(store.contains(&mortal));
    }

    #[test]
    fn test_incremental_deletion() {
        let mut store = Store::new();
        let human = Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        );
        store.add(human.clone());

        // Human(?x) => Mortal(?x)
        let rule = make_rule(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Human"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Mortal"),
            )],
        );

        let mut reasoner = IncrementalReasoner::new();
        reasoner.add_rule(rule);
        reasoner.materialize(&mut store);

        let mortal = Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Mortal"),
        );
        assert!(store.contains(&mortal));

        // Remove the human - should retract mortal
        let delta = reasoner.remove_triple(&mut store, &human);

        assert!(!store.contains(&mortal));
        assert!(delta.deletions.len() >= 1);
    }

    #[test]
    fn test_dependency_graph_stats() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/subClassOf"),
            Term::uri("http://example.org/b"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/b"),
            Term::uri("http://example.org/subClassOf"),
            Term::uri("http://example.org/c"),
        ));

        // Transitive closure rule
        let rule = make_rule(
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri("http://example.org/subClassOf"),
                    Term::universal("y"),
                ),
                Triple::new(
                    Term::universal("y"),
                    Term::uri("http://example.org/subClassOf"),
                    Term::universal("z"),
                ),
            ],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/subClassOf"),
                Term::universal("z"),
            )],
        );

        let mut reasoner = IncrementalReasoner::new();
        reasoner.add_rule(rule);
        reasoner.materialize(&mut store);

        let stats = reasoner.dependency_stats();
        assert!(stats.asserted_triples >= 2);
        assert!(stats.derived_triples >= 1);
    }

    #[test]
    fn test_multiple_derivation_paths() {
        let mut store = Store::new();

        // Two ways to derive the same conclusion
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/q"),
            Term::uri("http://example.org/b"),
        ));

        // Rule 1: p(a,b) => r(a,b)
        let rule1 = make_rule(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/p"),
                Term::universal("y"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/r"),
                Term::universal("y"),
            )],
        );

        // Rule 2: q(a,b) => r(a,b)
        let rule2 = make_rule(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/q"),
                Term::universal("y"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/r"),
                Term::universal("y"),
            )],
        );

        let mut reasoner = IncrementalReasoner::new();
        reasoner.add_rule(rule1);
        reasoner.add_rule(rule2);
        reasoner.materialize(&mut store);

        let derived = Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/r"),
            Term::uri("http://example.org/b"),
        );
        assert!(store.contains(&derived));

        // Remove one premise - derived should still exist via other path
        let p_triple = Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        );
        reasoner.remove_triple(&mut store, &p_triple);

        // r(a,b) should still be derivable from q(a,b)
        assert!(store.contains(&derived));
    }
}
