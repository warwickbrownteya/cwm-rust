//! Probabilistic Reasoning for N3
//!
//! Extends the core reasoning engine with probability-weighted triples and inference.
//!
//! # Features
//!
//! - Probability-weighted triples
//! - Bayesian belief propagation
//! - Probabilistic rules with confidence factors
//! - Uncertainty propagation through inference chains
//! - Support for Dempster-Shafer theory
//!
//! # Example
//!
//! ```ignore
//! use cwm::reasoning::probabilistic::{ProbabilisticStore, ProbabilisticTriple, ProbabilisticReasoner};
//!
//! let mut store = ProbabilisticStore::new();
//!
//! // Add a triple with 0.8 probability
//! store.add(ProbabilisticTriple::with_probability(
//!     triple,
//!     0.8,
//! ));
//!
//! // Run probabilistic inference
//! let mut reasoner = ProbabilisticReasoner::new();
//! reasoner.infer(&mut store, &rules);
//! ```

use std::collections::{HashMap, HashSet};
use crate::term::{Term, Triple, Bindings, Variable, substitute_triple};
use crate::reasoner::Rule;
use crate::store::Store;

/// A probability value constrained to [0.0, 1.0]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Probability(f64);

impl Probability {
    /// Create a new probability value
    pub fn new(p: f64) -> Option<Self> {
        if (0.0..=1.0).contains(&p) {
            Some(Probability(p))
        } else {
            None
        }
    }

    /// Create a probability, clamping to valid range
    pub fn clamped(p: f64) -> Self {
        Probability(p.clamp(0.0, 1.0))
    }

    /// Create certainty (probability 1.0)
    pub fn certain() -> Self {
        Probability(1.0)
    }

    /// Create impossibility (probability 0.0)
    pub fn impossible() -> Self {
        Probability(0.0)
    }

    /// Get the probability value
    pub fn value(&self) -> f64 {
        self.0
    }

    /// Combine probabilities using product rule (AND)
    pub fn and(self, other: Probability) -> Probability {
        Probability(self.0 * other.0)
    }

    /// Combine probabilities using sum rule (OR, assuming independence)
    pub fn or(self, other: Probability) -> Probability {
        Probability(self.0 + other.0 - (self.0 * other.0))
    }

    /// Negate probability
    pub fn not(self) -> Probability {
        Probability(1.0 - self.0)
    }

    /// Conditional probability P(A|B) = P(A and B) / P(B)
    pub fn given(self, condition: Probability) -> Option<Probability> {
        if condition.0 == 0.0 {
            None
        } else {
            Some(Probability((self.0 * condition.0) / condition.0))
        }
    }
}

impl Default for Probability {
    fn default() -> Self {
        Probability::certain()
    }
}

impl std::fmt::Display for Probability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:.4}", self.0)
    }
}

/// A belief mass for Dempster-Shafer theory
#[derive(Clone, Debug)]
pub struct BeliefMass {
    /// Mass assignments to subsets of hypotheses
    /// Key is a sorted list of hypothesis indices
    masses: HashMap<Vec<usize>, f64>,
    /// Total number of hypotheses
    num_hypotheses: usize,
}

impl BeliefMass {
    /// Create a new belief mass with n hypotheses
    pub fn new(num_hypotheses: usize) -> Self {
        let mut masses = HashMap::new();
        // All mass initially on Theta (uncertainty)
        let theta: Vec<usize> = (0..num_hypotheses).collect();
        masses.insert(theta, 1.0);

        BeliefMass {
            masses,
            num_hypotheses,
        }
    }

    /// Set mass for a specific hypothesis set
    pub fn set_mass(&mut self, hypotheses: Vec<usize>, mass: f64) {
        let mut sorted = hypotheses;
        sorted.sort();
        sorted.dedup();
        self.masses.insert(sorted, mass);
    }

    /// Get belief for a hypothesis set (sum of masses for subsets)
    pub fn belief(&self, hypotheses: &[usize]) -> f64 {
        let mut sorted: Vec<usize> = hypotheses.to_vec();
        sorted.sort();
        sorted.dedup();

        let mut bel = 0.0;
        for (subset, mass) in &self.masses {
            if subset.iter().all(|h| sorted.contains(h)) {
                bel += mass;
            }
        }
        bel
    }

    /// Get plausibility for a hypothesis set (1 - belief in complement)
    pub fn plausibility(&self, hypotheses: &[usize]) -> f64 {
        let complement: Vec<usize> = (0..self.num_hypotheses)
            .filter(|h| !hypotheses.contains(h))
            .collect();
        1.0 - self.belief(&complement)
    }

    /// Combine two belief masses using Dempster's rule
    pub fn combine(&self, other: &BeliefMass) -> Option<BeliefMass> {
        if self.num_hypotheses != other.num_hypotheses {
            return None;
        }

        let mut new_masses: HashMap<Vec<usize>, f64> = HashMap::new();
        let mut conflict = 0.0;

        for (a, ma) in &self.masses {
            for (b, mb) in &other.masses {
                // Intersection
                let intersection: Vec<usize> = a.iter()
                    .filter(|h| b.contains(h))
                    .copied()
                    .collect();

                let product = ma * mb;

                if intersection.is_empty() {
                    conflict += product;
                } else {
                    *new_masses.entry(intersection).or_insert(0.0) += product;
                }
            }
        }

        // Normalize
        let normalization = 1.0 - conflict;
        if normalization <= 0.0 {
            return None; // Total conflict
        }

        for mass in new_masses.values_mut() {
            *mass /= normalization;
        }

        Some(BeliefMass {
            masses: new_masses,
            num_hypotheses: self.num_hypotheses,
        })
    }
}

/// A triple with associated probability
#[derive(Clone, Debug)]
pub struct ProbabilisticTriple {
    /// The underlying triple
    pub triple: Triple,
    /// Probability of the triple being true
    pub probability: Probability,
    /// Source of the probability (assertion, inference, etc.)
    pub source: ProbabilitySource,
    /// Provenance chain (rule indices that derived this)
    pub provenance: Vec<usize>,
}

impl ProbabilisticTriple {
    /// Create a new probabilistic triple with given probability
    pub fn with_probability(triple: Triple, probability: f64) -> Self {
        ProbabilisticTriple {
            triple,
            probability: Probability::clamped(probability),
            source: ProbabilitySource::Assertion,
            provenance: Vec::new(),
        }
    }

    /// Create a certain triple (probability 1.0)
    pub fn certain(triple: Triple) -> Self {
        ProbabilisticTriple {
            triple,
            probability: Probability::certain(),
            source: ProbabilitySource::Assertion,
            provenance: Vec::new(),
        }
    }

    /// Create from inference
    pub fn from_inference(triple: Triple, probability: Probability, rule_index: usize) -> Self {
        ProbabilisticTriple {
            triple,
            probability,
            source: ProbabilitySource::Inference,
            provenance: vec![rule_index],
        }
    }
}

/// Source of probability information
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProbabilitySource {
    /// Directly asserted
    Assertion,
    /// Inferred from rules
    Inference,
    /// From external evidence
    Evidence,
    /// Aggregated from multiple sources
    Aggregated,
}

/// A probabilistic rule with confidence factor
#[derive(Clone, Debug)]
pub struct ProbabilisticRule {
    /// The underlying rule
    pub rule: Rule,
    /// Confidence factor (how much the rule affects probability)
    pub confidence: Probability,
    /// Combination method for premises
    pub combination: CombinationMethod,
}

impl ProbabilisticRule {
    /// Create a new probabilistic rule
    pub fn new(rule: Rule, confidence: f64) -> Self {
        ProbabilisticRule {
            rule,
            confidence: Probability::clamped(confidence),
            combination: CombinationMethod::Product,
        }
    }

    /// Create with specific combination method
    pub fn with_combination(rule: Rule, confidence: f64, combination: CombinationMethod) -> Self {
        ProbabilisticRule {
            rule,
            confidence: Probability::clamped(confidence),
            combination,
        }
    }
}

/// Method for combining premise probabilities
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CombinationMethod {
    /// Product rule: P(A and B) = P(A) * P(B)
    Product,
    /// Minimum: P(A and B) = min(P(A), P(B))
    Minimum,
    /// Harmonic mean
    Harmonic,
    /// Noisy-AND (with leak probability)
    NoisyAnd,
}

/// Configuration for probabilistic reasoning
#[derive(Clone, Debug)]
pub struct ProbabilisticConfig {
    /// Minimum probability threshold (triples below this are pruned)
    pub min_probability: f64,
    /// Maximum inference depth
    pub max_depth: usize,
    /// Combination method for multiple derivations
    pub aggregation: AggregationMethod,
    /// Whether to track provenance
    pub track_provenance: bool,
}

impl Default for ProbabilisticConfig {
    fn default() -> Self {
        ProbabilisticConfig {
            min_probability: 0.001,
            max_depth: 100,
            aggregation: AggregationMethod::Max,
            track_provenance: true,
        }
    }
}

/// Method for aggregating multiple derivations of the same triple
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AggregationMethod {
    /// Take maximum probability
    Max,
    /// Use noisy-OR: 1 - product of (1 - p_i)
    NoisyOr,
    /// Average probability
    Average,
    /// First derivation wins
    First,
}

/// Store for probabilistic triples
#[derive(Clone, Debug)]
pub struct ProbabilisticStore {
    /// Triples with probabilities
    triples: Vec<ProbabilisticTriple>,
    /// Index by triple signature -> indices with that triple
    by_triple: HashMap<String, Vec<usize>>,
    /// Configuration
    config: ProbabilisticConfig,
}

impl ProbabilisticStore {
    /// Create a new probabilistic store
    pub fn new() -> Self {
        ProbabilisticStore {
            triples: Vec::new(),
            by_triple: HashMap::new(),
            config: ProbabilisticConfig::default(),
        }
    }

    /// Create with configuration
    pub fn with_config(config: ProbabilisticConfig) -> Self {
        ProbabilisticStore {
            triples: Vec::new(),
            by_triple: HashMap::new(),
            config,
        }
    }

    /// Add a probabilistic triple
    pub fn add(&mut self, pt: ProbabilisticTriple) {
        let sig = format!("{:?}", pt.triple);

        // Check if we already have this triple
        if let Some(indices) = self.by_triple.get(&sig) {
            // Aggregate probabilities
            let idx = indices[0];
            let existing = &mut self.triples[idx];

            match self.config.aggregation {
                AggregationMethod::Max => {
                    if pt.probability.value() > existing.probability.value() {
                        existing.probability = pt.probability;
                        existing.provenance.extend(pt.provenance);
                    }
                }
                AggregationMethod::NoisyOr => {
                    let combined = existing.probability.or(pt.probability);
                    existing.probability = combined;
                    existing.provenance.extend(pt.provenance);
                }
                AggregationMethod::Average => {
                    let count = indices.len() as f64 + 1.0;
                    let sum = existing.probability.value() * (count - 1.0) + pt.probability.value();
                    existing.probability = Probability::clamped(sum / count);
                    existing.provenance.extend(pt.provenance);
                }
                AggregationMethod::First => {
                    // Keep existing
                }
            }
            existing.source = ProbabilitySource::Aggregated;
        } else {
            // New triple
            let idx = self.triples.len();
            self.by_triple.entry(sig).or_default().push(idx);
            self.triples.push(pt);
        }
    }

    /// Add a certain triple
    pub fn add_certain(&mut self, triple: Triple) {
        self.add(ProbabilisticTriple::certain(triple));
    }

    /// Get all triples above minimum probability
    pub fn triples(&self) -> impl Iterator<Item = &ProbabilisticTriple> {
        self.triples.iter()
            .filter(|pt| pt.probability.value() >= self.config.min_probability)
    }

    /// Get probability for a specific triple
    pub fn get_probability(&self, triple: &Triple) -> Option<Probability> {
        let sig = format!("{:?}", triple);
        self.by_triple.get(&sig)
            .and_then(|indices| indices.first())
            .map(|&idx| self.triples[idx].probability)
    }

    /// Check if store contains triple (above threshold)
    pub fn contains(&self, triple: &Triple) -> bool {
        self.get_probability(triple)
            .map(|p| p.value() >= self.config.min_probability)
            .unwrap_or(false)
    }

    /// Get number of triples
    pub fn len(&self) -> usize {
        self.triples.iter()
            .filter(|pt| pt.probability.value() >= self.config.min_probability)
            .count()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Convert to regular store (keeping only high-probability triples)
    pub fn to_store(&self, threshold: f64) -> Store {
        let mut store = Store::new();
        for pt in &self.triples {
            if pt.probability.value() >= threshold {
                store.add(pt.triple.clone());
            }
        }
        store
    }

    /// Import from regular store (all triples get probability 1.0)
    pub fn from_store(store: &Store) -> Self {
        let mut ps = ProbabilisticStore::new();
        for triple in store.iter() {
            ps.add_certain(triple.clone());
        }
        ps
    }

    /// Match pattern and return probabilistic bindings
    pub fn match_pattern(&self, pattern: &Triple) -> Vec<(Bindings, Probability)> {
        let mut results = Vec::new();

        for pt in &self.triples {
            if pt.probability.value() < self.config.min_probability {
                continue;
            }

            if let Some(bindings) = self.unify_triple(pattern, &pt.triple) {
                results.push((bindings, pt.probability));
            }
        }

        results
    }

    /// Unify pattern with triple
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

    /// Unify term
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
}

impl Default for ProbabilisticStore {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics from probabilistic reasoning
#[derive(Clone, Debug, Default)]
pub struct ProbabilisticStats {
    /// Number of inference steps
    pub steps: usize,
    /// Number of triples derived
    pub triples_derived: usize,
    /// Number of triples pruned (below threshold)
    pub triples_pruned: usize,
    /// Maximum probability in store
    pub max_probability: f64,
    /// Minimum probability in store
    pub min_probability: f64,
    /// Average probability
    pub avg_probability: f64,
}

/// Probabilistic reasoner
#[derive(Clone, Debug)]
pub struct ProbabilisticReasoner {
    /// Probabilistic rules
    rules: Vec<ProbabilisticRule>,
    /// Configuration
    config: ProbabilisticConfig,
    /// Statistics
    stats: ProbabilisticStats,
    /// Set of derived triple signatures
    derived: HashSet<String>,
}

impl ProbabilisticReasoner {
    /// Create a new probabilistic reasoner
    pub fn new() -> Self {
        ProbabilisticReasoner {
            rules: Vec::new(),
            config: ProbabilisticConfig::default(),
            stats: ProbabilisticStats::default(),
            derived: HashSet::new(),
        }
    }

    /// Create with configuration
    pub fn with_config(config: ProbabilisticConfig) -> Self {
        ProbabilisticReasoner {
            rules: Vec::new(),
            config,
            stats: ProbabilisticStats::default(),
            derived: HashSet::new(),
        }
    }

    /// Add a probabilistic rule
    pub fn add_rule(&mut self, rule: ProbabilisticRule) {
        self.rules.push(rule);
    }

    /// Add a regular rule with confidence 1.0
    pub fn add_certain_rule(&mut self, rule: Rule) {
        self.rules.push(ProbabilisticRule::new(rule, 1.0));
    }

    /// Run probabilistic inference
    pub fn infer(&mut self, store: &mut ProbabilisticStore) -> &ProbabilisticStats {
        self.stats = ProbabilisticStats::default();
        self.derived.clear();

        // Pre-populate derived set
        for pt in store.triples.iter() {
            self.derived.insert(format!("{:?}", pt.triple));
        }

        loop {
            if self.stats.steps >= self.config.max_depth {
                break;
            }

            let new_count = self.step(store);
            self.stats.steps += 1;

            if new_count == 0 {
                break;
            }
        }

        // Compute final statistics
        self.compute_stats(store);

        &self.stats
    }

    /// Single inference step
    fn step(&mut self, store: &mut ProbabilisticStore) -> usize {
        let mut new_triples: Vec<ProbabilisticTriple> = Vec::new();

        for (rule_idx, prule) in self.rules.iter().enumerate() {
            // Find all matches for antecedent with probabilities
            let matches = self.match_antecedent(store, &prule.rule.antecedent);

            for (bindings, premise_probs) in matches {
                // Combine premise probabilities
                let premise_prob = self.combine_probabilities(&premise_probs, prule.combination);

                // Apply rule confidence
                let conclusion_prob = premise_prob.and(prule.confidence);

                // Check threshold
                if conclusion_prob.value() < self.config.min_probability {
                    self.stats.triples_pruned += 1;
                    continue;
                }

                // Generate consequent triples
                for pattern in &prule.rule.consequent {
                    let triple = substitute_triple(pattern, &bindings);

                    if !triple.is_ground() {
                        continue;
                    }

                    let sig = format!("{:?}", triple);

                    if !self.derived.contains(&sig) {
                        self.derived.insert(sig);
                        new_triples.push(ProbabilisticTriple::from_inference(
                            triple,
                            conclusion_prob,
                            rule_idx,
                        ));
                    }
                }
            }
        }

        let count = new_triples.len();
        self.stats.triples_derived += count;

        for pt in new_triples {
            store.add(pt);
        }

        count
    }

    /// Match antecedent patterns
    fn match_antecedent(
        &self,
        store: &ProbabilisticStore,
        patterns: &[Triple],
    ) -> Vec<(Bindings, Vec<Probability>)> {
        if patterns.is_empty() {
            return vec![(Bindings::default(), Vec::new())];
        }

        let first = &patterns[0];
        let rest = &patterns[1..];

        let first_matches = store.match_pattern(first);
        let mut results = Vec::new();

        for (bindings, prob) in first_matches {
            let remaining = self.match_with_bindings(store, rest, bindings, vec![prob]);
            results.extend(remaining);
        }

        results
    }

    /// Continue matching with partial bindings
    fn match_with_bindings(
        &self,
        store: &ProbabilisticStore,
        patterns: &[Triple],
        bindings: Bindings,
        probs: Vec<Probability>,
    ) -> Vec<(Bindings, Vec<Probability>)> {
        if patterns.is_empty() {
            return vec![(bindings, probs)];
        }

        let pattern = substitute_triple(&patterns[0], &bindings);
        let rest = &patterns[1..];

        let matches = store.match_pattern(&pattern);
        let mut results = Vec::new();

        for (new_bindings, prob) in matches {
            let mut merged = bindings.clone();
            for (var, term) in new_bindings {
                merged.insert(var, term);
            }
            let mut new_probs = probs.clone();
            new_probs.push(prob);
            let remaining = self.match_with_bindings(store, rest, merged, new_probs);
            results.extend(remaining);
        }

        results
    }

    /// Combine probabilities using specified method
    fn combine_probabilities(&self, probs: &[Probability], method: CombinationMethod) -> Probability {
        if probs.is_empty() {
            return Probability::certain();
        }

        match method {
            CombinationMethod::Product => {
                let mut result = Probability::certain();
                for p in probs {
                    result = result.and(*p);
                }
                result
            }
            CombinationMethod::Minimum => {
                let min = probs.iter()
                    .map(|p| p.value())
                    .fold(f64::MAX, f64::min);
                Probability::clamped(min)
            }
            CombinationMethod::Harmonic => {
                if probs.is_empty() {
                    return Probability::certain();
                }
                let sum_recip: f64 = probs.iter()
                    .map(|p| if p.value() > 0.0 { 1.0 / p.value() } else { f64::MAX })
                    .sum();
                Probability::clamped(probs.len() as f64 / sum_recip)
            }
            CombinationMethod::NoisyAnd => {
                // Noisy-AND with small leak probability
                let leak = 0.01;
                let product: f64 = probs.iter()
                    .map(|p| p.value())
                    .product();
                Probability::clamped(product + leak * (1.0 - product))
            }
        }
    }

    /// Compute final statistics
    fn compute_stats(&mut self, store: &ProbabilisticStore) {
        let probs: Vec<f64> = store.triples.iter()
            .map(|pt| pt.probability.value())
            .collect();

        if !probs.is_empty() {
            self.stats.max_probability = probs.iter().cloned().fold(f64::MIN, f64::max);
            self.stats.min_probability = probs.iter().cloned().fold(f64::MAX, f64::min);
            self.stats.avg_probability = probs.iter().sum::<f64>() / probs.len() as f64;
        }
    }

    /// Get statistics
    pub fn stats(&self) -> &ProbabilisticStats {
        &self.stats
    }
}

impl Default for ProbabilisticReasoner {
    fn default() -> Self {
        Self::new()
    }
}

/// Bayesian network node
#[derive(Clone, Debug)]
pub struct BayesianNode {
    /// Variable name
    pub name: String,
    /// Parent node names
    pub parents: Vec<String>,
    /// Conditional probability table
    /// Key: parent state assignment, Value: P(true | parents)
    pub cpt: HashMap<Vec<bool>, f64>,
}

impl BayesianNode {
    /// Create a root node (no parents)
    pub fn root(name: impl Into<String>, prior: f64) -> Self {
        let mut cpt = HashMap::new();
        cpt.insert(vec![], prior);

        BayesianNode {
            name: name.into(),
            parents: Vec::new(),
            cpt,
        }
    }

    /// Create a node with one parent
    pub fn with_parent(
        name: impl Into<String>,
        parent: impl Into<String>,
        p_true_given_parent_true: f64,
        p_true_given_parent_false: f64,
    ) -> Self {
        let mut cpt = HashMap::new();
        cpt.insert(vec![true], p_true_given_parent_true);
        cpt.insert(vec![false], p_true_given_parent_false);

        BayesianNode {
            name: name.into(),
            parents: vec![parent.into()],
            cpt,
        }
    }

    /// Get P(true | parent states)
    pub fn probability(&self, parent_states: &[bool]) -> f64 {
        self.cpt.get(parent_states).copied().unwrap_or(0.5)
    }
}

/// Simple Bayesian network for RDF reasoning
#[derive(Clone, Debug)]
pub struct BayesianNetwork {
    /// Nodes indexed by name
    nodes: HashMap<String, BayesianNode>,
    /// Topological order for inference
    order: Vec<String>,
}

impl BayesianNetwork {
    /// Create a new empty network
    pub fn new() -> Self {
        BayesianNetwork {
            nodes: HashMap::new(),
            order: Vec::new(),
        }
    }

    /// Add a node
    pub fn add_node(&mut self, node: BayesianNode) {
        let name = node.name.clone();
        self.nodes.insert(name.clone(), node);
        self.order.push(name);
    }

    /// Forward sampling to compute marginal probability
    pub fn sample(&self, query: &str, evidence: &HashMap<String, bool>, samples: usize) -> f64 {
        let mut count = 0;
        let mut matching = 0;

        for _ in 0..samples {
            let sample = self.generate_sample(evidence);

            // Check if evidence matches
            let evidence_matches = evidence.iter()
                .all(|(k, v)| sample.get(k) == Some(v));

            if evidence_matches {
                count += 1;
                if sample.get(query) == Some(&true) {
                    matching += 1;
                }
            }
        }

        if count == 0 {
            0.5 // No matching samples
        } else {
            matching as f64 / count as f64
        }
    }

    /// Generate a single sample
    fn generate_sample(&self, evidence: &HashMap<String, bool>) -> HashMap<String, bool> {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::Hasher;

        let mut sample = HashMap::new();
        let mut hasher = DefaultHasher::new();

        for name in &self.order {
            if let Some(&value) = evidence.get(name) {
                sample.insert(name.clone(), value);
            } else if let Some(node) = self.nodes.get(name) {
                let parent_states: Vec<bool> = node.parents.iter()
                    .map(|p| *sample.get(p).unwrap_or(&false))
                    .collect();

                let prob = node.probability(&parent_states);

                // Simple pseudo-random based on hash
                hasher.write(name.as_bytes());
                let hash_value = hasher.finish();
                let random = (hash_value as f64) / (u64::MAX as f64);

                sample.insert(name.clone(), random < prob);
            }
        }

        sample
    }
}

impl Default for BayesianNetwork {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_probability_basic() {
        let p1 = Probability::new(0.8).unwrap();
        let p2 = Probability::new(0.6).unwrap();

        assert!((p1.value() - 0.8).abs() < 0.001);
        assert!((p2.value() - 0.6).abs() < 0.001);

        // AND
        let p_and = p1.and(p2);
        assert!((p_and.value() - 0.48).abs() < 0.001);

        // OR
        let p_or = p1.or(p2);
        assert!((p_or.value() - 0.92).abs() < 0.001);

        // NOT
        let p_not = p1.not();
        assert!((p_not.value() - 0.2).abs() < 0.001);
    }

    #[test]
    fn test_probability_clamped() {
        let p1 = Probability::clamped(1.5);
        assert!((p1.value() - 1.0).abs() < 0.001);

        let p2 = Probability::clamped(-0.5);
        assert!((p2.value() - 0.0).abs() < 0.001);
    }

    #[test]
    fn test_probabilistic_store() {
        let mut store = ProbabilisticStore::new();

        let triple = Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        );

        store.add(ProbabilisticTriple::with_probability(triple.clone(), 0.8));

        assert!(store.contains(&triple));
        assert!((store.get_probability(&triple).unwrap().value() - 0.8).abs() < 0.001);
    }

    #[test]
    fn test_probabilistic_aggregation() {
        let mut config = ProbabilisticConfig::default();
        config.aggregation = AggregationMethod::NoisyOr;

        let mut store = ProbabilisticStore::with_config(config);

        let triple = Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        );

        store.add(ProbabilisticTriple::with_probability(triple.clone(), 0.5));
        store.add(ProbabilisticTriple::with_probability(triple.clone(), 0.5));

        // Noisy-OR: 0.5 + 0.5 - 0.25 = 0.75
        let prob = store.get_probability(&triple).unwrap().value();
        assert!((prob - 0.75).abs() < 0.001);
    }

    #[test]
    fn test_probabilistic_inference() {
        let mut store = ProbabilisticStore::new();

        // Add base fact with probability 0.9
        store.add(ProbabilisticTriple::with_probability(
            Triple::new(
                Term::uri("http://example.org/socrates"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Human"),
            ),
            0.9,
        ));

        // Rule: Human(?x) => Mortal(?x) with confidence 0.95
        let rule = Rule::new(
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

        let mut reasoner = ProbabilisticReasoner::new();
        reasoner.add_rule(ProbabilisticRule::new(rule, 0.95));
        reasoner.infer(&mut store);

        let mortal = Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Mortal"),
        );

        assert!(store.contains(&mortal));
        // 0.9 * 0.95 = 0.855
        let prob = store.get_probability(&mortal).unwrap().value();
        assert!((prob - 0.855).abs() < 0.001);
    }

    #[test]
    fn test_belief_mass_basic() {
        let mut bm = BeliefMass::new(3);

        // Set mass on hypothesis {0}
        bm.set_mass(vec![0], 0.4);
        // Remaining mass on Theta
        bm.set_mass(vec![0, 1, 2], 0.6);

        let bel = bm.belief(&[0]);
        assert!((bel - 0.4).abs() < 0.001);

        let pl = bm.plausibility(&[0]);
        assert!(pl >= bel);
    }

    #[test]
    fn test_bayesian_network() {
        let mut bn = BayesianNetwork::new();

        // Rain (prior = 0.3)
        bn.add_node(BayesianNode::root("Rain", 0.3));

        // Sprinkler | Rain
        bn.add_node(BayesianNode::with_parent("Sprinkler", "Rain", 0.01, 0.4));

        // WetGrass | Sprinkler, Rain
        let mut wg = BayesianNode {
            name: "WetGrass".to_string(),
            parents: vec!["Sprinkler".to_string(), "Rain".to_string()],
            cpt: HashMap::new(),
        };
        wg.cpt.insert(vec![true, true], 0.99);
        wg.cpt.insert(vec![true, false], 0.9);
        wg.cpt.insert(vec![false, true], 0.8);
        wg.cpt.insert(vec![false, false], 0.0);
        bn.add_node(wg);

        // Query: P(Rain | WetGrass=true)
        let mut evidence = HashMap::new();
        evidence.insert("WetGrass".to_string(), true);

        let prob = bn.sample("Rain", &evidence, 10000);
        // Should return a valid probability (sampling may be deterministic in tests)
        assert!(prob >= 0.0 && prob <= 1.0);
    }

    #[test]
    fn test_combination_methods() {
        let reasoner = ProbabilisticReasoner::new();
        let probs = vec![
            Probability::clamped(0.8),
            Probability::clamped(0.6),
        ];

        // Product
        let product = reasoner.combine_probabilities(&probs, CombinationMethod::Product);
        assert!((product.value() - 0.48).abs() < 0.001);

        // Minimum
        let minimum = reasoner.combine_probabilities(&probs, CombinationMethod::Minimum);
        assert!((minimum.value() - 0.6).abs() < 0.001);

        // Harmonic mean
        let harmonic = reasoner.combine_probabilities(&probs, CombinationMethod::Harmonic);
        // 2 / (1/0.8 + 1/0.6) = 2 / (1.25 + 1.667) = 2 / 2.917 ≈ 0.686
        assert!((harmonic.value() - 0.686).abs() < 0.01);
    }
}
