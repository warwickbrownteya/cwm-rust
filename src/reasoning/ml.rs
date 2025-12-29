//! Machine Learning Integration for N3 Reasoning
//!
//! Embedding-based similarity, ML-guided rule selection, and pattern learning.
//!
//! # Features
//!
//! - Entity and relation embeddings
//! - Similarity-based pattern matching
//! - ML-guided rule prioritization
//! - Pattern learning from data
//! - Link prediction support
//!
//! # Example
//!
//! ```ignore
//! use cwm::reasoning::ml::{EmbeddingSpace, MLReasoner, EmbeddingConfig};
//!
//! let mut embeddings = EmbeddingSpace::new(128);
//!
//! // Train embeddings from knowledge graph
//! embeddings.train(&store);
//!
//! // Use for similarity queries
//! let similar = embeddings.most_similar("http://example.org/entity", 10);
//! ```

use std::collections::{HashMap, HashSet};
use crate::term::{Term, Triple, Bindings};
use crate::store::Store;
use crate::reasoner::Rule;

/// A vector embedding
#[derive(Clone, Debug)]
pub struct Embedding {
    /// The embedding vector
    pub vector: Vec<f64>,
    /// Entity or relation this embedding represents
    pub entity: String,
}

impl Embedding {
    /// Create a new embedding
    pub fn new(entity: impl Into<String>, dimensions: usize) -> Self {
        Embedding {
            vector: vec![0.0; dimensions],
            entity: entity.into(),
        }
    }

    /// Create with specific values
    pub fn with_values(entity: impl Into<String>, vector: Vec<f64>) -> Self {
        Embedding {
            vector,
            entity: entity.into(),
        }
    }

    /// Initialize with random values
    pub fn random(entity: impl Into<String>, dimensions: usize, seed: u64) -> Self {
        let entity_str = entity.into();
        let mut vector = Vec::with_capacity(dimensions);

        // Simple pseudo-random based on seed and index
        for i in 0..dimensions {
            let x = ((seed.wrapping_mul(1103515245).wrapping_add(12345 + i as u64)) % 1000) as f64 / 1000.0;
            vector.push(x * 2.0 - 1.0); // Range [-1, 1]
        }

        // Normalize
        let norm: f64 = vector.iter().map(|x| x * x).sum::<f64>().sqrt();
        if norm > 0.0 {
            for v in &mut vector {
                *v /= norm;
            }
        }

        Embedding {
            vector,
            entity: entity_str,
        }
    }

    /// Get dimensionality
    pub fn dimensions(&self) -> usize {
        self.vector.len()
    }

    /// Compute dot product with another embedding
    pub fn dot(&self, other: &Embedding) -> f64 {
        self.vector.iter()
            .zip(other.vector.iter())
            .map(|(a, b)| a * b)
            .sum()
    }

    /// Compute cosine similarity with another embedding
    pub fn cosine_similarity(&self, other: &Embedding) -> f64 {
        let dot = self.dot(other);
        let norm_a: f64 = self.vector.iter().map(|x| x * x).sum::<f64>().sqrt();
        let norm_b: f64 = other.vector.iter().map(|x| x * x).sum::<f64>().sqrt();

        if norm_a > 0.0 && norm_b > 0.0 {
            dot / (norm_a * norm_b)
        } else {
            0.0
        }
    }

    /// Compute Euclidean distance
    pub fn euclidean_distance(&self, other: &Embedding) -> f64 {
        self.vector.iter()
            .zip(other.vector.iter())
            .map(|(a, b)| (a - b).powi(2))
            .sum::<f64>()
            .sqrt()
    }

    /// Add another embedding
    pub fn add(&self, other: &Embedding) -> Embedding {
        let vector: Vec<f64> = self.vector.iter()
            .zip(other.vector.iter())
            .map(|(a, b)| a + b)
            .collect();

        Embedding {
            vector,
            entity: format!("{}+{}", self.entity, other.entity),
        }
    }

    /// Subtract another embedding
    pub fn subtract(&self, other: &Embedding) -> Embedding {
        let vector: Vec<f64> = self.vector.iter()
            .zip(other.vector.iter())
            .map(|(a, b)| a - b)
            .collect();

        Embedding {
            vector,
            entity: format!("{}-{}", self.entity, other.entity),
        }
    }

    /// Normalize to unit length
    pub fn normalize(&mut self) {
        let norm: f64 = self.vector.iter().map(|x| x * x).sum::<f64>().sqrt();
        if norm > 0.0 {
            for v in &mut self.vector {
                *v /= norm;
            }
        }
    }
}

/// Configuration for embedding space
#[derive(Clone, Debug)]
pub struct EmbeddingConfig {
    /// Embedding dimensions
    pub dimensions: usize,
    /// Learning rate
    pub learning_rate: f64,
    /// Number of training epochs
    pub epochs: usize,
    /// Negative sampling ratio
    pub negative_samples: usize,
    /// Margin for margin-based loss
    pub margin: f64,
    /// Regularization factor
    pub regularization: f64,
}

impl Default for EmbeddingConfig {
    fn default() -> Self {
        EmbeddingConfig {
            dimensions: 128,
            learning_rate: 0.01,
            epochs: 100,
            negative_samples: 5,
            margin: 1.0,
            regularization: 0.001,
        }
    }
}

/// Embedding space for entities and relations
#[derive(Clone, Debug)]
pub struct EmbeddingSpace {
    /// Entity embeddings
    entities: HashMap<String, Embedding>,
    /// Relation embeddings
    relations: HashMap<String, Embedding>,
    /// Configuration
    config: EmbeddingConfig,
    /// Training statistics
    stats: TrainingStats,
}

impl EmbeddingSpace {
    /// Create a new embedding space
    pub fn new(dimensions: usize) -> Self {
        EmbeddingSpace {
            entities: HashMap::new(),
            relations: HashMap::new(),
            config: EmbeddingConfig {
                dimensions,
                ..Default::default()
            },
            stats: TrainingStats::default(),
        }
    }

    /// Create with configuration
    pub fn with_config(config: EmbeddingConfig) -> Self {
        EmbeddingSpace {
            entities: HashMap::new(),
            relations: HashMap::new(),
            config,
            stats: TrainingStats::default(),
        }
    }

    /// Get entity embedding
    pub fn entity(&self, uri: &str) -> Option<&Embedding> {
        self.entities.get(uri)
    }

    /// Get relation embedding
    pub fn relation(&self, uri: &str) -> Option<&Embedding> {
        self.relations.get(uri)
    }

    /// Initialize embeddings from a store
    pub fn initialize(&mut self, store: &Store) {
        let mut entities = HashSet::new();
        let mut relations = HashSet::new();

        // Collect entities and relations
        for triple in store.iter() {
            if let Term::Uri(uri) = &triple.subject {
                entities.insert(uri.as_str().to_string());
            }
            if let Term::Uri(uri) = &triple.predicate {
                relations.insert(uri.as_str().to_string());
            }
            if let Term::Uri(uri) = &triple.object {
                entities.insert(uri.as_str().to_string());
            }
        }

        // Create random embeddings
        let mut seed = 42u64;
        for entity in entities {
            seed = seed.wrapping_mul(1103515245).wrapping_add(12345);
            self.entities.insert(
                entity.clone(),
                Embedding::random(&entity, self.config.dimensions, seed),
            );
        }

        for relation in relations {
            seed = seed.wrapping_mul(1103515245).wrapping_add(12345);
            self.relations.insert(
                relation.clone(),
                Embedding::random(&relation, self.config.dimensions, seed),
            );
        }
    }

    /// Train embeddings using TransE-style algorithm
    pub fn train(&mut self, store: &Store) -> &TrainingStats {
        self.stats = TrainingStats::default();

        // Initialize if needed
        if self.entities.is_empty() {
            self.initialize(store);
        }

        let triples: Vec<_> = store.iter().collect();

        for epoch in 0..self.config.epochs {
            let mut epoch_loss = 0.0;

            for triple in &triples {
                // Get URIs
                let (subj, pred, obj) = match (&triple.subject, &triple.predicate, &triple.object) {
                    (Term::Uri(s), Term::Uri(p), Term::Uri(o)) => {
                        (s.as_str(), p.as_str(), o.as_str())
                    }
                    _ => continue,
                };

                // TransE: h + r ≈ t
                // Loss = max(0, margin + d(h+r, t) - d(h'+r, t'))
                if let (Some(h), Some(r), Some(t)) = (
                    self.entities.get(subj),
                    self.relations.get(pred),
                    self.entities.get(obj),
                ) {
                    let positive_score = self.score_triple(h, r, t);

                    // Generate negative sample
                    let neg_obj = self.sample_negative_entity(obj);
                    if let Some(t_neg) = self.entities.get(&neg_obj) {
                        let negative_score = self.score_triple(h, r, t_neg);

                        // Margin loss
                        let loss = (self.config.margin + positive_score - negative_score).max(0.0);
                        epoch_loss += loss;

                        // Update embeddings (gradient descent)
                        if loss > 0.0 {
                            self.update_embeddings(subj, pred, obj, &neg_obj);
                        }
                    }
                }
            }

            self.stats.epoch_losses.push(epoch_loss / triples.len() as f64);
            self.stats.epochs = epoch + 1;
        }

        &self.stats
    }

    /// Score a triple using TransE
    fn score_triple(&self, h: &Embedding, r: &Embedding, t: &Embedding) -> f64 {
        // TransE score: ||h + r - t||
        h.add(r).euclidean_distance(t)
    }

    /// Sample a negative entity
    fn sample_negative_entity(&self, exclude: &str) -> String {
        // Simple random sampling
        let entities: Vec<_> = self.entities.keys()
            .filter(|e| e.as_str() != exclude)
            .collect();

        if entities.is_empty() {
            return exclude.to_string();
        }

        // Use hash for pseudo-random selection
        let idx = (exclude.len() * 31) % entities.len();
        entities[idx].clone()
    }

    /// Update embeddings with gradient descent
    fn update_embeddings(&mut self, subj: &str, pred: &str, obj: &str, neg_obj: &str) {
        let lr = self.config.learning_rate;

        // Get current embeddings
        let h = self.entities.get(subj).cloned();
        let r = self.relations.get(pred).cloned();
        let t = self.entities.get(obj).cloned();
        let t_neg = self.entities.get(neg_obj).cloned();

        if let (Some(h), Some(r), Some(t), Some(t_neg)) = (h, r, t, t_neg) {
            // Gradient for positive triple
            let h_plus_r = h.add(&r);
            let gradient: Vec<f64> = h_plus_r.vector.iter()
                .zip(t.vector.iter())
                .map(|(hr, t_v)| hr - t_v)
                .collect();

            // Gradient for negative triple
            let neg_gradient: Vec<f64> = h_plus_r.vector.iter()
                .zip(t_neg.vector.iter())
                .map(|(hr, t_v)| hr - t_v)
                .collect();

            // Update h
            if let Some(h_emb) = self.entities.get_mut(subj) {
                for (i, v) in h_emb.vector.iter_mut().enumerate() {
                    *v -= lr * (gradient[i] - neg_gradient[i]);
                }
            }

            // Update r
            if let Some(r_emb) = self.relations.get_mut(pred) {
                for (i, v) in r_emb.vector.iter_mut().enumerate() {
                    *v -= lr * (gradient[i] - neg_gradient[i]);
                }
            }

            // Update t
            if let Some(t_emb) = self.entities.get_mut(obj) {
                for (i, v) in t_emb.vector.iter_mut().enumerate() {
                    *v += lr * gradient[i];
                }
            }

            // Update t_neg
            if let Some(t_neg_emb) = self.entities.get_mut(neg_obj) {
                for (i, v) in t_neg_emb.vector.iter_mut().enumerate() {
                    *v -= lr * neg_gradient[i];
                }
            }
        }
    }

    /// Find most similar entities
    pub fn most_similar(&self, entity: &str, top_k: usize) -> Vec<(String, f64)> {
        let query = match self.entities.get(entity) {
            Some(e) => e,
            None => return Vec::new(),
        };

        let mut similarities: Vec<(String, f64)> = self.entities.iter()
            .filter(|(k, _)| k.as_str() != entity)
            .map(|(k, v)| (k.clone(), query.cosine_similarity(v)))
            .collect();

        similarities.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        similarities.truncate(top_k);

        similarities
    }

    /// Predict link (tail entity given head and relation)
    pub fn predict_link(&self, head: &str, relation: &str, top_k: usize) -> Vec<(String, f64)> {
        let h = match self.entities.get(head) {
            Some(e) => e,
            None => return Vec::new(),
        };

        let r = match self.relations.get(relation) {
            Some(e) => e,
            None => return Vec::new(),
        };

        let h_plus_r = h.add(r);

        let mut predictions: Vec<(String, f64)> = self.entities.iter()
            .filter(|(k, _)| k.as_str() != head)
            .map(|(k, v)| (k.clone(), -h_plus_r.euclidean_distance(v)))
            .collect();

        predictions.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        predictions.truncate(top_k);

        predictions
    }

    /// Check if a triple is plausible
    pub fn is_plausible(&self, head: &str, relation: &str, tail: &str, threshold: f64) -> bool {
        if let (Some(h), Some(r), Some(t)) = (
            self.entities.get(head),
            self.relations.get(relation),
            self.entities.get(tail),
        ) {
            self.score_triple(h, r, t) < threshold
        } else {
            false
        }
    }

    /// Get embedding dimensions
    pub fn dimensions(&self) -> usize {
        self.config.dimensions
    }

    /// Get number of entities
    pub fn num_entities(&self) -> usize {
        self.entities.len()
    }

    /// Get number of relations
    pub fn num_relations(&self) -> usize {
        self.relations.len()
    }
}

impl Default for EmbeddingSpace {
    fn default() -> Self {
        Self::new(128)
    }
}

/// Training statistics
#[derive(Clone, Debug, Default)]
pub struct TrainingStats {
    /// Number of epochs completed
    pub epochs: usize,
    /// Loss per epoch
    pub epoch_losses: Vec<f64>,
}

/// ML-guided rule prioritization
#[derive(Clone, Debug)]
pub struct RulePrioritizer {
    /// Rule scores
    scores: HashMap<usize, f64>,
    /// Rule usage counts
    usage: HashMap<usize, usize>,
    /// Rule success rates
    success_rates: HashMap<usize, f64>,
}

impl RulePrioritizer {
    /// Create a new prioritizer
    pub fn new() -> Self {
        RulePrioritizer {
            scores: HashMap::new(),
            usage: HashMap::new(),
            success_rates: HashMap::new(),
        }
    }

    /// Record rule usage
    pub fn record_usage(&mut self, rule_idx: usize, success: bool) {
        *self.usage.entry(rule_idx).or_insert(0) += 1;

        let total = *self.usage.get(&rule_idx).unwrap() as f64;
        let current_rate = *self.success_rates.get(&rule_idx).unwrap_or(&0.0);

        // Update success rate with exponential moving average
        let alpha = 0.1;
        let new_rate = if success {
            current_rate * (1.0 - alpha) + alpha
        } else {
            current_rate * (1.0 - alpha)
        };

        self.success_rates.insert(rule_idx, new_rate);

        // Update score
        let score = new_rate * (total + 1.0).ln();
        self.scores.insert(rule_idx, score);
    }

    /// Get rule priority score
    pub fn score(&self, rule_idx: usize) -> f64 {
        *self.scores.get(&rule_idx).unwrap_or(&0.0)
    }

    /// Sort rules by priority
    pub fn prioritize<'a>(&self, rules: &'a [Rule]) -> Vec<(usize, &'a Rule)> {
        let mut indexed: Vec<(usize, &'a Rule)> = rules.iter()
            .enumerate()
            .collect();

        indexed.sort_by(|(a, _), (b, _)| {
            let score_a = self.score(*a);
            let score_b = self.score(*b);
            score_b.partial_cmp(&score_a).unwrap()
        });

        indexed
    }
}

impl Default for RulePrioritizer {
    fn default() -> Self {
        Self::new()
    }
}

/// Pattern learner for discovering rules from data
#[derive(Clone, Debug)]
pub struct PatternLearner {
    /// Minimum support threshold
    min_support: f64,
    /// Minimum confidence threshold
    min_confidence: f64,
    /// Maximum pattern length
    max_length: usize,
    /// Learned patterns
    patterns: Vec<LearnedPattern>,
}

impl PatternLearner {
    /// Create a new pattern learner
    pub fn new(min_support: f64, min_confidence: f64) -> Self {
        PatternLearner {
            min_support,
            min_confidence,
            max_length: 3,
            patterns: Vec::new(),
        }
    }

    /// Learn patterns from a store
    pub fn learn(&mut self, store: &Store) -> Vec<LearnedPattern> {
        self.patterns.clear();

        // Count predicate pairs
        let pairs = self.count_pairs(store);

        // Generate candidate rules
        for ((p1, p2), (support, confidence)) in pairs {
            if support >= self.min_support && confidence >= self.min_confidence {
                let pattern = LearnedPattern {
                    antecedent: vec![p1],
                    consequent: p2,
                    support,
                    confidence,
                };
                self.patterns.push(pattern);
            }
        }

        // Sort by confidence
        self.patterns.sort_by(|a, b| b.confidence.partial_cmp(&a.confidence).unwrap());

        self.patterns.clone()
    }

    /// Count predicate pairs for pattern mining
    fn count_pairs(&self, store: &Store) -> HashMap<(String, String), (f64, f64)> {
        let mut pair_counts: HashMap<(String, String), usize> = HashMap::new();
        let mut pred_counts: HashMap<String, usize> = HashMap::new();

        // Index triples by subject
        let mut by_subject: HashMap<String, Vec<String>> = HashMap::new();

        for triple in store.iter() {
            if let (Term::Uri(subj), Term::Uri(pred)) = (&triple.subject, &triple.predicate) {
                by_subject.entry(subj.as_str().to_string())
                    .or_default()
                    .push(pred.as_str().to_string());

                *pred_counts.entry(pred.as_str().to_string()).or_insert(0) += 1;
            }
        }

        // Count co-occurrences
        for predicates in by_subject.values() {
            for p1 in predicates {
                for p2 in predicates {
                    if p1 != p2 {
                        *pair_counts.entry((p1.clone(), p2.clone())).or_insert(0) += 1;
                    }
                }
            }
        }

        // Compute support and confidence
        let total = store.len() as f64;
        let mut results = HashMap::new();

        for ((p1, p2), count) in pair_counts {
            let support = count as f64 / total;
            let p1_count = *pred_counts.get(&p1).unwrap_or(&1) as f64;
            let confidence = count as f64 / p1_count;

            results.insert((p1, p2), (support, confidence));
        }

        results
    }

    /// Convert learned patterns to rules
    pub fn to_rules(&self) -> Vec<Rule> {
        self.patterns.iter()
            .map(|p| p.to_rule())
            .collect()
    }
}

impl Default for PatternLearner {
    fn default() -> Self {
        Self::new(0.01, 0.5)
    }
}

/// A learned pattern
#[derive(Clone, Debug)]
pub struct LearnedPattern {
    /// Antecedent predicates
    pub antecedent: Vec<String>,
    /// Consequent predicate
    pub consequent: String,
    /// Support (frequency of pattern)
    pub support: f64,
    /// Confidence (conditional probability)
    pub confidence: f64,
}

impl LearnedPattern {
    /// Convert to a rule
    pub fn to_rule(&self) -> Rule {
        let antecedent: Vec<Triple> = self.antecedent.iter()
            .map(|pred| Triple::new(
                Term::universal("x"),
                Term::uri(pred),
                Term::universal("y"),
            ))
            .collect();

        let consequent = vec![Triple::new(
            Term::universal("x"),
            Term::uri(&self.consequent),
            Term::universal("z"),
        )];

        Rule::new(antecedent, consequent)
    }
}

/// ML-enhanced reasoner
#[derive(Clone)]
pub struct MLReasoner {
    /// Embedding space
    embeddings: EmbeddingSpace,
    /// Rule prioritizer
    prioritizer: RulePrioritizer,
    /// Pattern learner
    learner: PatternLearner,
    /// Rules
    rules: Vec<Rule>,
    /// Configuration
    config: MLReasonerConfig,
}

impl MLReasoner {
    /// Create a new ML reasoner
    pub fn new() -> Self {
        MLReasoner {
            embeddings: EmbeddingSpace::default(),
            prioritizer: RulePrioritizer::new(),
            learner: PatternLearner::default(),
            rules: Vec::new(),
            config: MLReasonerConfig::default(),
        }
    }

    /// Train from a store
    pub fn train(&mut self, store: &Store) {
        // Train embeddings
        self.embeddings.train(store);

        // Learn patterns
        if self.config.learn_patterns {
            let patterns = self.learner.learn(store);
            for pattern in patterns {
                self.rules.push(pattern.to_rule());
            }
        }
    }

    /// Add a rule
    pub fn add_rule(&mut self, rule: Rule) {
        self.rules.push(rule);
    }

    /// Infer new triples using ML guidance
    pub fn infer(&mut self, store: &mut Store) -> MLInferenceStats {
        let mut stats = MLInferenceStats::default();

        // Get prioritized rule indices (clone rules to avoid borrow conflicts)
        let prioritized_indices: Vec<usize> = {
            let prioritized = self.prioritizer.prioritize(&self.rules);
            prioritized.into_iter().map(|(idx, _)| idx).collect()
        };

        for rule_idx in prioritized_indices {
            let rule = self.rules[rule_idx].clone();

            // Match antecedent
            let matches = store.query(&rule.antecedent);

            for bindings in matches {
                // Generate consequent
                for consequent in &rule.consequent {
                    let triple = crate::term::substitute_triple(consequent, &bindings);

                    if !triple.is_ground() {
                        continue;
                    }

                    // Check plausibility using embeddings
                    if self.config.use_embeddings {
                        if let (Term::Uri(s), Term::Uri(p), Term::Uri(o)) =
                            (&triple.subject, &triple.predicate, &triple.object)
                        {
                            if !self.embeddings.is_plausible(
                                s.as_str(),
                                p.as_str(),
                                o.as_str(),
                                self.config.plausibility_threshold,
                            ) {
                                stats.filtered_implausible += 1;
                                continue;
                            }
                        }
                    }

                    if !store.contains(&triple) {
                        store.add(triple);
                        stats.triples_derived += 1;
                        self.prioritizer.record_usage(rule_idx, true);
                    }
                }
            }
        }

        stats
    }

    /// Get embedding space
    pub fn embeddings(&self) -> &EmbeddingSpace {
        &self.embeddings
    }

    /// Get mutable embedding space
    pub fn embeddings_mut(&mut self) -> &mut EmbeddingSpace {
        &mut self.embeddings
    }
}

impl Default for MLReasoner {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for MLReasoner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MLReasoner")
            .field("embeddings", &self.embeddings.num_entities())
            .field("rules", &self.rules.len())
            .finish()
    }
}

/// Configuration for ML reasoner
#[derive(Clone, Debug)]
pub struct MLReasonerConfig {
    /// Use embeddings for plausibility checking
    pub use_embeddings: bool,
    /// Plausibility threshold
    pub plausibility_threshold: f64,
    /// Learn patterns from data
    pub learn_patterns: bool,
}

impl Default for MLReasonerConfig {
    fn default() -> Self {
        MLReasonerConfig {
            use_embeddings: true,
            plausibility_threshold: 3.0,
            learn_patterns: false,
        }
    }
}

/// Statistics from ML-guided inference
#[derive(Clone, Debug, Default)]
pub struct MLInferenceStats {
    /// Triples derived
    pub triples_derived: usize,
    /// Triples filtered as implausible
    pub filtered_implausible: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_embedding_basic() {
        let e1 = Embedding::with_values("a", vec![1.0, 0.0, 0.0]);
        let e2 = Embedding::with_values("b", vec![0.0, 1.0, 0.0]);
        let e3 = Embedding::with_values("c", vec![1.0, 0.0, 0.0]);

        assert!((e1.cosine_similarity(&e2) - 0.0).abs() < 0.001);
        assert!((e1.cosine_similarity(&e3) - 1.0).abs() < 0.001);
    }

    #[test]
    fn test_embedding_distance() {
        let e1 = Embedding::with_values("a", vec![0.0, 0.0]);
        let e2 = Embedding::with_values("b", vec![3.0, 4.0]);

        assert!((e1.euclidean_distance(&e2) - 5.0).abs() < 0.001);
    }

    #[test]
    fn test_embedding_space() {
        let mut space = EmbeddingSpace::new(32);
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

        space.initialize(&store);

        assert_eq!(space.num_entities(), 3);
        assert_eq!(space.num_relations(), 1);
    }

    #[test]
    fn test_embedding_training() {
        let mut space = EmbeddingSpace::with_config(EmbeddingConfig {
            dimensions: 16,
            epochs: 10,
            ..Default::default()
        });

        let mut store = Store::new();

        for i in 0..10 {
            store.add(Triple::new(
                Term::uri(format!("http://example.org/e{}", i)),
                Term::uri("http://example.org/knows"),
                Term::uri(format!("http://example.org/e{}", (i + 1) % 10)),
            ));
        }

        let stats = space.train(&store);
        assert_eq!(stats.epochs, 10);
    }

    #[test]
    fn test_most_similar() {
        let mut space = EmbeddingSpace::new(16);

        // Add embeddings manually for testing
        space.entities.insert(
            "a".to_string(),
            Embedding::with_values("a", vec![1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                                              0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]),
        );
        space.entities.insert(
            "b".to_string(),
            Embedding::with_values("b", vec![0.9, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                                              0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]),
        );
        space.entities.insert(
            "c".to_string(),
            Embedding::with_values("c", vec![0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                                              0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]),
        );

        let similar = space.most_similar("a", 2);
        assert_eq!(similar.len(), 2);
        assert_eq!(similar[0].0, "b"); // b should be most similar to a
    }

    #[test]
    fn test_rule_prioritizer() {
        let mut prioritizer = RulePrioritizer::new();

        prioritizer.record_usage(0, true);
        prioritizer.record_usage(0, true);
        prioritizer.record_usage(1, false);

        assert!(prioritizer.score(0) > prioritizer.score(1));
    }

    #[test]
    fn test_pattern_learner() {
        let mut store = Store::new();

        // Create pattern: entities with type Person often have knows
        for i in 0..10 {
            let entity = format!("http://example.org/person{}", i);
            store.add(Triple::new(
                Term::uri(&entity),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Person"),
            ));
            store.add(Triple::new(
                Term::uri(&entity),
                Term::uri("http://example.org/knows"),
                Term::uri(format!("http://example.org/person{}", (i + 1) % 10)),
            ));
        }

        let mut learner = PatternLearner::new(0.01, 0.1);
        let patterns = learner.learn(&store);

        assert!(!patterns.is_empty());
    }

    #[test]
    fn test_ml_reasoner() {
        let mut reasoner = MLReasoner::new();
        let mut store = Store::new();

        store.add(Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        ));

        reasoner.add_rule(Rule::new(
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
        ));

        // Disable embedding check since we haven't trained
        reasoner.config.use_embeddings = false;

        let stats = reasoner.infer(&mut store);
        assert_eq!(stats.triples_derived, 1);
    }
}
