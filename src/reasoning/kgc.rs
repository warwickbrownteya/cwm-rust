//! Knowledge Graph Completion Module
//!
//! This module provides knowledge graph completion capabilities:
//! - Link prediction
//! - Entity type inference
//! - Relation extraction
//! - Path-based reasoning
//! - Graph pattern mining

use crate::term::{Term, Triple};
use std::collections::{HashMap, HashSet, VecDeque};

/// Confidence score for predictions
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Confidence(f64);

impl Confidence {
    pub fn new(value: f64) -> Self {
        Self(value.clamp(0.0, 1.0))
    }

    pub fn value(&self) -> f64 {
        self.0
    }

    pub fn combine_and(&self, other: &Self) -> Self {
        Self::new(self.0 * other.0)
    }

    pub fn combine_or(&self, other: &Self) -> Self {
        Self::new(1.0 - (1.0 - self.0) * (1.0 - other.0))
    }

    pub fn combine_avg(&self, other: &Self) -> Self {
        Self::new((self.0 + other.0) / 2.0)
    }
}

impl Default for Confidence {
    fn default() -> Self {
        Self(0.0)
    }
}

/// A predicted triple with confidence
#[derive(Debug, Clone)]
pub struct Prediction {
    /// The predicted triple
    pub triple: Triple,
    /// Confidence in the prediction
    pub confidence: Confidence,
    /// Evidence supporting the prediction
    pub evidence: Vec<Evidence>,
    /// Method used for prediction
    pub method: PredictionMethod,
}

impl Prediction {
    pub fn new(triple: Triple, confidence: Confidence, method: PredictionMethod) -> Self {
        Self {
            triple,
            confidence,
            evidence: Vec::new(),
            method,
        }
    }

    pub fn with_evidence(mut self, evidence: Vec<Evidence>) -> Self {
        self.evidence = evidence;
        self
    }
}

/// Evidence for a prediction
#[derive(Debug, Clone)]
pub enum Evidence {
    /// Path in the graph
    Path(Vec<Triple>),
    /// Similar entities
    SimilarEntity(Term, f64),
    /// Pattern match
    PatternMatch(Pattern),
    /// Rule application
    RuleApplication(String),
    /// Statistical co-occurrence
    CoOccurrence(usize),
}

/// Method used for prediction
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PredictionMethod {
    PathRanking,
    TypeInference,
    Similarity,
    RuleMining,
    Statistical,
    Ensemble,
}

/// Graph pattern for mining
#[derive(Debug, Clone)]
pub struct Pattern {
    /// Pattern triples (with variables)
    pub triples: Vec<Triple>,
    /// Support (number of matches)
    pub support: usize,
    /// Confidence
    pub confidence: Confidence,
}

/// Entity statistics
#[derive(Debug, Clone, Default)]
pub struct EntityStats {
    /// Outgoing predicates and their objects
    pub outgoing: HashMap<String, HashSet<String>>,
    /// Incoming predicates and their subjects
    pub incoming: HashMap<String, HashSet<String>>,
    /// Types of this entity
    pub types: HashSet<String>,
}

/// Relation statistics
#[derive(Debug, Clone, Default)]
pub struct RelationStats {
    /// Domain types (subject types)
    pub domain_types: HashMap<String, usize>,
    /// Range types (object types)
    pub range_types: HashMap<String, usize>,
    /// Total count
    pub count: usize,
    /// Functional score
    pub functional_score: f64,
    /// Inverse functional score
    pub inverse_functional_score: f64,
}

/// A learned rule for prediction
#[derive(Debug, Clone)]
pub struct LearnedRule {
    /// Body patterns
    pub body: Vec<Triple>,
    /// Head pattern
    pub head: Triple,
    /// Support count
    pub support: usize,
    /// Confidence
    pub confidence: Confidence,
    /// Head coverage
    pub head_coverage: f64,
}

/// A path pattern between entities
#[derive(Debug, Clone)]
pub struct PathPattern {
    /// Sequence of predicates
    pub predicates: Vec<String>,
    /// Directions (true = forward, false = backward)
    pub directions: Vec<bool>,
    /// Count of occurrences
    pub count: usize,
    /// Confidence for prediction
    pub confidence: Confidence,
}

/// Configuration for KGC
#[derive(Debug, Clone)]
pub struct KGCConfig {
    /// Maximum path length for path ranking
    pub max_path_length: usize,
    /// Minimum support for rule mining
    pub min_support: usize,
    /// Minimum confidence for predictions
    pub min_confidence: f64,
    /// Number of top predictions to return
    pub top_k: usize,
}

impl Default for KGCConfig {
    fn default() -> Self {
        Self {
            max_path_length: 3,
            min_support: 2,
            min_confidence: 0.5,
            top_k: 10,
        }
    }
}

/// Graph statistics
#[derive(Debug, Clone)]
pub struct GraphStatistics {
    pub triple_count: usize,
    pub entity_count: usize,
    pub relation_count: usize,
    pub rule_count: usize,
}

/// Knowledge Graph Completion Engine
pub struct KGCompletionEngine {
    /// All triples in the graph
    triples: HashSet<Triple>,
    /// Entity statistics
    entity_stats: HashMap<String, EntityStats>,
    /// Relation statistics
    relation_stats: HashMap<String, RelationStats>,
    /// Learned rules for prediction
    learned_rules: Vec<LearnedRule>,
    /// Configuration
    config: KGCConfig,
}

impl KGCompletionEngine {
    pub fn new() -> Self {
        Self::with_config(KGCConfig::default())
    }

    pub fn with_config(config: KGCConfig) -> Self {
        Self {
            triples: HashSet::new(),
            entity_stats: HashMap::new(),
            relation_stats: HashMap::new(),
            learned_rules: Vec::new(),
            config,
        }
    }

    fn term_to_string(term: &Term) -> String {
        match term {
            Term::Uri(u) => u.as_str().to_string(),
            Term::BlankNode(b) => format!("_:{}", b.id()),
            Term::Literal(l) => l.value().to_string(),
            Term::Variable(v) => format!("?{}", v.name()),
            _ => String::new(),
        }
    }

    /// Add a triple to the graph
    pub fn add_triple(&mut self, triple: Triple) {
        let subj_iri = Self::term_to_string(&triple.subject);
        let pred_iri = Self::term_to_string(&triple.predicate);
        let obj_iri = Self::term_to_string(&triple.object);

        // Update subject stats
        let subj_stats = self.entity_stats.entry(subj_iri.clone()).or_default();
        subj_stats.outgoing.entry(pred_iri.clone()).or_default().insert(obj_iri.clone());

        // Update object stats
        let obj_stats = self.entity_stats.entry(obj_iri.clone()).or_default();
        obj_stats.incoming.entry(pred_iri.clone()).or_default().insert(subj_iri.clone());

        // Track types
        if pred_iri.contains("type") || pred_iri.contains("instanceOf") || pred_iri.ends_with("/a") {
            let subj_stats = self.entity_stats.entry(subj_iri).or_default();
            subj_stats.types.insert(obj_iri);
        }

        // Update relation stats
        let rel_stats = self.relation_stats.entry(pred_iri).or_default();
        rel_stats.count += 1;

        self.triples.insert(triple);
    }

    /// Add multiple triples
    pub fn add_triples(&mut self, triples: impl IntoIterator<Item = Triple>) {
        for triple in triples {
            self.add_triple(triple);
        }
    }

    /// Predict missing links for a subject and predicate (tail prediction)
    pub fn predict_tail(&self, subject: &Term, predicate: &Term) -> Vec<Prediction> {
        let mut predictions = Vec::new();
        let subj_iri = Self::term_to_string(subject);
        let pred_iri = Self::term_to_string(predicate);

        // Type-based prediction
        predictions.extend(self.predict_by_type(&subj_iri, &pred_iri, true));

        self.deduplicate_predictions(predictions)
    }

    /// Predict missing links for a predicate and object (head prediction)
    pub fn predict_head(&self, predicate: &Term, object: &Term) -> Vec<Prediction> {
        let mut predictions = Vec::new();
        let pred_iri = Self::term_to_string(predicate);
        let obj_iri = Self::term_to_string(object);

        predictions.extend(self.predict_by_type(&obj_iri, &pred_iri, false));

        self.deduplicate_predictions(predictions)
    }

    fn predict_by_type(&self, entity: &str, predicate: &str, is_tail: bool) -> Vec<Prediction> {
        let mut predictions = Vec::new();

        let entity_types = self.entity_stats.get(entity)
            .map(|s| &s.types)
            .cloned()
            .unwrap_or_default();

        let rel_stats = match self.relation_stats.get(predicate) {
            Some(s) => s,
            None => return predictions,
        };

        let type_map = if is_tail { &rel_stats.range_types } else { &rel_stats.domain_types };

        for (entity_iri, stats) in &self.entity_stats {
            if entity_iri == entity {
                continue;
            }

            let overlap: usize = stats.types.iter()
                .filter(|t| type_map.contains_key(*t))
                .count();

            if overlap > 0 {
                let confidence = Confidence::new(overlap as f64 / stats.types.len().max(1) as f64);
                if confidence.value() >= self.config.min_confidence {
                    let triple = if is_tail {
                        Triple {
                            subject: Term::uri(entity),
                            predicate: Term::uri(predicate),
                            object: Term::uri(entity_iri),
                        }
                    } else {
                        Triple {
                            subject: Term::uri(entity_iri),
                            predicate: Term::uri(predicate),
                            object: Term::uri(entity),
                        }
                    };

                    if !self.triples.contains(&triple) {
                        predictions.push(Prediction::new(triple, confidence, PredictionMethod::TypeInference));
                    }
                }
            }
        }

        predictions
    }

    fn deduplicate_predictions(&self, mut predictions: Vec<Prediction>) -> Vec<Prediction> {
        predictions.sort_by(|a, b| b.confidence.value().partial_cmp(&a.confidence.value()).unwrap());

        let mut seen = HashSet::new();
        let mut result = Vec::new();

        for pred in predictions {
            let key = format!("{:?}", pred.triple);
            if !seen.contains(&key) {
                seen.insert(key);
                result.push(pred);
            }

            if result.len() >= self.config.top_k {
                break;
            }
        }

        result
    }

    /// Infer entity types
    pub fn infer_types(&self, entity: &Term) -> Vec<(String, Confidence)> {
        let entity_iri = Self::term_to_string(entity);
        let mut type_scores: HashMap<String, f64> = HashMap::new();

        let stats = match self.entity_stats.get(&entity_iri) {
            Some(s) => s,
            None => return Vec::new(),
        };

        for t in &stats.types {
            *type_scores.entry(t.clone()).or_insert(0.0) += 1.0;
        }

        let max_score = type_scores.values().cloned().fold(0.0, f64::max);
        let mut results: Vec<_> = type_scores
            .into_iter()
            .map(|(t, s)| (t, Confidence::new(s / max_score.max(1.0))))
            .filter(|(_, c)| c.value() >= self.config.min_confidence)
            .collect();

        results.sort_by(|a, b| b.1.value().partial_cmp(&a.1.value()).unwrap());
        results
    }

    /// Get all learned rules
    pub fn learned_rules(&self) -> &[LearnedRule] {
        &self.learned_rules
    }

    /// Get graph statistics
    pub fn statistics(&self) -> GraphStatistics {
        GraphStatistics {
            triple_count: self.triples.len(),
            entity_count: self.entity_stats.len(),
            relation_count: self.relation_stats.len(),
            rule_count: self.learned_rules.len(),
        }
    }
}

impl Default for KGCompletionEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_triple(s: &str, p: &str, o: &str) -> Triple {
        Triple {
            subject: Term::uri(s),
            predicate: Term::uri(p),
            object: Term::uri(o),
        }
    }

    #[test]
    fn test_add_triples() {
        let mut engine = KGCompletionEngine::new();

        engine.add_triple(make_triple(":Alice", ":knows", ":Bob"));
        engine.add_triple(make_triple(":Bob", ":knows", ":Charlie"));
        engine.add_triple(make_triple(":Alice", "rdf:type", ":Person"));

        let stats = engine.statistics();
        assert_eq!(stats.triple_count, 3);
        assert_eq!(stats.entity_count, 4);
    }

    #[test]
    fn test_confidence() {
        let c1 = Confidence::new(0.8);
        let c2 = Confidence::new(0.6);

        let and = c1.combine_and(&c2);
        assert!((and.value() - 0.48).abs() < 0.001);

        let or = c1.combine_or(&c2);
        assert!((or.value() - 0.92).abs() < 0.001);

        let avg = c1.combine_avg(&c2);
        assert!((avg.value() - 0.7).abs() < 0.001);
    }

    #[test]
    fn test_graph_statistics() {
        let mut engine = KGCompletionEngine::new();

        engine.add_triple(make_triple(":A", ":p1", ":B"));
        engine.add_triple(make_triple(":B", ":p2", ":C"));
        engine.add_triple(make_triple(":C", ":p1", ":D"));

        let stats = engine.statistics();
        assert_eq!(stats.triple_count, 3);
        assert_eq!(stats.entity_count, 4);
        assert_eq!(stats.relation_count, 2);
    }
}
