//! Natural Language Interface for N3 Reasoning
//!
//! Rule translation, query generation, and natural language explanations.
//!
//! # Features
//!
//! - Natural language query parsing
//! - Rule verbalization
//! - Human-readable explanation generation
//! - Template-based NLG
//! - Entity mention detection
//!
//! # Example
//!
//! ```ignore
//! use cwm::reasoning::nlp::{NLInterface, QueryParser, RuleVerbalizer};
//!
//! let interface = NLInterface::new();
//!
//! // Parse natural language query
//! let patterns = interface.parse_query("Who knows Bob?")?;
//!
//! // Verbalize a rule
//! let text = interface.verbalize_rule(&rule);
//! ```

use std::collections::HashMap;
use crate::term::{Term, Triple, Bindings};
use crate::reasoner::{Rule, Proof, ProofStep};
use crate::store::Store;

/// Natural language interface
#[derive(Clone, Debug)]
pub struct NLInterface {
    /// Query parser
    parser: QueryParser,
    /// Rule verbalizer
    verbalizer: RuleVerbalizer,
    /// Explanation generator
    explainer: NLExplainer,
    /// Vocabulary for entities
    vocabulary: Vocabulary,
}

impl NLInterface {
    /// Create a new NL interface
    pub fn new() -> Self {
        NLInterface {
            parser: QueryParser::new(),
            verbalizer: RuleVerbalizer::new(),
            explainer: NLExplainer::new(),
            vocabulary: Vocabulary::new(),
        }
    }

    /// Parse a natural language query into triple patterns
    pub fn parse_query(&self, query: &str) -> Result<Vec<Triple>, NLError> {
        self.parser.parse(query)
    }

    /// Verbalize a rule into natural language
    pub fn verbalize_rule(&self, rule: &Rule) -> String {
        self.verbalizer.verbalize(rule)
    }

    /// Generate natural language explanation for a proof
    pub fn explain_proof(&self, proof: &Proof) -> String {
        self.explainer.explain(proof)
    }

    /// Generate explanation for a single derivation
    pub fn explain_derivation(&self, step: &ProofStep) -> String {
        self.explainer.explain_step(step)
    }

    /// Add vocabulary mapping
    pub fn add_vocabulary(&mut self, uri: &str, label: &str, description: Option<&str>) {
        self.vocabulary.add(uri, label, description);
    }

    /// Get vocabulary
    pub fn vocabulary(&self) -> &Vocabulary {
        &self.vocabulary
    }

    /// Get mutable vocabulary
    pub fn vocabulary_mut(&mut self) -> &mut Vocabulary {
        &mut self.vocabulary
    }
}

impl Default for NLInterface {
    fn default() -> Self {
        Self::new()
    }
}

/// Query parser for natural language
#[derive(Clone, Debug)]
pub struct QueryParser {
    /// Keyword patterns
    patterns: Vec<QueryPattern>,
    /// Namespace prefixes
    prefixes: HashMap<String, String>,
}

impl QueryParser {
    /// Create a new query parser
    pub fn new() -> Self {
        let mut parser = QueryParser {
            patterns: Vec::new(),
            prefixes: HashMap::new(),
        };

        // Add default patterns
        parser.add_default_patterns();

        parser
    }

    /// Add default query patterns
    fn add_default_patterns(&mut self) {
        // "Who knows X?"
        self.patterns.push(QueryPattern {
            regex: r"(?i)who\s+knows\s+(.+)\??".to_string(),
            template: QueryTemplate::SubjectQuery {
                predicate: "http://xmlns.com/foaf/0.1/knows".to_string(),
                object_var: 1,
            },
        });

        // "What is X?"
        self.patterns.push(QueryPattern {
            regex: r"(?i)what\s+is\s+(.+)\??".to_string(),
            template: QueryTemplate::TypeQuery {
                subject_var: 1,
            },
        });

        // "Is X a Y?"
        self.patterns.push(QueryPattern {
            regex: r"(?i)is\s+(.+)\s+a\s+(.+)\??".to_string(),
            template: QueryTemplate::TypeCheck {
                subject_var: 1,
                type_var: 2,
            },
        });

        // "Who does X know?"
        self.patterns.push(QueryPattern {
            regex: r"(?i)who\s+does\s+(.+)\s+know\??".to_string(),
            template: QueryTemplate::ObjectQuery {
                subject_var: 1,
                predicate: "http://xmlns.com/foaf/0.1/knows".to_string(),
            },
        });

        // "Find all X with property Y"
        self.patterns.push(QueryPattern {
            regex: r"(?i)find\s+all\s+(.+)\s+with\s+(.+)".to_string(),
            template: QueryTemplate::PropertyQuery {
                subject_type_var: 1,
                property_var: 2,
            },
        });
    }

    /// Parse a natural language query
    pub fn parse(&self, query: &str) -> Result<Vec<Triple>, NLError> {
        let query = query.trim();

        for pattern in &self.patterns {
            if let Some(captures) = self.match_pattern(query, &pattern.regex) {
                return self.apply_template(&pattern.template, &captures);
            }
        }

        // Try keyword extraction as fallback
        self.parse_keywords(query)
    }

    /// Match a regex pattern
    fn match_pattern(&self, text: &str, pattern: &str) -> Option<Vec<String>> {
        // Simple pattern matching (not full regex for portability)
        let pattern_lower = pattern.to_lowercase();
        let text_lower = text.to_lowercase();

        // Check for "who knows X" pattern
        if pattern_lower.contains("who") && pattern_lower.contains("knows") {
            if text_lower.starts_with("who knows ") {
                let rest = &text[10..].trim_end_matches('?').trim();
                return Some(vec![rest.to_string()]);
            }
        }

        // Check for "what is X" pattern
        if pattern_lower.contains("what") && pattern_lower.contains("is") {
            if text_lower.starts_with("what is ") {
                let rest = &text[8..].trim_end_matches('?').trim();
                return Some(vec![rest.to_string()]);
            }
        }

        // Check for "is X a Y" pattern
        // The regex contains "\s+a\s+" so we check for that or literal " a "
        if pattern_lower.contains("is") && (pattern_lower.contains(" a ") || pattern_lower.contains("\\s+a\\s+")) {
            if text_lower.starts_with("is ") {
                let rest = &text[3..].trim_end_matches('?').trim();
                if let Some(idx) = rest.to_lowercase().find(" a ") {
                    let subject = rest[..idx].trim();
                    let obj_type = rest[idx + 3..].trim();
                    return Some(vec![subject.to_string(), obj_type.to_string()]);
                }
            }
        }

        None
    }

    /// Apply a query template
    fn apply_template(
        &self,
        template: &QueryTemplate,
        captures: &[String],
    ) -> Result<Vec<Triple>, NLError> {
        match template {
            QueryTemplate::SubjectQuery { predicate, object_var } => {
                let object = captures.get(*object_var - 1)
                    .ok_or(NLError::ParseError("Missing object".to_string()))?;

                Ok(vec![Triple::new(
                    Term::universal("x"),
                    Term::uri(predicate),
                    self.term_from_string(object),
                )])
            }
            QueryTemplate::ObjectQuery { subject_var, predicate } => {
                let subject = captures.get(*subject_var - 1)
                    .ok_or(NLError::ParseError("Missing subject".to_string()))?;

                Ok(vec![Triple::new(
                    self.term_from_string(subject),
                    Term::uri(predicate),
                    Term::universal("x"),
                )])
            }
            QueryTemplate::TypeQuery { subject_var } => {
                let subject = captures.get(*subject_var - 1)
                    .ok_or(NLError::ParseError("Missing subject".to_string()))?;

                Ok(vec![Triple::new(
                    self.term_from_string(subject),
                    Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                    Term::universal("type"),
                )])
            }
            QueryTemplate::TypeCheck { subject_var, type_var } => {
                let subject = captures.get(*subject_var - 1)
                    .ok_or(NLError::ParseError("Missing subject".to_string()))?;
                let type_name = captures.get(*type_var - 1)
                    .ok_or(NLError::ParseError("Missing type".to_string()))?;

                Ok(vec![Triple::new(
                    self.term_from_string(subject),
                    Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                    self.term_from_string(type_name),
                )])
            }
            QueryTemplate::PropertyQuery { subject_type_var, property_var } => {
                let subject_type = captures.get(*subject_type_var - 1)
                    .ok_or(NLError::ParseError("Missing subject type".to_string()))?;
                let property = captures.get(*property_var - 1)
                    .ok_or(NLError::ParseError("Missing property".to_string()))?;

                Ok(vec![
                    Triple::new(
                        Term::universal("x"),
                        Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                        self.term_from_string(subject_type),
                    ),
                    Triple::new(
                        Term::universal("x"),
                        self.term_from_string(property),
                        Term::universal("y"),
                    ),
                ])
            }
        }
    }

    /// Convert string to term
    fn term_from_string(&self, s: &str) -> Term {
        let s = s.trim();

        // Check for URI
        if s.starts_with('<') && s.ends_with('>') {
            return Term::uri(&s[1..s.len()-1]);
        }

        // Check for prefixed name
        if let Some(idx) = s.find(':') {
            let prefix = &s[..idx];
            let local = &s[idx+1..];
            if let Some(ns) = self.prefixes.get(prefix) {
                return Term::uri(format!("{}{}", ns, local));
            }
        }

        // Check for common entity patterns
        let normalized = s.to_lowercase().replace(' ', "_");
        if s.starts_with("http://") || s.starts_with("https://") {
            Term::uri(s)
        } else {
            // Create a URI from the entity name
            Term::uri(format!("http://example.org/{}", normalized))
        }
    }

    /// Parse using keyword extraction
    fn parse_keywords(&self, query: &str) -> Result<Vec<Triple>, NLError> {
        let words: Vec<&str> = query.split_whitespace().collect();

        if words.is_empty() {
            return Err(NLError::ParseError("Empty query".to_string()));
        }

        // Simple heuristic: first noun phrase is subject, verb is predicate, rest is object
        let mut subject = None;
        let mut predicate = None;
        let mut object = None;

        for word in words {
            let word_lower = word.to_lowercase();
            let word_clean = word_lower.trim_matches(|c: char| !c.is_alphanumeric());

            // Skip common words
            if ["the", "a", "an", "is", "are", "was", "were", "what", "who", "how"].contains(&word_clean) {
                continue;
            }

            if subject.is_none() {
                subject = Some(word_clean.to_string());
            } else if predicate.is_none() {
                predicate = Some(word_clean.to_string());
            } else if object.is_none() {
                object = Some(word_clean.to_string());
            }
        }

        let subj = subject.map(|s| Term::uri(format!("http://example.org/{}", s)))
            .unwrap_or_else(|| Term::universal("s"));
        let pred = predicate.map(|p| Term::uri(format!("http://example.org/{}", p)))
            .unwrap_or_else(|| Term::universal("p"));
        let obj = object.map(|o| Term::uri(format!("http://example.org/{}", o)))
            .unwrap_or_else(|| Term::universal("o"));

        Ok(vec![Triple::new(subj, pred, obj)])
    }

    /// Add a namespace prefix
    pub fn add_prefix(&mut self, prefix: &str, namespace: &str) {
        self.prefixes.insert(prefix.to_string(), namespace.to_string());
    }

    /// Add a custom query pattern
    pub fn add_pattern(&mut self, pattern: QueryPattern) {
        self.patterns.push(pattern);
    }
}

impl Default for QueryParser {
    fn default() -> Self {
        Self::new()
    }
}

/// A query pattern
#[derive(Clone, Debug)]
pub struct QueryPattern {
    /// Regex pattern
    pub regex: String,
    /// Template for generating triples
    pub template: QueryTemplate,
}

/// Template for query generation
#[derive(Clone, Debug)]
pub enum QueryTemplate {
    /// Query for subjects: ?x predicate object
    SubjectQuery {
        predicate: String,
        object_var: usize,
    },
    /// Query for objects: subject predicate ?x
    ObjectQuery {
        subject_var: usize,
        predicate: String,
    },
    /// Query for type: subject rdf:type ?type
    TypeQuery {
        subject_var: usize,
    },
    /// Check type: subject rdf:type type
    TypeCheck {
        subject_var: usize,
        type_var: usize,
    },
    /// Query with property: ?x type ?; ?x property ?y
    PropertyQuery {
        subject_type_var: usize,
        property_var: usize,
    },
}

/// Rule verbalizer
#[derive(Clone, Debug)]
pub struct RuleVerbalizer {
    /// Vocabulary for URIs
    vocabulary: Vocabulary,
    /// Templates for different rule patterns
    templates: HashMap<String, String>,
}

impl RuleVerbalizer {
    /// Create a new verbalizer
    pub fn new() -> Self {
        let mut verbalizer = RuleVerbalizer {
            vocabulary: Vocabulary::new(),
            templates: HashMap::new(),
        };

        // Add common vocabulary
        verbalizer.vocabulary.add(
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
            "is a",
            Some("type relationship"),
        );
        verbalizer.vocabulary.add(
            "http://www.w3.org/2000/01/rdf-schema#subClassOf",
            "is a subclass of",
            Some("subclass relationship"),
        );
        verbalizer.vocabulary.add(
            "http://xmlns.com/foaf/0.1/knows",
            "knows",
            Some("knows relationship"),
        );

        verbalizer
    }

    /// Verbalize a rule
    pub fn verbalize(&self, rule: &Rule) -> String {
        let ant = self.verbalize_patterns(&rule.antecedent, "and");
        let cons = self.verbalize_patterns(&rule.consequent, "and");

        format!("If {} then {}.", ant, cons)
    }

    /// Verbalize a list of triple patterns
    fn verbalize_patterns(&self, patterns: &[Triple], connector: &str) -> String {
        if patterns.is_empty() {
            return "nothing".to_string();
        }

        let verbalized: Vec<String> = patterns.iter()
            .map(|p| self.verbalize_triple(p))
            .collect();

        if verbalized.len() == 1 {
            verbalized[0].clone()
        } else {
            verbalized.join(&format!(" {} ", connector))
        }
    }

    /// Verbalize a single triple
    fn verbalize_triple(&self, triple: &Triple) -> String {
        let subj = self.verbalize_term(&triple.subject);
        let pred = self.verbalize_predicate(&triple.predicate);
        let obj = self.verbalize_term(&triple.object);

        format!("{} {} {}", subj, pred, obj)
    }

    /// Verbalize a term
    fn verbalize_term(&self, term: &Term) -> String {
        match term {
            Term::Variable(v) => {
                let name = v.name();
                format!("something ({})", name)
            }
            Term::Uri(uri) => {
                if let Some(entry) = self.vocabulary.get(uri.as_str()) {
                    entry.label.clone()
                } else {
                    // Extract local name
                    let uri_str = uri.as_str();
                    if let Some(idx) = uri_str.rfind(|c| c == '/' || c == '#') {
                        uri_str[idx + 1..].to_string()
                    } else {
                        uri_str.to_string()
                    }
                }
            }
            Term::Literal(lit) => {
                format!("\"{}\"", lit.value())
            }
            Term::BlankNode(bn) => {
                format!("something ({})", bn.id())
            }
            Term::List(items) => {
                let verbalized: Vec<String> = items.iter()
                    .map(|t| self.verbalize_term(t))
                    .collect();
                format!("[{}]", verbalized.join(", "))
            }
            Term::Formula(_) => "[formula]".to_string(),
        }
    }

    /// Verbalize a predicate
    fn verbalize_predicate(&self, term: &Term) -> String {
        if let Term::Uri(uri) = term {
            if let Some(entry) = self.vocabulary.get(uri.as_str()) {
                return entry.label.clone();
            }
        }
        self.verbalize_term(term)
    }

    /// Add vocabulary entry
    pub fn add_vocabulary(&mut self, uri: &str, label: &str, description: Option<&str>) {
        self.vocabulary.add(uri, label, description);
    }
}

impl Default for RuleVerbalizer {
    fn default() -> Self {
        Self::new()
    }
}

/// Natural language explainer for proofs
#[derive(Clone, Debug)]
pub struct NLExplainer {
    /// Vocabulary
    vocabulary: Vocabulary,
    /// Explanation templates
    templates: HashMap<String, String>,
}

impl NLExplainer {
    /// Create a new explainer
    pub fn new() -> Self {
        let mut explainer = NLExplainer {
            vocabulary: Vocabulary::new(),
            templates: HashMap::new(),
        };

        // Add explanation templates
        explainer.templates.insert(
            "inference".to_string(),
            "Because {} we can conclude that {}.".to_string(),
        );
        explainer.templates.insert(
            "assertion".to_string(),
            "{} is known to be true.".to_string(),
        );
        explainer.templates.insert(
            "rule_application".to_string(),
            "By the rule \"{}\", since {}, we derive {}.".to_string(),
        );

        explainer
    }

    /// Explain a complete proof
    pub fn explain(&self, proof: &Proof) -> String {
        let mut explanations = Vec::new();

        // Explain assertions
        if !proof.assertions.is_empty() {
            explanations.push("We start with the following known facts:".to_string());
            for assertion in &proof.assertions {
                explanations.push(format!("  - {}", self.verbalize_triple(assertion)));
            }
        }

        // Explain inference steps
        if !proof.steps.is_empty() {
            explanations.push("\nThrough reasoning, we derive:".to_string());
            for step in &proof.steps {
                explanations.push(format!("\n{}. {}", step.step_number, self.explain_step(step)));
            }
        }

        explanations.join("\n")
    }

    /// Explain a single proof step
    pub fn explain_step(&self, step: &ProofStep) -> String {
        let conclusion = self.verbalize_triple(&step.conclusion);

        if step.premises.is_empty() {
            return format!("{} (given)", conclusion);
        }

        let premises: Vec<String> = step.premises.iter()
            .map(|p| self.verbalize_triple(p))
            .collect();
        let premises_text = premises.join(" and ");

        let rule_name = step.rule_name.as_ref()
            .map(|n| format!("\"{}\"", n))
            .unwrap_or_else(|| format!("Rule {}", step.rule_index));

        format!(
            "We can conclude that {} because {} (applying {})",
            conclusion, premises_text, rule_name
        )
    }

    /// Verbalize a triple
    fn verbalize_triple(&self, triple: &Triple) -> String {
        let subj = self.verbalize_term(&triple.subject);
        let pred = self.verbalize_term(&triple.predicate);
        let obj = self.verbalize_term(&triple.object);

        // Special handling for rdf:type
        if let Term::Uri(uri) = &triple.predicate {
            if uri.as_str() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" {
                return format!("{} is a {}", subj, obj);
            }
        }

        format!("{} {} {}", subj, pred, obj)
    }

    /// Verbalize a term
    fn verbalize_term(&self, term: &Term) -> String {
        match term {
            Term::Variable(v) => v.name().to_string(),
            Term::Uri(uri) => {
                if let Some(entry) = self.vocabulary.get(uri.as_str()) {
                    entry.label.clone()
                } else {
                    let uri_str = uri.as_str();
                    if let Some(idx) = uri_str.rfind(|c| c == '/' || c == '#') {
                        uri_str[idx + 1..].to_string()
                    } else {
                        uri_str.to_string()
                    }
                }
            }
            Term::Literal(lit) => lit.value().to_string(),
            Term::BlankNode(bn) => format!("_{}", bn.id()),
            Term::List(items) => {
                let terms: Vec<String> = items.iter()
                    .map(|t| self.verbalize_term(t))
                    .collect();
                format!("[{}]", terms.join(", "))
            }
            Term::Formula(_) => "[formula]".to_string(),
        }
    }

    /// Add vocabulary entry
    pub fn add_vocabulary(&mut self, uri: &str, label: &str, description: Option<&str>) {
        self.vocabulary.add(uri, label, description);
    }
}

impl Default for NLExplainer {
    fn default() -> Self {
        Self::new()
    }
}

/// Vocabulary for mapping URIs to human-readable labels
#[derive(Clone, Debug, Default)]
pub struct Vocabulary {
    /// Entries by URI
    entries: HashMap<String, VocabularyEntry>,
}

impl Vocabulary {
    /// Create a new vocabulary
    pub fn new() -> Self {
        let mut vocab = Vocabulary {
            entries: HashMap::new(),
        };

        // Add common RDF/RDFS/OWL vocabulary
        vocab.add("http://www.w3.org/1999/02/22-rdf-syntax-ns#type", "type", None);
        vocab.add("http://www.w3.org/2000/01/rdf-schema#subClassOf", "subclass of", None);
        vocab.add("http://www.w3.org/2000/01/rdf-schema#domain", "domain", None);
        vocab.add("http://www.w3.org/2000/01/rdf-schema#range", "range", None);
        vocab.add("http://www.w3.org/2000/01/rdf-schema#label", "label", None);
        vocab.add("http://www.w3.org/2000/01/rdf-schema#comment", "comment", None);
        vocab.add("http://www.w3.org/2002/07/owl#Class", "class", None);
        vocab.add("http://www.w3.org/2002/07/owl#ObjectProperty", "object property", None);
        vocab.add("http://www.w3.org/2002/07/owl#DatatypeProperty", "datatype property", None);
        vocab.add("http://www.w3.org/2002/07/owl#sameAs", "same as", None);
        vocab.add("http://www.w3.org/2002/07/owl#differentFrom", "different from", None);

        vocab
    }

    /// Add a vocabulary entry
    pub fn add(&mut self, uri: &str, label: &str, description: Option<&str>) {
        self.entries.insert(uri.to_string(), VocabularyEntry {
            uri: uri.to_string(),
            label: label.to_string(),
            description: description.map(|s| s.to_string()),
        });
    }

    /// Get a vocabulary entry
    pub fn get(&self, uri: &str) -> Option<&VocabularyEntry> {
        self.entries.get(uri)
    }

    /// Check if URI is in vocabulary
    pub fn contains(&self, uri: &str) -> bool {
        self.entries.contains_key(uri)
    }

    /// Load vocabulary from a store (using rdfs:label and rdfs:comment)
    pub fn load_from_store(&mut self, store: &Store) {
        let label_uri = "http://www.w3.org/2000/01/rdf-schema#label";
        let comment_uri = "http://www.w3.org/2000/01/rdf-schema#comment";

        // Index labels
        let mut labels: HashMap<String, String> = HashMap::new();
        let mut comments: HashMap<String, String> = HashMap::new();

        for triple in store.iter() {
            if let (Term::Uri(subj), Term::Uri(pred), Term::Literal(obj)) =
                (&triple.subject, &triple.predicate, &triple.object)
            {
                if pred.as_str() == label_uri {
                    labels.insert(subj.as_str().to_string(), obj.value().to_string());
                } else if pred.as_str() == comment_uri {
                    comments.insert(subj.as_str().to_string(), obj.value().to_string());
                }
            }
        }

        // Create vocabulary entries
        for (uri, label) in labels {
            let description = comments.get(&uri).map(|s| s.as_str());
            self.add(&uri, &label, description);
        }
    }

    /// Get number of entries
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// A vocabulary entry
#[derive(Clone, Debug)]
pub struct VocabularyEntry {
    /// URI
    pub uri: String,
    /// Human-readable label
    pub label: String,
    /// Description (optional)
    pub description: Option<String>,
}

/// NL interface errors
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NLError {
    /// Parse error
    ParseError(String),
    /// Unknown entity
    UnknownEntity(String),
    /// Ambiguous query
    AmbiguousQuery(String),
}

impl std::fmt::Display for NLError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NLError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            NLError::UnknownEntity(entity) => write!(f, "Unknown entity: {}", entity),
            NLError::AmbiguousQuery(msg) => write!(f, "Ambiguous query: {}", msg),
        }
    }
}

impl std::error::Error for NLError {}

/// Entity mention in text
#[derive(Clone, Debug)]
pub struct EntityMention {
    /// Start position in text
    pub start: usize,
    /// End position in text
    pub end: usize,
    /// Matched text
    pub text: String,
    /// Entity URI (if resolved)
    pub uri: Option<String>,
    /// Confidence score
    pub confidence: f64,
}

/// Entity linker for mapping text mentions to URIs
#[derive(Clone, Debug)]
pub struct EntityLinker {
    /// Known entities
    entities: HashMap<String, String>,
    /// Aliases
    aliases: HashMap<String, String>,
}

impl EntityLinker {
    /// Create a new entity linker
    pub fn new() -> Self {
        EntityLinker {
            entities: HashMap::new(),
            aliases: HashMap::new(),
        }
    }

    /// Add an entity
    pub fn add_entity(&mut self, label: &str, uri: &str) {
        self.entities.insert(label.to_lowercase(), uri.to_string());
    }

    /// Add an alias
    pub fn add_alias(&mut self, alias: &str, label: &str) {
        self.aliases.insert(alias.to_lowercase(), label.to_lowercase());
    }

    /// Find entity mentions in text
    pub fn find_mentions(&self, text: &str) -> Vec<EntityMention> {
        let mut mentions = Vec::new();
        let text_lower = text.to_lowercase();

        for (label, uri) in &self.entities {
            if let Some(start) = text_lower.find(label) {
                mentions.push(EntityMention {
                    start,
                    end: start + label.len(),
                    text: text[start..start + label.len()].to_string(),
                    uri: Some(uri.clone()),
                    confidence: 1.0,
                });
            }
        }

        // Sort by position
        mentions.sort_by_key(|m| m.start);

        mentions
    }

    /// Resolve an entity mention
    pub fn resolve(&self, text: &str) -> Option<String> {
        let text_lower = text.to_lowercase().trim().to_string();

        // Check direct match
        if let Some(uri) = self.entities.get(&text_lower) {
            return Some(uri.clone());
        }

        // Check aliases
        if let Some(label) = self.aliases.get(&text_lower) {
            if let Some(uri) = self.entities.get(label) {
                return Some(uri.clone());
            }
        }

        None
    }

    /// Load entities from a store
    pub fn load_from_store(&mut self, store: &Store) {
        let label_uri = "http://www.w3.org/2000/01/rdf-schema#label";

        for triple in store.iter() {
            if let (Term::Uri(subj), Term::Uri(pred), Term::Literal(obj)) =
                (&triple.subject, &triple.predicate, &triple.object)
            {
                if pred.as_str() == label_uri {
                    self.add_entity(obj.value(), subj.as_str());
                }
            }
        }
    }
}

impl Default for EntityLinker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_query_parser_who_knows() {
        let parser = QueryParser::new();
        let patterns = parser.parse("Who knows Bob?").unwrap();

        assert_eq!(patterns.len(), 1);
        assert!(matches!(patterns[0].subject, Term::Variable(_)));
        assert!(matches!(patterns[0].object, Term::Uri(_)));
    }

    #[test]
    fn test_query_parser_what_is() {
        let parser = QueryParser::new();
        let patterns = parser.parse("What is Socrates?").unwrap();

        assert_eq!(patterns.len(), 1);
        // Should query for rdf:type
        if let Term::Uri(pred) = &patterns[0].predicate {
            assert!(pred.as_str().contains("type"));
        }
    }

    #[test]
    fn test_query_parser_is_a() {
        let parser = QueryParser::new();
        let patterns = parser.parse("Is Socrates a Human?").unwrap();

        assert_eq!(patterns.len(), 1);
        // Both subject and object should be terms
        assert!(matches!(patterns[0].subject, Term::Uri(_)));
        assert!(matches!(patterns[0].object, Term::Uri(_)));
    }

    #[test]
    fn test_rule_verbalizer() {
        let verbalizer = RuleVerbalizer::new();

        let rule = Rule::new(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                Term::uri("http://example.org/Human"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                Term::uri("http://example.org/Mortal"),
            )],
        );

        let text = verbalizer.verbalize(&rule);
        assert!(text.contains("If"));
        assert!(text.contains("then"));
    }

    #[test]
    fn test_vocabulary() {
        let mut vocab = Vocabulary::new();

        vocab.add("http://example.org/Person", "Person", Some("A human being"));

        assert!(vocab.contains("http://example.org/Person"));
        let entry = vocab.get("http://example.org/Person").unwrap();
        assert_eq!(entry.label, "Person");
    }

    #[test]
    fn test_entity_linker() {
        let mut linker = EntityLinker::new();

        linker.add_entity("bob", "http://example.org/Bob");
        linker.add_alias("robert", "bob");

        assert_eq!(
            linker.resolve("Bob"),
            Some("http://example.org/Bob".to_string())
        );
        assert_eq!(
            linker.resolve("Robert"),
            Some("http://example.org/Bob".to_string())
        );
    }

    #[test]
    fn test_entity_mention_finding() {
        let mut linker = EntityLinker::new();
        linker.add_entity("alice", "http://example.org/Alice");
        linker.add_entity("bob", "http://example.org/Bob");

        let mentions = linker.find_mentions("Alice knows Bob");

        assert_eq!(mentions.len(), 2);
        assert_eq!(mentions[0].text.to_lowercase(), "alice");
        assert_eq!(mentions[1].text.to_lowercase(), "bob");
    }

    #[test]
    fn test_nl_interface() {
        let interface = NLInterface::new();

        // Test query parsing
        let patterns = interface.parse_query("Who knows Bob?").unwrap();
        assert!(!patterns.is_empty());

        // Test rule verbalization
        let rule = Rule::new(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/parent"),
                Term::universal("y"),
            )],
            vec![Triple::new(
                Term::universal("y"),
                Term::uri("http://example.org/child"),
                Term::universal("x"),
            )],
        );

        let text = interface.verbalize_rule(&rule);
        assert!(!text.is_empty());
    }

    #[test]
    fn test_nl_explainer() {
        let explainer = NLExplainer::new();

        let step = ProofStep {
            conclusion: Triple::new(
                Term::uri("http://example.org/Socrates"),
                Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                Term::uri("http://example.org/Mortal"),
            ),
            rule_index: 0,
            rule_name: Some("mortality rule".to_string()),
            rule_antecedent: vec![],
            rule_consequent: vec![],
            bindings: vec![],
            premises: vec![Triple::new(
                Term::uri("http://example.org/Socrates"),
                Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                Term::uri("http://example.org/Human"),
            )],
            step_number: 1,
        };

        let explanation = explainer.explain_step(&step);
        assert!(explanation.contains("Socrates"));
        assert!(explanation.contains("Mortal"));
    }
}
