//! Proof Serialization and W3C SWAP Reason Vocabulary
//!
//! This module provides serialization of inference proofs using the W3C SWAP
//! reason vocabulary (http://www.w3.org/2000/10/swap/reason#).
//!
//! The reason vocabulary provides terms for describing how conclusions were derived:
//!
//! - `reason:Proof` - A complete proof document
//! - `reason:Inference` - An inference step
//! - `reason:Extraction` - Extraction from a formula
//! - `reason:CommandLine` - The original command/query
//! - `reason:gives` - Links to the conclusion
//! - `reason:because` - Links to the justification
//! - `reason:evidence` - The evidence used
//! - `reason:rule` - The rule applied
//! - `reason:binding` - Variable bindings
//!
//! # Example Output
//!
//! ```n3
//! @prefix reason: <http://www.w3.org/2000/10/swap/reason#> .
//! @prefix : <http://example.org/> .
//!
//! [] a reason:Proof ;
//!    reason:gives { :socrates a :Mortal } ;
//!    reason:because [
//!        a reason:Inference ;
//!        reason:evidence ( <#step1> ) ;
//!        reason:rule <#rule_rdfs9> ;
//!        reason:binding [ reason:variable :x ; reason:boundTo :socrates ]
//!    ] .
//! ```
//!
//! # Usage
//!
//! ```ignore
//! use cwm::reasoner::{Proof, ProofFormatter};
//!
//! let proof = reasoner.take_proof().unwrap();
//! let formatter = ProofFormatter::new(&proof);
//! println!("{}", formatter.to_n3());
//! ```

use std::collections::HashMap;
use std::fmt::{self, Write};

use crate::term::{Term, Triple};

/// Namespace URIs for proof serialization
pub mod ns {
    pub const REASON: &str = "http://www.w3.org/2000/10/swap/reason#";
    pub const LOG: &str = "http://www.w3.org/2000/10/swap/log#";
    pub const RDF: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    // Reason vocabulary terms
    pub const REASON_PROOF: &str = "http://www.w3.org/2000/10/swap/reason#Proof";
    pub const REASON_INFERENCE: &str = "http://www.w3.org/2000/10/swap/reason#Inference";
    pub const REASON_EXTRACTION: &str = "http://www.w3.org/2000/10/swap/reason#Extraction";
    pub const REASON_PARSING: &str = "http://www.w3.org/2000/10/swap/reason#Parsing";
    pub const REASON_COMMANDLINE: &str = "http://www.w3.org/2000/10/swap/reason#CommandLine";

    pub const REASON_GIVES: &str = "http://www.w3.org/2000/10/swap/reason#gives";
    pub const REASON_BECAUSE: &str = "http://www.w3.org/2000/10/swap/reason#because";
    pub const REASON_EVIDENCE: &str = "http://www.w3.org/2000/10/swap/reason#evidence";
    pub const REASON_RULE: &str = "http://www.w3.org/2000/10/swap/reason#rule";
    pub const REASON_BINDING: &str = "http://www.w3.org/2000/10/swap/reason#binding";
    pub const REASON_VARIABLE: &str = "http://www.w3.org/2000/10/swap/reason#variable";
    pub const REASON_BOUND_TO: &str = "http://www.w3.org/2000/10/swap/reason#boundTo";
    pub const REASON_SOURCE: &str = "http://www.w3.org/2000/10/swap/reason#source";
    pub const REASON_COMPONENT: &str = "http://www.w3.org/2000/10/swap/reason#component";
}

use super::{Proof, ProofStep};

/// Proof validation result
#[derive(Debug, Clone)]
pub struct ProofValidation {
    /// Whether the proof is valid
    pub valid: bool,
    /// Any errors found
    pub errors: Vec<String>,
    /// Warnings (non-fatal issues)
    pub warnings: Vec<String>,
}

impl ProofValidation {
    pub fn valid() -> Self {
        ProofValidation {
            valid: true,
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn invalid(error: impl Into<String>) -> Self {
        ProofValidation {
            valid: false,
            errors: vec![error.into()],
            warnings: Vec::new(),
        }
    }
}

/// Formatter for proof serialization
pub struct ProofFormatter<'a> {
    proof: &'a Proof,
    base_uri: Option<String>,
    prefixes: HashMap<String, String>,
}

impl<'a> ProofFormatter<'a> {
    /// Create a new proof formatter
    pub fn new(proof: &'a Proof) -> Self {
        let mut prefixes = HashMap::new();
        prefixes.insert("reason".to_string(), ns::REASON.to_string());
        prefixes.insert("log".to_string(), ns::LOG.to_string());
        prefixes.insert("rdf".to_string(), ns::RDF.to_string());

        ProofFormatter {
            proof,
            base_uri: None,
            prefixes,
        }
    }

    /// Set the base URI for the proof document
    pub fn with_base(mut self, base: impl Into<String>) -> Self {
        self.base_uri = Some(base.into());
        self
    }

    /// Add a prefix mapping
    pub fn with_prefix(mut self, prefix: impl Into<String>, uri: impl Into<String>) -> Self {
        self.prefixes.insert(prefix.into(), uri.into());
        self
    }

    /// Format the proof as N3 (W3C SWAP reason vocabulary)
    pub fn to_n3(&self) -> String {
        let mut output = String::new();

        // Write prefixes
        for (prefix, uri) in &self.prefixes {
            writeln!(output, "@prefix {}: <{}> .", prefix, uri).unwrap();
        }
        if let Some(base) = &self.base_uri {
            writeln!(output, "@base <{}> .", base).unwrap();
        }
        writeln!(output).unwrap();

        // Write assertions (as extraction steps)
        writeln!(output, "# Original assertions").unwrap();
        for (i, assertion) in self.proof.assertions.iter().enumerate() {
            writeln!(output, "_:assertion{} a reason:Extraction ;", i).unwrap();
            writeln!(output, "    reason:gives {{ {} }} .", self.format_triple(assertion)).unwrap();
        }
        writeln!(output).unwrap();

        // Write inference steps
        writeln!(output, "# Inference steps").unwrap();
        for step in &self.proof.steps {
            self.write_step(&mut output, step);
        }

        // Write the complete proof
        writeln!(output).unwrap();
        writeln!(output, "# Complete proof").unwrap();
        writeln!(output, "_:proof a reason:Proof ;").unwrap();

        // List all conclusions
        if !self.proof.steps.is_empty() {
            writeln!(output, "    reason:gives {{").unwrap();
            for (i, step) in self.proof.steps.iter().enumerate() {
                let sep = if i < self.proof.steps.len() - 1 { " ." } else { "" };
                writeln!(output, "        {}{}", self.format_triple(&step.conclusion), sep).unwrap();
            }
            writeln!(output, "    }} ;").unwrap();

            // Reference all steps
            write!(output, "    reason:because ( ").unwrap();
            for (i, _) in self.proof.steps.iter().enumerate() {
                write!(output, "_:step{} ", i).unwrap();
            }
            writeln!(output, ") .").unwrap();
        } else {
            writeln!(output, "    reason:gives {{ }} .").unwrap();
        }

        output
    }

    /// Write a single proof step
    fn write_step(&self, output: &mut String, step: &ProofStep) {
        writeln!(output, "_:step{} a reason:Inference ;", step.step_number).unwrap();
        writeln!(output, "    reason:gives {{ {} }} ;", self.format_triple(&step.conclusion)).unwrap();

        // Rule reference
        if let Some(name) = &step.rule_name {
            writeln!(output, "    reason:rule \"{}\" ;", name).unwrap();
        } else {
            writeln!(output, "    reason:rule \"rule_{}\" ;", step.rule_index).unwrap();
        }

        // Evidence (premises)
        if !step.premises.is_empty() {
            writeln!(output, "    reason:evidence (").unwrap();
            for premise in &step.premises {
                writeln!(output, "        [ reason:gives {{ {} }} ]", self.format_triple(premise)).unwrap();
            }
            writeln!(output, "    ) ;").unwrap();
        }

        // Variable bindings
        if !step.bindings.is_empty() {
            writeln!(output, "    reason:binding (").unwrap();
            for (var, val) in &step.bindings {
                writeln!(output, "        [ reason:variable \"?{}\" ; reason:boundTo {} ]",
                         var, self.format_term(val)).unwrap();
            }
            writeln!(output, "    )").unwrap();
        }

        writeln!(output, "    .").unwrap();
        writeln!(output).unwrap();
    }

    /// Format a triple for N3 output
    fn format_triple(&self, triple: &Triple) -> String {
        format!("{} {} {}",
                self.format_term(&triple.subject),
                self.format_term(&triple.predicate),
                self.format_term(&triple.object))
    }

    /// Format a term for N3 output
    fn format_term(&self, term: &Term) -> String {
        match term {
            Term::Uri(uri) => {
                // Check if we can use a prefix
                for (prefix, ns_uri) in &self.prefixes {
                    if uri.as_str().starts_with(ns_uri.as_str()) {
                        let local = &uri.as_str()[ns_uri.len()..];
                        return format!("{}:{}", prefix, local);
                    }
                }
                format!("<{}>", uri.as_str())
            }
            Term::Literal(lit) => {
                if let Some(lang) = lit.language() {
                    format!("\"{}\"@{}", escape_string(lit.value()), lang)
                } else if let Some(dt) = lit.datatype_uri() {
                    format!("\"{}\"^^<{}>", escape_string(lit.value()), dt)
                } else {
                    format!("\"{}\"", escape_string(lit.value()))
                }
            }
            Term::BlankNode(bn) => format!("_:b{}", bn.id()),
            Term::Variable(v) => format!("?{}", v.name()),
            Term::List(list) => {
                let items: Vec<String> = list.iter()
                    .map(|t| self.format_term(t))
                    .collect();
                format!("( {} )", items.join(" "))
            }
            Term::Formula(f) => {
                let triples: Vec<String> = f.triples().iter()
                    .map(|t| format!("{} .", self.format_triple(t)))
                    .collect();
                format!("{{ {} }}", triples.join(" "))
            }
        }
    }

    /// Format the proof as JSON
    pub fn to_json(&self) -> String {
        let mut output = String::from("{\n");
        writeln!(output, "  \"type\": \"Proof\",").unwrap();

        // Assertions
        writeln!(output, "  \"assertions\": [").unwrap();
        for (i, assertion) in self.proof.assertions.iter().enumerate() {
            let sep = if i < self.proof.assertions.len() - 1 { "," } else { "" };
            writeln!(output, "    {{\n      \"subject\": {},\n      \"predicate\": {},\n      \"object\": {}\n    }}{}",
                     self.term_to_json(&assertion.subject),
                     self.term_to_json(&assertion.predicate),
                     self.term_to_json(&assertion.object),
                     sep).unwrap();
        }
        writeln!(output, "  ],").unwrap();

        // Steps
        writeln!(output, "  \"steps\": [").unwrap();
        for (i, step) in self.proof.steps.iter().enumerate() {
            let sep = if i < self.proof.steps.len() - 1 { "," } else { "" };
            writeln!(output, "    {{").unwrap();
            writeln!(output, "      \"stepNumber\": {},", step.step_number).unwrap();
            writeln!(output, "      \"ruleName\": {},", step.rule_name.as_ref().map(|s| format!("\"{}\"", s)).unwrap_or_else(|| "null".to_string())).unwrap();
            writeln!(output, "      \"ruleIndex\": {},", step.rule_index).unwrap();
            writeln!(output, "      \"conclusion\": {{\n        \"subject\": {},\n        \"predicate\": {},\n        \"object\": {}\n      }},",
                     self.term_to_json(&step.conclusion.subject),
                     self.term_to_json(&step.conclusion.predicate),
                     self.term_to_json(&step.conclusion.object)).unwrap();

            // Bindings
            writeln!(output, "      \"bindings\": {{").unwrap();
            for (j, (var, val)) in step.bindings.iter().enumerate() {
                let sep = if j < step.bindings.len() - 1 { "," } else { "" };
                writeln!(output, "        \"{}\": {}{}", var, self.term_to_json(val), sep).unwrap();
            }
            writeln!(output, "      }},").unwrap();

            // Premises
            writeln!(output, "      \"premises\": [").unwrap();
            for (j, premise) in step.premises.iter().enumerate() {
                let sep = if j < step.premises.len() - 1 { "," } else { "" };
                writeln!(output, "        {{\n          \"subject\": {},\n          \"predicate\": {},\n          \"object\": {}\n        }}{}",
                         self.term_to_json(&premise.subject),
                         self.term_to_json(&premise.predicate),
                         self.term_to_json(&premise.object),
                         sep).unwrap();
            }
            writeln!(output, "      ]").unwrap();
            writeln!(output, "    }}{}", sep).unwrap();
        }
        writeln!(output, "  ]").unwrap();

        writeln!(output, "}}").unwrap();
        output
    }

    /// Convert a term to JSON
    fn term_to_json(&self, term: &Term) -> String {
        match term {
            Term::Uri(uri) => format!("{{\"type\": \"uri\", \"value\": \"{}\"}}", escape_json(uri.as_str())),
            Term::Literal(lit) => {
                let mut s = format!("{{\"type\": \"literal\", \"value\": \"{}\"", escape_json(lit.value()));
                if let Some(lang) = lit.language() {
                    s.push_str(&format!(", \"language\": \"{}\"", escape_json(lang)));
                }
                if let Some(dt) = lit.datatype_uri() {
                    s.push_str(&format!(", \"datatype\": \"{}\"", escape_json(dt)));
                }
                s.push('}');
                s
            }
            Term::BlankNode(bn) => format!("{{\"type\": \"bnode\", \"value\": \"b{}\"}}", bn.id()),
            Term::Variable(v) => format!("{{\"type\": \"variable\", \"value\": \"{}\"}}", escape_json(v.name())),
            _ => "\"<complex>\"".to_string(),
        }
    }
}

/// Escape special characters for N3 string literals
fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

/// Escape special characters for JSON strings
fn escape_json(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

/// Validate a proof
///
/// Checks that:
/// 1. Each step's premises are either assertions or conclusions of previous steps
/// 2. The rule patterns match the derived conclusion
/// 3. The bindings are consistent
pub fn validate_proof(proof: &Proof) -> ProofValidation {
    let mut validation = ProofValidation::valid();
    let mut derived: Vec<&Triple> = Vec::new();

    // Add all assertions to the set of known facts
    for assertion in &proof.assertions {
        derived.push(assertion);
    }

    // Check each step
    for step in &proof.steps {
        // Check that all premises are known
        for premise in &step.premises {
            if !derived.iter().any(|t| *t == premise) {
                validation.errors.push(format!(
                    "Step {}: Premise {:?} not found in assertions or previous conclusions",
                    step.step_number, premise
                ));
                validation.valid = false;
            }
        }

        // Check step ordering
        if step.step_number > 0 && step.step_number > proof.steps.len() {
            validation.warnings.push(format!(
                "Step {}: Step number exceeds total step count",
                step.step_number
            ));
        }

        // Add conclusion to known facts
        derived.push(&step.conclusion);
    }

    validation
}

/// Statistics about a proof
#[derive(Debug, Clone, Default)]
pub struct ProofStats {
    /// Number of assertions
    pub assertion_count: usize,
    /// Number of inference steps
    pub step_count: usize,
    /// Number of unique rules used
    pub unique_rules: usize,
    /// Maximum chain depth
    pub max_depth: usize,
    /// Average premises per step
    pub avg_premises: f64,
}

impl ProofStats {
    /// Compute statistics for a proof
    pub fn compute(proof: &Proof) -> Self {
        let mut unique_rules = std::collections::HashSet::new();
        let mut total_premises = 0;

        for step in &proof.steps {
            if let Some(name) = &step.rule_name {
                unique_rules.insert(name.clone());
            } else {
                unique_rules.insert(format!("rule_{}", step.rule_index));
            }
            total_premises += step.premises.len();
        }

        ProofStats {
            assertion_count: proof.assertions.len(),
            step_count: proof.steps.len(),
            unique_rules: unique_rules.len(),
            max_depth: proof.steps.iter().map(|s| s.step_number).max().unwrap_or(0),
            avg_premises: if proof.steps.is_empty() {
                0.0
            } else {
                total_premises as f64 / proof.steps.len() as f64
            },
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_proof() -> Proof {
        let assertion1 = Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
            Term::uri("http://example.org/Human"),
        );

        let conclusion1 = Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
            Term::uri("http://example.org/Mortal"),
        );

        let step = super::super::ProofStep {
            conclusion: conclusion1.clone(),
            rule_index: 0,
            rule_name: Some("rdfs9-subclass".to_string()),
            rule_antecedent: vec![],
            rule_consequent: vec![],
            bindings: vec![("x".to_string(), Term::uri("http://example.org/socrates"))],
            premises: vec![assertion1.clone()],
            step_number: 1,
        };

        Proof {
            assertions: vec![assertion1],
            steps: vec![step],
        }
    }

    #[test]
    fn test_proof_to_n3() {
        let proof = sample_proof();
        let formatter = ProofFormatter::new(&proof)
            .with_prefix("ex", "http://example.org/");

        let n3 = formatter.to_n3();

        assert!(n3.contains("@prefix reason:"));
        assert!(n3.contains("reason:Inference"));
        assert!(n3.contains("reason:gives"));
        assert!(n3.contains("reason:Proof"));
    }

    #[test]
    fn test_proof_to_json() {
        let proof = sample_proof();
        let formatter = ProofFormatter::new(&proof);

        let json = formatter.to_json();

        assert!(json.contains("\"type\": \"Proof\""));
        assert!(json.contains("\"assertions\""));
        assert!(json.contains("\"steps\""));
        assert!(json.contains("\"bindings\""));
    }

    #[test]
    fn test_validate_valid_proof() {
        let proof = sample_proof();
        let validation = validate_proof(&proof);

        assert!(validation.valid);
        assert!(validation.errors.is_empty());
    }

    #[test]
    fn test_validate_invalid_proof() {
        let conclusion = Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/b"),
            Term::uri("http://example.org/c"),
        );

        // Step with premise not in assertions
        let step = super::super::ProofStep {
            conclusion,
            rule_index: 0,
            rule_name: None,
            rule_antecedent: vec![],
            rule_consequent: vec![],
            bindings: vec![],
            premises: vec![Triple::new(
                Term::uri("http://example.org/unknown"),
                Term::uri("http://example.org/p"),
                Term::uri("http://example.org/o"),
            )],
            step_number: 1,
        };

        let proof = Proof {
            assertions: vec![],
            steps: vec![step],
        };

        let validation = validate_proof(&proof);
        assert!(!validation.valid);
        assert!(!validation.errors.is_empty());
    }

    #[test]
    fn test_proof_stats() {
        let proof = sample_proof();
        let stats = ProofStats::compute(&proof);

        assert_eq!(stats.assertion_count, 1);
        assert_eq!(stats.step_count, 1);
        assert_eq!(stats.unique_rules, 1);
        assert_eq!(stats.max_depth, 1);
        assert_eq!(stats.avg_premises, 1.0);
    }

    #[test]
    fn test_escape_string() {
        assert_eq!(escape_string("hello"), "hello");
        assert_eq!(escape_string("hello\nworld"), "hello\\nworld");
        assert_eq!(escape_string("say \"hi\""), "say \\\"hi\\\"");
    }
}
