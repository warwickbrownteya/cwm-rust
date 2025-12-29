//! Explanation generation for inference results
//!
//! This module provides human-readable explanations for why certain triples
//! were inferred, building on the proof and dependency tracking infrastructure.
//!
//! # Features
//!
//! - **Natural Language Explanations**: Generate readable explanations for inferences
//! - **Multiple Detail Levels**: Summary, detailed, and full trace modes
//! - **Multiple Formats**: Text, Markdown, JSON, and HTML output
//! - **Dependency Visualization**: Show how triples relate to each other
//!
//! # Example
//!
//! ```ignore
//! use cwm::{ExplanationGenerator, Store, Reasoner};
//!
//! let mut reasoner = Reasoner::new();
//! reasoner.enable_proof();
//! reasoner.run(&mut store);
//!
//! let explainer = ExplanationGenerator::new(&reasoner);
//! let explanation = explainer.explain_triple(&derived_triple);
//! println!("{}", explanation.to_markdown());
//! ```

use std::collections::HashMap;
use crate::term::{Term, Triple};
use super::{Proof, ProofStep, Rule};
use super::incremental::{DependencyGraph, TripleId, TripleOrigin};

/// Level of detail for explanations
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExplanationLevel {
    /// Brief one-line summary
    Summary,
    /// Moderate detail with key points
    Detailed,
    /// Full trace with all steps
    Trace,
}

impl Default for ExplanationLevel {
    fn default() -> Self {
        ExplanationLevel::Detailed
    }
}

/// A single explanation step
#[derive(Clone, Debug)]
pub struct ExplanationStep {
    /// The triple being explained
    pub triple: Triple,
    /// Natural language description
    pub description: String,
    /// Why this step happened (rule name, assertion, etc.)
    pub reason: ExplanationReason,
    /// Supporting evidence (premises)
    pub evidence: Vec<Triple>,
    /// Depth in the explanation tree
    pub depth: usize,
}

/// Reason for an inference or assertion
#[derive(Clone, Debug)]
pub enum ExplanationReason {
    /// Triple was asserted (base fact)
    Asserted,
    /// Triple was derived by a rule
    Derived {
        rule_name: Option<String>,
        rule_description: String,
    },
    /// Triple came from a builtin predicate
    Builtin {
        predicate: String,
    },
}

/// Complete explanation for a triple or query result
#[derive(Clone, Debug)]
pub struct Explanation {
    /// Target triple(s) being explained
    pub targets: Vec<Triple>,
    /// Explanation steps in order
    pub steps: Vec<ExplanationStep>,
    /// Summary text
    pub summary: String,
    /// Detail level used
    pub level: ExplanationLevel,
}

impl Default for Explanation {
    fn default() -> Self {
        Explanation {
            targets: Vec::new(),
            steps: Vec::new(),
            summary: String::new(),
            level: ExplanationLevel::default(),
        }
    }
}

impl Explanation {
    /// Create a new empty explanation
    pub fn new() -> Self {
        Self::default()
    }

    /// Format as plain text
    pub fn to_text(&self) -> String {
        let mut output = String::new();

        output.push_str(&format!("Explanation: {}\n", self.summary));
        output.push_str(&format!("Detail level: {:?}\n\n", self.level));

        for step in &self.steps {
            let indent = "  ".repeat(step.depth);
            output.push_str(&format!("{}• {}\n", indent, step.description));

            match &step.reason {
                ExplanationReason::Asserted => {
                    output.push_str(&format!("{}  [Asserted as a fact]\n", indent));
                }
                ExplanationReason::Derived { rule_name, rule_description } => {
                    if let Some(name) = rule_name {
                        output.push_str(&format!("{}  [Rule: {}]\n", indent, name));
                    }
                    output.push_str(&format!("{}  {}\n", indent, rule_description));
                }
                ExplanationReason::Builtin { predicate } => {
                    output.push_str(&format!("{}  [Builtin: {}]\n", indent, predicate));
                }
            }

            if !step.evidence.is_empty() {
                output.push_str(&format!("{}  Evidence:\n", indent));
                for evidence in &step.evidence {
                    output.push_str(&format!("{}    - {}\n", indent, format_triple(evidence)));
                }
            }
            output.push('\n');
        }

        output
    }

    /// Format as Markdown
    pub fn to_markdown(&self) -> String {
        let mut output = String::new();

        output.push_str(&format!("# Explanation\n\n"));
        output.push_str(&format!("**Summary:** {}\n\n", self.summary));
        output.push_str(&format!("**Detail Level:** {:?}\n\n", self.level));

        if !self.targets.is_empty() {
            output.push_str("## Target Triples\n\n");
            for target in &self.targets {
                output.push_str(&format!("- `{}`\n", format_triple(target)));
            }
            output.push('\n');
        }

        output.push_str("## Reasoning Steps\n\n");

        for (i, step) in self.steps.iter().enumerate() {
            let depth_marker = "#".repeat(3 + step.depth.min(3));
            output.push_str(&format!("{} Step {} - {}\n\n", depth_marker, i + 1, step.description));

            output.push_str(&format!("**Triple:** `{}`\n\n", format_triple(&step.triple)));

            match &step.reason {
                ExplanationReason::Asserted => {
                    output.push_str("> 📌 This is an **asserted fact** (base data).\n\n");
                }
                ExplanationReason::Derived { rule_name, rule_description } => {
                    if let Some(name) = rule_name {
                        output.push_str(&format!("> 🔄 **Rule:** `{}`\n", name));
                    }
                    output.push_str(&format!("> {}\n\n", rule_description));
                }
                ExplanationReason::Builtin { predicate } => {
                    output.push_str(&format!("> ⚙️ **Builtin predicate:** `{}`\n\n", predicate));
                }
            }

            if !step.evidence.is_empty() {
                output.push_str("**Evidence:**\n\n");
                for evidence in &step.evidence {
                    output.push_str(&format!("- `{}`\n", format_triple(evidence)));
                }
                output.push('\n');
            }
        }

        output
    }

    /// Format as JSON
    pub fn to_json(&self) -> String {
        let mut output = String::new();
        output.push_str("{\n");
        output.push_str(&format!("  \"summary\": \"{}\",\n", escape_json(&self.summary)));
        output.push_str(&format!("  \"level\": \"{:?}\",\n", self.level));

        // Targets
        output.push_str("  \"targets\": [\n");
        for (i, target) in self.targets.iter().enumerate() {
            let comma = if i < self.targets.len() - 1 { "," } else { "" };
            output.push_str(&format!("    \"{}\"{}\n", escape_json(&format_triple(target)), comma));
        }
        output.push_str("  ],\n");

        // Steps
        output.push_str("  \"steps\": [\n");
        for (i, step) in self.steps.iter().enumerate() {
            output.push_str("    {\n");
            output.push_str(&format!("      \"triple\": \"{}\",\n", escape_json(&format_triple(&step.triple))));
            output.push_str(&format!("      \"description\": \"{}\",\n", escape_json(&step.description)));
            output.push_str(&format!("      \"depth\": {},\n", step.depth));

            // Reason
            match &step.reason {
                ExplanationReason::Asserted => {
                    output.push_str("      \"reason\": { \"type\": \"asserted\" },\n");
                }
                ExplanationReason::Derived { rule_name, rule_description } => {
                    output.push_str("      \"reason\": {\n");
                    output.push_str("        \"type\": \"derived\",\n");
                    if let Some(name) = rule_name {
                        output.push_str(&format!("        \"ruleName\": \"{}\",\n", escape_json(name)));
                    }
                    output.push_str(&format!("        \"ruleDescription\": \"{}\"\n", escape_json(rule_description)));
                    output.push_str("      },\n");
                }
                ExplanationReason::Builtin { predicate } => {
                    output.push_str(&format!("      \"reason\": {{ \"type\": \"builtin\", \"predicate\": \"{}\" }},\n", escape_json(predicate)));
                }
            }

            // Evidence
            output.push_str("      \"evidence\": [\n");
            for (j, evidence) in step.evidence.iter().enumerate() {
                let comma = if j < step.evidence.len() - 1 { "," } else { "" };
                output.push_str(&format!("        \"{}\"{}\n", escape_json(&format_triple(evidence)), comma));
            }
            output.push_str("      ]\n");

            let comma = if i < self.steps.len() - 1 { "," } else { "" };
            output.push_str(&format!("    }}{}\n", comma));
        }
        output.push_str("  ]\n");
        output.push_str("}\n");

        output
    }
}

/// Configuration for explanation generation
#[derive(Clone, Debug)]
pub struct ExplanationConfig {
    /// Level of detail
    pub level: ExplanationLevel,
    /// Maximum depth to traverse
    pub max_depth: usize,
    /// Include all alternative derivations
    pub include_alternatives: bool,
    /// Use friendly names for URIs
    pub use_prefixes: bool,
    /// Custom prefix mappings
    pub prefixes: HashMap<String, String>,
}

impl Default for ExplanationConfig {
    fn default() -> Self {
        let mut prefixes = HashMap::new();
        prefixes.insert("http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string(), "rdf:".to_string());
        prefixes.insert("http://www.w3.org/2000/01/rdf-schema#".to_string(), "rdfs:".to_string());
        prefixes.insert("http://www.w3.org/2002/07/owl#".to_string(), "owl:".to_string());
        prefixes.insert("http://www.w3.org/2001/XMLSchema#".to_string(), "xsd:".to_string());

        ExplanationConfig {
            level: ExplanationLevel::Detailed,
            max_depth: 10,
            include_alternatives: false,
            use_prefixes: true,
            prefixes,
        }
    }
}

/// Generator for explanations
pub struct ExplanationGenerator<'a> {
    /// Configuration
    config: ExplanationConfig,
    /// Optional proof trace
    proof: Option<&'a Proof>,
    /// Optional dependency graph
    dependencies: Option<&'a DependencyGraph>,
    /// Rules for generating descriptions
    rules: Option<&'a [Rule]>,
}

impl<'a> ExplanationGenerator<'a> {
    /// Create a new explanation generator
    pub fn new() -> Self {
        ExplanationGenerator {
            config: ExplanationConfig::default(),
            proof: None,
            dependencies: None,
            rules: None,
        }
    }

    /// Set the configuration
    pub fn with_config(mut self, config: ExplanationConfig) -> Self {
        self.config = config;
        self
    }

    /// Set the proof trace
    pub fn with_proof(mut self, proof: &'a Proof) -> Self {
        self.proof = Some(proof);
        self
    }

    /// Set the dependency graph
    pub fn with_dependencies(mut self, deps: &'a DependencyGraph) -> Self {
        self.dependencies = Some(deps);
        self
    }

    /// Set the rules for description generation
    pub fn with_rules(mut self, rules: &'a [Rule]) -> Self {
        self.rules = Some(rules);
        self
    }

    /// Explain a single triple
    pub fn explain_triple(&self, triple: &Triple) -> Explanation {
        let mut explanation = Explanation::new();
        explanation.targets.push(triple.clone());
        explanation.level = self.config.level;

        // Try dependency graph first (more complete)
        if let Some(deps) = self.dependencies {
            if let Some(id) = deps.get_id(triple) {
                self.explain_from_dependencies(&mut explanation, deps, id, 0);
            }
        }
        // Fall back to proof trace
        else if let Some(proof) = self.proof {
            self.explain_from_proof(&mut explanation, proof, triple, 0);
        }

        // Generate summary
        explanation.summary = self.generate_summary(&explanation);

        explanation
    }

    /// Explain multiple triples
    pub fn explain_triples(&self, triples: &[Triple]) -> Explanation {
        let mut explanation = Explanation::new();
        explanation.targets = triples.to_vec();
        explanation.level = self.config.level;

        for triple in triples {
            if let Some(deps) = self.dependencies {
                if let Some(id) = deps.get_id(triple) {
                    self.explain_from_dependencies(&mut explanation, deps, id, 0);
                }
            } else if let Some(proof) = self.proof {
                self.explain_from_proof(&mut explanation, proof, triple, 0);
            }
        }

        explanation.summary = self.generate_summary(&explanation);

        explanation
    }

    /// Build explanation from dependency graph
    fn explain_from_dependencies(
        &self,
        explanation: &mut Explanation,
        deps: &DependencyGraph,
        id: TripleId,
        depth: usize,
    ) {
        if depth >= self.config.max_depth {
            return;
        }

        let entry = match deps.get(id) {
            Some(e) => e,
            None => return,
        };

        // Avoid duplicates
        if explanation.steps.iter().any(|s| &s.triple == &entry.triple) {
            return;
        }

        // Process each origin
        for origin in &entry.origins {
            match origin {
                TripleOrigin::Asserted => {
                    let step = ExplanationStep {
                        triple: entry.triple.clone(),
                        description: format!(
                            "{} is a base fact",
                            self.format_triple_short(&entry.triple)
                        ),
                        reason: ExplanationReason::Asserted,
                        evidence: Vec::new(),
                        depth,
                    };
                    explanation.steps.push(step);
                }
                TripleOrigin::Derived { rule_index, premises, .. } => {
                    let rule_name = self.rules
                        .and_then(|r| r.get(*rule_index))
                        .and_then(|r| r.name.clone());

                    let rule_description = self.describe_rule(*rule_index);

                    // Collect evidence triples
                    let evidence: Vec<Triple> = premises
                        .iter()
                        .filter_map(|&pid| deps.get(pid).map(|e| e.triple.clone()))
                        .collect();

                    let step = ExplanationStep {
                        triple: entry.triple.clone(),
                        description: format!(
                            "{} was derived",
                            self.format_triple_short(&entry.triple)
                        ),
                        reason: ExplanationReason::Derived {
                            rule_name,
                            rule_description,
                        },
                        evidence: evidence.clone(),
                        depth,
                    };
                    explanation.steps.push(step);

                    // Recursively explain premises (only in Trace mode)
                    if self.config.level == ExplanationLevel::Trace {
                        for &premise_id in premises {
                            self.explain_from_dependencies(explanation, deps, premise_id, depth + 1);
                        }
                    }
                }
            }

            // Only show first origin unless configured otherwise
            if !self.config.include_alternatives {
                break;
            }
        }
    }

    /// Build explanation from proof trace
    fn explain_from_proof(
        &self,
        explanation: &mut Explanation,
        proof: &Proof,
        triple: &Triple,
        depth: usize,
    ) {
        if depth >= self.config.max_depth {
            return;
        }

        // Check if it's an assertion
        if proof.assertions.contains(triple) {
            let step = ExplanationStep {
                triple: triple.clone(),
                description: format!(
                    "{} is a base fact",
                    self.format_triple_short(triple)
                ),
                reason: ExplanationReason::Asserted,
                evidence: Vec::new(),
                depth,
            };
            explanation.steps.push(step);
            return;
        }

        // Find the proof step that concluded this triple
        for proof_step in &proof.steps {
            if &proof_step.conclusion == triple {
                let step = ExplanationStep {
                    triple: triple.clone(),
                    description: format!(
                        "{} was derived",
                        self.format_triple_short(triple)
                    ),
                    reason: ExplanationReason::Derived {
                        rule_name: proof_step.rule_name.clone(),
                        rule_description: self.describe_proof_step(proof_step),
                    },
                    evidence: proof_step.premises.clone(),
                    depth,
                };
                explanation.steps.push(step);

                // Recursively explain premises in Trace mode
                if self.config.level == ExplanationLevel::Trace {
                    for premise in &proof_step.premises {
                        self.explain_from_proof(explanation, proof, premise, depth + 1);
                    }
                }

                break;
            }
        }
    }

    /// Generate a summary of the explanation
    fn generate_summary(&self, explanation: &Explanation) -> String {
        if explanation.steps.is_empty() {
            return "No explanation available".to_string();
        }

        let asserted_count = explanation.steps.iter()
            .filter(|s| matches!(s.reason, ExplanationReason::Asserted))
            .count();
        let derived_count = explanation.steps.iter()
            .filter(|s| matches!(s.reason, ExplanationReason::Derived { .. }))
            .count();

        if explanation.targets.len() == 1 {
            let target = self.format_triple_short(&explanation.targets[0]);
            if asserted_count == 1 && derived_count == 0 {
                format!("{} is a base fact in the data", target)
            } else if derived_count > 0 {
                format!(
                    "{} was derived using {} step(s) from {} base fact(s)",
                    target, derived_count, asserted_count
                )
            } else {
                format!("{} explanation with {} step(s)", target, explanation.steps.len())
            }
        } else {
            format!(
                "Explanation for {} triple(s): {} derived, {} asserted",
                explanation.targets.len(), derived_count, asserted_count
            )
        }
    }

    /// Format a triple in a compact way
    fn format_triple_short(&self, triple: &Triple) -> String {
        if self.config.use_prefixes {
            format!(
                "({}, {}, {})",
                self.apply_prefixes(&format_term(&triple.subject)),
                self.apply_prefixes(&format_term(&triple.predicate)),
                self.apply_prefixes(&format_term(&triple.object))
            )
        } else {
            format_triple(triple)
        }
    }

    /// Apply prefix mappings to a URI string
    fn apply_prefixes(&self, uri: &str) -> String {
        for (namespace, prefix) in &self.config.prefixes {
            if uri.starts_with(namespace) {
                return uri.replace(namespace, prefix);
            }
        }
        uri.to_string()
    }

    /// Generate a description for a rule
    fn describe_rule(&self, rule_index: usize) -> String {
        if let Some(rules) = self.rules {
            if let Some(rule) = rules.get(rule_index) {
                let antecedent_count = rule.antecedent.len();
                let consequent_count = rule.consequent.len();

                if let Some(name) = &rule.name {
                    format!(
                        "Applied rule '{}' ({} conditions → {} conclusions)",
                        name, antecedent_count, consequent_count
                    )
                } else {
                    format!(
                        "Applied rule #{} ({} conditions → {} conclusions)",
                        rule_index, antecedent_count, consequent_count
                    )
                }
            } else {
                format!("Applied rule #{}", rule_index)
            }
        } else {
            format!("Applied rule #{}", rule_index)
        }
    }

    /// Describe a proof step
    fn describe_proof_step(&self, step: &ProofStep) -> String {
        if let Some(name) = &step.rule_name {
            format!(
                "Applied rule '{}' with {} premises",
                name, step.premises.len()
            )
        } else {
            format!(
                "Applied rule #{} with {} premises",
                step.rule_index, step.premises.len()
            )
        }
    }
}

impl<'a> Default for ExplanationGenerator<'a> {
    fn default() -> Self {
        Self::new()
    }
}

/// Format a triple as a string
fn format_triple(triple: &Triple) -> String {
    format!("{}", triple)
}

/// Format a term as a string
fn format_term(term: &Term) -> String {
    format!("{}", term)
}

/// Escape a string for JSON
fn escape_json(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

/// Why-not explanation for triples that weren't derived
#[derive(Clone, Debug)]
pub struct WhyNotExplanation {
    /// The triple that wasn't derived
    pub target: Triple,
    /// Possible reasons why not
    pub reasons: Vec<WhyNotReason>,
    /// Closest rules that could have derived it
    pub near_miss_rules: Vec<NearMissRule>,
}

/// Reason why a triple wasn't derived
#[derive(Clone, Debug)]
pub enum WhyNotReason {
    /// No rule could derive this triple
    NoApplicableRule,
    /// Rule exists but premises weren't satisfied
    UnsatisfiedPremises {
        rule_name: Option<String>,
        missing_premises: Vec<Triple>,
    },
    /// Rule was blocked by negation
    BlockedByNegation {
        rule_name: Option<String>,
        blocking_triple: Triple,
    },
}

/// A rule that almost matched
#[derive(Clone, Debug)]
pub struct NearMissRule {
    /// Rule index
    pub rule_index: usize,
    /// Rule name if any
    pub rule_name: Option<String>,
    /// How close the match was (0.0 to 1.0)
    pub match_score: f64,
    /// Which premises matched
    pub matched_premises: Vec<Triple>,
    /// Which premises didn't match
    pub unmatched_premises: Vec<Triple>,
}

/// Generator for why-not explanations
pub struct WhyNotGenerator<'a> {
    /// Rules to analyze
    rules: &'a [Rule],
    /// Store to check facts
    store: &'a crate::store::Store,
}

impl<'a> WhyNotGenerator<'a> {
    /// Create a new why-not generator
    pub fn new(rules: &'a [Rule], store: &'a crate::store::Store) -> Self {
        WhyNotGenerator { rules, store }
    }

    /// Explain why a triple wasn't derived
    pub fn explain(&self, target: &Triple) -> WhyNotExplanation {
        let mut reasons = Vec::new();
        let mut near_misses = Vec::new();

        // Check each rule to see if it could have derived this triple
        for (rule_index, rule) in self.rules.iter().enumerate() {
            // Check if any consequent pattern matches the target
            for consequent in &rule.consequent {
                if let Some(bindings) = try_unify(consequent, target) {
                    // This rule could potentially derive the target
                    // Check which premises are satisfied
                    let (matched, unmatched) = self.check_premises(&rule.antecedent, &bindings);

                    if unmatched.is_empty() {
                        // All premises matched but triple still wasn't derived
                        // This shouldn't happen normally
                    } else {
                        // Some premises didn't match
                        let match_score = matched.len() as f64 / (matched.len() + unmatched.len()) as f64;

                        near_misses.push(NearMissRule {
                            rule_index,
                            rule_name: rule.name.clone(),
                            match_score,
                            matched_premises: matched,
                            unmatched_premises: unmatched.clone(),
                        });

                        reasons.push(WhyNotReason::UnsatisfiedPremises {
                            rule_name: rule.name.clone(),
                            missing_premises: unmatched,
                        });
                    }
                }
            }
        }

        // Sort near misses by match score
        near_misses.sort_by(|a, b| b.match_score.partial_cmp(&a.match_score).unwrap_or(std::cmp::Ordering::Equal));

        if reasons.is_empty() {
            reasons.push(WhyNotReason::NoApplicableRule);
        }

        WhyNotExplanation {
            target: target.clone(),
            reasons,
            near_miss_rules: near_misses,
        }
    }

    /// Check which premises are satisfied
    fn check_premises(&self, premises: &[Triple], bindings: &crate::term::Bindings) -> (Vec<Triple>, Vec<Triple>) {
        let mut matched = Vec::new();
        let mut unmatched = Vec::new();

        for premise in premises {
            let ground = crate::term::substitute_triple(premise, bindings);

            if ground.is_ground() && self.store.contains(&ground) {
                matched.push(ground);
            } else {
                // Try pattern matching
                let matches = self.store.match_pattern(&ground);
                if !matches.is_empty() {
                    matched.push(ground);
                } else {
                    unmatched.push(ground);
                }
            }
        }

        (matched, unmatched)
    }
}

/// Try to unify two triples, returning bindings if successful
fn try_unify(pattern: &Triple, ground: &Triple) -> Option<crate::term::Bindings> {
    let mut bindings = crate::term::Bindings::default();

    if !unify_term(&pattern.subject, &ground.subject, &mut bindings) {
        return None;
    }
    if !unify_term(&pattern.predicate, &ground.predicate, &mut bindings) {
        return None;
    }
    if !unify_term(&pattern.object, &ground.object, &mut bindings) {
        return None;
    }

    Some(bindings)
}

/// Unify a single term
fn unify_term(pattern: &Term, ground: &Term, bindings: &mut crate::term::Bindings) -> bool {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::store::Store;
    use crate::reasoner::incremental::IncrementalReasoner;

    fn make_rule(antecedent: Vec<Triple>, consequent: Vec<Triple>) -> Rule {
        Rule::new(antecedent, consequent)
    }

    #[test]
    fn test_explanation_asserted() {
        let mut store = Store::new();
        let triple = Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        );
        store.add(triple.clone());

        let mut reasoner = IncrementalReasoner::new();
        reasoner.materialize(&mut store);

        let generator = ExplanationGenerator::new()
            .with_dependencies(reasoner.dependencies());

        let explanation = generator.explain_triple(&triple);

        assert!(!explanation.steps.is_empty());
        assert!(matches!(
            explanation.steps[0].reason,
            ExplanationReason::Asserted
        ));
    }

    #[test]
    fn test_explanation_derived() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        ));

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

        let derived = Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Mortal"),
        );

        let generator = ExplanationGenerator::new()
            .with_dependencies(reasoner.dependencies())
            .with_rules(reasoner.rules());

        let explanation = generator.explain_triple(&derived);

        assert!(!explanation.steps.is_empty());
        assert!(matches!(
            explanation.steps[0].reason,
            ExplanationReason::Derived { .. }
        ));
    }

    #[test]
    fn test_explanation_markdown() {
        let mut explanation = Explanation::new();
        explanation.summary = "Test explanation".to_string();
        explanation.level = ExplanationLevel::Detailed;
        explanation.targets.push(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        ));
        explanation.steps.push(ExplanationStep {
            triple: Triple::new(
                Term::uri("http://example.org/a"),
                Term::uri("http://example.org/p"),
                Term::uri("http://example.org/b"),
            ),
            description: "Test step".to_string(),
            reason: ExplanationReason::Asserted,
            evidence: Vec::new(),
            depth: 0,
        });

        let markdown = explanation.to_markdown();
        assert!(markdown.contains("# Explanation"));
        assert!(markdown.contains("Test explanation"));
    }

    #[test]
    fn test_why_not_explanation() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        ));

        let rule = Rule::named(
            "mortal-rule",
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri("http://example.org/type"),
                    Term::uri("http://example.org/Human"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::uri("http://example.org/age"),
                    Term::universal("y"),
                ),
            ],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Mortal"),
            )],
        );

        let rules = vec![rule];
        let generator = WhyNotGenerator::new(&rules, &store);

        // Try to explain why socrates isn't mortal (missing age fact)
        let target = Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Mortal"),
        );

        let explanation = generator.explain(&target);

        assert!(!explanation.near_miss_rules.is_empty());
        assert!(explanation.near_miss_rules[0].match_score > 0.0);
    }
}
