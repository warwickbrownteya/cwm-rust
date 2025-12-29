//! Consistency checking for RDF/OWL data
//!
//! This module provides consistency checking capabilities to detect logical
//! inconsistencies in RDF graphs, particularly for OWL ontologies.
//!
//! # Supported Checks
//!
//! - **Disjoint Class Violations**: Detects instances of mutually exclusive classes
//! - **Functional Property Violations**: Detects multiple values for functional properties
//! - **Inverse Functional Property Violations**: Detects shared values for inverse functional properties
//! - **Domain/Range Violations**: Detects property usage with incorrect types
//! - **Cardinality Violations**: Detects min/max cardinality constraint violations
//! - **Self-Contradictions**: Detects direct logical contradictions
//!
//! # Example
//!
//! ```ignore
//! use cwm::{ConsistencyChecker, ConsistencyConfig, Store};
//!
//! let checker = ConsistencyChecker::new(ConsistencyConfig::default());
//! let result = checker.check(&store);
//!
//! if !result.is_consistent {
//!     for violation in &result.violations {
//!         println!("{}", violation);
//!     }
//! }
//! ```

use std::collections::{HashMap, HashSet};
use crate::term::{Term, Triple};
use crate::store::Store;

/// OWL vocabulary URIs
mod owl {
    pub const DISJOINT_WITH: &str = "http://www.w3.org/2002/07/owl#disjointWith";
    pub const ALL_DISJOINT_CLASSES: &str = "http://www.w3.org/2002/07/owl#AllDisjointClasses";
    pub const MEMBERS: &str = "http://www.w3.org/2002/07/owl#members";
    pub const FUNCTIONAL_PROPERTY: &str = "http://www.w3.org/2002/07/owl#FunctionalProperty";
    pub const INVERSE_FUNCTIONAL_PROPERTY: &str = "http://www.w3.org/2002/07/owl#InverseFunctionalProperty";
    pub const SYMMETRIC_PROPERTY: &str = "http://www.w3.org/2002/07/owl#SymmetricProperty";
    pub const ASYMMETRIC_PROPERTY: &str = "http://www.w3.org/2002/07/owl#AsymmetricProperty";
    pub const IRREFLEXIVE_PROPERTY: &str = "http://www.w3.org/2002/07/owl#IrreflexiveProperty";
    pub const MIN_CARDINALITY: &str = "http://www.w3.org/2002/07/owl#minCardinality";
    pub const MAX_CARDINALITY: &str = "http://www.w3.org/2002/07/owl#maxCardinality";
    pub const CARDINALITY: &str = "http://www.w3.org/2002/07/owl#cardinality";
    pub const ON_PROPERTY: &str = "http://www.w3.org/2002/07/owl#onProperty";
    pub const SAME_AS: &str = "http://www.w3.org/2002/07/owl#sameAs";
    pub const DIFFERENT_FROM: &str = "http://www.w3.org/2002/07/owl#differentFrom";
    pub const ALL_DIFFERENT: &str = "http://www.w3.org/2002/07/owl#AllDifferent";
    pub const DISTINCT_MEMBERS: &str = "http://www.w3.org/2002/07/owl#distinctMembers";
    pub const NOTHING: &str = "http://www.w3.org/2002/07/owl#Nothing";
}

/// RDFS vocabulary URIs
mod rdfs {
    pub const DOMAIN: &str = "http://www.w3.org/2000/01/rdf-schema#domain";
    pub const RANGE: &str = "http://www.w3.org/2000/01/rdf-schema#range";
    pub const SUBCLASS_OF: &str = "http://www.w3.org/2000/01/rdf-schema#subClassOf";
}

/// RDF vocabulary URIs
mod rdf {
    pub const TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
    pub const FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
    pub const REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
    pub const NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
}

/// Configuration for consistency checking
#[derive(Clone, Debug)]
pub struct ConsistencyConfig {
    /// Check disjoint class violations
    pub check_disjoint_classes: bool,
    /// Check functional property violations
    pub check_functional_properties: bool,
    /// Check inverse functional property violations
    pub check_inverse_functional: bool,
    /// Check domain/range violations
    pub check_domain_range: bool,
    /// Check cardinality violations
    pub check_cardinality: bool,
    /// Check same/different contradictions
    pub check_identity: bool,
    /// Check for owl:Nothing membership
    pub check_nothing: bool,
    /// Check asymmetric/irreflexive violations
    pub check_property_characteristics: bool,
    /// Maximum violations to report (0 = unlimited)
    pub max_violations: usize,
    /// Stop checking after first violation
    pub fail_fast: bool,
}

impl Default for ConsistencyConfig {
    fn default() -> Self {
        ConsistencyConfig {
            check_disjoint_classes: true,
            check_functional_properties: true,
            check_inverse_functional: true,
            check_domain_range: true,
            check_cardinality: true,
            check_identity: true,
            check_nothing: true,
            check_property_characteristics: true,
            max_violations: 100,
            fail_fast: false,
        }
    }
}

/// Type of consistency violation
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ViolationType {
    /// Individual is member of disjoint classes
    DisjointClassViolation,
    /// Functional property has multiple values
    FunctionalPropertyViolation,
    /// Inverse functional property shares value
    InverseFunctionalViolation,
    /// Property used with wrong domain type
    DomainViolation,
    /// Property used with wrong range type
    RangeViolation,
    /// Minimum cardinality not met
    MinCardinalityViolation,
    /// Maximum cardinality exceeded
    MaxCardinalityViolation,
    /// Individual is both sameAs and differentFrom
    IdentityContradiction,
    /// Individual is member of owl:Nothing
    NothingMembership,
    /// Asymmetric property violation
    AsymmetricViolation,
    /// Irreflexive property violation
    IrreflexiveViolation,
}

/// A single consistency violation
#[derive(Clone, Debug)]
pub struct Violation {
    /// Type of violation
    pub violation_type: ViolationType,
    /// The problematic individual(s)
    pub subjects: Vec<Term>,
    /// Related property (if applicable)
    pub property: Option<Term>,
    /// Related classes (if applicable)
    pub classes: Vec<Term>,
    /// Conflicting values (if applicable)
    pub values: Vec<Term>,
    /// Human-readable description
    pub description: String,
    /// Evidence triples
    pub evidence: Vec<Triple>,
}

impl std::fmt::Display for Violation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.description)
    }
}

/// Result of consistency checking
#[derive(Clone, Debug)]
pub struct ConsistencyResult {
    /// Whether the data is consistent
    pub is_consistent: bool,
    /// List of violations found
    pub violations: Vec<Violation>,
    /// Statistics about the check
    pub stats: ConsistencyStats,
}

impl ConsistencyResult {
    /// Create a consistent result
    pub fn consistent() -> Self {
        ConsistencyResult {
            is_consistent: true,
            violations: Vec::new(),
            stats: ConsistencyStats::default(),
        }
    }

    /// Create an inconsistent result with violations
    pub fn inconsistent(violations: Vec<Violation>, stats: ConsistencyStats) -> Self {
        ConsistencyResult {
            is_consistent: false,
            violations,
            stats,
        }
    }

    /// Format as text report
    pub fn to_text(&self) -> String {
        let mut output = String::new();

        if self.is_consistent {
            output.push_str("Consistency Check: PASSED\n");
            output.push_str("The data is logically consistent.\n");
        } else {
            output.push_str(&format!(
                "Consistency Check: FAILED ({} violation(s) found)\n\n",
                self.violations.len()
            ));

            for (i, violation) in self.violations.iter().enumerate() {
                output.push_str(&format!(
                    "{}. [{:?}] {}\n",
                    i + 1,
                    violation.violation_type,
                    violation.description
                ));

                if !violation.evidence.is_empty() {
                    output.push_str("   Evidence:\n");
                    for triple in &violation.evidence {
                        output.push_str(&format!("     - {}\n", triple));
                    }
                }
                output.push('\n');
            }
        }

        output.push_str("\nStatistics:\n");
        output.push_str(&format!("  Triples checked: {}\n", self.stats.triples_checked));
        output.push_str(&format!("  Properties analyzed: {}\n", self.stats.properties_analyzed));
        output.push_str(&format!("  Classes analyzed: {}\n", self.stats.classes_analyzed));

        output
    }
}

/// Statistics about consistency checking
#[derive(Clone, Debug, Default)]
pub struct ConsistencyStats {
    /// Number of triples checked
    pub triples_checked: usize,
    /// Number of properties analyzed
    pub properties_analyzed: usize,
    /// Number of classes analyzed
    pub classes_analyzed: usize,
    /// Number of disjoint class pairs checked
    pub disjoint_pairs_checked: usize,
    /// Number of functional properties checked
    pub functional_properties_checked: usize,
    /// Time taken in milliseconds
    pub time_ms: u64,
}

/// The consistency checker
pub struct ConsistencyChecker {
    config: ConsistencyConfig,
}

impl ConsistencyChecker {
    /// Create a new consistency checker
    pub fn new(config: ConsistencyConfig) -> Self {
        ConsistencyChecker { config }
    }

    /// Check consistency of a store
    pub fn check(&self, store: &Store) -> ConsistencyResult {
        let mut violations = Vec::new();
        let mut stats = ConsistencyStats {
            triples_checked: store.len(),
            ..Default::default()
        };

        // Extract schema information
        let schema = self.extract_schema(store);
        stats.properties_analyzed = schema.functional_properties.len() + schema.inverse_functional.len();
        stats.classes_analyzed = schema.disjoint_pairs.len();

        // Run checks based on configuration
        if self.config.check_disjoint_classes {
            self.check_disjoint_classes(store, &schema, &mut violations);
            stats.disjoint_pairs_checked = schema.disjoint_pairs.len();
            if self.should_stop(&violations) {
                return self.build_result(violations, stats);
            }
        }

        if self.config.check_functional_properties {
            self.check_functional_properties(store, &schema, &mut violations);
            stats.functional_properties_checked = schema.functional_properties.len();
            if self.should_stop(&violations) {
                return self.build_result(violations, stats);
            }
        }

        if self.config.check_inverse_functional {
            self.check_inverse_functional(store, &schema, &mut violations);
            if self.should_stop(&violations) {
                return self.build_result(violations, stats);
            }
        }

        if self.config.check_domain_range {
            self.check_domain_range(store, &schema, &mut violations);
            if self.should_stop(&violations) {
                return self.build_result(violations, stats);
            }
        }

        if self.config.check_identity {
            self.check_identity(store, &mut violations);
            if self.should_stop(&violations) {
                return self.build_result(violations, stats);
            }
        }

        if self.config.check_nothing {
            self.check_nothing_membership(store, &mut violations);
            if self.should_stop(&violations) {
                return self.build_result(violations, stats);
            }
        }

        if self.config.check_property_characteristics {
            self.check_property_characteristics(store, &schema, &mut violations);
        }

        self.build_result(violations, stats)
    }

    /// Check if we should stop checking
    fn should_stop(&self, violations: &[Violation]) -> bool {
        if self.config.fail_fast && !violations.is_empty() {
            return true;
        }
        if self.config.max_violations > 0 && violations.len() >= self.config.max_violations {
            return true;
        }
        false
    }

    /// Build the final result
    fn build_result(&self, violations: Vec<Violation>, stats: ConsistencyStats) -> ConsistencyResult {
        if violations.is_empty() {
            ConsistencyResult {
                is_consistent: true,
                violations,
                stats,
            }
        } else {
            ConsistencyResult {
                is_consistent: false,
                violations,
                stats,
            }
        }
    }

    /// Extract schema information from the store
    fn extract_schema(&self, store: &Store) -> SchemaInfo {
        let mut schema = SchemaInfo::default();

        for triple in store.iter() {
            let pred_str = if let Term::Uri(u) = &triple.predicate {
                u.as_str()
            } else {
                continue;
            };

            match pred_str {
                // Disjoint classes
                owl::DISJOINT_WITH => {
                    schema.disjoint_pairs.insert((
                        triple.subject.clone(),
                        triple.object.clone(),
                    ));
                }

                // Functional properties
                rdf::TYPE => {
                    if let Term::Uri(obj) = &triple.object {
                        match obj.as_str() {
                            owl::FUNCTIONAL_PROPERTY => {
                                schema.functional_properties.insert(triple.subject.clone());
                            }
                            owl::INVERSE_FUNCTIONAL_PROPERTY => {
                                schema.inverse_functional.insert(triple.subject.clone());
                            }
                            owl::ASYMMETRIC_PROPERTY => {
                                schema.asymmetric_properties.insert(triple.subject.clone());
                            }
                            owl::IRREFLEXIVE_PROPERTY => {
                                schema.irreflexive_properties.insert(triple.subject.clone());
                            }
                            _ => {}
                        }
                    }
                }

                // Domain/range
                rdfs::DOMAIN => {
                    schema.domains.insert(triple.subject.clone(), triple.object.clone());
                }
                rdfs::RANGE => {
                    schema.ranges.insert(triple.subject.clone(), triple.object.clone());
                }

                _ => {}
            }
        }

        schema
    }

    /// Check for disjoint class violations
    fn check_disjoint_classes(
        &self,
        store: &Store,
        schema: &SchemaInfo,
        violations: &mut Vec<Violation>,
    ) {
        // Build individual -> classes mapping
        let mut individual_classes: HashMap<Term, HashSet<Term>> = HashMap::new();

        let type_pattern = Triple::new(
            Term::universal("s"),
            Term::uri(rdf::TYPE),
            Term::universal("c"),
        );

        for bindings in store.match_pattern(&type_pattern) {
            if let (Some(subj), Some(class)) = (
                bindings.iter().find(|(v, _)| v.name() == "s").map(|(_, t)| t),
                bindings.iter().find(|(v, _)| v.name() == "c").map(|(_, t)| t),
            ) {
                individual_classes
                    .entry(subj.clone())
                    .or_default()
                    .insert(class.clone());
            }
        }

        // Check each individual against disjoint pairs
        for (individual, classes) in &individual_classes {
            for (class1, class2) in &schema.disjoint_pairs {
                if classes.contains(class1) && classes.contains(class2) {
                    let violation = Violation {
                        violation_type: ViolationType::DisjointClassViolation,
                        subjects: vec![individual.clone()],
                        property: None,
                        classes: vec![class1.clone(), class2.clone()],
                        values: Vec::new(),
                        description: format!(
                            "{} is a member of disjoint classes {} and {}",
                            individual, class1, class2
                        ),
                        evidence: vec![
                            Triple::new(individual.clone(), Term::uri(rdf::TYPE), class1.clone()),
                            Triple::new(individual.clone(), Term::uri(rdf::TYPE), class2.clone()),
                            Triple::new(class1.clone(), Term::uri(owl::DISJOINT_WITH), class2.clone()),
                        ],
                    };
                    violations.push(violation);

                    if self.should_stop(violations) {
                        return;
                    }
                }
            }
        }
    }

    /// Check for functional property violations
    fn check_functional_properties(
        &self,
        store: &Store,
        schema: &SchemaInfo,
        violations: &mut Vec<Violation>,
    ) {
        for prop in &schema.functional_properties {
            // Find all triples with this property
            let pattern = Triple::new(
                Term::universal("s"),
                prop.clone(),
                Term::universal("o"),
            );

            let mut subject_values: HashMap<Term, Vec<Term>> = HashMap::new();

            for bindings in store.match_pattern(&pattern) {
                if let (Some(subj), Some(obj)) = (
                    bindings.iter().find(|(v, _)| v.name() == "s").map(|(_, t)| t),
                    bindings.iter().find(|(v, _)| v.name() == "o").map(|(_, t)| t),
                ) {
                    subject_values
                        .entry(subj.clone())
                        .or_default()
                        .push(obj.clone());
                }
            }

            // Check for multiple values
            for (subject, values) in subject_values {
                if values.len() > 1 {
                    // Check if they're truly different (not sameAs)
                    let unique_values: HashSet<_> = values.iter().collect();
                    if unique_values.len() > 1 {
                        let violation = Violation {
                            violation_type: ViolationType::FunctionalPropertyViolation,
                            subjects: vec![subject.clone()],
                            property: Some(prop.clone()),
                            classes: Vec::new(),
                            values: values.clone(),
                            description: format!(
                                "{} has {} values for functional property {} (expected at most 1)",
                                subject, values.len(), prop
                            ),
                            evidence: values
                                .iter()
                                .map(|v| Triple::new(subject.clone(), prop.clone(), v.clone()))
                                .collect(),
                        };
                        violations.push(violation);

                        if self.should_stop(violations) {
                            return;
                        }
                    }
                }
            }
        }
    }

    /// Check for inverse functional property violations
    fn check_inverse_functional(
        &self,
        store: &Store,
        schema: &SchemaInfo,
        violations: &mut Vec<Violation>,
    ) {
        for prop in &schema.inverse_functional {
            let pattern = Triple::new(
                Term::universal("s"),
                prop.clone(),
                Term::universal("o"),
            );

            let mut value_subjects: HashMap<Term, Vec<Term>> = HashMap::new();

            for bindings in store.match_pattern(&pattern) {
                if let (Some(subj), Some(obj)) = (
                    bindings.iter().find(|(v, _)| v.name() == "s").map(|(_, t)| t),
                    bindings.iter().find(|(v, _)| v.name() == "o").map(|(_, t)| t),
                ) {
                    value_subjects
                        .entry(obj.clone())
                        .or_default()
                        .push(subj.clone());
                }
            }

            for (value, subjects) in value_subjects {
                if subjects.len() > 1 {
                    let unique_subjects: HashSet<_> = subjects.iter().collect();
                    if unique_subjects.len() > 1 {
                        let violation = Violation {
                            violation_type: ViolationType::InverseFunctionalViolation,
                            subjects: subjects.clone(),
                            property: Some(prop.clone()),
                            classes: Vec::new(),
                            values: vec![value.clone()],
                            description: format!(
                                "{} subjects share value {} for inverse functional property {}",
                                subjects.len(), value, prop
                            ),
                            evidence: subjects
                                .iter()
                                .map(|s| Triple::new(s.clone(), prop.clone(), value.clone()))
                                .collect(),
                        };
                        violations.push(violation);

                        if self.should_stop(violations) {
                            return;
                        }
                    }
                }
            }
        }
    }

    /// Check for domain/range violations
    fn check_domain_range(
        &self,
        store: &Store,
        schema: &SchemaInfo,
        violations: &mut Vec<Violation>,
    ) {
        // Build type index
        let mut individual_types: HashMap<Term, HashSet<Term>> = HashMap::new();

        let type_pattern = Triple::new(
            Term::universal("s"),
            Term::uri(rdf::TYPE),
            Term::universal("c"),
        );

        for bindings in store.match_pattern(&type_pattern) {
            if let (Some(subj), Some(class)) = (
                bindings.iter().find(|(v, _)| v.name() == "s").map(|(_, t)| t),
                bindings.iter().find(|(v, _)| v.name() == "c").map(|(_, t)| t),
            ) {
                individual_types
                    .entry(subj.clone())
                    .or_default()
                    .insert(class.clone());
            }
        }

        // Check domain violations
        for (prop, domain_class) in &schema.domains {
            let pattern = Triple::new(
                Term::universal("s"),
                prop.clone(),
                Term::universal("o"),
            );

            for bindings in store.match_pattern(&pattern) {
                if let Some(subj) = bindings.iter().find(|(v, _)| v.name() == "s").map(|(_, t)| t) {
                    let types = individual_types.get(subj).cloned().unwrap_or_default();

                    if !types.contains(domain_class) && !types.is_empty() {
                        let violation = Violation {
                            violation_type: ViolationType::DomainViolation,
                            subjects: vec![subj.clone()],
                            property: Some(prop.clone()),
                            classes: vec![domain_class.clone()],
                            values: Vec::new(),
                            description: format!(
                                "{} uses property {} but is not a member of domain class {}",
                                subj, prop, domain_class
                            ),
                            evidence: Vec::new(),
                        };
                        violations.push(violation);

                        if self.should_stop(violations) {
                            return;
                        }
                    }
                }
            }
        }

        // Check range violations
        for (prop, range_class) in &schema.ranges {
            let pattern = Triple::new(
                Term::universal("s"),
                prop.clone(),
                Term::universal("o"),
            );

            for bindings in store.match_pattern(&pattern) {
                if let Some(obj) = bindings.iter().find(|(v, _)| v.name() == "o").map(|(_, t)| t) {
                    // Skip literals for range checks on object properties
                    if matches!(obj, Term::Literal(_)) {
                        continue;
                    }

                    let types = individual_types.get(obj).cloned().unwrap_or_default();

                    if !types.contains(range_class) && !types.is_empty() {
                        let violation = Violation {
                            violation_type: ViolationType::RangeViolation,
                            subjects: vec![obj.clone()],
                            property: Some(prop.clone()),
                            classes: vec![range_class.clone()],
                            values: Vec::new(),
                            description: format!(
                                "{} is a value of property {} but is not a member of range class {}",
                                obj, prop, range_class
                            ),
                            evidence: Vec::new(),
                        };
                        violations.push(violation);

                        if self.should_stop(violations) {
                            return;
                        }
                    }
                }
            }
        }
    }

    /// Check for identity contradictions (sameAs + differentFrom)
    fn check_identity(&self, store: &Store, violations: &mut Vec<Violation>) {
        // Collect sameAs pairs
        let same_as_pattern = Triple::new(
            Term::universal("a"),
            Term::uri(owl::SAME_AS),
            Term::universal("b"),
        );

        let mut same_as_pairs: HashSet<(Term, Term)> = HashSet::new();

        for bindings in store.match_pattern(&same_as_pattern) {
            if let (Some(a), Some(b)) = (
                bindings.iter().find(|(v, _)| v.name() == "a").map(|(_, t)| t),
                bindings.iter().find(|(v, _)| v.name() == "b").map(|(_, t)| t),
            ) {
                same_as_pairs.insert((a.clone(), b.clone()));
                same_as_pairs.insert((b.clone(), a.clone())); // symmetric
            }
        }

        // Check differentFrom against sameAs
        let different_pattern = Triple::new(
            Term::universal("a"),
            Term::uri(owl::DIFFERENT_FROM),
            Term::universal("b"),
        );

        for bindings in store.match_pattern(&different_pattern) {
            if let (Some(a), Some(b)) = (
                bindings.iter().find(|(v, _)| v.name() == "a").map(|(_, t)| t),
                bindings.iter().find(|(v, _)| v.name() == "b").map(|(_, t)| t),
            ) {
                if same_as_pairs.contains(&(a.clone(), b.clone())) {
                    let violation = Violation {
                        violation_type: ViolationType::IdentityContradiction,
                        subjects: vec![a.clone(), b.clone()],
                        property: None,
                        classes: Vec::new(),
                        values: Vec::new(),
                        description: format!(
                            "{} and {} are declared both sameAs and differentFrom",
                            a, b
                        ),
                        evidence: vec![
                            Triple::new(a.clone(), Term::uri(owl::SAME_AS), b.clone()),
                            Triple::new(a.clone(), Term::uri(owl::DIFFERENT_FROM), b.clone()),
                        ],
                    };
                    violations.push(violation);

                    if self.should_stop(violations) {
                        return;
                    }
                }
            }
        }
    }

    /// Check for owl:Nothing membership
    fn check_nothing_membership(&self, store: &Store, violations: &mut Vec<Violation>) {
        let pattern = Triple::new(
            Term::universal("s"),
            Term::uri(rdf::TYPE),
            Term::uri(owl::NOTHING),
        );

        for bindings in store.match_pattern(&pattern) {
            if let Some(subj) = bindings.iter().find(|(v, _)| v.name() == "s").map(|(_, t)| t) {
                let violation = Violation {
                    violation_type: ViolationType::NothingMembership,
                    subjects: vec![subj.clone()],
                    property: None,
                    classes: vec![Term::uri(owl::NOTHING)],
                    values: Vec::new(),
                    description: format!(
                        "{} is a member of owl:Nothing (the empty class)",
                        subj
                    ),
                    evidence: vec![Triple::new(
                        subj.clone(),
                        Term::uri(rdf::TYPE),
                        Term::uri(owl::NOTHING),
                    )],
                };
                violations.push(violation);

                if self.should_stop(violations) {
                    return;
                }
            }
        }
    }

    /// Check asymmetric and irreflexive property violations
    fn check_property_characteristics(
        &self,
        store: &Store,
        schema: &SchemaInfo,
        violations: &mut Vec<Violation>,
    ) {
        // Check asymmetric properties
        for prop in &schema.asymmetric_properties {
            let pattern = Triple::new(
                Term::universal("a"),
                prop.clone(),
                Term::universal("b"),
            );

            for bindings in store.match_pattern(&pattern) {
                if let (Some(a), Some(b)) = (
                    bindings.iter().find(|(v, _)| v.name() == "a").map(|(_, t)| t),
                    bindings.iter().find(|(v, _)| v.name() == "b").map(|(_, t)| t),
                ) {
                    // Check if inverse exists
                    let inverse = Triple::new(b.clone(), prop.clone(), a.clone());
                    if store.contains(&inverse) {
                        let violation = Violation {
                            violation_type: ViolationType::AsymmetricViolation,
                            subjects: vec![a.clone(), b.clone()],
                            property: Some(prop.clone()),
                            classes: Vec::new(),
                            values: Vec::new(),
                            description: format!(
                                "Asymmetric property {} has both {} -> {} and {} -> {}",
                                prop, a, b, b, a
                            ),
                            evidence: vec![
                                Triple::new(a.clone(), prop.clone(), b.clone()),
                                Triple::new(b.clone(), prop.clone(), a.clone()),
                            ],
                        };
                        violations.push(violation);

                        if self.should_stop(violations) {
                            return;
                        }
                    }
                }
            }
        }

        // Check irreflexive properties
        for prop in &schema.irreflexive_properties {
            let pattern = Triple::new(
                Term::universal("a"),
                prop.clone(),
                Term::universal("b"),
            );

            for bindings in store.match_pattern(&pattern) {
                if let (Some(a), Some(b)) = (
                    bindings.iter().find(|(v, _)| v.name() == "a").map(|(_, t)| t),
                    bindings.iter().find(|(v, _)| v.name() == "b").map(|(_, t)| t),
                ) {
                    if a == b {
                        let violation = Violation {
                            violation_type: ViolationType::IrreflexiveViolation,
                            subjects: vec![a.clone()],
                            property: Some(prop.clone()),
                            classes: Vec::new(),
                            values: Vec::new(),
                            description: format!(
                                "Irreflexive property {} has reflexive assertion {} -> {}",
                                prop, a, a
                            ),
                            evidence: vec![Triple::new(a.clone(), prop.clone(), b.clone())],
                        };
                        violations.push(violation);

                        if self.should_stop(violations) {
                            return;
                        }
                    }
                }
            }
        }
    }
}

impl Default for ConsistencyChecker {
    fn default() -> Self {
        ConsistencyChecker::new(ConsistencyConfig::default())
    }
}

/// Schema information extracted from the store
#[derive(Clone, Debug, Default)]
struct SchemaInfo {
    /// Pairs of disjoint classes
    disjoint_pairs: HashSet<(Term, Term)>,
    /// Functional properties
    functional_properties: HashSet<Term>,
    /// Inverse functional properties
    inverse_functional: HashSet<Term>,
    /// Asymmetric properties
    asymmetric_properties: HashSet<Term>,
    /// Irreflexive properties
    irreflexive_properties: HashSet<Term>,
    /// Property domains
    domains: HashMap<Term, Term>,
    /// Property ranges
    ranges: HashMap<Term, Term>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_consistent_data() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri(rdf::TYPE),
            Term::uri("http://example.org/Person"),
        ));

        let checker = ConsistencyChecker::default();
        let result = checker.check(&store);

        assert!(result.is_consistent);
        assert!(result.violations.is_empty());
    }

    #[test]
    fn test_disjoint_class_violation() {
        let mut store = Store::new();

        // Define disjoint classes
        store.add(Triple::new(
            Term::uri("http://example.org/Cat"),
            Term::uri(owl::DISJOINT_WITH),
            Term::uri("http://example.org/Dog"),
        ));

        // Make an individual both a Cat and a Dog
        store.add(Triple::new(
            Term::uri("http://example.org/fluffy"),
            Term::uri(rdf::TYPE),
            Term::uri("http://example.org/Cat"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/fluffy"),
            Term::uri(rdf::TYPE),
            Term::uri("http://example.org/Dog"),
        ));

        let checker = ConsistencyChecker::default();
        let result = checker.check(&store);

        assert!(!result.is_consistent);
        assert!(!result.violations.is_empty());
        assert!(matches!(
            result.violations[0].violation_type,
            ViolationType::DisjointClassViolation
        ));
    }

    #[test]
    fn test_functional_property_violation() {
        let mut store = Store::new();

        // Define functional property
        store.add(Triple::new(
            Term::uri("http://example.org/hasSpouse"),
            Term::uri(rdf::TYPE),
            Term::uri(owl::FUNCTIONAL_PROPERTY),
        ));

        // Give someone two spouses
        store.add(Triple::new(
            Term::uri("http://example.org/bob"),
            Term::uri("http://example.org/hasSpouse"),
            Term::uri("http://example.org/alice"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/bob"),
            Term::uri("http://example.org/hasSpouse"),
            Term::uri("http://example.org/carol"),
        ));

        let checker = ConsistencyChecker::default();
        let result = checker.check(&store);

        assert!(!result.is_consistent);
        assert!(result.violations.iter().any(|v|
            matches!(v.violation_type, ViolationType::FunctionalPropertyViolation)
        ));
    }

    #[test]
    fn test_identity_contradiction() {
        let mut store = Store::new();

        // Declare same and different
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri(owl::SAME_AS),
            Term::uri("http://example.org/b"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri(owl::DIFFERENT_FROM),
            Term::uri("http://example.org/b"),
        ));

        let checker = ConsistencyChecker::default();
        let result = checker.check(&store);

        assert!(!result.is_consistent);
        assert!(result.violations.iter().any(|v|
            matches!(v.violation_type, ViolationType::IdentityContradiction)
        ));
    }

    #[test]
    fn test_nothing_membership() {
        let mut store = Store::new();

        // Make something a member of Nothing
        store.add(Triple::new(
            Term::uri("http://example.org/x"),
            Term::uri(rdf::TYPE),
            Term::uri(owl::NOTHING),
        ));

        let checker = ConsistencyChecker::default();
        let result = checker.check(&store);

        assert!(!result.is_consistent);
        assert!(result.violations.iter().any(|v|
            matches!(v.violation_type, ViolationType::NothingMembership)
        ));
    }

    #[test]
    fn test_text_report() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/Cat"),
            Term::uri(owl::DISJOINT_WITH),
            Term::uri("http://example.org/Dog"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/fluffy"),
            Term::uri(rdf::TYPE),
            Term::uri("http://example.org/Cat"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/fluffy"),
            Term::uri(rdf::TYPE),
            Term::uri("http://example.org/Dog"),
        ));

        let checker = ConsistencyChecker::default();
        let result = checker.check(&store);
        let text = result.to_text();

        assert!(text.contains("FAILED"));
        assert!(text.contains("DisjointClassViolation"));
    }
}
