//! Description Logic Tableau Reasoner
//!
//! Implementation of tableau-based reasoning for Description Logics,
//! supporting ALC and extensions. This enables OWL-style ontology reasoning.
//!
//! Supported constructs:
//! - Atomic concepts and roles
//! - Conjunction (⊓), Disjunction (⊔)
//! - Negation (¬)
//! - Existential restriction (∃R.C)
//! - Universal restriction (∀R.C)
//! - Number restrictions (≥n R.C, ≤n R.C)

use std::collections::{HashMap, HashSet, VecDeque};

/// A concept (class) expression
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Concept {
    /// Top concept (⊤) - everything
    Top,
    /// Bottom concept (⊥) - nothing
    Bottom,
    /// Atomic concept (named class)
    Atomic(String),
    /// Negation (¬C)
    Not(Box<Concept>),
    /// Conjunction (C ⊓ D)
    And(Box<Concept>, Box<Concept>),
    /// Disjunction (C ⊔ D)
    Or(Box<Concept>, Box<Concept>),
    /// Existential restriction (∃R.C)
    Exists(String, Box<Concept>),
    /// Universal restriction (∀R.C)
    Forall(String, Box<Concept>),
    /// At-least restriction (≥n R.C)
    AtLeast(usize, String, Box<Concept>),
    /// At-most restriction (≤n R.C)
    AtMost(usize, String, Box<Concept>),
}

impl Concept {
    pub fn atomic(name: &str) -> Self {
        Concept::Atomic(name.to_string())
    }

    pub fn not(c: Concept) -> Self {
        Concept::Not(Box::new(c))
    }

    pub fn and(c1: Concept, c2: Concept) -> Self {
        Concept::And(Box::new(c1), Box::new(c2))
    }

    pub fn or(c1: Concept, c2: Concept) -> Self {
        Concept::Or(Box::new(c1), Box::new(c2))
    }

    pub fn exists(role: &str, c: Concept) -> Self {
        Concept::Exists(role.to_string(), Box::new(c))
    }

    pub fn forall(role: &str, c: Concept) -> Self {
        Concept::Forall(role.to_string(), Box::new(c))
    }

    /// Convert to Negation Normal Form (NNF)
    pub fn to_nnf(&self) -> Concept {
        match self {
            Concept::Top | Concept::Bottom | Concept::Atomic(_) => self.clone(),

            Concept::Not(ref inner) => match &**inner {
                Concept::Top => Concept::Bottom,
                Concept::Bottom => Concept::Top,
                Concept::Atomic(_) => self.clone(),
                Concept::Not(ref c) => c.to_nnf(),
                Concept::And(ref c1, ref c2) => {
                    Concept::Or(
                        Box::new(Concept::Not(Box::new((**c1).clone())).to_nnf()),
                        Box::new(Concept::Not(Box::new((**c2).clone())).to_nnf()),
                    )
                }
                Concept::Or(ref c1, ref c2) => {
                    Concept::And(
                        Box::new(Concept::Not(Box::new((**c1).clone())).to_nnf()),
                        Box::new(Concept::Not(Box::new((**c2).clone())).to_nnf()),
                    )
                }
                Concept::Exists(r, ref c) => {
                    Concept::Forall(r.clone(), Box::new(Concept::Not(Box::new((**c).clone())).to_nnf()))
                }
                Concept::Forall(r, ref c) => {
                    Concept::Exists(r.clone(), Box::new(Concept::Not(Box::new((**c).clone())).to_nnf()))
                }
                Concept::AtLeast(n, r, ref c) => {
                    if *n == 0 {
                        Concept::Bottom
                    } else {
                        Concept::AtMost(n - 1, r.clone(), Box::new((**c).clone()))
                    }
                }
                Concept::AtMost(n, r, ref c) => {
                    Concept::AtLeast(n + 1, r.clone(), Box::new((**c).clone()))
                }
            },

            Concept::And(ref c1, ref c2) => {
                Concept::And(Box::new(c1.to_nnf()), Box::new(c2.to_nnf()))
            }
            Concept::Or(ref c1, ref c2) => {
                Concept::Or(Box::new(c1.to_nnf()), Box::new(c2.to_nnf()))
            }
            Concept::Exists(r, ref c) => {
                Concept::Exists(r.clone(), Box::new(c.to_nnf()))
            }
            Concept::Forall(r, ref c) => {
                Concept::Forall(r.clone(), Box::new(c.to_nnf()))
            }
            Concept::AtLeast(n, r, ref c) => {
                Concept::AtLeast(*n, r.clone(), Box::new(c.to_nnf()))
            }
            Concept::AtMost(n, r, ref c) => {
                Concept::AtMost(*n, r.clone(), Box::new(c.to_nnf()))
            }
        }
    }
}

/// An individual (instance)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Individual {
    pub name: String,
    pub is_nominal: bool, // true if named, false if generated
}

impl Individual {
    pub fn named(name: &str) -> Self {
        Individual {
            name: name.to_string(),
            is_nominal: true,
        }
    }

    pub fn generated(id: usize) -> Self {
        Individual {
            name: format!("_x{}", id),
            is_nominal: false,
        }
    }
}

/// A role (property) assertion
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RoleAssertion {
    pub role: String,
    pub from: Individual,
    pub to: Individual,
}

/// A concept assertion
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConceptAssertion {
    pub concept: Concept,
    pub individual: Individual,
}

/// Tableau node label
#[derive(Debug, Clone, Default)]
pub struct NodeLabel {
    /// Concepts asserted for this individual
    pub concepts: HashSet<Concept>,
    /// Concepts to be expanded
    pub unexpanded: VecDeque<Concept>,
}

/// Tableau configuration
#[derive(Debug, Clone)]
pub struct DlTableauConfig {
    /// Maximum number of individuals
    pub max_individuals: usize,
    /// Maximum expansion depth
    pub max_depth: usize,
    /// Verbose output
    pub verbose: bool,
}

impl Default for DlTableauConfig {
    fn default() -> Self {
        DlTableauConfig {
            max_individuals: 10000,
            max_depth: 100,
            verbose: false,
        }
    }
}

/// Result of DL reasoning
#[derive(Debug, Clone)]
pub enum DlResult {
    /// Concept is satisfiable
    Satisfiable,
    /// Concept is unsatisfiable
    Unsatisfiable,
    /// Subsumption holds (C ⊑ D)
    Subsumed,
    /// Subsumption does not hold
    NotSubsumed,
    /// Resource limit reached
    Unknown(String),
}

/// DL Tableau Reasoner
pub struct DlTableau {
    /// Configuration
    config: DlTableauConfig,
    /// TBox (concept inclusions)
    tbox: Vec<(Concept, Concept)>, // C ⊑ D as (C, D)
    /// ABox (assertions)
    abox_concepts: Vec<ConceptAssertion>,
    abox_roles: Vec<RoleAssertion>,
    /// Node labels
    labels: HashMap<String, NodeLabel>,
    /// Role assertions
    roles: HashSet<RoleAssertion>,
    /// Generated individual counter
    individual_counter: usize,
    /// Clash detected
    clash: bool,
}

impl DlTableau {
    pub fn new() -> Self {
        Self::with_config(DlTableauConfig::default())
    }

    pub fn with_config(config: DlTableauConfig) -> Self {
        DlTableau {
            config,
            tbox: Vec::new(),
            abox_concepts: Vec::new(),
            abox_roles: Vec::new(),
            labels: HashMap::new(),
            roles: HashSet::new(),
            individual_counter: 0,
            clash: false,
        }
    }

    /// Add a concept inclusion (C ⊑ D)
    pub fn add_subsumption(&mut self, sub: Concept, sup: Concept) {
        // C ⊑ D is equivalent to ⊤ ⊑ ¬C ⊔ D
        self.tbox.push((sub, sup));
    }

    /// Add a concept assertion (a : C)
    pub fn add_concept_assertion(&mut self, individual: &str, concept: Concept) {
        self.abox_concepts.push(ConceptAssertion {
            concept,
            individual: Individual::named(individual),
        });
    }

    /// Add a role assertion ((a, b) : R)
    pub fn add_role_assertion(&mut self, from: &str, role: &str, to: &str) {
        self.abox_roles.push(RoleAssertion {
            role: role.to_string(),
            from: Individual::named(from),
            to: Individual::named(to),
        });
    }

    /// Check if a concept is satisfiable
    pub fn is_satisfiable(&mut self, concept: &Concept) -> DlResult {
        self.reset();

        // Create initial individual
        let ind = self.new_individual();
        let concept_nnf = concept.to_nnf();

        // Add concept to individual's label
        self.add_concept_to_label(&ind, concept_nnf);

        // Apply TBox unfolding
        self.apply_tbox(&ind);

        // Run tableau expansion
        if self.expand() {
            DlResult::Satisfiable
        } else {
            DlResult::Unsatisfiable
        }
    }

    /// Check if C ⊑ D (C is subsumed by D)
    pub fn is_subsumed(&mut self, c: &Concept, d: &Concept) -> DlResult {
        // C ⊑ D iff C ⊓ ¬D is unsatisfiable
        let test = Concept::and(c.clone(), Concept::not(d.clone()));

        match self.is_satisfiable(&test) {
            DlResult::Unsatisfiable => DlResult::Subsumed,
            DlResult::Satisfiable => DlResult::NotSubsumed,
            other => other,
        }
    }

    /// Check ABox consistency
    pub fn is_consistent(&mut self) -> DlResult {
        self.reset();

        // Add ABox assertions
        for assertion in &self.abox_concepts.clone() {
            let concept_nnf = assertion.concept.to_nnf();
            self.add_concept_to_label(&assertion.individual, concept_nnf);
        }

        for assertion in &self.abox_roles.clone() {
            self.roles.insert(assertion.clone());
        }

        // Apply TBox to all individuals
        for (name, _) in self.labels.clone() {
            self.apply_tbox(&Individual { name, is_nominal: true });
        }

        // Run tableau expansion
        if self.expand() {
            DlResult::Satisfiable
        } else {
            DlResult::Unsatisfiable
        }
    }

    /// Reset the tableau
    fn reset(&mut self) {
        self.labels.clear();
        self.roles.clear();
        self.individual_counter = 0;
        self.clash = false;
    }

    /// Create a new individual
    fn new_individual(&mut self) -> Individual {
        self.individual_counter += 1;
        let ind = Individual::generated(self.individual_counter);
        self.labels.insert(ind.name.clone(), NodeLabel::default());
        ind
    }

    /// Add a concept to an individual's label
    fn add_concept_to_label(&mut self, ind: &Individual, concept: Concept) {
        let label = self.labels.entry(ind.name.clone()).or_default();

        if label.concepts.contains(&concept) {
            return;
        }

        // Check for clash
        let negation = Concept::Not(Box::new(concept.clone())).to_nnf();
        if label.concepts.contains(&negation) {
            self.clash = true;
            return;
        }

        // Check for bottom
        if concept == Concept::Bottom {
            self.clash = true;
            return;
        }

        label.concepts.insert(concept.clone());
        label.unexpanded.push_back(concept);
    }

    /// Apply TBox to an individual
    fn apply_tbox(&mut self, ind: &Individual) {
        for (sub, sup) in &self.tbox.clone() {
            // C ⊑ D means add ¬C ⊔ D to every individual
            let unfolded = Concept::or(Concept::not(sub.clone()), sup.clone()).to_nnf();
            self.add_concept_to_label(ind, unfolded);
        }
    }

    /// Main expansion loop
    fn expand(&mut self) -> bool {
        let mut iterations = 0;

        while !self.clash && iterations < self.config.max_individuals * 10 {
            iterations += 1;

            // Find an unexpanded concept
            let mut found = None;
            for (name, label) in &mut self.labels {
                if let Some(concept) = label.unexpanded.pop_front() {
                    found = Some((name.clone(), concept));
                    break;
                }
            }

            let (ind_name, concept) = match found {
                Some(x) => x,
                None => break, // All expanded
            };

            let ind = Individual {
                name: ind_name,
                is_nominal: true,
            };

            // Apply expansion rules
            match &concept {
                Concept::And(ref c1, ref c2) => {
                    // ⊓-rule: add both conjuncts
                    self.add_concept_to_label(&ind, (**c1).clone());
                    self.add_concept_to_label(&ind, (**c2).clone());
                }

                Concept::Or(ref c1, ref c2) => {
                    // ⊔-rule: non-deterministic choice
                    // Try c1 first (in a real implementation, would need backtracking)
                    let label = self.labels.get(&ind.name).unwrap();
                    if !label.concepts.contains(&**c1) && !label.concepts.contains(&**c2) {
                        // Try to add c1, if clash, try c2
                        let backup_labels = self.labels.clone();
                        let backup_roles = self.roles.clone();
                        let backup_clash = self.clash;

                        self.add_concept_to_label(&ind, (**c1).clone());

                        if self.clash {
                            // Restore and try c2
                            self.labels = backup_labels;
                            self.roles = backup_roles;
                            self.clash = backup_clash;
                            self.add_concept_to_label(&ind, (**c2).clone());
                        }
                    }
                }

                Concept::Exists(role, ref filler) => {
                    // ∃-rule: create new individual with filler
                    // Check if we already have such a successor
                    let has_successor = self.roles.iter().any(|r| {
                        r.role == *role && r.from == ind && {
                            let succ_label = self.labels.get(&r.to.name);
                            succ_label.map_or(false, |l| l.concepts.contains(&**filler))
                        }
                    });

                    if !has_successor && self.labels.len() < self.config.max_individuals {
                        let new_ind = self.new_individual();
                        self.roles.insert(RoleAssertion {
                            role: role.clone(),
                            from: ind.clone(),
                            to: new_ind.clone(),
                        });
                        self.add_concept_to_label(&new_ind, (**filler).clone());
                        self.apply_tbox(&new_ind);
                    }
                }

                Concept::Forall(role, ref filler) => {
                    // ∀-rule: add filler to all R-successors
                    let successors: Vec<Individual> = self
                        .roles
                        .iter()
                        .filter(|r| r.role == *role && r.from == ind)
                        .map(|r| r.to.clone())
                        .collect();

                    for succ in successors {
                        self.add_concept_to_label(&succ, (**filler).clone());
                    }
                }

                Concept::AtLeast(n, role, ref filler) if *n > 0 => {
                    // ≥n rule: create n new successors if needed
                    let current_count = self
                        .roles
                        .iter()
                        .filter(|r| {
                            r.role == *role && r.from == ind && {
                                let succ_label = self.labels.get(&r.to.name);
                                succ_label.map_or(false, |l| l.concepts.contains(&**filler))
                            }
                        })
                        .count();

                    for _ in current_count..*n {
                        if self.labels.len() >= self.config.max_individuals {
                            break;
                        }
                        let new_ind = self.new_individual();
                        self.roles.insert(RoleAssertion {
                            role: role.clone(),
                            from: ind.clone(),
                            to: new_ind.clone(),
                        });
                        self.add_concept_to_label(&new_ind, (**filler).clone());
                        self.apply_tbox(&new_ind);
                    }
                }

                Concept::AtMost(n, role, ref filler) => {
                    // ≤n rule: check cardinality constraint
                    let matching_successors: Vec<&RoleAssertion> = self
                        .roles
                        .iter()
                        .filter(|r| {
                            r.role == *role && r.from == ind && {
                                let succ_label = self.labels.get(&r.to.name);
                                succ_label.map_or(false, |l| l.concepts.contains(filler))
                            }
                        })
                        .collect();

                    if matching_successors.len() > *n {
                        // Clash - too many successors
                        // In a real implementation, would try merging
                        self.clash = true;
                    }
                }

                _ => {}
            }
        }

        !self.clash
    }

    /// Get statistics
    pub fn stats(&self) -> (usize, usize) {
        (self.labels.len(), self.roles.len())
    }
}

impl Default for DlTableau {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_concept_satisfiability() {
        let mut reasoner = DlTableau::new();

        // Person ⊓ ∃hasChild.Person should be satisfiable
        let concept = Concept::and(
            Concept::atomic("Person"),
            Concept::exists("hasChild", Concept::atomic("Person")),
        );

        assert!(matches!(reasoner.is_satisfiable(&concept), DlResult::Satisfiable));
    }

    #[test]
    fn test_concept_unsatisfiability() {
        let mut reasoner = DlTableau::new();

        // A ⊓ ¬A is unsatisfiable
        let concept = Concept::and(
            Concept::atomic("A"),
            Concept::not(Concept::atomic("A")),
        );

        assert!(matches!(reasoner.is_satisfiable(&concept), DlResult::Unsatisfiable));
    }

    #[test]
    fn test_subsumption() {
        let mut reasoner = DlTableau::new();

        // A ⊓ B ⊑ A should hold
        let c = Concept::and(Concept::atomic("A"), Concept::atomic("B"));
        let d = Concept::atomic("A");

        assert!(matches!(reasoner.is_subsumed(&c, &d), DlResult::Subsumed));
    }

    #[test]
    fn test_nnf_conversion() {
        // ¬(A ⊓ B) should become ¬A ⊔ ¬B
        let concept = Concept::not(Concept::and(
            Concept::atomic("A"),
            Concept::atomic("B"),
        ));

        let nnf = concept.to_nnf();

        match nnf {
            Concept::Or(ref left, ref right) => {
                if let (Concept::Not(ref l_inner), Concept::Not(ref r_inner)) = (&**left, &**right) {
                    if let (Concept::Atomic(ref a), Concept::Atomic(ref b)) = (&**l_inner, &**r_inner) {
                        assert_eq!(a, "A");
                        assert_eq!(b, "B");
                        return;
                    }
                }
                panic!("Expected Or of negations");
            }
            _ => panic!("Expected Or of negations"),
        }
    }
}
