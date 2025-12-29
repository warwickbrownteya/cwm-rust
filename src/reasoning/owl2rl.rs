//! OWL 2 RL Reasoning Rules
//!
//! This module implements the OWL 2 RL (Rule Language) profile as defined in the
//! W3C OWL 2 Profiles specification (https://www.w3.org/TR/owl2-profiles/#OWL_2_RL).
//!
//! OWL 2 RL is designed for applications that require scalable reasoning without
//! sacrificing too much expressive power. It can be implemented using forward-chaining
//! rule engines.
//!
//! # Rule Categories
//!
//! ## Property Rules
//! | Rule | Pattern | Inference |
//! |------|---------|-----------|
//! | prp-symp | `P a owl:SymmetricProperty`, `X P Y` | `Y P X` |
//! | prp-trp | `P a owl:TransitiveProperty`, `X P Y`, `Y P Z` | `X P Z` |
//! | prp-inv1 | `P1 owl:inverseOf P2`, `X P1 Y` | `Y P2 X` |
//! | prp-inv2 | `P1 owl:inverseOf P2`, `X P2 Y` | `Y P1 X` |
//! | prp-fp | `P a owl:FunctionalProperty`, `X P Y1`, `X P Y2` | `Y1 owl:sameAs Y2` |
//! | prp-ifp | `P a owl:InverseFunctionalProperty`, `X1 P Y`, `X2 P Y` | `X1 owl:sameAs X2` |
//! | prp-eqp1 | `P1 owl:equivalentProperty P2`, `X P1 Y` | `X P2 Y` |
//! | prp-eqp2 | `P1 owl:equivalentProperty P2`, `X P2 Y` | `X P1 Y` |
//! | prp-spo1 | `P1 rdfs:subPropertyOf P2`, `X P1 Y` | `X P2 Y` |
//!
//! ## Class Rules
//! | Rule | Pattern | Inference |
//! |------|---------|-----------|
//! | cax-sco | `C1 rdfs:subClassOf C2`, `X rdf:type C1` | `X rdf:type C2` |
//! | cax-eqc1 | `C1 owl:equivalentClass C2`, `X rdf:type C1` | `X rdf:type C2` |
//! | cax-eqc2 | `C1 owl:equivalentClass C2`, `X rdf:type C2` | `X rdf:type C1` |
//!
//! ## Individual Rules
//! | Rule | Pattern | Inference |
//! |------|---------|-----------|
//! | eq-rep-s | `S1 owl:sameAs S2`, `S1 P O` | `S2 P O` |
//! | eq-rep-p | `P1 owl:sameAs P2`, `S P1 O` | `S P2 O` |
//! | eq-rep-o | `O1 owl:sameAs O2`, `S P O1` | `S P O2` |
//! | eq-sym | `X owl:sameAs Y` | `Y owl:sameAs X` |
//! | eq-trans | `X owl:sameAs Y`, `Y owl:sameAs Z` | `X owl:sameAs Z` |
//!
//! # Usage
//!
//! ```ignore
//! use cwm::reasoning::owl2rl::Owl2RlRules;
//! use cwm::Reasoner;
//!
//! let mut reasoner = Reasoner::new();
//! reasoner.add_rules(Owl2RlRules::all());
//! reasoner.run(&mut store);
//! ```

use crate::term::{Term, Triple};
use crate::reasoner::Rule;

/// OWL 2 namespace URIs
pub mod ns {
    pub const OWL: &str = "http://www.w3.org/2002/07/owl#";
    pub const RDF: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    pub const RDFS: &str = "http://www.w3.org/2000/01/rdf-schema#";

    // RDF terms
    pub const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
    pub const RDF_FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
    pub const RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
    pub const RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";

    // RDFS terms
    pub const RDFS_SUBCLASS_OF: &str = "http://www.w3.org/2000/01/rdf-schema#subClassOf";
    pub const RDFS_SUBPROPERTY_OF: &str = "http://www.w3.org/2000/01/rdf-schema#subPropertyOf";
    pub const RDFS_DOMAIN: &str = "http://www.w3.org/2000/01/rdf-schema#domain";
    pub const RDFS_RANGE: &str = "http://www.w3.org/2000/01/rdf-schema#range";

    // OWL terms - Properties
    pub const OWL_SYMMETRIC_PROPERTY: &str = "http://www.w3.org/2002/07/owl#SymmetricProperty";
    pub const OWL_TRANSITIVE_PROPERTY: &str = "http://www.w3.org/2002/07/owl#TransitiveProperty";
    pub const OWL_FUNCTIONAL_PROPERTY: &str = "http://www.w3.org/2002/07/owl#FunctionalProperty";
    pub const OWL_INVERSE_FUNCTIONAL_PROPERTY: &str = "http://www.w3.org/2002/07/owl#InverseFunctionalProperty";
    pub const OWL_INVERSE_OF: &str = "http://www.w3.org/2002/07/owl#inverseOf";
    pub const OWL_EQUIVALENT_PROPERTY: &str = "http://www.w3.org/2002/07/owl#equivalentProperty";

    // OWL terms - Classes
    pub const OWL_CLASS: &str = "http://www.w3.org/2002/07/owl#Class";
    pub const OWL_EQUIVALENT_CLASS: &str = "http://www.w3.org/2002/07/owl#equivalentClass";
    pub const OWL_INTERSECTION_OF: &str = "http://www.w3.org/2002/07/owl#intersectionOf";
    pub const OWL_UNION_OF: &str = "http://www.w3.org/2002/07/owl#unionOf";
    pub const OWL_COMPLEMENT_OF: &str = "http://www.w3.org/2002/07/owl#complementOf";
    pub const OWL_DISJOINT_WITH: &str = "http://www.w3.org/2002/07/owl#disjointWith";

    // OWL terms - Restrictions
    pub const OWL_ON_PROPERTY: &str = "http://www.w3.org/2002/07/owl#onProperty";
    pub const OWL_SOME_VALUES_FROM: &str = "http://www.w3.org/2002/07/owl#someValuesFrom";
    pub const OWL_ALL_VALUES_FROM: &str = "http://www.w3.org/2002/07/owl#allValuesFrom";
    pub const OWL_HAS_VALUE: &str = "http://www.w3.org/2002/07/owl#hasValue";
    pub const OWL_ON_CLASS: &str = "http://www.w3.org/2002/07/owl#onClass";

    // OWL terms - Individuals
    pub const OWL_SAME_AS: &str = "http://www.w3.org/2002/07/owl#sameAs";
    pub const OWL_DIFFERENT_FROM: &str = "http://www.w3.org/2002/07/owl#differentFrom";
    pub const OWL_ALL_DIFFERENT: &str = "http://www.w3.org/2002/07/owl#AllDifferent";
    pub const OWL_MEMBERS: &str = "http://www.w3.org/2002/07/owl#members";

    // OWL terms - Other
    pub const OWL_THING: &str = "http://www.w3.org/2002/07/owl#Thing";
    pub const OWL_NOTHING: &str = "http://www.w3.org/2002/07/owl#Nothing";
    pub const OWL_HAS_KEY: &str = "http://www.w3.org/2002/07/owl#hasKey";
    pub const OWL_SOURCE_INDIVIDUAL: &str = "http://www.w3.org/2002/07/owl#sourceIndividual";
    pub const OWL_ASSERTION_PROPERTY: &str = "http://www.w3.org/2002/07/owl#assertionProperty";
    pub const OWL_TARGET_INDIVIDUAL: &str = "http://www.w3.org/2002/07/owl#targetIndividual";
    pub const OWL_TARGET_VALUE: &str = "http://www.w3.org/2002/07/owl#targetValue";
}

/// OWL 2 RL entailment rules generator
pub struct Owl2RlRules;

impl Owl2RlRules {
    /// Create all OWL 2 RL entailment rules
    ///
    /// Returns a vector of rules implementing the core OWL 2 RL entailment patterns.
    pub fn all() -> Vec<Rule> {
        let mut rules = Vec::new();
        rules.extend(Self::property_rules());
        rules.extend(Self::class_rules());
        rules.extend(Self::individual_rules());
        rules.extend(Self::restriction_rules());
        rules
    }

    /// Get property-related rules
    pub fn property_rules() -> Vec<Rule> {
        vec![
            Self::prp_symp(),
            Self::prp_trp(),
            Self::prp_inv1(),
            Self::prp_inv2(),
            Self::prp_eqp1(),
            Self::prp_eqp2(),
            Self::prp_spo1(),
        ]
    }

    /// Get class-related rules
    pub fn class_rules() -> Vec<Rule> {
        vec![
            Self::cax_sco(),
            Self::cax_eqc1(),
            Self::cax_eqc2(),
            Self::scm_sco(),
            Self::scm_eqc1(),
            Self::scm_eqc2(),
        ]
    }

    /// Get individual/equality rules
    pub fn individual_rules() -> Vec<Rule> {
        vec![
            Self::eq_ref(),
            Self::eq_sym(),
            Self::eq_trans(),
            Self::eq_rep_s(),
            Self::eq_rep_p(),
            Self::eq_rep_o(),
        ]
    }

    /// Get restriction-related rules
    pub fn restriction_rules() -> Vec<Rule> {
        vec![
            Self::cls_svf1(),
            Self::cls_svf2(),
            Self::cls_avf(),
            Self::cls_hv1(),
            Self::cls_hv2(),
        ]
    }

    // =========================================================================
    // Property Rules (prp-*)
    // =========================================================================

    /// prp-symp: Symmetric property inference
    ///
    /// If P is a symmetric property and X P Y, then Y P X.
    ///
    /// ```text
    /// { ?p a owl:SymmetricProperty . ?x ?p ?y } => { ?y ?p ?x }
    /// ```
    pub fn prp_symp() -> Rule {
        Rule::named(
            "prp-symp",
            vec![
                Triple::new(
                    Term::universal("p"),
                    Term::uri(ns::RDF_TYPE),
                    Term::uri(ns::OWL_SYMMETRIC_PROPERTY),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p"),
                    Term::universal("y"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("y"),
                    Term::universal("p"),
                    Term::universal("x"),
                ),
            ],
        )
    }

    /// prp-trp: Transitive property inference
    ///
    /// If P is a transitive property, X P Y, and Y P Z, then X P Z.
    ///
    /// ```text
    /// { ?p a owl:TransitiveProperty . ?x ?p ?y . ?y ?p ?z } => { ?x ?p ?z }
    /// ```
    pub fn prp_trp() -> Rule {
        Rule::named(
            "prp-trp",
            vec![
                Triple::new(
                    Term::universal("p"),
                    Term::uri(ns::RDF_TYPE),
                    Term::uri(ns::OWL_TRANSITIVE_PROPERTY),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p"),
                    Term::universal("y"),
                ),
                Triple::new(
                    Term::universal("y"),
                    Term::universal("p"),
                    Term::universal("z"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p"),
                    Term::universal("z"),
                ),
            ],
        )
    }

    /// prp-inv1: Inverse property inference (direction 1)
    ///
    /// If P1 is the inverse of P2 and X P1 Y, then Y P2 X.
    ///
    /// ```text
    /// { ?p1 owl:inverseOf ?p2 . ?x ?p1 ?y } => { ?y ?p2 ?x }
    /// ```
    pub fn prp_inv1() -> Rule {
        Rule::named(
            "prp-inv1",
            vec![
                Triple::new(
                    Term::universal("p1"),
                    Term::uri(ns::OWL_INVERSE_OF),
                    Term::universal("p2"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p1"),
                    Term::universal("y"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("y"),
                    Term::universal("p2"),
                    Term::universal("x"),
                ),
            ],
        )
    }

    /// prp-inv2: Inverse property inference (direction 2)
    ///
    /// If P1 is the inverse of P2 and X P2 Y, then Y P1 X.
    ///
    /// ```text
    /// { ?p1 owl:inverseOf ?p2 . ?x ?p2 ?y } => { ?y ?p1 ?x }
    /// ```
    pub fn prp_inv2() -> Rule {
        Rule::named(
            "prp-inv2",
            vec![
                Triple::new(
                    Term::universal("p1"),
                    Term::uri(ns::OWL_INVERSE_OF),
                    Term::universal("p2"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p2"),
                    Term::universal("y"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("y"),
                    Term::universal("p1"),
                    Term::universal("x"),
                ),
            ],
        )
    }

    /// prp-eqp1: Equivalent property inference (direction 1)
    ///
    /// If P1 is equivalent to P2 and X P1 Y, then X P2 Y.
    ///
    /// ```text
    /// { ?p1 owl:equivalentProperty ?p2 . ?x ?p1 ?y } => { ?x ?p2 ?y }
    /// ```
    pub fn prp_eqp1() -> Rule {
        Rule::named(
            "prp-eqp1",
            vec![
                Triple::new(
                    Term::universal("p1"),
                    Term::uri(ns::OWL_EQUIVALENT_PROPERTY),
                    Term::universal("p2"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p1"),
                    Term::universal("y"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p2"),
                    Term::universal("y"),
                ),
            ],
        )
    }

    /// prp-eqp2: Equivalent property inference (direction 2)
    ///
    /// If P1 is equivalent to P2 and X P2 Y, then X P1 Y.
    ///
    /// ```text
    /// { ?p1 owl:equivalentProperty ?p2 . ?x ?p2 ?y } => { ?x ?p1 ?y }
    /// ```
    pub fn prp_eqp2() -> Rule {
        Rule::named(
            "prp-eqp2",
            vec![
                Triple::new(
                    Term::universal("p1"),
                    Term::uri(ns::OWL_EQUIVALENT_PROPERTY),
                    Term::universal("p2"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p2"),
                    Term::universal("y"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p1"),
                    Term::universal("y"),
                ),
            ],
        )
    }

    /// prp-spo1: Subproperty inference (same as RDFS7)
    ///
    /// If P1 is a subproperty of P2 and X P1 Y, then X P2 Y.
    ///
    /// ```text
    /// { ?p1 rdfs:subPropertyOf ?p2 . ?x ?p1 ?y } => { ?x ?p2 ?y }
    /// ```
    pub fn prp_spo1() -> Rule {
        Rule::named(
            "prp-spo1",
            vec![
                Triple::new(
                    Term::universal("p1"),
                    Term::uri(ns::RDFS_SUBPROPERTY_OF),
                    Term::universal("p2"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p1"),
                    Term::universal("y"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p2"),
                    Term::universal("y"),
                ),
            ],
        )
    }

    // =========================================================================
    // Class Axiom Rules (cax-*)
    // =========================================================================

    /// cax-sco: Subclass type inference (same as RDFS9)
    ///
    /// If C1 is a subclass of C2 and X is of type C1, then X is of type C2.
    ///
    /// ```text
    /// { ?c1 rdfs:subClassOf ?c2 . ?x a ?c1 } => { ?x a ?c2 }
    /// ```
    pub fn cax_sco() -> Rule {
        Rule::named(
            "cax-sco",
            vec![
                Triple::new(
                    Term::universal("c1"),
                    Term::uri(ns::RDFS_SUBCLASS_OF),
                    Term::universal("c2"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("c1"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("c2"),
                ),
            ],
        )
    }

    /// cax-eqc1: Equivalent class type inference (direction 1)
    ///
    /// If C1 is equivalent to C2 and X is of type C1, then X is of type C2.
    ///
    /// ```text
    /// { ?c1 owl:equivalentClass ?c2 . ?x a ?c1 } => { ?x a ?c2 }
    /// ```
    pub fn cax_eqc1() -> Rule {
        Rule::named(
            "cax-eqc1",
            vec![
                Triple::new(
                    Term::universal("c1"),
                    Term::uri(ns::OWL_EQUIVALENT_CLASS),
                    Term::universal("c2"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("c1"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("c2"),
                ),
            ],
        )
    }

    /// cax-eqc2: Equivalent class type inference (direction 2)
    ///
    /// If C1 is equivalent to C2 and X is of type C2, then X is of type C1.
    ///
    /// ```text
    /// { ?c1 owl:equivalentClass ?c2 . ?x a ?c2 } => { ?x a ?c1 }
    /// ```
    pub fn cax_eqc2() -> Rule {
        Rule::named(
            "cax-eqc2",
            vec![
                Triple::new(
                    Term::universal("c1"),
                    Term::uri(ns::OWL_EQUIVALENT_CLASS),
                    Term::universal("c2"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("c2"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("c1"),
                ),
            ],
        )
    }

    /// scm-sco: Subclass transitivity (same as RDFS11)
    ///
    /// If C1 is a subclass of C2 and C2 is a subclass of C3, then C1 is a subclass of C3.
    ///
    /// ```text
    /// { ?c1 rdfs:subClassOf ?c2 . ?c2 rdfs:subClassOf ?c3 } => { ?c1 rdfs:subClassOf ?c3 }
    /// ```
    pub fn scm_sco() -> Rule {
        Rule::named(
            "scm-sco",
            vec![
                Triple::new(
                    Term::universal("c1"),
                    Term::uri(ns::RDFS_SUBCLASS_OF),
                    Term::universal("c2"),
                ),
                Triple::new(
                    Term::universal("c2"),
                    Term::uri(ns::RDFS_SUBCLASS_OF),
                    Term::universal("c3"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("c1"),
                    Term::uri(ns::RDFS_SUBCLASS_OF),
                    Term::universal("c3"),
                ),
            ],
        )
    }

    /// scm-eqc1: Equivalent class implies mutual subclass (direction 1)
    ///
    /// If C1 is equivalent to C2, then C1 is a subclass of C2.
    ///
    /// ```text
    /// { ?c1 owl:equivalentClass ?c2 } => { ?c1 rdfs:subClassOf ?c2 }
    /// ```
    pub fn scm_eqc1() -> Rule {
        Rule::named(
            "scm-eqc1",
            vec![
                Triple::new(
                    Term::universal("c1"),
                    Term::uri(ns::OWL_EQUIVALENT_CLASS),
                    Term::universal("c2"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("c1"),
                    Term::uri(ns::RDFS_SUBCLASS_OF),
                    Term::universal("c2"),
                ),
            ],
        )
    }

    /// scm-eqc2: Equivalent class implies mutual subclass (direction 2)
    ///
    /// If C1 is equivalent to C2, then C2 is a subclass of C1.
    ///
    /// ```text
    /// { ?c1 owl:equivalentClass ?c2 } => { ?c2 rdfs:subClassOf ?c1 }
    /// ```
    pub fn scm_eqc2() -> Rule {
        Rule::named(
            "scm-eqc2",
            vec![
                Triple::new(
                    Term::universal("c1"),
                    Term::uri(ns::OWL_EQUIVALENT_CLASS),
                    Term::universal("c2"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("c2"),
                    Term::uri(ns::RDFS_SUBCLASS_OF),
                    Term::universal("c1"),
                ),
            ],
        )
    }

    // =========================================================================
    // Equality Rules (eq-*)
    // =========================================================================

    /// eq-ref: Everything is owl:sameAs itself (reflexivity)
    ///
    /// For any X that appears in a triple, X sameAs X.
    /// Note: This rule is expensive and often omitted in practice.
    ///
    /// ```text
    /// { ?x ?p ?y } => { ?x owl:sameAs ?x }
    /// ```
    pub fn eq_ref() -> Rule {
        Rule::named(
            "eq-ref",
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p"),
                    Term::universal("y"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::OWL_SAME_AS),
                    Term::universal("x"),
                ),
            ],
        )
    }

    /// eq-sym: sameAs is symmetric
    ///
    /// If X sameAs Y, then Y sameAs X.
    ///
    /// ```text
    /// { ?x owl:sameAs ?y } => { ?y owl:sameAs ?x }
    /// ```
    pub fn eq_sym() -> Rule {
        Rule::named(
            "eq-sym",
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::OWL_SAME_AS),
                    Term::universal("y"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("y"),
                    Term::uri(ns::OWL_SAME_AS),
                    Term::universal("x"),
                ),
            ],
        )
    }

    /// eq-trans: sameAs is transitive
    ///
    /// If X sameAs Y and Y sameAs Z, then X sameAs Z.
    ///
    /// ```text
    /// { ?x owl:sameAs ?y . ?y owl:sameAs ?z } => { ?x owl:sameAs ?z }
    /// ```
    pub fn eq_trans() -> Rule {
        Rule::named(
            "eq-trans",
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::OWL_SAME_AS),
                    Term::universal("y"),
                ),
                Triple::new(
                    Term::universal("y"),
                    Term::uri(ns::OWL_SAME_AS),
                    Term::universal("z"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::OWL_SAME_AS),
                    Term::universal("z"),
                ),
            ],
        )
    }

    /// eq-rep-s: Subject replacement under sameAs
    ///
    /// If S1 sameAs S2 and S1 P O, then S2 P O.
    ///
    /// ```text
    /// { ?s1 owl:sameAs ?s2 . ?s1 ?p ?o } => { ?s2 ?p ?o }
    /// ```
    pub fn eq_rep_s() -> Rule {
        Rule::named(
            "eq-rep-s",
            vec![
                Triple::new(
                    Term::universal("s1"),
                    Term::uri(ns::OWL_SAME_AS),
                    Term::universal("s2"),
                ),
                Triple::new(
                    Term::universal("s1"),
                    Term::universal("p"),
                    Term::universal("o"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("s2"),
                    Term::universal("p"),
                    Term::universal("o"),
                ),
            ],
        )
    }

    /// eq-rep-p: Predicate replacement under sameAs
    ///
    /// If P1 sameAs P2 and S P1 O, then S P2 O.
    ///
    /// ```text
    /// { ?p1 owl:sameAs ?p2 . ?s ?p1 ?o } => { ?s ?p2 ?o }
    /// ```
    pub fn eq_rep_p() -> Rule {
        Rule::named(
            "eq-rep-p",
            vec![
                Triple::new(
                    Term::universal("p1"),
                    Term::uri(ns::OWL_SAME_AS),
                    Term::universal("p2"),
                ),
                Triple::new(
                    Term::universal("s"),
                    Term::universal("p1"),
                    Term::universal("o"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("s"),
                    Term::universal("p2"),
                    Term::universal("o"),
                ),
            ],
        )
    }

    /// eq-rep-o: Object replacement under sameAs
    ///
    /// If O1 sameAs O2 and S P O1, then S P O2.
    ///
    /// ```text
    /// { ?o1 owl:sameAs ?o2 . ?s ?p ?o1 } => { ?s ?p ?o2 }
    /// ```
    pub fn eq_rep_o() -> Rule {
        Rule::named(
            "eq-rep-o",
            vec![
                Triple::new(
                    Term::universal("o1"),
                    Term::uri(ns::OWL_SAME_AS),
                    Term::universal("o2"),
                ),
                Triple::new(
                    Term::universal("s"),
                    Term::universal("p"),
                    Term::universal("o1"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("s"),
                    Term::universal("p"),
                    Term::universal("o2"),
                ),
            ],
        )
    }

    // =========================================================================
    // Property Restriction Rules (cls-*)
    // =========================================================================

    /// cls-svf1: someValuesFrom inference (type propagation)
    ///
    /// If X is of type C which is a restriction with someValuesFrom D on property P,
    /// and X P Y where Y is of type D, then inference is satisfied.
    /// This rule infers types from someValuesFrom restrictions.
    ///
    /// ```text
    /// { ?c owl:someValuesFrom ?d . ?c owl:onProperty ?p . ?x ?p ?y . ?y a ?d } => { ?x a ?c }
    /// ```
    pub fn cls_svf1() -> Rule {
        Rule::named(
            "cls-svf1",
            vec![
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::OWL_SOME_VALUES_FROM),
                    Term::universal("d"),
                ),
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::OWL_ON_PROPERTY),
                    Term::universal("p"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p"),
                    Term::universal("y"),
                ),
                Triple::new(
                    Term::universal("y"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("d"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("c"),
                ),
            ],
        )
    }

    /// cls-svf2: someValuesFrom with owl:Thing
    ///
    /// If C has someValuesFrom owl:Thing on property P, and X P Y, then X is of type C.
    ///
    /// ```text
    /// { ?c owl:someValuesFrom owl:Thing . ?c owl:onProperty ?p . ?x ?p ?y } => { ?x a ?c }
    /// ```
    pub fn cls_svf2() -> Rule {
        Rule::named(
            "cls-svf2",
            vec![
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::OWL_SOME_VALUES_FROM),
                    Term::uri(ns::OWL_THING),
                ),
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::OWL_ON_PROPERTY),
                    Term::universal("p"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p"),
                    Term::universal("y"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("c"),
                ),
            ],
        )
    }

    /// cls-avf: allValuesFrom inference
    ///
    /// If X is of type C which is a restriction with allValuesFrom D on property P,
    /// and X P Y, then Y is of type D.
    ///
    /// ```text
    /// { ?x a ?c . ?c owl:allValuesFrom ?d . ?c owl:onProperty ?p . ?x ?p ?y } => { ?y a ?d }
    /// ```
    pub fn cls_avf() -> Rule {
        Rule::named(
            "cls-avf",
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("c"),
                ),
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::OWL_ALL_VALUES_FROM),
                    Term::universal("d"),
                ),
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::OWL_ON_PROPERTY),
                    Term::universal("p"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p"),
                    Term::universal("y"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("y"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("d"),
                ),
            ],
        )
    }

    /// cls-hv1: hasValue inference (type to property)
    ///
    /// If X is of type C which is a restriction with hasValue V on property P,
    /// then X P V.
    ///
    /// ```text
    /// { ?x a ?c . ?c owl:hasValue ?v . ?c owl:onProperty ?p } => { ?x ?p ?v }
    /// ```
    pub fn cls_hv1() -> Rule {
        Rule::named(
            "cls-hv1",
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("c"),
                ),
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::OWL_HAS_VALUE),
                    Term::universal("v"),
                ),
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::OWL_ON_PROPERTY),
                    Term::universal("p"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p"),
                    Term::universal("v"),
                ),
            ],
        )
    }

    /// cls-hv2: hasValue inference (property to type)
    ///
    /// If C is a restriction with hasValue V on property P, and X P V, then X is of type C.
    ///
    /// ```text
    /// { ?c owl:hasValue ?v . ?c owl:onProperty ?p . ?x ?p ?v } => { ?x a ?c }
    /// ```
    pub fn cls_hv2() -> Rule {
        Rule::named(
            "cls-hv2",
            vec![
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::OWL_HAS_VALUE),
                    Term::universal("v"),
                ),
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::OWL_ON_PROPERTY),
                    Term::universal("p"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::universal("p"),
                    Term::universal("v"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("c"),
                ),
            ],
        )
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Store;
    use crate::Reasoner;

    #[test]
    fn test_all_rules_count() {
        let rules = Owl2RlRules::all();
        // 7 property + 6 class + 6 individual + 5 restriction = 24 rules
        assert_eq!(rules.len(), 24);
    }

    #[test]
    fn test_symmetric_property() {
        let mut store = Store::new();
        let mut reasoner = Reasoner::new();

        // Define :knows as symmetric
        store.add(Triple::new(
            Term::uri("http://ex.org/knows"),
            Term::uri(ns::RDF_TYPE),
            Term::uri(ns::OWL_SYMMETRIC_PROPERTY),
        ));

        // Alice knows Bob
        store.add(Triple::new(
            Term::uri("http://ex.org/alice"),
            Term::uri("http://ex.org/knows"),
            Term::uri("http://ex.org/bob"),
        ));

        reasoner.add_rule(Owl2RlRules::prp_symp());
        reasoner.run(&mut store);

        // Bob should now know Alice
        let expected = Triple::new(
            Term::uri("http://ex.org/bob"),
            Term::uri("http://ex.org/knows"),
            Term::uri("http://ex.org/alice"),
        );
        assert!(store.iter().any(|t| t == &expected), "Symmetric inference failed");
    }

    #[test]
    fn test_transitive_property() {
        let mut store = Store::new();
        let mut reasoner = Reasoner::new();

        // Define :ancestor as transitive
        store.add(Triple::new(
            Term::uri("http://ex.org/ancestor"),
            Term::uri(ns::RDF_TYPE),
            Term::uri(ns::OWL_TRANSITIVE_PROPERTY),
        ));

        // A is ancestor of B, B is ancestor of C
        store.add(Triple::new(
            Term::uri("http://ex.org/a"),
            Term::uri("http://ex.org/ancestor"),
            Term::uri("http://ex.org/b"),
        ));
        store.add(Triple::new(
            Term::uri("http://ex.org/b"),
            Term::uri("http://ex.org/ancestor"),
            Term::uri("http://ex.org/c"),
        ));

        reasoner.add_rule(Owl2RlRules::prp_trp());
        reasoner.run(&mut store);

        // A should be ancestor of C
        let expected = Triple::new(
            Term::uri("http://ex.org/a"),
            Term::uri("http://ex.org/ancestor"),
            Term::uri("http://ex.org/c"),
        );
        assert!(store.iter().any(|t| t == &expected), "Transitive inference failed");
    }

    #[test]
    fn test_inverse_property() {
        let mut store = Store::new();
        let mut reasoner = Reasoner::new();

        // :hasChild is inverse of :hasParent
        store.add(Triple::new(
            Term::uri("http://ex.org/hasChild"),
            Term::uri(ns::OWL_INVERSE_OF),
            Term::uri("http://ex.org/hasParent"),
        ));

        // Alice hasChild Bob
        store.add(Triple::new(
            Term::uri("http://ex.org/alice"),
            Term::uri("http://ex.org/hasChild"),
            Term::uri("http://ex.org/bob"),
        ));

        reasoner.add_rule(Owl2RlRules::prp_inv1());
        reasoner.run(&mut store);

        // Bob hasParent Alice
        let expected = Triple::new(
            Term::uri("http://ex.org/bob"),
            Term::uri("http://ex.org/hasParent"),
            Term::uri("http://ex.org/alice"),
        );
        assert!(store.iter().any(|t| t == &expected), "Inverse property inference failed");
    }

    #[test]
    fn test_equivalent_class() {
        let mut store = Store::new();
        let mut reasoner = Reasoner::new();

        // :Person equivalent to :Human
        store.add(Triple::new(
            Term::uri("http://ex.org/Person"),
            Term::uri(ns::OWL_EQUIVALENT_CLASS),
            Term::uri("http://ex.org/Human"),
        ));

        // Alice is a Person
        store.add(Triple::new(
            Term::uri("http://ex.org/alice"),
            Term::uri(ns::RDF_TYPE),
            Term::uri("http://ex.org/Person"),
        ));

        reasoner.add_rule(Owl2RlRules::cax_eqc1());
        reasoner.run(&mut store);

        // Alice should be a Human
        let expected = Triple::new(
            Term::uri("http://ex.org/alice"),
            Term::uri(ns::RDF_TYPE),
            Term::uri("http://ex.org/Human"),
        );
        assert!(store.iter().any(|t| t == &expected), "Equivalent class inference failed");
    }

    #[test]
    fn test_same_as_symmetry() {
        let mut store = Store::new();
        let mut reasoner = Reasoner::new();

        // :alice sameAs :aliceSmith
        store.add(Triple::new(
            Term::uri("http://ex.org/alice"),
            Term::uri(ns::OWL_SAME_AS),
            Term::uri("http://ex.org/aliceSmith"),
        ));

        reasoner.add_rule(Owl2RlRules::eq_sym());
        reasoner.run(&mut store);

        // :aliceSmith should sameAs :alice
        let expected = Triple::new(
            Term::uri("http://ex.org/aliceSmith"),
            Term::uri(ns::OWL_SAME_AS),
            Term::uri("http://ex.org/alice"),
        );
        assert!(store.iter().any(|t| t == &expected), "sameAs symmetry failed");
    }

    #[test]
    fn test_same_as_transitivity() {
        let mut store = Store::new();
        let mut reasoner = Reasoner::new();

        // :a sameAs :b, :b sameAs :c
        store.add(Triple::new(
            Term::uri("http://ex.org/a"),
            Term::uri(ns::OWL_SAME_AS),
            Term::uri("http://ex.org/b"),
        ));
        store.add(Triple::new(
            Term::uri("http://ex.org/b"),
            Term::uri(ns::OWL_SAME_AS),
            Term::uri("http://ex.org/c"),
        ));

        reasoner.add_rule(Owl2RlRules::eq_trans());
        reasoner.run(&mut store);

        // :a should sameAs :c
        let expected = Triple::new(
            Term::uri("http://ex.org/a"),
            Term::uri(ns::OWL_SAME_AS),
            Term::uri("http://ex.org/c"),
        );
        assert!(store.iter().any(|t| t == &expected), "sameAs transitivity failed");
    }

    #[test]
    fn test_has_value_restriction() {
        let mut store = Store::new();
        let mut reasoner = Reasoner::new();

        // Define restriction: hasValue "red" on :color
        store.add(Triple::new(
            Term::uri("http://ex.org/RedThing"),
            Term::uri(ns::OWL_HAS_VALUE),
            Term::literal("red"),
        ));
        store.add(Triple::new(
            Term::uri("http://ex.org/RedThing"),
            Term::uri(ns::OWL_ON_PROPERTY),
            Term::uri("http://ex.org/color"),
        ));

        // Apple has color "red"
        store.add(Triple::new(
            Term::uri("http://ex.org/apple"),
            Term::uri("http://ex.org/color"),
            Term::literal("red"),
        ));

        reasoner.add_rule(Owl2RlRules::cls_hv2());
        reasoner.run(&mut store);

        // Apple should be a RedThing
        let expected = Triple::new(
            Term::uri("http://ex.org/apple"),
            Term::uri(ns::RDF_TYPE),
            Term::uri("http://ex.org/RedThing"),
        );
        assert!(store.iter().any(|t| t == &expected), "hasValue restriction inference failed");
    }

    #[test]
    fn test_all_values_from() {
        let mut store = Store::new();
        let mut reasoner = Reasoner::new();

        // Define restriction: Parent has allValuesFrom Person on :hasChild
        store.add(Triple::new(
            Term::uri("http://ex.org/Parent"),
            Term::uri(ns::OWL_ALL_VALUES_FROM),
            Term::uri("http://ex.org/Person"),
        ));
        store.add(Triple::new(
            Term::uri("http://ex.org/Parent"),
            Term::uri(ns::OWL_ON_PROPERTY),
            Term::uri("http://ex.org/hasChild"),
        ));

        // Alice is a Parent and hasChild Bob
        store.add(Triple::new(
            Term::uri("http://ex.org/alice"),
            Term::uri(ns::RDF_TYPE),
            Term::uri("http://ex.org/Parent"),
        ));
        store.add(Triple::new(
            Term::uri("http://ex.org/alice"),
            Term::uri("http://ex.org/hasChild"),
            Term::uri("http://ex.org/bob"),
        ));

        reasoner.add_rule(Owl2RlRules::cls_avf());
        reasoner.run(&mut store);

        // Bob should be a Person
        let expected = Triple::new(
            Term::uri("http://ex.org/bob"),
            Term::uri(ns::RDF_TYPE),
            Term::uri("http://ex.org/Person"),
        );
        assert!(store.iter().any(|t| t == &expected), "allValuesFrom inference failed");
    }
}
