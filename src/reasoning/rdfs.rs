//! RDFS Entailment Rules
//!
//! This module implements the standard RDFS entailment rules as defined in the
//! W3C RDF Semantics specification (https://www.w3.org/TR/rdf11-mt/).
//!
//! # Entailment Rules
//!
//! The following RDFS entailment patterns are implemented:
//!
//! | Rule   | Pattern                                           | Inference                      |
//! |--------|---------------------------------------------------|--------------------------------|
//! | rdfs2  | `P rdfs:domain C`, `S P O`                        | `S rdf:type C`                 |
//! | rdfs3  | `P rdfs:range C`, `S P O`                         | `O rdf:type C`                 |
//! | rdfs5  | `P rdfs:subPropertyOf Q`, `Q rdfs:subPropertyOf R`| `P rdfs:subPropertyOf R`       |
//! | rdfs7  | `P rdfs:subPropertyOf Q`, `S P O`                 | `S Q O`                        |
//! | rdfs9  | `C rdfs:subClassOf D`, `X rdf:type C`             | `X rdf:type D`                 |
//! | rdfs11 | `C rdfs:subClassOf D`, `D rdfs:subClassOf E`      | `C rdfs:subClassOf E`          |
//!
//! # Usage
//!
//! ```ignore
//! use cwm::reasoning::rdfs::RdfsRules;
//! use cwm::Reasoner;
//!
//! let mut reasoner = Reasoner::new();
//! reasoner.add_rules(RdfsRules::all());
//! reasoner.run(&mut store);
//! ```

use crate::term::{Term, Triple};
use crate::reasoner::Rule;

/// Standard RDF and RDFS namespace URIs
pub mod ns {
    pub const RDF: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    pub const RDFS: &str = "http://www.w3.org/2000/01/rdf-schema#";

    // RDF terms
    pub const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
    pub const RDF_PROPERTY: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property";
    pub const RDF_FIRST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
    pub const RDF_REST: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
    pub const RDF_NIL: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";

    // RDFS terms
    pub const RDFS_SUBCLASS_OF: &str = "http://www.w3.org/2000/01/rdf-schema#subClassOf";
    pub const RDFS_SUBPROPERTY_OF: &str = "http://www.w3.org/2000/01/rdf-schema#subPropertyOf";
    pub const RDFS_DOMAIN: &str = "http://www.w3.org/2000/01/rdf-schema#domain";
    pub const RDFS_RANGE: &str = "http://www.w3.org/2000/01/rdf-schema#range";
    pub const RDFS_CLASS: &str = "http://www.w3.org/2000/01/rdf-schema#Class";
    pub const RDFS_RESOURCE: &str = "http://www.w3.org/2000/01/rdf-schema#Resource";
    pub const RDFS_LITERAL: &str = "http://www.w3.org/2000/01/rdf-schema#Literal";
    pub const RDFS_DATATYPE: &str = "http://www.w3.org/2000/01/rdf-schema#Datatype";
    pub const RDFS_MEMBER: &str = "http://www.w3.org/2000/01/rdf-schema#member";
    pub const RDFS_CONTAINER: &str = "http://www.w3.org/2000/01/rdf-schema#Container";
    pub const RDFS_CONTAINER_MEMBERSHIP_PROPERTY: &str = "http://www.w3.org/2000/01/rdf-schema#ContainerMembershipProperty";
}

/// RDFS entailment rules generator
pub struct RdfsRules;

impl RdfsRules {
    /// Create all RDFS entailment rules
    ///
    /// Returns a vector of rules implementing the core RDFS entailment patterns.
    pub fn all() -> Vec<Rule> {
        vec![
            Self::rdfs2_domain(),
            Self::rdfs3_range(),
            Self::rdfs5_subproperty_transitivity(),
            Self::rdfs7_subproperty_inference(),
            Self::rdfs9_subclass_type_inference(),
            Self::rdfs11_subclass_transitivity(),
        ]
    }

    /// Get only the essential rules (most commonly needed)
    ///
    /// Returns subclass/subproperty transitivity and type inference rules.
    pub fn essential() -> Vec<Rule> {
        vec![
            Self::rdfs9_subclass_type_inference(),
            Self::rdfs11_subclass_transitivity(),
            Self::rdfs7_subproperty_inference(),
            Self::rdfs5_subproperty_transitivity(),
        ]
    }

    /// Get only transitivity rules
    ///
    /// Returns just the transitivity rules for subClassOf and subPropertyOf.
    pub fn transitivity() -> Vec<Rule> {
        vec![
            Self::rdfs11_subclass_transitivity(),
            Self::rdfs5_subproperty_transitivity(),
        ]
    }

    /// rdfs2: Domain inference
    ///
    /// If property P has domain D, and subject S has property P,
    /// then S is of type D.
    ///
    /// ```text
    /// { ?p rdfs:domain ?d . ?s ?p ?o } => { ?s a ?d }
    /// ```
    pub fn rdfs2_domain() -> Rule {
        Rule::named(
            "rdfs2-domain",
            vec![
                Triple::new(
                    Term::universal("p"),
                    Term::uri(ns::RDFS_DOMAIN),
                    Term::universal("d"),
                ),
                Triple::new(
                    Term::universal("s"),
                    Term::universal("p"),
                    Term::universal("o"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("s"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("d"),
                ),
            ],
        )
    }

    /// rdfs3: Range inference
    ///
    /// If property P has range R, and subject S has property P with object O,
    /// then O is of type R.
    ///
    /// ```text
    /// { ?p rdfs:range ?r . ?s ?p ?o } => { ?o a ?r }
    /// ```
    pub fn rdfs3_range() -> Rule {
        Rule::named(
            "rdfs3-range",
            vec![
                Triple::new(
                    Term::universal("p"),
                    Term::uri(ns::RDFS_RANGE),
                    Term::universal("r"),
                ),
                Triple::new(
                    Term::universal("s"),
                    Term::universal("p"),
                    Term::universal("o"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("o"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("r"),
                ),
            ],
        )
    }

    /// rdfs5: SubPropertyOf transitivity
    ///
    /// If P is a subproperty of Q, and Q is a subproperty of R,
    /// then P is a subproperty of R.
    ///
    /// ```text
    /// { ?p rdfs:subPropertyOf ?q . ?q rdfs:subPropertyOf ?r } => { ?p rdfs:subPropertyOf ?r }
    /// ```
    pub fn rdfs5_subproperty_transitivity() -> Rule {
        Rule::named(
            "rdfs5-subPropertyOf-trans",
            vec![
                Triple::new(
                    Term::universal("p"),
                    Term::uri(ns::RDFS_SUBPROPERTY_OF),
                    Term::universal("q"),
                ),
                Triple::new(
                    Term::universal("q"),
                    Term::uri(ns::RDFS_SUBPROPERTY_OF),
                    Term::universal("r"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("p"),
                    Term::uri(ns::RDFS_SUBPROPERTY_OF),
                    Term::universal("r"),
                ),
            ],
        )
    }

    /// rdfs7: SubPropertyOf inference
    ///
    /// If P is a subproperty of Q, and S has property P with value O,
    /// then S also has property Q with value O.
    ///
    /// ```text
    /// { ?p rdfs:subPropertyOf ?q . ?s ?p ?o } => { ?s ?q ?o }
    /// ```
    pub fn rdfs7_subproperty_inference() -> Rule {
        Rule::named(
            "rdfs7-subPropertyOf-infer",
            vec![
                Triple::new(
                    Term::universal("p"),
                    Term::uri(ns::RDFS_SUBPROPERTY_OF),
                    Term::universal("q"),
                ),
                Triple::new(
                    Term::universal("s"),
                    Term::universal("p"),
                    Term::universal("o"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("s"),
                    Term::universal("q"),
                    Term::universal("o"),
                ),
            ],
        )
    }

    /// rdfs9: SubClassOf type inference
    ///
    /// If class C is a subclass of D, and X is of type C,
    /// then X is also of type D.
    ///
    /// ```text
    /// { ?c rdfs:subClassOf ?d . ?x a ?c } => { ?x a ?d }
    /// ```
    pub fn rdfs9_subclass_type_inference() -> Rule {
        Rule::named(
            "rdfs9-subClassOf-type",
            vec![
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::RDFS_SUBCLASS_OF),
                    Term::universal("d"),
                ),
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("c"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("x"),
                    Term::uri(ns::RDF_TYPE),
                    Term::universal("d"),
                ),
            ],
        )
    }

    /// rdfs11: SubClassOf transitivity
    ///
    /// If C is a subclass of D, and D is a subclass of E,
    /// then C is a subclass of E.
    ///
    /// ```text
    /// { ?c rdfs:subClassOf ?d . ?d rdfs:subClassOf ?e } => { ?c rdfs:subClassOf ?e }
    /// ```
    pub fn rdfs11_subclass_transitivity() -> Rule {
        Rule::named(
            "rdfs11-subClassOf-trans",
            vec![
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::RDFS_SUBCLASS_OF),
                    Term::universal("d"),
                ),
                Triple::new(
                    Term::universal("d"),
                    Term::uri(ns::RDFS_SUBCLASS_OF),
                    Term::universal("e"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::RDFS_SUBCLASS_OF),
                    Term::universal("e"),
                ),
            ],
        )
    }

    // Additional RDFS rules (less commonly needed but included for completeness)

    /// rdfs4a: Everything has type rdfs:Resource (subject)
    ///
    /// ```text
    /// { ?s ?p ?o } => { ?s a rdfs:Resource }
    /// ```
    #[allow(dead_code)]
    pub fn rdfs4a_resource_subject() -> Rule {
        Rule::named(
            "rdfs4a-resource-s",
            vec![
                Triple::new(
                    Term::universal("s"),
                    Term::universal("p"),
                    Term::universal("o"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("s"),
                    Term::uri(ns::RDF_TYPE),
                    Term::uri(ns::RDFS_RESOURCE),
                ),
            ],
        )
    }

    /// rdfs4b: Everything has type rdfs:Resource (object, for URI/blank nodes)
    ///
    /// Note: This should only apply to non-literal objects.
    /// ```text
    /// { ?s ?p ?o } => { ?o a rdfs:Resource }
    /// ```
    #[allow(dead_code)]
    pub fn rdfs4b_resource_object() -> Rule {
        Rule::named(
            "rdfs4b-resource-o",
            vec![
                Triple::new(
                    Term::universal("s"),
                    Term::universal("p"),
                    Term::universal("o"),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("o"),
                    Term::uri(ns::RDF_TYPE),
                    Term::uri(ns::RDFS_RESOURCE),
                ),
            ],
        )
    }

    /// rdfs6: Every property is a subproperty of itself (reflexive)
    ///
    /// ```text
    /// { ?p a rdf:Property } => { ?p rdfs:subPropertyOf ?p }
    /// ```
    #[allow(dead_code)]
    pub fn rdfs6_subproperty_reflexive() -> Rule {
        Rule::named(
            "rdfs6-subPropertyOf-refl",
            vec![
                Triple::new(
                    Term::universal("p"),
                    Term::uri(ns::RDF_TYPE),
                    Term::uri(ns::RDF_PROPERTY),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("p"),
                    Term::uri(ns::RDFS_SUBPROPERTY_OF),
                    Term::universal("p"),
                ),
            ],
        )
    }

    /// rdfs8: Every class is a subclass of rdfs:Resource
    ///
    /// ```text
    /// { ?c a rdfs:Class } => { ?c rdfs:subClassOf rdfs:Resource }
    /// ```
    #[allow(dead_code)]
    pub fn rdfs8_class_subclass_resource() -> Rule {
        Rule::named(
            "rdfs8-class-resource",
            vec![
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::RDF_TYPE),
                    Term::uri(ns::RDFS_CLASS),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::RDFS_SUBCLASS_OF),
                    Term::uri(ns::RDFS_RESOURCE),
                ),
            ],
        )
    }

    /// rdfs10: Every class is a subclass of itself (reflexive)
    ///
    /// ```text
    /// { ?c a rdfs:Class } => { ?c rdfs:subClassOf ?c }
    /// ```
    #[allow(dead_code)]
    pub fn rdfs10_subclass_reflexive() -> Rule {
        Rule::named(
            "rdfs10-subClassOf-refl",
            vec![
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::RDF_TYPE),
                    Term::uri(ns::RDFS_CLASS),
                ),
            ],
            vec![
                Triple::new(
                    Term::universal("c"),
                    Term::uri(ns::RDFS_SUBCLASS_OF),
                    Term::universal("c"),
                ),
            ],
        )
    }

    /// Get extended rules including axioms
    ///
    /// Returns all rules plus the less commonly needed axiom rules.
    /// Warning: These can generate a large number of triples.
    pub fn extended() -> Vec<Rule> {
        vec![
            // Core rules
            Self::rdfs2_domain(),
            Self::rdfs3_range(),
            Self::rdfs5_subproperty_transitivity(),
            Self::rdfs7_subproperty_inference(),
            Self::rdfs9_subclass_type_inference(),
            Self::rdfs11_subclass_transitivity(),
            // Reflexivity rules
            Self::rdfs6_subproperty_reflexive(),
            Self::rdfs10_subclass_reflexive(),
            // Resource/Class axioms
            Self::rdfs8_class_subclass_resource(),
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Store, Reasoner};
    use crate::core::TripleStore;

    #[test]
    fn test_subclass_transitivity() {
        let mut store = Store::new();

        // Animal > Mammal > Dog
        store.add(Triple::new(
            Term::uri("http://example.org/Dog"),
            Term::uri(ns::RDFS_SUBCLASS_OF),
            Term::uri("http://example.org/Mammal"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/Mammal"),
            Term::uri(ns::RDFS_SUBCLASS_OF),
            Term::uri("http://example.org/Animal"),
        ));

        let mut reasoner = Reasoner::new();
        reasoner.add_rules(RdfsRules::transitivity());
        reasoner.run(&mut store);

        // Dog should be subclass of Animal
        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/Dog"),
            Term::uri(ns::RDFS_SUBCLASS_OF),
            Term::uri("http://example.org/Animal"),
        )));
    }

    #[test]
    fn test_subclass_type_inference() {
        let mut store = Store::new();

        // Dog subClassOf Animal
        store.add(Triple::new(
            Term::uri("http://example.org/Dog"),
            Term::uri(ns::RDFS_SUBCLASS_OF),
            Term::uri("http://example.org/Animal"),
        ));
        // fido is a Dog
        store.add(Triple::new(
            Term::uri("http://example.org/fido"),
            Term::uri(ns::RDF_TYPE),
            Term::uri("http://example.org/Dog"),
        ));

        let mut reasoner = Reasoner::new();
        reasoner.add_rules(RdfsRules::essential());
        reasoner.run(&mut store);

        // fido should be an Animal
        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/fido"),
            Term::uri(ns::RDF_TYPE),
            Term::uri("http://example.org/Animal"),
        )));
    }

    #[test]
    fn test_domain_inference() {
        let mut store = Store::new();

        // hasFather has domain Person
        store.add(Triple::new(
            Term::uri("http://example.org/hasFather"),
            Term::uri(ns::RDFS_DOMAIN),
            Term::uri("http://example.org/Person"),
        ));
        // john hasFather bob
        store.add(Triple::new(
            Term::uri("http://example.org/john"),
            Term::uri("http://example.org/hasFather"),
            Term::uri("http://example.org/bob"),
        ));

        let mut reasoner = Reasoner::new();
        reasoner.add_rule(RdfsRules::rdfs2_domain());
        reasoner.run(&mut store);

        // john should be a Person
        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/john"),
            Term::uri(ns::RDF_TYPE),
            Term::uri("http://example.org/Person"),
        )));
    }

    #[test]
    fn test_range_inference() {
        let mut store = Store::new();

        // hasFather has range Person
        store.add(Triple::new(
            Term::uri("http://example.org/hasFather"),
            Term::uri(ns::RDFS_RANGE),
            Term::uri("http://example.org/Person"),
        ));
        // john hasFather bob
        store.add(Triple::new(
            Term::uri("http://example.org/john"),
            Term::uri("http://example.org/hasFather"),
            Term::uri("http://example.org/bob"),
        ));

        let mut reasoner = Reasoner::new();
        reasoner.add_rule(RdfsRules::rdfs3_range());
        reasoner.run(&mut store);

        // bob should be a Person
        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/bob"),
            Term::uri(ns::RDF_TYPE),
            Term::uri("http://example.org/Person"),
        )));
    }

    #[test]
    fn test_subproperty_inference() {
        let mut store = Store::new();

        // hasFather subPropertyOf hasParent
        store.add(Triple::new(
            Term::uri("http://example.org/hasFather"),
            Term::uri(ns::RDFS_SUBPROPERTY_OF),
            Term::uri("http://example.org/hasParent"),
        ));
        // john hasFather bob
        store.add(Triple::new(
            Term::uri("http://example.org/john"),
            Term::uri("http://example.org/hasFather"),
            Term::uri("http://example.org/bob"),
        ));

        let mut reasoner = Reasoner::new();
        reasoner.add_rule(RdfsRules::rdfs7_subproperty_inference());
        reasoner.run(&mut store);

        // john should hasParent bob
        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/john"),
            Term::uri("http://example.org/hasParent"),
            Term::uri("http://example.org/bob"),
        )));
    }

    #[test]
    fn test_combined_rdfs_reasoning() {
        let mut store = Store::new();

        // Schema: Dog < Mammal < Animal
        store.add(Triple::new(
            Term::uri("http://example.org/Dog"),
            Term::uri(ns::RDFS_SUBCLASS_OF),
            Term::uri("http://example.org/Mammal"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/Mammal"),
            Term::uri(ns::RDFS_SUBCLASS_OF),
            Term::uri("http://example.org/Animal"),
        ));
        // hasOwner domain Person, range Animal
        store.add(Triple::new(
            Term::uri("http://example.org/hasOwner"),
            Term::uri(ns::RDFS_DOMAIN),
            Term::uri("http://example.org/Animal"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/hasOwner"),
            Term::uri(ns::RDFS_RANGE),
            Term::uri("http://example.org/Person"),
        ));
        // Data: fido is a Dog, fido hasOwner john
        store.add(Triple::new(
            Term::uri("http://example.org/fido"),
            Term::uri(ns::RDF_TYPE),
            Term::uri("http://example.org/Dog"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/fido"),
            Term::uri("http://example.org/hasOwner"),
            Term::uri("http://example.org/john"),
        ));

        let initial_count = store.len();

        let mut reasoner = Reasoner::new();
        reasoner.add_rules(RdfsRules::all());
        reasoner.run(&mut store);

        // fido should be a Mammal and Animal
        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/fido"),
            Term::uri(ns::RDF_TYPE),
            Term::uri("http://example.org/Mammal"),
        )));
        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/fido"),
            Term::uri(ns::RDF_TYPE),
            Term::uri("http://example.org/Animal"),
        )));

        // fido should be inferred as Animal from domain
        // john should be inferred as Person from range
        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/john"),
            Term::uri(ns::RDF_TYPE),
            Term::uri("http://example.org/Person"),
        )));

        // Dog should be subClassOf Animal (transitive)
        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/Dog"),
            Term::uri(ns::RDFS_SUBCLASS_OF),
            Term::uri("http://example.org/Animal"),
        )));

        // Should have derived new triples
        assert!(store.len() > initial_count);
    }

    #[test]
    fn test_all_rules_generated() {
        let rules = RdfsRules::all();
        assert_eq!(rules.len(), 6);

        let essential = RdfsRules::essential();
        assert_eq!(essential.len(), 4);

        let transitivity = RdfsRules::transitivity();
        assert_eq!(transitivity.len(), 2);

        let extended = RdfsRules::extended();
        assert_eq!(extended.len(), 9);
    }
}
