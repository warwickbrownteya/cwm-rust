//! Standard RDF and N3 namespace URI constants
//!
//! These constants provide compile-time verified namespace URIs for
//! use throughout the codebase, eliminating string typos and enabling
//! better refactoring support.

/// Standard namespace URI constants
pub mod ns {
    // W3C Core Vocabularies
    /// RDF namespace
    pub const RDF: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    /// RDF Schema namespace
    pub const RDFS: &str = "http://www.w3.org/2000/01/rdf-schema#";
    /// XML Schema Datatypes namespace
    pub const XSD: &str = "http://www.w3.org/2001/XMLSchema#";
    /// OWL Web Ontology Language namespace
    pub const OWL: &str = "http://www.w3.org/2002/07/owl#";

    // N3/SWAP Builtins
    /// N3 Logic namespace (log:)
    pub const LOG: &str = "http://www.w3.org/2000/10/swap/log#";
    /// N3 Math namespace (math:)
    pub const MATH: &str = "http://www.w3.org/2000/10/swap/math#";
    /// N3 String namespace (string:)
    pub const STRING: &str = "http://www.w3.org/2000/10/swap/string#";
    /// N3 List namespace (list:)
    pub const LIST: &str = "http://www.w3.org/2000/10/swap/list#";
    /// N3 Time namespace (time:)
    pub const TIME: &str = "http://www.w3.org/2000/10/swap/time#";
    /// N3 Crypto namespace (crypto:)
    pub const CRYPTO: &str = "http://www.w3.org/2000/10/swap/crypto#";
    /// N3 OS namespace (os:)
    pub const OS: &str = "http://www.w3.org/2000/10/swap/os#";
    /// N3 Graph namespace (graph:)
    pub const GRAPH: &str = "http://www.w3.org/2000/10/swap/graph#";

    // Proof and Reasoning
    /// SWAP Reason vocabulary (proof generation)
    pub const REASON: &str = "http://www.w3.org/2000/10/swap/reason#";

    // Common External Vocabularies
    /// Dublin Core Terms
    pub const DC: &str = "http://purl.org/dc/terms/";
    /// FOAF (Friend of a Friend)
    pub const FOAF: &str = "http://xmlns.com/foaf/0.1/";
    /// SKOS (Simple Knowledge Organization System)
    pub const SKOS: &str = "http://www.w3.org/2004/02/skos/core#";
    /// SHACL (Shapes Constraint Language)
    pub const SHACL: &str = "http://www.w3.org/ns/shacl#";

    /// Helper to construct a full URI from namespace and local name
    #[inline]
    pub fn uri(namespace: &str, local: &str) -> String {
        format!("{}{}", namespace, local)
    }

    /// Check if a URI belongs to a namespace
    #[inline]
    pub fn in_namespace(uri: &str, namespace: &str) -> bool {
        uri.starts_with(namespace)
    }

    /// Extract local name from a URI given its namespace
    #[inline]
    pub fn local_name<'a>(uri: &'a str, namespace: &str) -> Option<&'a str> {
        uri.strip_prefix(namespace)
    }
}

#[cfg(test)]
mod tests {
    use super::ns;

    #[test]
    fn test_uri_construction() {
        assert_eq!(
            ns::uri(ns::MATH, "sum"),
            "http://www.w3.org/2000/10/swap/math#sum"
        );
        assert_eq!(
            ns::uri(ns::RDF, "type"),
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
        );
    }

    #[test]
    fn test_namespace_check() {
        assert!(ns::in_namespace(
            "http://www.w3.org/2000/10/swap/math#sum",
            ns::MATH
        ));
        assert!(!ns::in_namespace(
            "http://www.w3.org/2000/10/swap/string#concat",
            ns::MATH
        ));
    }

    #[test]
    fn test_local_name() {
        assert_eq!(
            ns::local_name("http://www.w3.org/2000/10/swap/math#sum", ns::MATH),
            Some("sum")
        );
        assert_eq!(
            ns::local_name("http://www.w3.org/2000/10/swap/math#sum", ns::STRING),
            None
        );
    }
}
