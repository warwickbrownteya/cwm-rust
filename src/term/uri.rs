//! URI (IRI) representation

use std::fmt;

/// A URI reference
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Uri {
    value: String,
}

impl Uri {
    /// Create a new URI
    pub fn new(value: String) -> Self {
        Uri { value }
    }

    /// Get the URI as a string slice
    pub fn as_str(&self) -> &str {
        &self.value
    }

    /// Get the namespace (everything up to and including the last # or /)
    pub fn namespace(&self) -> &str {
        if let Some(pos) = self.value.rfind('#') {
            &self.value[..=pos]
        } else if let Some(pos) = self.value.rfind('/') {
            &self.value[..=pos]
        } else {
            &self.value
        }
    }

    /// Get the local name (fragment or last path segment)
    pub fn local_name(&self) -> &str {
        if let Some(pos) = self.value.rfind('#') {
            &self.value[pos + 1..]
        } else if let Some(pos) = self.value.rfind('/') {
            &self.value[pos + 1..]
        } else {
            &self.value
        }
    }

    /// Resolve a relative URI against this base
    pub fn resolve(&self, relative: &str) -> Uri {
        if relative.starts_with("http://") || relative.starts_with("https://") || relative.starts_with("file://") {
            return Uri::new(relative.to_string());
        }

        if relative.starts_with('#') {
            // Fragment-only reference
            let base = if let Some(pos) = self.value.find('#') {
                &self.value[..pos]
            } else {
                &self.value
            };
            return Uri::new(format!("{}{}", base, relative));
        }

        if relative.starts_with('/') {
            // Absolute path
            if let Some(scheme_end) = self.value.find("://") {
                let authority_start = scheme_end + 3;
                if let Some(path_start) = self.value[authority_start..].find('/') {
                    let base = &self.value[..authority_start + path_start];
                    return Uri::new(format!("{}{}", base, relative));
                }
            }
        }

        // Relative path - resolve against base
        let base = self.namespace();
        Uri::new(format!("{}{}", base, relative))
    }
}

impl fmt::Debug for Uri {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}>", self.value)
    }
}

impl fmt::Display for Uri {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}>", self.value)
    }
}

impl From<&str> for Uri {
    fn from(s: &str) -> Self {
        Uri::new(s.to_string())
    }
}

impl From<String> for Uri {
    fn from(s: String) -> Self {
        Uri::new(s)
    }
}

/// Well-known namespace URIs
pub mod ns {
    use super::Uri;

    pub const RDF: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    pub const RDFS: &str = "http://www.w3.org/2000/01/rdf-schema#";
    pub const XSD: &str = "http://www.w3.org/2001/XMLSchema#";
    pub const OWL: &str = "http://www.w3.org/2002/07/owl#";
    pub const LOG: &str = "http://www.w3.org/2000/10/swap/log#";
    pub const MATH: &str = "http://www.w3.org/2000/10/swap/math#";
    pub const STRING: &str = "http://www.w3.org/2000/10/swap/string#";
    pub const LIST: &str = "http://www.w3.org/2000/10/swap/list#";
    pub const CRYPTO: &str = "http://www.w3.org/2000/10/swap/crypto#";
    pub const TIME: &str = "http://www.w3.org/2000/10/swap/time#";
    pub const OS: &str = "http://www.w3.org/2000/10/swap/os#";
    pub const GRAPH: &str = "http://www.w3.org/2000/10/swap/graph#";
    pub const DB: &str = "http://www.w3.org/2000/10/swap/db#";

    // Common RDF terms
    pub fn rdf_type() -> Uri { Uri::new(format!("{}type", RDF)) }
    pub fn rdf_first() -> Uri { Uri::new(format!("{}first", RDF)) }
    pub fn rdf_rest() -> Uri { Uri::new(format!("{}rest", RDF)) }
    pub fn rdf_nil() -> Uri { Uri::new(format!("{}nil", RDF)) }

    // N3 logic terms
    pub fn log_implies() -> Uri { Uri::new(format!("{}implies", LOG)) }
    pub fn log_forall() -> Uri { Uri::new(format!("{}forAll", LOG)) }
    pub fn log_forsome() -> Uri { Uri::new(format!("{}forSome", LOG)) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_namespace() {
        let uri = Uri::new("http://example.org/foo#bar".into());
        assert_eq!(uri.namespace(), "http://example.org/foo#");
        assert_eq!(uri.local_name(), "bar");

        let uri2 = Uri::new("http://example.org/path/name".into());
        assert_eq!(uri2.namespace(), "http://example.org/path/");
        assert_eq!(uri2.local_name(), "name");
    }

    #[test]
    fn test_resolve() {
        let base = Uri::new("http://example.org/base/doc".into());

        assert_eq!(base.resolve("#frag").as_str(), "http://example.org/base/doc#frag");
        assert_eq!(base.resolve("other").as_str(), "http://example.org/base/other");
        assert_eq!(base.resolve("http://other.org/").as_str(), "http://other.org/");
    }
}
