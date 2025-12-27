//! Literal value representation

use std::fmt;

/// Datatype for a literal
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Datatype {
    /// Plain literal (no datatype)
    Plain,
    /// Language-tagged literal
    Language(String),
    /// Typed literal with datatype URI
    Typed(String),
}

/// An RDF literal value
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Literal {
    value: String,
    datatype: Datatype,
}

impl Literal {
    /// Create a plain literal
    pub fn plain(value: String) -> Self {
        Literal {
            value,
            datatype: Datatype::Plain,
        }
    }

    /// Create a typed literal
    pub fn typed(value: String, datatype: String) -> Self {
        Literal {
            value,
            datatype: Datatype::Typed(datatype),
        }
    }

    /// Create a language-tagged literal
    pub fn with_language(value: String, lang: String) -> Self {
        Literal {
            value,
            datatype: Datatype::Language(lang.to_lowercase()),
        }
    }

    /// Get the lexical value
    pub fn value(&self) -> &str {
        &self.value
    }

    /// Get the datatype
    pub fn datatype(&self) -> &Datatype {
        &self.datatype
    }

    /// Check if this is a plain literal
    pub fn is_plain(&self) -> bool {
        matches!(self.datatype, Datatype::Plain)
    }

    /// Get the language tag if present
    pub fn language(&self) -> Option<&str> {
        match &self.datatype {
            Datatype::Language(lang) => Some(lang),
            _ => None,
        }
    }

    /// Get the datatype URI if present
    pub fn datatype_uri(&self) -> Option<&str> {
        match &self.datatype {
            Datatype::Typed(uri) => Some(uri),
            _ => None,
        }
    }

    /// Try to parse as an integer
    pub fn as_integer(&self) -> Option<i64> {
        self.value.parse().ok()
    }

    /// Try to parse as a float
    pub fn as_float(&self) -> Option<f64> {
        self.value.parse().ok()
    }

    /// Try to parse as a boolean
    pub fn as_boolean(&self) -> Option<bool> {
        match self.value.as_str() {
            "true" | "1" => Some(true),
            "false" | "0" => Some(false),
            _ => None,
        }
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.datatype {
            Datatype::Plain => write!(f, "\"{}\"", self.value),
            Datatype::Language(lang) => write!(f, "\"{}\"@{}", self.value, lang),
            Datatype::Typed(dt) => write!(f, "\"{}\"^^<{}>", self.value, dt),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.datatype {
            Datatype::Plain => write!(f, "\"{}\"", self.value),
            Datatype::Language(lang) => write!(f, "\"{}\"@{}", self.value, lang),
            Datatype::Typed(dt) => write!(f, "\"{}\"^^<{}>", self.value, dt),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plain_literal() {
        let lit = Literal::plain("hello".into());
        assert_eq!(lit.value(), "hello");
        assert!(lit.is_plain());
        assert_eq!(format!("{}", lit), "\"hello\"");
    }

    #[test]
    fn test_typed_literal() {
        let lit = Literal::typed("42".into(), "http://www.w3.org/2001/XMLSchema#integer".into());
        assert_eq!(lit.as_integer(), Some(42));
        assert!(!lit.is_plain());
    }

    #[test]
    fn test_lang_literal() {
        let lit = Literal::with_language("hello".into(), "en".into());
        assert_eq!(lit.language(), Some("en"));
        assert_eq!(format!("{}", lit), "\"hello\"@en");
    }
}
