//! Variable representation for N3 patterns and rules

use std::fmt;

/// Quantification type for a variable
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Quantification {
    /// Universal quantification (forAll)
    Universal,
    /// Existential quantification (forSome)
    Existential,
}

/// A variable in N3
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    name: String,
    quantification: Quantification,
}

impl Variable {
    /// Create a universally quantified variable
    pub fn universal(name: String) -> Self {
        Variable {
            name,
            quantification: Quantification::Universal,
        }
    }

    /// Create an existentially quantified variable
    pub fn existential(name: String) -> Self {
        Variable {
            name,
            quantification: Quantification::Existential,
        }
    }

    /// Get the variable name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get the quantification type
    pub fn quantification(&self) -> Quantification {
        self.quantification
    }

    /// Check if this is a universal variable
    pub fn is_universal(&self) -> bool {
        self.quantification == Quantification::Universal
    }

    /// Check if this is an existential variable
    pub fn is_existential(&self) -> bool {
        self.quantification == Quantification::Existential
    }
}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.quantification {
            Quantification::Universal => write!(f, "?{}", self.name),
            Quantification::Existential => write!(f, "_:{}", self.name),
        }
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.quantification {
            Quantification::Universal => write!(f, "?{}", self.name),
            Quantification::Existential => write!(f, "_:{}", self.name),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_universal_variable() {
        let v = Variable::universal("x".into());
        assert_eq!(v.name(), "x");
        assert!(v.is_universal());
        assert!(!v.is_existential());
        assert_eq!(format!("{}", v), "?x");
    }

    #[test]
    fn test_existential_variable() {
        let v = Variable::existential("y".into());
        assert_eq!(v.name(), "y");
        assert!(!v.is_universal());
        assert!(v.is_existential());
        assert_eq!(format!("{}", v), "_:y");
    }

    #[test]
    fn test_variable_equality() {
        let v1 = Variable::universal("x".into());
        let v2 = Variable::universal("x".into());
        let v3 = Variable::existential("x".into());

        assert_eq!(v1, v2);
        assert_ne!(v1, v3); // Different quantification
    }
}
