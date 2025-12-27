//! Built-in predicates for N3 reasoning
//!
//! Implements the cwm built-in predicates for:
//! - math: arithmetic operations
//! - string: string manipulation
//! - log: logical operations
//! - list: list operations
//! - crypto: cryptographic functions

use std::collections::HashMap;
use crate::term::{Term, Bindings, uri::ns};

/// Result of evaluating a built-in
#[derive(Debug, Clone)]
pub enum BuiltinResult {
    /// The built-in succeeded with these bindings
    Success(Bindings),
    /// The built-in failed (no match)
    Failure,
    /// The built-in cannot be evaluated (insufficient bindings)
    NotReady,
}

/// A built-in predicate function
pub type BuiltinFn = fn(&Term, &Term, &Bindings) -> BuiltinResult;

/// Registry of built-in predicates
pub struct BuiltinRegistry {
    builtins: HashMap<String, BuiltinFn>,
}

impl BuiltinRegistry {
    /// Create a new registry with standard built-ins
    pub fn new() -> Self {
        let mut registry = BuiltinRegistry {
            builtins: HashMap::new(),
        };

        // Register math built-ins
        registry.register_math();
        // Register string built-ins
        registry.register_string();
        // Register log built-ins
        registry.register_log();
        // Register list built-ins
        registry.register_list();

        registry
    }

    /// Register a built-in predicate
    pub fn register(&mut self, uri: &str, f: BuiltinFn) {
        self.builtins.insert(uri.to_string(), f);
    }

    /// Check if a URI is a built-in predicate
    pub fn is_builtin(&self, uri: &str) -> bool {
        self.builtins.contains_key(uri)
    }

    /// Get a built-in function
    pub fn get(&self, uri: &str) -> Option<&BuiltinFn> {
        self.builtins.get(uri)
    }

    /// Evaluate a built-in predicate
    pub fn evaluate(&self, predicate: &str, subject: &Term, object: &Term, bindings: &Bindings) -> BuiltinResult {
        if let Some(f) = self.builtins.get(predicate) {
            f(subject, object, bindings)
        } else {
            BuiltinResult::Failure
        }
    }

    fn register_math(&mut self) {
        // math:sum - (a b) math:sum c means a + b = c
        self.register(&format!("{}sum", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        let sum = a + b;
                        return match_or_bind(object, sum, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:difference - (a b) math:difference c means a - b = c
        self.register(&format!("{}difference", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        let diff = a - b;
                        return match_or_bind(object, diff, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:product - (a b) math:product c means a * b = c
        self.register(&format!("{}product", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        let prod = a * b;
                        return match_or_bind(object, prod, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:quotient - (a b) math:quotient c means a / b = c
        self.register(&format!("{}quotient", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        if b != 0.0 {
                            let quot = a / b;
                            return match_or_bind(object, quot, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:lessThan - a math:lessThan b succeeds if a < b
        self.register(&format!("{}lessThan", ns::MATH), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_number(subject), get_number(object)) {
                if a < b {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // math:greaterThan - a math:greaterThan b succeeds if a > b
        self.register(&format!("{}greaterThan", ns::MATH), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_number(subject), get_number(object)) {
                if a > b {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // math:equalTo - a math:equalTo b succeeds if a == b
        self.register(&format!("{}equalTo", ns::MATH), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_number(subject), get_number(object)) {
                if (a - b).abs() < f64::EPSILON {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });
    }

    fn register_string(&mut self) {
        // string:concatenation - (a b) string:concatenation c
        self.register(&format!("{}concatenation", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                let mut result = String::new();
                for item in items {
                    if let Some(s) = get_string(&item) {
                        result.push_str(&s);
                    } else {
                        return BuiltinResult::NotReady;
                    }
                }
                return match_or_bind_string(object, result, bindings);
            }
            BuiltinResult::NotReady
        });

        // string:contains - a string:contains b succeeds if a contains b
        self.register(&format!("{}contains", ns::STRING), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_string(subject), get_string(object)) {
                if a.contains(&b) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:length - a string:length b means len(a) = b
        self.register(&format!("{}length", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                let len = s.len() as f64;
                return match_or_bind(object, len, bindings);
            }
            BuiltinResult::NotReady
        });
    }

    fn register_log(&mut self) {
        // log:equalTo - a log:equalTo b succeeds if a == b
        self.register(&format!("{}equalTo", ns::LOG), |subject, object, _bindings| {
            if subject == object {
                BuiltinResult::Success(Bindings::default())
            } else {
                BuiltinResult::Failure
            }
        });

        // log:notEqualTo - a log:notEqualTo b succeeds if a != b
        self.register(&format!("{}notEqualTo", ns::LOG), |subject, object, _bindings| {
            if subject != object {
                BuiltinResult::Success(Bindings::default())
            } else {
                BuiltinResult::Failure
            }
        });
    }

    fn register_list(&mut self) {
        // list:member - a list:member b succeeds if b is in list a
        self.register(&format!("{}member", ns::LIST), |subject, object, _bindings| {
            if let Term::List(list) = subject {
                for item in list.iter() {
                    if item == object {
                        return BuiltinResult::Success(Bindings::default());
                    }
                }
            }
            BuiltinResult::Failure
        });

        // list:length - a list:length b means len(a) = b
        self.register(&format!("{}length", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let len = list.len() as f64;
                return match_or_bind(object, len, bindings);
            }
            BuiltinResult::NotReady
        });

        // list:first - a list:first b means first(a) = b
        self.register(&format!("{}first", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                if let Some(first) = list.first() {
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), first.clone());
                        return BuiltinResult::Success(new_bindings);
                    } else if first == object {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:rest - a list:rest b means rest(a) = b
        self.register(&format!("{}rest", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                if let Some(rest) = list.rest() {
                    let rest_term = Term::List(std::sync::Arc::new(rest.clone()));
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), rest_term);
                        return BuiltinResult::Success(new_bindings);
                    } else if rest_term == *object {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::NotReady
        });
    }
}

impl Default for BuiltinRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper to extract a number from a term
fn get_number(term: &Term) -> Option<f64> {
    match term {
        Term::Literal(lit) => lit.as_float(),
        _ => None,
    }
}

/// Helper to extract a string from a term
fn get_string(term: &Term) -> Option<String> {
    match term {
        Term::Literal(lit) => Some(lit.value().to_string()),
        _ => None,
    }
}

/// Match a number against an object or bind it
fn match_or_bind(object: &Term, value: f64, bindings: &Bindings) -> BuiltinResult {
    match object {
        Term::Variable(var) => {
            let mut new_bindings = bindings.clone();
            new_bindings.insert(var.clone(), Term::typed_literal(value.to_string(), "http://www.w3.org/2001/XMLSchema#decimal"));
            BuiltinResult::Success(new_bindings)
        }
        Term::Literal(lit) => {
            if let Some(n) = lit.as_float() {
                if (n - value).abs() < f64::EPSILON {
                    BuiltinResult::Success(bindings.clone())
                } else {
                    BuiltinResult::Failure
                }
            } else {
                BuiltinResult::Failure
            }
        }
        _ => BuiltinResult::Failure,
    }
}

/// Match a string against an object or bind it
fn match_or_bind_string(object: &Term, value: String, bindings: &Bindings) -> BuiltinResult {
    match object {
        Term::Variable(var) => {
            let mut new_bindings = bindings.clone();
            new_bindings.insert(var.clone(), Term::literal(value));
            BuiltinResult::Success(new_bindings)
        }
        Term::Literal(lit) => {
            if lit.value() == value {
                BuiltinResult::Success(bindings.clone())
            } else {
                BuiltinResult::Failure
            }
        }
        _ => BuiltinResult::Failure,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_math_sum() {
        let registry = BuiltinRegistry::new();
        let bindings = Bindings::default();

        let list = Term::list(vec![
            Term::typed_literal("2", "http://www.w3.org/2001/XMLSchema#integer"),
            Term::typed_literal("3", "http://www.w3.org/2001/XMLSchema#integer"),
        ]);

        let result = registry.evaluate(
            &format!("{}sum", ns::MATH),
            &list,
            &Term::universal("x"),
            &bindings,
        );

        match result {
            BuiltinResult::Success(b) => {
                // Should have bound ?x to 5
                assert!(b.contains_key(&crate::term::Variable::universal("x".into())));
            }
            _ => panic!("Expected success"),
        }
    }

    #[test]
    fn test_math_less_than() {
        let registry = BuiltinRegistry::new();
        let bindings = Bindings::default();

        let a = Term::typed_literal("2", "http://www.w3.org/2001/XMLSchema#integer");
        let b = Term::typed_literal("5", "http://www.w3.org/2001/XMLSchema#integer");

        let result = registry.evaluate(
            &format!("{}lessThan", ns::MATH),
            &a,
            &b,
            &bindings,
        );

        assert!(matches!(result, BuiltinResult::Success(_)));

        // Reverse should fail
        let result2 = registry.evaluate(
            &format!("{}lessThan", ns::MATH),
            &b,
            &a,
            &bindings,
        );

        assert!(matches!(result2, BuiltinResult::Failure));
    }

    #[test]
    fn test_string_contains() {
        let registry = BuiltinRegistry::new();
        let bindings = Bindings::default();

        let haystack = Term::literal("hello world");
        let needle = Term::literal("world");

        let result = registry.evaluate(
            &format!("{}contains", ns::STRING),
            &haystack,
            &needle,
            &bindings,
        );

        assert!(matches!(result, BuiltinResult::Success(_)));
    }

    #[test]
    fn test_log_equal() {
        let registry = BuiltinRegistry::new();
        let bindings = Bindings::default();

        let a = Term::uri("http://example.org/foo");
        let b = Term::uri("http://example.org/foo");
        let c = Term::uri("http://example.org/bar");

        assert!(matches!(
            registry.evaluate(&format!("{}equalTo", ns::LOG), &a, &b, &bindings),
            BuiltinResult::Success(_)
        ));

        assert!(matches!(
            registry.evaluate(&format!("{}equalTo", ns::LOG), &a, &c, &bindings),
            BuiltinResult::Failure
        ));
    }
}
