//! Shared utility functions for builtin predicate implementations
//!
//! This module provides common operations used across multiple builtin namespaces:
//! - Value extraction (numbers, strings, URIs)
//! - Result binding (match or bind pattern)
//! - Pattern matching (for SNAF)

use crate::term::{Term, Triple, Bindings, Variable};
use super::BuiltinResult;
use crate::core::EvalResult;

// ============================================================================
// Value Extraction
// ============================================================================

/// Extract a numeric value from a term
///
/// Works with:
/// - xsd:integer, xsd:decimal, xsd:float, xsd:double literals
/// - Plain numeric literals
pub fn get_number(term: &Term) -> Option<f64> {
    match term {
        Term::Literal(lit) => lit.as_float(),
        _ => None,
    }
}

/// Extract a string value from a term
///
/// Works with:
/// - String literals (plain or typed)
/// - URIs (returns the URI string)
pub fn get_string(term: &Term) -> Option<String> {
    match term {
        Term::Literal(lit) => Some(lit.value().to_string()),
        Term::Uri(uri) => Some(uri.as_str().to_string()),
        _ => None,
    }
}

/// Extract a string value from a literal term only (not URIs)
pub fn get_literal_string(term: &Term) -> Option<String> {
    match term {
        Term::Literal(lit) => Some(lit.value().to_string()),
        _ => None,
    }
}

/// Extract a URI string from a term
pub fn get_uri(term: &Term) -> Option<String> {
    match term {
        Term::Uri(uri) => Some(uri.as_str().to_string()),
        _ => None,
    }
}

/// Extract a boolean value from a term
pub fn get_bool(term: &Term) -> Option<bool> {
    match term {
        Term::Literal(lit) => {
            match lit.value() {
                "true" | "1" => Some(true),
                "false" | "0" => Some(false),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Extract items from a list term
pub fn get_list_items(term: &Term) -> Option<Vec<Term>> {
    match term {
        Term::List(list) => Some(list.to_vec()),
        _ => None,
    }
}

// ============================================================================
// Result Binding
// ============================================================================

/// Match a numeric value against an object or bind it to a variable
///
/// If object is a variable, binds the value to it.
/// If object is a literal, checks for equality (with epsilon tolerance).
pub fn match_or_bind(object: &Term, value: f64, bindings: &Bindings) -> BuiltinResult {
    match object {
        Term::Variable(var) => {
            let mut new_bindings = bindings.clone();
            new_bindings.insert(
                var.clone(),
                Term::typed_literal(value.to_string(), "http://www.w3.org/2001/XMLSchema#decimal"),
            );
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

/// Match a numeric value against an object or bind it (returns EvalResult)
pub fn match_or_bind_eval(object: &Term, value: f64, bindings: &Bindings) -> EvalResult {
    match object {
        Term::Variable(var) => {
            let mut new_bindings = bindings.clone();
            new_bindings.insert(
                var.clone(),
                Term::typed_literal(value.to_string(), "http://www.w3.org/2001/XMLSchema#decimal"),
            );
            EvalResult::Success(new_bindings)
        }
        Term::Literal(lit) => {
            if let Some(n) = lit.as_float() {
                if (n - value).abs() < f64::EPSILON {
                    EvalResult::Success(bindings.clone())
                } else {
                    EvalResult::Failure
                }
            } else {
                EvalResult::Failure
            }
        }
        _ => EvalResult::Failure,
    }
}

/// Match a string value against an object or bind it to a variable
pub fn match_or_bind_string(object: &Term, value: String, bindings: &Bindings) -> BuiltinResult {
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

/// Match a string value against an object or bind it (returns EvalResult)
pub fn match_or_bind_string_eval(object: &Term, value: String, bindings: &Bindings) -> EvalResult {
    match object {
        Term::Variable(var) => {
            let mut new_bindings = bindings.clone();
            new_bindings.insert(var.clone(), Term::literal(value));
            EvalResult::Success(new_bindings)
        }
        Term::Literal(lit) => {
            if lit.value() == value {
                EvalResult::Success(bindings.clone())
            } else {
                EvalResult::Failure
            }
        }
        _ => EvalResult::Failure,
    }
}

/// Match a URI value against an object or bind it to a variable
pub fn match_or_bind_uri(object: &Term, value: String, bindings: &Bindings) -> BuiltinResult {
    match object {
        Term::Variable(var) => {
            let mut new_bindings = bindings.clone();
            new_bindings.insert(var.clone(), Term::uri(value));
            BuiltinResult::Success(new_bindings)
        }
        Term::Uri(uri) => {
            if uri.as_str() == value {
                BuiltinResult::Success(bindings.clone())
            } else {
                BuiltinResult::Failure
            }
        }
        _ => BuiltinResult::Failure,
    }
}

/// Match a boolean value against an object or bind it to a variable
pub fn match_or_bind_bool(object: &Term, value: bool, bindings: &Bindings) -> BuiltinResult {
    let str_value = if value { "true" } else { "false" };
    match object {
        Term::Variable(var) => {
            let mut new_bindings = bindings.clone();
            new_bindings.insert(
                var.clone(),
                Term::typed_literal(str_value, "http://www.w3.org/2001/XMLSchema#boolean"),
            );
            BuiltinResult::Success(new_bindings)
        }
        Term::Literal(lit) => {
            if let Some(b) = get_bool(&Term::Literal(lit.clone())) {
                if b == value {
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

/// Match a list against an object or bind it to a variable
pub fn match_or_bind_list(object: &Term, items: Vec<Term>, bindings: &Bindings) -> BuiltinResult {
    match object {
        Term::Variable(var) => {
            let mut new_bindings = bindings.clone();
            new_bindings.insert(var.clone(), Term::list(items));
            BuiltinResult::Success(new_bindings)
        }
        Term::List(list) => {
            let obj_items = list.to_vec();
            if obj_items == items {
                BuiltinResult::Success(bindings.clone())
            } else {
                BuiltinResult::Failure
            }
        }
        _ => BuiltinResult::Failure,
    }
}

// ============================================================================
// Variable Binding
// ============================================================================

/// Bind a variable to a term value
pub fn bind_variable(var: &Variable, value: Term, bindings: &Bindings) -> Bindings {
    let mut new_bindings = bindings.clone();
    new_bindings.insert(var.clone(), value);
    new_bindings
}

/// Check if a term is bound (not a variable, or a bound variable)
pub fn is_bound(term: &Term, bindings: &Bindings) -> bool {
    match term {
        Term::Variable(var) => bindings.contains_key(var),
        _ => true,
    }
}

/// Resolve a term to its bound value if it's a variable
pub fn resolve(term: &Term, bindings: &Bindings) -> Term {
    match term {
        Term::Variable(var) => bindings.get(var).cloned().unwrap_or_else(|| term.clone()),
        _ => term.clone(),
    }
}

// ============================================================================
// Pattern Matching (for SNAF)
// ============================================================================

/// Check if a term matches another term, supporting variables as wildcards
///
/// Variables in the pattern match anything. Non-variables must match structurally.
pub fn term_matches(pattern: &Term, term: &Term) -> bool {
    match pattern {
        // Variables in the pattern match anything
        Term::Variable(_) => true,
        // For non-variables, do structural comparison
        Term::Uri(uri1) => {
            if let Term::Uri(uri2) = term {
                uri1.as_str() == uri2.as_str()
            } else {
                false
            }
        }
        Term::Literal(lit1) => {
            if let Term::Literal(lit2) = term {
                lit1.value() == lit2.value() && lit1.datatype() == lit2.datatype()
            } else {
                false
            }
        }
        Term::BlankNode(bn1) => {
            if let Term::BlankNode(bn2) = term {
                bn1.label() == bn2.label()
            } else {
                false
            }
        }
        Term::Formula(f1) => {
            if let Term::Formula(f2) = term {
                let triples1 = f1.triples();
                let triples2 = f2.triples();
                if triples1.len() != triples2.len() {
                    return false;
                }
                triples1
                    .iter()
                    .zip(triples2.iter())
                    .all(|(t1, t2)| pattern_matches(t1, t2))
            } else {
                false
            }
        }
        Term::List(list1) => {
            if let Term::List(list2) = term {
                if list1.len() != list2.len() {
                    return false;
                }
                list1
                    .iter()
                    .zip(list2.iter())
                    .all(|(t1, t2)| term_matches(t1, t2))
            } else {
                false
            }
        }
    }
}

/// Check if a pattern triple matches a target triple
///
/// Variables in the pattern act as wildcards.
pub fn pattern_matches(pattern: &Triple, triple: &Triple) -> bool {
    term_matches(&pattern.subject, &triple.subject)
        && term_matches(&pattern.predicate, &triple.predicate)
        && term_matches(&pattern.object, &triple.object)
}

// ============================================================================
// Type Checking
// ============================================================================

/// Check if a term is a number
pub fn is_number(term: &Term) -> bool {
    get_number(term).is_some()
}

/// Check if a term is a string literal
pub fn is_string(term: &Term) -> bool {
    matches!(term, Term::Literal(_))
}

/// Check if a term is a URI
pub fn is_uri(term: &Term) -> bool {
    matches!(term, Term::Uri(_))
}

/// Check if a term is a blank node
pub fn is_blank(term: &Term) -> bool {
    matches!(term, Term::BlankNode(_))
}

/// Check if a term is a list
pub fn is_list(term: &Term) -> bool {
    matches!(term, Term::List(_))
}

/// Check if a term is a formula
pub fn is_formula(term: &Term) -> bool {
    matches!(term, Term::Formula(_))
}

/// Check if a term is a variable
pub fn is_variable(term: &Term) -> bool {
    matches!(term, Term::Variable(_))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_number() {
        let lit = Term::typed_literal("42", "http://www.w3.org/2001/XMLSchema#integer");
        assert_eq!(get_number(&lit), Some(42.0));

        let uri = Term::uri("http://example.org/");
        assert_eq!(get_number(&uri), None);
    }

    #[test]
    fn test_get_string() {
        let lit = Term::literal("hello");
        assert_eq!(get_string(&lit), Some("hello".to_string()));

        let uri = Term::uri("http://example.org/");
        assert_eq!(get_string(&uri), Some("http://example.org/".to_string()));
    }

    #[test]
    fn test_term_matches() {
        let var = Term::universal("x");
        let uri = Term::uri("http://example.org/");

        // Variables match anything
        assert!(term_matches(&var, &uri));
        assert!(term_matches(&var, &var));

        // URIs must match exactly
        let uri2 = Term::uri("http://example.org/");
        let uri3 = Term::uri("http://other.org/");
        assert!(term_matches(&uri, &uri2));
        assert!(!term_matches(&uri, &uri3));
    }

    #[test]
    fn test_match_or_bind_number() {
        let bindings = Bindings::default();
        let var = Term::universal("x");

        // Bind to variable
        if let BuiltinResult::Success(b) = match_or_bind(&var, 42.0, &bindings) {
            let var_key = crate::term::Variable::universal("x".to_string());
            assert!(b.contains_key(&var_key));
        } else {
            panic!("Expected success");
        }

        // Match existing value
        let lit = Term::typed_literal("42", "http://www.w3.org/2001/XMLSchema#decimal");
        assert!(matches!(match_or_bind(&lit, 42.0, &bindings), BuiltinResult::Success(_)));
        assert!(matches!(match_or_bind(&lit, 43.0, &bindings), BuiltinResult::Failure));
    }
}
