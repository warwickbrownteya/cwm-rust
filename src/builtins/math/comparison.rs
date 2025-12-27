//! Comparison operations for math namespace

use crate::core::{BuiltinPredicate, EvalResult};
use crate::term::{Term, Bindings};
use crate::builtins::helpers::get_number;

/// math:lessThan - a math:lessThan b means a < b
#[derive(Debug)]
pub struct LessThan;

impl BuiltinPredicate for LessThan {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "lessThan")
    }

    fn required_groundedness(&self) -> u8 {
        2
    }

    fn description(&self) -> &str {
        "a math:lessThan b means a < b"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        match (get_number(subject), get_number(object)) {
            (Some(a), Some(b)) => {
                if a < b {
                    EvalResult::Success(bindings.clone())
                } else {
                    EvalResult::Failure
                }
            }
            _ => EvalResult::NotReady,
        }
    }
}

/// math:greaterThan - a math:greaterThan b means a > b
#[derive(Debug)]
pub struct GreaterThan;

impl BuiltinPredicate for GreaterThan {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "greaterThan")
    }

    fn required_groundedness(&self) -> u8 {
        2
    }

    fn description(&self) -> &str {
        "a math:greaterThan b means a > b"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        match (get_number(subject), get_number(object)) {
            (Some(a), Some(b)) => {
                if a > b {
                    EvalResult::Success(bindings.clone())
                } else {
                    EvalResult::Failure
                }
            }
            _ => EvalResult::NotReady,
        }
    }
}

/// math:notLessThan - a math:notLessThan b means a >= b
#[derive(Debug)]
pub struct NotLessThan;

impl BuiltinPredicate for NotLessThan {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "notLessThan")
    }

    fn required_groundedness(&self) -> u8 {
        2
    }

    fn description(&self) -> &str {
        "a math:notLessThan b means a >= b"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        match (get_number(subject), get_number(object)) {
            (Some(a), Some(b)) => {
                if a >= b {
                    EvalResult::Success(bindings.clone())
                } else {
                    EvalResult::Failure
                }
            }
            _ => EvalResult::NotReady,
        }
    }
}

/// math:notGreaterThan - a math:notGreaterThan b means a <= b
#[derive(Debug)]
pub struct NotGreaterThan;

impl BuiltinPredicate for NotGreaterThan {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "notGreaterThan")
    }

    fn required_groundedness(&self) -> u8 {
        2
    }

    fn description(&self) -> &str {
        "a math:notGreaterThan b means a <= b"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        match (get_number(subject), get_number(object)) {
            (Some(a), Some(b)) => {
                if a <= b {
                    EvalResult::Success(bindings.clone())
                } else {
                    EvalResult::Failure
                }
            }
            _ => EvalResult::NotReady,
        }
    }
}

/// math:equalTo - a math:equalTo b means a == b (numeric equality)
#[derive(Debug)]
pub struct EqualTo;

impl BuiltinPredicate for EqualTo {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "equalTo")
    }

    fn required_groundedness(&self) -> u8 {
        2
    }

    fn description(&self) -> &str {
        "a math:equalTo b means a == b (numeric equality with epsilon tolerance)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        match (get_number(subject), get_number(object)) {
            (Some(a), Some(b)) => {
                if (a - b).abs() < f64::EPSILON {
                    EvalResult::Success(bindings.clone())
                } else {
                    EvalResult::Failure
                }
            }
            _ => EvalResult::NotReady,
        }
    }
}

/// math:notEqualTo - a math:notEqualTo b means a != b (numeric inequality)
#[derive(Debug)]
pub struct NotEqualTo;

impl BuiltinPredicate for NotEqualTo {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "notEqualTo")
    }

    fn required_groundedness(&self) -> u8 {
        2
    }

    fn description(&self) -> &str {
        "a math:notEqualTo b means a != b (numeric inequality)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        match (get_number(subject), get_number(object)) {
            (Some(a), Some(b)) => {
                if (a - b).abs() >= f64::EPSILON {
                    EvalResult::Success(bindings.clone())
                } else {
                    EvalResult::Failure
                }
            }
            _ => EvalResult::NotReady,
        }
    }
}
