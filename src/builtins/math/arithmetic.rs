//! Arithmetic operations for math namespace

use crate::core::{BuiltinPredicate, EvalResult};
use crate::term::{Term, Bindings};
use crate::builtins::helpers::{get_number, match_or_bind_eval};

/// math:sum - (a b) math:sum c means a + b = c
///
/// BIDIRECTIONAL: Can solve for any one unknown variable.
#[derive(Debug)]
pub struct Sum;

impl BuiltinPredicate for Sum {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "sum")
    }

    fn is_bidirectional(&self) -> bool {
        true
    }

    fn description(&self) -> &str {
        "(a b) math:sum c means a + b = c. Can solve for any one unknown."
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        let Term::List(list) = subject else {
            return EvalResult::NotReady;
        };

        let items = list.to_vec();
        if items.len() != 2 {
            return EvalResult::NotReady;
        }

        let a_val = get_number(&items[0]);
        let b_val = get_number(&items[1]);
        let c_val = get_number(object);

        match (a_val, b_val, c_val) {
            // Forward: a + b = ?c
            (Some(a), Some(b), _) => match_or_bind_eval(object, a + b, bindings),
            // Backward: ?a + b = c → a = c - b
            (None, Some(b), Some(c)) => {
                if let Term::Variable(var) = &items[0] {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(
                        var.clone(),
                        Term::typed_literal(
                            (c - b).to_string(),
                            "http://www.w3.org/2001/XMLSchema#decimal",
                        ),
                    );
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            // Backward: a + ?b = c → b = c - a
            (Some(a), None, Some(c)) => {
                if let Term::Variable(var) = &items[1] {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(
                        var.clone(),
                        Term::typed_literal(
                            (c - a).to_string(),
                            "http://www.w3.org/2001/XMLSchema#decimal",
                        ),
                    );
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            _ => EvalResult::NotReady,
        }
    }
}

/// math:difference - a math:difference b = c means a - b = c
///
/// BIDIRECTIONAL: Can solve for any one unknown variable.
#[derive(Debug)]
pub struct Difference;

impl BuiltinPredicate for Difference {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "difference")
    }

    fn is_bidirectional(&self) -> bool {
        true
    }

    fn description(&self) -> &str {
        "(a b) math:difference c means a - b = c. Can solve for any one unknown."
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        let Term::List(list) = subject else {
            return EvalResult::NotReady;
        };

        let items = list.to_vec();
        if items.len() != 2 {
            return EvalResult::NotReady;
        }

        let a_val = get_number(&items[0]);
        let b_val = get_number(&items[1]);
        let c_val = get_number(object);

        match (a_val, b_val, c_val) {
            // Forward: a - b = ?c
            (Some(a), Some(b), _) => match_or_bind_eval(object, a - b, bindings),
            // Backward: ?a - b = c → a = c + b
            (None, Some(b), Some(c)) => {
                if let Term::Variable(var) = &items[0] {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(
                        var.clone(),
                        Term::typed_literal(
                            (c + b).to_string(),
                            "http://www.w3.org/2001/XMLSchema#decimal",
                        ),
                    );
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            // Backward: a - ?b = c → b = a - c
            (Some(a), None, Some(c)) => {
                if let Term::Variable(var) = &items[1] {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(
                        var.clone(),
                        Term::typed_literal(
                            (a - c).to_string(),
                            "http://www.w3.org/2001/XMLSchema#decimal",
                        ),
                    );
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            _ => EvalResult::NotReady,
        }
    }
}

/// math:product - (a b) math:product c means a * b = c
///
/// BIDIRECTIONAL: Can solve for any one unknown variable.
#[derive(Debug)]
pub struct Product;

impl BuiltinPredicate for Product {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "product")
    }

    fn is_bidirectional(&self) -> bool {
        true
    }

    fn description(&self) -> &str {
        "(a b) math:product c means a * b = c. Can solve for any one unknown."
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        let Term::List(list) = subject else {
            return EvalResult::NotReady;
        };

        let items = list.to_vec();
        if items.len() != 2 {
            return EvalResult::NotReady;
        }

        let a_val = get_number(&items[0]);
        let b_val = get_number(&items[1]);
        let c_val = get_number(object);

        match (a_val, b_val, c_val) {
            // Forward: a * b = ?c
            (Some(a), Some(b), _) => match_or_bind_eval(object, a * b, bindings),
            // Backward: ?a * b = c → a = c / b (if b != 0)
            (None, Some(b), Some(c)) if b.abs() > f64::EPSILON => {
                if let Term::Variable(var) = &items[0] {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(
                        var.clone(),
                        Term::typed_literal(
                            (c / b).to_string(),
                            "http://www.w3.org/2001/XMLSchema#decimal",
                        ),
                    );
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            // Backward: a * ?b = c → b = c / a (if a != 0)
            (Some(a), None, Some(c)) if a.abs() > f64::EPSILON => {
                if let Term::Variable(var) = &items[1] {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(
                        var.clone(),
                        Term::typed_literal(
                            (c / a).to_string(),
                            "http://www.w3.org/2001/XMLSchema#decimal",
                        ),
                    );
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            _ => EvalResult::NotReady,
        }
    }
}

/// math:quotient - (a b) math:quotient c means a / b = c
///
/// BIDIRECTIONAL: Can solve for any one unknown variable.
#[derive(Debug)]
pub struct Quotient;

impl BuiltinPredicate for Quotient {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "quotient")
    }

    fn is_bidirectional(&self) -> bool {
        true
    }

    fn description(&self) -> &str {
        "(a b) math:quotient c means a / b = c. Can solve for any one unknown."
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        let Term::List(list) = subject else {
            return EvalResult::NotReady;
        };

        let items = list.to_vec();
        if items.len() != 2 {
            return EvalResult::NotReady;
        }

        let a_val = get_number(&items[0]);
        let b_val = get_number(&items[1]);
        let c_val = get_number(object);

        match (a_val, b_val, c_val) {
            // Forward: a / b = ?c (if b != 0)
            (Some(a), Some(b), _) if b.abs() > f64::EPSILON => {
                match_or_bind_eval(object, a / b, bindings)
            }
            // Backward: ?a / b = c → a = c * b
            (None, Some(b), Some(c)) => {
                if let Term::Variable(var) = &items[0] {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(
                        var.clone(),
                        Term::typed_literal(
                            (c * b).to_string(),
                            "http://www.w3.org/2001/XMLSchema#decimal",
                        ),
                    );
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            // Backward: a / ?b = c → b = a / c (if c != 0)
            (Some(a), None, Some(c)) if c.abs() > f64::EPSILON => {
                if let Term::Variable(var) = &items[1] {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(
                        var.clone(),
                        Term::typed_literal(
                            (a / c).to_string(),
                            "http://www.w3.org/2001/XMLSchema#decimal",
                        ),
                    );
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            _ => EvalResult::NotReady,
        }
    }
}

/// math:remainder - (a b) math:remainder c means a % b = c
#[derive(Debug)]
pub struct Remainder;

impl BuiltinPredicate for Remainder {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "remainder")
    }

    fn description(&self) -> &str {
        "(a b) math:remainder c means a % b = c (modulo operation)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        let Term::List(list) = subject else {
            return EvalResult::NotReady;
        };

        let items = list.to_vec();
        if items.len() != 2 {
            return EvalResult::NotReady;
        }

        let a = get_number(&items[0]);
        let b = get_number(&items[1]);

        match (a, b) {
            (Some(a), Some(b)) if b.abs() > f64::EPSILON => {
                match_or_bind_eval(object, a % b, bindings)
            }
            _ => EvalResult::NotReady,
        }
    }
}

/// math:exponentiation - (a b) math:exponentiation c means a^b = c
///
/// BIDIRECTIONAL: Can solve for base, exponent, or result.
#[derive(Debug)]
pub struct Exponentiation;

impl BuiltinPredicate for Exponentiation {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "exponentiation")
    }

    fn is_bidirectional(&self) -> bool {
        true
    }

    fn description(&self) -> &str {
        "(a b) math:exponentiation c means a^b = c. Can solve for any one unknown."
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        let Term::List(list) = subject else {
            return EvalResult::NotReady;
        };

        let items = list.to_vec();
        if items.len() != 2 {
            return EvalResult::NotReady;
        }

        let base = get_number(&items[0]);
        let exp = get_number(&items[1]);
        let result = get_number(object);

        match (base, exp, result) {
            // Forward: a^b = ?c
            (Some(a), Some(b), _) => match_or_bind_eval(object, a.powf(b), bindings),
            // Backward: ?a^b = c → a = c^(1/b)
            (None, Some(b), Some(c)) if b.abs() > f64::EPSILON => {
                if let Term::Variable(var) = &items[0] {
                    let a = c.powf(1.0 / b);
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(
                        var.clone(),
                        Term::typed_literal(
                            a.to_string(),
                            "http://www.w3.org/2001/XMLSchema#decimal",
                        ),
                    );
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            // Backward: a^?b = c → b = log_a(c)
            (Some(a), None, Some(c)) if a > 0.0 && a != 1.0 && c > 0.0 => {
                if let Term::Variable(var) = &items[1] {
                    let b = c.ln() / a.ln();
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(
                        var.clone(),
                        Term::typed_literal(
                            b.to_string(),
                            "http://www.w3.org/2001/XMLSchema#decimal",
                        ),
                    );
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            _ => EvalResult::NotReady,
        }
    }
}

/// math:negation - a math:negation b means b = -a
#[derive(Debug)]
pub struct Negation;

impl BuiltinPredicate for Negation {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "negation")
    }

    fn is_bidirectional(&self) -> bool {
        true
    }

    fn description(&self) -> &str {
        "a math:negation b means b = -a. Bidirectional."
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        let a = get_number(subject);
        let b = get_number(object);

        match (a, b) {
            (Some(a), _) => match_or_bind_eval(object, -a, bindings),
            (None, Some(b)) => {
                if let Term::Variable(var) = subject {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(
                        var.clone(),
                        Term::typed_literal(
                            (-b).to_string(),
                            "http://www.w3.org/2001/XMLSchema#decimal",
                        ),
                    );
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            _ => EvalResult::NotReady,
        }
    }
}

/// math:absoluteValue - a math:absoluteValue b means b = |a|
#[derive(Debug)]
pub struct AbsoluteValue;

impl BuiltinPredicate for AbsoluteValue {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "absoluteValue")
    }

    fn description(&self) -> &str {
        "a math:absoluteValue b means b = |a|"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Some(a) = get_number(subject) {
            match_or_bind_eval(object, a.abs(), bindings)
        } else {
            EvalResult::NotReady
        }
    }
}

/// math:rounded - a math:rounded b means b = round(a)
#[derive(Debug)]
pub struct Rounded;

impl BuiltinPredicate for Rounded {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "rounded")
    }

    fn description(&self) -> &str {
        "a math:rounded b means b = round(a) (round to nearest integer)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Some(a) = get_number(subject) {
            match_or_bind_eval(object, a.round(), bindings)
        } else {
            EvalResult::NotReady
        }
    }
}

/// math:floor - a math:floor b means b = floor(a)
#[derive(Debug)]
pub struct Floor;

impl BuiltinPredicate for Floor {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "floor")
    }

    fn description(&self) -> &str {
        "a math:floor b means b = floor(a) (round down)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Some(a) = get_number(subject) {
            match_or_bind_eval(object, a.floor(), bindings)
        } else {
            EvalResult::NotReady
        }
    }
}

/// math:ceiling - a math:ceiling b means b = ceil(a)
#[derive(Debug)]
pub struct Ceiling;

impl BuiltinPredicate for Ceiling {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "ceiling")
    }

    fn description(&self) -> &str {
        "a math:ceiling b means b = ceil(a) (round up)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Some(a) = get_number(subject) {
            match_or_bind_eval(object, a.ceil(), bindings)
        } else {
            EvalResult::NotReady
        }
    }
}

/// math:sin - a math:sin b means b = sin(a) (a in radians)
#[derive(Debug)]
pub struct Sin;

impl BuiltinPredicate for Sin {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "sin")
    }

    fn description(&self) -> &str {
        "a math:sin b means b = sin(a) (a in radians)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Some(a) = get_number(subject) {
            match_or_bind_eval(object, a.sin(), bindings)
        } else {
            EvalResult::NotReady
        }
    }
}

/// math:cos - a math:cos b means b = cos(a) (a in radians)
#[derive(Debug)]
pub struct Cos;

impl BuiltinPredicate for Cos {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "cos")
    }

    fn description(&self) -> &str {
        "a math:cos b means b = cos(a) (a in radians)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Some(a) = get_number(subject) {
            match_or_bind_eval(object, a.cos(), bindings)
        } else {
            EvalResult::NotReady
        }
    }
}

/// math:tan - a math:tan b means b = tan(a) (a in radians)
#[derive(Debug)]
pub struct Tan;

impl BuiltinPredicate for Tan {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "tan")
    }

    fn description(&self) -> &str {
        "a math:tan b means b = tan(a) (a in radians)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Some(a) = get_number(subject) {
            match_or_bind_eval(object, a.tan(), bindings)
        } else {
            EvalResult::NotReady
        }
    }
}

/// math:asin - a math:asin b means b = asin(a) (result in radians)
#[derive(Debug)]
pub struct ASin;

impl BuiltinPredicate for ASin {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "asin")
    }

    fn description(&self) -> &str {
        "a math:asin b means b = asin(a) (result in radians)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Some(a) = get_number(subject) {
            if a >= -1.0 && a <= 1.0 {
                match_or_bind_eval(object, a.asin(), bindings)
            } else {
                EvalResult::Failure
            }
        } else {
            EvalResult::NotReady
        }
    }
}

/// math:acos - a math:acos b means b = acos(a) (result in radians)
#[derive(Debug)]
pub struct ACos;

impl BuiltinPredicate for ACos {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "acos")
    }

    fn description(&self) -> &str {
        "a math:acos b means b = acos(a) (result in radians)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Some(a) = get_number(subject) {
            if a >= -1.0 && a <= 1.0 {
                match_or_bind_eval(object, a.acos(), bindings)
            } else {
                EvalResult::Failure
            }
        } else {
            EvalResult::NotReady
        }
    }
}

/// math:atan - a math:atan b means b = atan(a) (result in radians)
#[derive(Debug)]
pub struct ATan;

impl BuiltinPredicate for ATan {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "atan")
    }

    fn description(&self) -> &str {
        "a math:atan b means b = atan(a) (result in radians)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Some(a) = get_number(subject) {
            match_or_bind_eval(object, a.atan(), bindings)
        } else {
            EvalResult::NotReady
        }
    }
}

/// math:sinh - a math:sinh b means b = sinh(a)
#[derive(Debug)]
pub struct SinH;

impl BuiltinPredicate for SinH {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "sinh")
    }

    fn description(&self) -> &str {
        "a math:sinh b means b = sinh(a) (hyperbolic sine)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Some(a) = get_number(subject) {
            match_or_bind_eval(object, a.sinh(), bindings)
        } else {
            EvalResult::NotReady
        }
    }
}

/// math:cosh - a math:cosh b means b = cosh(a)
#[derive(Debug)]
pub struct CosH;

impl BuiltinPredicate for CosH {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "cosh")
    }

    fn description(&self) -> &str {
        "a math:cosh b means b = cosh(a) (hyperbolic cosine)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Some(a) = get_number(subject) {
            match_or_bind_eval(object, a.cosh(), bindings)
        } else {
            EvalResult::NotReady
        }
    }
}

/// math:tanh - a math:tanh b means b = tanh(a)
#[derive(Debug)]
pub struct TanH;

impl BuiltinPredicate for TanH {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "tanh")
    }

    fn description(&self) -> &str {
        "a math:tanh b means b = tanh(a) (hyperbolic tangent)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Some(a) = get_number(subject) {
            match_or_bind_eval(object, a.tanh(), bindings)
        } else {
            EvalResult::NotReady
        }
    }
}

/// math:log - (base value) math:log result means result = log_base(value)
#[derive(Debug)]
pub struct Log;

impl BuiltinPredicate for Log {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "log")
    }

    fn description(&self) -> &str {
        "(base value) math:log result means result = log_base(value)"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        let Term::List(list) = subject else {
            return EvalResult::NotReady;
        };

        let items = list.to_vec();
        if items.len() != 2 {
            return EvalResult::NotReady;
        }

        let base = get_number(&items[0]);
        let value = get_number(&items[1]);

        match (base, value) {
            (Some(b), Some(v)) if b > 0.0 && b != 1.0 && v > 0.0 => {
                match_or_bind_eval(object, v.ln() / b.ln(), bindings)
            }
            _ => EvalResult::NotReady,
        }
    }
}

/// math:memberCount - list math:memberCount count
#[derive(Debug)]
pub struct MemberCount;

impl BuiltinPredicate for MemberCount {
    fn uri(&self) -> &str {
        concat!("http://www.w3.org/2000/10/swap/math#", "memberCount")
    }

    fn description(&self) -> &str {
        "list math:memberCount count - count is the length of the list"
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        if let Term::List(list) = subject {
            match_or_bind_eval(object, list.len() as f64, bindings)
        } else {
            EvalResult::NotReady
        }
    }
}
