//! Built-in predicates for N3 reasoning
//!
//! Implements the cwm built-in predicates for:
//! - math: arithmetic operations
//! - string: string manipulation
//! - log: logical operations
//! - list: list operations
//! - crypto: cryptographic functions
//! - time: date and time operations
//! - os: operating system functions

use std::collections::HashMap;
use regex::Regex;
use chrono::{DateTime, Datelike, Timelike, Utc, Local, TimeZone};
use sha2::{Sha256, Sha512, Digest};
use sha1::Sha1;
use md5::Md5;
use hmac::{Hmac, Mac};
use base64::{Engine as _, engine::general_purpose};
use crate::term::{Term, Bindings, uri::ns, FormulaRef};
use crate::parser;

// Re-export ureq for web fetching
use ureq;

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
        // Register time built-ins
        registry.register_time();
        // Register crypto built-ins
        registry.register_crypto();
        // Register os built-ins
        registry.register_os();
        // Register graph built-ins
        registry.register_graph();

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

        // math:notEqualTo - a math:notEqualTo b succeeds if a != b
        self.register(&format!("{}notEqualTo", ns::MATH), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_number(subject), get_number(object)) {
                if (a - b).abs() >= f64::EPSILON {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // math:notLessThan - a math:notLessThan b succeeds if a >= b
        self.register(&format!("{}notLessThan", ns::MATH), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_number(subject), get_number(object)) {
                if a >= b {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // math:notGreaterThan - a math:notGreaterThan b succeeds if a <= b
        self.register(&format!("{}notGreaterThan", ns::MATH), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_number(subject), get_number(object)) {
                if a <= b {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // math:negation - a math:negation b means -a = b
        self.register(&format!("{}negation", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, -a, bindings);
            }
            // Reverse: if object is bound, compute subject
            if let Some(b) = get_number(object) {
                if let Term::Variable(var) = subject {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), Term::typed_literal((-b).to_string(), "http://www.w3.org/2001/XMLSchema#decimal"));
                    return BuiltinResult::Success(new_bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // math:absoluteValue - a math:absoluteValue b means |a| = b
        self.register(&format!("{}absoluteValue", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.abs(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:remainder - (a b) math:remainder c means a % b = c
        self.register(&format!("{}remainder", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        if b != 0.0 {
                            let rem = a % b;
                            return match_or_bind(object, rem, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:integerQuotient - (a b) math:integerQuotient c means floor(a / b) = c
        self.register(&format!("{}integerQuotient", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        if b != 0.0 {
                            let quot = (a / b).floor();
                            return match_or_bind(object, quot, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:exponentiation - (a b) math:exponentiation c means a^b = c
        self.register(&format!("{}exponentiation", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        let result = a.powf(b);
                        return match_or_bind(object, result, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:rounded - a math:rounded b means round(a) = b
        self.register(&format!("{}rounded", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.round(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:floor - a math:floor b means floor(a) = b
        self.register(&format!("{}floor", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.floor(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:ceiling - a math:ceiling b means ceil(a) = b
        self.register(&format!("{}ceiling", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.ceil(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:sin - a math:sin b means sin(a) = b (a in radians)
        self.register(&format!("{}sin", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.sin(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:cos - a math:cos b means cos(a) = b (a in radians)
        self.register(&format!("{}cos", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.cos(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:tan - a math:tan b means tan(a) = b (a in radians)
        self.register(&format!("{}tan", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.tan(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:sinh - a math:sinh b means sinh(a) = b
        self.register(&format!("{}sinh", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.sinh(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:cosh - a math:cosh b means cosh(a) = b
        self.register(&format!("{}cosh", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.cosh(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:tanh - a math:tanh b means tanh(a) = b
        self.register(&format!("{}tanh", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.tanh(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:asin - a math:asin b means asin(a) = b
        self.register(&format!("{}asin", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                if a >= -1.0 && a <= 1.0 {
                    return match_or_bind(object, a.asin(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // math:acos - a math:acos b means acos(a) = b
        self.register(&format!("{}acos", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                if a >= -1.0 && a <= 1.0 {
                    return match_or_bind(object, a.acos(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // math:atan - a math:atan b means atan(a) = b
        self.register(&format!("{}atan", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.atan(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:atan2 - (y x) math:atan2 b means atan2(y, x) = b
        self.register(&format!("{}atan2", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(y), Some(x)) = (get_number(&items[0]), get_number(&items[1])) {
                        return match_or_bind(object, y.atan2(x), bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:degrees - a math:degrees b means a (radians) = b (degrees)
        self.register(&format!("{}degrees", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.to_degrees(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:radians - a math:radians b means a (degrees) = b (radians) [extension]
        self.register(&format!("{}radians", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.to_radians(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:logarithm - a math:logarithm b means ln(a) = b [extension]
        self.register(&format!("{}logarithm", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                if a > 0.0 {
                    return match_or_bind(object, a.ln(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // math:sqrt - a math:sqrt b means sqrt(a) = b [extension]
        self.register(&format!("{}sqrt", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                if a >= 0.0 {
                    return match_or_bind(object, a.sqrt(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // math:memberCount - list math:memberCount count
        self.register(&format!("{}memberCount", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let count = list.len() as f64;
                return match_or_bind(object, count, bindings);
            }
            BuiltinResult::NotReady
        });

        // math:sumOf - list math:sumOf sum (sum of all numbers in list)
        self.register(&format!("{}sumOf", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let mut sum = 0.0;
                for item in list.iter() {
                    if let Some(n) = get_number(item) {
                        sum += n;
                    } else {
                        return BuiltinResult::NotReady;
                    }
                }
                return match_or_bind(object, sum, bindings);
            }
            BuiltinResult::NotReady
        });

        // math:productOf - list math:productOf product (product of all numbers in list)
        self.register(&format!("{}productOf", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let mut product = 1.0;
                for item in list.iter() {
                    if let Some(n) = get_number(item) {
                        product *= n;
                    } else {
                        return BuiltinResult::NotReady;
                    }
                }
                return match_or_bind(object, product, bindings);
            }
            BuiltinResult::NotReady
        });

        // math:min - list math:min minimum (minimum value in list)
        self.register(&format!("{}min", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.is_empty() {
                    return BuiltinResult::NotReady;
                }
                let mut min = f64::INFINITY;
                for item in &items {
                    if let Some(n) = get_number(item) {
                        if n < min {
                            min = n;
                        }
                    } else {
                        return BuiltinResult::NotReady;
                    }
                }
                return match_or_bind(object, min, bindings);
            }
            BuiltinResult::NotReady
        });

        // math:max - list math:max maximum (maximum value in list)
        self.register(&format!("{}max", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.is_empty() {
                    return BuiltinResult::NotReady;
                }
                let mut max = f64::NEG_INFINITY;
                for item in &items {
                    if let Some(n) = get_number(item) {
                        if n > max {
                            max = n;
                        }
                    } else {
                        return BuiltinResult::NotReady;
                    }
                }
                return match_or_bind(object, max, bindings);
            }
            BuiltinResult::NotReady
        });

        // math:average - list math:average avg (average of numbers in list)
        self.register(&format!("{}average", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.is_empty() {
                    return BuiltinResult::NotReady;
                }
                let mut sum = 0.0;
                for item in &items {
                    if let Some(n) = get_number(item) {
                        sum += n;
                    } else {
                        return BuiltinResult::NotReady;
                    }
                }
                let avg = sum / items.len() as f64;
                return match_or_bind(object, avg, bindings);
            }
            BuiltinResult::NotReady
        });

        // math:gcd - (a b) math:gcd c (greatest common divisor)
        self.register(&format!("{}gcd", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        let a = a.abs() as i64;
                        let b = b.abs() as i64;
                        fn gcd(mut a: i64, mut b: i64) -> i64 {
                            while b != 0 {
                                let temp = b;
                                b = a % b;
                                a = temp;
                            }
                            a
                        }
                        let result = gcd(a, b) as f64;
                        return match_or_bind(object, result, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:lcm - (a b) math:lcm c (least common multiple)
        self.register(&format!("{}lcm", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        let a = a.abs() as i64;
                        let b = b.abs() as i64;
                        fn gcd(mut a: i64, mut b: i64) -> i64 {
                            while b != 0 {
                                let temp = b;
                                b = a % b;
                                a = temp;
                            }
                            a
                        }
                        let result = if a == 0 || b == 0 {
                            0.0
                        } else {
                            ((a / gcd(a, b)) * b) as f64
                        };
                        return match_or_bind(object, result, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:factors - n math:factors listOfPrimeFactors
        self.register(&format!("{}factors", ns::MATH), |subject, object, bindings| {
            if let Some(n) = get_number(subject) {
                let mut n = n.abs() as i64;
                if n <= 1 {
                    let result = Term::list(vec![]);
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), result);
                        return BuiltinResult::Success(new_bindings);
                    }
                    return BuiltinResult::NotReady;
                }
                let mut factors = Vec::new();
                let mut d = 2i64;
                while d * d <= n {
                    while n % d == 0 {
                        factors.push(Term::typed_literal(d.to_string(), "http://www.w3.org/2001/XMLSchema#integer"));
                        n /= d;
                    }
                    d += 1;
                }
                if n > 1 {
                    factors.push(Term::typed_literal(n.to_string(), "http://www.w3.org/2001/XMLSchema#integer"));
                }
                let result = Term::list(factors);
                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if result == *object {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::NotReady
        });

        // math:bitAnd - (a b) math:bitAnd c (bitwise AND)
        self.register(&format!("{}bitAnd", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        let result = ((a as i64) & (b as i64)) as f64;
                        return match_or_bind(object, result, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:bitOr - (a b) math:bitOr c (bitwise OR)
        self.register(&format!("{}bitOr", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        let result = ((a as i64) | (b as i64)) as f64;
                        return match_or_bind(object, result, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:bitXor - (a b) math:bitXor c (bitwise XOR)
        self.register(&format!("{}bitXor", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        let result = ((a as i64) ^ (b as i64)) as f64;
                        return match_or_bind(object, result, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:bitNot - a math:bitNot b (bitwise NOT)
        self.register(&format!("{}bitNot", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                let result = (!(a as i64)) as f64;
                return match_or_bind(object, result, bindings);
            }
            BuiltinResult::NotReady
        });

        // math:leftShift - (a n) math:leftShift c (left shift a by n bits)
        self.register(&format!("{}leftShift", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(n)) = (get_number(&items[0]), get_number(&items[1])) {
                        let result = ((a as i64) << (n as u32)) as f64;
                        return match_or_bind(object, result, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:rightShift - (a n) math:rightShift c (right shift a by n bits)
        self.register(&format!("{}rightShift", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(n)) = (get_number(&items[0]), get_number(&items[1])) {
                        let result = ((a as i64) >> (n as u32)) as f64;
                        return match_or_bind(object, result, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:mod - (a b) math:mod c (modulo, always positive result)
        self.register(&format!("{}mod", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        if b != 0.0 {
                            let result = ((a % b) + b) % b;  // Euclidean modulo
                            return match_or_bind(object, result, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:sign - a math:sign b (returns -1, 0, or 1)
        self.register(&format!("{}sign", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                let result = if a > 0.0 { 1.0 } else if a < 0.0 { -1.0 } else { 0.0 };
                return match_or_bind(object, result, bindings);
            }
            BuiltinResult::NotReady
        });

        // math:log10 - a math:log10 b (base-10 logarithm)
        self.register(&format!("{}log10", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                if a > 0.0 {
                    return match_or_bind(object, a.log10(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // math:log2 - a math:log2 b (base-2 logarithm)
        self.register(&format!("{}log2", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                if a > 0.0 {
                    return match_or_bind(object, a.log2(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // math:exp - a math:exp b (e^a)
        self.register(&format!("{}exp", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.exp(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:cbrt - a math:cbrt b (cube root)
        self.register(&format!("{}cbrt", ns::MATH), |subject, object, bindings| {
            if let Some(a) = get_number(subject) {
                return match_or_bind(object, a.cbrt(), bindings);
            }
            BuiltinResult::NotReady
        });

        // math:hypot - (a b) math:hypot c (hypotenuse, sqrt(a^2 + b^2))
        self.register(&format!("{}hypot", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_number(&items[0]), get_number(&items[1])) {
                        return match_or_bind(object, a.hypot(b), bindings);
                    }
                }
            }
            BuiltinResult::NotReady
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

        // string:startsWith - a string:startsWith b succeeds if a starts with b
        self.register(&format!("{}startsWith", ns::STRING), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_string(subject), get_string(object)) {
                if a.starts_with(&b) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:endsWith - a string:endsWith b succeeds if a ends with b
        self.register(&format!("{}endsWith", ns::STRING), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_string(subject), get_string(object)) {
                if a.ends_with(&b) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:lowerCase - a string:lowerCase b means lowercase(a) = b
        self.register(&format!("{}lowerCase", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                return match_or_bind_string(object, s.to_lowercase(), bindings);
            }
            BuiltinResult::NotReady
        });

        // string:upperCase - a string:upperCase b means uppercase(a) = b
        self.register(&format!("{}upperCase", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                return match_or_bind_string(object, s.to_uppercase(), bindings);
            }
            BuiltinResult::NotReady
        });

        // string:notContains - a string:notContains b succeeds if a does not contain b
        self.register(&format!("{}notContains", ns::STRING), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_string(subject), get_string(object)) {
                if !a.contains(&b) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:equalIgnoringCase - a string:equalIgnoringCase b succeeds if lowercase(a) == lowercase(b)
        self.register(&format!("{}equalIgnoringCase", ns::STRING), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_string(subject), get_string(object)) {
                if a.to_lowercase() == b.to_lowercase() {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:notEqualIgnoringCase - a string:notEqualIgnoringCase b succeeds if lowercase(a) != lowercase(b)
        self.register(&format!("{}notEqualIgnoringCase", ns::STRING), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_string(subject), get_string(object)) {
                if a.to_lowercase() != b.to_lowercase() {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:lessThan - a string:lessThan b succeeds if a < b lexicographically
        self.register(&format!("{}lessThan", ns::STRING), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_string(subject), get_string(object)) {
                if a < b {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:greaterThan - a string:greaterThan b succeeds if a > b lexicographically
        self.register(&format!("{}greaterThan", ns::STRING), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_string(subject), get_string(object)) {
                if a > b {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:notLessThan - a string:notLessThan b succeeds if a >= b lexicographically
        self.register(&format!("{}notLessThan", ns::STRING), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_string(subject), get_string(object)) {
                if a >= b {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:notGreaterThan - a string:notGreaterThan b succeeds if a <= b lexicographically
        self.register(&format!("{}notGreaterThan", ns::STRING), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_string(subject), get_string(object)) {
                if a <= b {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:matches - a string:matches b succeeds if a matches regex b
        self.register(&format!("{}matches", ns::STRING), |subject, object, _bindings| {
            if let (Some(text), Some(pattern)) = (get_string(subject), get_string(object)) {
                if let Ok(re) = Regex::new(&pattern) {
                    if re.is_match(&text) {
                        return BuiltinResult::Success(Bindings::default());
                    }
                }
            }
            BuiltinResult::Failure
        });

        // string:notMatches - a string:notMatches b succeeds if a does not match regex b
        self.register(&format!("{}notMatches", ns::STRING), |subject, object, _bindings| {
            if let (Some(text), Some(pattern)) = (get_string(subject), get_string(object)) {
                if let Ok(re) = Regex::new(&pattern) {
                    if !re.is_match(&text) {
                        return BuiltinResult::Success(Bindings::default());
                    }
                }
            }
            BuiltinResult::Failure
        });

        // string:replace - (str pattern replacement) string:replace result
        self.register(&format!("{}replace", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 3 {
                    if let (Some(text), Some(pattern), Some(replacement)) =
                        (get_string(&items[0]), get_string(&items[1]), get_string(&items[2]))
                    {
                        if let Ok(re) = Regex::new(&pattern) {
                            let result = re.replace_all(&text, replacement.as_str()).to_string();
                            return match_or_bind_string(object, result, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:scrape - (str pattern) string:scrape firstMatch (extracts first capture group)
        self.register(&format!("{}scrape", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(text), Some(pattern)) = (get_string(&items[0]), get_string(&items[1])) {
                        if let Ok(re) = Regex::new(&pattern) {
                            if let Some(captures) = re.captures(&text) {
                                // Return first capture group, or whole match if no groups
                                if let Some(m) = captures.get(1).or_else(|| captures.get(0)) {
                                    return match_or_bind_string(object, m.as_str().to_string(), bindings);
                                }
                            }
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:format - (formatString arg1 arg2 ...) string:format result
        self.register(&format!("{}format", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if !items.is_empty() {
                    if let Some(format_str) = get_string(&items[0]) {
                        let args: Vec<String> = items[1..].iter()
                            .filter_map(|item| get_string(item).or_else(|| {
                                // Also format numbers
                                if let Some(n) = get_number(item) {
                                    Some(n.to_string())
                                } else {
                                    None
                                }
                            }))
                            .collect();

                        // Simple placeholder replacement: %s or {}
                        let mut result = format_str.clone();
                        for arg in &args {
                            if let Some(pos) = result.find("%s") {
                                result = format!("{}{}{}", &result[..pos], arg, &result[pos+2..]);
                            } else if let Some(pos) = result.find("{}") {
                                result = format!("{}{}{}", &result[..pos], arg, &result[pos+2..]);
                            }
                        }
                        return match_or_bind_string(object, result, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:substring - (str start length) string:substring result
        self.register(&format!("{}substring", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() >= 2 {
                    if let (Some(text), Some(start)) = (get_string(&items[0]), get_number(&items[1])) {
                        let start = start as usize;
                        let len = if items.len() > 2 {
                            get_number(&items[2]).map(|n| n as usize)
                        } else {
                            None
                        };

                        if start <= text.len() {
                            let result = if let Some(l) = len {
                                text.chars().skip(start).take(l).collect()
                            } else {
                                text.chars().skip(start).collect()
                            };
                            return match_or_bind_string(object, result, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:split - (str separator) string:split listOfStrings
        self.register(&format!("{}split", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(text), Some(sep)) = (get_string(&items[0]), get_string(&items[1])) {
                        let parts: Vec<Term> = text.split(&sep)
                            .map(|s| Term::literal(s.to_string()))
                            .collect();
                        let result = Term::list(parts);

                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        } else if result == *object {
                            return BuiltinResult::Success(bindings.clone());
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:concat - same as concatenation but takes exactly 2 strings
        self.register(&format!("{}concat", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(a), Some(b)) = (get_string(&items[0]), get_string(&items[1])) {
                        let result = format!("{}{}", a, b);
                        return match_or_bind_string(object, result, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:containsIgnoringCase - a string:containsIgnoringCase b succeeds if lowercase(a) contains lowercase(b)
        self.register(&format!("{}containsIgnoringCase", ns::STRING), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_string(subject), get_string(object)) {
                if a.to_lowercase().contains(&b.to_lowercase()) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:containsRoughly - a string:containsRoughly b (case-insensitive, whitespace-normalized)
        self.register(&format!("{}containsRoughly", ns::STRING), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_string(subject), get_string(object)) {
                let a_norm: String = a.to_lowercase().split_whitespace().collect::<Vec<_>>().join(" ");
                let b_norm: String = b.to_lowercase().split_whitespace().collect::<Vec<_>>().join(" ");
                if a_norm.contains(&b_norm) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:notContainsRoughly - a string:notContainsRoughly b
        self.register(&format!("{}notContainsRoughly", ns::STRING), |subject, object, _bindings| {
            if let (Some(a), Some(b)) = (get_string(subject), get_string(object)) {
                let a_norm: String = a.to_lowercase().split_whitespace().collect::<Vec<_>>().join(" ");
                let b_norm: String = b.to_lowercase().split_whitespace().collect::<Vec<_>>().join(" ");
                if !a_norm.contains(&b_norm) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:trim - a string:trim b (removes leading/trailing whitespace)
        self.register(&format!("{}trim", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                return match_or_bind_string(object, s.trim().to_string(), bindings);
            }
            BuiltinResult::NotReady
        });

        // string:trimLeft - a string:trimLeft b (removes leading whitespace)
        self.register(&format!("{}trimLeft", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                return match_or_bind_string(object, s.trim_start().to_string(), bindings);
            }
            BuiltinResult::NotReady
        });

        // string:trimRight - a string:trimRight b (removes trailing whitespace)
        self.register(&format!("{}trimRight", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                return match_or_bind_string(object, s.trim_end().to_string(), bindings);
            }
            BuiltinResult::NotReady
        });

        // string:normalize - a string:normalize b (normalizes whitespace)
        self.register(&format!("{}normalize", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                let normalized: String = s.split_whitespace().collect::<Vec<_>>().join(" ");
                return match_or_bind_string(object, normalized, bindings);
            }
            BuiltinResult::NotReady
        });

        // string:reverse - a string:reverse b (reverses string)
        self.register(&format!("{}reverse", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                let reversed: String = s.chars().rev().collect();
                return match_or_bind_string(object, reversed, bindings);
            }
            BuiltinResult::NotReady
        });

        // string:indexOf - (str substring) string:indexOf index (-1 if not found)
        self.register(&format!("{}indexOf", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(text), Some(search)) = (get_string(&items[0]), get_string(&items[1])) {
                        let index = text.find(&search).map(|i| i as f64).unwrap_or(-1.0);
                        return match_or_bind(object, index, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:lastIndexOf - (str substring) string:lastIndexOf index (-1 if not found)
        self.register(&format!("{}lastIndexOf", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(text), Some(search)) = (get_string(&items[0]), get_string(&items[1])) {
                        let index = text.rfind(&search).map(|i| i as f64).unwrap_or(-1.0);
                        return match_or_bind(object, index, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:repeat - (str count) string:repeat result
        self.register(&format!("{}repeat", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(s), Some(count)) = (get_string(&items[0]), get_number(&items[1])) {
                        let count = count as usize;
                        let repeated = s.repeat(count);
                        return match_or_bind_string(object, repeated, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:padStart - (str length padChar) string:padStart result
        self.register(&format!("{}padStart", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() >= 2 {
                    if let (Some(s), Some(length)) = (get_string(&items[0]), get_number(&items[1])) {
                        let length = length as usize;
                        let pad_char = if items.len() > 2 {
                            get_string(&items[2]).and_then(|p| p.chars().next()).unwrap_or(' ')
                        } else {
                            ' '
                        };
                        if s.len() < length {
                            let padding: String = std::iter::repeat(pad_char).take(length - s.len()).collect();
                            let result = format!("{}{}", padding, s);
                            return match_or_bind_string(object, result, bindings);
                        } else {
                            return match_or_bind_string(object, s, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:padEnd - (str length padChar) string:padEnd result
        self.register(&format!("{}padEnd", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() >= 2 {
                    if let (Some(s), Some(length)) = (get_string(&items[0]), get_number(&items[1])) {
                        let length = length as usize;
                        let pad_char = if items.len() > 2 {
                            get_string(&items[2]).and_then(|p| p.chars().next()).unwrap_or(' ')
                        } else {
                            ' '
                        };
                        if s.len() < length {
                            let padding: String = std::iter::repeat(pad_char).take(length - s.len()).collect();
                            let result = format!("{}{}", s, padding);
                            return match_or_bind_string(object, result, bindings);
                        } else {
                            return match_or_bind_string(object, s, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:join - (listOfStrings separator) string:join result
        self.register(&format!("{}join", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Term::List(strings), Some(sep)) = (&items[0], get_string(&items[1])) {
                        let parts: Vec<String> = strings.iter()
                            .filter_map(|item| get_string(item))
                            .collect();
                        let result = parts.join(&sep);
                        return match_or_bind_string(object, result, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:lines - str string:lines listOfLines (splits by newline)
        self.register(&format!("{}lines", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                let lines: Vec<Term> = s.lines()
                    .map(|line| Term::literal(line.to_string()))
                    .collect();
                let result = Term::list(lines);
                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if result == *object {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::NotReady
        });

        // string:words - str string:words listOfWords (splits by whitespace)
        self.register(&format!("{}words", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                let words: Vec<Term> = s.split_whitespace()
                    .map(|word| Term::literal(word.to_string()))
                    .collect();
                let result = Term::list(words);
                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if result == *object {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::NotReady
        });

        // string:xmlEscape - str string:xmlEscape escaped (escapes XML special chars)
        self.register(&format!("{}xmlEscape", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                let escaped = s
                    .replace('&', "&amp;")
                    .replace('<', "&lt;")
                    .replace('>', "&gt;")
                    .replace('"', "&quot;")
                    .replace('\'', "&apos;");
                return match_or_bind_string(object, escaped, bindings);
            }
            BuiltinResult::NotReady
        });

        // string:htmlEscape - str string:htmlEscape escaped (escapes HTML special chars)
        self.register(&format!("{}htmlEscape", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                let escaped = s
                    .replace('&', "&amp;")
                    .replace('<', "&lt;")
                    .replace('>', "&gt;")
                    .replace('"', "&quot;");
                return match_or_bind_string(object, escaped, bindings);
            }
            BuiltinResult::NotReady
        });

        // string:encodeForURI - str string:encodeForURI encoded (percent-encodes for URI)
        self.register(&format!("{}encodeForURI", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                let mut encoded = String::new();
                for c in s.chars() {
                    match c {
                        'A'..='Z' | 'a'..='z' | '0'..='9' | '-' | '_' | '.' | '~' => {
                            encoded.push(c);
                        }
                        _ => {
                            for byte in c.to_string().as_bytes() {
                                encoded.push_str(&format!("%{:02X}", byte));
                            }
                        }
                    }
                }
                return match_or_bind_string(object, encoded, bindings);
            }
            BuiltinResult::NotReady
        });

        // string:decodeFromURI - encoded string:decodeFromURI str (decodes percent-encoding)
        self.register(&format!("{}decodeFromURI", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                let mut decoded = String::new();
                let mut chars = s.chars().peekable();
                while let Some(c) = chars.next() {
                    if c == '%' {
                        let hex: String = chars.by_ref().take(2).collect();
                        if hex.len() == 2 {
                            if let Ok(byte) = u8::from_str_radix(&hex, 16) {
                                decoded.push(byte as char);
                                continue;
                            }
                        }
                        decoded.push('%');
                        decoded.push_str(&hex);
                    } else if c == '+' {
                        decoded.push(' ');
                    } else {
                        decoded.push(c);
                    }
                }
                return match_or_bind_string(object, decoded, bindings);
            }
            BuiltinResult::NotReady
        });

        // string:uriScheme - uri string:uriScheme scheme (extracts scheme from URI)
        self.register(&format!("{}uriScheme", ns::STRING), |subject, object, bindings| {
            if let Some(uri) = get_string(subject) {
                if let Some(pos) = uri.find("://") {
                    let scheme = &uri[..pos];
                    return match_or_bind_string(object, scheme.to_string(), bindings);
                } else if let Some(pos) = uri.find(':') {
                    let scheme = &uri[..pos];
                    return match_or_bind_string(object, scheme.to_string(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // string:uriHost - uri string:uriHost host (extracts host from URI)
        self.register(&format!("{}uriHost", ns::STRING), |subject, object, bindings| {
            if let Some(uri) = get_string(subject) {
                if let Some(start) = uri.find("://") {
                    let after_scheme = &uri[start + 3..];
                    let end = after_scheme.find('/').unwrap_or(after_scheme.len());
                    let host_port = &after_scheme[..end];
                    let host = host_port.split(':').next().unwrap_or(host_port);
                    return match_or_bind_string(object, host.to_string(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // string:uriPort - uri string:uriPort port (extracts port from URI)
        self.register(&format!("{}uriPort", ns::STRING), |subject, object, bindings| {
            if let Some(uri) = get_string(subject) {
                if let Some(start) = uri.find("://") {
                    let after_scheme = &uri[start + 3..];
                    let end = after_scheme.find('/').unwrap_or(after_scheme.len());
                    let host_port = &after_scheme[..end];
                    if let Some(colon) = host_port.rfind(':') {
                        let port_str = &host_port[colon + 1..];
                        if let Ok(port) = port_str.parse::<f64>() {
                            return match_or_bind(object, port, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:uriPath - uri string:uriPath path (extracts path from URI)
        self.register(&format!("{}uriPath", ns::STRING), |subject, object, bindings| {
            if let Some(uri) = get_string(subject) {
                if let Some(start) = uri.find("://") {
                    let after_scheme = &uri[start + 3..];
                    if let Some(slash) = after_scheme.find('/') {
                        let path_query = &after_scheme[slash..];
                        let path = path_query.split('?').next().unwrap_or(path_query);
                        let path = path.split('#').next().unwrap_or(path);
                        return match_or_bind_string(object, path.to_string(), bindings);
                    }
                    return match_or_bind_string(object, "/".to_string(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // string:uriQuery - uri string:uriQuery query (extracts query string from URI)
        self.register(&format!("{}uriQuery", ns::STRING), |subject, object, bindings| {
            if let Some(uri) = get_string(subject) {
                if let Some(qmark) = uri.find('?') {
                    let query = &uri[qmark + 1..];
                    let query = query.split('#').next().unwrap_or(query);
                    return match_or_bind_string(object, query.to_string(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // string:uriFragment - uri string:uriFragment fragment (extracts fragment from URI)
        self.register(&format!("{}uriFragment", ns::STRING), |subject, object, bindings| {
            if let Some(uri) = get_string(subject) {
                if let Some(hash) = uri.find('#') {
                    let fragment = &uri[hash + 1..];
                    return match_or_bind_string(object, fragment.to_string(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // string:tokenize - (str pattern) string:tokenize listOfTokens (splits by regex)
        self.register(&format!("{}tokenize", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(text), Some(pattern)) = (get_string(&items[0]), get_string(&items[1])) {
                        if let Ok(re) = Regex::new(&pattern) {
                            let tokens: Vec<Term> = re.split(&text)
                                .map(|s| Term::literal(s.to_string()))
                                .collect();
                            let result = Term::list(tokens);
                            if let Term::Variable(var) = object {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), result);
                                return BuiltinResult::Success(new_bindings);
                            } else if result == *object {
                                return BuiltinResult::Success(bindings.clone());
                            }
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:findAll - (str pattern) string:findAll listOfMatches (finds all regex matches)
        self.register(&format!("{}findAll", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(text), Some(pattern)) = (get_string(&items[0]), get_string(&items[1])) {
                        if let Ok(re) = Regex::new(&pattern) {
                            let matches: Vec<Term> = re.find_iter(&text)
                                .map(|m| Term::literal(m.as_str().to_string()))
                                .collect();
                            let result = Term::list(matches);
                            if let Term::Variable(var) = object {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), result);
                                return BuiltinResult::Success(new_bindings);
                            } else if result == *object {
                                return BuiltinResult::Success(bindings.clone());
                            }
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:captureGroups - (str pattern) string:captureGroups listOfCaptures
        self.register(&format!("{}captureGroups", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(text), Some(pattern)) = (get_string(&items[0]), get_string(&items[1])) {
                        if let Ok(re) = Regex::new(&pattern) {
                            if let Some(captures) = re.captures(&text) {
                                let groups: Vec<Term> = captures.iter()
                                    .skip(1)  // Skip full match
                                    .map(|m| {
                                        if let Some(m) = m {
                                            Term::literal(m.as_str().to_string())
                                        } else {
                                            Term::literal("".to_string())
                                        }
                                    })
                                    .collect();
                                let result = Term::list(groups);
                                if let Term::Variable(var) = object {
                                    let mut new_bindings = bindings.clone();
                                    new_bindings.insert(var.clone(), result);
                                    return BuiltinResult::Success(new_bindings);
                                }
                            }
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:capitalize - str string:capitalize result (capitalizes first letter)
        self.register(&format!("{}capitalize", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                let mut chars = s.chars();
                let result = match chars.next() {
                    None => String::new(),
                    Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                };
                return match_or_bind_string(object, result, bindings);
            }
            BuiltinResult::NotReady
        });

        // string:charCode - str string:charCode code (Unicode code point of first char)
        self.register(&format!("{}charCode", ns::STRING), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                if let Some(c) = s.chars().next() {
                    return match_or_bind(object, c as u32 as f64, bindings);
                }
            }
            // Reverse: code to char
            if let Some(code) = get_number(object) {
                if let Some(c) = char::from_u32(code as u32) {
                    if let Term::Variable(var) = subject {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), Term::literal(c.to_string()));
                        return BuiltinResult::Success(new_bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // string:isAlpha - str string:isAlpha str (succeeds if all alphabetic)
        self.register(&format!("{}isAlpha", ns::STRING), |subject, _object, _bindings| {
            if let Some(s) = get_string(subject) {
                if !s.is_empty() && s.chars().all(|c| c.is_alphabetic()) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:isNumeric - str string:isNumeric str (succeeds if all numeric)
        self.register(&format!("{}isNumeric", ns::STRING), |subject, _object, _bindings| {
            if let Some(s) = get_string(subject) {
                if !s.is_empty() && s.chars().all(|c| c.is_numeric()) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:isAlphanumeric - str string:isAlphanumeric str
        self.register(&format!("{}isAlphanumeric", ns::STRING), |subject, _object, _bindings| {
            if let Some(s) = get_string(subject) {
                if !s.is_empty() && s.chars().all(|c| c.is_alphanumeric()) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // string:isWhitespace - str string:isWhitespace str
        self.register(&format!("{}isWhitespace", ns::STRING), |subject, _object, _bindings| {
            if let Some(s) = get_string(subject) {
                if !s.is_empty() && s.chars().all(|c| c.is_whitespace()) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
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

        // log:uri - a log:uri b means the URI string of a = b
        self.register(&format!("{}uri", ns::LOG), |subject, object, bindings| {
            if let Term::Uri(uri) = subject {
                let uri_string = uri.as_str().to_string();
                return match_or_bind_string(object, uri_string, bindings);
            }
            BuiltinResult::NotReady
        });

        // log:includes - a log:includes b succeeds if formula a includes all triples of formula b
        self.register(&format!("{}includes", ns::LOG), |subject, object, _bindings| {
            if let (Term::Formula(formula_a), Term::Formula(formula_b)) = (subject, object) {
                let triples_a: std::collections::HashSet<_> = formula_a.triples().iter().cloned().collect();
                for triple in formula_b.triples() {
                    if !triples_a.contains(triple) {
                        return BuiltinResult::Failure;
                    }
                }
                return BuiltinResult::Success(Bindings::default());
            }
            BuiltinResult::Failure
        });

        // log:notIncludes - a log:notIncludes b succeeds if formula a does not include any triple of formula b
        self.register(&format!("{}notIncludes", ns::LOG), |subject, object, _bindings| {
            if let (Term::Formula(formula_a), Term::Formula(formula_b)) = (subject, object) {
                let triples_a: std::collections::HashSet<_> = formula_a.triples().iter().cloned().collect();
                for triple in formula_b.triples() {
                    if triples_a.contains(triple) {
                        return BuiltinResult::Failure;
                    }
                }
                return BuiltinResult::Success(Bindings::default());
            }
            BuiltinResult::Failure
        });

        // log:rawType - a log:rawType b where b is the type of a (URI, Literal, Formula, List, BlankNode)
        self.register(&format!("{}rawType", ns::LOG), |subject, object, bindings| {
            let type_uri = match subject {
                Term::Uri(_) => "http://www.w3.org/2000/10/swap/log#Uri",
                Term::Literal(_) => "http://www.w3.org/2000/10/swap/log#Literal",
                Term::Formula(_) => "http://www.w3.org/2000/10/swap/log#Formula",
                Term::List(_) => "http://www.w3.org/2000/10/swap/log#List",
                Term::BlankNode(_) => "http://www.w3.org/2000/10/swap/log#BlankNode",
                Term::Variable(_) => return BuiltinResult::NotReady,
            };

            let type_term = Term::uri(type_uri);
            if let Term::Variable(var) = object {
                let mut new_bindings = bindings.clone();
                new_bindings.insert(var.clone(), type_term);
                return BuiltinResult::Success(new_bindings);
            } else if type_term == *object {
                return BuiltinResult::Success(bindings.clone());
            }
            BuiltinResult::Failure
        });

        // log:conjunction - (formula1 formula2 ...) log:conjunction combinedFormula
        self.register(&format!("{}conjunction", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let mut all_triples = Vec::new();
                for item in list.iter() {
                    if let Term::Formula(formula) = item {
                        all_triples.extend(formula.triples().iter().cloned());
                    } else {
                        return BuiltinResult::NotReady;
                    }
                }

                let result = Term::Formula(
                    crate::term::FormulaRef::new(0, all_triples)
                );

                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if result == *object {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::NotReady
        });

        // log:dtlit - (value datatype) log:dtlit typedLiteral
        self.register(&format!("{}dtlit", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(value), Term::Uri(datatype)) = (get_string(&items[0]), &items[1]) {
                        let result = Term::typed_literal(value, datatype.as_str());
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        } else if result == *object {
                            return BuiltinResult::Success(bindings.clone());
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // log:rawUri - a log:rawUri b where b is the URI string representation
        self.register(&format!("{}rawUri", ns::LOG), |subject, object, bindings| {
            if let Term::Uri(uri) = subject {
                return match_or_bind_string(object, uri.as_str().to_string(), bindings);
            }
            // Reverse: string to URI
            if let Some(uri_str) = get_string(object) {
                if let Term::Variable(var) = subject {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), Term::uri(&uri_str));
                    return BuiltinResult::Success(new_bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // log:langlit - (value lang) log:langlit langLiteral
        self.register(&format!("{}langlit", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(value), Some(lang)) = (get_string(&items[0]), get_string(&items[1])) {
                        let result = Term::lang_literal(value, &lang);
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        } else if result == *object {
                            return BuiltinResult::Success(bindings.clone());
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // log:n3String - formula log:n3String n3StringRepresentation
        self.register(&format!("{}n3String", ns::LOG), |subject, object, bindings| {
            if let Term::Formula(formula) = subject {
                // Serialize formula to N3
                let mut n3 = String::new();
                n3.push_str("{ ");
                for (i, triple) in formula.triples().iter().enumerate() {
                    if i > 0 {
                        n3.push_str(" . ");
                    }
                    n3.push_str(&format!("{} {} {}", triple.subject, triple.predicate, triple.object));
                }
                n3.push_str(" }");
                return match_or_bind_string(object, n3, bindings);
            }
            BuiltinResult::NotReady
        });

        // log:content - uri log:content stringContent (reads file content)
        self.register(&format!("{}content", ns::LOG), |subject, object, bindings| {
            if let Term::Uri(uri) = subject {
                let uri_str = uri.as_str();

                // Handle file:// URIs
                if uri_str.starts_with("file://") {
                    let path = &uri_str[7..];
                    if let Ok(content) = std::fs::read_to_string(path) {
                        return match_or_bind_string(object, content, bindings);
                    }
                }
                // Handle http:// and https:// URIs
                else if uri_str.starts_with("http://") || uri_str.starts_with("https://") {
                    if let Ok(response) = ureq::get(uri_str).call() {
                        if let Ok(content) = response.into_string() {
                            return match_or_bind_string(object, content, bindings);
                        }
                    }
                }
                // Handle local file paths
                else if let Ok(content) = std::fs::read_to_string(uri_str) {
                    return match_or_bind_string(object, content, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // log:outputString - value log:outputString value (identity, marks for string output)
        self.register(&format!("{}outputString", ns::LOG), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                return match_or_bind_string(object, s, bindings);
            }
            BuiltinResult::NotReady
        });

        // log:semantics - uri log:semantics formula (loads and parses document)
        // Fetches the document at URI and parses it as N3, returning the formula
        self.register(&format!("{}semantics", ns::LOG), |subject, object, bindings| {
            if let Term::Uri(uri) = subject {
                let uri_str = uri.as_str();
                let content = if uri_str.starts_with("file://") {
                    std::fs::read_to_string(&uri_str[7..]).ok()
                } else if uri_str.starts_with("http://") || uri_str.starts_with("https://") {
                    ureq::get(uri_str).call().ok().and_then(|r| r.into_string().ok())
                } else {
                    std::fs::read_to_string(uri_str).ok()
                };

                if let Some(content) = content {
                    // Parse the content using the N3 parser
                    match parser::parse(&content) {
                        Ok(parse_result) => {
                            let result = Term::Formula(FormulaRef::new(0, parse_result.triples));
                            if let Term::Variable(var) = object {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), result);
                                return BuiltinResult::Success(new_bindings);
                            } else if let Term::Formula(obj_formula) = object {
                                if let Term::Formula(res_formula) = &result {
                                    if res_formula.triples() == obj_formula.triples() {
                                        return BuiltinResult::Success(bindings.clone());
                                    }
                                }
                            }
                        }
                        Err(_) => {
                            return BuiltinResult::Failure;
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // log:bound - variable log:bound value (succeeds if variable is bound)
        self.register(&format!("{}bound", ns::LOG), |subject, _object, _bindings| {
            match subject {
                Term::Variable(_) => BuiltinResult::Failure,
                _ => BuiltinResult::Success(Bindings::default()),
            }
        });

        // log:notBound - variable log:notBound value (succeeds if variable is not bound)
        self.register(&format!("{}notBound", ns::LOG), |subject, _object, _bindings| {
            match subject {
                Term::Variable(_) => BuiltinResult::Success(Bindings::default()),
                _ => BuiltinResult::Failure,
            }
        });

        // log:racine - uri log:racine baseUri (removes fragment from URI)
        self.register(&format!("{}racine", ns::LOG), |subject, object, bindings| {
            if let Term::Uri(uri) = subject {
                let uri_str = uri.as_str();
                // Remove fragment if present
                let base = if let Some(pos) = uri_str.find('#') {
                    &uri_str[..pos]
                } else {
                    uri_str
                };
                let result = Term::uri(base);
                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if result == *object {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::NotReady
        });

        // log:parsedAsN3 - stringContent log:parsedAsN3 formula
        // Parses N3 string content into a formula
        self.register(&format!("{}parsedAsN3", ns::LOG), |subject, object, bindings| {
            if let Some(content) = get_string(subject) {
                // Parse the N3 content using the parser
                match parser::parse(&content) {
                    Ok(parse_result) => {
                        // Create a formula from the parsed triples
                        let result = Term::Formula(FormulaRef::new(0, parse_result.triples));
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        } else if let Term::Formula(obj_formula) = object {
                            // Check if formulas match (same triples)
                            if let Term::Formula(res_formula) = &result {
                                if res_formula.triples() == obj_formula.triples() {
                                    return BuiltinResult::Success(bindings.clone());
                                }
                            }
                        }
                    }
                    Err(_) => {
                        // Parse error - return failure
                        return BuiltinResult::Failure;
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // log:semanticsOrError - uri log:semanticsOrError formulaOrError
        // Returns formula if successful, error string otherwise
        self.register(&format!("{}semanticsOrError", ns::LOG), |subject, object, bindings| {
            if let Term::Uri(uri) = subject {
                let uri_str = uri.as_str();
                let content_result = if uri_str.starts_with("file://") {
                    std::fs::read_to_string(&uri_str[7..])
                } else if uri_str.starts_with("http://") || uri_str.starts_with("https://") {
                    ureq::get(uri_str).call()
                        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))
                        .and_then(|r| r.into_string().map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e)))
                } else {
                    std::fs::read_to_string(uri_str)
                };

                let result = match content_result {
                    Ok(content) => {
                        // Parse the content using the N3 parser
                        match parser::parse(&content) {
                            Ok(parse_result) => {
                                Term::Formula(FormulaRef::new(0, parse_result.triples))
                            }
                            Err(e) => {
                                Term::literal(format!("Parse error: {}", e))
                            }
                        }
                    }
                    Err(e) => {
                        Term::literal(format!("Fetch error: {}", e))
                    }
                };

                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // log:conclusion - formula log:conclusion conclusions
        // Derives all possible conclusions from a formula (forward-chaining)
        // Note: This requires reasoner integration - placeholder
        self.register(&format!("{}conclusion", ns::LOG), |subject, object, bindings| {
            if let Term::Formula(formula) = subject {
                // Placeholder: return same formula (would run reasoner)
                let result = Term::Formula(formula.clone());
                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // log:notIncludes already implemented above

        // log:collectAllIn - Collect all matching bindings
        self.register(&format!("{}collectAllIn", ns::LOG), |subject, object, bindings| {
            // Placeholder: pattern matching not directly implementable here
            // Would need reasoner integration
            if let (Term::Formula(_), Term::Formula(_)) = (subject, object) {
                return BuiltinResult::Success(bindings.clone());
            }
            BuiltinResult::NotReady
        });

        // log:conjunction - list log:conjunction formula (merges formulas via AND)
        // Takes a list of formulas and returns their conjunction (union of triples)
        self.register(&format!("{}conjunction", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let mut all_triples = Vec::new();
                for item in list.iter() {
                    if let Term::Formula(formula) = item {
                        all_triples.extend(formula.triples().to_vec());
                    } else {
                        return BuiltinResult::NotReady;
                    }
                }
                let result = Term::Formula(FormulaRef::new(0, all_triples));
                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if let Term::Formula(obj_formula) = object {
                    if let Term::Formula(res_formula) = &result {
                        if res_formula.triples() == obj_formula.triples() {
                            return BuiltinResult::Success(bindings.clone());
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // log:dtlit - (value datatypeUri) log:dtlit typedLiteral
        // Creates a typed literal from a value string and datatype URI
        self.register(&format!("{}dtlit", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(value), Term::Uri(dt_uri)) = (get_string(&items[0]), &items[1]) {
                        let result = Term::typed_literal(value, dt_uri.as_str());
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        } else if result == *object {
                            return BuiltinResult::Success(bindings.clone());
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // log:rawType - term log:rawType typeUri
        // Returns the low-level type: Formula, Literal, List, BlankNode, Uri, Variable
        self.register(&format!("{}rawType", ns::LOG), |subject, object, bindings| {
            let type_uri = match subject {
                Term::Formula(_) => format!("{}Formula", ns::LOG),
                Term::Literal(_) => format!("{}Literal", ns::LOG),
                Term::List(_) => format!("{}List", ns::LOG),
                Term::BlankNode(_) => format!("{}BlankNode", ns::LOG),
                Term::Uri(_) => format!("{}Uri", ns::LOG),
                Term::Variable(_) => format!("{}Variable", ns::LOG),
            };
            let result = Term::uri(&type_uri);
            if let Term::Variable(var) = object {
                let mut new_bindings = bindings.clone();
                new_bindings.insert(var.clone(), result);
                return BuiltinResult::Success(new_bindings);
            } else if result == *object {
                return BuiltinResult::Success(bindings.clone());
            }
            BuiltinResult::NotReady
        });

        // log:content - document log:content string
        // For a formula, serializes it back to N3 string
        self.register(&format!("{}content", ns::LOG), |subject, object, bindings| {
            if let Term::Formula(formula) = subject {
                // Simple serialization of triples
                let mut content = String::new();
                for triple in formula.triples() {
                    content.push_str(&format!("{} {} {} .\n", triple.subject, triple.predicate, triple.object));
                }
                return match_or_bind_string(object, content, bindings);
            }
            BuiltinResult::NotReady
        });

        // log:ground - term log:ground term (succeeds if term has no unbound variables)
        self.register(&format!("{}ground", ns::LOG), |subject, _object, _bindings| {
            fn is_ground(term: &Term) -> bool {
                match term {
                    Term::Variable(_) => false,
                    Term::List(list) => list.iter().all(is_ground),
                    Term::Formula(formula) => {
                        formula.triples().iter().all(|t| {
                            is_ground(&t.subject) && is_ground(&t.predicate) && is_ground(&t.object)
                        })
                    }
                    _ => true,
                }
            }
            if is_ground(subject) {
                BuiltinResult::Success(Bindings::default())
            } else {
                BuiltinResult::Failure
            }
        });

        // log:notSame - a log:notSame b succeeds if a and b are not the same term
        // This is for negation as failure - different from log:notEqualTo which tests value equality
        self.register(&format!("{}notSame", ns::LOG), |subject, object, _bindings| {
            // Check structural identity, not just value equality
            let same = std::ptr::eq(subject, object) || format!("{:?}", subject) == format!("{:?}", object);
            if same {
                BuiltinResult::Failure
            } else {
                BuiltinResult::Success(Bindings::default())
            }
        });

        // log:allDistinct - list log:allDistinct true (succeeds if all elements are distinct)
        self.register(&format!("{}allDistinct", ns::LOG), |subject, _object, _bindings| {
            if let Term::List(list) = subject {
                let mut seen = std::collections::HashSet::new();
                for item in list.iter() {
                    let key = format!("{:?}", item);
                    if seen.contains(&key) {
                        return BuiltinResult::Failure;
                    }
                    seen.insert(key);
                }
                return BuiltinResult::Success(Bindings::default());
            }
            BuiltinResult::Failure
        });

        // log:uuid - _ log:uuid uuidString (generates a UUID)
        self.register(&format!("{}uuid", ns::LOG), |_subject, object, bindings| {
            use std::time::{SystemTime, UNIX_EPOCH};
            let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
            let uuid = format!("{:08x}-{:04x}-{:04x}-{:04x}-{:012x}",
                now.as_secs() as u32,
                (now.as_millis() & 0xFFFF) as u16,
                0x4000 | ((now.as_nanos() >> 16) & 0x0FFF) as u16,
                0x8000 | ((now.as_nanos() >> 32) & 0x3FFF) as u16,
                now.as_nanos() as u64 & 0xFFFFFFFFFFFF
            );
            match_or_bind_string(object, uuid, bindings)
        });

        // log:skolem - term log:skolem skolemizedTerm (creates skolem constant)
        self.register(&format!("{}skolem", ns::LOG), |subject, object, bindings| {
            // Create a skolem URI based on the term
            let skolem_name = format!("_:sk{:x}", {
                use std::collections::hash_map::DefaultHasher;
                use std::hash::{Hash, Hasher};
                let mut hasher = DefaultHasher::new();
                format!("{:?}", subject).hash(&mut hasher);
                hasher.finish()
            });
            let result = Term::BlankNode(crate::term::BlankNode::labeled(skolem_name));
            if let Term::Variable(var) = object {
                let mut new_bindings = bindings.clone();
                new_bindings.insert(var.clone(), result);
                return BuiltinResult::Success(new_bindings);
            } else if result == *object {
                return BuiltinResult::Success(bindings.clone());
            }
            BuiltinResult::NotReady
        });

        // log:term - (predicate subject object) log:term triple-as-list
        self.register(&format!("{}term", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 3 {
                    let result = Term::list(items);
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), result);
                        return BuiltinResult::Success(new_bindings);
                    } else if result == *object {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // log:implies - (antecedent consequent) log:implies true
        // This is mostly for querying rule structure
        self.register(&format!("{}implies", ns::LOG), |subject, _object, _bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    // Just check that both are formulas
                    if let (Term::Formula(_), Term::Formula(_)) = (&items[0], &items[1]) {
                        return BuiltinResult::Success(Bindings::default());
                    }
                }
            }
            BuiltinResult::Failure
        });

        // log:supports - (evidence conclusion) log:supports true
        // Placeholder for proof tracking
        self.register(&format!("{}supports", ns::LOG), |subject, _object, _bindings| {
            if let Term::List(list) = subject {
                if list.len() == 2 {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // log:definitiveDocument - uri log:definitiveDocument uri
        // Returns the definitive document for a namespace
        self.register(&format!("{}definitiveDocument", ns::LOG), |subject, object, bindings| {
            if let Term::Uri(uri) = subject {
                let uri_str = uri.as_str();
                // Remove fragment to get document URI
                let doc = if let Some(pos) = uri_str.find('#') {
                    &uri_str[..pos]
                } else {
                    uri_str
                };
                let result = Term::uri(doc);
                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if result == *object {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::NotReady
        });

        // log:definitiveService - uri log:definitiveService uri
        // Returns the definitive service for a namespace (same as document for now)
        self.register(&format!("{}definitiveService", ns::LOG), |subject, object, bindings| {
            if let Term::Uri(uri) = subject {
                let result = Term::uri(uri.as_str());
                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // log:forAllIn - (formula pattern) log:forAllIn true
        // Succeeds if pattern matches all triples in formula
        self.register(&format!("{}forAllIn", ns::LOG), |subject, _object, _bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let Term::Formula(_formula) = &items[0] {
                        // Placeholder: would need pattern matching
                        return BuiltinResult::Success(Bindings::default());
                    }
                }
            }
            BuiltinResult::Failure
        });

        // log:ifThenElse - (condition thenResult elseResult) log:ifThenElse result
        // Returns thenResult if condition is a non-empty formula or true, elseResult otherwise
        self.register(&format!("{}ifThenElse", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 3 {
                    let condition = &items[0];
                    let then_result = &items[1];
                    let else_result = &items[2];

                    // Evaluate condition
                    let condition_true = match condition {
                        Term::Formula(f) => !f.triples().is_empty(),
                        Term::Literal(lit) => {
                            let s = lit.value();
                            s != "" && s.to_lowercase() != "false" && s != "0"
                        }
                        Term::List(l) => !l.is_empty(),
                        Term::Uri(_) | Term::BlankNode(_) => true,
                        Term::Variable(_) => false,  // Unbound variables are false
                    };

                    let result = if condition_true {
                        then_result.clone()
                    } else {
                        else_result.clone()
                    };

                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), result);
                        return BuiltinResult::Success(new_bindings);
                    } else if result == *object {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // log:isTrue - term log:isTrue true (succeeds if term is truthy)
        self.register(&format!("{}isTrue", ns::LOG), |subject, _object, _bindings| {
            let is_true = match subject {
                Term::Formula(f) => !f.triples().is_empty(),
                Term::Literal(lit) => {
                    let s = lit.value();
                    s != "" && s.to_lowercase() != "false" && s != "0"
                }
                Term::List(l) => !l.is_empty(),
                Term::Uri(_) | Term::BlankNode(_) => true,
                Term::Variable(_) => false,
            };
            if is_true {
                BuiltinResult::Success(Bindings::default())
            } else {
                BuiltinResult::Failure
            }
        });

        // log:isFalse - term log:isFalse true (succeeds if term is falsy)
        self.register(&format!("{}isFalse", ns::LOG), |subject, _object, _bindings| {
            let is_false = match subject {
                Term::Formula(f) => f.triples().is_empty(),
                Term::Literal(lit) => {
                    let s = lit.value();
                    s == "" || s.to_lowercase() == "false" || s == "0"
                }
                Term::List(l) => l.is_empty(),
                Term::Uri(_) | Term::BlankNode(_) => false,
                Term::Variable(_) => true,  // Unbound variables are false
            };
            if is_false {
                BuiltinResult::Success(Bindings::default())
            } else {
                BuiltinResult::Failure
            }
        });

        // log:either - (a b) log:either result (returns a if truthy, else b)
        self.register(&format!("{}either", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    let a_truthy = match &items[0] {
                        Term::Variable(_) => false,
                        Term::Formula(f) => !f.triples().is_empty(),
                        Term::Literal(lit) => {
                            let s = lit.value();
                            s != "" && s.to_lowercase() != "false" && s != "0"
                        }
                        _ => true,
                    };
                    let result = if a_truthy {
                        items[0].clone()
                    } else {
                        items[1].clone()
                    };
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), result);
                        return BuiltinResult::Success(new_bindings);
                    } else if result == *object {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // log:and - (a b) log:and true (succeeds if both are truthy)
        self.register(&format!("{}and", ns::LOG), |subject, _object, _bindings| {
            fn is_truthy(term: &Term) -> bool {
                match term {
                    Term::Variable(_) => false,
                    Term::Formula(f) => !f.triples().is_empty(),
                    Term::Literal(lit) => {
                        let s = lit.value();
                        s != "" && s.to_lowercase() != "false" && s != "0"
                    }
                    _ => true,
                }
            }
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if is_truthy(&items[0]) && is_truthy(&items[1]) {
                        return BuiltinResult::Success(Bindings::default());
                    }
                }
            }
            BuiltinResult::Failure
        });

        // log:or - (a b) log:or true (succeeds if either is truthy)
        self.register(&format!("{}or", ns::LOG), |subject, _object, _bindings| {
            fn is_truthy(term: &Term) -> bool {
                match term {
                    Term::Variable(_) => false,
                    Term::Formula(f) => !f.triples().is_empty(),
                    Term::Literal(lit) => {
                        let s = lit.value();
                        s != "" && s.to_lowercase() != "false" && s != "0"
                    }
                    _ => true,
                }
            }
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if is_truthy(&items[0]) || is_truthy(&items[1]) {
                        return BuiltinResult::Success(Bindings::default());
                    }
                }
            }
            BuiltinResult::Failure
        });

        // log:not - term log:not true (succeeds if term is falsy)
        self.register(&format!("{}not", ns::LOG), |subject, _object, _bindings| {
            let is_falsy = match subject {
                Term::Variable(_) => true,
                Term::Formula(f) => f.triples().is_empty(),
                Term::Literal(lit) => {
                    let s = lit.value();
                    s == "" || s.to_lowercase() == "false" || s == "0"
                }
                Term::List(l) => l.is_empty(),
                _ => false,
            };
            if is_falsy {
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

        // list:last - a list:last b means last(a) = b
        self.register(&format!("{}last", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if let Some(last) = items.last() {
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), last.clone());
                        return BuiltinResult::Success(new_bindings);
                    } else if last == object {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:in - a list:in b succeeds if a is in list b
        // (reverse of list:member - a is element, b is list)
        self.register(&format!("{}in", ns::LIST), |subject, object, _bindings| {
            if let Term::List(list) = object {
                for item in list.iter() {
                    if item == subject {
                        return BuiltinResult::Success(Bindings::default());
                    }
                }
            }
            BuiltinResult::Failure
        });

        // list:append - (list1 list2) list:append result (concatenates lists)
        self.register(&format!("{}append", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let (Term::List(list1), Term::List(list2)) = (&items[0], &items[1]) {
                        let mut combined = list1.to_vec();
                        combined.extend(list2.to_vec());
                        let result = Term::list(combined);

                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        } else if result == *object {
                            return BuiltinResult::Success(bindings.clone());
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:remove - (list element) list:remove result (removes all occurrences of element)
        self.register(&format!("{}remove", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let Term::List(list) = &items[0] {
                        let element = &items[1];
                        let filtered: Vec<Term> = list.iter()
                            .filter(|item| *item != element)
                            .cloned()
                            .collect();
                        let result = Term::list(filtered);

                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        } else if result == *object {
                            return BuiltinResult::Success(bindings.clone());
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:unique - a list:unique b (removes duplicates, preserving order)
        self.register(&format!("{}unique", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let mut seen = std::collections::HashSet::new();
                let unique: Vec<Term> = list.iter()
                    .filter(|item| {
                        let key = format!("{:?}", item);
                        if seen.contains(&key) {
                            false
                        } else {
                            seen.insert(key);
                            true
                        }
                    })
                    .cloned()
                    .collect();
                let result = Term::list(unique);

                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if result == *object {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::NotReady
        });

        // list:sort - a list:sort b (sorts list)
        self.register(&format!("{}sort", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let mut sorted = list.to_vec();
                sorted.sort_by(|a, b| format!("{:?}", a).cmp(&format!("{:?}", b)));
                let result = Term::list(sorted);

                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if result == *object {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::NotReady
        });

        // list:reverse - a list:reverse b (reverses list)
        self.register(&format!("{}reverse", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let mut reversed = list.to_vec();
                reversed.reverse();
                let result = Term::list(reversed);

                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if result == *object {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::NotReady
        });

        // list:nth - (list index) list:nth element (0-indexed)
        self.register(&format!("{}nth", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let Term::List(list) = &items[0] {
                        if let Some(idx) = get_number(&items[1]) {
                            let idx = idx as usize;
                            if idx < list.len() {
                                let element = list.iter().nth(idx).unwrap().clone();
                                if let Term::Variable(var) = object {
                                    let mut new_bindings = bindings.clone();
                                    new_bindings.insert(var.clone(), element);
                                    return BuiltinResult::Success(new_bindings);
                                } else if element == *object {
                                    return BuiltinResult::Success(bindings.clone());
                                }
                            }
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:flat - nestedList list:flat flatList (flattens one level)
        self.register(&format!("{}flat", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let mut flattened = Vec::new();
                for item in list.iter() {
                    if let Term::List(inner) = item {
                        flattened.extend(inner.to_vec());
                    } else {
                        flattened.push(item.clone());
                    }
                }
                let result = Term::list(flattened);
                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if result == *object {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::NotReady
        });

        // list:iterate - list list:iterate (index element) pairs
        // Note: This is a generator-style builtin, returning list of pairs
        self.register(&format!("{}iterate", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let pairs: Vec<Term> = list.iter().enumerate()
                    .map(|(i, item)| {
                        Term::list(vec![
                            Term::typed_literal(i.to_string(), "http://www.w3.org/2001/XMLSchema#integer"),
                            item.clone()
                        ])
                    })
                    .collect();
                let result = Term::list(pairs);
                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if result == *object {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::NotReady
        });

        // list:setDifference - (list1 list2) list:setDifference result
        // Returns elements in list1 that are not in list2
        self.register(&format!("{}setDifference", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let (Term::List(list1), Term::List(list2)) = (&items[0], &items[1]) {
                        let set2: std::collections::HashSet<_> = list2.iter()
                            .map(|t| format!("{:?}", t))
                            .collect();
                        let difference: Vec<Term> = list1.iter()
                            .filter(|t| !set2.contains(&format!("{:?}", t)))
                            .cloned()
                            .collect();
                        let result = Term::list(difference);
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        } else if result == *object {
                            return BuiltinResult::Success(bindings.clone());
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:setIntersection - (list1 list2) list:setIntersection result
        // Returns elements common to both lists
        self.register(&format!("{}setIntersection", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let (Term::List(list1), Term::List(list2)) = (&items[0], &items[1]) {
                        let set2: std::collections::HashSet<_> = list2.iter()
                            .map(|t| format!("{:?}", t))
                            .collect();
                        let intersection: Vec<Term> = list1.iter()
                            .filter(|t| set2.contains(&format!("{:?}", t)))
                            .cloned()
                            .collect();
                        let result = Term::list(intersection);
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        } else if result == *object {
                            return BuiltinResult::Success(bindings.clone());
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:setUnion - (list1 list2) list:setUnion result
        // Returns combined unique elements from both lists
        self.register(&format!("{}setUnion", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let (Term::List(list1), Term::List(list2)) = (&items[0], &items[1]) {
                        let mut seen = std::collections::HashSet::new();
                        let mut union = Vec::new();
                        for item in list1.iter().chain(list2.iter()) {
                            let key = format!("{:?}", item);
                            if !seen.contains(&key) {
                                seen.insert(key);
                                union.push(item.clone());
                            }
                        }
                        let result = Term::list(union);
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        } else if result == *object {
                            return BuiltinResult::Success(bindings.clone());
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:take - (list count) list:take result (takes first N elements)
        self.register(&format!("{}take", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let Term::List(list) = &items[0] {
                        if let Some(count) = get_number(&items[1]) {
                            let count = count as usize;
                            let taken: Vec<Term> = list.iter().take(count).cloned().collect();
                            let result = Term::list(taken);
                            if let Term::Variable(var) = object {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), result);
                                return BuiltinResult::Success(new_bindings);
                            } else if result == *object {
                                return BuiltinResult::Success(bindings.clone());
                            }
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:drop - (list count) list:drop result (drops first N elements)
        self.register(&format!("{}drop", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let Term::List(list) = &items[0] {
                        if let Some(count) = get_number(&items[1]) {
                            let count = count as usize;
                            let dropped: Vec<Term> = list.iter().skip(count).cloned().collect();
                            let result = Term::list(dropped);
                            if let Term::Variable(var) = object {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), result);
                                return BuiltinResult::Success(new_bindings);
                            } else if result == *object {
                                return BuiltinResult::Success(bindings.clone());
                            }
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:slice - (list start end) list:slice result (extracts portion of list)
        self.register(&format!("{}slice", ns::LIST), |subject, object, bindings| {
            if let Term::List(triple) = subject {
                let items = triple.to_vec();
                if items.len() == 3 {
                    if let Term::List(list) = &items[0] {
                        if let (Some(start), Some(end)) = (get_number(&items[1]), get_number(&items[2])) {
                            let start = start as usize;
                            let end = end as usize;
                            let sliced: Vec<Term> = list.iter()
                                .skip(start)
                                .take(end.saturating_sub(start))
                                .cloned()
                                .collect();
                            let result = Term::list(sliced);
                            if let Term::Variable(var) = object {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), result);
                                return BuiltinResult::Success(new_bindings);
                            } else if result == *object {
                                return BuiltinResult::Success(bindings.clone());
                            }
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:zip - (list1 list2) list:zip listOfPairs
        self.register(&format!("{}zip", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let (Term::List(list1), Term::List(list2)) = (&items[0], &items[1]) {
                        let zipped: Vec<Term> = list1.iter().zip(list2.iter())
                            .map(|(a, b)| Term::list(vec![a.clone(), b.clone()]))
                            .collect();
                        let result = Term::list(zipped);
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        } else if result == *object {
                            return BuiltinResult::Success(bindings.clone());
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:stringToList - str list:stringToList listOfChars
        self.register(&format!("{}stringToList", ns::LIST), |subject, object, bindings| {
            if let Some(s) = get_string(subject) {
                let chars: Vec<Term> = s.chars()
                    .map(|c| Term::literal(c.to_string()))
                    .collect();
                let result = Term::list(chars);
                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if result == *object {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::NotReady
        });

        // list:listToString - listOfChars list:listToString str
        self.register(&format!("{}listToString", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let mut s = String::new();
                for item in list.iter() {
                    if let Some(ch) = get_string(item) {
                        s.push_str(&ch);
                    } else {
                        return BuiltinResult::NotReady;
                    }
                }
                return match_or_bind_string(object, s, bindings);
            }
            BuiltinResult::NotReady
        });

        // list:isEmpty - list list:isEmpty true (succeeds if list is empty)
        self.register(&format!("{}isEmpty", ns::LIST), |subject, _object, _bindings| {
            if let Term::List(list) = subject {
                if list.is_empty() {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // list:notEmpty - list list:notEmpty true (succeeds if list is not empty)
        self.register(&format!("{}notEmpty", ns::LIST), |subject, _object, _bindings| {
            if let Term::List(list) = subject {
                if !list.is_empty() {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // list:contains - (list element) list:contains true (succeeds if list contains element)
        self.register(&format!("{}contains", ns::LIST), |subject, _object, _bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let Term::List(list) = &items[0] {
                        let element = &items[1];
                        for item in list.iter() {
                            if item == element {
                                return BuiltinResult::Success(Bindings::default());
                            }
                        }
                    }
                }
            }
            BuiltinResult::Failure
        });

        // list:count - (list element) list:count count (counts occurrences of element)
        self.register(&format!("{}count", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let Term::List(list) = &items[0] {
                        let element = &items[1];
                        let count = list.iter().filter(|item| *item == element).count() as f64;
                        return match_or_bind(object, count, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:indexOf - (list element) list:indexOf index (-1 if not found)
        self.register(&format!("{}indexOf", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let Term::List(list) = &items[0] {
                        let element = &items[1];
                        let index = list.iter()
                            .position(|item| item == element)
                            .map(|i| i as f64)
                            .unwrap_or(-1.0);
                        return match_or_bind(object, index, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:range - (start end) list:range list (creates list of integers from start to end-1)
        self.register(&format!("{}range", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() >= 1 {
                    let (start, end) = if items.len() == 1 {
                        (0i64, get_number(&items[0]).map(|n| n as i64).unwrap_or(0))
                    } else {
                        (
                            get_number(&items[0]).map(|n| n as i64).unwrap_or(0),
                            get_number(&items[1]).map(|n| n as i64).unwrap_or(0)
                        )
                    };
                    let range: Vec<Term> = (start..end)
                        .map(|n| Term::typed_literal(n.to_string(), "http://www.w3.org/2001/XMLSchema#integer"))
                        .collect();
                    let result = Term::list(range);
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), result);
                        return BuiltinResult::Success(new_bindings);
                    } else if result == *object {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:butLast - list list:butLast result (all elements except last)
        self.register(&format!("{}butLast", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if !items.is_empty() {
                    let result = Term::list(items[..items.len()-1].to_vec());
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), result);
                        return BuiltinResult::Success(new_bindings);
                    } else if result == *object {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:cons - (head tail) list:cons list (constructs list from head and tail)
        self.register(&format!("{}cons", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    let head = items[0].clone();
                    if let Term::List(tail) = &items[1] {
                        let mut result_items = vec![head];
                        result_items.extend(tail.to_vec());
                        let result = Term::list(result_items);
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        } else if result == *object {
                            return BuiltinResult::Success(bindings.clone());
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:uncons - list list:uncons (head tail) (deconstructs list to head and tail)
        self.register(&format!("{}uncons", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if !items.is_empty() {
                    let head = items[0].clone();
                    let tail = Term::list(items[1..].to_vec());
                    let result = Term::list(vec![head, tail]);
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), result);
                        return BuiltinResult::Success(new_bindings);
                    } else if result == *object {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:rotateLeft - list list:rotateLeft result (first element moves to end)
        self.register(&format!("{}rotateLeft", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if !items.is_empty() {
                    let mut rotated = items[1..].to_vec();
                    rotated.push(items[0].clone());
                    let result = Term::list(rotated);
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), result);
                        return BuiltinResult::Success(new_bindings);
                    } else if result == *object {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:rotateRight - list list:rotateRight result (last element moves to front)
        self.register(&format!("{}rotateRight", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if !items.is_empty() {
                    let mut rotated = vec![items[items.len()-1].clone()];
                    rotated.extend(items[..items.len()-1].to_vec());
                    let result = Term::list(rotated);
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), result);
                        return BuiltinResult::Success(new_bindings);
                    } else if result == *object {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:insert - (list index element) list:insert result
        self.register(&format!("{}insert", ns::LIST), |subject, object, bindings| {
            if let Term::List(triple) = subject {
                let items = triple.to_vec();
                if items.len() == 3 {
                    if let Term::List(list) = &items[0] {
                        if let Some(idx) = get_number(&items[1]) {
                            let idx = idx as usize;
                            let element = items[2].clone();
                            let mut result_items = list.to_vec();
                            let insert_pos = idx.min(result_items.len());
                            result_items.insert(insert_pos, element);
                            let result = Term::list(result_items);
                            if let Term::Variable(var) = object {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), result);
                                return BuiltinResult::Success(new_bindings);
                            } else if result == *object {
                                return BuiltinResult::Success(bindings.clone());
                            }
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:removeAt - (list index) list:removeAt result
        self.register(&format!("{}removeAt", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let Term::List(list) = &items[0] {
                        if let Some(idx) = get_number(&items[1]) {
                            let idx = idx as usize;
                            let mut result_items = list.to_vec();
                            if idx < result_items.len() {
                                result_items.remove(idx);
                                let result = Term::list(result_items);
                                if let Term::Variable(var) = object {
                                    let mut new_bindings = bindings.clone();
                                    new_bindings.insert(var.clone(), result);
                                    return BuiltinResult::Success(new_bindings);
                                } else if result == *object {
                                    return BuiltinResult::Success(bindings.clone());
                                }
                            }
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // list:replaceAt - (list index element) list:replaceAt result
        self.register(&format!("{}replaceAt", ns::LIST), |subject, object, bindings| {
            if let Term::List(triple) = subject {
                let items = triple.to_vec();
                if items.len() == 3 {
                    if let Term::List(list) = &items[0] {
                        if let Some(idx) = get_number(&items[1]) {
                            let idx = idx as usize;
                            let element = items[2].clone();
                            let mut result_items = list.to_vec();
                            if idx < result_items.len() {
                                result_items[idx] = element;
                                let result = Term::list(result_items);
                                if let Term::Variable(var) = object {
                                    let mut new_bindings = bindings.clone();
                                    new_bindings.insert(var.clone(), result);
                                    return BuiltinResult::Success(new_bindings);
                                } else if result == *object {
                                    return BuiltinResult::Success(bindings.clone());
                                }
                            }
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });
    }

    fn register_time(&mut self) {
        // time:inSeconds - dateTimeString time:inSeconds secondsSinceEpoch
        self.register(&format!("{}inSeconds", ns::TIME), |subject, object, bindings| {
            if let Some(dt_str) = get_string(subject) {
                // Try to parse ISO 8601 datetime
                if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                    let seconds = dt.timestamp() as f64;
                    return match_or_bind(object, seconds, bindings);
                }
                // Try parsing as Unix timestamp
                if let Ok(secs) = dt_str.parse::<f64>() {
                    return match_or_bind(object, secs, bindings);
                }
            }
            // Reverse: seconds to datetime string
            if let Some(secs) = get_number(object) {
                if let Term::Variable(var) = subject {
                    let dt = Utc.timestamp_opt(secs as i64, 0).single();
                    if let Some(dt) = dt {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), Term::literal(dt.to_rfc3339()));
                        return BuiltinResult::Success(new_bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // time:year - dateTimeString time:year year
        self.register(&format!("{}year", ns::TIME), |subject, object, bindings| {
            if let Some(dt_str) = get_string(subject) {
                if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                    return match_or_bind(object, dt.year() as f64, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // time:month - dateTimeString time:month month (1-12)
        self.register(&format!("{}month", ns::TIME), |subject, object, bindings| {
            if let Some(dt_str) = get_string(subject) {
                if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                    return match_or_bind(object, dt.month() as f64, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // time:day - dateTimeString time:day day (1-31)
        self.register(&format!("{}day", ns::TIME), |subject, object, bindings| {
            if let Some(dt_str) = get_string(subject) {
                if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                    return match_or_bind(object, dt.day() as f64, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // time:hour - dateTimeString time:hour hour (0-23)
        self.register(&format!("{}hour", ns::TIME), |subject, object, bindings| {
            if let Some(dt_str) = get_string(subject) {
                if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                    return match_or_bind(object, dt.hour() as f64, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // time:minute - dateTimeString time:minute minute (0-59)
        self.register(&format!("{}minute", ns::TIME), |subject, object, bindings| {
            if let Some(dt_str) = get_string(subject) {
                if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                    return match_or_bind(object, dt.minute() as f64, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // time:second - dateTimeString time:second second (0-59)
        self.register(&format!("{}second", ns::TIME), |subject, object, bindings| {
            if let Some(dt_str) = get_string(subject) {
                if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                    return match_or_bind(object, dt.second() as f64, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // time:dayOfWeek - dateTimeString time:dayOfWeek dayNum (0=Sunday, 6=Saturday)
        self.register(&format!("{}dayOfWeek", ns::TIME), |subject, object, bindings| {
            if let Some(dt_str) = get_string(subject) {
                if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                    // chrono uses Mon=0, Sun=6, but cwm uses Sun=0, Sat=6
                    let dow = dt.weekday().num_days_from_sunday() as f64;
                    return match_or_bind(object, dow, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // time:timeZone - dateTimeString time:timeZone tzOffsetMinutes
        self.register(&format!("{}timeZone", ns::TIME), |subject, object, bindings| {
            if let Some(dt_str) = get_string(subject) {
                if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                    let offset_secs = dt.offset().local_minus_utc();
                    let offset_mins = (offset_secs / 60) as f64;
                    return match_or_bind(object, offset_mins, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // time:gmTime - secondsSinceEpoch time:gmTime dateTimeString (UTC)
        self.register(&format!("{}gmTime", ns::TIME), |subject, object, bindings| {
            if let Some(secs) = get_number(subject) {
                if let Some(dt) = Utc.timestamp_opt(secs as i64, 0).single() {
                    return match_or_bind_string(object, dt.to_rfc3339(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // time:localTime - secondsSinceEpoch time:localTime dateTimeString (local timezone)
        self.register(&format!("{}localTime", ns::TIME), |subject, object, bindings| {
            if let Some(secs) = get_number(subject) {
                if let Some(dt) = Local.timestamp_opt(secs as i64, 0).single() {
                    return match_or_bind_string(object, dt.to_rfc3339(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // time:now - _ time:now currentDateTime (current time as RFC3339 string)
        self.register(&format!("{}now", ns::TIME), |_subject, object, bindings| {
            let now = Utc::now();
            match_or_bind_string(object, now.to_rfc3339(), bindings)
        });

        // time:nowSeconds - _ time:nowSeconds secondsSinceEpoch
        self.register(&format!("{}nowSeconds", ns::TIME), |_subject, object, bindings| {
            use std::time::{SystemTime, UNIX_EPOCH};
            let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
            match_or_bind(object, now.as_secs_f64(), bindings)
        });

        // time:dayOfYear - dateTimeString time:dayOfYear dayNum (1-366)
        self.register(&format!("{}dayOfYear", ns::TIME), |subject, object, bindings| {
            if let Some(dt_str) = get_string(subject) {
                if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                    return match_or_bind(object, dt.ordinal() as f64, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // time:weekOfYear - dateTimeString time:weekOfYear weekNum
        self.register(&format!("{}weekOfYear", ns::TIME), |subject, object, bindings| {
            if let Some(dt_str) = get_string(subject) {
                if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                    let week = dt.iso_week().week() as f64;
                    return match_or_bind(object, week, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // time:isLeapYear - dateTimeString time:isLeapYear true (succeeds if leap year)
        self.register(&format!("{}isLeapYear", ns::TIME), |subject, _object, _bindings| {
            if let Some(dt_str) = get_string(subject) {
                if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                    let year = dt.year();
                    let is_leap = (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
                    if is_leap {
                        return BuiltinResult::Success(Bindings::default());
                    }
                }
            }
            BuiltinResult::Failure
        });

        // time:format - (datetime formatString) time:format formattedString
        self.register(&format!("{}format", ns::TIME), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(dt_str), Some(fmt)) = (get_string(&items[0]), get_string(&items[1])) {
                        if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                            // Simple format string replacement
                            let result = fmt
                                .replace("%Y", &dt.year().to_string())
                                .replace("%m", &format!("{:02}", dt.month()))
                                .replace("%d", &format!("{:02}", dt.day()))
                                .replace("%H", &format!("{:02}", dt.hour()))
                                .replace("%M", &format!("{:02}", dt.minute()))
                                .replace("%S", &format!("{:02}", dt.second()));
                            return match_or_bind_string(object, result, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // time:addSeconds - (datetime seconds) time:addSeconds newDatetime
        self.register(&format!("{}addSeconds", ns::TIME), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(dt_str), Some(secs)) = (get_string(&items[0]), get_number(&items[1])) {
                        if let Ok(dt) = DateTime::parse_from_rfc3339(&dt_str) {
                            let new_dt = dt + chrono::Duration::seconds(secs as i64);
                            return match_or_bind_string(object, new_dt.to_rfc3339(), bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // time:diffSeconds - (datetime1 datetime2) time:diffSeconds seconds
        self.register(&format!("{}diffSeconds", ns::TIME), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(dt1_str), Some(dt2_str)) = (get_string(&items[0]), get_string(&items[1])) {
                        if let (Ok(dt1), Ok(dt2)) = (DateTime::parse_from_rfc3339(&dt1_str), DateTime::parse_from_rfc3339(&dt2_str)) {
                            let diff = (dt1 - dt2).num_seconds() as f64;
                            return match_or_bind(object, diff, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // time:isBefore - (datetime1 datetime2) time:isBefore true
        self.register(&format!("{}isBefore", ns::TIME), |subject, _object, _bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(dt1_str), Some(dt2_str)) = (get_string(&items[0]), get_string(&items[1])) {
                        if let (Ok(dt1), Ok(dt2)) = (DateTime::parse_from_rfc3339(&dt1_str), DateTime::parse_from_rfc3339(&dt2_str)) {
                            if dt1 < dt2 {
                                return BuiltinResult::Success(Bindings::default());
                            }
                        }
                    }
                }
            }
            BuiltinResult::Failure
        });

        // time:isAfter - (datetime1 datetime2) time:isAfter true
        self.register(&format!("{}isAfter", ns::TIME), |subject, _object, _bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(dt1_str), Some(dt2_str)) = (get_string(&items[0]), get_string(&items[1])) {
                        if let (Ok(dt1), Ok(dt2)) = (DateTime::parse_from_rfc3339(&dt1_str), DateTime::parse_from_rfc3339(&dt2_str)) {
                            if dt1 > dt2 {
                                return BuiltinResult::Success(Bindings::default());
                            }
                        }
                    }
                }
            }
            BuiltinResult::Failure
        });
    }

    fn register_crypto(&mut self) {
        type HmacSha256 = Hmac<Sha256>;

        // crypto:md5 - data crypto:md5 hash
        self.register(&format!("{}md5", ns::CRYPTO), |subject, object, bindings| {
            if let Some(data) = get_string(subject) {
                let mut hasher = Md5::new();
                hasher.update(data.as_bytes());
                let result = hasher.finalize();
                let hex = format!("{:x}", result);
                return match_or_bind_string(object, hex, bindings);
            }
            BuiltinResult::NotReady
        });

        // crypto:sha - data crypto:sha hash (SHA-256)
        self.register(&format!("{}sha", ns::CRYPTO), |subject, object, bindings| {
            if let Some(data) = get_string(subject) {
                let mut hasher = Sha256::new();
                hasher.update(data.as_bytes());
                let result = hasher.finalize();
                let hex = format!("{:x}", result);
                return match_or_bind_string(object, hex, bindings);
            }
            BuiltinResult::NotReady
        });

        // crypto:sha1 - data crypto:sha1 hash
        self.register(&format!("{}sha1", ns::CRYPTO), |subject, object, bindings| {
            if let Some(data) = get_string(subject) {
                let mut hasher = Sha1::new();
                hasher.update(data.as_bytes());
                let result = hasher.finalize();
                let hex = format!("{:x}", result);
                return match_or_bind_string(object, hex, bindings);
            }
            BuiltinResult::NotReady
        });

        // crypto:sha512 - data crypto:sha512 hash
        self.register(&format!("{}sha512", ns::CRYPTO), |subject, object, bindings| {
            if let Some(data) = get_string(subject) {
                let mut hasher = Sha512::new();
                hasher.update(data.as_bytes());
                let result = hasher.finalize();
                let hex = format!("{:x}", result);
                return match_or_bind_string(object, hex, bindings);
            }
            BuiltinResult::NotReady
        });

        // crypto:sign - (data key) crypto:sign signature (HMAC-SHA256)
        self.register(&format!("{}sign", ns::CRYPTO), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(data), Some(key)) = (get_string(&items[0]), get_string(&items[1])) {
                        if let Ok(mut mac) = HmacSha256::new_from_slice(key.as_bytes()) {
                            mac.update(data.as_bytes());
                            let result = mac.finalize();
                            let signature = general_purpose::STANDARD.encode(result.into_bytes());
                            return match_or_bind_string(object, signature, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // crypto:verify - ((data key) signature) crypto:verify bool
        // Returns "1" if valid, "0" if invalid
        self.register(&format!("{}verify", ns::CRYPTO), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Term::List(data_key), Some(signature)) = (&items[0], get_string(&items[1])) {
                        let dk_items = data_key.to_vec();
                        if dk_items.len() == 2 {
                            if let (Some(data), Some(key)) = (get_string(&dk_items[0]), get_string(&dk_items[1])) {
                                if let Ok(sig_bytes) = general_purpose::STANDARD.decode(&signature) {
                                    if let Ok(mut mac) = HmacSha256::new_from_slice(key.as_bytes()) {
                                        mac.update(data.as_bytes());
                                        let valid = mac.verify_slice(&sig_bytes).is_ok();
                                        let result = if valid { "1" } else { "0" };
                                        return match_or_bind_string(object, result.to_string(), bindings);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // crypto:verifyBoolean - same as verify but succeeds/fails
        self.register(&format!("{}verifyBoolean", ns::CRYPTO), |subject, _object, _bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Term::List(data_key), Some(signature)) = (&items[0], get_string(&items[1])) {
                        let dk_items = data_key.to_vec();
                        if dk_items.len() == 2 {
                            if let (Some(data), Some(key)) = (get_string(&dk_items[0]), get_string(&dk_items[1])) {
                                if let Ok(sig_bytes) = general_purpose::STANDARD.decode(&signature) {
                                    if let Ok(mut mac) = HmacSha256::new_from_slice(key.as_bytes()) {
                                        mac.update(data.as_bytes());
                                        if mac.verify_slice(&sig_bytes).is_ok() {
                                            return BuiltinResult::Success(Bindings::default());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            BuiltinResult::Failure
        });

        // crypto:base64Encode - data crypto:base64Encode encoded
        self.register(&format!("{}base64Encode", ns::CRYPTO), |subject, object, bindings| {
            if let Some(data) = get_string(subject) {
                let encoded = general_purpose::STANDARD.encode(data.as_bytes());
                return match_or_bind_string(object, encoded, bindings);
            }
            BuiltinResult::NotReady
        });

        // crypto:base64Decode - encoded crypto:base64Decode data
        self.register(&format!("{}base64Decode", ns::CRYPTO), |subject, object, bindings| {
            if let Some(encoded) = get_string(subject) {
                if let Ok(decoded) = general_purpose::STANDARD.decode(&encoded) {
                    if let Ok(text) = String::from_utf8(decoded) {
                        return match_or_bind_string(object, text, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // crypto:hashFunction - algorithm crypto:hashFunction true (tests if secure hash)
        self.register(&format!("{}hashFunction", ns::CRYPTO), |subject, _object, _bindings| {
            if let Some(algo) = get_string(subject) {
                let algo_lower = algo.to_lowercase();
                let is_secure = matches!(algo_lower.as_str(),
                    "sha256" | "sha-256" | "sha512" | "sha-512" | "sha384" | "sha-384" |
                    "sha3-256" | "sha3-384" | "sha3-512" | "blake2b" | "blake2s"
                );
                if is_secure {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // crypto:keyLength - key crypto:keyLength length (returns key length in bits)
        self.register(&format!("{}keyLength", ns::CRYPTO), |subject, object, bindings| {
            if let Some(key) = get_string(subject) {
                // Assume hex-encoded key, each char = 4 bits
                let length = if key.chars().all(|c| c.is_ascii_hexdigit()) {
                    (key.len() * 4) as f64
                } else {
                    // Raw bytes, each byte = 8 bits
                    (key.len() * 8) as f64
                };
                return match_or_bind(object, length, bindings);
            }
            BuiltinResult::NotReady
        });

        // crypto:publicKeyObject - term crypto:publicKeyObject true (tests if term is a key)
        // For simplicity, test if it looks like a hex string of appropriate length
        self.register(&format!("{}publicKeyObject", ns::CRYPTO), |subject, _object, _bindings| {
            if let Some(key) = get_string(subject) {
                // RSA 2048 public key is ~512 hex chars, ECDSA P-256 is ~128 hex chars
                if key.len() >= 64 && key.chars().all(|c| c.is_ascii_hexdigit()) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // crypto:canSign - key crypto:canSign true (tests if key can sign)
        // Private keys can sign; detect by checking if key material is longer
        self.register(&format!("{}canSign", ns::CRYPTO), |subject, _object, _bindings| {
            if let Some(key) = get_string(subject) {
                // Private keys are typically longer than public keys
                // RSA 2048 private key is ~2000+ hex chars
                if key.len() >= 1024 && key.chars().all(|c| c.is_ascii_hexdigit()) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // crypto:canEncrypt - key crypto:canEncrypt true (tests if key can encrypt)
        // Public keys can encrypt; any valid key material can encrypt
        self.register(&format!("{}canEncrypt", ns::CRYPTO), |subject, _object, _bindings| {
            if let Some(key) = get_string(subject) {
                if key.len() >= 64 && key.chars().all(|c| c.is_ascii_hexdigit()) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // crypto:hasPrivate - keypair crypto:hasPrivate true (tests if keypair has private component)
        self.register(&format!("{}hasPrivate", ns::CRYPTO), |subject, _object, _bindings| {
            if let Some(key) = get_string(subject) {
                // Private key material is typically longer
                if key.len() >= 1024 && key.chars().all(|c| c.is_ascii_hexdigit()) {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // crypto:hexEncode - data crypto:hexEncode hexString
        self.register(&format!("{}hexEncode", ns::CRYPTO), |subject, object, bindings| {
            if let Some(data) = get_string(subject) {
                let hex: String = data.bytes().map(|b| format!("{:02x}", b)).collect();
                return match_or_bind_string(object, hex, bindings);
            }
            BuiltinResult::NotReady
        });

        // crypto:hexDecode - hexString crypto:hexDecode data
        self.register(&format!("{}hexDecode", ns::CRYPTO), |subject, object, bindings| {
            if let Some(hex) = get_string(subject) {
                let bytes: Result<Vec<u8>, _> = (0..hex.len())
                    .step_by(2)
                    .map(|i| u8::from_str_radix(&hex[i..i+2], 16))
                    .collect();
                if let Ok(bytes) = bytes {
                    if let Ok(text) = String::from_utf8(bytes) {
                        return match_or_bind_string(object, text, bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // crypto:randomBytes - count crypto:randomBytes hexBytes
        self.register(&format!("{}randomBytes", ns::CRYPTO), |subject, object, bindings| {
            if let Some(count) = get_number(subject) {
                use std::time::{SystemTime, UNIX_EPOCH};
                let count = count as usize;
                // Simple pseudo-random (not cryptographically secure in this impl)
                let seed = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
                let mut bytes = Vec::with_capacity(count);
                let mut state = seed;
                for _ in 0..count {
                    state = state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
                    bytes.push((state >> 32) as u8);
                }
                let hex: String = bytes.iter().map(|b| format!("{:02x}", b)).collect();
                return match_or_bind_string(object, hex, bindings);
            }
            BuiltinResult::NotReady
        });
    }

    fn register_os(&mut self) {
        // os:environ - envVarName os:environ value
        self.register(&format!("{}environ", ns::OS), |subject, object, bindings| {
            if let Some(var_name) = get_string(subject) {
                if let Ok(value) = std::env::var(&var_name) {
                    return match_or_bind_string(object, value, bindings);
                }
            }
            BuiltinResult::Failure
        });

        // os:argv - index os:argv argumentValue
        self.register(&format!("{}argv", ns::OS), |subject, object, bindings| {
            if let Some(idx) = get_number(subject) {
                let args: Vec<String> = std::env::args().collect();
                let idx = idx as usize;
                if idx < args.len() {
                    return match_or_bind_string(object, args[idx].clone(), bindings);
                }
            }
            BuiltinResult::Failure
        });

        // os:baseAbsolute - relativePath os:baseAbsolute absolutePath
        self.register(&format!("{}baseAbsolute", ns::OS), |subject, object, bindings| {
            if let Some(rel_path) = get_string(subject) {
                if let Ok(abs_path) = std::fs::canonicalize(&rel_path) {
                    if let Some(path_str) = abs_path.to_str() {
                        return match_or_bind_string(object, path_str.to_string(), bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // os:baseRelative - absolutePath os:baseRelative relativePath
        self.register(&format!("{}baseRelative", ns::OS), |subject, object, bindings| {
            if let Some(abs_path) = get_string(subject) {
                if let Ok(cwd) = std::env::current_dir() {
                    let abs = std::path::Path::new(&abs_path);
                    if let Ok(rel) = abs.strip_prefix(&cwd) {
                        if let Some(rel_str) = rel.to_str() {
                            return match_or_bind_string(object, rel_str.to_string(), bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // os:cwd - _ os:cwd currentWorkingDirectory
        self.register(&format!("{}cwd", ns::OS), |_subject, object, bindings| {
            if let Ok(cwd) = std::env::current_dir() {
                if let Some(cwd_str) = cwd.to_str() {
                    return match_or_bind_string(object, cwd_str.to_string(), bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // os:hostname - _ os:hostname hostname
        self.register(&format!("{}hostname", ns::OS), |_subject, object, bindings| {
            if let Ok(hostname) = std::env::var("HOSTNAME").or_else(|_| std::env::var("HOST")) {
                return match_or_bind_string(object, hostname, bindings);
            }
            // Fallback: try to get from file (Unix)
            if let Ok(hostname) = std::fs::read_to_string("/etc/hostname") {
                return match_or_bind_string(object, hostname.trim().to_string(), bindings);
            }
            BuiltinResult::NotReady
        });

        // os:platform - _ os:platform platformName
        self.register(&format!("{}platform", ns::OS), |_subject, object, bindings| {
            let platform = std::env::consts::OS;
            match_or_bind_string(object, platform.to_string(), bindings)
        });

        // os:arch - _ os:arch architecture
        self.register(&format!("{}arch", ns::OS), |_subject, object, bindings| {
            let arch = std::env::consts::ARCH;
            match_or_bind_string(object, arch.to_string(), bindings)
        });

        // os:exists - path os:exists true (succeeds if file/dir exists)
        self.register(&format!("{}exists", ns::OS), |subject, _object, _bindings| {
            if let Some(path) = get_string(subject) {
                if std::path::Path::new(&path).exists() {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // os:isFile - path os:isFile true (succeeds if path is a file)
        self.register(&format!("{}isFile", ns::OS), |subject, _object, _bindings| {
            if let Some(path) = get_string(subject) {
                if std::path::Path::new(&path).is_file() {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // os:isDir - path os:isDir true (succeeds if path is a directory)
        self.register(&format!("{}isDir", ns::OS), |subject, _object, _bindings| {
            if let Some(path) = get_string(subject) {
                if std::path::Path::new(&path).is_dir() {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // os:fileSize - path os:fileSize sizeInBytes
        self.register(&format!("{}fileSize", ns::OS), |subject, object, bindings| {
            if let Some(path) = get_string(subject) {
                if let Ok(metadata) = std::fs::metadata(&path) {
                    return match_or_bind(object, metadata.len() as f64, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // os:fileModTime - path os:fileModTime secondsSinceEpoch
        self.register(&format!("{}fileModTime", ns::OS), |subject, object, bindings| {
            if let Some(path) = get_string(subject) {
                if let Ok(metadata) = std::fs::metadata(&path) {
                    if let Ok(modified) = metadata.modified() {
                        if let Ok(duration) = modified.duration_since(std::time::UNIX_EPOCH) {
                            return match_or_bind(object, duration.as_secs_f64(), bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // os:dirContents - path os:dirContents listOfFilenames
        self.register(&format!("{}dirContents", ns::OS), |subject, object, bindings| {
            if let Some(path) = get_string(subject) {
                if let Ok(entries) = std::fs::read_dir(&path) {
                    let files: Vec<Term> = entries
                        .filter_map(|e| e.ok())
                        .filter_map(|e| e.file_name().into_string().ok())
                        .map(|name| Term::literal(name))
                        .collect();
                    let result = Term::list(files);
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), result);
                        return BuiltinResult::Success(new_bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // os:tempDir - _ os:tempDir tempDirectoryPath
        self.register(&format!("{}tempDir", ns::OS), |_subject, object, bindings| {
            let temp_dir = std::env::temp_dir();
            if let Some(temp_str) = temp_dir.to_str() {
                return match_or_bind_string(object, temp_str.to_string(), bindings);
            }
            BuiltinResult::NotReady
        });

        // os:homeDir - _ os:homeDir homeDirectoryPath
        self.register(&format!("{}homeDir", ns::OS), |_subject, object, bindings| {
            if let Ok(home) = std::env::var("HOME").or_else(|_| std::env::var("USERPROFILE")) {
                return match_or_bind_string(object, home, bindings);
            }
            BuiltinResult::NotReady
        });

        // os:joinPath - (path1 path2) os:joinPath combinedPath
        self.register(&format!("{}joinPath", ns::OS), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(p1), Some(p2)) = (get_string(&items[0]), get_string(&items[1])) {
                        let joined = std::path::Path::new(&p1).join(&p2);
                        if let Some(joined_str) = joined.to_str() {
                            return match_or_bind_string(object, joined_str.to_string(), bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // os:dirname - path os:dirname directoryName
        self.register(&format!("{}dirname", ns::OS), |subject, object, bindings| {
            if let Some(path) = get_string(subject) {
                if let Some(parent) = std::path::Path::new(&path).parent() {
                    if let Some(parent_str) = parent.to_str() {
                        return match_or_bind_string(object, parent_str.to_string(), bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // os:basename - path os:basename filename
        self.register(&format!("{}basename", ns::OS), |subject, object, bindings| {
            if let Some(path) = get_string(subject) {
                if let Some(name) = std::path::Path::new(&path).file_name() {
                    if let Some(name_str) = name.to_str() {
                        return match_or_bind_string(object, name_str.to_string(), bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // os:extension - path os:extension extension
        self.register(&format!("{}extension", ns::OS), |subject, object, bindings| {
            if let Some(path) = get_string(subject) {
                if let Some(ext) = std::path::Path::new(&path).extension() {
                    if let Some(ext_str) = ext.to_str() {
                        return match_or_bind_string(object, ext_str.to_string(), bindings);
                    }
                }
            }
            BuiltinResult::NotReady
        });
    }

    fn register_graph(&mut self) {
        // graph:difference - (graph1 graph2) graph:difference result
        // Returns triples in graph1 that are not in graph2
        self.register(&format!("{}difference", ns::GRAPH), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let (Term::Formula(g1), Term::Formula(g2)) = (&items[0], &items[1]) {
                        let set2: std::collections::HashSet<_> = g2.triples().iter()
                            .map(|t| format!("{:?}", t))
                            .collect();
                        let diff: Vec<crate::term::Triple> = g1.triples().iter()
                            .filter(|t| !set2.contains(&format!("{:?}", t)))
                            .cloned()
                            .collect();
                        let result = Term::Formula(crate::term::FormulaRef::new(0, diff));
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // graph:union - (graph1 graph2) graph:union result
        // Returns union of triples from both graphs
        self.register(&format!("{}union", ns::GRAPH), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let (Term::Formula(g1), Term::Formula(g2)) = (&items[0], &items[1]) {
                        let mut all_triples: Vec<crate::term::Triple> = g1.triples().to_vec();
                        let set1: std::collections::HashSet<_> = g1.triples().iter()
                            .map(|t| format!("{:?}", t))
                            .collect();
                        for triple in g2.triples() {
                            if !set1.contains(&format!("{:?}", triple)) {
                                all_triples.push(triple.clone());
                            }
                        }
                        let result = Term::Formula(crate::term::FormulaRef::new(0, all_triples));
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // graph:intersection - (graph1 graph2) graph:intersection result
        // Returns triples common to both graphs
        self.register(&format!("{}intersection", ns::GRAPH), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let (Term::Formula(g1), Term::Formula(g2)) = (&items[0], &items[1]) {
                        let set2: std::collections::HashSet<_> = g2.triples().iter()
                            .map(|t| format!("{:?}", t))
                            .collect();
                        let intersection: Vec<crate::term::Triple> = g1.triples().iter()
                            .filter(|t| set2.contains(&format!("{:?}", t)))
                            .cloned()
                            .collect();
                        let result = Term::Formula(crate::term::FormulaRef::new(0, intersection));
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // graph:length - graph graph:length count
        // Returns number of triples in graph
        self.register(&format!("{}length", ns::GRAPH), |subject, object, bindings| {
            if let Term::Formula(formula) = subject {
                let count = formula.triples().len() as f64;
                return match_or_bind(object, count, bindings);
            }
            BuiltinResult::NotReady
        });

        // graph:member - graph graph:member triple
        // Generates all triples in the graph (or checks membership)
        self.register(&format!("{}member", ns::GRAPH), |subject, object, bindings| {
            if let Term::Formula(formula) = subject {
                // If object is a variable, return first triple
                // Note: full implementation would need backtracking
                if let Term::Variable(var) = object {
                    if let Some(triple) = formula.triples().first() {
                        // Return triple as a list (subject, predicate, object)
                        let triple_list = Term::list(vec![
                            triple.subject.clone(),
                            triple.predicate.clone(),
                            triple.object.clone(),
                        ]);
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), triple_list);
                        return BuiltinResult::Success(new_bindings);
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
