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
use md5::Md5;
use crate::term::{Term, Bindings, uri::ns};

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
        // Note: This is a simplified implementation that returns empty formula
        // Full implementation would require parser integration
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

                if let Some(_content) = content {
                    // Return an empty formula as placeholder
                    // Full implementation would parse the content
                    let result = Term::Formula(crate::term::FormulaRef::new(0, Vec::new()));
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), result);
                        return BuiltinResult::Success(new_bindings);
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
    }

    fn register_crypto(&mut self) {
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
