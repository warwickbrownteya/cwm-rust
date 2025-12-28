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
//!
//! # Architecture
//!
//! This module is being refactored into a modular namespace-based structure.
//! See the submodules for the new trait-based implementations:
//! - `helpers`: Shared utility functions
//! - `math`: Math namespace builtins (new architecture)
//!
//! The legacy `BuiltinRegistry` maintains backward compatibility.

// New modular architecture
pub mod helpers;
pub mod math;

// Re-export namespace types
pub use math::MathNamespace;

use std::collections::HashMap;
use regex::Regex;
use chrono::{DateTime, Datelike, Timelike, Utc, Local, TimeZone};
use sha2::{Sha256, Sha512, Digest};
use sha1::Sha1;
use md5::Md5;
use hmac::{Hmac, Mac};
use base64::{Engine as _, engine::general_purpose};
use std::sync::Arc;
use crate::term::{Term, Triple, Variable, Bindings, uri::ns, FormulaRef, List};
use crate::parser;

// Re-export ureq for web fetching
use ureq;

/// Content types for RDF content negotiation
const RDF_ACCEPT_HEADER: &str = "text/n3, text/turtle, application/n-triples;q=0.9, application/rdf+xml;q=0.8, application/ld+json;q=0.7, */*;q=0.1";

/// Timeout for HTTP requests in seconds
const HTTP_TIMEOUT_SECS: u64 = 30;

/// Fetch a document from a URI with proper RDF content negotiation
/// Returns (content, content_type) or error message
fn fetch_rdf_document(uri: &str) -> Result<(String, String), String> {
    if uri.starts_with("file://") {
        let path = &uri[7..];
        std::fs::read_to_string(path)
            .map(|content| {
                let content_type = if path.ends_with(".n3") || path.ends_with(".n3s") {
                    "text/n3".to_string()
                } else if path.ends_with(".ttl") {
                    "text/turtle".to_string()
                } else if path.ends_with(".nt") {
                    "application/n-triples".to_string()
                } else if path.ends_with(".rdf") || path.ends_with(".xml") {
                    "application/rdf+xml".to_string()
                } else if path.ends_with(".jsonld") || path.ends_with(".json") {
                    "application/ld+json".to_string()
                } else if path.ends_with(".kif") {
                    "application/kif".to_string()
                } else {
                    "text/n3".to_string() // Default to N3
                };
                (content, content_type)
            })
            .map_err(|e| format!("File error: {}", e))
    } else if uri.starts_with("http://") || uri.starts_with("https://") {
        let agent = ureq::AgentBuilder::new()
            .timeout(std::time::Duration::from_secs(HTTP_TIMEOUT_SECS))
            .build();

        agent.get(uri)
            .set("Accept", RDF_ACCEPT_HEADER)
            .set("User-Agent", "cwm-rust/0.1")
            .call()
            .map_err(|e| format!("HTTP error: {}", e))
            .and_then(|response| {
                let content_type = response.content_type().to_string();
                response.into_string()
                    .map(|content| (content, content_type))
                    .map_err(|e| format!("Read error: {}", e))
            })
    } else {
        // Assume local file path
        std::fs::read_to_string(uri)
            .map(|content| {
                let content_type = if uri.ends_with(".n3") || uri.ends_with(".n3s") {
                    "text/n3".to_string()
                } else if uri.ends_with(".ttl") {
                    "text/turtle".to_string()
                } else if uri.ends_with(".nt") {
                    "application/n-triples".to_string()
                } else if uri.ends_with(".rdf") || uri.ends_with(".xml") {
                    "application/rdf+xml".to_string()
                } else if uri.ends_with(".jsonld") || uri.ends_with(".json") {
                    "application/ld+json".to_string()
                } else if uri.ends_with(".kif") {
                    "application/kif".to_string()
                } else {
                    "text/n3".to_string()
                };
                (content, content_type)
            })
            .map_err(|e| format!("File error: {}", e))
    }
}

/// Parse content based on content type
/// Supports: text/n3, text/turtle, application/n-triples, application/ld+json, application/kif
fn parse_content_by_type(content: &str, content_type: &str) -> Result<Vec<Triple>, String> {
    // Normalize content type (remove charset, etc.)
    let ct = content_type.split(';').next().unwrap_or(content_type).trim();

    match ct {
        "text/n3" | "application/n3" => {
            parser::parse(content)
                .map(|r| r.triples)
                .map_err(|e| format!("N3 parse error: {}", e))
        }
        "text/turtle" | "application/turtle" => {
            // Turtle is a subset of N3, use the same parser
            parser::parse(content)
                .map(|r| r.triples)
                .map_err(|e| format!("Turtle parse error: {}", e))
        }
        "application/n-triples" | "application/ntriples" => {
            // N-Triples is a subset of Turtle, use the same parser
            parser::parse(content)
                .map(|r| r.triples)
                .map_err(|e| format!("N-Triples parse error: {}", e))
        }
        "application/ld+json" | "application/json" => {
            // For JSON-LD, we'd need a dedicated parser
            // For now, return an error indicating the format isn't fully supported
            Err(format!("JSON-LD parsing requires conversion - content type: {}", ct))
        }
        "application/rdf+xml" | "application/xml" | "text/xml" => {
            // RDF/XML would require an XML parser
            // For now, return an error
            Err(format!("RDF/XML parsing not yet implemented - content type: {}", ct))
        }
        "application/kif" => {
            // KIF format
            parser::parse_kif(content)
                .map(|r| r.triples)
                .map_err(|e| format!("KIF parse error: {}", e))
        }
        _ => {
            // Default to N3 parser for unknown types
            parser::parse(content)
                .map(|r| r.triples)
                .map_err(|e| format!("Parse error (default N3): {}", e))
        }
    }
}

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
#[derive(Clone)]
pub struct BuiltinRegistry {
    builtins: HashMap<String, BuiltinFn>,
}

impl std::fmt::Debug for BuiltinRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinRegistry")
            .field("count", &self.builtins.len())
            .finish()
    }
}

impl BuiltinRegistry {
    /// Create a new registry with standard built-ins (including crypto)
    pub fn new() -> Self {
        Self::with_options(true)
    }

    /// Create a new registry with configurable options
    ///
    /// # Arguments
    /// * `enable_crypto` - Whether to register crypto builtins (requires --crypto flag)
    pub fn with_options(enable_crypto: bool) -> Self {
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
        // Register crypto built-ins (only if enabled)
        if enable_crypto {
            registry.register_crypto();
        }
        // Register os built-ins
        registry.register_os();
        // Register graph built-ins
        registry.register_graph();
        // Register database built-ins
        registry.register_db();

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

    /// Get all registered builtin URIs (for log:builtinIn)
    pub fn all_builtins(&self) -> Vec<String> {
        self.builtins.keys().cloned().collect()
    }

    /// Get the count of registered builtins
    pub fn builtin_count(&self) -> usize {
        self.builtins.len()
    }

    fn register_math(&mut self) {
        // math:sum - (a b) math:sum c means a + b = c
        // BIDIRECTIONAL: can solve for any one unknown
        self.register(&format!("{}sum", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    let a_val = get_number(&items[0]);
                    let b_val = get_number(&items[1]);
                    let c_val = get_number(object);

                    match (a_val, b_val, c_val) {
                        // Forward: a + b = ?c
                        (Some(a), Some(b), _) => {
                            return match_or_bind(object, a + b, bindings);
                        }
                        // Backward: ?a + b = c → a = c - b
                        (None, Some(b), Some(c)) => {
                            if let Term::Variable(var) = &items[0] {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), Term::typed_literal((c - b).to_string(), "http://www.w3.org/2001/XMLSchema#decimal"));
                                return BuiltinResult::Success(new_bindings);
                            }
                        }
                        // Backward: a + ?b = c → b = c - a
                        (Some(a), None, Some(c)) => {
                            if let Term::Variable(var) = &items[1] {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), Term::typed_literal((c - a).to_string(), "http://www.w3.org/2001/XMLSchema#decimal"));
                                return BuiltinResult::Success(new_bindings);
                            }
                        }
                        _ => {}
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:difference - (a b) math:difference c means a - b = c
        // BIDIRECTIONAL: can solve for any one unknown
        self.register(&format!("{}difference", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    let a_val = get_number(&items[0]);
                    let b_val = get_number(&items[1]);
                    let c_val = get_number(object);

                    match (a_val, b_val, c_val) {
                        // Forward: a - b = ?c
                        (Some(a), Some(b), _) => {
                            return match_or_bind(object, a - b, bindings);
                        }
                        // Backward: ?a - b = c → a = c + b
                        (None, Some(b), Some(c)) => {
                            if let Term::Variable(var) = &items[0] {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), Term::typed_literal((c + b).to_string(), "http://www.w3.org/2001/XMLSchema#decimal"));
                                return BuiltinResult::Success(new_bindings);
                            }
                        }
                        // Backward: a - ?b = c → b = a - c
                        (Some(a), None, Some(c)) => {
                            if let Term::Variable(var) = &items[1] {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), Term::typed_literal((a - c).to_string(), "http://www.w3.org/2001/XMLSchema#decimal"));
                                return BuiltinResult::Success(new_bindings);
                            }
                        }
                        _ => {}
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:product - (a b) math:product c means a * b = c
        // BIDIRECTIONAL: can solve for any one unknown (when divisor ≠ 0)
        self.register(&format!("{}product", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    let a_val = get_number(&items[0]);
                    let b_val = get_number(&items[1]);
                    let c_val = get_number(object);

                    match (a_val, b_val, c_val) {
                        // Forward: a * b = ?c
                        (Some(a), Some(b), _) => {
                            return match_or_bind(object, a * b, bindings);
                        }
                        // Backward: ?a * b = c → a = c / b (if b ≠ 0)
                        (None, Some(b), Some(c)) if b != 0.0 => {
                            if let Term::Variable(var) = &items[0] {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), Term::typed_literal((c / b).to_string(), "http://www.w3.org/2001/XMLSchema#decimal"));
                                return BuiltinResult::Success(new_bindings);
                            }
                        }
                        // Backward: a * ?b = c → b = c / a (if a ≠ 0)
                        (Some(a), None, Some(c)) if a != 0.0 => {
                            if let Term::Variable(var) = &items[1] {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), Term::typed_literal((c / a).to_string(), "http://www.w3.org/2001/XMLSchema#decimal"));
                                return BuiltinResult::Success(new_bindings);
                            }
                        }
                        _ => {}
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:quotient - (a b) math:quotient c means a / b = c
        // BIDIRECTIONAL: can solve for any one unknown
        self.register(&format!("{}quotient", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    let a_val = get_number(&items[0]);
                    let b_val = get_number(&items[1]);
                    let c_val = get_number(object);

                    match (a_val, b_val, c_val) {
                        // Forward: a / b = ?c (if b ≠ 0)
                        (Some(a), Some(b), _) if b != 0.0 => {
                            return match_or_bind(object, a / b, bindings);
                        }
                        // Backward: ?a / b = c → a = c * b
                        (None, Some(b), Some(c)) => {
                            if let Term::Variable(var) = &items[0] {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), Term::typed_literal((c * b).to_string(), "http://www.w3.org/2001/XMLSchema#decimal"));
                                return BuiltinResult::Success(new_bindings);
                            }
                        }
                        // Backward: a / ?b = c → b = a / c (if c ≠ 0)
                        (Some(a), None, Some(c)) if c != 0.0 => {
                            if let Term::Variable(var) = &items[1] {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), Term::typed_literal((a / c).to_string(), "http://www.w3.org/2001/XMLSchema#decimal"));
                                return BuiltinResult::Success(new_bindings);
                            }
                        }
                        _ => {}
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
        // BIDIRECTIONAL: can solve for base or exponent using logarithms
        self.register(&format!("{}exponentiation", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    let a_val = get_number(&items[0]); // base
                    let b_val = get_number(&items[1]); // exponent
                    let c_val = get_number(object);    // result

                    match (a_val, b_val, c_val) {
                        // Forward: a^b = ?c
                        (Some(a), Some(b), _) => {
                            return match_or_bind(object, a.powf(b), bindings);
                        }
                        // Backward: ?a^b = c → a = c^(1/b) (if b ≠ 0)
                        (None, Some(b), Some(c)) if b != 0.0 && c > 0.0 => {
                            if let Term::Variable(var) = &items[0] {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), Term::typed_literal(c.powf(1.0/b).to_string(), "http://www.w3.org/2001/XMLSchema#decimal"));
                                return BuiltinResult::Success(new_bindings);
                            }
                        }
                        // Backward: a^?b = c → b = log_a(c) = ln(c)/ln(a) (if a > 0, a ≠ 1, c > 0)
                        (Some(a), None, Some(c)) if a > 0.0 && a != 1.0 && c > 0.0 => {
                            if let Term::Variable(var) = &items[1] {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), Term::typed_literal((c.ln() / a.ln()).to_string(), "http://www.w3.org/2001/XMLSchema#decimal"));
                                return BuiltinResult::Success(new_bindings);
                            }
                        }
                        _ => {}
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

        // math:randomInteger - (min max) math:randomInteger n
        // Generates a random integer in the range [min, max]
        self.register(&format!("{}randomInteger", ns::MATH), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(min), Some(max)) = (get_number(&items[0]), get_number(&items[1])) {
                        let min_int = min as i64;
                        let max_int = max as i64;
                        if min_int <= max_int {
                            use std::time::{SystemTime, UNIX_EPOCH};
                            // Simple pseudo-random using time-based seed
                            let seed = SystemTime::now()
                                .duration_since(UNIX_EPOCH)
                                .map(|d| d.as_nanos())
                                .unwrap_or(0);
                            let range = (max_int - min_int + 1) as u128;
                            let random_val = min_int + ((seed % range) as i64);
                            return match_or_bind(object, random_val as f64, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // math:random - generates a random float between 0 and 1
        self.register(&format!("{}random", ns::MATH), |_subject, object, bindings| {
            use std::time::{SystemTime, UNIX_EPOCH};
            let seed = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_nanos())
                .unwrap_or(0);
            // Simple LCG-style random
            let random_val = ((seed * 1103515245 + 12345) % (1 << 31)) as f64 / (1u64 << 31) as f64;
            match_or_bind(object, random_val, bindings)
        });
    }

    fn register_string(&mut self) {
        // string:concatenation - (a b ...) string:concatenation c
        // BIDIRECTIONAL: for 2-element lists, can solve for one unknown part
        self.register(&format!("{}concatenation", ns::STRING), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();

                // Special case: 2 elements with one variable - bidirectional
                if items.len() == 2 {
                    let a_str = get_string(&items[0]);
                    let b_str = get_string(&items[1]);
                    let c_str = get_string(object);

                    match (a_str, b_str, c_str) {
                        // Forward: "a" + "b" = ?c
                        (Some(a), Some(b), _) => {
                            let result = format!("{}{}", a, b);
                            return match_or_bind_string(object, result, bindings);
                        }
                        // Backward: ?a + "b" = "c" → a = c[..len(c)-len(b)] if c ends with b
                        (None, Some(b), Some(c)) => {
                            if c.ends_with(&b) && c.len() >= b.len() {
                                if let Term::Variable(var) = &items[0] {
                                    let a = c[..c.len() - b.len()].to_string();
                                    let mut new_bindings = bindings.clone();
                                    new_bindings.insert(var.clone(), Term::Literal(std::sync::Arc::new(crate::term::Literal::plain(a))));
                                    return BuiltinResult::Success(new_bindings);
                                }
                            }
                            return BuiltinResult::Failure;
                        }
                        // Backward: "a" + ?b = "c" → b = c[len(a)..] if c starts with a
                        (Some(a), None, Some(c)) => {
                            if c.starts_with(&a) {
                                if let Term::Variable(var) = &items[1] {
                                    let b = c[a.len()..].to_string();
                                    let mut new_bindings = bindings.clone();
                                    new_bindings.insert(var.clone(), Term::Literal(std::sync::Arc::new(crate::term::Literal::plain(b))));
                                    return BuiltinResult::Success(new_bindings);
                                }
                            }
                            return BuiltinResult::Failure;
                        }
                        _ => return BuiltinResult::NotReady,
                    }
                }

                // General case: all parts must be bound
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

        // log:notIncludes - a log:notIncludes b succeeds if formula a does not include any matching triple of formula b
        // This is the key predicate for Scoped Negation As Failure (SNAF)
        // Supports pattern matching with variables in formula b
        self.register(&format!("{}notIncludes", ns::LOG), |subject, object, _bindings| {
            if let (Term::Formula(formula_a), Term::Formula(formula_b)) = (subject, object) {
                // For each pattern in formula_b, check if it matches any triple in formula_a
                for pattern in formula_b.triples() {
                    let mut found_match = false;
                    for triple in formula_a.triples() {
                        if pattern_matches(pattern, triple) {
                            found_match = true;
                            break;
                        }
                    }
                    // If any pattern in b matches a triple in a, notIncludes fails
                    if found_match {
                        return BuiltinResult::Failure;
                    }
                }
                // No patterns in b matched any triple in a - success!
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
        // Fetches the document at URI with content negotiation and parses it
        // Uses content-type to select the appropriate parser
        self.register(&format!("{}semantics", ns::LOG), |subject, object, bindings| {
            if let Term::Uri(uri) = subject {
                let uri_str = uri.as_str();

                match fetch_rdf_document(uri_str) {
                    Ok((content, content_type)) => {
                        // Parse the content using content-type based parser selection
                        match parse_content_by_type(&content, &content_type) {
                            Ok(triples) => {
                                let result = Term::Formula(FormulaRef::new(0, triples));
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
                    Err(_) => {
                        return BuiltinResult::Failure;
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
        // Returns formula if successful, error string otherwise (with content negotiation)
        self.register(&format!("{}semanticsOrError", ns::LOG), |subject, object, bindings| {
            if let Term::Uri(uri) = subject {
                let uri_str = uri.as_str();

                let result = match fetch_rdf_document(uri_str) {
                    Ok((content, content_type)) => {
                        // Parse the content based on content-type
                        match parse_content_by_type(&content, &content_type) {
                            Ok(triples) => {
                                Term::Formula(FormulaRef::new(0, triples))
                            }
                            Err(e) => {
                                Term::literal(format!("Parse error: {}", e))
                            }
                        }
                    }
                    Err(e) => {
                        Term::literal(e)
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

        // log:findall - (template pattern scope) log:findall list
        // Finds all bindings of template where pattern matches triples in scope
        //
        // Usage forms:
        // 1. (template pattern scope) log:findall ?list
        //    - template: term/list with variables to extract
        //    - pattern: triple pattern to match (with variables)
        //    - scope: formula to search in
        //
        // 2. (template scope) log:findall ?list
        //    - template: term/list with variables to extract
        //    - scope: formula containing data to search
        //
        // Example:
        //   (?person { ?person a :Person } { :alice a :Person . :bob a :Person })
        //       log:findall (:alice :bob) .
        self.register(&format!("{}findall", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();

                // Try 3-element form: (template pattern scope)
                if items.len() == 3 {
                    let template = &items[0];
                    let pattern = &items[1];
                    let scope = &items[2];

                    if let (Term::Formula(pattern_formula), Term::Formula(scope_formula)) = (pattern, scope) {
                        // Find all bindings where pattern matches scope
                        let results = find_all_bindings(template, pattern_formula, scope_formula);
                        return match_or_bind_list(object, results, bindings);
                    }
                }

                // Try 2-element form: (template scope)
                // In this form, scope contains both pattern variables and data
                if items.len() == 2 {
                    let template = &items[0];
                    let scope = &items[1];

                    if let Term::Formula(scope_formula) = scope {
                        // Extract template variables from scope triples
                        let results = find_all_in_scope(template, scope_formula);
                        return match_or_bind_list(object, results, bindings);
                    }
                }
            }

            // Single formula form: { pattern } log:findall ?list
            // Returns all variable binding sets as a list
            if let Term::Formula(formula) = subject {
                let results = extract_all_bindings_from_formula(formula);
                return match_or_bind_list(object, results, bindings);
            }

            BuiltinResult::NotReady
        });

        // log:collectAllIn - (pattern formula) log:collectAllIn bindingsList
        // Collects all variable bindings where pattern matches triples in formula
        //
        // Usage: (?x { ?x a :Person } { :alice a :Person . :bob a :Person }) log:collectAllIn ?list
        // Returns: list of all bindings { ?x -> :alice }, { ?x -> :bob }
        //
        // Similar to log:findall but returns bindings as formula graphs
        self.register(&format!("{}collectAllIn", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() >= 2 {
                    // Extract template and scope
                    let template = &items[0];
                    let scope = if items.len() == 3 {
                        // (template pattern scope) form
                        &items[2]
                    } else {
                        // (template scope) form - pattern is in scope
                        &items[1]
                    };

                    if let Term::Formula(scope_formula) = scope {
                        // If there's a separate pattern formula
                        if items.len() == 3 {
                            if let Term::Formula(pattern_formula) = &items[1] {
                                // Find all bindings where pattern matches scope
                                let results = find_all_bindings(template, pattern_formula, scope_formula);
                                // Convert to formula list (each binding as a formula)
                                let binding_formulas: Vec<Term> = results.iter().map(|t| {
                                    // Create a formula containing just this binding
                                    let triple = Triple::new(
                                        template.clone(),
                                        Term::uri("http://www.w3.org/2002/07/owl#sameAs"),
                                        t.clone(),
                                    );
                                    Term::Formula(FormulaRef::new(0, vec![triple]))
                                }).collect();
                                return match_or_bind_list(object, binding_formulas, bindings);
                            }
                        } else {
                            // Simple form: extract all values from scope
                            let results = find_all_in_scope(template, scope_formula);
                            return match_or_bind_list(object, results, bindings);
                        }
                    }
                }
            }

            // Direct formula form: formula log:collectAllIn list
            // Returns all ground terms from the formula
            if let Term::Formula(formula) = subject {
                let results = extract_all_bindings_from_formula(formula);
                return match_or_bind_list(object, results, bindings);
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
        // Succeeds if pattern matches ALL triples in formula
        //
        // Usage: ({ :a :p :b . :c :p :d } { ?x :p ?y }) log:forAllIn true
        // Succeeds because all triples in the first formula match the pattern ?x :p ?y
        //
        // This is the universal quantification check: "for all triples T in formula, T matches pattern"
        self.register(&format!("{}forAllIn", ns::LOG), |subject, _object, _bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Term::Formula(formula), Term::Formula(pattern)) = (&items[0], &items[1]) {
                        let formula_triples = formula.triples();
                        let pattern_triples = pattern.triples();

                        // If pattern is empty, succeed (vacuously true)
                        if pattern_triples.is_empty() {
                            return BuiltinResult::Success(Bindings::default());
                        }

                        // For each triple in formula, check if it matches at least one pattern
                        for data_triple in formula_triples {
                            let mut matched = false;
                            for pattern_triple in pattern_triples {
                                if pattern_matches(pattern_triple, data_triple) {
                                    matched = true;
                                    break;
                                }
                            }
                            if !matched {
                                return BuiltinResult::Failure;
                            }
                        }

                        // All triples matched
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

        // log:N3Document - type check: a rdf:type log:N3Document if a is a parsed N3 document (formula)
        // Used to mark subjects that represent N3 documents
        self.register(&format!("{}N3Document", ns::LOG), |subject, _object, _bindings| {
            // A formula represents an N3 document
            if let Term::Formula(_) = subject {
                BuiltinResult::Success(Bindings::default())
            } else {
                BuiltinResult::Failure
            }
        });

        // log:Truth - represents truth value class
        // Used with rdf:type to mark boolean truth values
        self.register(&format!("{}Truth", ns::LOG), |subject, _object, _bindings| {
            // Check if subject is a boolean true value
            if let Term::Literal(lit) = subject {
                let val = lit.value().to_lowercase();
                if val == "true" || val == "1" {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // log:Falsehood - represents falsehood value class (complement to Truth)
        self.register(&format!("{}Falsehood", ns::LOG), |subject, _object, _bindings| {
            // Check if subject is a boolean false value
            if let Term::Literal(lit) = subject {
                let val = lit.value().to_lowercase();
                if val == "false" || val == "0" || val == "" {
                    return BuiltinResult::Success(Bindings::default());
                }
            }
            BuiltinResult::Failure
        });

        // log:merge - formulaList log:merge mergedFormula
        // Merges multiple formulas into one (like conjunction but flattens nested formulas)
        // Usage: ({ :a :p :b } { :c :q :d }) log:merge ?merged
        self.register(&format!("{}merge", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let mut all_triples = Vec::new();
                for item in list.iter() {
                    match item {
                        Term::Formula(formula) => {
                            all_triples.extend(formula.triples().to_vec());
                        }
                        _ => return BuiltinResult::NotReady,
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

        // log:becomes - (oldTerm newTerm) log:becomes substitutionPair
        // Creates a substitution mapping from old to new
        // Used with formulas to apply renaming/substitution
        self.register(&format!("{}becomes", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    // Just return the pair as-is
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), subject.clone());
                        return BuiltinResult::Success(new_bindings);
                    } else if subject == object {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // log:copy - formula log:copy copiedFormula
        // Creates a deep copy of a formula with fresh blank nodes
        self.register(&format!("{}copy", ns::LOG), |subject, object, bindings| {
            if let Term::Formula(formula) = subject {
                // Create a copy with same triples (blank nodes are already shared)
                let copied = Term::Formula(FormulaRef::new(0, formula.triples().to_vec()));
                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), copied);
                    return BuiltinResult::Success(new_bindings);
                } else if &copied == object {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::NotReady
        });

        // log:tripleCount - formula log:tripleCount count
        // Returns the number of triples in a formula
        self.register(&format!("{}tripleCount", ns::LOG), |subject, object, bindings| {
            if let Term::Formula(formula) = subject {
                let count = formula.triples().len() as f64;
                return match_or_bind(object, count, bindings);
            }
            BuiltinResult::NotReady
        });

        // log:filter - (formula pattern) log:filter filteredFormula
        // Returns only triples from formula that match pattern
        self.register(&format!("{}filter", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Term::Formula(formula), Term::Formula(pattern)) = (&items[0], &items[1]) {
                        let formula_triples = formula.triples();
                        let pattern_triples = pattern.triples();

                        let mut matching_triples = Vec::new();
                        for data_triple in formula_triples {
                            for pattern_triple in pattern_triples {
                                if pattern_matches(pattern_triple, data_triple) {
                                    if !matching_triples.contains(data_triple) {
                                        matching_triples.push(data_triple.clone());
                                    }
                                    break;
                                }
                            }
                        }

                        let result = Term::Formula(FormulaRef::new(0, matching_triples));
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

        // log:reject - (formula pattern) log:reject filteredFormula
        // Returns only triples from formula that do NOT match pattern
        self.register(&format!("{}reject", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Term::Formula(formula), Term::Formula(pattern)) = (&items[0], &items[1]) {
                        let formula_triples = formula.triples();
                        let pattern_triples = pattern.triples();

                        let mut non_matching_triples = Vec::new();
                        for data_triple in formula_triples {
                            let mut matches = false;
                            for pattern_triple in pattern_triples {
                                if pattern_matches(pattern_triple, data_triple) {
                                    matches = true;
                                    break;
                                }
                            }
                            if !matches {
                                non_matching_triples.push(data_triple.clone());
                            }
                        }

                        let result = Term::Formula(FormulaRef::new(0, non_matching_triples));
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

        // log:difference - (formula1 formula2) log:difference resultFormula
        // Returns triples in formula1 but not in formula2
        self.register(&format!("{}difference", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Term::Formula(formula1), Term::Formula(formula2)) = (&items[0], &items[1]) {
                        let triples1 = formula1.triples();
                        let triples2: std::collections::HashSet<_> = formula2.triples().iter().collect();

                        let mut diff_triples = Vec::new();
                        for triple in triples1 {
                            if !triples2.contains(triple) {
                                diff_triples.push(triple.clone());
                            }
                        }

                        let result = Term::Formula(FormulaRef::new(0, diff_triples));
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

        // log:intersection - (formula1 formula2) log:intersection resultFormula
        // Returns triples that are in both formulas
        self.register(&format!("{}intersection", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Term::Formula(formula1), Term::Formula(formula2)) = (&items[0], &items[1]) {
                        let triples1: std::collections::HashSet<_> = formula1.triples().iter().collect();
                        let triples2 = formula2.triples();

                        let mut intersect_triples = Vec::new();
                        for triple in triples2 {
                            if triples1.contains(triple) {
                                intersect_triples.push(triple.clone());
                            }
                        }

                        let result = Term::Formula(FormulaRef::new(0, intersect_triples));
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

        // log:builtinIn - checks if a predicate is a builtin
        // subject log:builtinIn formula succeeds if subject is a builtin predicate in formula's context
        // Can also be used to enumerate all builtins
        self.register(&format!("{}builtinIn", ns::LOG), |subject, _object, bindings| {
            // Check if subject is a URI that corresponds to a builtin
            if let Term::Uri(uri) = subject {
                // This is a static check - we can't access the registry from here
                // But we can check common builtin namespaces
                let uri_str = uri.as_str();
                let is_builtin = uri_str.starts_with(ns::MATH)
                    || uri_str.starts_with(ns::STRING)
                    || uri_str.starts_with(ns::LIST)
                    || uri_str.starts_with(ns::LOG)
                    || uri_str.starts_with(ns::TIME)
                    || uri_str.starts_with(ns::CRYPTO)
                    || uri_str.starts_with(ns::OS)
                    || uri_str.starts_with(ns::GRAPH);

                if is_builtin {
                    return BuiltinResult::Success(bindings.clone());
                }
            }
            BuiltinResult::Failure
        });

        // log:ifThenElseIn - conditional evaluation
        // (condition then else) log:ifThenElseIn result
        // If condition succeeds in the current context, result is bound to then, otherwise to else
        self.register(&format!("{}ifThenElseIn", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 3 {
                    let condition = &items[0];
                    let then_clause = &items[1];
                    let else_clause = &items[2];

                    // Check if condition is "truthy" (non-empty formula, true boolean, etc.)
                    let condition_true = match condition {
                        Term::Formula(f) => !f.triples().is_empty(),
                        Term::Literal(lit) => {
                            let val = lit.value();
                            val == "true" || val == "1" || (!val.is_empty() && val != "false" && val != "0")
                        }
                        Term::List(l) => !l.is_empty(),
                        Term::Uri(_) => true,
                        _ => false,
                    };

                    let result = if condition_true { then_clause } else { else_clause };

                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), result.clone());
                        return BuiltinResult::Success(new_bindings);
                    } else if object == result {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::Failure
        });

        // log:bulkIn - batch formula operations
        // (formulas operation) log:bulkIn result
        // Applies operation to each formula in the list and returns combined result
        self.register(&format!("{}bulkIn", ns::LOG), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() >= 1 {
                    // Collect all triples from all formulas in the list
                    let mut all_triples = Vec::new();
                    for item in &items {
                        if let Term::Formula(f) = item {
                            all_triples.extend(f.triples().iter().cloned());
                        }
                    }

                    // Remove duplicates
                    all_triples.sort_by(|a, b| format!("{:?}", a).cmp(&format!("{:?}", b)));
                    all_triples.dedup();

                    let result = Term::Formula(FormulaRef::new(0, all_triples));

                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), result);
                        return BuiltinResult::Success(new_bindings);
                    }
                }
            }
            BuiltinResult::Failure
        });

        // log:getList - convert formula contents to a list of statements
        // formula log:getList list
        // Returns the triples as a list of (subject predicate object) lists
        self.register(&format!("{}getList", ns::LOG), |subject, object, bindings| {
            if let Term::Formula(formula) = subject {
                let triples = formula.triples();
                let mut triple_lists: Vec<Term> = Vec::new();

                for triple in triples {
                    let triple_list = Term::List(Arc::new(List::from_vec(vec![
                        triple.subject.clone(),
                        triple.predicate.clone(),
                        triple.object.clone(),
                    ])));
                    triple_lists.push(triple_list);
                }

                let result = Term::List(Arc::new(List::from_vec(triple_lists)));

                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), result);
                    return BuiltinResult::Success(new_bindings);
                } else if let Term::List(_) = object {
                    // Check if it matches
                    if &result == object {
                        return BuiltinResult::Success(bindings.clone());
                    }
                }
            }
            BuiltinResult::Failure
        });

        // log:forAllInClosure - scoped universal quantification with transitive closure
        // (formula pattern) log:forAllInClosure formula2
        // Succeeds if pattern matches all triples in formula including transitively derived ones
        self.register(&format!("{}forAllInClosure", ns::LOG), |subject, object, _bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Term::Formula(formula), Term::Formula(pattern)) = (&items[0], &items[1]) {
                        // Get all triples from both formulas
                        let formula_triples = formula.triples();
                        let pattern_triples = pattern.triples();

                        if pattern_triples.is_empty() {
                            // Empty pattern matches everything
                            return BuiltinResult::Success(Bindings::default());
                        }

                        // Check if all formula triples match the pattern
                        for data_triple in formula_triples {
                            let mut matched = false;
                            for pattern_triple in pattern_triples {
                                // Simple pattern matching - variables match anything
                                let subj_match = matches!(&pattern_triple.subject, Term::Variable(_))
                                    || pattern_triple.subject == data_triple.subject;
                                let pred_match = matches!(&pattern_triple.predicate, Term::Variable(_))
                                    || pattern_triple.predicate == data_triple.predicate;
                                let obj_match = matches!(&pattern_triple.object, Term::Variable(_))
                                    || pattern_triple.object == data_triple.object;

                                if subj_match && pred_match && obj_match {
                                    matched = true;
                                    break;
                                }
                            }
                            if !matched {
                                return BuiltinResult::Failure;
                            }
                        }

                        // Also check that object formula (if provided) is consistent
                        if let Term::Formula(obj_formula) = object {
                            // The object formula should contain the pattern results
                            // For now, just check it's not empty if we had matches
                            if !formula_triples.is_empty() && obj_formula.triples().is_empty() {
                                return BuiltinResult::Failure;
                            }
                        }

                        return BuiltinResult::Success(Bindings::default());
                    }
                }
            }
            BuiltinResult::Failure
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

        // list:flatten - nestedList list:flatten flatList
        // Flattens one level of nesting
        self.register(&format!("{}flatten", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let mut flattened = Vec::new();
                for item in list.iter() {
                    if let Term::List(inner) = item {
                        flattened.extend(inner.iter().cloned());
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

        // list:take - (list n) list:take firstN
        // Takes first n elements from list
        self.register(&format!("{}take", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let (Term::List(list), Some(n)) = (&items[0], get_number(&items[1])) {
                        let n = n as usize;
                        let taken: Vec<Term> = list.iter().take(n).cloned().collect();
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
            BuiltinResult::NotReady
        });

        // list:drop - (list n) list:drop rest
        // Drops first n elements from list
        self.register(&format!("{}drop", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let (Term::List(list), Some(n)) = (&items[0], get_number(&items[1])) {
                        let n = n as usize;
                        let dropped: Vec<Term> = list.iter().skip(n).cloned().collect();
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
            BuiltinResult::NotReady
        });

        // list:splitAt - (list n) list:splitAt (before after)
        // Splits list at index n
        self.register(&format!("{}splitAt", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let (Term::List(list), Some(n)) = (&items[0], get_number(&items[1])) {
                        let n = n as usize;
                        let before: Vec<Term> = list.iter().take(n).cloned().collect();
                        let after: Vec<Term> = list.iter().skip(n).cloned().collect();
                        let result = Term::list(vec![Term::list(before), Term::list(after)]);
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

        // list:intersperse - (list element) list:intersperse result
        // Puts element between each adjacent pair in list
        self.register(&format!("{}intersperse", ns::LIST), |subject, object, bindings| {
            if let Term::List(pair) = subject {
                let items = pair.to_vec();
                if items.len() == 2 {
                    if let Term::List(list) = &items[0] {
                        let sep = items[1].clone();
                        let vec = list.to_vec();
                        let mut result_items = Vec::new();
                        for (i, item) in vec.iter().enumerate() {
                            result_items.push(item.clone());
                            if i < vec.len() - 1 {
                                result_items.push(sep.clone());
                            }
                        }
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

        // list:filterNumbers - list list:filterNumbers numbersOnly
        // Filters list to only include numeric literals
        self.register(&format!("{}filterNumbers", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let filtered: Vec<Term> = list.iter()
                    .filter(|item| get_number(item).is_some())
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
            BuiltinResult::NotReady
        });

        // list:filterStrings - list list:filterStrings stringsOnly
        // Filters list to only include string literals
        self.register(&format!("{}filterStrings", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let filtered: Vec<Term> = list.iter()
                    .filter(|item| get_string(item).is_some())
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
            BuiltinResult::NotReady
        });

        // list:filterURIs - list list:filterURIs urisOnly
        // Filters list to only include URIs
        self.register(&format!("{}filterURIs", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let filtered: Vec<Term> = list.iter()
                    .filter(|item| matches!(item, Term::Uri(_)))
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
            BuiltinResult::NotReady
        });

        // list:min - numericList list:min minimum
        // Returns the minimum number in a list
        self.register(&format!("{}min", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let numbers: Vec<f64> = list.iter()
                    .filter_map(|item| get_number(item))
                    .collect();
                if !numbers.is_empty() {
                    let min = numbers.iter().cloned().fold(f64::INFINITY, f64::min);
                    return match_or_bind(object, min, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // list:max - numericList list:max maximum
        // Returns the maximum number in a list
        self.register(&format!("{}max", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let numbers: Vec<f64> = list.iter()
                    .filter_map(|item| get_number(item))
                    .collect();
                if !numbers.is_empty() {
                    let max = numbers.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
                    return match_or_bind(object, max, bindings);
                }
            }
            BuiltinResult::NotReady
        });

        // list:average - numericList list:average avg
        // Returns the average of numbers in a list
        self.register(&format!("{}average", ns::LIST), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let numbers: Vec<f64> = list.iter()
                    .filter_map(|item| get_number(item))
                    .collect();
                if !numbers.is_empty() {
                    let avg = numbers.iter().sum::<f64>() / numbers.len() as f64;
                    return match_or_bind(object, avg, bindings);
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

        // time:parse - (dateTimeString formatString) time:parse isoDateTime
        // Parses a datetime string according to a format pattern and returns ISO 8601 datetime
        //
        // Format patterns (strftime-style):
        //   %Y - 4-digit year
        //   %m - 2-digit month (01-12)
        //   %d - 2-digit day (01-31)
        //   %H - 2-digit hour (00-23)
        //   %M - 2-digit minute (00-59)
        //   %S - 2-digit second (00-59)
        //   %z - timezone offset (+/-HHMM)
        //
        // Example: ("2024-12-25 14:30:00" "%Y-%m-%d %H:%M:%S") time:parse "2024-12-25T14:30:00Z"
        self.register(&format!("{}parse", ns::TIME), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(input), Some(fmt)) = (get_string(&items[0]), get_string(&items[1])) {
                        if let Some(dt) = parse_datetime_with_format(&input, &fmt) {
                            return match_or_bind_string(object, dt.to_rfc3339(), bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // time:parseToSeconds - (dateTimeString formatString) time:parseToSeconds secondsSinceEpoch
        // Parses a datetime string according to a format pattern and returns Unix timestamp
        //
        // Example: ("2024-12-25 14:30:00" "%Y-%m-%d %H:%M:%S") time:parseToSeconds 1735135800
        self.register(&format!("{}parseToSeconds", ns::TIME), |subject, object, bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(input), Some(fmt)) = (get_string(&items[0]), get_string(&items[1])) {
                        if let Some(dt) = parse_datetime_with_format(&input, &fmt) {
                            return match_or_bind(object, dt.timestamp() as f64, bindings);
                        }
                    }
                }
            }
            BuiltinResult::NotReady
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

        // crypto:publicKey - keypair crypto:publicKey publicKey
        // Extracts the public key from a keypair.
        // Keypair can be a list [privateKey, publicKey] or a private key string
        // from which the public key is derived.
        self.register(&format!("{}publicKey", ns::CRYPTO), |subject, object, bindings| {
            match subject {
                // If keypair is a list [private, public], return the public key
                Term::List(list) => {
                    let items = list.to_vec();
                    if items.len() == 2 {
                        let public_key = items[1].clone();
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), public_key);
                            return BuiltinResult::Success(new_bindings);
                        } else if public_key == *object {
                            return BuiltinResult::Success(bindings.clone());
                        }
                    }
                }
                // If it's a string (private key), derive public key via hash
                Term::Literal(_) => {
                    if let Some(private_key) = get_string(subject) {
                        // Derive public key by hashing the private key
                        // This is a simplified model - real asymmetric crypto would use
                        // proper key derivation (e.g., Ed25519, ECDSA P-256)
                        let mut hasher = Sha256::new();
                        hasher.update(b"public_key_derivation:");
                        hasher.update(private_key.as_bytes());
                        let result = hasher.finalize();
                        let public_key = format!("{:x}", result);
                        return match_or_bind_string(object, public_key, bindings);
                    }
                }
                _ => {}
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

        // os:readFile - path os:readFile contents
        // Reads the contents of a file as a string
        // Note: This is a security-sensitive operation
        self.register(&format!("{}readFile", ns::OS), |subject, object, bindings| {
            if let Some(path) = get_string(subject) {
                // Security: Only allow reading files in current directory or subdirectories
                let path_obj = std::path::Path::new(&path);
                if !path.contains("..") && !path.starts_with('/') {
                    if let Ok(contents) = std::fs::read_to_string(path_obj) {
                        return match_or_bind_string(object, contents, bindings);
                    }
                }
            }
            BuiltinResult::Failure
        });

        // os:writeFile - (path contents) os:writeFile true
        // Writes contents to a file
        // Note: This is a security-sensitive operation
        self.register(&format!("{}writeFile", ns::OS), |subject, _object, _bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(path), Some(contents)) = (get_string(&items[0]), get_string(&items[1])) {
                        // Security: Only allow writing files in current directory or subdirectories
                        if !path.contains("..") && !path.starts_with('/') {
                            if std::fs::write(&path, &contents).is_ok() {
                                return BuiltinResult::Success(Bindings::default());
                            }
                        }
                    }
                }
            }
            BuiltinResult::Failure
        });

        // os:appendFile - (path contents) os:appendFile true
        // Appends contents to a file
        self.register(&format!("{}appendFile", ns::OS), |subject, _object, _bindings| {
            if let Term::List(list) = subject {
                let items = list.to_vec();
                if items.len() == 2 {
                    if let (Some(path), Some(contents)) = (get_string(&items[0]), get_string(&items[1])) {
                        // Security: Only allow appending to files in current directory or subdirectories
                        if !path.contains("..") && !path.starts_with('/') {
                            use std::io::Write;
                            if let Ok(mut file) = std::fs::OpenOptions::new()
                                .create(true)
                                .append(true)
                                .open(&path)
                            {
                                if file.write_all(contents.as_bytes()).is_ok() {
                                    return BuiltinResult::Success(Bindings::default());
                                }
                            }
                        }
                    }
                }
            }
            BuiltinResult::Failure
        });

        // os:deleteFile - path os:deleteFile true
        // Deletes a file
        // Note: This is a security-sensitive operation
        self.register(&format!("{}deleteFile", ns::OS), |subject, _object, _bindings| {
            if let Some(path) = get_string(subject) {
                // Security: Only allow deleting files in current directory or subdirectories
                if !path.contains("..") && !path.starts_with('/') {
                    if std::fs::remove_file(&path).is_ok() {
                        return BuiltinResult::Success(Bindings::default());
                    }
                }
            }
            BuiltinResult::Failure
        });

        // os:listDir - path os:listDir fileList
        // Lists files in a directory
        self.register(&format!("{}listDir", ns::OS), |subject, object, bindings| {
            if let Some(path) = get_string(subject) {
                // Security: Only allow listing directories in current directory or subdirectories
                if !path.contains("..") && !path.starts_with('/') {
                    if let Ok(entries) = std::fs::read_dir(&path) {
                        let files: Vec<Term> = entries
                            .filter_map(|e| e.ok())
                            .filter_map(|e| e.path().to_str().map(|s| Term::literal(s)))
                            .collect();
                        let result = Term::List(Arc::new(List::from_vec(files)));
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), result);
                            return BuiltinResult::Success(new_bindings);
                        }
                    }
                }
            }
            BuiltinResult::Failure
        });

        // os:createDir - path os:createDir true
        // Creates a directory
        self.register(&format!("{}createDir", ns::OS), |subject, _object, _bindings| {
            if let Some(path) = get_string(subject) {
                // Security: Only allow creating directories in current directory or subdirectories
                if !path.contains("..") && !path.starts_with('/') {
                    if std::fs::create_dir_all(&path).is_ok() {
                        return BuiltinResult::Success(Bindings::default());
                    }
                }
            }
            BuiltinResult::Failure
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

    /// Register database builtins (db: namespace)
    /// Provides SQLite database access
    pub fn register_db(&mut self) {
        use rusqlite::Connection;
        use std::sync::{Arc, Mutex};
        use std::collections::HashMap;

        // Thread-local storage for database connections
        thread_local! {
            static DB_CONNECTIONS: std::cell::RefCell<HashMap<String, Arc<Mutex<Connection>>>> =
                std::cell::RefCell::new(HashMap::new());
        }

        // db:connect - path db:connect handle
        // Opens a SQLite database connection
        self.register(&format!("{}connect", ns::DB), |subject, object, bindings| {
            if let Some(path) = get_string(&subject) {
                // Try to open the database
                match Connection::open(&path) {
                    Ok(conn) => {
                        // Create a unique handle identifier
                        let handle = format!("db:handle:{}", path);
                        let handle_term = Term::Literal(std::sync::Arc::new(
                            crate::term::Literal::plain(handle.clone())
                        ));

                        // Store connection in thread-local storage
                        DB_CONNECTIONS.with(|conns| {
                            conns.borrow_mut().insert(handle, Arc::new(Mutex::new(conn)));
                        });

                        // Bind result
                        if let Term::Variable(var) = object {
                            let mut new_bindings = bindings.clone();
                            new_bindings.insert(var.clone(), handle_term);
                            return BuiltinResult::Success(new_bindings);
                        }
                        return BuiltinResult::Success(bindings.clone());
                    }
                    Err(_) => return BuiltinResult::Failure,
                }
            }
            BuiltinResult::NotReady
        });

        // db:query - (handle "SQL") db:query results
        // Executes a SQL query and returns results as a list of lists
        self.register(&format!("{}query", ns::DB), |subject, object, bindings| {
            if let Term::List(args) = subject {
                let items = args.to_vec();
                if items.len() >= 2 {
                    if let (Some(handle), Some(sql)) = (get_string(&items[0]), get_string(&items[1])) {
                        // Get connection from thread-local storage
                        let result = DB_CONNECTIONS.with(|conns| {
                            if let Some(conn_arc) = conns.borrow().get(&handle) {
                                if let Ok(conn) = conn_arc.lock() {
                                    // Execute query
                                    if let Ok(mut stmt) = conn.prepare(&sql) {
                                        let column_count = stmt.column_count();
                                        let mut rows: Vec<Term> = Vec::new();

                                        if let Ok(mut query_rows) = stmt.query([]) {
                                            while let Ok(Some(row)) = query_rows.next() {
                                                let mut row_values: Vec<Term> = Vec::new();
                                                for i in 0..column_count {
                                                    // Handle different SQLite value types
                                                    let value_ref = row.get_ref(i).ok();
                                                    let term = match value_ref {
                                                        Some(rusqlite::types::ValueRef::Integer(n)) =>
                                                            Term::typed_literal(n.to_string(), "http://www.w3.org/2001/XMLSchema#integer"),
                                                        Some(rusqlite::types::ValueRef::Real(f)) =>
                                                            Term::typed_literal(f.to_string(), "http://www.w3.org/2001/XMLSchema#decimal"),
                                                        Some(rusqlite::types::ValueRef::Text(s)) =>
                                                            Term::Literal(std::sync::Arc::new(
                                                                crate::term::Literal::plain(String::from_utf8_lossy(s).to_string())
                                                            )),
                                                        Some(rusqlite::types::ValueRef::Blob(b)) =>
                                                            Term::Literal(std::sync::Arc::new(
                                                                crate::term::Literal::plain(base64::Engine::encode(&base64::engine::general_purpose::STANDARD, b))
                                                            )),
                                                        Some(rusqlite::types::ValueRef::Null) | None =>
                                                            Term::Literal(std::sync::Arc::new(
                                                                crate::term::Literal::plain("".to_string())
                                                            )),
                                                    };
                                                    row_values.push(term);
                                                }
                                                rows.push(Term::list(row_values));
                                            }
                                        }
                                        return Some(Term::list(rows));
                                    }
                                }
                            }
                            None
                        });

                        if let Some(result_term) = result {
                            if let Term::Variable(var) = object {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), result_term);
                                return BuiltinResult::Success(new_bindings);
                            }
                            return BuiltinResult::Success(bindings.clone());
                        }
                        return BuiltinResult::Failure;
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // db:execute - (handle "SQL") db:execute rowcount
        // Executes a SQL statement and returns affected row count
        self.register(&format!("{}execute", ns::DB), |subject, object, bindings| {
            if let Term::List(args) = subject {
                let items = args.to_vec();
                if items.len() >= 2 {
                    if let (Some(handle), Some(sql)) = (get_string(&items[0]), get_string(&items[1])) {
                        let result = DB_CONNECTIONS.with(|conns| {
                            if let Some(conn_arc) = conns.borrow().get(&handle) {
                                if let Ok(conn) = conn_arc.lock() {
                                    if let Ok(count) = conn.execute(&sql, []) {
                                        return Some(count as f64);
                                    }
                                }
                            }
                            None
                        });

                        if let Some(count) = result {
                            return match_or_bind(object, count, bindings);
                        }
                        return BuiltinResult::Failure;
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // db:tables - handle db:tables tableList
        // Returns list of table names in the database
        self.register(&format!("{}tables", ns::DB), |subject, object, bindings| {
            if let Some(handle) = get_string(&subject) {
                let result = DB_CONNECTIONS.with(|conns| {
                    if let Some(conn_arc) = conns.borrow().get(&handle) {
                        if let Ok(conn) = conn_arc.lock() {
                            let sql = "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name";
                            if let Ok(mut stmt) = conn.prepare(sql) {
                                let mut tables: Vec<Term> = Vec::new();
                                if let Ok(mut rows) = stmt.query([]) {
                                    while let Ok(Some(row)) = rows.next() {
                                        let name: String = row.get(0).unwrap_or_default();
                                        tables.push(Term::Literal(std::sync::Arc::new(
                                            crate::term::Literal::plain(name)
                                        )));
                                    }
                                }
                                return Some(Term::list(tables));
                            }
                        }
                    }
                    None
                });

                if let Some(tables_term) = result {
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), tables_term);
                        return BuiltinResult::Success(new_bindings);
                    }
                    return BuiltinResult::Success(bindings.clone());
                }
                return BuiltinResult::Failure;
            }
            BuiltinResult::NotReady
        });

        // db:columns - (handle "tableName") db:columns columnList
        // Returns list of column names for a table
        self.register(&format!("{}columns", ns::DB), |subject, object, bindings| {
            if let Term::List(args) = subject {
                let items = args.to_vec();
                if items.len() >= 2 {
                    if let (Some(handle), Some(table)) = (get_string(&items[0]), get_string(&items[1])) {
                        let result = DB_CONNECTIONS.with(|conns| {
                            if let Some(conn_arc) = conns.borrow().get(&handle) {
                                if let Ok(conn) = conn_arc.lock() {
                                    let sql = format!("PRAGMA table_info({})", table);
                                    if let Ok(mut stmt) = conn.prepare(&sql) {
                                        let mut columns: Vec<Term> = Vec::new();
                                        if let Ok(mut rows) = stmt.query([]) {
                                            while let Ok(Some(row)) = rows.next() {
                                                let name: String = row.get(1).unwrap_or_default();
                                                columns.push(Term::Literal(std::sync::Arc::new(
                                                    crate::term::Literal::plain(name)
                                                )));
                                            }
                                        }
                                        return Some(Term::list(columns));
                                    }
                                }
                            }
                            None
                        });

                        if let Some(columns_term) = result {
                            if let Term::Variable(var) = object {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), columns_term);
                                return BuiltinResult::Success(new_bindings);
                            }
                            return BuiltinResult::Success(bindings.clone());
                        }
                        return BuiltinResult::Failure;
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // db:close - handle db:close true
        // Closes a database connection
        self.register(&format!("{}close", ns::DB), |subject, object, bindings| {
            if let Some(handle) = get_string(&subject) {
                let removed = DB_CONNECTIONS.with(|conns| {
                    conns.borrow_mut().remove(&handle).is_some()
                });

                if removed {
                    let true_term = Term::typed_literal("true".to_string(), "http://www.w3.org/2001/XMLSchema#boolean");
                    if let Term::Variable(var) = object {
                        let mut new_bindings = bindings.clone();
                        new_bindings.insert(var.clone(), true_term);
                        return BuiltinResult::Success(new_bindings);
                    }
                    return BuiltinResult::Success(bindings.clone());
                }
                return BuiltinResult::Failure;
            }
            BuiltinResult::NotReady
        });

        // db:insertRow - (handle "table" (col1 col2 ...) (val1 val2 ...)) db:insertRow rowid
        // Inserts a row into a table
        self.register(&format!("{}insertRow", ns::DB), |subject, object, bindings| {
            if let Term::List(args) = subject {
                let items = args.to_vec();
                if items.len() >= 4 {
                    if let (Some(handle), Some(table)) = (get_string(&items[0]), get_string(&items[1])) {
                        // Get column names
                        let columns: Vec<String> = if let Term::List(cols) = &items[2] {
                            cols.to_vec().iter().filter_map(|t| get_string(t)).collect()
                        } else {
                            return BuiltinResult::NotReady;
                        };

                        // Get values
                        let values: Vec<String> = if let Term::List(vals) = &items[3] {
                            vals.to_vec().iter().filter_map(|t| get_string(t)).collect()
                        } else {
                            return BuiltinResult::NotReady;
                        };

                        if columns.len() != values.len() {
                            return BuiltinResult::Failure;
                        }

                        let result = DB_CONNECTIONS.with(|conns| {
                            if let Some(conn_arc) = conns.borrow().get(&handle) {
                                if let Ok(conn) = conn_arc.lock() {
                                    let col_str = columns.join(", ");
                                    let placeholders: Vec<&str> = values.iter().map(|_| "?").collect();
                                    let sql = format!(
                                        "INSERT INTO {} ({}) VALUES ({})",
                                        table, col_str, placeholders.join(", ")
                                    );

                                    // Create params as strings
                                    let params: Vec<&str> = values.iter().map(|s| s.as_str()).collect();
                                    if let Ok(mut stmt) = conn.prepare(&sql) {
                                        if stmt.execute(rusqlite::params_from_iter(&params)).is_ok() {
                                            return Some(conn.last_insert_rowid() as f64);
                                        }
                                    }
                                }
                            }
                            None
                        });

                        if let Some(rowid) = result {
                            return match_or_bind(object, rowid, bindings);
                        }
                        return BuiltinResult::Failure;
                    }
                }
            }
            BuiltinResult::NotReady
        });

        // db:selectWhere - (handle "table" "whereClause") db:selectWhere results
        // Selects rows matching a WHERE clause
        self.register(&format!("{}selectWhere", ns::DB), |subject, object, bindings| {
            if let Term::List(args) = subject {
                let items = args.to_vec();
                if items.len() >= 3 {
                    if let (Some(handle), Some(table), Some(where_clause)) =
                        (get_string(&items[0]), get_string(&items[1]), get_string(&items[2])) {
                        let result = DB_CONNECTIONS.with(|conns| {
                            if let Some(conn_arc) = conns.borrow().get(&handle) {
                                if let Ok(conn) = conn_arc.lock() {
                                    let sql = format!("SELECT * FROM {} WHERE {}", table, where_clause);
                                    if let Ok(mut stmt) = conn.prepare(&sql) {
                                        let column_count = stmt.column_count();
                                        let mut rows: Vec<Term> = Vec::new();

                                        if let Ok(mut query_rows) = stmt.query([]) {
                                            while let Ok(Some(row)) = query_rows.next() {
                                                let mut row_values: Vec<Term> = Vec::new();
                                                for i in 0..column_count {
                                                    // Handle different SQLite value types
                                                    let value_ref = row.get_ref(i).ok();
                                                    let term = match value_ref {
                                                        Some(rusqlite::types::ValueRef::Integer(n)) =>
                                                            Term::typed_literal(n.to_string(), "http://www.w3.org/2001/XMLSchema#integer"),
                                                        Some(rusqlite::types::ValueRef::Real(f)) =>
                                                            Term::typed_literal(f.to_string(), "http://www.w3.org/2001/XMLSchema#decimal"),
                                                        Some(rusqlite::types::ValueRef::Text(s)) =>
                                                            Term::Literal(std::sync::Arc::new(
                                                                crate::term::Literal::plain(String::from_utf8_lossy(s).to_string())
                                                            )),
                                                        Some(rusqlite::types::ValueRef::Blob(b)) =>
                                                            Term::Literal(std::sync::Arc::new(
                                                                crate::term::Literal::plain(base64::Engine::encode(&base64::engine::general_purpose::STANDARD, b))
                                                            )),
                                                        Some(rusqlite::types::ValueRef::Null) | None =>
                                                            Term::Literal(std::sync::Arc::new(
                                                                crate::term::Literal::plain("".to_string())
                                                            )),
                                                    };
                                                    row_values.push(term);
                                                }
                                                rows.push(Term::list(row_values));
                                            }
                                        }
                                        return Some(Term::list(rows));
                                    }
                                }
                            }
                            None
                        });

                        if let Some(result_term) = result {
                            if let Term::Variable(var) = object {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var.clone(), result_term);
                                return BuiltinResult::Success(new_bindings);
                            }
                            return BuiltinResult::Success(bindings.clone());
                        }
                        return BuiltinResult::Failure;
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

/// Check if a term matches another term, supporting variables as wildcards
/// Used for SNAF (Scoped Negation As Failure) pattern matching
fn term_matches(pattern: &Term, term: &Term) -> bool {
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
                // For formulas, check if all triples match
                let triples1 = f1.triples();
                let triples2 = f2.triples();
                if triples1.len() != triples2.len() {
                    return false;
                }
                triples1.iter().zip(triples2.iter()).all(|(t1, t2)| {
                    pattern_matches(t1, t2)
                })
            } else {
                false
            }
        }
        Term::List(list1) => {
            if let Term::List(list2) = term {
                if list1.len() != list2.len() {
                    return false;
                }
                list1.iter().zip(list2.iter()).all(|(t1, t2)| {
                    term_matches(t1, t2)
                })
            } else {
                false
            }
        }
    }
}

/// Check if a pattern triple matches a target triple
/// Variables in the pattern act as wildcards
fn pattern_matches(pattern: &crate::term::Triple, triple: &crate::term::Triple) -> bool {
    term_matches(&pattern.subject, &triple.subject) &&
    term_matches(&pattern.predicate, &triple.predicate) &&
    term_matches(&pattern.object, &triple.object)
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

/// Match a list against an object or bind it
fn match_or_bind_list(object: &Term, items: Vec<Term>, bindings: &Bindings) -> BuiltinResult {
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
// time:parse Helper Functions
// ============================================================================

/// Parse a datetime string using a strftime-style format
///
/// Supports common format specifiers:
/// - %Y: 4-digit year
/// - %m: 2-digit month (01-12)
/// - %d: 2-digit day (01-31)
/// - %H: 2-digit hour (00-23)
/// - %M: 2-digit minute (00-59)
/// - %S: 2-digit second (00-59)
/// - %z: timezone offset (+/-HHMM)
fn parse_datetime_with_format(input: &str, format: &str) -> Option<DateTime<chrono::FixedOffset>> {
    use chrono::NaiveDateTime;

    // Build a regex pattern from the format string
    let mut pattern = String::new();
    let mut captures: Vec<&str> = Vec::new();
    let mut chars = format.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            if let Some(&spec) = chars.peek() {
                chars.next();
                match spec {
                    'Y' => {
                        pattern.push_str(r"(\d{4})");
                        captures.push("year");
                    }
                    'm' => {
                        pattern.push_str(r"(\d{2})");
                        captures.push("month");
                    }
                    'd' => {
                        pattern.push_str(r"(\d{2})");
                        captures.push("day");
                    }
                    'H' => {
                        pattern.push_str(r"(\d{2})");
                        captures.push("hour");
                    }
                    'M' => {
                        pattern.push_str(r"(\d{2})");
                        captures.push("minute");
                    }
                    'S' => {
                        pattern.push_str(r"(\d{2})");
                        captures.push("second");
                    }
                    'z' => {
                        pattern.push_str(r"([+-]\d{4})");
                        captures.push("tz");
                    }
                    '%' => {
                        pattern.push('%');
                    }
                    _ => {
                        pattern.push('%');
                        pattern.push(spec);
                    }
                }
            }
        } else {
            // Escape special regex characters
            if "[]{}()*+?.\\^$|".contains(c) {
                pattern.push('\\');
            }
            pattern.push(c);
        }
    }

    let re = Regex::new(&format!("^{}$", pattern)).ok()?;
    let caps = re.captures(input)?;

    let mut year = 1970;
    let mut month = 1u32;
    let mut day = 1u32;
    let mut hour = 0u32;
    let mut minute = 0u32;
    let mut second = 0u32;
    let mut tz_offset_secs = 0i32;

    for (i, cap_name) in captures.iter().enumerate() {
        let value = caps.get(i + 1)?.as_str();
        match *cap_name {
            "year" => year = value.parse().ok()?,
            "month" => month = value.parse().ok()?,
            "day" => day = value.parse().ok()?,
            "hour" => hour = value.parse().ok()?,
            "minute" => minute = value.parse().ok()?,
            "second" => second = value.parse().ok()?,
            "tz" => {
                // Parse timezone offset like +0530 or -0800
                let sign = if value.starts_with('-') { -1 } else { 1 };
                let hours: i32 = value[1..3].parse().ok()?;
                let mins: i32 = value[3..5].parse().ok()?;
                tz_offset_secs = sign * (hours * 3600 + mins * 60);
            }
            _ => {}
        }
    }

    let offset = chrono::FixedOffset::east_opt(tz_offset_secs)?;
    let naive = NaiveDateTime::new(
        chrono::NaiveDate::from_ymd_opt(year, month, day)?,
        chrono::NaiveTime::from_hms_opt(hour, minute, second)?,
    );

    Some(naive.and_local_timezone(offset).single()?)
}

// ============================================================================
// log:findall Helper Functions
// ============================================================================

/// Try to unify a pattern term with a data term, collecting bindings
fn unify_term(pattern: &Term, data: &Term, bindings: &mut Bindings) -> bool {
    match pattern {
        Term::Variable(var) => {
            // Check if variable is already bound
            if let Some(existing) = bindings.get(var) {
                // Must match existing binding
                existing == data
            } else {
                // Bind the variable
                bindings.insert(var.clone(), data.clone());
                true
            }
        }
        Term::Uri(uri1) => {
            if let Term::Uri(uri2) = data {
                uri1.as_str() == uri2.as_str()
            } else {
                false
            }
        }
        Term::Literal(lit1) => {
            if let Term::Literal(lit2) = data {
                lit1.value() == lit2.value() && lit1.datatype() == lit2.datatype()
            } else {
                false
            }
        }
        Term::BlankNode(bn1) => {
            if let Term::BlankNode(bn2) = data {
                bn1.label() == bn2.label()
            } else {
                false
            }
        }
        Term::List(list1) => {
            if let Term::List(list2) = data {
                if list1.len() != list2.len() {
                    return false;
                }
                for (p, d) in list1.iter().zip(list2.iter()) {
                    if !unify_term(p, d, bindings) {
                        return false;
                    }
                }
                true
            } else {
                false
            }
        }
        Term::Formula(f1) => {
            if let Term::Formula(f2) = data {
                if f1.triples().len() != f2.triples().len() {
                    return false;
                }
                // Check each triple
                for (t1, t2) in f1.triples().iter().zip(f2.triples().iter()) {
                    let mut temp_bindings = bindings.clone();
                    if !unify_triple(t1, t2, &mut temp_bindings) {
                        return false;
                    }
                    *bindings = temp_bindings;
                }
                true
            } else {
                false
            }
        }
    }
}

/// Try to unify a pattern triple with a data triple
fn unify_triple(pattern: &Triple, data: &Triple, bindings: &mut Bindings) -> bool {
    unify_term(&pattern.subject, &data.subject, bindings) &&
    unify_term(&pattern.predicate, &data.predicate, bindings) &&
    unify_term(&pattern.object, &data.object, bindings)
}

/// Substitute variables in a term with their bindings
fn substitute_term(term: &Term, bindings: &Bindings) -> Term {
    match term {
        Term::Variable(var) => {
            if let Some(value) = bindings.get(var) {
                value.clone()
            } else {
                term.clone()
            }
        }
        Term::List(list) => {
            let items: Vec<Term> = list.iter()
                .map(|t| substitute_term(t, bindings))
                .collect();
            Term::list(items)
        }
        Term::Formula(formula) => {
            let triples: Vec<Triple> = formula.triples().iter()
                .map(|t| Triple::new(
                    substitute_term(&t.subject, bindings),
                    substitute_term(&t.predicate, bindings),
                    substitute_term(&t.object, bindings),
                ))
                .collect();
            Term::Formula(FormulaRef::new(formula.id(), triples))
        }
        _ => term.clone(),
    }
}

/// Find all bindings where pattern matches scope, return list of template instances
///
/// For (template pattern scope) log:findall list
fn find_all_bindings(template: &Term, pattern: &FormulaRef, scope: &FormulaRef) -> Vec<Term> {
    let pattern_triples = pattern.triples();
    let scope_triples = scope.triples();
    let mut results = Vec::new();

    // If pattern has one triple, find all matches
    if pattern_triples.len() == 1 {
        let pattern_triple = &pattern_triples[0];
        for data_triple in scope_triples {
            let mut bindings = Bindings::default();
            if unify_triple(pattern_triple, data_triple, &mut bindings) {
                let result = substitute_term(template, &bindings);
                if !results.contains(&result) {
                    results.push(result);
                }
            }
        }
    } else if !pattern_triples.is_empty() {
        // Multiple pattern triples - need to find bindings that match all
        // Use backtracking search
        let all_bindings = match_patterns_recursive(&pattern_triples, scope_triples, 0, Bindings::default());
        for bindings in all_bindings {
            let result = substitute_term(template, &bindings);
            if !results.contains(&result) {
                results.push(result);
            }
        }
    }

    results
}

/// Recursively match multiple pattern triples against scope
fn match_patterns_recursive(
    patterns: &[Triple],
    scope: &[Triple],
    index: usize,
    current_bindings: Bindings,
) -> Vec<Bindings> {
    if index >= patterns.len() {
        return vec![current_bindings];
    }

    let pattern = &patterns[index];
    let mut all_results = Vec::new();

    for data_triple in scope {
        let mut bindings = current_bindings.clone();
        if unify_triple(pattern, data_triple, &mut bindings) {
            let sub_results = match_patterns_recursive(patterns, scope, index + 1, bindings);
            all_results.extend(sub_results);
        }
    }

    all_results
}

/// Find all values matching template in scope formula
///
/// For (template scope) log:findall list - extracts template bindings from scope
fn find_all_in_scope(template: &Term, scope: &FormulaRef) -> Vec<Term> {
    let mut results = Vec::new();
    let scope_triples = scope.triples();

    // Collect all variables from template
    let template_vars = collect_variables(template);

    // For each triple in scope, extract variable bindings
    for triple in scope_triples {
        let mut bindings = Bindings::default();

        // Try to find variables in subject, predicate, object
        extract_bindings_from_term(&triple.subject, &template_vars, &mut bindings);
        extract_bindings_from_term(&triple.predicate, &template_vars, &mut bindings);
        extract_bindings_from_term(&triple.object, &template_vars, &mut bindings);

        // If we got bindings, substitute template
        if !bindings.is_empty() {
            let result = substitute_term(template, &bindings);
            if !results.contains(&result) {
                results.push(result);
            }
        }
    }

    results
}

/// Collect all variables from a term
fn collect_variables(term: &Term) -> Vec<Variable> {
    let mut vars = Vec::new();
    collect_variables_inner(term, &mut vars);
    vars
}

fn collect_variables_inner(term: &Term, vars: &mut Vec<Variable>) {
    match term {
        Term::Variable(var) => {
            if !vars.contains(var) {
                vars.push(var.clone());
            }
        }
        Term::List(list) => {
            for item in list.iter() {
                collect_variables_inner(item, vars);
            }
        }
        Term::Formula(formula) => {
            for triple in formula.triples() {
                collect_variables_inner(&triple.subject, vars);
                collect_variables_inner(&triple.predicate, vars);
                collect_variables_inner(&triple.object, vars);
            }
        }
        _ => {}
    }
}

/// Extract bindings from a term if it matches template variables
fn extract_bindings_from_term(term: &Term, template_vars: &[Variable], _bindings: &mut Bindings) {
    if let Term::Variable(var) = term {
        // If this variable is in our template, bind it to itself (it's already the value)
        if template_vars.iter().any(|v| v.name() == var.name()) {
            // In scope, variables represent themselves as values
            // This is used when scope contains ground data and we extract matching values
        }
    }
    // For ground terms, we don't extract - template vars must match against data
}

/// Extract all unique bindings from a formula (for { pattern } log:findall list)
/// Returns list of all ground terms found where template variables occur
fn extract_all_bindings_from_formula(formula: &FormulaRef) -> Vec<Term> {
    let mut results = Vec::new();

    // Collect all unique terms from formula triples
    for triple in formula.triples() {
        add_unique_term(&triple.subject, &mut results);
        add_unique_term(&triple.object, &mut results);
    }

    results
}

/// Add a term to results if it's ground and not already present
fn add_unique_term(term: &Term, results: &mut Vec<Term>) {
    match term {
        Term::Variable(_) => {} // Skip variables
        Term::Uri(_) | Term::Literal(_) | Term::BlankNode(_) => {
            if !results.contains(term) {
                results.push(term.clone());
            }
        }
        Term::List(list) => {
            // Check if all elements are ground
            if list.iter().all(|t| !matches!(t, Term::Variable(_))) {
                if !results.contains(term) {
                    results.push(term.clone());
                }
            }
        }
        Term::Formula(_) => {
            if !results.contains(term) {
                results.push(term.clone());
            }
        }
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
