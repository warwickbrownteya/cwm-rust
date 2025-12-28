//! Structured Error Handling for cwm-rust
//!
//! Provides a unified error type hierarchy with:
//! - Error codes for programmatic handling
//! - Structured error responses (JSON-friendly)
//! - Context preservation through error chains
//! - HTTP status code mapping
//!
//! # Error Categories
//!
//! - `ParseError` - Syntax errors in N3/RDF/SPARQL parsing
//! - `ReasoningError` - Inference engine failures
//! - `StoreError` - Triple store operations
//! - `NetworkError` - HTTP/network failures
//! - `ConfigError` - Configuration issues
//! - `ValidationError` - Input validation failures
//! - `SecurityError` - Security policy violations
//!
//! # Example
//!
//! ```rust,ignore
//! use cwm::error::{CwmError, ErrorCode, ErrorContext};
//!
//! fn process_query(query: &str) -> Result<(), CwmError> {
//!     if query.is_empty() {
//!         return Err(CwmError::validation("Empty query")
//!             .with_code(ErrorCode::EmptyInput)
//!             .with_context("query", query));
//!     }
//!     Ok(())
//! }
//! ```

use std::collections::HashMap;
use std::fmt;
use serde::{Deserialize, Serialize};

// ============================================================================
// Error Codes
// ============================================================================

/// Unique error codes for programmatic error handling
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum ErrorCode {
    // Parse errors (1xxx)
    /// Generic parse error
    ParseError = 1000,
    /// Invalid N3 syntax
    InvalidN3Syntax = 1001,
    /// Invalid RDF/XML syntax
    InvalidRdfXml = 1002,
    /// Invalid SPARQL syntax
    InvalidSparql = 1003,
    /// Invalid URI
    InvalidUri = 1004,
    /// Invalid literal value
    InvalidLiteral = 1005,
    /// Unexpected end of input
    UnexpectedEof = 1006,
    /// Invalid prefix declaration
    InvalidPrefix = 1007,

    // Reasoning errors (2xxx)
    /// Generic reasoning error
    ReasoningError = 2000,
    /// Maximum inference steps exceeded
    MaxStepsExceeded = 2001,
    /// Infinite loop detected
    InfiniteLoop = 2002,
    /// Unstratifiable negation
    UnstratifiableNegation = 2003,
    /// Builtin evaluation failed
    BuiltinFailed = 2004,
    /// Rule application failed
    RuleApplicationFailed = 2005,
    /// Proof generation failed
    ProofGenerationFailed = 2006,

    // Store errors (3xxx)
    /// Generic store error
    StoreError = 3000,
    /// Triple not found
    TripleNotFound = 3001,
    /// Duplicate triple
    DuplicateTriple = 3002,
    /// Store capacity exceeded
    CapacityExceeded = 3003,
    /// Transaction failed
    TransactionFailed = 3004,
    /// Connection failed
    ConnectionFailed = 3005,

    // Network errors (4xxx)
    /// Generic network error
    NetworkError = 4000,
    /// Connection timeout
    ConnectionTimeout = 4001,
    /// Request timeout
    RequestTimeout = 4002,
    /// DNS resolution failed
    DnsResolutionFailed = 4003,
    /// SSL/TLS error
    TlsError = 4004,
    /// HTTP error response
    HttpError = 4005,
    /// Network disabled by policy
    NetworkDisabled = 4006,
    /// Domain not allowed
    DomainNotAllowed = 4007,

    // Validation errors (5xxx)
    /// Generic validation error
    ValidationError = 5000,
    /// Empty input
    EmptyInput = 5001,
    /// Input too large
    InputTooLarge = 5002,
    /// Invalid format
    InvalidFormat = 5003,
    /// Missing required field
    MissingRequired = 5004,
    /// Invalid value
    InvalidValue = 5005,
    /// Constraint violation
    ConstraintViolation = 5006,

    // Security errors (6xxx)
    /// Generic security error
    SecurityError = 6000,
    /// Rate limit exceeded
    RateLimitExceeded = 6001,
    /// Unauthorized access
    Unauthorized = 6002,
    /// Forbidden operation
    Forbidden = 6003,
    /// Crypto operation failed
    CryptoFailed = 6004,
    /// File access denied
    FileAccessDenied = 6005,

    // Config errors (7xxx)
    /// Generic config error
    ConfigError = 7000,
    /// Config file not found
    ConfigNotFound = 7001,
    /// Invalid config syntax
    InvalidConfigSyntax = 7002,
    /// Unknown profile
    UnknownProfile = 7003,
    /// Invalid config value
    InvalidConfigValue = 7004,

    // Internal errors (9xxx)
    /// Internal error
    InternalError = 9000,
    /// Not implemented
    NotImplemented = 9001,
    /// Unexpected state
    UnexpectedState = 9002,
}

impl ErrorCode {
    /// Get the numeric code value
    pub fn code(&self) -> u32 {
        *self as u32
    }

    /// Get a short description of the error code
    pub fn description(&self) -> &'static str {
        match self {
            // Parse errors
            ErrorCode::ParseError => "Parse error",
            ErrorCode::InvalidN3Syntax => "Invalid N3 syntax",
            ErrorCode::InvalidRdfXml => "Invalid RDF/XML syntax",
            ErrorCode::InvalidSparql => "Invalid SPARQL syntax",
            ErrorCode::InvalidUri => "Invalid URI",
            ErrorCode::InvalidLiteral => "Invalid literal value",
            ErrorCode::UnexpectedEof => "Unexpected end of input",
            ErrorCode::InvalidPrefix => "Invalid prefix declaration",

            // Reasoning errors
            ErrorCode::ReasoningError => "Reasoning error",
            ErrorCode::MaxStepsExceeded => "Maximum inference steps exceeded",
            ErrorCode::InfiniteLoop => "Infinite loop detected",
            ErrorCode::UnstratifiableNegation => "Unstratifiable negation",
            ErrorCode::BuiltinFailed => "Builtin evaluation failed",
            ErrorCode::RuleApplicationFailed => "Rule application failed",
            ErrorCode::ProofGenerationFailed => "Proof generation failed",

            // Store errors
            ErrorCode::StoreError => "Store error",
            ErrorCode::TripleNotFound => "Triple not found",
            ErrorCode::DuplicateTriple => "Duplicate triple",
            ErrorCode::CapacityExceeded => "Store capacity exceeded",
            ErrorCode::TransactionFailed => "Transaction failed",
            ErrorCode::ConnectionFailed => "Connection failed",

            // Network errors
            ErrorCode::NetworkError => "Network error",
            ErrorCode::ConnectionTimeout => "Connection timeout",
            ErrorCode::RequestTimeout => "Request timeout",
            ErrorCode::DnsResolutionFailed => "DNS resolution failed",
            ErrorCode::TlsError => "SSL/TLS error",
            ErrorCode::HttpError => "HTTP error",
            ErrorCode::NetworkDisabled => "Network disabled by policy",
            ErrorCode::DomainNotAllowed => "Domain not allowed",

            // Validation errors
            ErrorCode::ValidationError => "Validation error",
            ErrorCode::EmptyInput => "Empty input",
            ErrorCode::InputTooLarge => "Input too large",
            ErrorCode::InvalidFormat => "Invalid format",
            ErrorCode::MissingRequired => "Missing required field",
            ErrorCode::InvalidValue => "Invalid value",
            ErrorCode::ConstraintViolation => "Constraint violation",

            // Security errors
            ErrorCode::SecurityError => "Security error",
            ErrorCode::RateLimitExceeded => "Rate limit exceeded",
            ErrorCode::Unauthorized => "Unauthorized",
            ErrorCode::Forbidden => "Forbidden",
            ErrorCode::CryptoFailed => "Crypto operation failed",
            ErrorCode::FileAccessDenied => "File access denied",

            // Config errors
            ErrorCode::ConfigError => "Configuration error",
            ErrorCode::ConfigNotFound => "Configuration file not found",
            ErrorCode::InvalidConfigSyntax => "Invalid configuration syntax",
            ErrorCode::UnknownProfile => "Unknown profile",
            ErrorCode::InvalidConfigValue => "Invalid configuration value",

            // Internal errors
            ErrorCode::InternalError => "Internal error",
            ErrorCode::NotImplemented => "Not implemented",
            ErrorCode::UnexpectedState => "Unexpected state",
        }
    }

    /// Get the HTTP status code for this error
    pub fn http_status(&self) -> u16 {
        match self {
            // Parse/Validation errors -> 400 Bad Request
            ErrorCode::ParseError
            | ErrorCode::InvalidN3Syntax
            | ErrorCode::InvalidRdfXml
            | ErrorCode::InvalidSparql
            | ErrorCode::InvalidUri
            | ErrorCode::InvalidLiteral
            | ErrorCode::UnexpectedEof
            | ErrorCode::InvalidPrefix
            | ErrorCode::ValidationError
            | ErrorCode::EmptyInput
            | ErrorCode::InputTooLarge
            | ErrorCode::InvalidFormat
            | ErrorCode::MissingRequired
            | ErrorCode::InvalidValue
            | ErrorCode::ConstraintViolation => 400,

            // Security errors
            ErrorCode::Unauthorized => 401,
            ErrorCode::Forbidden
            | ErrorCode::FileAccessDenied
            | ErrorCode::NetworkDisabled
            | ErrorCode::DomainNotAllowed => 403,
            ErrorCode::RateLimitExceeded => 429,

            // Not found
            ErrorCode::TripleNotFound | ErrorCode::ConfigNotFound => 404,

            // Timeout errors
            ErrorCode::ConnectionTimeout | ErrorCode::RequestTimeout => 408,

            // Conflict
            ErrorCode::DuplicateTriple => 409,

            // Payload too large
            ErrorCode::CapacityExceeded => 413,

            // Unprocessable entity (reasoning failures)
            ErrorCode::ReasoningError
            | ErrorCode::MaxStepsExceeded
            | ErrorCode::InfiniteLoop
            | ErrorCode::UnstratifiableNegation
            | ErrorCode::BuiltinFailed
            | ErrorCode::RuleApplicationFailed
            | ErrorCode::ProofGenerationFailed => 422,

            // Internal server errors
            ErrorCode::StoreError
            | ErrorCode::TransactionFailed
            | ErrorCode::ConnectionFailed
            | ErrorCode::NetworkError
            | ErrorCode::DnsResolutionFailed
            | ErrorCode::TlsError
            | ErrorCode::HttpError
            | ErrorCode::SecurityError
            | ErrorCode::CryptoFailed
            | ErrorCode::ConfigError
            | ErrorCode::InvalidConfigSyntax
            | ErrorCode::UnknownProfile
            | ErrorCode::InvalidConfigValue
            | ErrorCode::InternalError
            | ErrorCode::UnexpectedState => 500,

            // Not implemented
            ErrorCode::NotImplemented => 501,
        }
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

// ============================================================================
// Error Context
// ============================================================================

/// Additional context information for an error
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ErrorContext {
    /// Key-value pairs of context information
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub fields: HashMap<String, String>,
    /// Source location (file:line)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub location: Option<String>,
    /// Stack of error causes
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub causes: Vec<String>,
}

impl ErrorContext {
    /// Create a new empty context
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a field to the context
    pub fn field(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.fields.insert(key.into(), value.into());
        self
    }

    /// Add source location
    pub fn at(mut self, location: impl Into<String>) -> Self {
        self.location = Some(location.into());
        self
    }

    /// Add a cause to the error chain
    pub fn cause(mut self, cause: impl Into<String>) -> Self {
        self.causes.push(cause.into());
        self
    }
}

// ============================================================================
// Main Error Type
// ============================================================================

/// The main error type for cwm-rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CwmError {
    /// Error code for programmatic handling
    pub code: ErrorCode,
    /// Human-readable error message
    pub message: String,
    /// Additional context
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub context: Option<ErrorContext>,
    /// Hint for resolving the error
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hint: Option<String>,
}

impl CwmError {
    /// Create a new error with a code and message
    pub fn new(code: ErrorCode, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            context: None,
            hint: None,
        }
    }

    // ========================================================================
    // Factory methods for common error types
    // ========================================================================

    /// Create a parse error
    pub fn parse(message: impl Into<String>) -> Self {
        Self::new(ErrorCode::ParseError, message)
    }

    /// Create an N3 syntax error
    pub fn n3_syntax(message: impl Into<String>) -> Self {
        Self::new(ErrorCode::InvalidN3Syntax, message)
    }

    /// Create a SPARQL syntax error
    pub fn sparql_syntax(message: impl Into<String>) -> Self {
        Self::new(ErrorCode::InvalidSparql, message)
    }

    /// Create a reasoning error
    pub fn reasoning(message: impl Into<String>) -> Self {
        Self::new(ErrorCode::ReasoningError, message)
    }

    /// Create a max steps exceeded error
    pub fn max_steps(steps: usize, limit: usize) -> Self {
        Self::new(
            ErrorCode::MaxStepsExceeded,
            format!("Inference stopped after {} steps (limit: {})", steps, limit),
        )
    }

    /// Create a store error
    pub fn store(message: impl Into<String>) -> Self {
        Self::new(ErrorCode::StoreError, message)
    }

    /// Create a network error
    pub fn network(message: impl Into<String>) -> Self {
        Self::new(ErrorCode::NetworkError, message)
    }

    /// Create a timeout error
    pub fn timeout(message: impl Into<String>) -> Self {
        Self::new(ErrorCode::RequestTimeout, message)
    }

    /// Create a validation error
    pub fn validation(message: impl Into<String>) -> Self {
        Self::new(ErrorCode::ValidationError, message)
    }

    /// Create an empty input error
    pub fn empty_input(field: &str) -> Self {
        Self::new(ErrorCode::EmptyInput, format!("{} cannot be empty", field))
    }

    /// Create an input too large error
    pub fn input_too_large(size: usize, limit: usize) -> Self {
        Self::new(
            ErrorCode::InputTooLarge,
            format!("Input size {} exceeds limit {}", size, limit),
        )
    }

    /// Create a security error
    pub fn security(message: impl Into<String>) -> Self {
        Self::new(ErrorCode::SecurityError, message)
    }

    /// Create a rate limit error
    pub fn rate_limited() -> Self {
        Self::new(
            ErrorCode::RateLimitExceeded,
            "Rate limit exceeded. Please try again later.",
        )
    }

    /// Create a forbidden error
    pub fn forbidden(message: impl Into<String>) -> Self {
        Self::new(ErrorCode::Forbidden, message)
    }

    /// Create a config error
    pub fn config(message: impl Into<String>) -> Self {
        Self::new(ErrorCode::ConfigError, message)
    }

    /// Create an internal error
    pub fn internal(message: impl Into<String>) -> Self {
        Self::new(ErrorCode::InternalError, message)
    }

    /// Create a not implemented error
    pub fn not_implemented(feature: &str) -> Self {
        Self::new(
            ErrorCode::NotImplemented,
            format!("{} is not yet implemented", feature),
        )
    }

    // ========================================================================
    // Builder methods
    // ========================================================================

    /// Set the error code
    pub fn with_code(mut self, code: ErrorCode) -> Self {
        self.code = code;
        self
    }

    /// Add context to the error
    pub fn with_context(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        let ctx = self.context.get_or_insert_with(ErrorContext::new);
        ctx.fields.insert(key.into(), value.into());
        self
    }

    /// Add a cause to the error chain
    pub fn with_cause(mut self, cause: impl Into<String>) -> Self {
        let ctx = self.context.get_or_insert_with(ErrorContext::new);
        ctx.causes.push(cause.into());
        self
    }

    /// Add source location
    pub fn at(mut self, location: impl Into<String>) -> Self {
        let ctx = self.context.get_or_insert_with(ErrorContext::new);
        ctx.location = Some(location.into());
        self
    }

    /// Add a hint for resolving the error
    pub fn with_hint(mut self, hint: impl Into<String>) -> Self {
        self.hint = Some(hint.into());
        self
    }

    // ========================================================================
    // Accessors
    // ========================================================================

    /// Get the HTTP status code for this error
    pub fn http_status(&self) -> u16 {
        self.code.http_status()
    }

    /// Check if this is a client error (4xx)
    pub fn is_client_error(&self) -> bool {
        let status = self.http_status();
        (400..500).contains(&status)
    }

    /// Check if this is a server error (5xx)
    pub fn is_server_error(&self) -> bool {
        let status = self.http_status();
        (500..600).contains(&status)
    }

    /// Convert to JSON string
    pub fn to_json(&self) -> String {
        serde_json::to_string(self).unwrap_or_else(|_| {
            format!(r#"{{"code":"INTERNAL_ERROR","message":"{}"}}"#, self.message)
        })
    }

    /// Convert to pretty JSON string
    pub fn to_json_pretty(&self) -> String {
        serde_json::to_string_pretty(self).unwrap_or_else(|_| self.to_json())
    }
}

impl fmt::Display for CwmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}", self.code.code(), self.message)?;

        if let Some(ref ctx) = self.context {
            if let Some(ref loc) = ctx.location {
                write!(f, " at {}", loc)?;
            }
            if !ctx.causes.is_empty() {
                write!(f, "\nCaused by:")?;
                for cause in &ctx.causes {
                    write!(f, "\n  - {}", cause)?;
                }
            }
        }

        if let Some(ref hint) = self.hint {
            write!(f, "\nHint: {}", hint)?;
        }

        Ok(())
    }
}

impl std::error::Error for CwmError {}

// ============================================================================
// Conversions from other error types
// ============================================================================

impl From<std::io::Error> for CwmError {
    fn from(err: std::io::Error) -> Self {
        use std::io::ErrorKind;
        let code = match err.kind() {
            ErrorKind::NotFound => ErrorCode::ConfigNotFound,
            ErrorKind::PermissionDenied => ErrorCode::FileAccessDenied,
            ErrorKind::TimedOut => ErrorCode::RequestTimeout,
            ErrorKind::ConnectionRefused | ErrorKind::ConnectionReset => ErrorCode::ConnectionFailed,
            _ => ErrorCode::InternalError,
        };
        CwmError::new(code, err.to_string())
    }
}

impl From<serde_json::Error> for CwmError {
    fn from(err: serde_json::Error) -> Self {
        CwmError::parse(err.to_string())
            .with_code(ErrorCode::InvalidFormat)
            .with_context("format", "JSON")
    }
}

impl From<toml::de::Error> for CwmError {
    fn from(err: toml::de::Error) -> Self {
        CwmError::config(err.to_string())
            .with_code(ErrorCode::InvalidConfigSyntax)
    }
}

// ============================================================================
// Result type alias
// ============================================================================

/// A Result type using CwmError
pub type CwmResult<T> = Result<T, CwmError>;

// ============================================================================
// Error response for HTTP APIs
// ============================================================================

/// Structured error response for HTTP APIs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorResponse {
    /// Error indicator
    pub error: bool,
    /// Error code (string form)
    pub code: String,
    /// Numeric error code
    pub code_num: u32,
    /// HTTP status code
    pub status: u16,
    /// Error message
    pub message: String,
    /// Additional details
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub details: Option<HashMap<String, String>>,
    /// Hint for resolution
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hint: Option<String>,
}

impl From<&CwmError> for ErrorResponse {
    fn from(err: &CwmError) -> Self {
        Self {
            error: true,
            code: format!("{:?}", err.code),
            code_num: err.code.code(),
            status: err.http_status(),
            message: err.message.clone(),
            details: err.context.as_ref().map(|c| c.fields.clone()),
            hint: err.hint.clone(),
        }
    }
}

impl From<CwmError> for ErrorResponse {
    fn from(err: CwmError) -> Self {
        Self::from(&err)
    }
}

impl ErrorResponse {
    /// Convert to JSON string
    pub fn to_json(&self) -> String {
        serde_json::to_string(self).unwrap_or_else(|_| {
            format!(r#"{{"error":true,"message":"{}"}}"#, self.message)
        })
    }
}

// ============================================================================
// Macros for convenient error creation
// ============================================================================

/// Create a CwmError with context from the current location
#[macro_export]
macro_rules! cwm_error {
    ($code:expr, $msg:expr) => {
        $crate::error::CwmError::new($code, $msg)
            .at(format!("{}:{}", file!(), line!()))
    };
    ($code:expr, $fmt:expr, $($arg:tt)*) => {
        $crate::error::CwmError::new($code, format!($fmt, $($arg)*))
            .at(format!("{}:{}", file!(), line!()))
    };
}

/// Bail out early with an error
#[macro_export]
macro_rules! cwm_bail {
    ($code:expr, $msg:expr) => {
        return Err($crate::cwm_error!($code, $msg))
    };
    ($code:expr, $fmt:expr, $($arg:tt)*) => {
        return Err($crate::cwm_error!($code, $fmt, $($arg)*))
    };
}

/// Ensure a condition holds, or return an error
#[macro_export]
macro_rules! cwm_ensure {
    ($cond:expr, $code:expr, $msg:expr) => {
        if !$cond {
            $crate::cwm_bail!($code, $msg);
        }
    };
    ($cond:expr, $code:expr, $fmt:expr, $($arg:tt)*) => {
        if !$cond {
            $crate::cwm_bail!($code, $fmt, $($arg)*);
        }
    };
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let err = CwmError::validation("test error");
        assert_eq!(err.code, ErrorCode::ValidationError);
        assert_eq!(err.message, "test error");
    }

    #[test]
    fn test_error_with_context() {
        let err = CwmError::parse("syntax error")
            .with_context("line", "42")
            .with_context("column", "10");

        assert!(err.context.is_some());
        let ctx = err.context.as_ref().unwrap();
        assert_eq!(ctx.fields.get("line"), Some(&"42".to_string()));
        assert_eq!(ctx.fields.get("column"), Some(&"10".to_string()));
    }

    #[test]
    fn test_error_with_cause() {
        let err = CwmError::store("failed to write")
            .with_cause("disk full")
            .with_cause("no space left");

        let ctx = err.context.as_ref().unwrap();
        assert_eq!(ctx.causes.len(), 2);
    }

    #[test]
    fn test_error_with_hint() {
        let err = CwmError::config("unknown profile")
            .with_hint("Available profiles: default, rdfs, owl");

        assert_eq!(
            err.hint,
            Some("Available profiles: default, rdfs, owl".to_string())
        );
    }

    #[test]
    fn test_error_http_status() {
        assert_eq!(CwmError::validation("test").http_status(), 400);
        assert_eq!(CwmError::rate_limited().http_status(), 429);
        assert_eq!(CwmError::internal("test").http_status(), 500);
    }

    #[test]
    fn test_error_is_client_error() {
        assert!(CwmError::validation("test").is_client_error());
        assert!(!CwmError::internal("test").is_client_error());
    }

    #[test]
    fn test_error_is_server_error() {
        assert!(!CwmError::validation("test").is_server_error());
        assert!(CwmError::internal("test").is_server_error());
    }

    #[test]
    fn test_error_to_json() {
        let err = CwmError::validation("test error");
        let json = err.to_json();
        assert!(json.contains("ValidationError") || json.contains("VALIDATION_ERROR"));
        assert!(json.contains("test error"));
    }

    #[test]
    fn test_error_display() {
        let err = CwmError::parse("syntax error")
            .at("input.n3:42")
            .with_cause("unexpected token")
            .with_hint("Check your syntax");

        let display = err.to_string();
        assert!(display.contains("[1000]"));
        assert!(display.contains("syntax error"));
        assert!(display.contains("input.n3:42"));
        assert!(display.contains("unexpected token"));
        assert!(display.contains("Check your syntax"));
    }

    #[test]
    fn test_error_response_from_error() {
        let err = CwmError::validation("invalid input")
            .with_context("field", "name");

        let resp = ErrorResponse::from(&err);
        assert!(resp.error);
        assert_eq!(resp.status, 400);
        assert_eq!(resp.message, "invalid input");
        assert!(resp.details.is_some());
    }

    #[test]
    fn test_error_code_description() {
        assert_eq!(ErrorCode::ParseError.description(), "Parse error");
        assert_eq!(
            ErrorCode::RateLimitExceeded.description(),
            "Rate limit exceeded"
        );
    }

    #[test]
    fn test_max_steps_error() {
        let err = CwmError::max_steps(10000, 10000);
        assert_eq!(err.code, ErrorCode::MaxStepsExceeded);
        assert!(err.message.contains("10000"));
    }

    #[test]
    fn test_empty_input_error() {
        let err = CwmError::empty_input("query");
        assert_eq!(err.code, ErrorCode::EmptyInput);
        assert!(err.message.contains("query"));
    }

    #[test]
    fn test_input_too_large_error() {
        let err = CwmError::input_too_large(2000000, 1000000);
        assert_eq!(err.code, ErrorCode::InputTooLarge);
        assert!(err.message.contains("2000000"));
        assert!(err.message.contains("1000000"));
    }
}
