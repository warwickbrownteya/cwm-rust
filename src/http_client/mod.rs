//! HTTP Client Module with Connection Pooling
//!
//! Provides shared HTTP clients for efficient connection reuse across the application.
//! Supports both synchronous (ureq) and asynchronous (reqwest) operations.
//!
//! # Features
//!
//! - Connection pooling for better performance
//! - Configurable timeouts and retry policies
//! - Domain allowlisting for security
//! - Shared global clients for resource efficiency
//!
//! # Example
//!
//! ```rust,ignore
//! use cwm::http_client::{get_sync_client, get_async_client};
//!
//! // Synchronous request
//! let client = get_sync_client();
//! let response = client.get("https://example.org/data.ttl").call()?;
//!
//! // Asynchronous request
//! let client = get_async_client();
//! let response = client.get("https://example.org/data.ttl").send().await?;
//! ```

use std::sync::OnceLock;
use std::time::Duration;

// ============================================================================
// Configuration
// ============================================================================

/// HTTP client configuration
#[derive(Debug, Clone)]
pub struct HttpClientConfig {
    /// Connection timeout in seconds
    pub connect_timeout_secs: u64,
    /// Request timeout in seconds
    pub request_timeout_secs: u64,
    /// Maximum idle connections per host
    pub pool_idle_per_host: usize,
    /// Idle connection timeout in seconds
    pub pool_idle_timeout_secs: u64,
    /// User agent string
    pub user_agent: String,
    /// Enable following redirects
    pub follow_redirects: bool,
    /// Maximum number of redirects to follow
    pub max_redirects: u32,
}

impl Default for HttpClientConfig {
    fn default() -> Self {
        Self {
            connect_timeout_secs: 10,
            request_timeout_secs: 30,
            pool_idle_per_host: 10,
            pool_idle_timeout_secs: 90,
            user_agent: format!("cwm-rust/{}", env!("CARGO_PKG_VERSION")),
            follow_redirects: true,
            max_redirects: 10,
        }
    }
}

// ============================================================================
// Synchronous Client (ureq)
// ============================================================================

/// Global synchronous HTTP client with connection pooling
static SYNC_CLIENT: OnceLock<ureq::Agent> = OnceLock::new();

/// Get the shared synchronous HTTP client
///
/// This client uses connection pooling for efficient reuse of connections.
/// The first call initializes the client with default settings.
pub fn get_sync_client() -> &'static ureq::Agent {
    SYNC_CLIENT.get_or_init(|| {
        create_sync_client(&HttpClientConfig::default())
    })
}

/// Create a synchronous HTTP client with custom configuration
pub fn create_sync_client(config: &HttpClientConfig) -> ureq::Agent {
    ureq::AgentBuilder::new()
        .timeout_connect(Duration::from_secs(config.connect_timeout_secs))
        .timeout(Duration::from_secs(config.request_timeout_secs))
        .user_agent(&config.user_agent)
        .redirects(if config.follow_redirects { config.max_redirects } else { 0 })
        .build()
}

/// Create a synchronous HTTP client from CwmConfig settings
pub fn create_sync_client_from_config(security: &crate::config::SecurityConfig) -> ureq::Agent {
    let config = HttpClientConfig {
        request_timeout_secs: security.http_timeout_secs,
        ..Default::default()
    };
    create_sync_client(&config)
}

// ============================================================================
// Asynchronous Client (reqwest)
// ============================================================================

/// Global asynchronous HTTP client with connection pooling
static ASYNC_CLIENT: OnceLock<reqwest::Client> = OnceLock::new();

/// Get the shared asynchronous HTTP client
///
/// This client uses connection pooling for efficient reuse of connections.
/// The first call initializes the client with default settings.
pub fn get_async_client() -> &'static reqwest::Client {
    ASYNC_CLIENT.get_or_init(|| {
        create_async_client(&HttpClientConfig::default())
            .expect("Failed to create async HTTP client")
    })
}

/// Create an asynchronous HTTP client with custom configuration
pub fn create_async_client(config: &HttpClientConfig) -> Result<reqwest::Client, reqwest::Error> {
    reqwest::Client::builder()
        .connect_timeout(Duration::from_secs(config.connect_timeout_secs))
        .timeout(Duration::from_secs(config.request_timeout_secs))
        .pool_idle_timeout(Duration::from_secs(config.pool_idle_timeout_secs))
        .pool_max_idle_per_host(config.pool_idle_per_host)
        .user_agent(&config.user_agent)
        .redirect(if config.follow_redirects {
            reqwest::redirect::Policy::limited(config.max_redirects as usize)
        } else {
            reqwest::redirect::Policy::none()
        })
        .build()
}

/// Create an asynchronous HTTP client from CwmConfig settings
pub fn create_async_client_from_config(security: &crate::config::SecurityConfig) -> Result<reqwest::Client, reqwest::Error> {
    let config = HttpClientConfig {
        request_timeout_secs: security.http_timeout_secs,
        ..Default::default()
    };
    create_async_client(&config)
}

// ============================================================================
// Domain Validation
// ============================================================================

/// Check if a URL's domain is allowed based on security configuration
pub fn is_domain_allowed(url: &str, allowed_domains: &[String]) -> bool {
    // If no domains specified, allow all
    if allowed_domains.is_empty() {
        return true;
    }

    // Parse the URL to extract host
    let host = extract_host(url);
    if host.is_empty() {
        return false;
    }

    // Check if host matches any allowed domain
    for domain in allowed_domains {
        if host == domain.as_str() {
            return true;
        }
        // Allow subdomains (e.g., "example.org" allows "api.example.org")
        if host.ends_with(&format!(".{}", domain)) {
            return true;
        }
    }

    false
}

/// Extract host from URL
fn extract_host(url: &str) -> String {
    // Simple URL parsing without external crate
    let url = url.trim();

    // Remove protocol
    let without_proto = if let Some(pos) = url.find("://") {
        &url[pos + 3..]
    } else {
        url
    };

    // Get host (before path, query, or port)
    let host_end = without_proto
        .find('/')
        .or_else(|| without_proto.find('?'))
        .or_else(|| without_proto.find('#'))
        .unwrap_or(without_proto.len());

    let host_with_port = &without_proto[..host_end];

    // Remove port if present
    if let Some(pos) = host_with_port.rfind(':') {
        // Check if it's actually a port (all digits after colon)
        let after_colon = &host_with_port[pos + 1..];
        if after_colon.chars().all(|c| c.is_ascii_digit()) {
            return host_with_port[..pos].to_lowercase();
        }
    }

    host_with_port.to_lowercase()
}

// ============================================================================
// Convenience Functions
// ============================================================================

/// Perform a GET request with the shared sync client
pub fn sync_get(url: &str) -> Result<String, HttpError> {
    let client = get_sync_client();
    let response = client.get(url).call()
        .map_err(|e| HttpError::RequestFailed(e.to_string()))?;

    if response.status() != 200 {
        return Err(HttpError::BadStatus(response.status()));
    }

    response.into_string()
        .map_err(|e| HttpError::ReadFailed(e.to_string()))
}

/// Perform a GET request with the shared async client
pub async fn async_get(url: &str) -> Result<String, HttpError> {
    let client = get_async_client();
    let response = client.get(url).send().await
        .map_err(|e| HttpError::RequestFailed(e.to_string()))?;

    if !response.status().is_success() {
        return Err(HttpError::BadStatus(response.status().as_u16()));
    }

    response.text().await
        .map_err(|e| HttpError::ReadFailed(e.to_string()))
}

// ============================================================================
// Error Types
// ============================================================================

/// HTTP client errors
#[derive(Debug, Clone)]
pub enum HttpError {
    /// Request failed to send
    RequestFailed(String),
    /// Server returned non-success status
    BadStatus(u16),
    /// Failed to read response body
    ReadFailed(String),
    /// Domain not in allowlist
    DomainNotAllowed(String),
    /// Network disabled
    NetworkDisabled,
}

impl std::fmt::Display for HttpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HttpError::RequestFailed(msg) => write!(f, "HTTP request failed: {}", msg),
            HttpError::BadStatus(code) => write!(f, "HTTP status {}", code),
            HttpError::ReadFailed(msg) => write!(f, "Failed to read response: {}", msg),
            HttpError::DomainNotAllowed(domain) => write!(f, "Domain not allowed: {}", domain),
            HttpError::NetworkDisabled => write!(f, "Network access is disabled"),
        }
    }
}

impl std::error::Error for HttpError {}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_host() {
        assert_eq!(extract_host("https://example.org/path"), "example.org");
        assert_eq!(extract_host("http://api.example.org:8080/path"), "api.example.org");
        assert_eq!(extract_host("https://example.org"), "example.org");
        assert_eq!(extract_host("https://Example.ORG/Path"), "example.org");
        assert_eq!(extract_host("http://localhost:3000"), "localhost");
    }

    #[test]
    fn test_domain_allowed_empty() {
        // Empty allowlist means all domains are allowed
        assert!(is_domain_allowed("https://example.org", &[]));
        assert!(is_domain_allowed("https://any.domain.com", &[]));
    }

    #[test]
    fn test_domain_allowed_exact() {
        let allowed = vec!["example.org".to_string()];
        assert!(is_domain_allowed("https://example.org/path", &allowed));
        assert!(!is_domain_allowed("https://other.org/path", &allowed));
    }

    #[test]
    fn test_domain_allowed_subdomain() {
        let allowed = vec!["example.org".to_string()];
        assert!(is_domain_allowed("https://api.example.org/path", &allowed));
        assert!(is_domain_allowed("https://sub.api.example.org/path", &allowed));
        assert!(!is_domain_allowed("https://notexample.org/path", &allowed));
    }

    #[test]
    fn test_default_config() {
        let config = HttpClientConfig::default();
        assert_eq!(config.connect_timeout_secs, 10);
        assert_eq!(config.request_timeout_secs, 30);
        assert!(config.follow_redirects);
    }

    #[test]
    fn test_sync_client_creation() {
        let config = HttpClientConfig::default();
        let _client = create_sync_client(&config);
        // Client created successfully
    }

    #[test]
    fn test_async_client_creation() {
        let config = HttpClientConfig::default();
        let client = create_async_client(&config);
        assert!(client.is_ok());
    }

    #[test]
    fn test_get_sync_client_singleton() {
        let client1 = get_sync_client();
        let client2 = get_sync_client();
        // Both should be the same instance
        assert!(std::ptr::eq(client1, client2));
    }

    #[test]
    fn test_get_async_client_singleton() {
        let client1 = get_async_client();
        let client2 = get_async_client();
        // Both should be the same instance
        assert!(std::ptr::eq(client1, client2));
    }
}
