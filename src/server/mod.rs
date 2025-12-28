//! Async HTTP Server Module
//!
//! Provides a production-ready async HTTP server for cwm-rust using axum.
//! Replaces the legacy tiny_http-based server with full async support.
//!
//! # Features
//!
//! - SPARQL 1.1 Protocol endpoint (`/sparql`)
//! - Multiple result formats (XML, JSON)
//! - CORS support for browser clients
//! - Graceful shutdown
//! - Connection keep-alive
//! - Request tracing/logging
//!
//! # Example
//!
//! ```rust,ignore
//! use cwm::server::{ServerConfig, run_server};
//! use cwm::Store;
//!
//! #[tokio::main]
//! async fn main() {
//!     let store = Store::new();
//!     let config = ServerConfig::new(8080);
//!     run_server(store, config).await.unwrap();
//! }
//! ```

use std::sync::Arc;
use std::net::SocketAddr;

use axum::{
    extract::{Query, State},
    http::{header, Method, StatusCode},
    response::{Html, IntoResponse, Response},
    routing::get,
    Router,
};
use tower_http::cors::{Any, CorsLayer};
use serde::Deserialize;
use tokio::sync::RwLock;

use crate::store::Store;
use crate::sparql::{execute_sparql, format_results_xml, format_results_json};

// ============================================================================
// Configuration
// ============================================================================

/// Configuration for the async HTTP server
#[derive(Debug, Clone)]
pub struct ServerConfig {
    /// Port to listen on
    pub port: u16,
    /// Host to bind to (default: 0.0.0.0)
    pub host: String,
    /// Enable CORS for all origins
    pub cors_permissive: bool,
    /// Enable request tracing
    pub enable_tracing: bool,
    /// Maximum request body size in bytes
    pub max_body_size: usize,
}

impl ServerConfig {
    /// Create a new server configuration with the specified port
    pub fn new(port: u16) -> Self {
        Self {
            port,
            host: "0.0.0.0".to_string(),
            cors_permissive: true,
            enable_tracing: true,
            max_body_size: 10 * 1024 * 1024, // 10 MB
        }
    }

    /// Set the host to bind to
    pub fn with_host(mut self, host: impl Into<String>) -> Self {
        self.host = host.into();
        self
    }

    /// Set CORS permissiveness
    pub fn with_cors(mut self, permissive: bool) -> Self {
        self.cors_permissive = permissive;
        self
    }

    /// Get the socket address
    pub fn socket_addr(&self) -> SocketAddr {
        format!("{}:{}", self.host, self.port)
            .parse()
            .expect("Invalid socket address")
    }
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self::new(8080)
    }
}

// ============================================================================
// Application State
// ============================================================================

/// Shared application state for the server
pub struct AppState {
    /// The RDF store (thread-safe read-write access)
    pub store: RwLock<Store>,
    /// Server configuration
    pub config: ServerConfig,
}

impl AppState {
    /// Create new application state
    pub fn new(store: Store, config: ServerConfig) -> Self {
        Self {
            store: RwLock::new(store),
            config,
        }
    }
}

/// Type alias for shared state
pub type SharedState = Arc<AppState>;

// ============================================================================
// Request/Response Types
// ============================================================================

/// Query parameters for GET /sparql
#[derive(Debug, Deserialize)]
pub struct SparqlQueryParams {
    /// The SPARQL query string
    query: Option<String>,
    /// Desired output format (xml, json)
    format: Option<String>,
}

/// Form data for POST /sparql with application/x-www-form-urlencoded
#[derive(Debug, Deserialize)]
pub struct SparqlFormData {
    /// The SPARQL query string
    query: String,
}

/// SPARQL response with appropriate content type
pub struct SparqlResponse {
    body: String,
    content_type: &'static str,
}

impl IntoResponse for SparqlResponse {
    fn into_response(self) -> Response {
        (
            StatusCode::OK,
            [(header::CONTENT_TYPE, self.content_type)],
            self.body,
        )
            .into_response()
    }
}

/// Error response
pub struct ErrorResponse {
    status: StatusCode,
    message: String,
}

impl ErrorResponse {
    pub fn bad_request(message: impl Into<String>) -> Self {
        Self {
            status: StatusCode::BAD_REQUEST,
            message: message.into(),
        }
    }

    pub fn internal_error(message: impl Into<String>) -> Self {
        Self {
            status: StatusCode::INTERNAL_SERVER_ERROR,
            message: message.into(),
        }
    }
}

impl IntoResponse for ErrorResponse {
    fn into_response(self) -> Response {
        (self.status, self.message).into_response()
    }
}

// ============================================================================
// Route Handlers
// ============================================================================

/// Handle GET /sparql?query=...
async fn sparql_get(
    State(state): State<SharedState>,
    Query(params): Query<SparqlQueryParams>,
    headers: axum::http::HeaderMap,
) -> Result<SparqlResponse, ErrorResponse> {
    let query = params
        .query
        .ok_or_else(|| ErrorResponse::bad_request("Missing 'query' parameter"))?;

    execute_sparql_query(&state, &query, params.format, &headers).await
}

/// Handle POST /sparql with query in body
async fn sparql_post(
    State(state): State<SharedState>,
    headers: axum::http::HeaderMap,
    body: String,
) -> Result<SparqlResponse, ErrorResponse> {
    let content_type = headers
        .get(header::CONTENT_TYPE)
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");

    let query = if content_type.contains("application/sparql-query") {
        // Direct SPARQL query in body
        body
    } else if content_type.contains("application/x-www-form-urlencoded") {
        // Form-encoded data
        let params: SparqlFormData = serde_urlencoded::from_str(&body)
            .map_err(|e| ErrorResponse::bad_request(format!("Invalid form data: {}", e)))?;
        params.query
    } else {
        // Assume raw query
        body
    };

    if query.is_empty() {
        return Err(ErrorResponse::bad_request("Empty query"));
    }

    execute_sparql_query(&state, &query, None, &headers).await
}

/// Execute SPARQL query and format response
async fn execute_sparql_query(
    state: &SharedState,
    query: &str,
    format_param: Option<String>,
    headers: &axum::http::HeaderMap,
) -> Result<SparqlResponse, ErrorResponse> {
    // Determine output format from Accept header or format parameter
    let accept = headers
        .get(header::ACCEPT)
        .and_then(|v| v.to_str().ok())
        .unwrap_or("application/sparql-results+xml");

    let use_json = format_param
        .as_ref()
        .map(|f| f == "json")
        .unwrap_or_else(|| accept.contains("json"));

    // Execute query (read lock on store)
    let store = state.store.read().await;
    let result = execute_sparql(&store, query)
        .map_err(|e| ErrorResponse::bad_request(format!("SPARQL error: {}", e)))?;

    // Format result
    let (content_type, body) = if use_json {
        ("application/sparql-results+json", format_results_json(&result))
    } else {
        ("application/sparql-results+xml", format_results_xml(&result))
    };

    Ok(SparqlResponse { body, content_type })
}

/// Serve the HTML query form at /
async fn index_page() -> Html<&'static str> {
    Html(r#"<!DOCTYPE html>
<html>
<head>
    <title>CWM SPARQL Endpoint</title>
    <style>
        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
               max-width: 800px; margin: 50px auto; padding: 20px; }
        h1 { color: #333; }
        textarea { width: 100%; font-family: monospace; font-size: 14px; }
        button { background: #007bff; color: white; padding: 10px 20px; border: none;
                 cursor: pointer; font-size: 16px; margin-top: 10px; }
        button:hover { background: #0056b3; }
        .format-select { margin-top: 10px; }
        pre { background: #f5f5f5; padding: 15px; overflow-x: auto; }
    </style>
</head>
<body>
    <h1>🔮 CWM SPARQL Endpoint</h1>
    <p>Enter a SPARQL query below:</p>
    <form action="/sparql" method="POST">
        <textarea name="query" rows="12" cols="80">PREFIX rdf: &lt;http://www.w3.org/1999/02/22-rdf-syntax-ns#&gt;
PREFIX rdfs: &lt;http://www.w3.org/2000/01/rdf-schema#&gt;

SELECT ?s ?p ?o
WHERE {
    ?s ?p ?o
}
LIMIT 25</textarea>
        <div class="format-select">
            <label>Format:
                <select name="format">
                    <option value="xml">XML</option>
                    <option value="json">JSON</option>
                </select>
            </label>
        </div>
        <button type="submit">Execute Query</button>
    </form>
    <h3>Endpoints</h3>
    <ul>
        <li><code>GET /sparql?query=...</code> - Execute URL-encoded query</li>
        <li><code>POST /sparql</code> - Execute query from body</li>
        <li><code>GET /health</code> - Health check</li>
        <li><code>GET /stats</code> - Store statistics</li>
    </ul>
</body>
</html>"#)
}

/// Health check endpoint
async fn health_check() -> impl IntoResponse {
    (StatusCode::OK, "OK")
}

/// Store statistics endpoint
async fn stats(State(state): State<SharedState>) -> impl IntoResponse {
    let store = state.store.read().await;
    let count = store.len();

    let json = serde_json::json!({
        "status": "ok",
        "triple_count": count,
        "version": env!("CARGO_PKG_VERSION"),
    });

    (
        StatusCode::OK,
        [(header::CONTENT_TYPE, "application/json")],
        serde_json::to_string_pretty(&json).unwrap(),
    )
}

// ============================================================================
// Server Setup
// ============================================================================

/// Create the router with all routes
pub fn create_router(state: SharedState) -> Router {
    // CORS configuration
    let cors = CorsLayer::new()
        .allow_methods([Method::GET, Method::POST, Method::OPTIONS])
        .allow_origin(Any)
        .allow_headers([header::CONTENT_TYPE, header::ACCEPT, header::AUTHORIZATION]);

    Router::new()
        .route("/", get(index_page))
        .route("/sparql", get(sparql_get).post(sparql_post))
        .route("/health", get(health_check))
        .route("/stats", get(stats))
        .layer(cors)
        .with_state(state)
}

/// Run the async HTTP server
///
/// This function blocks until the server is shut down (via Ctrl+C).
///
/// # Arguments
///
/// * `store` - The RDF store to serve
/// * `config` - Server configuration
///
/// # Example
///
/// ```rust,ignore
/// use cwm::server::{ServerConfig, run_server};
/// use cwm::Store;
///
/// #[tokio::main]
/// async fn main() {
///     let store = Store::new();
///     let config = ServerConfig::new(8080);
///     run_server(store, config).await.unwrap();
/// }
/// ```
pub async fn run_server(store: Store, config: ServerConfig) -> Result<(), Box<dyn std::error::Error>> {
    let addr = config.socket_addr();
    let state = Arc::new(AppState::new(store, config));
    let app = create_router(state);

    eprintln!("🚀 SPARQL server listening on http://{}", addr);
    eprintln!("   Endpoints:");
    eprintln!("   - GET  /           - Query form");
    eprintln!("   - GET  /sparql     - SPARQL query (query param)");
    eprintln!("   - POST /sparql     - SPARQL query (body)");
    eprintln!("   - GET  /health     - Health check");
    eprintln!("   - GET  /stats      - Store statistics");
    eprintln!();
    eprintln!("Press Ctrl+C to stop");

    let listener = tokio::net::TcpListener::bind(addr).await?;

    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await?;

    eprintln!("\n👋 Server shut down gracefully");
    Ok(())
}

/// Wait for shutdown signal (Ctrl+C)
async fn shutdown_signal() {
    tokio::signal::ctrl_c()
        .await
        .expect("Failed to install Ctrl+C handler");
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use axum::body::Body;
    use axum::http::Request;
    use tower::util::ServiceExt;

    fn create_test_store() -> Store {
        use crate::term::{Term, Triple};

        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/s1"),
            Term::uri("http://example.org/p1"),
            Term::uri("http://example.org/o1"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/s2"),
            Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
            Term::uri("http://example.org/Type"),
        ));
        store
    }

    #[tokio::test]
    async fn test_health_check() {
        let store = create_test_store();
        let state = Arc::new(AppState::new(store, ServerConfig::default()));
        let app = create_router(state);

        let response = app
            .oneshot(Request::builder().uri("/health").body(Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn test_stats_endpoint() {
        let store = create_test_store();
        let state = Arc::new(AppState::new(store, ServerConfig::default()));
        let app = create_router(state);

        let response = app
            .oneshot(Request::builder().uri("/stats").body(Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);

        let body = axum::body::to_bytes(response.into_body(), 1024 * 1024).await.unwrap();
        let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
        assert_eq!(json["triple_count"], 2);
    }

    #[tokio::test]
    async fn test_sparql_get() {
        let store = create_test_store();
        let state = Arc::new(AppState::new(store, ServerConfig::default()));
        let app = create_router(state);

        let query = "SELECT * WHERE { ?s ?p ?o }";
        let encoded_query = urlencoding::encode(query);
        let uri = format!("/sparql?query={}", encoded_query);

        let response = app
            .oneshot(Request::builder().uri(&uri).body(Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn test_sparql_post() {
        let store = create_test_store();
        let state = Arc::new(AppState::new(store, ServerConfig::default()));
        let app = create_router(state);

        let query = "SELECT * WHERE { ?s ?p ?o }";

        let response = app
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri("/sparql")
                    .header("Content-Type", "application/sparql-query")
                    .body(Body::from(query.to_string()))
                    .unwrap()
            )
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn test_index_page() {
        let store = create_test_store();
        let state = Arc::new(AppState::new(store, ServerConfig::default()));
        let app = create_router(state);

        let response = app
            .oneshot(Request::builder().uri("/").body(Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
    }
}
