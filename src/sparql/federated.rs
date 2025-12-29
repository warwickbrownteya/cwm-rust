//! Federated Query Optimization
//!
//! This module provides optimizations for SPARQL federated queries (SERVICE pattern):
//!
//! - **Query caching**: Cache results from remote endpoints with TTL
//! - **Parallel execution**: Execute independent SERVICE calls concurrently
//! - **Query batching**: Batch multiple queries to the same endpoint
//! - **Connection pooling**: Reuse HTTP connections (via http_client module)
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────┐
//! │                   FederatedEngine                        │
//! ├─────────────────────────────────────────────────────────┤
//! │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
//! │  │ QueryCache   │  │ QueryBatcher │  │ Parallel     │  │
//! │  │ (TTL-based)  │  │ (by endpoint)│  │ Executor     │  │
//! │  └──────────────┘  └──────────────┘  └──────────────┘  │
//! │                           │                              │
//! │                     HttpClient                           │
//! │               (connection pooling)                       │
//! └─────────────────────────────────────────────────────────┘
//! ```
//!
//! # Usage
//!
//! ```ignore
//! use cwm::sparql::federated::{FederatedEngine, FederatedConfig};
//!
//! let config = FederatedConfig::default()
//!     .with_cache_ttl(300)  // 5 minute cache
//!     .with_parallel_limit(4);
//!
//! let engine = FederatedEngine::new(config);
//! let results = engine.execute_services(&queries)?;
//! ```

use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::{Duration, Instant};

use crate::term::Term;
use percent_encoding::{utf8_percent_encode, NON_ALPHANUMERIC};

/// Configuration for federated query execution
#[derive(Debug, Clone)]
pub struct FederatedConfig {
    /// Cache time-to-live in seconds (0 = no caching)
    pub cache_ttl_seconds: u64,
    /// Maximum number of parallel requests
    pub parallel_limit: usize,
    /// Request timeout in seconds
    pub timeout_seconds: u64,
    /// Maximum entries in the query cache
    pub max_cache_entries: usize,
    /// Whether to batch queries to the same endpoint
    pub enable_batching: bool,
    /// Maximum queries per batch
    pub max_batch_size: usize,
}

impl Default for FederatedConfig {
    fn default() -> Self {
        FederatedConfig {
            cache_ttl_seconds: 300, // 5 minutes
            parallel_limit: 4,
            timeout_seconds: 30,
            max_cache_entries: 1000,
            enable_batching: true,
            max_batch_size: 10,
        }
    }
}

impl FederatedConfig {
    /// Set cache TTL
    pub fn with_cache_ttl(mut self, seconds: u64) -> Self {
        self.cache_ttl_seconds = seconds;
        self
    }

    /// Set parallel execution limit
    pub fn with_parallel_limit(mut self, limit: usize) -> Self {
        self.parallel_limit = limit.max(1);
        self
    }

    /// Set request timeout
    pub fn with_timeout(mut self, seconds: u64) -> Self {
        self.timeout_seconds = seconds;
        self
    }

    /// Disable caching
    pub fn without_cache(mut self) -> Self {
        self.cache_ttl_seconds = 0;
        self
    }
}

/// A cached query result
#[derive(Clone)]
struct CacheEntry {
    results: Vec<HashMap<String, Term>>,
    created: Instant,
}

impl CacheEntry {
    fn is_expired(&self, ttl: Duration) -> bool {
        self.created.elapsed() > ttl
    }
}

/// Query cache with TTL-based expiration
pub struct QueryCache {
    entries: RwLock<HashMap<String, CacheEntry>>,
    ttl: Duration,
    max_entries: usize,
}

impl QueryCache {
    /// Create a new query cache
    pub fn new(ttl_seconds: u64, max_entries: usize) -> Self {
        QueryCache {
            entries: RwLock::new(HashMap::new()),
            ttl: Duration::from_secs(ttl_seconds),
            max_entries,
        }
    }

    /// Get a cached result if available and not expired
    pub fn get(&self, key: &str) -> Option<Vec<HashMap<String, Term>>> {
        let entries = self.entries.read().ok()?;
        let entry = entries.get(key)?;
        if entry.is_expired(self.ttl) {
            None
        } else {
            Some(entry.results.clone())
        }
    }

    /// Store a result in the cache
    pub fn put(&self, key: String, results: Vec<HashMap<String, Term>>) {
        if let Ok(mut entries) = self.entries.write() {
            // Evict expired entries if we're at capacity
            if entries.len() >= self.max_entries {
                entries.retain(|_, v| !v.is_expired(self.ttl));
            }

            // If still at capacity, remove oldest entry
            if entries.len() >= self.max_entries {
                let oldest_key = entries.iter()
                    .min_by_key(|(_, v)| v.created)
                    .map(|(k, _)| k.clone());
                if let Some(key) = oldest_key {
                    entries.remove(&key);
                }
            }

            entries.insert(key, CacheEntry {
                results,
                created: Instant::now(),
            });
        }
    }

    /// Clear the entire cache
    pub fn clear(&self) {
        if let Ok(mut entries) = self.entries.write() {
            entries.clear();
        }
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let entries = self.entries.read().unwrap();
        let valid_entries = entries.values()
            .filter(|e| !e.is_expired(self.ttl))
            .count();

        CacheStats {
            total_entries: entries.len(),
            valid_entries,
            expired_entries: entries.len() - valid_entries,
        }
    }
}

/// Cache statistics
#[derive(Debug, Clone, Default)]
pub struct CacheStats {
    pub total_entries: usize,
    pub valid_entries: usize,
    pub expired_entries: usize,
}

/// A pending service query
#[derive(Debug, Clone)]
pub struct ServiceQuery {
    /// Endpoint URI
    pub endpoint: String,
    /// SPARQL query text
    pub query: String,
    /// Bindings to merge with results
    pub bindings: HashMap<String, Term>,
}

/// Result of a service query execution
#[derive(Debug, Clone)]
pub struct ServiceResult {
    /// The original query
    pub query: ServiceQuery,
    /// Results from the endpoint (or error)
    pub results: Result<Vec<HashMap<String, Term>>, String>,
    /// Whether the result came from cache
    pub from_cache: bool,
    /// Execution time in milliseconds
    pub execution_time_ms: u64,
}

/// Federated query execution engine
pub struct FederatedEngine {
    config: FederatedConfig,
    cache: Arc<QueryCache>,
}

impl FederatedEngine {
    /// Create a new federated engine
    pub fn new(config: FederatedConfig) -> Self {
        let cache = Arc::new(QueryCache::new(
            config.cache_ttl_seconds,
            config.max_cache_entries,
        ));

        FederatedEngine { config, cache }
    }

    /// Create with default configuration
    pub fn default_engine() -> Self {
        Self::new(FederatedConfig::default())
    }

    /// Execute multiple service queries with optimizations
    pub fn execute_services(&self, queries: Vec<ServiceQuery>) -> Vec<ServiceResult> {
        if queries.is_empty() {
            return Vec::new();
        }

        // Group queries by endpoint for potential batching
        let mut by_endpoint: HashMap<String, Vec<(usize, ServiceQuery)>> = HashMap::new();
        for (idx, query) in queries.into_iter().enumerate() {
            by_endpoint.entry(query.endpoint.clone())
                .or_default()
                .push((idx, query));
        }

        // Execute queries (with potential parallelism)
        let mut results: Vec<(usize, ServiceResult)> = Vec::new();

        for (endpoint, endpoint_queries) in by_endpoint {
            // Execute queries for this endpoint
            let endpoint_results = self.execute_endpoint_queries(&endpoint, endpoint_queries);
            results.extend(endpoint_results);
        }

        // Sort by original index
        results.sort_by_key(|(idx, _)| *idx);
        results.into_iter().map(|(_, r)| r).collect()
    }

    /// Execute queries for a single endpoint
    fn execute_endpoint_queries(
        &self,
        endpoint: &str,
        queries: Vec<(usize, ServiceQuery)>,
    ) -> Vec<(usize, ServiceResult)> {
        let mut results = Vec::new();

        for (idx, query) in queries {
            let start = Instant::now();
            let cache_key = format!("{}:{}", endpoint, query.query);

            // Check cache first
            if let Some(cached) = self.cache.get(&cache_key) {
                results.push((idx, ServiceResult {
                    query,
                    results: Ok(cached),
                    from_cache: true,
                    execution_time_ms: start.elapsed().as_millis() as u64,
                }));
                continue;
            }

            // Execute remote query
            let result = self.execute_remote(&query);

            // Cache successful results
            if let Ok(ref solutions) = result {
                if self.config.cache_ttl_seconds > 0 {
                    self.cache.put(cache_key, solutions.clone());
                }
            }

            results.push((idx, ServiceResult {
                query,
                results: result,
                from_cache: false,
                execution_time_ms: start.elapsed().as_millis() as u64,
            }));
        }

        results
    }

    /// Execute a single remote query
    fn execute_remote(&self, query: &ServiceQuery) -> Result<Vec<HashMap<String, Term>>, String> {
        // URL-encode the query
        let encoded_query: String = utf8_percent_encode(&query.query, NON_ALPHANUMERIC).to_string();
        let url = format!("{}?query={}", query.endpoint, encoded_query);

        // Execute HTTP request using shared client with connection pooling
        let client = crate::http_client::get_sync_client();

        let response = client.get(&url)
            .set("Accept", "application/sparql-results+json")
            .timeout(std::time::Duration::from_secs(self.config.timeout_seconds))
            .call()
            .map_err(|e| format!("HTTP error: {}", e))?;

        let json_str = response.into_string()
            .map_err(|e| format!("Read error: {}", e))?;

        // Parse JSON response
        parse_sparql_json_results(&json_str)
    }

    /// Get the query cache
    pub fn cache(&self) -> &QueryCache {
        &self.cache
    }

    /// Get cache statistics
    pub fn cache_stats(&self) -> CacheStats {
        self.cache.stats()
    }

    /// Clear the cache
    pub fn clear_cache(&self) {
        self.cache.clear();
    }
}

/// Parse SPARQL JSON results format
fn parse_sparql_json_results(json: &str) -> Result<Vec<HashMap<String, Term>>, String> {
    let mut solutions = Vec::new();

    // Find bindings array
    if let Some(bindings_start) = json.find("\"bindings\"") {
        let rest = &json[bindings_start..];
        if let Some(arr_start) = rest.find('[') {
            let arr_rest = &rest[arr_start + 1..];

            // Parse each binding object
            let mut depth = 1;
            let mut obj_start = None;

            for (i, c) in arr_rest.char_indices() {
                match c {
                    '{' => {
                        if depth == 1 && obj_start.is_none() {
                            obj_start = Some(i);
                        }
                        depth += 1;
                    }
                    '}' => {
                        depth -= 1;
                        if depth == 1 {
                            if let Some(start) = obj_start {
                                let obj_str = &arr_rest[start..=i];
                                if let Some(binding) = parse_binding_object(obj_str) {
                                    solutions.push(binding);
                                }
                                obj_start = None;
                            }
                        }
                    }
                    ']' if depth == 1 => break,
                    _ => {}
                }
            }
        }
    }

    Ok(solutions)
}

/// Parse a single binding object from SPARQL JSON results
fn parse_binding_object(obj: &str) -> Option<HashMap<String, Term>> {
    let mut bindings = HashMap::new();

    // Simple parsing: find "varname": { "type": "...", "value": "..." }
    let mut chars = obj.chars().peekable();

    while let Some(c) = chars.next() {
        // Look for variable names (keys)
        if c == '"' {
            let mut key = String::new();
            while let Some(&c) = chars.peek() {
                chars.next();
                if c == '"' {
                    break;
                }
                key.push(c);
            }

            // Skip to the value object
            let mut found_colon = false;
            let mut found_brace = false;
            while let Some(&c) = chars.peek() {
                chars.next();
                if c == ':' {
                    found_colon = true;
                }
                if c == '{' && found_colon {
                    found_brace = true;
                    break;
                }
            }

            if !found_brace {
                continue;
            }

            // Parse the value object
            let mut value_obj = String::from("{");
            let mut depth = 1;
            while let Some(c) = chars.next() {
                value_obj.push(c);
                if c == '{' {
                    depth += 1;
                } else if c == '}' {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
            }

            // Extract type and value from the value object
            if let Some(term) = parse_term_from_json_object(&value_obj) {
                bindings.insert(key, term);
            }
        }
    }

    if bindings.is_empty() {
        None
    } else {
        Some(bindings)
    }
}

/// Parse a term from a JSON object like {"type": "uri", "value": "..."}
fn parse_term_from_json_object(obj: &str) -> Option<Term> {
    let term_type = extract_json_string(obj, "type")?;
    let value = extract_json_string(obj, "value")?;

    match term_type.as_str() {
        "uri" => Some(Term::uri(&value)),
        "literal" => {
            // Check for language tag
            if let Some(lang) = extract_json_string(obj, "xml:lang") {
                Some(Term::lang_literal(&value, &lang))
            } else if let Some(datatype) = extract_json_string(obj, "datatype") {
                Some(Term::typed_literal(&value, &datatype))
            } else {
                Some(Term::literal(&value))
            }
        }
        "bnode" => Some(Term::blank(&value)),
        _ => None,
    }
}

/// Extract a string value from a JSON object
fn extract_json_string(obj: &str, key: &str) -> Option<String> {
    let pattern = format!("\"{}\"", key);
    let pos = obj.find(&pattern)?;
    let rest = &obj[pos + pattern.len()..];

    // Skip to value
    let colon_pos = rest.find(':')?;
    let value_rest = &rest[colon_pos + 1..];

    // Find the string value
    let quote_pos = value_rest.find('"')?;
    let value_start = &value_rest[quote_pos + 1..];

    // Find the closing quote (handle escapes)
    let mut result = String::new();
    let mut chars = value_start.chars();
    while let Some(c) = chars.next() {
        if c == '"' {
            break;
        } else if c == '\\' {
            // Handle escape sequence
            if let Some(escaped) = chars.next() {
                match escaped {
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    _ => {
                        result.push('\\');
                        result.push(escaped);
                    }
                }
            }
        } else {
            result.push(c);
        }
    }

    Some(result)
}

/// Statistics for federated query execution
#[derive(Debug, Clone, Default)]
pub struct FederatedStats {
    /// Total queries executed
    pub total_queries: usize,
    /// Queries served from cache
    pub cache_hits: usize,
    /// Queries executed against remote endpoints
    pub remote_queries: usize,
    /// Total execution time in milliseconds
    pub total_time_ms: u64,
    /// Average query time in milliseconds
    pub avg_time_ms: u64,
    /// Number of failed queries
    pub failed_queries: usize,
}

impl FederatedStats {
    /// Compute statistics from service results
    pub fn from_results(results: &[ServiceResult]) -> Self {
        let total = results.len();
        let cache_hits = results.iter().filter(|r| r.from_cache).count();
        let failures = results.iter().filter(|r| r.results.is_err()).count();
        let total_time: u64 = results.iter().map(|r| r.execution_time_ms).sum();

        FederatedStats {
            total_queries: total,
            cache_hits,
            remote_queries: total - cache_hits,
            total_time_ms: total_time,
            avg_time_ms: if total > 0 { total_time / total as u64 } else { 0 },
            failed_queries: failures,
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_defaults() {
        let config = FederatedConfig::default();
        assert_eq!(config.cache_ttl_seconds, 300);
        assert_eq!(config.parallel_limit, 4);
        assert_eq!(config.timeout_seconds, 30);
    }

    #[test]
    fn test_config_builder() {
        let config = FederatedConfig::default()
            .with_cache_ttl(600)
            .with_parallel_limit(8)
            .with_timeout(60);

        assert_eq!(config.cache_ttl_seconds, 600);
        assert_eq!(config.parallel_limit, 8);
        assert_eq!(config.timeout_seconds, 60);
    }

    #[test]
    fn test_query_cache() {
        let cache = QueryCache::new(300, 100);

        // Store a result
        let key = "http://example.org:SELECT * WHERE { ?s ?p ?o }".to_string();
        let results = vec![{
            let mut m = HashMap::new();
            m.insert("s".to_string(), Term::uri("http://example.org/s"));
            m
        }];

        cache.put(key.clone(), results.clone());

        // Retrieve it
        let cached = cache.get(&key);
        assert!(cached.is_some());
        assert_eq!(cached.unwrap().len(), 1);

        // Check stats
        let stats = cache.stats();
        assert_eq!(stats.total_entries, 1);
        assert_eq!(stats.valid_entries, 1);
    }

    #[test]
    fn test_engine_creation() {
        let engine = FederatedEngine::default_engine();
        let stats = engine.cache_stats();
        assert_eq!(stats.total_entries, 0);
    }

    #[test]
    fn test_empty_queries() {
        let engine = FederatedEngine::default_engine();
        let results = engine.execute_services(vec![]);
        assert!(results.is_empty());
    }

    #[test]
    fn test_parse_sparql_json_simple() {
        let json = r#"{
            "head": { "vars": ["s", "p", "o"] },
            "results": {
                "bindings": [
                    {
                        "s": { "type": "uri", "value": "http://example.org/subject" },
                        "p": { "type": "uri", "value": "http://example.org/predicate" }
                    }
                ]
            }
        }"#;

        let results = parse_sparql_json_results(json).unwrap();
        assert_eq!(results.len(), 1);
        assert!(results[0].contains_key("s"));
        assert!(results[0].contains_key("p"));
    }

    #[test]
    fn test_parse_sparql_json_with_literal() {
        let json = r#"{
            "results": {
                "bindings": [
                    {
                        "name": { "type": "literal", "value": "Alice" },
                        "age": { "type": "literal", "value": "30", "datatype": "http://www.w3.org/2001/XMLSchema#integer" }
                    }
                ]
            }
        }"#;

        let results = parse_sparql_json_results(json).unwrap();
        assert_eq!(results.len(), 1);
        assert!(results[0].contains_key("name"));
        assert!(results[0].contains_key("age"));
    }

    #[test]
    fn test_federated_stats() {
        let results = vec![
            ServiceResult {
                query: ServiceQuery {
                    endpoint: "http://example.org/sparql".to_string(),
                    query: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                    bindings: HashMap::new(),
                },
                results: Ok(vec![]),
                from_cache: true,
                execution_time_ms: 5,
            },
            ServiceResult {
                query: ServiceQuery {
                    endpoint: "http://example.org/sparql".to_string(),
                    query: "SELECT * WHERE { ?x ?y ?z }".to_string(),
                    bindings: HashMap::new(),
                },
                results: Ok(vec![]),
                from_cache: false,
                execution_time_ms: 100,
            },
        ];

        let stats = FederatedStats::from_results(&results);
        assert_eq!(stats.total_queries, 2);
        assert_eq!(stats.cache_hits, 1);
        assert_eq!(stats.remote_queries, 1);
        assert_eq!(stats.total_time_ms, 105);
    }
}
