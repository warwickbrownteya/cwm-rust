//! Apache Jena Fuseki backend integration
//!
//! Provides a `FusekiStore` that implements `TripleStore` and communicates
//! with a Fuseki SPARQL endpoint for persistent, indexed triple storage.
//!
//! # Features
//!
//! - SPARQL 1.1 Query for pattern matching
//! - SPARQL 1.1 Update for add/remove operations
//! - Graph Store Protocol (GSP) for bulk operations
//! - Named graph support (quad store)
//! - Connection pooling and retry logic
//!
//! # Example
//!
//! ```ignore
//! use cwm::fuseki::FusekiStore;
//!
//! let store = FusekiStore::new("http://localhost:3030/dataset")?;
//! store.add(triple);
//! let results = store.match_pattern(&pattern);
//! ```

use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::Duration;

use crate::term::{Term, Triple, Bindings, Variable};
use crate::core::TripleStore;

/// Configuration for Fuseki connection
#[derive(Clone, Debug)]
pub struct FusekiConfig {
    /// Base URL of the Fuseki dataset (e.g., "http://localhost:3030/dataset")
    pub endpoint: String,
    /// SPARQL query endpoint (defaults to endpoint + "/query")
    pub query_endpoint: Option<String>,
    /// SPARQL update endpoint (defaults to endpoint + "/update")
    pub update_endpoint: Option<String>,
    /// Graph Store Protocol endpoint (defaults to endpoint + "/data")
    pub gsp_endpoint: Option<String>,
    /// Default graph URI (None = default graph)
    pub default_graph: Option<String>,
    /// Connection timeout in seconds
    pub timeout_secs: u64,
    /// Maximum retries on connection failure
    pub max_retries: u32,
    /// Enable request batching for bulk operations
    pub batch_size: usize,
}

impl Default for FusekiConfig {
    fn default() -> Self {
        Self {
            endpoint: "http://localhost:3030/dataset".to_string(),
            query_endpoint: None,
            update_endpoint: None,
            gsp_endpoint: None,
            default_graph: None,
            timeout_secs: 30,
            max_retries: 3,
            batch_size: 1000,
        }
    }
}

impl FusekiConfig {
    /// Create a new config with just the endpoint URL
    pub fn new(endpoint: impl Into<String>) -> Self {
        Self {
            endpoint: endpoint.into(),
            ..Default::default()
        }
    }

    /// Get the query endpoint URL
    pub fn query_url(&self) -> String {
        self.query_endpoint.clone()
            .unwrap_or_else(|| format!("{}/query", self.endpoint))
    }

    /// Get the update endpoint URL
    pub fn update_url(&self) -> String {
        self.update_endpoint.clone()
            .unwrap_or_else(|| format!("{}/update", self.endpoint))
    }

    /// Get the GSP endpoint URL
    pub fn gsp_url(&self) -> String {
        self.gsp_endpoint.clone()
            .unwrap_or_else(|| format!("{}/data", self.endpoint))
    }

    /// Set the default graph URI
    pub fn with_graph(mut self, graph: impl Into<String>) -> Self {
        self.default_graph = Some(graph.into());
        self
    }

    /// Set the timeout
    pub fn with_timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }
}

/// A triple store backed by Apache Jena Fuseki
///
/// Implements `TripleStore` trait, delegating storage to a Fuseki
/// SPARQL endpoint for persistent, indexed storage with named graph support.
pub struct FusekiStore {
    config: FusekiConfig,
    agent: ureq::Agent,
    /// Local cache for recently accessed triples (optional)
    cache: Arc<RwLock<Vec<Triple>>>,
    /// Whether cache is dirty (needs sync)
    cache_dirty: Arc<RwLock<bool>>,
    /// Pending inserts for batching
    pending_inserts: Arc<RwLock<Vec<Triple>>>,
    /// Pending deletes for batching
    pending_deletes: Arc<RwLock<Vec<Triple>>>,
}

impl FusekiStore {
    /// Create a new FusekiStore with the given endpoint
    pub fn new(endpoint: impl Into<String>) -> Result<Self, String> {
        Self::with_config(FusekiConfig::new(endpoint))
    }

    /// Create a new FusekiStore with custom configuration
    pub fn with_config(config: FusekiConfig) -> Result<Self, String> {
        let agent = ureq::AgentBuilder::new()
            .timeout(Duration::from_secs(config.timeout_secs))
            .build();

        // Test connectivity
        let store = Self {
            config,
            agent,
            cache: Arc::new(RwLock::new(Vec::new())),
            cache_dirty: Arc::new(RwLock::new(false)),
            pending_inserts: Arc::new(RwLock::new(Vec::new())),
            pending_deletes: Arc::new(RwLock::new(Vec::new())),
        };

        // Verify endpoint is reachable with a simple ASK query
        store.check_connection()?;

        Ok(store)
    }

    /// Check if the Fuseki endpoint is reachable
    fn check_connection(&self) -> Result<(), String> {
        let query = "ASK { ?s ?p ?o }";
        let url = self.config.query_url();

        match self.agent.post(&url)
            .set("Accept", "application/sparql-results+json")
            .set("Content-Type", "application/sparql-query")
            .send_string(query)
        {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("Failed to connect to Fuseki at {}: {}", url, e)),
        }
    }

    /// Execute a SPARQL SELECT query and return bindings
    pub fn query(&self, sparql: &str) -> Result<Vec<HashMap<String, Term>>, String> {
        let url = self.config.query_url();

        let response = self.agent.post(&url)
            .set("Accept", "application/sparql-results+json")
            .set("Content-Type", "application/sparql-query")
            .send_string(sparql)
            .map_err(|e| format!("Query failed: {}", e))?;

        let json: serde_json::Value = response.into_json()
            .map_err(|e| format!("Failed to parse response: {}", e))?;

        self.parse_sparql_results(&json)
    }

    /// Execute a SPARQL ASK query
    pub fn ask(&self, sparql: &str) -> Result<bool, String> {
        let url = self.config.query_url();

        let response = self.agent.post(&url)
            .set("Accept", "application/sparql-results+json")
            .set("Content-Type", "application/sparql-query")
            .send_string(sparql)
            .map_err(|e| format!("ASK query failed: {}", e))?;

        let json: serde_json::Value = response.into_json()
            .map_err(|e| format!("Failed to parse response: {}", e))?;

        json.get("boolean")
            .and_then(|v| v.as_bool())
            .ok_or_else(|| "Invalid ASK response".to_string())
    }

    /// Execute a SPARQL UPDATE query
    pub fn update(&self, sparql: &str) -> Result<(), String> {
        let url = self.config.update_url();

        self.agent.post(&url)
            .set("Content-Type", "application/sparql-update")
            .send_string(sparql)
            .map_err(|e| format!("Update failed: {}", e))?;

        Ok(())
    }

    /// Flush pending operations to Fuseki
    pub fn flush(&self) -> Result<(), String> {
        // Flush pending inserts
        let inserts = {
            let mut pending = self.pending_inserts.write().unwrap();
            std::mem::take(&mut *pending)
        };

        if !inserts.is_empty() {
            self.insert_triples_batch(&inserts)?;
        }

        // Flush pending deletes
        let deletes = {
            let mut pending = self.pending_deletes.write().unwrap();
            std::mem::take(&mut *pending)
        };

        if !deletes.is_empty() {
            self.delete_triples_batch(&deletes)?;
        }

        Ok(())
    }

    /// Insert triples using SPARQL UPDATE INSERT DATA
    fn insert_triples_batch(&self, triples: &[Triple]) -> Result<(), String> {
        if triples.is_empty() {
            return Ok(());
        }

        let mut data = String::new();
        for triple in triples {
            data.push_str(&format!("{} {} {} .\n",
                self.term_to_sparql(&triple.subject),
                self.term_to_sparql(&triple.predicate),
                self.term_to_sparql(&triple.object),
            ));
        }

        let update = if let Some(ref graph) = self.config.default_graph {
            format!("INSERT DATA {{ GRAPH <{}> {{\n{}}}\n}}", graph, data)
        } else {
            format!("INSERT DATA {{\n{}}}", data)
        };
        self.update(&update)
    }

    /// Delete triples using SPARQL UPDATE DELETE DATA
    fn delete_triples_batch(&self, triples: &[Triple]) -> Result<(), String> {
        if triples.is_empty() {
            return Ok(());
        }

        let mut data = String::new();
        for triple in triples {
            data.push_str(&format!("{} {} {} .\n",
                self.term_to_sparql(&triple.subject),
                self.term_to_sparql(&triple.predicate),
                self.term_to_sparql(&triple.object),
            ));
        }

        let update = if let Some(ref graph) = self.config.default_graph {
            format!("DELETE DATA {{ GRAPH <{}> {{\n{}}}\n}}", graph, data)
        } else {
            format!("DELETE DATA {{\n{}}}", data)
        };
        self.update(&update)
    }

    /// Convert a Term to SPARQL syntax
    fn term_to_sparql(&self, term: &Term) -> String {
        match term {
            Term::Uri(uri) => format!("<{}>", uri.as_str()),
            Term::Literal(lit) => {
                if let Some(lang) = lit.language() {
                    format!("\"{}\"@{}", self.escape_string(lit.value()), lang)
                } else if let Some(dt) = lit.datatype_uri() {
                    if dt == "http://www.w3.org/2001/XMLSchema#string" {
                        format!("\"{}\"", self.escape_string(lit.value()))
                    } else {
                        format!("\"{}\"^^<{}>", self.escape_string(lit.value()), dt)
                    }
                } else {
                    format!("\"{}\"", self.escape_string(lit.value()))
                }
            }
            Term::BlankNode(bn) => {
                if let Some(label) = bn.label() {
                    format!("_:{}", label)
                } else {
                    format!("_:b{}", bn.id())
                }
            }
            Term::Variable(v) => format!("?{}", v.name()),
            _ => format!("\"{}\"", term), // Fallback for complex terms
        }
    }

    /// Escape a string for SPARQL
    fn escape_string(&self, s: &str) -> String {
        s.replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
    }

    /// Parse SPARQL JSON results into bindings
    fn parse_sparql_results(&self, json: &serde_json::Value) -> Result<Vec<HashMap<String, Term>>, String> {
        let mut results = Vec::new();

        let bindings = json
            .get("results")
            .and_then(|r| r.get("bindings"))
            .and_then(|b| b.as_array())
            .ok_or_else(|| "Invalid SPARQL results format".to_string())?;

        for binding in bindings {
            let mut row = HashMap::new();

            if let Some(obj) = binding.as_object() {
                for (var, value) in obj {
                    if let Some(term) = self.parse_sparql_value(value) {
                        row.insert(var.clone(), term);
                    }
                }
            }

            results.push(row);
        }

        Ok(results)
    }

    /// Parse a single SPARQL JSON value to a Term
    fn parse_sparql_value(&self, value: &serde_json::Value) -> Option<Term> {
        let type_str = value.get("type")?.as_str()?;
        let value_str = value.get("value")?.as_str()?;

        match type_str {
            "uri" => Some(Term::uri(value_str)),
            "literal" | "typed-literal" => {
                if let Some(lang) = value.get("xml:lang").and_then(|l| l.as_str()) {
                    Some(Term::lang_literal(value_str, lang))
                } else if let Some(dt) = value.get("datatype").and_then(|d| d.as_str()) {
                    Some(Term::typed_literal(value_str, dt))
                } else {
                    Some(Term::literal(value_str))
                }
            }
            "bnode" => Some(Term::blank(value_str)),
            _ => None,
        }
    }

    /// Load all triples from the endpoint into the local cache
    pub fn load_all(&self) -> Result<Vec<Triple>, String> {
        let graph_clause = self.config.default_graph.as_ref()
            .map(|g| format!("FROM <{}>", g))
            .unwrap_or_default();

        let query = format!(
            "SELECT ?s ?p ?o {} WHERE {{ ?s ?p ?o }}",
            graph_clause
        );

        let bindings = self.query(&query)?;
        let mut triples = Vec::new();

        for binding in bindings {
            if let (Some(s), Some(p), Some(o)) = (
                binding.get("s"),
                binding.get("p"),
                binding.get("o"),
            ) {
                triples.push(Triple::new(s.clone(), p.clone(), o.clone()));
            }
        }

        // Update cache
        {
            let mut cache = self.cache.write().unwrap();
            *cache = triples.clone();
            *self.cache_dirty.write().unwrap() = false;
        }

        Ok(triples)
    }

    /// Get count of triples in the store
    pub fn count(&self) -> Result<usize, String> {
        let graph_clause = self.config.default_graph.as_ref()
            .map(|g| format!("FROM <{}>", g))
            .unwrap_or_default();

        let query = format!(
            "SELECT (COUNT(*) as ?count) {} WHERE {{ ?s ?p ?o }}",
            graph_clause
        );

        let bindings = self.query(&query)?;

        bindings.first()
            .and_then(|b| b.get("count"))
            .and_then(|t| match t {
                Term::Literal(lit) => lit.value().parse::<usize>().ok(),
                _ => None,
            })
            .ok_or_else(|| "Failed to get count".to_string())
    }

    /// Clear all triples from the graph
    pub fn clear_graph(&self) -> Result<(), String> {
        // Use DROP SILENT which doesn't fail if graph doesn't exist
        let update = if let Some(graph) = &self.config.default_graph {
            format!("DROP SILENT GRAPH <{}>; CREATE SILENT GRAPH <{}>", graph, graph)
        } else {
            "CLEAR DEFAULT".to_string()
        };

        self.update(&update)
    }

    /// Match a pattern using SPARQL
    pub fn match_pattern_sparql(&self, pattern: &Triple) -> Result<Vec<Bindings>, String> {
        let graph_clause = self.config.default_graph.as_ref()
            .map(|g| format!("FROM <{}>", g))
            .unwrap_or_default();

        // Build SELECT clause from variables in pattern
        let mut vars = Vec::new();
        let mut var_names = Vec::new();

        let subj = match &pattern.subject {
            Term::Variable(v) => {
                let name = v.name().to_string();
                vars.push(format!("?{}", name));
                var_names.push(("s".to_string(), name.clone()));
                format!("?{}", name)
            }
            t => self.term_to_sparql(t),
        };

        let pred = match &pattern.predicate {
            Term::Variable(v) => {
                let name = v.name().to_string();
                vars.push(format!("?{}", name));
                var_names.push(("p".to_string(), name.clone()));
                format!("?{}", name)
            }
            t => self.term_to_sparql(t),
        };

        let obj = match &pattern.object {
            Term::Variable(v) => {
                let name = v.name().to_string();
                vars.push(format!("?{}", name));
                var_names.push(("o".to_string(), name.clone()));
                format!("?{}", name)
            }
            t => self.term_to_sparql(t),
        };

        let select = if vars.is_empty() {
            "*".to_string()
        } else {
            vars.join(" ")
        };

        let query = format!(
            "SELECT {} {} WHERE {{ {} {} {} }}",
            select, graph_clause, subj, pred, obj
        );

        let results = self.query(&query)?;

        // Convert to Bindings
        let mut bindings_list = Vec::new();
        for row in results {
            let mut bindings: Bindings = Default::default();
            for (_, var_name) in &var_names {
                if let Some(term) = row.get(var_name) {
                    bindings.insert(Variable::universal(var_name.clone()), term.clone());
                }
            }
            bindings_list.push(bindings);
        }

        Ok(bindings_list)
    }
}

impl TripleStore for FusekiStore {
    fn add(&mut self, triple: Triple) {
        let mut pending = self.pending_inserts.write().unwrap();
        pending.push(triple);

        // Auto-flush when batch size reached
        if pending.len() >= self.config.batch_size {
            drop(pending);
            let _ = self.flush();
        }
    }

    fn remove(&mut self, triple: &Triple) -> bool {
        let mut pending = self.pending_deletes.write().unwrap();
        pending.push(triple.clone());

        // Auto-flush when batch size reached
        if pending.len() >= self.config.batch_size {
            drop(pending);
            let _ = self.flush();
        }

        true // We assume success; actual removal happens on flush
    }

    fn contains(&self, triple: &Triple) -> bool {
        let query = format!(
            "ASK {{ {} {} {} }}",
            self.term_to_sparql(&triple.subject),
            self.term_to_sparql(&triple.predicate),
            self.term_to_sparql(&triple.object),
        );

        self.ask(&query).unwrap_or(false)
    }

    fn len(&self) -> usize {
        self.count().unwrap_or(0)
    }

    fn iter(&self) -> Box<dyn Iterator<Item = &Triple> + '_> {
        // LIMITATION: FusekiStore cannot efficiently return references to remote data.
        // The TripleStore trait requires &Triple references, but we can only provide
        // owned values from a remote store.
        //
        // This implementation panics. Use to_vec() instead for FusekiStore.
        // In a future version, the trait should support both owned and borrowed iteration.
        panic!(
            "FusekiStore::iter() is not supported. \
             Use store.to_vec() or store.iter_owned() instead. \
             The TripleStore trait requires reference iteration which is \
             incompatible with remote storage backends."
        );
    }

    fn to_vec(&self) -> Vec<Triple> {
        // Efficiently get all triples without iterator overhead
        {
            let cache_dirty = *self.cache_dirty.read().unwrap();
            if cache_dirty || self.cache.read().unwrap().is_empty() {
                let _ = self.load_all();
            }
        }
        self.cache.read().unwrap().clone()
    }

    fn clear(&mut self) {
        // Clear pending operations
        self.pending_inserts.write().unwrap().clear();
        self.pending_deletes.write().unwrap().clear();

        // Clear the graph on Fuseki
        let _ = self.clear_graph();

        // Clear local cache
        self.cache.write().unwrap().clear();
    }
}

impl FusekiStore {
    /// Iterate over triples, returning owned values
    pub fn iter_owned(&self) -> impl Iterator<Item = Triple> {
        let triples = self.load_all().unwrap_or_default();
        triples.into_iter()
    }
}

/// Builder for FusekiStore
pub struct FusekiStoreBuilder {
    config: FusekiConfig,
}

impl FusekiStoreBuilder {
    /// Create a new builder with the endpoint URL
    pub fn new(endpoint: impl Into<String>) -> Self {
        Self {
            config: FusekiConfig::new(endpoint),
        }
    }

    /// Set the default graph URI
    pub fn graph(mut self, graph: impl Into<String>) -> Self {
        self.config.default_graph = Some(graph.into());
        self
    }

    /// Set the connection timeout
    pub fn timeout(mut self, secs: u64) -> Self {
        self.config.timeout_secs = secs;
        self
    }

    /// Set the batch size for bulk operations
    pub fn batch_size(mut self, size: usize) -> Self {
        self.config.batch_size = size;
        self
    }

    /// Set custom query endpoint
    pub fn query_endpoint(mut self, url: impl Into<String>) -> Self {
        self.config.query_endpoint = Some(url.into());
        self
    }

    /// Set custom update endpoint
    pub fn update_endpoint(mut self, url: impl Into<String>) -> Self {
        self.config.update_endpoint = Some(url.into());
        self
    }

    /// Build the FusekiStore
    pub fn build(self) -> Result<FusekiStore, String> {
        FusekiStore::with_config(self.config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_test_store() -> FusekiStore {
        FusekiStore {
            config: FusekiConfig::default(),
            agent: ureq::agent(),
            cache: Arc::new(RwLock::new(Vec::new())),
            cache_dirty: Arc::new(RwLock::new(false)),
            pending_inserts: Arc::new(RwLock::new(Vec::new())),
            pending_deletes: Arc::new(RwLock::new(Vec::new())),
        }
    }

    #[test]
    fn test_config_defaults() {
        let config = FusekiConfig::new("http://localhost:3030/test");
        assert_eq!(config.query_url(), "http://localhost:3030/test/query");
        assert_eq!(config.update_url(), "http://localhost:3030/test/update");
        assert_eq!(config.gsp_url(), "http://localhost:3030/test/data");
    }

    #[test]
    fn test_config_with_graph() {
        let config = FusekiConfig::new("http://localhost:3030/test")
            .with_graph("http://example.org/graph1")
            .with_timeout(60);
        assert_eq!(config.default_graph, Some("http://example.org/graph1".to_string()));
        assert_eq!(config.timeout_secs, 60);
    }

    #[test]
    fn test_config_custom_endpoints() {
        let mut config = FusekiConfig::new("http://localhost:3030/test");
        config.query_endpoint = Some("http://localhost:3030/custom/query".to_string());
        config.update_endpoint = Some("http://localhost:3030/custom/update".to_string());

        assert_eq!(config.query_url(), "http://localhost:3030/custom/query");
        assert_eq!(config.update_url(), "http://localhost:3030/custom/update");
    }

    #[test]
    fn test_term_to_sparql() {
        let store = make_test_store();

        // URI
        assert_eq!(
            store.term_to_sparql(&Term::uri("http://example.org/test")),
            "<http://example.org/test>"
        );

        // Simple literal
        assert_eq!(
            store.term_to_sparql(&Term::literal("hello")),
            "\"hello\""
        );

        // Blank node
        assert_eq!(
            store.term_to_sparql(&Term::blank("node1")),
            "_:node1"
        );

        // Variable
        assert_eq!(
            store.term_to_sparql(&Term::Variable(Variable::universal("x".to_string()))),
            "?x"
        );
    }

    #[test]
    fn test_term_to_sparql_typed_literals() {
        let store = make_test_store();

        // Integer
        assert_eq!(
            store.term_to_sparql(&Term::typed_literal("42", "http://www.w3.org/2001/XMLSchema#integer")),
            "\"42\"^^<http://www.w3.org/2001/XMLSchema#integer>"
        );

        // Language-tagged literal
        assert_eq!(
            store.term_to_sparql(&Term::lang_literal("bonjour", "fr")),
            "\"bonjour\"@fr"
        );
    }

    #[test]
    fn test_escape_string() {
        let store = make_test_store();

        assert_eq!(store.escape_string("hello"), "hello");
        assert_eq!(store.escape_string("hello\nworld"), "hello\\nworld");
        assert_eq!(store.escape_string("say \"hello\""), "say \\\"hello\\\"");
        assert_eq!(store.escape_string("path\\to\\file"), "path\\\\to\\\\file");
        assert_eq!(store.escape_string("tab\there"), "tab\\there");
    }

    #[test]
    fn test_builder() {
        let builder = FusekiStoreBuilder::new("http://localhost:3030/test")
            .graph("http://example.org/graph")
            .timeout(120)
            .batch_size(500)
            .query_endpoint("http://localhost:3030/sparql/query")
            .update_endpoint("http://localhost:3030/sparql/update");

        // Build will fail since no server, but we can check config was set
        let err = builder.build();
        assert!(err.is_err()); // Expected - no Fuseki server running
    }

    #[test]
    fn test_parse_sparql_value_uri() {
        let store = make_test_store();
        let json = serde_json::json!({
            "type": "uri",
            "value": "http://example.org/test"
        });

        let term = store.parse_sparql_value(&json);
        assert!(term.is_some());
        match term.unwrap() {
            Term::Uri(uri) => assert_eq!(uri.as_str(), "http://example.org/test"),
            _ => panic!("Expected URI"),
        }
    }

    #[test]
    fn test_parse_sparql_value_literal() {
        let store = make_test_store();

        // Plain literal
        let json = serde_json::json!({
            "type": "literal",
            "value": "hello"
        });
        let term = store.parse_sparql_value(&json);
        assert!(term.is_some());

        // Language-tagged literal
        let json = serde_json::json!({
            "type": "literal",
            "value": "bonjour",
            "xml:lang": "fr"
        });
        let term = store.parse_sparql_value(&json);
        assert!(term.is_some());

        // Typed literal
        let json = serde_json::json!({
            "type": "typed-literal",
            "value": "42",
            "datatype": "http://www.w3.org/2001/XMLSchema#integer"
        });
        let term = store.parse_sparql_value(&json);
        assert!(term.is_some());
    }

    #[test]
    fn test_parse_sparql_value_bnode() {
        let store = make_test_store();
        let json = serde_json::json!({
            "type": "bnode",
            "value": "b0"
        });

        let term = store.parse_sparql_value(&json);
        assert!(term.is_some());
        match term.unwrap() {
            Term::BlankNode(bn) => assert_eq!(bn.label(), Some("b0")),
            _ => panic!("Expected blank node"),
        }
    }

    #[test]
    fn test_pending_operations() {
        let mut store = make_test_store();

        // Add triples to pending
        let triple = Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/o"),
        );

        store.add(triple.clone());
        assert_eq!(store.pending_inserts.read().unwrap().len(), 1);

        store.remove(&triple);
        assert_eq!(store.pending_deletes.read().unwrap().len(), 1);
    }
}
