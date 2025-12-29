//! Query Result Cache
//!
//! Provides LRU caching for SPARQL query results with TTL support.
//!
//! # Features
//!
//! - LRU (Least Recently Used) eviction policy
//! - Configurable TTL (Time To Live) for cache entries
//! - Thread-safe with async support
//! - Cache hit/miss statistics
//! - Manual and automatic cache invalidation
//!
//! # Usage
//!
//! ```ignore
//! use cwm::sparql::QueryCache;
//!
//! // Create a cache with max 100 entries, 5 minute TTL
//! let cache = QueryCache::new(100, Duration::from_secs(300));
//!
//! // Check cache before executing query
//! if let Some(result) = cache.get("SELECT * WHERE { ?s ?p ?o }").await {
//!     return result;
//! }
//!
//! // Execute query and cache result
//! let result = execute_sparql(&store, query)?;
//! cache.put(query, result.clone()).await;
//! ```

use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use tokio::sync::RwLock;

use super::QueryResult;

/// A cached query result with metadata
#[derive(Clone)]
struct CacheEntry {
    /// The cached query result
    result: QueryResult,
    /// When this entry was created
    created_at: Instant,
    /// Last access time for LRU tracking
    last_accessed: Instant,
    /// Access count for statistics
    access_count: u64,
}

impl CacheEntry {
    fn new(result: QueryResult) -> Self {
        let now = Instant::now();
        CacheEntry {
            result,
            created_at: now,
            last_accessed: now,
            access_count: 1,
        }
    }

    fn is_expired(&self, ttl: Duration) -> bool {
        self.created_at.elapsed() > ttl
    }

    fn touch(&mut self) {
        self.last_accessed = Instant::now();
        self.access_count += 1;
    }
}

/// Cache statistics
#[derive(Debug, Clone, Default)]
pub struct CacheStats {
    /// Number of cache hits
    pub hits: u64,
    /// Number of cache misses
    pub misses: u64,
    /// Number of entries currently in cache
    pub entries: usize,
    /// Number of entries evicted
    pub evictions: u64,
    /// Number of entries expired
    pub expirations: u64,
    /// Total bytes of cached results (approximate)
    pub bytes_cached: usize,
}

impl CacheStats {
    /// Calculate hit rate as a percentage
    pub fn hit_rate(&self) -> f64 {
        let total = self.hits + self.misses;
        if total == 0 {
            0.0
        } else {
            (self.hits as f64 / total as f64) * 100.0
        }
    }
}

/// Configuration for the query cache
#[derive(Debug, Clone)]
pub struct CacheConfig {
    /// Maximum number of entries in the cache
    pub max_entries: usize,
    /// Time-to-live for cache entries
    pub ttl: Duration,
    /// Whether caching is enabled
    pub enabled: bool,
    /// Maximum size of a single cached result (in bytes, approximate)
    pub max_result_size: usize,
}

impl Default for CacheConfig {
    fn default() -> Self {
        CacheConfig {
            max_entries: 1000,
            ttl: Duration::from_secs(300), // 5 minutes
            enabled: true,
            max_result_size: 10 * 1024 * 1024, // 10 MB
        }
    }
}

/// LRU Query Result Cache
///
/// Thread-safe cache for SPARQL query results with LRU eviction
/// and TTL expiration.
pub struct QueryCache {
    /// Cache entries keyed by query hash
    entries: RwLock<HashMap<u64, CacheEntry>>,
    /// LRU ordering (query hash -> last access time as nanos)
    lru_order: RwLock<HashMap<u64, u64>>,
    /// Cache configuration
    config: CacheConfig,
    /// Statistics
    hits: AtomicU64,
    misses: AtomicU64,
    evictions: AtomicU64,
    expirations: AtomicU64,
    /// Store generation counter (for invalidation)
    store_generation: AtomicU64,
    /// Cache generation (incremented on invalidation)
    cache_generation: AtomicU64,
}

impl QueryCache {
    /// Create a new query cache with the given configuration
    pub fn new(config: CacheConfig) -> Self {
        QueryCache {
            entries: RwLock::new(HashMap::new()),
            lru_order: RwLock::new(HashMap::new()),
            config,
            hits: AtomicU64::new(0),
            misses: AtomicU64::new(0),
            evictions: AtomicU64::new(0),
            expirations: AtomicU64::new(0),
            store_generation: AtomicU64::new(0),
            cache_generation: AtomicU64::new(0),
        }
    }

    /// Create a cache with default configuration
    pub fn with_defaults() -> Self {
        Self::new(CacheConfig::default())
    }

    /// Create a cache with specified max entries and TTL
    pub fn with_size_and_ttl(max_entries: usize, ttl: Duration) -> Self {
        Self::new(CacheConfig {
            max_entries,
            ttl,
            ..Default::default()
        })
    }

    /// Compute hash key for a query string
    fn query_hash(query: &str) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        let mut hasher = DefaultHasher::new();
        query.hash(&mut hasher);
        hasher.finish()
    }

    /// Estimate the size of a query result in bytes
    fn estimate_result_size(result: &QueryResult) -> usize {
        match result {
            QueryResult::Boolean(_) => 8,
            QueryResult::Bindings { variables, solutions } => {
                let vars_size: usize = variables.iter().map(|v| v.len()).sum();
                let sols_size: usize = solutions.iter().map(|sol| {
                    sol.iter().map(|(k, v)| k.len() + format!("{:?}", v).len()).sum::<usize>()
                }).sum();
                vars_size + sols_size
            }
            QueryResult::Graph(triples) => {
                triples.iter().map(|t| format!("{:?}", t).len()).sum()
            }
        }
    }

    /// Get a cached result for a query
    ///
    /// Returns `Some(result)` if found and not expired, `None` otherwise.
    pub async fn get(&self, query: &str) -> Option<QueryResult> {
        if !self.config.enabled {
            self.misses.fetch_add(1, Ordering::Relaxed);
            return None;
        }

        let hash = Self::query_hash(query);

        // Check if entry exists and is not expired
        let mut entries = self.entries.write().await;

        if let Some(entry) = entries.get_mut(&hash) {
            if entry.is_expired(self.config.ttl) {
                // Entry expired - remove it
                entries.remove(&hash);
                self.lru_order.write().await.remove(&hash);
                self.expirations.fetch_add(1, Ordering::Relaxed);
                self.misses.fetch_add(1, Ordering::Relaxed);
                return None;
            }

            // Update LRU and access count
            entry.touch();
            let now_nanos = Instant::now().elapsed().as_nanos() as u64;
            self.lru_order.write().await.insert(hash, now_nanos);

            self.hits.fetch_add(1, Ordering::Relaxed);
            return Some(entry.result.clone());
        }

        self.misses.fetch_add(1, Ordering::Relaxed);
        None
    }

    /// Store a query result in the cache
    ///
    /// Evicts LRU entries if cache is full.
    pub async fn put(&self, query: &str, result: QueryResult) {
        if !self.config.enabled {
            return;
        }

        // Check result size limit
        let result_size = Self::estimate_result_size(&result);
        if result_size > self.config.max_result_size {
            return; // Result too large to cache
        }

        let hash = Self::query_hash(query);
        let entry = CacheEntry::new(result);

        let mut entries = self.entries.write().await;

        // Evict if at capacity
        while entries.len() >= self.config.max_entries {
            self.evict_lru(&mut entries).await;
        }

        // Insert new entry
        entries.insert(hash, entry);
        let now_nanos = Instant::now().elapsed().as_nanos() as u64;
        self.lru_order.write().await.insert(hash, now_nanos);
    }

    /// Evict the least recently used entry
    async fn evict_lru(&self, entries: &mut HashMap<u64, CacheEntry>) {
        let mut lru = self.lru_order.write().await;

        // Find LRU entry
        if let Some((&hash, _)) = lru.iter().min_by_key(|(_, &time)| time) {
            entries.remove(&hash);
            lru.remove(&hash);
            self.evictions.fetch_add(1, Ordering::Relaxed);
        }
    }

    /// Invalidate all cached results
    ///
    /// Call this when the underlying store is modified.
    pub async fn invalidate(&self) {
        let mut entries = self.entries.write().await;
        let mut lru = self.lru_order.write().await;

        entries.clear();
        lru.clear();

        self.cache_generation.fetch_add(1, Ordering::Relaxed);
    }

    /// Invalidate entries matching a predicate
    ///
    /// Useful for selective invalidation based on query patterns.
    pub async fn invalidate_matching<F>(&self, _predicate: F)
    where
        F: Fn(&str) -> bool,
    {
        // For now, just invalidate all since we don't store query strings
        // A more sophisticated implementation would store query strings too
        self.invalidate().await;
    }

    /// Remove expired entries
    ///
    /// Call periodically to clean up expired entries.
    pub async fn cleanup_expired(&self) {
        let mut entries = self.entries.write().await;
        let mut lru = self.lru_order.write().await;

        let expired: Vec<u64> = entries
            .iter()
            .filter(|(_, entry)| entry.is_expired(self.config.ttl))
            .map(|(&hash, _)| hash)
            .collect();

        for hash in expired {
            entries.remove(&hash);
            lru.remove(&hash);
            self.expirations.fetch_add(1, Ordering::Relaxed);
        }
    }

    /// Get cache statistics
    pub async fn stats(&self) -> CacheStats {
        let entries = self.entries.read().await;

        let bytes_cached: usize = entries
            .values()
            .map(|e| Self::estimate_result_size(&e.result))
            .sum();

        CacheStats {
            hits: self.hits.load(Ordering::Relaxed),
            misses: self.misses.load(Ordering::Relaxed),
            entries: entries.len(),
            evictions: self.evictions.load(Ordering::Relaxed),
            expirations: self.expirations.load(Ordering::Relaxed),
            bytes_cached,
        }
    }

    /// Check if the cache is enabled
    pub fn is_enabled(&self) -> bool {
        self.config.enabled
    }

    /// Get the current number of cached entries
    pub async fn len(&self) -> usize {
        self.entries.read().await.len()
    }

    /// Check if the cache is empty
    pub async fn is_empty(&self) -> bool {
        self.entries.read().await.is_empty()
    }

    /// Notify that the store has been modified
    ///
    /// This increments the store generation counter, which can be used
    /// to invalidate cache entries selectively.
    pub fn notify_store_modified(&self) {
        self.store_generation.fetch_add(1, Ordering::Relaxed);
    }

    /// Get the current store generation
    pub fn store_generation(&self) -> u64 {
        self.store_generation.load(Ordering::Relaxed)
    }
}

impl Default for QueryCache {
    fn default() -> Self {
        Self::with_defaults()
    }
}

impl std::fmt::Debug for QueryCache {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("QueryCache")
            .field("enabled", &self.config.enabled)
            .field("max_entries", &self.config.max_entries)
            .field("ttl_secs", &self.config.ttl.as_secs())
            .field("hits", &self.hits.load(Ordering::Relaxed))
            .field("misses", &self.misses.load(Ordering::Relaxed))
            .finish()
    }
}

/// Thread-safe shared query cache
pub type SharedQueryCache = Arc<QueryCache>;

/// Create a new shared query cache
pub fn create_shared_cache(config: CacheConfig) -> SharedQueryCache {
    Arc::new(QueryCache::new(config))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap as StdHashMap;

    fn make_test_result(n: usize) -> QueryResult {
        let mut solutions = Vec::new();
        for i in 0..n {
            let mut row = StdHashMap::new();
            row.insert("x".to_string(), crate::Term::literal(&format!("value{}", i)));
            solutions.push(row);
        }
        QueryResult::Bindings {
            variables: vec!["x".to_string()],
            solutions,
        }
    }

    #[tokio::test]
    async fn test_cache_hit_miss() {
        let cache = QueryCache::with_size_and_ttl(10, Duration::from_secs(60));

        // Miss on first access
        assert!(cache.get("SELECT * WHERE { ?s ?p ?o }").await.is_none());

        // Put result
        let result = make_test_result(5);
        cache.put("SELECT * WHERE { ?s ?p ?o }", result.clone()).await;

        // Hit on second access
        let cached = cache.get("SELECT * WHERE { ?s ?p ?o }").await;
        assert!(cached.is_some());

        // Check stats
        let stats = cache.stats().await;
        assert_eq!(stats.hits, 1);
        assert_eq!(stats.misses, 1);
    }

    #[tokio::test]
    async fn test_cache_expiration() {
        let cache = QueryCache::with_size_and_ttl(10, Duration::from_millis(50));

        let result = make_test_result(1);
        cache.put("query1", result).await;

        // Should hit immediately
        assert!(cache.get("query1").await.is_some());

        // Wait for expiration
        tokio::time::sleep(Duration::from_millis(100)).await;

        // Should miss after expiration
        assert!(cache.get("query1").await.is_none());

        let stats = cache.stats().await;
        assert_eq!(stats.expirations, 1);
    }

    #[tokio::test]
    async fn test_cache_eviction() {
        let cache = QueryCache::with_size_and_ttl(3, Duration::from_secs(60));

        // Fill cache
        for i in 0..3 {
            let result = make_test_result(1);
            cache.put(&format!("query{}", i), result).await;
        }

        assert_eq!(cache.len().await, 3);

        // Add one more, should evict oldest
        let result = make_test_result(1);
        cache.put("query_new", result).await;

        assert_eq!(cache.len().await, 3);

        let stats = cache.stats().await;
        assert_eq!(stats.evictions, 1);
    }

    #[tokio::test]
    async fn test_cache_invalidation() {
        let cache = QueryCache::with_size_and_ttl(10, Duration::from_secs(60));

        // Add some entries
        for i in 0..5 {
            let result = make_test_result(1);
            cache.put(&format!("query{}", i), result).await;
        }

        assert_eq!(cache.len().await, 5);

        // Invalidate all
        cache.invalidate().await;

        assert!(cache.is_empty().await);
    }

    #[tokio::test]
    async fn test_cache_disabled() {
        let config = CacheConfig {
            enabled: false,
            ..Default::default()
        };
        let cache = QueryCache::new(config);

        let result = make_test_result(1);
        cache.put("query1", result).await;

        // Should always miss when disabled
        assert!(cache.get("query1").await.is_none());
        assert!(cache.is_empty().await);
    }

    #[tokio::test]
    async fn test_hit_rate() {
        let cache = QueryCache::with_size_and_ttl(10, Duration::from_secs(60));

        let result = make_test_result(1);
        cache.put("query1", result).await;

        // 3 hits, 2 misses
        cache.get("query1").await;
        cache.get("query1").await;
        cache.get("query1").await;
        cache.get("query2").await;
        cache.get("query3").await;

        let stats = cache.stats().await;
        assert_eq!(stats.hits, 3);
        assert_eq!(stats.misses, 2);
        assert!((stats.hit_rate() - 60.0).abs() < 0.1);
    }

    #[tokio::test]
    async fn test_result_size_limit() {
        let config = CacheConfig {
            max_result_size: 100, // Very small limit
            ..Default::default()
        };
        let cache = QueryCache::new(config);

        // Large result should not be cached
        let large_result = make_test_result(1000);
        cache.put("large_query", large_result).await;

        assert!(cache.is_empty().await);

        // Small result should be cached
        let small_result = make_test_result(1);
        cache.put("small_query", small_result).await;

        assert!(!cache.is_empty().await);
    }
}
