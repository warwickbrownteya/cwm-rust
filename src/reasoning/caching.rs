//! Semantic Caching Module
//!
//! This module provides intelligent caching for N3 reasoning:
//! - Query result caching with semantic equivalence
//! - Incremental cache invalidation
//! - Cache-aware query rewriting
//! - Materialized view management
//! - Dependency tracking for invalidation

use crate::term::{Term, Triple};
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};

/// Unique cache entry ID
pub type CacheId = u64;

static CACHE_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

fn next_cache_id() -> CacheId {
    CACHE_ID_COUNTER.fetch_add(1, Ordering::SeqCst)
}

/// A semantic hash that captures query meaning rather than syntax
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SemanticHash(u64);

impl SemanticHash {
    pub fn from_query(query: &QueryPattern) -> Self {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        query.semantic_hash(&mut hasher);
        Self(hasher.finish())
    }

    pub fn value(&self) -> u64 {
        self.0
    }
}

/// A query pattern for caching
#[derive(Debug, Clone)]
pub struct QueryPattern {
    /// Triple patterns in the query
    pub patterns: Vec<Triple>,
    /// Filter conditions
    pub filters: Vec<Filter>,
    /// Variables to project
    pub projection: Vec<String>,
    /// LIMIT
    pub limit: Option<usize>,
    /// OFFSET
    pub offset: Option<usize>,
}

impl QueryPattern {
    pub fn new(patterns: Vec<Triple>) -> Self {
        Self {
            patterns,
            filters: Vec::new(),
            projection: Vec::new(),
            limit: None,
            offset: None,
        }
    }

    /// Compute semantic hash for this query
    pub fn semantic_hash<H: Hasher>(&self, hasher: &mut H) {
        let mut normalized_patterns: Vec<String> = self
            .patterns
            .iter()
            .map(|p| self.normalize_pattern(p))
            .collect();
        normalized_patterns.sort();

        for p in &normalized_patterns {
            p.hash(hasher);
        }

        for f in &self.filters {
            f.hash(hasher);
        }

        let mut sorted_projection = self.projection.clone();
        sorted_projection.sort();
        for v in &sorted_projection {
            v.hash(hasher);
        }
    }

    fn normalize_pattern(&self, pattern: &Triple) -> String {
        let s = self.normalize_term(&pattern.subject);
        let p = self.normalize_term(&pattern.predicate);
        let o = self.normalize_term(&pattern.object);
        format!("({},{},{})", s, p, o)
    }

    fn normalize_term(&self, term: &Term) -> String {
        match term {
            Term::Variable(v) => format!("?{}", v.name()),
            Term::Uri(u) => u.as_str().to_string(),
            Term::BlankNode(b) => format!("_:{}", b.id()),
            Term::Literal(l) => format!("\"{}\"", l.value()),
            _ => String::new(),
        }
    }

    /// Extract the predicates used in this query
    pub fn predicates(&self) -> HashSet<String> {
        self.patterns
            .iter()
            .filter_map(|p| match &p.predicate {
                Term::Uri(u) => Some(u.as_str().to_string()),
                _ => None,
            })
            .collect()
    }
}

/// A filter condition
#[derive(Debug, Clone, Hash)]
pub struct Filter {
    pub expression: String,
}

/// Query result for caching
#[derive(Debug, Clone)]
pub struct QueryResult {
    /// Variable bindings (each row is a solution)
    pub bindings: Vec<HashMap<String, Term>>,
    /// Whether this is a complete result
    pub complete: bool,
}

impl QueryResult {
    pub fn new(bindings: Vec<HashMap<String, Term>>) -> Self {
        Self {
            bindings,
            complete: true,
        }
    }

    pub fn empty() -> Self {
        Self {
            bindings: Vec::new(),
            complete: true,
        }
    }
}

/// Cache entry with metadata
#[derive(Debug, Clone)]
pub struct CacheEntry {
    /// Unique ID
    pub id: CacheId,
    /// The query pattern
    pub query: QueryPattern,
    /// Cached result
    pub result: QueryResult,
    /// Semantic hash
    pub hash: SemanticHash,
    /// Creation time
    pub created_at: Instant,
    /// Last access time
    pub last_accessed: Instant,
    /// Access count
    pub access_count: u64,
    /// Predicates this entry depends on
    pub depends_on: HashSet<String>,
    /// Time to live
    pub ttl: Option<Duration>,
    /// Whether this entry is valid
    pub valid: bool,
}

impl CacheEntry {
    pub fn new(query: QueryPattern, result: QueryResult) -> Self {
        let hash = SemanticHash::from_query(&query);
        let depends_on = query.predicates();
        let now = Instant::now();

        Self {
            id: next_cache_id(),
            query,
            result,
            hash,
            created_at: now,
            last_accessed: now,
            access_count: 0,
            depends_on,
            ttl: None,
            valid: true,
        }
    }

    pub fn with_ttl(mut self, ttl: Duration) -> Self {
        self.ttl = Some(ttl);
        self
    }

    pub fn is_expired(&self) -> bool {
        if let Some(ttl) = self.ttl {
            self.created_at.elapsed() > ttl
        } else {
            false
        }
    }

    pub fn touch(&mut self) {
        self.last_accessed = Instant::now();
        self.access_count += 1;
    }
}

/// Cache eviction strategy
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EvictionStrategy {
    LRU,
    LFU,
    FIFO,
    TTL,
    SizeBased,
}

/// Semantic Cache configuration
#[derive(Debug, Clone)]
pub struct CacheConfig {
    /// Maximum number of entries
    pub max_entries: usize,
    /// Maximum memory usage (approximate, in bytes)
    pub max_memory: usize,
    /// Default TTL for entries
    pub default_ttl: Option<Duration>,
    /// Eviction strategy
    pub eviction_strategy: EvictionStrategy,
    /// Enable query subsumption
    pub enable_subsumption: bool,
    /// Enable partial result caching
    pub enable_partial: bool,
}

impl Default for CacheConfig {
    fn default() -> Self {
        Self {
            max_entries: 1000,
            max_memory: 100 * 1024 * 1024,
            default_ttl: None,
            eviction_strategy: EvictionStrategy::LRU,
            enable_subsumption: true,
            enable_partial: true,
        }
    }
}

/// Cache statistics
#[derive(Debug, Clone, Default)]
pub struct CacheStats {
    pub hits: u64,
    pub misses: u64,
    pub partial_hits: u64,
    pub evictions: u64,
    pub invalidations: u64,
    pub entries: usize,
}

impl CacheStats {
    pub fn hit_rate(&self) -> f64 {
        let total = self.hits + self.misses;
        if total == 0 { 0.0 } else { self.hits as f64 / total as f64 }
    }
}

/// Semantic Query Cache
pub struct SemanticCache {
    entries: HashMap<SemanticHash, CacheEntry>,
    predicate_index: HashMap<String, HashSet<CacheId>>,
    lru_queue: VecDeque<CacheId>,
    id_to_hash: HashMap<CacheId, SemanticHash>,
    config: CacheConfig,
    stats: CacheStats,
}

impl SemanticCache {
    pub fn new() -> Self {
        Self::with_config(CacheConfig::default())
    }

    pub fn with_config(config: CacheConfig) -> Self {
        Self {
            entries: HashMap::new(),
            predicate_index: HashMap::new(),
            lru_queue: VecDeque::new(),
            id_to_hash: HashMap::new(),
            config,
            stats: CacheStats::default(),
        }
    }

    /// Look up a query in the cache
    pub fn lookup(&mut self, query: &QueryPattern) -> Option<QueryResult> {
        let hash = SemanticHash::from_query(query);

        // First check if entry is valid and get result + id
        let lookup_result = self.entries.get_mut(&hash).and_then(|entry| {
            if entry.valid && !entry.is_expired() {
                entry.touch();
                Some((entry.id, entry.result.clone()))
            } else {
                None
            }
        });

        if let Some((id, result)) = lookup_result {
            self.update_lru(id);
            self.stats.hits += 1;
            return Some(result);
        }

        self.stats.misses += 1;
        None
    }

    /// Insert a query result into the cache
    pub fn insert(&mut self, query: QueryPattern, result: QueryResult) -> CacheId {
        while self.entries.len() >= self.config.max_entries {
            self.evict_one();
        }

        let mut entry = CacheEntry::new(query, result);
        if let Some(ttl) = self.config.default_ttl {
            entry = entry.with_ttl(ttl);
        }

        let id = entry.id;
        let hash = entry.hash;
        let depends_on = entry.depends_on.clone();

        for pred in &depends_on {
            self.predicate_index.entry(pred.clone()).or_default().insert(id);
        }

        self.lru_queue.push_back(id);
        self.id_to_hash.insert(id, hash);
        self.entries.insert(hash, entry);
        self.stats.entries = self.entries.len();

        id
    }

    /// Invalidate cache entries affected by changes to a predicate
    pub fn invalidate_predicate(&mut self, predicate: &str) {
        if let Some(affected_ids) = self.predicate_index.get(predicate).cloned() {
            for id in affected_ids {
                if let Some(hash) = self.id_to_hash.get(&id) {
                    if let Some(entry) = self.entries.get_mut(hash) {
                        entry.valid = false;
                        self.stats.invalidations += 1;
                    }
                }
            }
        }
    }

    /// Invalidate all cache entries
    pub fn invalidate_all(&mut self) {
        for entry in self.entries.values_mut() {
            entry.valid = false;
        }
        self.stats.invalidations += self.entries.len() as u64;
    }

    fn evict_one(&mut self) {
        match self.config.eviction_strategy {
            EvictionStrategy::LRU | EvictionStrategy::FIFO => self.evict_lru(),
            EvictionStrategy::LFU => self.evict_lfu(),
            EvictionStrategy::TTL => self.evict_expired(),
            EvictionStrategy::SizeBased => self.evict_largest(),
        }
    }

    fn evict_lru(&mut self) {
        while let Some(id) = self.lru_queue.pop_front() {
            if let Some(hash) = self.id_to_hash.remove(&id) {
                if let Some(entry) = self.entries.remove(&hash) {
                    self.remove_from_predicate_index(&entry);
                    self.stats.evictions += 1;
                    self.stats.entries = self.entries.len();
                    return;
                }
            }
        }
    }

    fn evict_lfu(&mut self) {
        let min_access = self.entries.values().map(|e| e.access_count).min().unwrap_or(0);
        let to_remove: Option<SemanticHash> = self.entries.iter()
            .find(|(_, e)| e.access_count == min_access)
            .map(|(h, _)| *h);

        if let Some(hash) = to_remove {
            if let Some(entry) = self.entries.remove(&hash) {
                self.id_to_hash.remove(&entry.id);
                self.lru_queue.retain(|&id| id != entry.id);
                self.remove_from_predicate_index(&entry);
                self.stats.evictions += 1;
                self.stats.entries = self.entries.len();
            }
        }
    }

    fn evict_expired(&mut self) {
        let expired: Vec<SemanticHash> = self.entries.iter()
            .filter(|(_, e)| e.is_expired())
            .map(|(h, _)| *h)
            .collect();

        for hash in expired {
            if let Some(entry) = self.entries.remove(&hash) {
                self.id_to_hash.remove(&entry.id);
                self.lru_queue.retain(|&id| id != entry.id);
                self.remove_from_predicate_index(&entry);
                self.stats.evictions += 1;
            }
        }
        self.stats.entries = self.entries.len();

        if self.entries.len() >= self.config.max_entries {
            self.evict_lru();
        }
    }

    fn evict_largest(&mut self) {
        let largest = self.entries.iter()
            .max_by_key(|(_, e)| e.result.bindings.len())
            .map(|(h, _)| *h);

        if let Some(hash) = largest {
            if let Some(entry) = self.entries.remove(&hash) {
                self.id_to_hash.remove(&entry.id);
                self.lru_queue.retain(|&id| id != entry.id);
                self.remove_from_predicate_index(&entry);
                self.stats.evictions += 1;
                self.stats.entries = self.entries.len();
            }
        }
    }

    fn remove_from_predicate_index(&mut self, entry: &CacheEntry) {
        for pred in &entry.depends_on {
            if let Some(ids) = self.predicate_index.get_mut(pred) {
                ids.remove(&entry.id);
            }
        }
    }

    fn update_lru(&mut self, id: CacheId) {
        self.lru_queue.retain(|&i| i != id);
        self.lru_queue.push_back(id);
    }

    pub fn stats(&self) -> &CacheStats {
        &self.stats
    }

    pub fn clear(&mut self) {
        self.entries.clear();
        self.predicate_index.clear();
        self.lru_queue.clear();
        self.id_to_hash.clear();
        self.stats.entries = 0;
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

impl Default for SemanticCache {
    fn default() -> Self {
        Self::new()
    }
}

/// Materialized View for caching derived facts
#[derive(Debug, Clone)]
pub struct MaterializedView {
    /// View identifier
    pub id: String,
    /// Query pattern defining the view
    pub definition: QueryPattern,
    /// Materialized triples
    pub triples: HashSet<Triple>,
    /// Whether the view is up to date
    pub valid: bool,
    /// Last refresh time
    pub last_refresh: Instant,
    /// Dependencies (predicates that affect this view)
    pub dependencies: HashSet<String>,
}

impl MaterializedView {
    pub fn new(id: impl Into<String>, definition: QueryPattern) -> Self {
        let dependencies = definition.predicates();
        Self {
            id: id.into(),
            definition,
            triples: HashSet::new(),
            valid: false,
            last_refresh: Instant::now(),
            dependencies,
        }
    }

    pub fn invalidate(&mut self) {
        self.valid = false;
    }

    pub fn refresh(&mut self, triples: HashSet<Triple>) {
        self.triples = triples;
        self.valid = true;
        self.last_refresh = Instant::now();
    }

    pub fn needs_refresh(&self) -> bool {
        !self.valid
    }
}

/// Materialized View Manager
pub struct ViewManager {
    views: HashMap<String, MaterializedView>,
    predicate_views: HashMap<String, HashSet<String>>,
}

impl ViewManager {
    pub fn new() -> Self {
        Self {
            views: HashMap::new(),
            predicate_views: HashMap::new(),
        }
    }

    pub fn register(&mut self, view: MaterializedView) {
        for dep in &view.dependencies {
            self.predicate_views.entry(dep.clone()).or_default().insert(view.id.clone());
        }
        self.views.insert(view.id.clone(), view);
    }

    pub fn get(&self, id: &str) -> Option<&MaterializedView> {
        self.views.get(id)
    }

    pub fn get_mut(&mut self, id: &str) -> Option<&mut MaterializedView> {
        self.views.get_mut(id)
    }

    pub fn invalidate_for_predicate(&mut self, predicate: &str) {
        if let Some(view_ids) = self.predicate_views.get(predicate).cloned() {
            for id in view_ids {
                if let Some(view) = self.views.get_mut(&id) {
                    view.invalidate();
                }
            }
        }
    }

    pub fn views_needing_refresh(&self) -> Vec<&str> {
        self.views.iter()
            .filter(|(_, v)| v.needs_refresh())
            .map(|(id, _)| id.as_str())
            .collect()
    }

    pub fn remove(&mut self, id: &str) -> Option<MaterializedView> {
        if let Some(view) = self.views.remove(id) {
            for dep in &view.dependencies {
                if let Some(ids) = self.predicate_views.get_mut(dep) {
                    ids.remove(id);
                }
            }
            Some(view)
        } else {
            None
        }
    }
}

impl Default for ViewManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Query Result Differential for incremental updates
#[derive(Debug, Clone, Default)]
pub struct ResultDiff {
    /// New bindings added
    pub added: Vec<HashMap<String, Term>>,
    /// Old bindings removed
    pub removed: Vec<HashMap<String, Term>>,
}

impl ResultDiff {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.added.is_empty() && self.removed.is_empty()
    }

    pub fn apply(&self, result: &mut QueryResult) {
        for removed in &self.removed {
            result.bindings.retain(|b| b != removed);
        }
        for added in &self.added {
            if !result.bindings.contains(added) {
                result.bindings.push(added.clone());
            }
        }
    }
}

/// Incremental Cache Maintainer
pub struct IncrementalMaintainer {
    cache: SemanticCache,
    pending_additions: Vec<Triple>,
    pending_deletions: Vec<Triple>,
}

impl IncrementalMaintainer {
    pub fn new(cache: SemanticCache) -> Self {
        Self {
            cache,
            pending_additions: Vec::new(),
            pending_deletions: Vec::new(),
        }
    }

    pub fn add_triple(&mut self, triple: Triple) {
        self.pending_additions.push(triple);
    }

    pub fn delete_triple(&mut self, triple: Triple) {
        self.pending_deletions.push(triple);
    }

    pub fn process_changes(&mut self) {
        let mut affected_predicates = HashSet::new();

        for triple in &self.pending_additions {
            if let Term::Uri(u) = &triple.predicate {
                affected_predicates.insert(u.as_str().to_string());
            }
        }

        for triple in &self.pending_deletions {
            if let Term::Uri(u) = &triple.predicate {
                affected_predicates.insert(u.as_str().to_string());
            }
        }

        for pred in affected_predicates {
            self.cache.invalidate_predicate(&pred);
        }

        self.pending_additions.clear();
        self.pending_deletions.clear();
    }

    pub fn cache(&self) -> &SemanticCache {
        &self.cache
    }

    pub fn cache_mut(&mut self) -> &mut SemanticCache {
        &mut self.cache
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_triple(s: &str, p: &str, o: &str) -> Triple {
        Triple {
            subject: Term::uri(s),
            predicate: Term::uri(p),
            object: Term::uri(o),
        }
    }

    fn make_var_triple(s: &str, p: &str, o: &str) -> Triple {
        Triple {
            subject: if s.starts_with('?') {
                Term::universal(&s[1..])
            } else {
                Term::uri(s)
            },
            predicate: if p.starts_with('?') {
                Term::universal(&p[1..])
            } else {
                Term::uri(p)
            },
            object: if o.starts_with('?') {
                Term::universal(&o[1..])
            } else {
                Term::uri(o)
            },
        }
    }

    #[test]
    fn test_semantic_hash() {
        let query1 = QueryPattern::new(vec![make_var_triple("?x", ":knows", "?y")]);
        let query2 = QueryPattern::new(vec![make_var_triple("?x", ":knows", "?y")]);
        let query3 = QueryPattern::new(vec![make_var_triple("?x", ":likes", "?y")]);

        let hash1 = SemanticHash::from_query(&query1);
        let hash2 = SemanticHash::from_query(&query2);
        let hash3 = SemanticHash::from_query(&query3);

        assert_eq!(hash1, hash2);
        assert_ne!(hash1, hash3);
    }

    #[test]
    fn test_cache_insert_lookup() {
        let mut cache = SemanticCache::new();

        let query = QueryPattern::new(vec![make_var_triple("?x", ":knows", ":Bob")]);

        let mut bindings = HashMap::new();
        bindings.insert("x".to_string(), Term::uri(":Alice"));
        let result = QueryResult::new(vec![bindings]);

        cache.insert(query.clone(), result);

        let cached = cache.lookup(&query);
        assert!(cached.is_some());
        assert_eq!(cached.unwrap().bindings.len(), 1);
    }

    #[test]
    fn test_cache_miss() {
        let mut cache = SemanticCache::new();

        let query = QueryPattern::new(vec![make_var_triple("?x", ":knows", ":Bob")]);

        let cached = cache.lookup(&query);
        assert!(cached.is_none());

        let stats = cache.stats();
        assert_eq!(stats.misses, 1);
    }

    #[test]
    fn test_cache_invalidation() {
        let mut cache = SemanticCache::new();

        let query = QueryPattern::new(vec![make_var_triple("?x", ":knows", "?y")]);
        let result = QueryResult::new(vec![]);

        cache.insert(query.clone(), result);
        assert!(cache.lookup(&query).is_some());

        cache.invalidate_predicate(":knows");
        assert!(cache.lookup(&query).is_none());
    }

    #[test]
    fn test_lru_eviction() {
        let config = CacheConfig {
            max_entries: 2,
            eviction_strategy: EvictionStrategy::LRU,
            ..Default::default()
        };
        let mut cache = SemanticCache::with_config(config);

        let query1 = QueryPattern::new(vec![make_var_triple("?x", ":p1", "?y")]);
        let query2 = QueryPattern::new(vec![make_var_triple("?x", ":p2", "?y")]);
        let query3 = QueryPattern::new(vec![make_var_triple("?x", ":p3", "?y")]);

        cache.insert(query1.clone(), QueryResult::new(vec![]));
        cache.insert(query2.clone(), QueryResult::new(vec![]));

        cache.lookup(&query1);

        cache.insert(query3.clone(), QueryResult::new(vec![]));

        assert!(cache.lookup(&query1).is_some());
        assert!(cache.lookup(&query3).is_some());

        assert_eq!(cache.stats().evictions, 1);
    }

    #[test]
    fn test_materialized_view() {
        let definition = QueryPattern::new(vec![make_var_triple("?x", ":knows", "?y")]);
        let mut view = MaterializedView::new("knows_view", definition);

        assert!(view.needs_refresh());

        let mut triples = HashSet::new();
        triples.insert(make_triple(":Alice", ":knows", ":Bob"));
        view.refresh(triples);

        assert!(!view.needs_refresh());
        assert_eq!(view.triples.len(), 1);

        view.invalidate();
        assert!(view.needs_refresh());
    }

    #[test]
    fn test_cache_clear() {
        let mut cache = SemanticCache::new();

        let query = QueryPattern::new(vec![make_var_triple("?x", ":knows", "?y")]);
        cache.insert(query, QueryResult::new(vec![]));

        assert!(!cache.is_empty());

        cache.clear();

        assert!(cache.is_empty());
        assert_eq!(cache.len(), 0);
    }
}
