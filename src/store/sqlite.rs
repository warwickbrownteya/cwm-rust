//! SQLite-backed triple store implementation
//!
//! Provides persistent storage for RDF triples with efficient indexing.
//!
//! # Features
//!
//! - Persistent storage (survives process restarts)
//! - Handles large datasets (100M+ triples)
//! - ACID transactions
//! - Efficient SPO indexes
//! - Term normalization for space efficiency
//!
//! # Usage
//!
//! ```ignore
//! use cwm::store::SqliteStore;
//!
//! // Create a new persistent store
//! let mut store = SqliteStore::open("triples.db")?;
//!
//! // Or use an in-memory SQLite database
//! let mut store = SqliteStore::in_memory()?;
//!
//! store.add(Triple::new(
//!     Term::uri("http://example.org/s"),
//!     Term::uri("http://example.org/p"),
//!     Term::literal("object"),
//! ));
//! ```

use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use rusqlite::{Connection, params, OptionalExtension};

use crate::term::{Term, Triple, Bindings, Datatype, FormulaRef};
use crate::core::TripleStore;

/// Error type for SQLite store operations
#[derive(Debug)]
pub enum SqliteStoreError {
    /// Database error
    Database(rusqlite::Error),
    /// Serialization error
    Serialization(String),
    /// Term not found
    TermNotFound(i64),
}

impl std::fmt::Display for SqliteStoreError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SqliteStoreError::Database(e) => write!(f, "SQLite error: {}", e),
            SqliteStoreError::Serialization(s) => write!(f, "Serialization error: {}", s),
            SqliteStoreError::TermNotFound(id) => write!(f, "Term not found: {}", id),
        }
    }
}

impl std::error::Error for SqliteStoreError {}

impl From<rusqlite::Error> for SqliteStoreError {
    fn from(e: rusqlite::Error) -> Self {
        SqliteStoreError::Database(e)
    }
}

/// Result type for SQLite store operations
pub type SqliteResult<T> = Result<T, SqliteStoreError>;

/// SQLite-backed triple store
///
/// Stores RDF triples in a SQLite database with efficient indexing.
/// Terms are normalized and stored in a separate table to avoid duplication.
pub struct SqliteStore {
    /// Database connection (wrapped in Mutex for Send/Sync)
    conn: Mutex<Connection>,
    /// In-memory cache of term ID -> Term for faster lookups
    term_cache: Mutex<HashMap<i64, Term>>,
    /// In-memory cache of term hash -> term ID
    term_id_cache: Mutex<HashMap<u64, i64>>,
    /// Triple count cache (avoids COUNT(*) queries)
    triple_count: Mutex<usize>,
}

impl SqliteStore {
    /// Open a SQLite store at the given path
    ///
    /// Creates the database file if it doesn't exist.
    pub fn open<P: AsRef<Path>>(path: P) -> SqliteResult<Self> {
        let conn = Connection::open(path)?;
        Self::from_connection(conn)
    }

    /// Create an in-memory SQLite store
    ///
    /// Data is lost when the store is dropped.
    pub fn in_memory() -> SqliteResult<Self> {
        let conn = Connection::open_in_memory()?;
        Self::from_connection(conn)
    }

    /// Create a store from an existing connection
    fn from_connection(conn: Connection) -> SqliteResult<Self> {
        // Enable WAL mode for better concurrent access
        conn.execute_batch("PRAGMA journal_mode = WAL; PRAGMA synchronous = NORMAL;")?;

        // Create tables
        conn.execute_batch(
            r#"
            -- Terms table stores unique terms with their serialized form
            CREATE TABLE IF NOT EXISTS terms (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                term_type INTEGER NOT NULL,  -- 0=URI, 1=Literal, 2=BlankNode, 3=Variable, 4=List, 5=Formula
                value TEXT NOT NULL,
                hash INTEGER NOT NULL,
                UNIQUE(hash, term_type, value)
            );

            -- Index on hash for fast lookups
            CREATE INDEX IF NOT EXISTS idx_terms_hash ON terms(hash);

            -- Triples table stores subject, predicate, object references
            CREATE TABLE IF NOT EXISTS triples (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                subject_id INTEGER NOT NULL REFERENCES terms(id),
                predicate_id INTEGER NOT NULL REFERENCES terms(id),
                object_id INTEGER NOT NULL REFERENCES terms(id),
                UNIQUE(subject_id, predicate_id, object_id)
            );

            -- SPO indexes for efficient pattern matching
            CREATE INDEX IF NOT EXISTS idx_triples_s ON triples(subject_id);
            CREATE INDEX IF NOT EXISTS idx_triples_p ON triples(predicate_id);
            CREATE INDEX IF NOT EXISTS idx_triples_o ON triples(object_id);
            CREATE INDEX IF NOT EXISTS idx_triples_sp ON triples(subject_id, predicate_id);
            CREATE INDEX IF NOT EXISTS idx_triples_po ON triples(predicate_id, object_id);
            CREATE INDEX IF NOT EXISTS idx_triples_so ON triples(subject_id, object_id);
            "#,
        )?;

        // Get initial triple count
        let count: usize = conn.query_row("SELECT COUNT(*) FROM triples", [], |row| row.get(0))?;

        Ok(SqliteStore {
            conn: Mutex::new(conn),
            term_cache: Mutex::new(HashMap::new()),
            term_id_cache: Mutex::new(HashMap::new()),
            triple_count: Mutex::new(count),
        })
    }

    /// Compute a hash for a term
    fn term_hash(term: &Term) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut hasher = DefaultHasher::new();
        term.hash(&mut hasher);
        hasher.finish()
    }

    /// Get the type code for a term
    fn term_type_code(term: &Term) -> i32 {
        match term {
            Term::Uri(_) => 0,
            Term::Literal(_) => 1,
            Term::BlankNode(_) => 2,
            Term::Variable(_) => 3,
            Term::List(_) => 4,
            Term::Formula(_) => 5,
        }
    }

    /// Serialize a term to a string for storage
    fn serialize_term(term: &Term) -> String {
        match term {
            Term::Uri(uri) => uri.as_str().to_string(),
            Term::Literal(lit) => {
                // Format: value|datatype_indicator|datatype_or_lang
                // datatype_indicator: P=plain, L=language, T=typed
                match lit.datatype() {
                    Datatype::Plain => format!("{}|P|", lit.value()),
                    Datatype::Language(lang) => format!("{}|L|{}", lit.value(), lang),
                    Datatype::Typed(dt) => format!("{}|T|{}", lit.value(), dt),
                }
            }
            Term::BlankNode(bn) => {
                // Format: id|label (label may be empty)
                let label = bn.label().unwrap_or("");
                format!("{}|{}", bn.id(), label)
            }
            Term::Variable(v) => {
                // Format: quantification|name (U=universal, E=existential)
                let quant = if v.is_existential() { "E" } else { "U" };
                format!("{}|{}", quant, v.name())
            }
            Term::List(list) => {
                // Serialize list elements as JSON array of serialized terms
                let elements: Vec<String> = list.to_vec().iter()
                    .map(|t| format!("{}:{}", Self::term_type_code(t), Self::serialize_term(t)))
                    .collect();
                serde_json::to_string(&elements).unwrap_or_else(|_| "[]".to_string())
            }
            Term::Formula(f) => {
                // Serialize formula: id followed by serialized triples
                let triples_json: Vec<(String, String, String)> = f.triples().iter()
                    .map(|t| (
                        format!("{}:{}", Self::term_type_code(&t.subject), Self::serialize_term(&t.subject)),
                        format!("{}:{}", Self::term_type_code(&t.predicate), Self::serialize_term(&t.predicate)),
                        format!("{}:{}", Self::term_type_code(&t.object), Self::serialize_term(&t.object)),
                    ))
                    .collect();
                format!("{}|{}", f.id(), serde_json::to_string(&triples_json).unwrap_or_else(|_| "[]".to_string()))
            }
        }
    }

    /// Deserialize a term from storage
    fn deserialize_term(term_type: i32, value: &str) -> SqliteResult<Term> {
        match term_type {
            0 => Ok(Term::uri(value)),
            1 => {
                // Parse: value|indicator|datatype_or_lang
                let parts: Vec<&str> = value.splitn(3, '|').collect();
                let val = parts.first().unwrap_or(&"");
                let indicator = parts.get(1).unwrap_or(&"P");
                let extra = parts.get(2).unwrap_or(&"");

                match *indicator {
                    "L" => Ok(Term::lang_literal(*val, *extra)),
                    "T" => Ok(Term::typed_literal(*val, *extra)),
                    _ => Ok(Term::literal(*val)),
                }
            }
            2 => {
                // Parse: id|label
                let parts: Vec<&str> = value.splitn(2, '|').collect();
                let label = parts.get(1).filter(|s| !s.is_empty());
                match label {
                    Some(lbl) => Ok(Term::blank(*lbl)),
                    None => Ok(Term::fresh_blank()),
                }
            }
            3 => {
                // Parse: quantification|name
                let parts: Vec<&str> = value.splitn(2, '|').collect();
                let quant = parts.first().unwrap_or(&"U");
                let name = parts.get(1).unwrap_or(&"");
                if *quant == "E" {
                    Ok(Term::existential(*name))
                } else {
                    Ok(Term::universal(*name))
                }
            }
            4 => {
                // Parse list from JSON array
                let elements: Vec<String> = serde_json::from_str(value)
                    .map_err(|e| SqliteStoreError::Serialization(e.to_string()))?;
                let terms: SqliteResult<Vec<Term>> = elements.iter()
                    .map(|s| {
                        // Parse "type:value"
                        let parts: Vec<&str> = s.splitn(2, ':').collect();
                        let t: i32 = parts.first().unwrap_or(&"0").parse().unwrap_or(0);
                        let v = parts.get(1).unwrap_or(&"");
                        Self::deserialize_term(t, v)
                    })
                    .collect();
                Ok(Term::list(terms?))
            }
            5 => {
                // Parse formula: id|triples_json
                let parts: Vec<&str> = value.splitn(2, '|').collect();
                let id: u64 = parts.first().unwrap_or(&"0").parse().unwrap_or(0);
                let triples_json = parts.get(1).unwrap_or(&"[]");

                let triples_data: Vec<(String, String, String)> = serde_json::from_str(triples_json)
                    .map_err(|e| SqliteStoreError::Serialization(e.to_string()))?;

                let triples: SqliteResult<Vec<Triple>> = triples_data.iter()
                    .map(|(s, p, o)| {
                        let parse_term = |t: &str| -> SqliteResult<Term> {
                            let parts: Vec<&str> = t.splitn(2, ':').collect();
                            let typ: i32 = parts.first().unwrap_or(&"0").parse().unwrap_or(0);
                            let val = parts.get(1).unwrap_or(&"");
                            Self::deserialize_term(typ, val)
                        };
                        Ok(Triple::new(parse_term(s)?, parse_term(p)?, parse_term(o)?))
                    })
                    .collect();

                Ok(Term::Formula(FormulaRef::new(id, triples?)))
            }
            _ => Err(SqliteStoreError::Serialization(format!("Unknown term type: {}", term_type))),
        }
    }

    /// Get or create a term ID
    fn get_or_create_term_id(&self, conn: &Connection, term: &Term) -> SqliteResult<i64> {
        let hash = Self::term_hash(term) as i64;

        // Check cache first
        {
            let cache = self.term_id_cache.lock().unwrap();
            if let Some(&id) = cache.get(&(hash as u64)) {
                return Ok(id);
            }
        }

        let term_type = Self::term_type_code(term);
        let value = Self::serialize_term(term);

        // Try to find existing term
        let existing: Option<i64> = conn.query_row(
            "SELECT id FROM terms WHERE hash = ? AND term_type = ? AND value = ?",
            params![hash, term_type, value],
            |row| row.get(0),
        ).optional()?;

        if let Some(id) = existing {
            // Cache it
            let mut cache = self.term_id_cache.lock().unwrap();
            cache.insert(hash as u64, id);
            return Ok(id);
        }

        // Insert new term
        conn.execute(
            "INSERT INTO terms (term_type, value, hash) VALUES (?, ?, ?)",
            params![term_type, value, hash],
        )?;
        let id = conn.last_insert_rowid();

        // Cache both directions
        {
            let mut id_cache = self.term_id_cache.lock().unwrap();
            id_cache.insert(hash as u64, id);
        }
        {
            let mut term_cache = self.term_cache.lock().unwrap();
            term_cache.insert(id, term.clone());
        }

        Ok(id)
    }

    /// Get a term by ID
    fn get_term_by_id(&self, conn: &Connection, id: i64) -> SqliteResult<Term> {
        // Check cache first
        {
            let cache = self.term_cache.lock().unwrap();
            if let Some(term) = cache.get(&id) {
                return Ok(term.clone());
            }
        }

        let (term_type, value): (i32, String) = conn.query_row(
            "SELECT term_type, value FROM terms WHERE id = ?",
            params![id],
            |row| Ok((row.get(0)?, row.get(1)?)),
        )?;

        let term = Self::deserialize_term(term_type, &value)?;

        // Cache it
        let mut cache = self.term_cache.lock().unwrap();
        cache.insert(id, term.clone());

        Ok(term)
    }

    /// Match a pattern against the store
    pub fn match_pattern(&self, pattern: &Triple) -> Vec<Bindings> {
        let conn = self.conn.lock().unwrap();
        self.match_pattern_with_conn(&conn, pattern).unwrap_or_default()
    }

    /// Internal pattern matching with connection
    fn match_pattern_with_conn(&self, conn: &Connection, pattern: &Triple) -> SqliteResult<Vec<Bindings>> {
        // Build query based on which positions are ground
        let subj_ground = !matches!(pattern.subject, Term::Variable(_));
        let pred_ground = !matches!(pattern.predicate, Term::Variable(_));
        let obj_ground = !matches!(pattern.object, Term::Variable(_));

        // Get term IDs for ground positions
        let subj_id = if subj_ground {
            self.get_or_create_term_id(conn, &pattern.subject).ok()
        } else {
            None
        };
        let pred_id = if pred_ground {
            self.get_or_create_term_id(conn, &pattern.predicate).ok()
        } else {
            None
        };
        let obj_id = if obj_ground {
            self.get_or_create_term_id(conn, &pattern.object).ok()
        } else {
            None
        };

        // Build WHERE clause
        let mut conditions = Vec::new();
        let mut params_vec: Vec<i64> = Vec::new();

        if let Some(id) = subj_id {
            conditions.push("subject_id = ?");
            params_vec.push(id);
        }
        if let Some(id) = pred_id {
            conditions.push("predicate_id = ?");
            params_vec.push(id);
        }
        if let Some(id) = obj_id {
            conditions.push("object_id = ?");
            params_vec.push(id);
        }

        let where_clause = if conditions.is_empty() {
            String::new()
        } else {
            format!(" WHERE {}", conditions.join(" AND "))
        };

        let query = format!(
            "SELECT subject_id, predicate_id, object_id FROM triples{}",
            where_clause
        );

        let mut stmt = conn.prepare(&query)?;

        let params_refs: Vec<&dyn rusqlite::ToSql> = params_vec.iter()
            .map(|p| p as &dyn rusqlite::ToSql)
            .collect();

        let rows = stmt.query_map(params_refs.as_slice(), |row| {
            Ok((row.get::<_, i64>(0)?, row.get::<_, i64>(1)?, row.get::<_, i64>(2)?))
        })?;

        let mut results = Vec::new();

        for row in rows {
            let (sid, pid, oid) = row?;

            let subj_term = self.get_term_by_id(conn, sid)?;
            let pred_term = self.get_term_by_id(conn, pid)?;
            let obj_term = self.get_term_by_id(conn, oid)?;

            // Try to unify with pattern
            if let Some(bindings) = self.unify_triple(pattern, &Triple::new(subj_term, pred_term, obj_term)) {
                results.push(bindings);
            }
        }

        Ok(results)
    }

    /// Unify a pattern with a ground triple
    fn unify_triple(&self, pattern: &Triple, ground: &Triple) -> Option<Bindings> {
        let mut bindings = Bindings::default();

        if !self.unify_term(&pattern.subject, &ground.subject, &mut bindings) {
            return None;
        }
        if !self.unify_term(&pattern.predicate, &ground.predicate, &mut bindings) {
            return None;
        }
        if !self.unify_term(&pattern.object, &ground.object, &mut bindings) {
            return None;
        }

        Some(bindings)
    }

    /// Unify a pattern term with a ground term
    fn unify_term(&self, pattern: &Term, ground: &Term, bindings: &mut Bindings) -> bool {
        match pattern {
            Term::Variable(var) => {
                if let Some(existing) = bindings.get(var) {
                    existing == ground
                } else {
                    bindings.insert(var.clone(), ground.clone());
                    true
                }
            }
            _ => pattern == ground,
        }
    }

    /// Query with multiple patterns
    pub fn query(&self, patterns: &[Triple]) -> Vec<Bindings> {
        if patterns.is_empty() {
            return vec![Bindings::default()];
        }

        let conn = self.conn.lock().unwrap();

        let mut results = self.match_pattern_with_conn(&conn, &patterns[0]).unwrap_or_default();

        for pattern in &patterns[1..] {
            let mut new_results = Vec::new();

            for bindings in results {
                // Apply current bindings to pattern
                let substituted = crate::term::substitute_triple(pattern, &bindings);

                // Match against store
                for new_bindings in self.match_pattern_with_conn(&conn, &substituted).unwrap_or_default() {
                    // Merge bindings
                    let mut merged = bindings.clone();
                    for (var, term) in new_bindings {
                        merged.insert(var, term);
                    }
                    new_results.push(merged);
                }
            }

            results = new_results;
        }

        results
    }

    /// Flush all pending writes and sync to disk
    pub fn flush(&self) -> SqliteResult<()> {
        let conn = self.conn.lock().unwrap();
        conn.execute_batch("PRAGMA wal_checkpoint(TRUNCATE);")?;
        Ok(())
    }

    /// Get statistics about the store
    pub fn stats(&self) -> SqliteStoreStats {
        let conn = self.conn.lock().unwrap();

        let triple_count = *self.triple_count.lock().unwrap();
        let term_count: usize = conn.query_row("SELECT COUNT(*) FROM terms", [], |row| row.get(0))
            .unwrap_or(0);
        let term_cache_size = self.term_cache.lock().unwrap().len();

        SqliteStoreStats {
            triple_count,
            term_count,
            term_cache_size,
        }
    }

    /// Begin a transaction for batch operations
    pub fn begin_transaction(&self) -> SqliteResult<()> {
        let conn = self.conn.lock().unwrap();
        conn.execute("BEGIN TRANSACTION", [])?;
        Ok(())
    }

    /// Commit the current transaction
    pub fn commit_transaction(&self) -> SqliteResult<()> {
        let conn = self.conn.lock().unwrap();
        conn.execute("COMMIT", [])?;
        Ok(())
    }

    /// Rollback the current transaction
    pub fn rollback_transaction(&self) -> SqliteResult<()> {
        let conn = self.conn.lock().unwrap();
        conn.execute("ROLLBACK", [])?;
        Ok(())
    }

    /// Add multiple triples efficiently in a transaction
    pub fn add_batch(&mut self, triples: impl IntoIterator<Item = Triple>) {
        let conn = self.conn.lock().unwrap();

        // Start transaction
        if conn.execute("BEGIN TRANSACTION", []).is_err() {
            return;
        }

        let mut count = 0;
        for triple in triples {
            if self.add_with_conn(&conn, triple).is_ok() {
                count += 1;
            }
        }

        // Commit transaction
        let _ = conn.execute("COMMIT", []);

        // Update count
        let mut triple_count = self.triple_count.lock().unwrap();
        *triple_count += count;
    }

    /// Internal add with connection
    fn add_with_conn(&self, conn: &Connection, triple: Triple) -> SqliteResult<bool> {
        let subj_id = self.get_or_create_term_id(conn, &triple.subject)?;
        let pred_id = self.get_or_create_term_id(conn, &triple.predicate)?;
        let obj_id = self.get_or_create_term_id(conn, &triple.object)?;

        // Insert with IGNORE to handle duplicates
        let rows = conn.execute(
            "INSERT OR IGNORE INTO triples (subject_id, predicate_id, object_id) VALUES (?, ?, ?)",
            params![subj_id, pred_id, obj_id],
        )?;

        Ok(rows > 0)
    }
}

/// Statistics about the SQLite store
#[derive(Debug, Clone)]
pub struct SqliteStoreStats {
    /// Number of triples in the store
    pub triple_count: usize,
    /// Number of unique terms
    pub term_count: usize,
    /// Size of the in-memory term cache
    pub term_cache_size: usize,
}

// Implement TripleStore trait
impl TripleStore for SqliteStore {
    fn add(&mut self, triple: Triple) {
        let conn = self.conn.lock().unwrap();
        if self.add_with_conn(&conn, triple).unwrap_or(false) {
            let mut count = self.triple_count.lock().unwrap();
            *count += 1;
        }
    }

    fn remove(&mut self, triple: &Triple) -> bool {
        let conn = self.conn.lock().unwrap();

        // Get term IDs
        let hash_s = Self::term_hash(&triple.subject) as i64;
        let hash_p = Self::term_hash(&triple.predicate) as i64;
        let hash_o = Self::term_hash(&triple.object) as i64;

        let type_s = Self::term_type_code(&triple.subject);
        let type_p = Self::term_type_code(&triple.predicate);
        let type_o = Self::term_type_code(&triple.object);

        let val_s = Self::serialize_term(&triple.subject);
        let val_p = Self::serialize_term(&triple.predicate);
        let val_o = Self::serialize_term(&triple.object);

        // Find term IDs
        let subj_id: Option<i64> = conn.query_row(
            "SELECT id FROM terms WHERE hash = ? AND term_type = ? AND value = ?",
            params![hash_s, type_s, val_s],
            |row| row.get(0),
        ).optional().ok().flatten();

        let pred_id: Option<i64> = conn.query_row(
            "SELECT id FROM terms WHERE hash = ? AND term_type = ? AND value = ?",
            params![hash_p, type_p, val_p],
            |row| row.get(0),
        ).optional().ok().flatten();

        let obj_id: Option<i64> = conn.query_row(
            "SELECT id FROM terms WHERE hash = ? AND term_type = ? AND value = ?",
            params![hash_o, type_o, val_o],
            |row| row.get(0),
        ).optional().ok().flatten();

        if let (Some(sid), Some(pid), Some(oid)) = (subj_id, pred_id, obj_id) {
            let rows = conn.execute(
                "DELETE FROM triples WHERE subject_id = ? AND predicate_id = ? AND object_id = ?",
                params![sid, pid, oid],
            ).unwrap_or(0);

            if rows > 0 {
                let mut count = self.triple_count.lock().unwrap();
                *count = count.saturating_sub(1);
                return true;
            }
        }

        false
    }

    fn contains(&self, triple: &Triple) -> bool {
        let conn = self.conn.lock().unwrap();

        // Get term IDs
        let hash_s = Self::term_hash(&triple.subject) as i64;
        let hash_p = Self::term_hash(&triple.predicate) as i64;
        let hash_o = Self::term_hash(&triple.object) as i64;

        let type_s = Self::term_type_code(&triple.subject);
        let type_p = Self::term_type_code(&triple.predicate);
        let type_o = Self::term_type_code(&triple.object);

        let val_s = Self::serialize_term(&triple.subject);
        let val_p = Self::serialize_term(&triple.predicate);
        let val_o = Self::serialize_term(&triple.object);

        // Check if all terms exist and triple exists
        let result: Option<i64> = conn.query_row(
            r#"
            SELECT t.id FROM triples t
            JOIN terms ts ON t.subject_id = ts.id
            JOIN terms tp ON t.predicate_id = tp.id
            JOIN terms to_ ON t.object_id = to_.id
            WHERE ts.hash = ? AND ts.term_type = ? AND ts.value = ?
              AND tp.hash = ? AND tp.term_type = ? AND tp.value = ?
              AND to_.hash = ? AND to_.term_type = ? AND to_.value = ?
            LIMIT 1
            "#,
            params![hash_s, type_s, val_s, hash_p, type_p, val_p, hash_o, type_o, val_o],
            |row| row.get(0),
        ).optional().ok().flatten();

        result.is_some()
    }

    fn len(&self) -> usize {
        *self.triple_count.lock().unwrap()
    }

    fn iter(&self) -> Box<dyn Iterator<Item = &Triple> + '_> {
        // SQLite store can't return references to internal storage
        // This is a limitation - we need to collect all triples
        // For large stores, use match_pattern or query instead

        // Return empty iterator - callers should use to_vec() for SQLite stores
        Box::new(std::iter::empty())
    }

    fn to_vec(&self) -> Vec<Triple> {
        let conn = self.conn.lock().unwrap();

        let mut stmt = match conn.prepare("SELECT subject_id, predicate_id, object_id FROM triples") {
            Ok(s) => s,
            Err(_) => return Vec::new(),
        };

        let rows = match stmt.query_map([], |row| {
            Ok((row.get::<_, i64>(0)?, row.get::<_, i64>(1)?, row.get::<_, i64>(2)?))
        }) {
            Ok(r) => r,
            Err(_) => return Vec::new(),
        };

        let mut triples = Vec::new();

        for row in rows {
            if let Ok((sid, pid, oid)) = row {
                if let (Ok(s), Ok(p), Ok(o)) = (
                    self.get_term_by_id(&conn, sid),
                    self.get_term_by_id(&conn, pid),
                    self.get_term_by_id(&conn, oid),
                ) {
                    triples.push(Triple::new(s, p, o));
                }
            }
        }

        triples
    }

    fn clear(&mut self) {
        let conn = self.conn.lock().unwrap();
        let _ = conn.execute_batch("DELETE FROM triples; DELETE FROM terms;");

        // Clear caches
        self.term_cache.lock().unwrap().clear();
        self.term_id_cache.lock().unwrap().clear();
        *self.triple_count.lock().unwrap() = 0;
    }
}

// Implement Send + Sync (Mutex provides thread safety)
unsafe impl Send for SqliteStore {}
unsafe impl Sync for SqliteStore {}

impl std::fmt::Debug for SqliteStore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stats = self.stats();
        f.debug_struct("SqliteStore")
            .field("triple_count", &stats.triple_count)
            .field("term_count", &stats.term_count)
            .field("term_cache_size", &stats.term_cache_size)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_in_memory_store() {
        let mut store = SqliteStore::in_memory().unwrap();

        let triple = Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::literal("object"),
        );

        store.add(triple.clone());
        assert_eq!(store.len(), 1);
        assert!(store.contains(&triple));
    }

    #[test]
    fn test_no_duplicates() {
        let mut store = SqliteStore::in_memory().unwrap();

        let triple = Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::literal("object"),
        );

        store.add(triple.clone());
        store.add(triple.clone());
        assert_eq!(store.len(), 1);
    }

    #[test]
    fn test_pattern_matching() {
        let mut store = SqliteStore::in_memory().unwrap();

        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/bob"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/charlie"),
        ));

        // Match all things alice knows
        let pattern = Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::universal("x"),
        );

        let results = store.match_pattern(&pattern);
        assert_eq!(results.len(), 2);
    }

    #[test]
    fn test_query() {
        let mut store = SqliteStore::in_memory().unwrap();

        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/bob"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/bob"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/charlie"),
        ));

        // Find X where alice knows X and X knows Y
        let patterns = vec![
            Triple::new(
                Term::uri("http://example.org/alice"),
                Term::uri("http://example.org/knows"),
                Term::universal("x"),
            ),
            Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/knows"),
                Term::universal("y"),
            ),
        ];

        let results = store.query(&patterns);
        assert_eq!(results.len(), 1);
    }

    #[test]
    fn test_remove() {
        let mut store = SqliteStore::in_memory().unwrap();

        let triple = Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::literal("object"),
        );

        store.add(triple.clone());
        assert_eq!(store.len(), 1);

        assert!(store.remove(&triple));
        assert_eq!(store.len(), 0);
        assert!(!store.contains(&triple));
    }

    #[test]
    fn test_to_vec() {
        let mut store = SqliteStore::in_memory().unwrap();

        store.add(Triple::new(
            Term::uri("http://example.org/s1"),
            Term::uri("http://example.org/p"),
            Term::literal("o1"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/s2"),
            Term::uri("http://example.org/p"),
            Term::literal("o2"),
        ));

        let triples = store.to_vec();
        assert_eq!(triples.len(), 2);
    }

    #[test]
    fn test_clear() {
        let mut store = SqliteStore::in_memory().unwrap();

        store.add(Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::literal("object"),
        ));

        store.clear();
        assert_eq!(store.len(), 0);
        assert!(store.to_vec().is_empty());
    }

    #[test]
    fn test_batch_add() {
        let mut store = SqliteStore::in_memory().unwrap();

        let triples = (0..100).map(|i| Triple::new(
            Term::uri(&format!("http://example.org/s{}", i)),
            Term::uri("http://example.org/p"),
            Term::literal(&format!("o{}", i)),
        ));

        store.add_batch(triples);
        assert_eq!(store.len(), 100);
    }

    #[test]
    fn test_literal_types() {
        let mut store = SqliteStore::in_memory().unwrap();

        // String literal
        store.add(Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/name"),
            Term::literal("Alice"),
        ));

        // Typed literal
        store.add(Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/age"),
            Term::typed_literal("30", "http://www.w3.org/2001/XMLSchema#integer"),
        ));

        // Language-tagged literal
        store.add(Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/label"),
            Term::lang_literal("Bonjour", "fr"),
        ));

        let triples = store.to_vec();
        assert_eq!(triples.len(), 3);
    }

    #[test]
    fn test_stats() {
        let mut store = SqliteStore::in_memory().unwrap();

        store.add(Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::literal("object"),
        ));

        let stats = store.stats();
        assert_eq!(stats.triple_count, 1);
        assert_eq!(stats.term_count, 3); // 3 unique terms
    }
}
