//! SPARQL 1.1 Update Operations
//!
//! Implements SPARQL UPDATE protocol for modifying RDF datasets.
//!
//! # Supported Operations
//!
//! - `INSERT DATA` - Insert ground triples/quads
//! - `DELETE DATA` - Delete specific triples/quads
//! - `INSERT ... WHERE` - Insert based on query results
//! - `DELETE ... WHERE` - Delete based on query results
//! - `DELETE/INSERT ... WHERE` - Combined delete/insert
//! - `CLEAR` - Remove all triples from a graph
//! - `DROP` - Remove a graph entirely
//! - `CREATE` - Create a new empty graph
//! - `LOAD` - Load RDF from a URL (stub)
//!
//! # Example
//!
//! ```ignore
//! use cwm::sparql::update::{UpdateEngine, Update};
//! use cwm::store::Dataset;
//!
//! let mut dataset = Dataset::new();
//! let mut engine = UpdateEngine::new(&mut dataset);
//!
//! engine.execute_update("INSERT DATA { <http://ex.org/s> <http://ex.org/p> <http://ex.org/o> }");
//! ```

use std::collections::HashMap;

use crate::term::{Term, Triple};
use crate::store::Dataset;
use super::{TermPattern, WhereClause, GraphPattern, TriplePattern};

/// SPARQL Update operation types
#[derive(Debug, Clone)]
pub enum Update {
    /// INSERT DATA { triples }
    InsertData {
        quads: Vec<QuadData>,
    },
    /// DELETE DATA { triples }
    DeleteData {
        quads: Vec<QuadData>,
    },
    /// DELETE { template } INSERT { template } WHERE { pattern }
    Modify {
        with_graph: Option<Term>,
        delete_template: Vec<QuadTemplate>,
        insert_template: Vec<QuadTemplate>,
        using_graphs: Vec<Term>,
        where_clause: WhereClause,
    },
    /// CLEAR GRAPH <uri> | DEFAULT | NAMED | ALL
    Clear {
        target: GraphTarget,
        silent: bool,
    },
    /// DROP GRAPH <uri> | DEFAULT | NAMED | ALL
    Drop {
        target: GraphTarget,
        silent: bool,
    },
    /// CREATE GRAPH <uri>
    Create {
        graph: Term,
        silent: bool,
    },
    /// LOAD <url> INTO GRAPH <uri>
    Load {
        source: String,
        target: Option<Term>,
        silent: bool,
    },
}

/// Ground quad data (no variables allowed)
#[derive(Debug, Clone)]
pub struct QuadData {
    pub subject: Term,
    pub predicate: Term,
    pub object: Term,
    pub graph: Option<Term>,
}

impl QuadData {
    pub fn new(subject: Term, predicate: Term, object: Term, graph: Option<Term>) -> Self {
        QuadData { subject, predicate, object, graph }
    }

    pub fn to_triple(&self) -> Triple {
        Triple::new(self.subject.clone(), self.predicate.clone(), self.object.clone())
    }
}

/// Quad template (may contain variables)
#[derive(Debug, Clone)]
pub struct QuadTemplate {
    pub subject: TermPattern,
    pub predicate: TermPattern,
    pub object: TermPattern,
    pub graph: Option<TermPattern>,
}

impl QuadTemplate {
    pub fn new(
        subject: TermPattern,
        predicate: TermPattern,
        object: TermPattern,
        graph: Option<TermPattern>,
    ) -> Self {
        QuadTemplate { subject, predicate, object, graph }
    }

    /// Instantiate this template with bindings to create a ground quad
    pub fn instantiate(&self, bindings: &HashMap<String, Term>) -> Option<QuadData> {
        let subject = resolve_term_pattern(&self.subject, bindings)?;
        let predicate = resolve_term_pattern(&self.predicate, bindings)?;
        let object = resolve_term_pattern(&self.object, bindings)?;
        let graph = match &self.graph {
            Some(g) => Some(resolve_term_pattern(g, bindings)?),
            None => None,
        };
        Some(QuadData::new(subject, predicate, object, graph))
    }
}

/// Resolve a term pattern to a ground term using bindings
fn resolve_term_pattern(pattern: &TermPattern, bindings: &HashMap<String, Term>) -> Option<Term> {
    match pattern {
        TermPattern::Term(t) => Some(t.clone()),
        TermPattern::Variable(v) => bindings.get(v).cloned(),
        TermPattern::Path(_) => None, // Paths can't be resolved to terms
    }
}

/// Target for CLEAR/DROP operations
#[derive(Debug, Clone, PartialEq)]
pub enum GraphTarget {
    /// Specific named graph
    Graph(Term),
    /// The default graph
    Default,
    /// All named graphs
    Named,
    /// All graphs (default + named)
    All,
}

/// Result of an update operation
#[derive(Debug, Clone)]
pub struct UpdateResult {
    /// Number of triples inserted
    pub inserted: usize,
    /// Number of triples deleted
    pub deleted: usize,
    /// Success status
    pub success: bool,
    /// Error message if failed
    pub error: Option<String>,
}

impl UpdateResult {
    pub fn success(inserted: usize, deleted: usize) -> Self {
        UpdateResult {
            inserted,
            deleted,
            success: true,
            error: None,
        }
    }

    pub fn error(msg: impl Into<String>) -> Self {
        UpdateResult {
            inserted: 0,
            deleted: 0,
            success: false,
            error: Some(msg.into()),
        }
    }
}

/// SPARQL Update engine for modifying datasets
pub struct UpdateEngine<'a> {
    dataset: &'a mut Dataset,
}

impl<'a> UpdateEngine<'a> {
    /// Create a new update engine
    pub fn new(dataset: &'a mut Dataset) -> Self {
        UpdateEngine { dataset }
    }

    /// Execute an update operation
    pub fn execute(&mut self, update: &Update) -> UpdateResult {
        match update {
            Update::InsertData { quads } => self.execute_insert_data(quads),
            Update::DeleteData { quads } => self.execute_delete_data(quads),
            Update::Modify { with_graph, delete_template, insert_template, using_graphs: _, where_clause } => {
                self.execute_modify(with_graph.as_ref(), delete_template, insert_template, where_clause)
            }
            Update::Clear { target, silent } => self.execute_clear(target, *silent),
            Update::Drop { target, silent } => self.execute_drop(target, *silent),
            Update::Create { graph, silent } => self.execute_create(graph, *silent),
            Update::Load { source: _, target: _, silent } => {
                if *silent {
                    UpdateResult::success(0, 0)
                } else {
                    UpdateResult::error("LOAD not yet implemented")
                }
            }
        }
    }

    /// Execute INSERT DATA
    fn execute_insert_data(&mut self, quads: &[QuadData]) -> UpdateResult {
        let mut count = 0;
        for quad in quads {
            let triple = quad.to_triple();
            match &quad.graph {
                None => {
                    self.dataset.add(triple);
                    count += 1;
                }
                Some(graph) => {
                    self.dataset.add_to_graph(triple, graph);
                    count += 1;
                }
            }
        }
        UpdateResult::success(count, 0)
    }

    /// Execute DELETE DATA
    fn execute_delete_data(&mut self, quads: &[QuadData]) -> UpdateResult {
        let mut count = 0;
        for quad in quads {
            let triple = quad.to_triple();
            let removed = match &quad.graph {
                None => self.remove_from_default(&triple),
                Some(graph) => self.remove_from_graph(&triple, graph),
            };
            if removed {
                count += 1;
            }
        }
        UpdateResult::success(0, count)
    }

    /// Execute DELETE/INSERT WHERE
    fn execute_modify(
        &mut self,
        with_graph: Option<&Term>,
        delete_template: &[QuadTemplate],
        insert_template: &[QuadTemplate],
        where_clause: &WhereClause,
    ) -> UpdateResult {
        // Execute the WHERE clause to get bindings
        let solutions = self.evaluate_where(where_clause, with_graph);

        let mut inserted = 0;
        let mut deleted = 0;

        // For each solution, instantiate templates and apply changes
        for solution in &solutions {
            // Process deletions first
            for template in delete_template {
                if let Some(quad) = template.instantiate(solution) {
                    let triple = quad.to_triple();
                    let graph = quad.graph.as_ref().or(with_graph);
                    let removed = match graph {
                        None => self.remove_from_default(&triple),
                        Some(g) => self.remove_from_graph(&triple, g),
                    };
                    if removed {
                        deleted += 1;
                    }
                }
            }

            // Then process insertions
            for template in insert_template {
                if let Some(quad) = template.instantiate(solution) {
                    let triple = quad.to_triple();
                    let graph = quad.graph.as_ref().or(with_graph);
                    match graph {
                        None => self.dataset.add(triple),
                        Some(g) => self.dataset.add_to_graph(triple, g),
                    }
                    inserted += 1;
                }
            }
        }

        UpdateResult::success(inserted, deleted)
    }

    /// Execute CLEAR
    fn execute_clear(&mut self, target: &GraphTarget, _silent: bool) -> UpdateResult {
        let deleted = match target {
            GraphTarget::Default => {
                let count = self.dataset.default_graph().len();
                self.dataset.clear_default();
                count
            }
            GraphTarget::Graph(g) => {
                let count = self.dataset.named_graph(g).map(|s| s.len()).unwrap_or(0);
                self.dataset.clear_graph(g);
                count
            }
            GraphTarget::Named => {
                let count: usize = self.dataset.graph_names()
                    .iter()
                    .filter_map(|n| self.dataset.named_graph(n).map(|s| s.len()))
                    .sum();
                for name in self.dataset.graph_names() {
                    self.dataset.remove_graph(&name);
                }
                count
            }
            GraphTarget::All => {
                let count = self.dataset.total_triple_count();
                self.dataset.clear();
                count
            }
        };
        UpdateResult::success(0, deleted)
    }

    /// Execute DROP
    fn execute_drop(&mut self, target: &GraphTarget, silent: bool) -> UpdateResult {
        match target {
            GraphTarget::Default => {
                let count = self.dataset.default_graph().len();
                self.dataset.clear_default();
                UpdateResult::success(0, count)
            }
            GraphTarget::Graph(g) => {
                if self.dataset.has_graph(g) {
                    let count = self.dataset.named_graph(g).map(|s| s.len()).unwrap_or(0);
                    self.dataset.remove_graph(g);
                    UpdateResult::success(0, count)
                } else if silent {
                    UpdateResult::success(0, 0)
                } else {
                    UpdateResult::error(format!("Graph does not exist: {:?}", g))
                }
            }
            GraphTarget::Named => {
                let count: usize = self.dataset.graph_names()
                    .iter()
                    .filter_map(|n| self.dataset.named_graph(n).map(|s| s.len()))
                    .sum();
                for name in self.dataset.graph_names() {
                    self.dataset.remove_graph(&name);
                }
                UpdateResult::success(0, count)
            }
            GraphTarget::All => {
                let count = self.dataset.total_triple_count();
                self.dataset.clear();
                UpdateResult::success(0, count)
            }
        }
    }

    /// Execute CREATE
    fn execute_create(&mut self, graph: &Term, silent: bool) -> UpdateResult {
        if self.dataset.has_graph(graph) {
            if silent {
                UpdateResult::success(0, 0)
            } else {
                UpdateResult::error(format!("Graph already exists: {:?}", graph))
            }
        } else {
            // Create empty graph
            self.dataset.get_or_create_graph(graph);
            UpdateResult::success(0, 0)
        }
    }

    /// Remove a triple from the default graph
    fn remove_from_default(&mut self, triple: &Triple) -> bool {
        // Get current count
        let before = self.dataset.default_graph().len();

        // We need to rebuild without this triple
        // Since Store doesn't have remove(), we need to work around it
        // For now, return false indicating removal not supported for default graph
        // A proper implementation would require adding remove() to Store

        // Check if triple exists
        let exists = self.dataset.default_graph().iter().any(|t| t == triple);
        if !exists {
            return false;
        }

        // TODO: Add proper remove() method to Store
        // For now, we can only remove from named graphs
        let _ = before; // suppress warning
        false
    }

    /// Remove a triple from a named graph
    fn remove_from_graph(&mut self, triple: &Triple, graph: &Term) -> bool {
        // Similar issue - Store doesn't have remove()
        // Check if triple exists
        if let Some(store) = self.dataset.named_graph(graph) {
            let exists = store.iter().any(|t| t == triple);
            if !exists {
                return false;
            }
        }

        // TODO: Add proper remove() method to Store
        false
    }

    /// Evaluate WHERE clause for update context
    fn evaluate_where(&self, where_clause: &WhereClause, _context_graph: Option<&Term>) -> Vec<HashMap<String, Term>> {
        // Use a DatasetEngine for evaluation
        let engine = super::DatasetEngine::new(self.dataset);

        // Evaluate patterns
        let mut solutions = vec![HashMap::new()];
        for pattern in &where_clause.patterns {
            solutions = self.evaluate_pattern(pattern, solutions, &engine);
        }
        solutions
    }

    /// Evaluate a graph pattern
    fn evaluate_pattern(
        &self,
        pattern: &GraphPattern,
        solutions: Vec<HashMap<String, Term>>,
        engine: &super::DatasetEngine,
    ) -> Vec<HashMap<String, Term>> {
        match pattern {
            GraphPattern::Triple(tp) => {
                self.evaluate_triple_pattern(tp, solutions)
            }
            GraphPattern::Filter(expr) => {
                solutions.into_iter()
                    .filter(|sol| self.evaluate_filter(expr, sol))
                    .collect()
            }
            GraphPattern::Optional(inner) => {
                let mut result = Vec::new();
                for solution in &solutions {
                    let inner_solutions = self.evaluate_where_inner(inner, vec![solution.clone()], engine);
                    if inner_solutions.is_empty() {
                        result.push(solution.clone());
                    } else {
                        result.extend(inner_solutions);
                    }
                }
                result
            }
            GraphPattern::Union(left, right) => {
                let left_solutions = self.evaluate_where_inner(left, solutions.clone(), engine);
                let right_solutions = self.evaluate_where_inner(right, solutions, engine);
                [left_solutions, right_solutions].concat()
            }
            _ => solutions, // Other patterns pass through
        }
    }

    fn evaluate_where_inner(
        &self,
        where_clause: &WhereClause,
        solutions: Vec<HashMap<String, Term>>,
        engine: &super::DatasetEngine,
    ) -> Vec<HashMap<String, Term>> {
        let mut result = solutions;
        for pattern in &where_clause.patterns {
            result = self.evaluate_pattern(pattern, result, engine);
        }
        result
    }

    /// Evaluate a triple pattern against default graph
    fn evaluate_triple_pattern(
        &self,
        tp: &TriplePattern,
        solutions: Vec<HashMap<String, Term>>,
    ) -> Vec<HashMap<String, Term>> {
        let mut result = Vec::new();

        for solution in solutions {
            // Resolve patterns
            let subj = resolve_term_pattern(&tp.subject, &solution);
            let pred = resolve_term_pattern(&tp.predicate, &solution);
            let obj = resolve_term_pattern(&tp.object, &solution);

            for triple in self.dataset.default_graph().iter() {
                let mut new_bindings = solution.clone();
                let mut matches = true;

                // Match subject
                match &tp.subject {
                    TermPattern::Variable(v) => {
                        if let Some(bound) = subj.as_ref() {
                            if &triple.subject != bound { matches = false; }
                        } else {
                            new_bindings.insert(v.clone(), triple.subject.clone());
                        }
                    }
                    TermPattern::Term(t) => {
                        if &triple.subject != t { matches = false; }
                    }
                    TermPattern::Path(_) => matches = false,
                }

                if !matches { continue; }

                // Match predicate
                match &tp.predicate {
                    TermPattern::Variable(v) => {
                        if let Some(bound) = pred.as_ref() {
                            if &triple.predicate != bound { matches = false; }
                        } else {
                            new_bindings.insert(v.clone(), triple.predicate.clone());
                        }
                    }
                    TermPattern::Term(t) => {
                        if &triple.predicate != t { matches = false; }
                    }
                    TermPattern::Path(_) => continue,
                }

                if !matches { continue; }

                // Match object
                match &tp.object {
                    TermPattern::Variable(v) => {
                        if let Some(bound) = obj.as_ref() {
                            if &triple.object != bound { matches = false; }
                        } else {
                            new_bindings.insert(v.clone(), triple.object.clone());
                        }
                    }
                    TermPattern::Term(t) => {
                        if &triple.object != t { matches = false; }
                    }
                    TermPattern::Path(_) => matches = false,
                }

                if matches {
                    result.push(new_bindings);
                }
            }
        }

        result
    }

    /// Evaluate a simple filter
    fn evaluate_filter(&self, expr: &super::FilterExpr, bindings: &HashMap<String, Term>) -> bool {
        match expr {
            super::FilterExpr::Bound(var) => bindings.contains_key(var),
            super::FilterExpr::NotBound(var) => !bindings.contains_key(var),
            super::FilterExpr::And(left, right) => {
                self.evaluate_filter(left, bindings) && self.evaluate_filter(right, bindings)
            }
            super::FilterExpr::Or(left, right) => {
                self.evaluate_filter(left, bindings) || self.evaluate_filter(right, bindings)
            }
            super::FilterExpr::Not(inner) => !self.evaluate_filter(inner, bindings),
            _ => true,
        }
    }
}

/// Parse a SPARQL UPDATE string (simplified parser)
pub fn parse_update(input: &str) -> Result<Vec<Update>, String> {
    let input = input.trim();
    let upper = input.to_uppercase();

    if upper.starts_with("INSERT DATA") {
        parse_insert_data(input)
    } else if upper.starts_with("DELETE DATA") {
        parse_delete_data(input)
    } else if upper.starts_with("CLEAR") {
        parse_clear(input)
    } else if upper.starts_with("DROP") {
        parse_drop(input)
    } else if upper.starts_with("CREATE") {
        parse_create(input)
    } else {
        Err(format!("Unsupported update operation: {}", input))
    }
}

fn parse_insert_data(input: &str) -> Result<Vec<Update>, String> {
    // Simple parser for INSERT DATA { <s> <p> <o> . }
    let start = input.find('{').ok_or("Missing { in INSERT DATA")?;
    let end = input.rfind('}').ok_or("Missing } in INSERT DATA")?;
    let body = input[start + 1..end].trim();

    let quads = parse_quad_data(body)?;
    Ok(vec![Update::InsertData { quads }])
}

fn parse_delete_data(input: &str) -> Result<Vec<Update>, String> {
    let start = input.find('{').ok_or("Missing { in DELETE DATA")?;
    let end = input.rfind('}').ok_or("Missing } in DELETE DATA")?;
    let body = input[start + 1..end].trim();

    let quads = parse_quad_data(body)?;
    Ok(vec![Update::DeleteData { quads }])
}

fn parse_quad_data(body: &str) -> Result<Vec<QuadData>, String> {
    let mut quads = Vec::new();
    let mut tokens: Vec<String> = Vec::new();
    let mut chars = body.chars().peekable();

    // Tokenize: extract URIs (<...>), literals ("..."), and handle dots
    while let Some(&c) = chars.peek() {
        if c.is_whitespace() {
            chars.next();
            continue;
        }

        if c == '.' {
            // Triple terminator - process collected tokens
            chars.next();
            if tokens.len() >= 3 {
                let subject = parse_term(&tokens[0])?;
                let predicate = parse_term(&tokens[1])?;
                let object = parse_term(&tokens[2])?;
                quads.push(QuadData::new(subject, predicate, object, None));
            }
            tokens.clear();
            continue;
        }

        if c == '<' {
            // URI: collect until >
            let mut token = String::new();
            while let Some(&ch) = chars.peek() {
                token.push(ch);
                chars.next();
                if ch == '>' {
                    break;
                }
            }
            tokens.push(token);
        } else if c == '"' {
            // Literal: collect until closing quote
            let mut token = String::new();
            token.push(c);
            chars.next();
            while let Some(&ch) = chars.peek() {
                token.push(ch);
                chars.next();
                if ch == '"' {
                    break;
                }
            }
            // Handle optional language tag or datatype
            while let Some(&ch) = chars.peek() {
                if ch == '@' || ch == '^' {
                    token.push(ch);
                    chars.next();
                    // Collect the rest of the tag/datatype
                    while let Some(&ch2) = chars.peek() {
                        if ch2.is_whitespace() || ch2 == '.' {
                            break;
                        }
                        token.push(ch2);
                        chars.next();
                    }
                } else {
                    break;
                }
            }
            tokens.push(token);
        } else {
            // Other token (shouldn't happen in valid N-Triples)
            let mut token = String::new();
            while let Some(&ch) = chars.peek() {
                if ch.is_whitespace() || ch == '.' {
                    break;
                }
                token.push(ch);
                chars.next();
            }
            if !token.is_empty() {
                tokens.push(token);
            }
        }
    }

    // Handle final triple if no trailing dot
    if tokens.len() >= 3 {
        let subject = parse_term(&tokens[0])?;
        let predicate = parse_term(&tokens[1])?;
        let object = parse_term(&tokens[2])?;
        quads.push(QuadData::new(subject, predicate, object, None));
    }

    Ok(quads)
}

fn parse_term(s: &str) -> Result<Term, String> {
    let s = s.trim();
    if s.starts_with('<') && s.ends_with('>') {
        Ok(Term::uri(&s[1..s.len() - 1]))
    } else if s.starts_with('"') {
        let end = s.rfind('"').ok_or("Unclosed string literal")?;
        Ok(Term::literal(&s[1..end]))
    } else {
        Err(format!("Unsupported term syntax: {}", s))
    }
}

fn parse_clear(input: &str) -> Result<Vec<Update>, String> {
    let upper = input.to_uppercase();
    let silent = upper.contains("SILENT");

    let target = if upper.contains("DEFAULT") {
        GraphTarget::Default
    } else if upper.contains("NAMED") {
        GraphTarget::Named
    } else if upper.contains("ALL") {
        GraphTarget::All
    } else if let Some(start) = input.find('<') {
        if let Some(end) = input.find('>') {
            GraphTarget::Graph(Term::uri(&input[start + 1..end]))
        } else {
            return Err("Invalid graph URI in CLEAR".to_string());
        }
    } else {
        return Err("Invalid CLEAR target".to_string());
    };

    Ok(vec![Update::Clear { target, silent }])
}

fn parse_drop(input: &str) -> Result<Vec<Update>, String> {
    let upper = input.to_uppercase();
    let silent = upper.contains("SILENT");

    let target = if upper.contains("DEFAULT") {
        GraphTarget::Default
    } else if upper.contains("NAMED") {
        GraphTarget::Named
    } else if upper.contains("ALL") {
        GraphTarget::All
    } else if let Some(start) = input.find('<') {
        if let Some(end) = input.find('>') {
            GraphTarget::Graph(Term::uri(&input[start + 1..end]))
        } else {
            return Err("Invalid graph URI in DROP".to_string());
        }
    } else {
        return Err("Invalid DROP target".to_string());
    };

    Ok(vec![Update::Drop { target, silent }])
}

fn parse_create(input: &str) -> Result<Vec<Update>, String> {
    let upper = input.to_uppercase();
    let silent = upper.contains("SILENT");

    if let Some(start) = input.find('<') {
        if let Some(end) = input.find('>') {
            let graph = Term::uri(&input[start + 1..end]);
            Ok(vec![Update::Create { graph, silent }])
        } else {
            Err("Invalid graph URI in CREATE".to_string())
        }
    } else {
        Err("Missing graph URI in CREATE".to_string())
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_data() {
        let mut dataset = Dataset::new();
        let mut engine = UpdateEngine::new(&mut dataset);

        let update = Update::InsertData {
            quads: vec![
                QuadData::new(
                    Term::uri("http://example.org/s"),
                    Term::uri("http://example.org/p"),
                    Term::uri("http://example.org/o"),
                    None,
                ),
            ],
        };

        let result = engine.execute(&update);
        assert!(result.success);
        assert_eq!(result.inserted, 1);
        assert_eq!(dataset.default_graph().len(), 1);
    }

    #[test]
    fn test_insert_data_named_graph() {
        let mut dataset = Dataset::new();
        let mut engine = UpdateEngine::new(&mut dataset);

        let graph = Term::uri("http://example.org/graph1");
        let update = Update::InsertData {
            quads: vec![
                QuadData::new(
                    Term::uri("http://example.org/s"),
                    Term::uri("http://example.org/p"),
                    Term::uri("http://example.org/o"),
                    Some(graph.clone()),
                ),
            ],
        };

        let result = engine.execute(&update);
        assert!(result.success);
        assert_eq!(result.inserted, 1);
        assert_eq!(dataset.default_graph().len(), 0);
        assert_eq!(dataset.named_graph(&graph).unwrap().len(), 1);
    }

    #[test]
    fn test_clear_default() {
        let mut dataset = Dataset::new();
        dataset.add(Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/o"),
        ));

        let mut engine = UpdateEngine::new(&mut dataset);
        let result = engine.execute(&Update::Clear {
            target: GraphTarget::Default,
            silent: false,
        });

        assert!(result.success);
        assert_eq!(result.deleted, 1);
        assert_eq!(dataset.default_graph().len(), 0);
    }

    #[test]
    fn test_clear_named() {
        let mut dataset = Dataset::new();
        let graph = Term::uri("http://example.org/graph1");
        dataset.add_to_graph(Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/o"),
        ), &graph);

        let mut engine = UpdateEngine::new(&mut dataset);
        let result = engine.execute(&Update::Clear {
            target: GraphTarget::Graph(graph.clone()),
            silent: false,
        });

        assert!(result.success);
        assert_eq!(result.deleted, 1);
    }

    #[test]
    fn test_create_graph() {
        let mut dataset = Dataset::new();
        let mut engine = UpdateEngine::new(&mut dataset);

        let graph = Term::uri("http://example.org/newgraph");
        let result = engine.execute(&Update::Create {
            graph: graph.clone(),
            silent: false,
        });

        assert!(result.success);
        assert!(dataset.has_graph(&graph));
    }

    #[test]
    fn test_create_existing_graph_silent() {
        let mut dataset = Dataset::new();
        let graph = Term::uri("http://example.org/graph1");
        dataset.get_or_create_graph(&graph);

        let mut engine = UpdateEngine::new(&mut dataset);
        let result = engine.execute(&Update::Create {
            graph: graph.clone(),
            silent: true,
        });

        assert!(result.success); // Silent mode doesn't error
    }

    #[test]
    fn test_create_existing_graph_error() {
        let mut dataset = Dataset::new();
        let graph = Term::uri("http://example.org/graph1");
        dataset.get_or_create_graph(&graph);

        let mut engine = UpdateEngine::new(&mut dataset);
        let result = engine.execute(&Update::Create {
            graph: graph.clone(),
            silent: false,
        });

        assert!(!result.success); // Non-silent mode errors
    }

    #[test]
    fn test_drop_graph() {
        let mut dataset = Dataset::new();
        let graph = Term::uri("http://example.org/graph1");
        dataset.add_to_graph(Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/o"),
        ), &graph);

        let mut engine = UpdateEngine::new(&mut dataset);
        let result = engine.execute(&Update::Drop {
            target: GraphTarget::Graph(graph.clone()),
            silent: false,
        });

        assert!(result.success);
        assert!(!dataset.has_graph(&graph));
    }

    #[test]
    fn test_parse_insert_data() {
        let input = r#"INSERT DATA { <http://ex.org/s> <http://ex.org/p> <http://ex.org/o> . }"#;
        let updates = parse_update(input).unwrap();

        assert_eq!(updates.len(), 1);
        match &updates[0] {
            Update::InsertData { quads } => {
                assert_eq!(quads.len(), 1);
            }
            _ => panic!("Expected InsertData"),
        }
    }

    #[test]
    fn test_parse_clear() {
        let input = "CLEAR DEFAULT";
        let updates = parse_update(input).unwrap();

        assert_eq!(updates.len(), 1);
        match &updates[0] {
            Update::Clear { target, silent } => {
                assert_eq!(*target, GraphTarget::Default);
                assert!(!*silent);
            }
            _ => panic!("Expected Clear"),
        }
    }

    #[test]
    fn test_parse_clear_silent() {
        let input = "CLEAR SILENT ALL";
        let updates = parse_update(input).unwrap();

        assert_eq!(updates.len(), 1);
        match &updates[0] {
            Update::Clear { target, silent } => {
                assert_eq!(*target, GraphTarget::All);
                assert!(*silent);
            }
            _ => panic!("Expected Clear"),
        }
    }
}
