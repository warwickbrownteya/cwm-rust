//! SPARQL Dataset Engine
//!
//! Extends the SPARQL engine with proper named graph support.
//! This engine evaluates GRAPH patterns against an RDF Dataset.
//!
//! # Example
//!
//! ```ignore
//! use cwm::sparql::DatasetEngine;
//! use cwm::store::Dataset;
//!
//! let dataset = Dataset::new();
//! let engine = DatasetEngine::new(&dataset);
//! let result = engine.execute_query("SELECT * WHERE { GRAPH ?g { ?s ?p ?o } }");
//! ```

use std::collections::HashMap;

use crate::term::Term;
use crate::store::Dataset;
use super::{
    Query, QueryResult, GraphPattern, TermPattern, WhereClause, PropertyPath,
    SparqlParser, AggregateFunction, Projection, FilterExpr, OrderCondition,
};

/// SPARQL engine that evaluates queries against an RDF Dataset
pub struct DatasetEngine<'a> {
    dataset: &'a Dataset,
    /// Current graph context (None = default graph)
    current_graph: Option<Term>,
}

impl<'a> DatasetEngine<'a> {
    /// Create a new dataset engine
    pub fn new(dataset: &'a Dataset) -> Self {
        DatasetEngine {
            dataset,
            current_graph: None,
        }
    }

    /// Execute a SPARQL query string
    pub fn execute_query(&self, query_str: &str) -> Result<QueryResult, String> {
        let mut parser = SparqlParser::new(query_str);
        let query = parser.parse()?;
        Ok(self.execute(&query))
    }

    /// Execute a parsed SPARQL query
    pub fn execute(&self, query: &Query) -> QueryResult {
        match query {
            Query::Select { variables, projections, distinct, where_clause, group_by, having, order_by, limit, offset } => {
                let mut solutions = self.evaluate_where(where_clause);

                // Apply GROUP BY and aggregates
                if group_by.is_some() || projections.iter().any(|p| matches!(p, Projection::Aggregate { .. })) {
                    solutions = self.apply_grouping_and_aggregates(&solutions, group_by.as_ref(), projections, having.as_ref());
                }

                // Apply ORDER BY
                if let Some(order) = order_by {
                    self.apply_order(&mut solutions, order);
                }

                // Apply OFFSET
                if let Some(off) = offset {
                    if *off < solutions.len() {
                        solutions = solutions[*off..].to_vec();
                    } else {
                        solutions.clear();
                    }
                }

                // Apply LIMIT
                if let Some(lim) = limit {
                    solutions.truncate(*lim);
                }

                // Apply DISTINCT
                if *distinct {
                    let mut seen = std::collections::HashSet::new();
                    solutions.retain(|sol| {
                        let key = format!("{:?}", sol);
                        seen.insert(key)
                    });
                }

                QueryResult::Bindings {
                    variables: variables.clone(),
                    solutions,
                }
            }
            Query::Ask { where_clause } => {
                let solutions = self.evaluate_where(where_clause);
                QueryResult::Boolean(!solutions.is_empty())
            }
            Query::Construct { template, where_clause } => {
                let solutions = self.evaluate_where(where_clause);
                let mut triples = Vec::new();

                for solution in &solutions {
                    for pattern in template {
                        if let (Some(s), Some(p), Some(o)) = (
                            self.resolve_term_pattern(&pattern.subject, solution),
                            self.resolve_predicate_pattern(&pattern.predicate, solution),
                            self.resolve_term_pattern(&pattern.object, solution),
                        ) {
                            triples.push(crate::term::Triple::new(s, p, o));
                        }
                    }
                }

                QueryResult::Graph(triples)
            }
            Query::Describe { resources } => {
                // Simple DESCRIBE: return all triples about the resources
                let mut triples = Vec::new();
                for resource in resources {
                    // Search default graph
                    for triple in self.dataset.default_graph().iter() {
                        if &triple.subject == resource || &triple.object == resource {
                            triples.push(triple.clone());
                        }
                    }
                    // Search named graphs
                    for name in self.dataset.graph_names() {
                        if let Some(graph) = self.dataset.named_graph(&name) {
                            for triple in graph.iter() {
                                if &triple.subject == resource || &triple.object == resource {
                                    triples.push(triple.clone());
                                }
                            }
                        }
                    }
                }
                QueryResult::Graph(triples)
            }
        }
    }

    /// Evaluate a WHERE clause
    fn evaluate_where(&self, where_clause: &WhereClause) -> Vec<HashMap<String, Term>> {
        let initial = vec![HashMap::new()];
        self.evaluate_where_with_solutions(where_clause, initial)
    }

    /// Evaluate WHERE clause with existing solutions
    fn evaluate_where_with_solutions(
        &self,
        where_clause: &WhereClause,
        solutions: Vec<HashMap<String, Term>>,
    ) -> Vec<HashMap<String, Term>> {
        let mut result = solutions;

        for pattern in &where_clause.patterns {
            result = self.evaluate_pattern(pattern, result);
        }

        result
    }

    /// Evaluate a single graph pattern
    fn evaluate_pattern(
        &self,
        pattern: &GraphPattern,
        solutions: Vec<HashMap<String, Term>>,
    ) -> Vec<HashMap<String, Term>> {
        match pattern {
            GraphPattern::Triple(tp) => {
                self.evaluate_triple_pattern(tp, solutions)
            }

            GraphPattern::Optional(inner) => {
                let mut result = Vec::new();
                for solution in &solutions {
                    let inner_solutions = self.evaluate_where_with_solutions(inner, vec![solution.clone()]);
                    if inner_solutions.is_empty() {
                        result.push(solution.clone());
                    } else {
                        result.extend(inner_solutions);
                    }
                }
                result
            }

            GraphPattern::Union(left, right) => {
                let left_solutions = self.evaluate_where_with_solutions(left, solutions.clone());
                let right_solutions = self.evaluate_where_with_solutions(right, solutions);
                [left_solutions, right_solutions].concat()
            }

            GraphPattern::Filter(expr) => {
                solutions.into_iter()
                    .filter(|sol| self.evaluate_filter(expr, sol))
                    .collect()
            }

            GraphPattern::Bind(expr, var) => {
                solutions.into_iter()
                    .filter_map(|mut sol| {
                        if let Some(value) = self.evaluate_expression(expr, &sol) {
                            sol.insert(var.clone(), value);
                            Some(sol)
                        } else {
                            Some(sol) // BIND with unbound expression keeps solution
                        }
                    })
                    .collect()
            }

            GraphPattern::Group(inner) => {
                self.evaluate_where_with_solutions(inner, solutions)
            }

            GraphPattern::Graph(graph_term, graph_clause) => {
                self.evaluate_graph_pattern(graph_term, graph_clause, solutions)
            }

            GraphPattern::Subquery(subquery) => {
                self.evaluate_subquery(subquery, solutions)
            }

            GraphPattern::Values(vars, data) => {
                self.evaluate_values(vars, data, solutions)
            }

            GraphPattern::Minus(inner) => {
                let inner_solutions = self.evaluate_where_with_solutions(inner, solutions.clone());
                solutions.into_iter()
                    .filter(|sol| !inner_solutions.iter().any(|inner_sol| {
                        sol.iter().all(|(k, v)| inner_sol.get(k).map(|iv| iv == v).unwrap_or(true))
                    }))
                    .collect()
            }

            GraphPattern::Service(endpoint, service_clause, silent) => {
                // SERVICE - would need HTTP client, for now return input
                if *silent {
                    solutions
                } else {
                    // In non-silent mode, an unavailable service should fail
                    // For now, just pass through
                    solutions
                }
            }
        }
    }

    /// Evaluate GRAPH pattern with proper named graph support
    fn evaluate_graph_pattern(
        &self,
        graph_term: &TermPattern,
        graph_clause: &WhereClause,
        solutions: Vec<HashMap<String, Term>>,
    ) -> Vec<HashMap<String, Term>> {
        let mut result = Vec::new();

        match graph_term {
            TermPattern::Term(graph_uri) => {
                // Fixed graph URI - evaluate against that specific named graph
                if let Some(graph_store) = self.dataset.named_graph(graph_uri) {
                    // Create a temporary engine for this graph
                    let graph_engine = GraphContextEngine {
                        store: graph_store,
                    };

                    for solution in solutions {
                        let inner_solutions = graph_engine.evaluate_where_with_solutions(graph_clause, vec![solution]);
                        result.extend(inner_solutions);
                    }
                }
                // If graph doesn't exist, no solutions
            }

            TermPattern::Variable(var) => {
                // Variable graph - enumerate all named graphs
                for graph_name in self.dataset.graph_names() {
                    if let Some(graph_store) = self.dataset.named_graph(&graph_name) {
                        let graph_engine = GraphContextEngine {
                            store: graph_store,
                        };

                        for solution in &solutions {
                            // Check if variable is already bound
                            if let Some(bound_graph) = solution.get(var) {
                                // Only evaluate if the bound value matches this graph
                                if bound_graph == &graph_name {
                                    let inner_solutions = graph_engine.evaluate_where_with_solutions(
                                        graph_clause,
                                        vec![solution.clone()],
                                    );
                                    result.extend(inner_solutions);
                                }
                            } else {
                                // Variable unbound - evaluate and bind
                                let inner_solutions = graph_engine.evaluate_where_with_solutions(
                                    graph_clause,
                                    vec![solution.clone()],
                                );
                                for mut inner_sol in inner_solutions {
                                    inner_sol.insert(var.clone(), graph_name.clone());
                                    result.push(inner_sol);
                                }
                            }
                        }
                    }
                }
            }

            TermPattern::Path(_) => {
                // Invalid - paths in GRAPH position
                result = solutions;
            }
        }

        result
    }

    /// Evaluate a triple pattern against current graph context
    fn evaluate_triple_pattern(
        &self,
        tp: &super::TriplePattern,
        solutions: Vec<HashMap<String, Term>>,
    ) -> Vec<HashMap<String, Term>> {
        let store = match &self.current_graph {
            None => self.dataset.default_graph(),
            Some(g) => match self.dataset.named_graph(g) {
                Some(s) => s,
                None => return Vec::new(),
            },
        };

        let mut result = Vec::new();

        for solution in solutions {
            // Handle property paths
            if let TermPattern::Path(path) = &tp.predicate {
                let path_solutions = self.evaluate_property_path(path, &tp.subject, &tp.object, vec![solution]);
                result.extend(path_solutions);
                continue;
            }

            // Resolve patterns with current bindings
            let subj = self.resolve_term_pattern(&tp.subject, &solution);
            let pred = self.resolve_predicate_pattern(&tp.predicate, &solution);
            let obj = self.resolve_term_pattern(&tp.object, &solution);

            // Match against store
            for triple in store.iter() {
                let mut new_bindings = solution.clone();
                let mut matches = true;

                // Match subject
                match &tp.subject {
                    TermPattern::Variable(v) => {
                        if let Some(bound) = subj.as_ref() {
                            if &triple.subject != bound {
                                matches = false;
                            }
                        } else {
                            new_bindings.insert(v.clone(), triple.subject.clone());
                        }
                    }
                    TermPattern::Term(t) => {
                        if &triple.subject != t {
                            matches = false;
                        }
                    }
                    TermPattern::Path(_) => matches = false,
                }

                if !matches { continue; }

                // Match predicate
                match &tp.predicate {
                    TermPattern::Variable(v) => {
                        if let Some(bound) = pred.as_ref() {
                            if &triple.predicate != bound {
                                matches = false;
                            }
                        } else {
                            new_bindings.insert(v.clone(), triple.predicate.clone());
                        }
                    }
                    TermPattern::Term(t) => {
                        if &triple.predicate != t {
                            matches = false;
                        }
                    }
                    TermPattern::Path(_) => {
                        // Already handled above
                        continue;
                    }
                }

                if !matches { continue; }

                // Match object
                match &tp.object {
                    TermPattern::Variable(v) => {
                        if let Some(bound) = obj.as_ref() {
                            if &triple.object != bound {
                                matches = false;
                            }
                        } else {
                            new_bindings.insert(v.clone(), triple.object.clone());
                        }
                    }
                    TermPattern::Term(t) => {
                        if &triple.object != t {
                            matches = false;
                        }
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

    /// Resolve a term pattern with current bindings
    fn resolve_term_pattern(&self, pattern: &TermPattern, bindings: &HashMap<String, Term>) -> Option<Term> {
        match pattern {
            TermPattern::Term(t) => Some(t.clone()),
            TermPattern::Variable(v) => bindings.get(v).cloned(),
            TermPattern::Path(_) => None,
        }
    }

    /// Resolve a predicate pattern (term only, not path)
    fn resolve_predicate_pattern(&self, pattern: &TermPattern, bindings: &HashMap<String, Term>) -> Option<Term> {
        match pattern {
            TermPattern::Term(t) => Some(t.clone()),
            TermPattern::Variable(v) => bindings.get(v).cloned(),
            TermPattern::Path(_) => None,
        }
    }

    /// Evaluate a property path
    fn evaluate_property_path(
        &self,
        path: &PropertyPath,
        subject: &TermPattern,
        object: &TermPattern,
        solutions: Vec<HashMap<String, Term>>,
    ) -> Vec<HashMap<String, Term>> {
        let store = match &self.current_graph {
            None => self.dataset.default_graph(),
            Some(g) => match self.dataset.named_graph(g) {
                Some(s) => s,
                None => return Vec::new(),
            },
        };

        let mut path_eval = super::path::PathEvaluator::new(store);
        let mut result = Vec::new();

        for solution in solutions {
            let subj_bound = match subject {
                TermPattern::Variable(v) => solution.get(v).cloned(),
                TermPattern::Term(t) => Some(t.clone()),
                TermPattern::Path(_) => None,
            };

            let obj_bound = match object {
                TermPattern::Variable(v) => solution.get(v).cloned(),
                TermPattern::Term(t) => Some(t.clone()),
                TermPattern::Path(_) => None,
            };

            let pairs = path_eval.evaluate(path, subj_bound.as_ref(), obj_bound.as_ref());

            for (s, o) in pairs {
                let mut new_bindings = solution.clone();
                let mut valid = true;

                if let TermPattern::Variable(v) = subject {
                    if let Some(existing) = new_bindings.get(v) {
                        if existing != &s { valid = false; }
                    } else {
                        new_bindings.insert(v.clone(), s.clone());
                    }
                }

                if let TermPattern::Variable(v) = object {
                    if let Some(existing) = new_bindings.get(v) {
                        if existing != &o { valid = false; }
                    } else {
                        new_bindings.insert(v.clone(), o);
                    }
                }

                if valid {
                    result.push(new_bindings);
                }
            }
        }

        result
    }

    /// Evaluate a filter expression
    fn evaluate_filter(&self, expr: &FilterExpr, bindings: &HashMap<String, Term>) -> bool {
        // Delegate to main engine's filter evaluation
        // For now, implement basic cases
        match expr {
            FilterExpr::Bound(var) => bindings.contains_key(var),
            FilterExpr::NotBound(var) => !bindings.contains_key(var),
            FilterExpr::Equals(left, right) => {
                let l = self.evaluate_expression(left, bindings);
                let r = self.evaluate_expression(right, bindings);
                l == r
            }
            FilterExpr::NotEquals(left, right) => {
                let l = self.evaluate_expression(left, bindings);
                let r = self.evaluate_expression(right, bindings);
                l != r
            }
            FilterExpr::And(left, right) => {
                self.evaluate_filter(left, bindings) && self.evaluate_filter(right, bindings)
            }
            FilterExpr::Or(left, right) => {
                self.evaluate_filter(left, bindings) || self.evaluate_filter(right, bindings)
            }
            FilterExpr::Not(inner) => !self.evaluate_filter(inner, bindings),
            _ => true, // Other filters pass by default
        }
    }

    /// Evaluate an expression
    fn evaluate_expression(&self, expr: &FilterExpr, bindings: &HashMap<String, Term>) -> Option<Term> {
        match expr {
            FilterExpr::Var(var) => bindings.get(var).cloned(),
            FilterExpr::Literal(t) => Some(t.clone()),
            _ => None,
        }
    }

    /// Evaluate a subquery
    fn evaluate_subquery(
        &self,
        subquery: &Query,
        solutions: Vec<HashMap<String, Term>>,
    ) -> Vec<HashMap<String, Term>> {
        let subquery_result = self.execute(subquery);

        let subquery_bindings = match subquery_result {
            QueryResult::Bindings { solutions: sub_sols, .. } => sub_sols,
            QueryResult::Boolean(true) => vec![HashMap::new()],
            QueryResult::Boolean(false) => vec![],
            QueryResult::Graph(_) => vec![],
        };

        let mut result = Vec::new();
        for outer_sol in &solutions {
            for inner_sol in &subquery_bindings {
                let mut combined = outer_sol.clone();
                let mut compatible = true;

                for (k, v) in inner_sol {
                    if let Some(existing) = combined.get(k) {
                        if existing != v {
                            compatible = false;
                            break;
                        }
                    } else {
                        combined.insert(k.clone(), v.clone());
                    }
                }

                if compatible {
                    result.push(combined);
                }
            }
        }

        result
    }

    /// Evaluate VALUES clause
    fn evaluate_values(
        &self,
        vars: &[String],
        data: &[Vec<Option<Term>>],
        solutions: Vec<HashMap<String, Term>>,
    ) -> Vec<HashMap<String, Term>> {
        let mut result = Vec::new();

        for solution in solutions {
            for row in data {
                let mut new_sol = solution.clone();
                let mut compatible = true;

                for (i, var) in vars.iter().enumerate() {
                    if let Some(Some(value)) = row.get(i) {
                        if let Some(existing) = new_sol.get(var) {
                            if existing != value {
                                compatible = false;
                                break;
                            }
                        } else {
                            new_sol.insert(var.clone(), value.clone());
                        }
                    }
                }

                if compatible {
                    result.push(new_sol);
                }
            }
        }

        result
    }

    /// Apply ORDER BY
    fn apply_order(&self, solutions: &mut [HashMap<String, Term>], order: &[OrderCondition]) {
        solutions.sort_by(|a, b| {
            for cond in order {
                let a_val = a.get(&cond.variable);
                let b_val = b.get(&cond.variable);

                let cmp = match (a_val, b_val) {
                    (None, None) => std::cmp::Ordering::Equal,
                    (None, Some(_)) => std::cmp::Ordering::Greater,
                    (Some(_), None) => std::cmp::Ordering::Less,
                    (Some(av), Some(bv)) => format!("{:?}", av).cmp(&format!("{:?}", bv)),
                };

                if cmp != std::cmp::Ordering::Equal {
                    return if cond.ascending { cmp } else { cmp.reverse() };
                }
            }
            std::cmp::Ordering::Equal
        });
    }

    /// Apply GROUP BY and aggregates (simplified)
    fn apply_grouping_and_aggregates(
        &self,
        solutions: &[HashMap<String, Term>],
        group_by: Option<&Vec<String>>,
        projections: &[Projection],
        _having: Option<&FilterExpr>,
    ) -> Vec<HashMap<String, Term>> {
        // Group solutions
        let mut groups: HashMap<String, Vec<&HashMap<String, Term>>> = HashMap::new();

        for sol in solutions {
            let key = match group_by {
                Some(vars) => {
                    vars.iter()
                        .map(|v| sol.get(v).map(|t| format!("{:?}", t)).unwrap_or_default())
                        .collect::<Vec<_>>()
                        .join("|")
                }
                None => String::new(),
            };
            groups.entry(key).or_default().push(sol);
        }

        // Apply aggregates to each group
        let mut result = Vec::new();

        for (_key, group_sols) in groups {
            let mut row = HashMap::new();

            // Copy group-by variables from first solution
            if let Some(first) = group_sols.first() {
                if let Some(vars) = group_by {
                    for var in vars {
                        if let Some(val) = first.get(var) {
                            row.insert(var.clone(), val.clone());
                        }
                    }
                }
            }

            // Compute aggregates
            for proj in projections {
                if let Projection::Aggregate { function, variable, alias, distinct: _ } = proj {
                    let alias_name = alias.as_ref()
                        .or(variable.as_ref())
                        .cloned()
                        .unwrap_or_else(|| "agg".to_string());

                    let values: Vec<_> = group_sols.iter()
                        .filter_map(|sol| variable.as_ref().and_then(|v| sol.get(v)))
                        .collect();

                    let agg_value = match function {
                        AggregateFunction::Count => {
                            Term::typed_literal(
                                &values.len().to_string(),
                                "http://www.w3.org/2001/XMLSchema#integer",
                            )
                        }
                        AggregateFunction::Sum => {
                            let sum: f64 = values.iter()
                                .filter_map(|t| self.term_to_number(t))
                                .sum();
                            Term::typed_literal(&sum.to_string(), "http://www.w3.org/2001/XMLSchema#decimal")
                        }
                        AggregateFunction::Avg => {
                            let nums: Vec<f64> = values.iter()
                                .filter_map(|t| self.term_to_number(t))
                                .collect();
                            let avg = if nums.is_empty() { 0.0 } else { nums.iter().sum::<f64>() / nums.len() as f64 };
                            Term::typed_literal(&avg.to_string(), "http://www.w3.org/2001/XMLSchema#decimal")
                        }
                        AggregateFunction::Min => {
                            values.iter()
                                .min_by(|a, b| format!("{:?}", a).cmp(&format!("{:?}", b)))
                                .map(|t| (*t).clone())
                                .unwrap_or_else(|| Term::literal(""))
                        }
                        AggregateFunction::Max => {
                            values.iter()
                                .max_by(|a, b| format!("{:?}", a).cmp(&format!("{:?}", b)))
                                .map(|t| (*t).clone())
                                .unwrap_or_else(|| Term::literal(""))
                        }
                        AggregateFunction::Sample => {
                            values.first().map(|t| (*t).clone()).unwrap_or_else(|| Term::literal(""))
                        }
                        AggregateFunction::GroupConcat { separator } => {
                            let sep = separator.as_deref().unwrap_or(" ");
                            let concat: String = values.iter()
                                .map(|t| format!("{}", t))
                                .collect::<Vec<_>>()
                                .join(sep);
                            Term::literal(&concat)
                        }
                    };

                    row.insert(alias_name, agg_value);
                }
            }

            result.push(row);
        }

        result
    }

    /// Convert a term to a number
    fn term_to_number(&self, term: &Term) -> Option<f64> {
        match term {
            Term::Literal(lit) => lit.value().parse().ok(),
            _ => None,
        }
    }
}

/// Helper engine for evaluating patterns in a specific graph context
struct GraphContextEngine<'a> {
    store: &'a crate::Store,
}

impl<'a> GraphContextEngine<'a> {
    fn evaluate_where_with_solutions(
        &self,
        where_clause: &WhereClause,
        solutions: Vec<HashMap<String, Term>>,
    ) -> Vec<HashMap<String, Term>> {
        // Use the standard SparqlEngine for graph-local evaluation
        let engine = super::SparqlEngine::new(self.store);
        engine.evaluate_where_with_solutions(where_clause, solutions)
    }
}

/// Execute a SPARQL query against a dataset
pub fn execute_sparql_dataset(dataset: &Dataset, query_str: &str) -> Result<QueryResult, String> {
    let engine = DatasetEngine::new(dataset);
    engine.execute_query(query_str)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::term::Triple;

    fn create_test_dataset() -> Dataset {
        let mut dataset = Dataset::new();

        // Add to default graph
        dataset.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://xmlns.com/foaf/0.1/name"),
            Term::literal("Alice"),
        ));
        dataset.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://xmlns.com/foaf/0.1/knows"),
            Term::uri("http://example.org/bob"),
        ));

        // Add to named graph 1
        let graph1 = Term::uri("http://example.org/graph1");
        dataset.add_to_graph(Triple::new(
            Term::uri("http://example.org/bob"),
            Term::uri("http://xmlns.com/foaf/0.1/name"),
            Term::literal("Bob"),
        ), &graph1);
        dataset.add_to_graph(Triple::new(
            Term::uri("http://example.org/bob"),
            Term::uri("http://xmlns.com/foaf/0.1/age"),
            Term::typed_literal("30", "http://www.w3.org/2001/XMLSchema#integer"),
        ), &graph1);

        // Add to named graph 2
        let graph2 = Term::uri("http://example.org/graph2");
        dataset.add_to_graph(Triple::new(
            Term::uri("http://example.org/carol"),
            Term::uri("http://xmlns.com/foaf/0.1/name"),
            Term::literal("Carol"),
        ), &graph2);

        dataset
    }

    #[test]
    fn test_default_graph_query() {
        let dataset = create_test_dataset();
        let engine = DatasetEngine::new(&dataset);

        let result = engine.execute_query("SELECT ?s ?o WHERE { ?s <http://xmlns.com/foaf/0.1/knows> ?o }").unwrap();

        match result {
            QueryResult::Bindings { solutions, .. } => {
                assert_eq!(solutions.len(), 1);
                assert_eq!(solutions[0].get("o").unwrap(), &Term::uri("http://example.org/bob"));
            }
            _ => panic!("Expected bindings result"),
        }
    }

    #[test]
    fn test_named_graph_query() {
        let dataset = create_test_dataset();
        let engine = DatasetEngine::new(&dataset);

        let result = engine.execute_query(
            "SELECT ?name WHERE { GRAPH <http://example.org/graph1> { ?s <http://xmlns.com/foaf/0.1/name> ?name } }"
        ).unwrap();

        match result {
            QueryResult::Bindings { solutions, .. } => {
                assert_eq!(solutions.len(), 1);
                assert_eq!(solutions[0].get("name").unwrap(), &Term::literal("Bob"));
            }
            _ => panic!("Expected bindings result"),
        }
    }

    #[test]
    fn test_graph_variable_query() {
        let dataset = create_test_dataset();
        let engine = DatasetEngine::new(&dataset);

        let result = engine.execute_query(
            "SELECT ?g ?name WHERE { GRAPH ?g { ?s <http://xmlns.com/foaf/0.1/name> ?name } }"
        ).unwrap();

        match result {
            QueryResult::Bindings { solutions, .. } => {
                // Should find Bob in graph1 and Carol in graph2
                assert_eq!(solutions.len(), 2);
            }
            _ => panic!("Expected bindings result"),
        }
    }

    #[test]
    fn test_ask_query() {
        let dataset = create_test_dataset();
        let engine = DatasetEngine::new(&dataset);

        let result = engine.execute_query(
            "ASK { <http://example.org/alice> <http://xmlns.com/foaf/0.1/knows> ?o }"
        ).unwrap();

        assert_eq!(result, QueryResult::Boolean(true));

        let result2 = engine.execute_query(
            "ASK { <http://example.org/alice> <http://example.org/doesNotExist> ?o }"
        ).unwrap();

        assert_eq!(result2, QueryResult::Boolean(false));
    }

    #[test]
    fn test_nonexistent_graph() {
        let dataset = create_test_dataset();
        let engine = DatasetEngine::new(&dataset);

        let result = engine.execute_query(
            "SELECT ?s WHERE { GRAPH <http://example.org/nonexistent> { ?s ?p ?o } }"
        ).unwrap();

        match result {
            QueryResult::Bindings { solutions, .. } => {
                assert_eq!(solutions.len(), 0);
            }
            _ => panic!("Expected bindings result"),
        }
    }
}
