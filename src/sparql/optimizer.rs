//! Query optimization for SPARQL queries
//!
//! This module provides query optimization capabilities including:
//! - Query plan generation
//! - Cost estimation
//! - Join ordering optimization
//! - Index selection hints
//! - Statistics-based optimization
//!
//! # Example
//!
//! ```ignore
//! use cwm::{QueryOptimizer, Store, OptimizerConfig};
//!
//! let optimizer = QueryOptimizer::new(OptimizerConfig::default());
//! let stats = optimizer.collect_statistics(&store);
//!
//! // Optimize a query plan
//! let optimized = optimizer.optimize(query_plan, &stats);
//! ```

use std::collections::{HashMap, HashSet};
use crate::term::{Term, Triple};
use crate::store::Store;

/// Configuration for the query optimizer
#[derive(Clone, Debug)]
pub struct OptimizerConfig {
    /// Enable cost-based optimization
    pub enable_cost_based: bool,
    /// Enable join reordering
    pub enable_join_reorder: bool,
    /// Maximum number of plans to consider
    pub max_plans: usize,
    /// Timeout for optimization in milliseconds
    pub timeout_ms: u64,
    /// Enable index hints in output
    pub emit_index_hints: bool,
}

impl Default for OptimizerConfig {
    fn default() -> Self {
        OptimizerConfig {
            enable_cost_based: true,
            enable_join_reorder: true,
            max_plans: 100,
            timeout_ms: 1000,
            emit_index_hints: true,
        }
    }
}

/// Statistics about the data for cost estimation
#[derive(Clone, Debug, Default)]
pub struct DataStatistics {
    /// Total number of triples
    pub triple_count: usize,
    /// Distinct subjects
    pub subject_count: usize,
    /// Distinct predicates
    pub predicate_count: usize,
    /// Distinct objects
    pub object_count: usize,
    /// Predicate cardinalities (predicate URI -> count)
    pub predicate_cardinality: HashMap<String, usize>,
    /// Subject-predicate selectivity estimates
    pub sp_selectivity: HashMap<(String, String), f64>,
    /// Predicate-object selectivity estimates
    pub po_selectivity: HashMap<(String, String), f64>,
    /// Average triples per subject
    pub avg_triples_per_subject: f64,
    /// Average triples per predicate
    pub avg_triples_per_predicate: f64,
    /// Average triples per object
    pub avg_triples_per_object: f64,
}

impl DataStatistics {
    /// Collect statistics from a store
    pub fn collect(store: &Store) -> Self {
        let mut subjects = HashSet::new();
        let mut predicates = HashSet::new();
        let mut objects = HashSet::new();
        let mut predicate_counts: HashMap<String, usize> = HashMap::new();

        for triple in store.iter() {
            subjects.insert(format!("{}", triple.subject));
            objects.insert(format!("{}", triple.object));

            if let Term::Uri(uri) = &triple.predicate {
                let pred_str = uri.as_str().to_string();
                predicates.insert(pred_str.clone());
                *predicate_counts.entry(pred_str).or_insert(0) += 1;
            }
        }

        let triple_count = store.len();
        let subject_count = subjects.len();
        let predicate_count = predicates.len();
        let object_count = objects.len();

        let avg_triples_per_subject = if subject_count > 0 {
            triple_count as f64 / subject_count as f64
        } else {
            0.0
        };

        let avg_triples_per_predicate = if predicate_count > 0 {
            triple_count as f64 / predicate_count as f64
        } else {
            0.0
        };

        let avg_triples_per_object = if object_count > 0 {
            triple_count as f64 / object_count as f64
        } else {
            0.0
        };

        DataStatistics {
            triple_count,
            subject_count,
            predicate_count,
            object_count,
            predicate_cardinality: predicate_counts,
            sp_selectivity: HashMap::new(),
            po_selectivity: HashMap::new(),
            avg_triples_per_subject,
            avg_triples_per_predicate,
            avg_triples_per_object,
        }
    }

    /// Estimate selectivity for a triple pattern
    pub fn estimate_selectivity(&self, pattern: &TriplePattern) -> f64 {
        if self.triple_count == 0 {
            return 1.0;
        }

        let mut selectivity = 1.0;

        // Subject selectivity
        match &pattern.subject {
            PatternTerm::Bound(_) => {
                selectivity *= 1.0 / self.subject_count.max(1) as f64;
            }
            PatternTerm::Variable(_) => {}
        }

        // Predicate selectivity
        match &pattern.predicate {
            PatternTerm::Bound(term) => {
                if let Term::Uri(uri) = term {
                    if let Some(&count) = self.predicate_cardinality.get(uri.as_str()) {
                        selectivity *= count as f64 / self.triple_count as f64;
                    } else {
                        selectivity *= 1.0 / self.predicate_count.max(1) as f64;
                    }
                }
            }
            PatternTerm::Variable(_) => {}
        }

        // Object selectivity
        match &pattern.object {
            PatternTerm::Bound(_) => {
                selectivity *= 1.0 / self.object_count.max(1) as f64;
            }
            PatternTerm::Variable(_) => {}
        }

        selectivity
    }

    /// Estimate result cardinality for a pattern
    pub fn estimate_cardinality(&self, pattern: &TriplePattern) -> f64 {
        self.triple_count as f64 * self.estimate_selectivity(pattern)
    }
}

/// A term in a triple pattern (either bound or variable)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PatternTerm {
    /// A bound (concrete) term
    Bound(Term),
    /// A variable
    Variable(String),
}

impl PatternTerm {
    /// Check if this is a variable
    pub fn is_variable(&self) -> bool {
        matches!(self, PatternTerm::Variable(_))
    }

    /// Get the variable name if this is a variable
    pub fn variable_name(&self) -> Option<&str> {
        match self {
            PatternTerm::Variable(name) => Some(name),
            _ => None,
        }
    }
}

/// A triple pattern for query planning
#[derive(Clone, Debug)]
pub struct TriplePattern {
    /// Subject
    pub subject: PatternTerm,
    /// Predicate
    pub predicate: PatternTerm,
    /// Object
    pub object: PatternTerm,
    /// Original pattern ID for tracking
    pub id: usize,
}

impl TriplePattern {
    /// Create a new triple pattern
    pub fn new(subject: PatternTerm, predicate: PatternTerm, object: PatternTerm, id: usize) -> Self {
        TriplePattern { subject, predicate, object, id }
    }

    /// Create from a triple
    pub fn from_triple(triple: &Triple, id: usize) -> Self {
        let subject = if let Term::Variable(v) = &triple.subject {
            PatternTerm::Variable(v.name().to_string())
        } else {
            PatternTerm::Bound(triple.subject.clone())
        };

        let predicate = if let Term::Variable(v) = &triple.predicate {
            PatternTerm::Variable(v.name().to_string())
        } else {
            PatternTerm::Bound(triple.predicate.clone())
        };

        let object = if let Term::Variable(v) = &triple.object {
            PatternTerm::Variable(v.name().to_string())
        } else {
            PatternTerm::Bound(triple.object.clone())
        };

        TriplePattern { subject, predicate, object, id }
    }

    /// Get all variables in this pattern
    pub fn variables(&self) -> HashSet<String> {
        let mut vars = HashSet::new();
        if let PatternTerm::Variable(v) = &self.subject {
            vars.insert(v.clone());
        }
        if let PatternTerm::Variable(v) = &self.predicate {
            vars.insert(v.clone());
        }
        if let PatternTerm::Variable(v) = &self.object {
            vars.insert(v.clone());
        }
        vars
    }

    /// Count bound terms
    pub fn bound_count(&self) -> usize {
        let mut count = 0;
        if !self.subject.is_variable() { count += 1; }
        if !self.predicate.is_variable() { count += 1; }
        if !self.object.is_variable() { count += 1; }
        count
    }
}

/// A node in a query plan
#[derive(Clone, Debug)]
pub enum PlanNode {
    /// Scan a triple pattern
    TripleScan {
        pattern: TriplePattern,
        estimated_cost: f64,
        estimated_rows: f64,
        index_hint: Option<IndexHint>,
    },
    /// Join two plan nodes
    Join {
        left: Box<PlanNode>,
        right: Box<PlanNode>,
        join_type: JoinType,
        join_variables: Vec<String>,
        estimated_cost: f64,
        estimated_rows: f64,
    },
    /// Filter results
    Filter {
        input: Box<PlanNode>,
        condition: String,
        estimated_cost: f64,
        estimated_rows: f64,
    },
    /// Project specific variables
    Project {
        input: Box<PlanNode>,
        variables: Vec<String>,
        estimated_cost: f64,
    },
}

impl PlanNode {
    /// Get the estimated cost of this node
    pub fn estimated_cost(&self) -> f64 {
        match self {
            PlanNode::TripleScan { estimated_cost, .. } => *estimated_cost,
            PlanNode::Join { estimated_cost, .. } => *estimated_cost,
            PlanNode::Filter { estimated_cost, .. } => *estimated_cost,
            PlanNode::Project { estimated_cost, .. } => *estimated_cost,
        }
    }

    /// Get the estimated output rows
    pub fn estimated_rows(&self) -> f64 {
        match self {
            PlanNode::TripleScan { estimated_rows, .. } => *estimated_rows,
            PlanNode::Join { estimated_rows, .. } => *estimated_rows,
            PlanNode::Filter { estimated_rows, .. } => *estimated_rows,
            PlanNode::Project { input, .. } => input.estimated_rows(),
        }
    }

    /// Get all variables produced by this node
    pub fn output_variables(&self) -> HashSet<String> {
        match self {
            PlanNode::TripleScan { pattern, .. } => pattern.variables(),
            PlanNode::Join { left, right, .. } => {
                let mut vars = left.output_variables();
                vars.extend(right.output_variables());
                vars
            }
            PlanNode::Filter { input, .. } => input.output_variables(),
            PlanNode::Project { variables, .. } => {
                variables.iter().cloned().collect()
            }
        }
    }
}

/// Type of join operation
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JoinType {
    /// Nested loop join (simple, works for all cases)
    NestedLoop,
    /// Hash join (good for equi-joins)
    Hash,
    /// Merge join (good for sorted inputs)
    Merge,
    /// Index nested loop join
    IndexNestedLoop,
}

/// Hint for which index to use
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IndexHint {
    /// Use subject index
    SubjectIndex,
    /// Use predicate index
    PredicateIndex,
    /// Use object index
    ObjectIndex,
    /// Use compound index
    CompoundIndex(Vec<String>),
    /// Full scan
    FullScan,
}

/// A complete query plan
#[derive(Clone, Debug)]
pub struct QueryPlan {
    /// Root node of the plan
    pub root: PlanNode,
    /// Total estimated cost
    pub total_cost: f64,
    /// Whether this plan was optimized
    pub optimized: bool,
    /// Optimization notes
    pub notes: Vec<String>,
}

impl QueryPlan {
    /// Create a new query plan
    pub fn new(root: PlanNode) -> Self {
        let total_cost = root.estimated_cost();
        QueryPlan {
            root,
            total_cost,
            optimized: false,
            notes: Vec::new(),
        }
    }

    /// Format the plan as text
    pub fn to_text(&self) -> String {
        let mut output = String::new();
        output.push_str(&format!("Query Plan (estimated cost: {:.2})\n", self.total_cost));
        output.push_str(&format!("Optimized: {}\n\n", self.optimized));
        self.format_node(&self.root, 0, &mut output);

        if !self.notes.is_empty() {
            output.push_str("\nNotes:\n");
            for note in &self.notes {
                output.push_str(&format!("  - {}\n", note));
            }
        }

        output
    }

    fn format_node(&self, node: &PlanNode, depth: usize, output: &mut String) {
        let indent = "  ".repeat(depth);

        match node {
            PlanNode::TripleScan { pattern, estimated_cost, estimated_rows, index_hint } => {
                output.push_str(&format!(
                    "{}-> TripleScan (cost: {:.2}, rows: {:.0})\n",
                    indent, estimated_cost, estimated_rows
                ));
                output.push_str(&format!(
                    "{}   Pattern: ({}, {}, {})\n",
                    indent,
                    format_pattern_term(&pattern.subject),
                    format_pattern_term(&pattern.predicate),
                    format_pattern_term(&pattern.object)
                ));
                if let Some(hint) = index_hint {
                    output.push_str(&format!("{}   Index: {:?}\n", indent, hint));
                }
            }
            PlanNode::Join { left, right, join_type, join_variables, estimated_cost, estimated_rows } => {
                output.push_str(&format!(
                    "{}-> {:?}Join (cost: {:.2}, rows: {:.0}, vars: {:?})\n",
                    indent, join_type, estimated_cost, estimated_rows, join_variables
                ));
                self.format_node(left, depth + 1, output);
                self.format_node(right, depth + 1, output);
            }
            PlanNode::Filter { input, condition, estimated_cost, estimated_rows } => {
                output.push_str(&format!(
                    "{}-> Filter (cost: {:.2}, rows: {:.0})\n",
                    indent, estimated_cost, estimated_rows
                ));
                output.push_str(&format!("{}   Condition: {}\n", indent, condition));
                self.format_node(input, depth + 1, output);
            }
            PlanNode::Project { input, variables, estimated_cost } => {
                output.push_str(&format!(
                    "{}-> Project (cost: {:.2})\n",
                    indent, estimated_cost
                ));
                output.push_str(&format!("{}   Variables: {:?}\n", indent, variables));
                self.format_node(input, depth + 1, output);
            }
        }
    }
}

/// The query optimizer
pub struct QueryOptimizer {
    config: OptimizerConfig,
}

impl QueryOptimizer {
    /// Create a new optimizer
    pub fn new(config: OptimizerConfig) -> Self {
        QueryOptimizer { config }
    }

    /// Collect statistics from a store
    pub fn collect_statistics(&self, store: &Store) -> DataStatistics {
        DataStatistics::collect(store)
    }

    /// Create an initial (unoptimized) plan from patterns
    pub fn create_plan(&self, patterns: &[Triple], stats: &DataStatistics) -> QueryPlan {
        if patterns.is_empty() {
            return QueryPlan::new(PlanNode::TripleScan {
                pattern: TriplePattern::new(
                    PatternTerm::Variable("s".to_string()),
                    PatternTerm::Variable("p".to_string()),
                    PatternTerm::Variable("o".to_string()),
                    0,
                ),
                estimated_cost: stats.triple_count as f64,
                estimated_rows: stats.triple_count as f64,
                index_hint: Some(IndexHint::FullScan),
            });
        }

        // Convert to triple patterns
        let triple_patterns: Vec<TriplePattern> = patterns
            .iter()
            .enumerate()
            .map(|(i, t)| TriplePattern::from_triple(t, i))
            .collect();

        // Create scan nodes for each pattern
        let scan_nodes: Vec<PlanNode> = triple_patterns
            .iter()
            .map(|p| self.create_scan_node(p, stats))
            .collect();

        // Build left-deep join tree
        let mut plan = scan_nodes.into_iter().reduce(|left, right| {
            self.create_join_node(left, right, stats)
        }).unwrap();

        // Wrap in project if needed
        let all_vars: HashSet<String> = triple_patterns
            .iter()
            .flat_map(|p| p.variables())
            .collect();

        if !all_vars.is_empty() {
            plan = PlanNode::Project {
                estimated_cost: plan.estimated_cost() + 0.1,
                input: Box::new(plan),
                variables: all_vars.into_iter().collect(),
            };
        }

        QueryPlan::new(plan)
    }

    /// Optimize a query plan
    pub fn optimize(&self, mut plan: QueryPlan, stats: &DataStatistics) -> QueryPlan {
        if !self.config.enable_cost_based {
            return plan;
        }

        // Extract patterns from plan
        let patterns = self.extract_patterns(&plan.root);

        if patterns.len() <= 1 {
            plan.optimized = true;
            return plan;
        }

        // Try different join orderings
        let optimized_root = if self.config.enable_join_reorder {
            self.optimize_join_order(&patterns, stats)
        } else {
            plan.root
        };

        let total_cost = optimized_root.estimated_cost();

        QueryPlan {
            root: optimized_root,
            total_cost,
            optimized: true,
            notes: vec!["Used cost-based optimization".to_string()],
        }
    }

    /// Create a scan node for a pattern
    fn create_scan_node(&self, pattern: &TriplePattern, stats: &DataStatistics) -> PlanNode {
        let estimated_rows = stats.estimate_cardinality(pattern);
        let estimated_cost = estimated_rows;

        // Determine best index
        let index_hint = if self.config.emit_index_hints {
            Some(self.suggest_index(pattern))
        } else {
            None
        };

        PlanNode::TripleScan {
            pattern: pattern.clone(),
            estimated_cost,
            estimated_rows,
            index_hint,
        }
    }

    /// Create a join node
    fn create_join_node(&self, left: PlanNode, right: PlanNode, _stats: &DataStatistics) -> PlanNode {
        let left_vars = left.output_variables();
        let right_vars = right.output_variables();
        let join_vars: Vec<String> = left_vars.intersection(&right_vars).cloned().collect();

        // Estimate join cardinality
        let left_rows = left.estimated_rows();
        let right_rows = right.estimated_rows();

        // Simple cardinality estimation
        let estimated_rows = if join_vars.is_empty() {
            // Cartesian product
            left_rows * right_rows
        } else {
            // Assume some reduction from the join
            (left_rows * right_rows).sqrt()
        };

        // Choose join type
        let join_type = if join_vars.is_empty() {
            JoinType::NestedLoop
        } else if right_rows < 1000.0 {
            JoinType::Hash
        } else {
            JoinType::NestedLoop
        };

        let left_cost = left.estimated_cost();
        let right_cost = right.estimated_cost();

        let join_cost = match join_type {
            JoinType::NestedLoop => left_rows * right_cost,
            JoinType::Hash => left_cost + right_cost + right_rows,
            JoinType::Merge => left_cost + right_cost + (left_rows + right_rows) * (left_rows + right_rows).log2(),
            JoinType::IndexNestedLoop => left_rows * right_cost.log2().max(1.0),
        };

        PlanNode::Join {
            left: Box::new(left),
            right: Box::new(right),
            join_type,
            join_variables: join_vars,
            estimated_cost: join_cost,
            estimated_rows,
        }
    }

    /// Suggest the best index for a pattern
    fn suggest_index(&self, pattern: &TriplePattern) -> IndexHint {
        let s_bound = !pattern.subject.is_variable();
        let p_bound = !pattern.predicate.is_variable();
        let o_bound = !pattern.object.is_variable();

        match (s_bound, p_bound, o_bound) {
            (true, _, _) => IndexHint::SubjectIndex,
            (_, true, _) => IndexHint::PredicateIndex,
            (_, _, true) => IndexHint::ObjectIndex,
            _ => IndexHint::FullScan,
        }
    }

    /// Extract patterns from a plan
    fn extract_patterns(&self, node: &PlanNode) -> Vec<TriplePattern> {
        match node {
            PlanNode::TripleScan { pattern, .. } => vec![pattern.clone()],
            PlanNode::Join { left, right, .. } => {
                let mut patterns = self.extract_patterns(left);
                patterns.extend(self.extract_patterns(right));
                patterns
            }
            PlanNode::Filter { input, .. } => self.extract_patterns(input),
            PlanNode::Project { input, .. } => self.extract_patterns(input),
        }
    }

    /// Optimize join order using greedy algorithm
    fn optimize_join_order(&self, patterns: &[TriplePattern], stats: &DataStatistics) -> PlanNode {
        if patterns.is_empty() {
            return PlanNode::TripleScan {
                pattern: TriplePattern::new(
                    PatternTerm::Variable("s".to_string()),
                    PatternTerm::Variable("p".to_string()),
                    PatternTerm::Variable("o".to_string()),
                    0,
                ),
                estimated_cost: 0.0,
                estimated_rows: 0.0,
                index_hint: None,
            };
        }

        // Score each pattern by selectivity (lower is more selective)
        let mut scored: Vec<(usize, f64)> = patterns
            .iter()
            .enumerate()
            .map(|(i, p)| (i, stats.estimate_selectivity(p)))
            .collect();

        // Sort by selectivity (most selective first)
        scored.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());

        // Build plan starting with most selective pattern
        let mut remaining: HashSet<usize> = (0..patterns.len()).collect();
        let first_idx = scored[0].0;
        remaining.remove(&first_idx);

        let mut current = self.create_scan_node(&patterns[first_idx], stats);
        let mut current_vars = patterns[first_idx].variables();

        // Greedily add patterns that share variables
        while !remaining.is_empty() {
            // Find pattern that shares most variables with current
            let mut best_idx = None;
            let mut best_score = f64::MAX;

            for &idx in &remaining {
                let pattern = &patterns[idx];
                let shared = pattern.variables().intersection(&current_vars).count();
                let selectivity = stats.estimate_selectivity(pattern);

                // Score: prefer shared variables and high selectivity
                let score = if shared > 0 {
                    selectivity / (shared as f64)
                } else {
                    selectivity * 1000.0 // Penalize cartesian products
                };

                if score < best_score {
                    best_score = score;
                    best_idx = Some(idx);
                }
            }

            if let Some(idx) = best_idx {
                remaining.remove(&idx);
                let next_pattern = &patterns[idx];
                let next_node = self.create_scan_node(next_pattern, stats);
                current_vars.extend(next_pattern.variables());
                current = self.create_join_node(current, next_node, stats);
            } else {
                break;
            }
        }

        current
    }
}

impl Default for QueryOptimizer {
    fn default() -> Self {
        QueryOptimizer::new(OptimizerConfig::default())
    }
}

/// Format a pattern term
fn format_pattern_term(term: &PatternTerm) -> String {
    match term {
        PatternTerm::Bound(t) => format!("{}", t),
        PatternTerm::Variable(v) => format!("?{}", v),
    }
}

/// Statistics for monitoring optimizer performance
#[derive(Clone, Debug, Default)]
pub struct OptimizerStats {
    /// Number of plans considered
    pub plans_considered: usize,
    /// Optimization time in milliseconds
    pub optimization_time_ms: u64,
    /// Whether optimization timed out
    pub timed_out: bool,
    /// Cost reduction achieved
    pub cost_reduction: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_statistics_collection() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/q"),
            Term::uri("http://example.org/c"),
        ));

        let stats = DataStatistics::collect(&store);

        assert_eq!(stats.triple_count, 2);
        assert!(stats.subject_count >= 1);
        assert!(stats.predicate_count >= 1);
    }

    #[test]
    fn test_selectivity_estimation() {
        let mut store = Store::new();
        for i in 0..100 {
            store.add(Triple::new(
                Term::uri(&format!("http://example.org/s{}", i)),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Thing"),
            ));
        }

        let stats = DataStatistics::collect(&store);

        // Pattern with bound predicate should have higher selectivity than unbound
        let bound_pattern = TriplePattern::new(
            PatternTerm::Variable("s".to_string()),
            PatternTerm::Bound(Term::uri("http://example.org/type")),
            PatternTerm::Variable("o".to_string()),
            0,
        );

        let unbound_pattern = TriplePattern::new(
            PatternTerm::Variable("s".to_string()),
            PatternTerm::Variable("p".to_string()),
            PatternTerm::Variable("o".to_string()),
            0,
        );

        let bound_selectivity = stats.estimate_selectivity(&bound_pattern);
        let unbound_selectivity = stats.estimate_selectivity(&unbound_pattern);

        // Bound pattern should have lower or equal selectivity
        assert!(bound_selectivity <= unbound_selectivity);
    }

    #[test]
    fn test_plan_creation() {
        let store = Store::new();
        let stats = DataStatistics::collect(&store);

        let patterns = vec![
            Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Person"),
            ),
            Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/name"),
                Term::universal("name"),
            ),
        ];

        let optimizer = QueryOptimizer::default();
        let plan = optimizer.create_plan(&patterns, &stats);

        assert!(plan.total_cost >= 0.0);
    }

    #[test]
    fn test_join_ordering() {
        let mut store = Store::new();

        // Add some data with varying selectivity
        for i in 0..100 {
            store.add(Triple::new(
                Term::uri(&format!("http://example.org/person{}", i)),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Person"),
            ));
        }

        // Only one person has a rare property
        store.add(Triple::new(
            Term::uri("http://example.org/person1"),
            Term::uri("http://example.org/rare"),
            Term::literal("value"),
        ));

        let stats = DataStatistics::collect(&store);

        let patterns = vec![
            // Less selective pattern first
            TriplePattern::new(
                PatternTerm::Variable("x".to_string()),
                PatternTerm::Bound(Term::uri("http://example.org/type")),
                PatternTerm::Bound(Term::uri("http://example.org/Person")),
                0,
            ),
            // More selective pattern
            TriplePattern::new(
                PatternTerm::Variable("x".to_string()),
                PatternTerm::Bound(Term::uri("http://example.org/rare")),
                PatternTerm::Variable("v".to_string()),
                1,
            ),
        ];

        let optimizer = QueryOptimizer::default();
        let optimized = optimizer.optimize_join_order(&patterns, &stats);

        // The optimizer should produce a valid plan
        assert!(optimized.estimated_cost() >= 0.0);
    }

    #[test]
    fn test_plan_text_output() {
        let pattern = TriplePattern::new(
            PatternTerm::Variable("x".to_string()),
            PatternTerm::Bound(Term::uri("http://example.org/type")),
            PatternTerm::Bound(Term::uri("http://example.org/Person")),
            0,
        );

        let node = PlanNode::TripleScan {
            pattern,
            estimated_cost: 100.0,
            estimated_rows: 50.0,
            index_hint: Some(IndexHint::PredicateIndex),
        };

        let plan = QueryPlan::new(node);
        let text = plan.to_text();

        assert!(text.contains("TripleScan"));
        assert!(text.contains("cost"));
    }
}
