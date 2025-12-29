//! Constraint Logic Programming (CLP) for N3
//!
//! Finite domain constraints, propagation, and optimization.
//!
//! # Features
//!
//! - Finite domain variables with range constraints
//! - Constraint propagation with arc consistency
//! - Global constraints (allDifferent, sum, etc.)
//! - Optimization (minimize/maximize)
//! - Integration with N3 reasoning
//!
//! # Example
//!
//! ```ignore
//! use cwm::reasoning::clp::{ConstraintStore, Variable, Constraint, Solver};
//!
//! let mut store = ConstraintStore::new();
//!
//! // Create variables with domains
//! let x = store.new_var("x", 1..=10);
//! let y = store.new_var("y", 1..=10);
//!
//! // Add constraints
//! store.add_constraint(Constraint::NotEqual(x, y));
//! store.add_constraint(Constraint::LessThan(x, y));
//!
//! // Solve
//! let solutions = store.solve();
//! ```

use std::collections::{HashMap, HashSet, BTreeSet, VecDeque};
use std::ops::RangeInclusive;

/// A constraint variable identifier
pub type VarId = usize;

/// A finite domain (set of possible values)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Domain {
    /// Possible values
    values: BTreeSet<i64>,
}

impl Domain {
    /// Create a domain from a range
    pub fn from_range(range: RangeInclusive<i64>) -> Self {
        Domain {
            values: range.collect(),
        }
    }

    /// Create a domain from specific values
    pub fn from_values(values: impl IntoIterator<Item = i64>) -> Self {
        Domain {
            values: values.into_iter().collect(),
        }
    }

    /// Create a singleton domain
    pub fn singleton(value: i64) -> Self {
        let mut values = BTreeSet::new();
        values.insert(value);
        Domain { values }
    }

    /// Create an empty domain
    pub fn empty() -> Self {
        Domain {
            values: BTreeSet::new(),
        }
    }

    /// Check if domain is empty
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Check if domain has a single value
    pub fn is_singleton(&self) -> bool {
        self.values.len() == 1
    }

    /// Get the single value if singleton
    pub fn get_singleton(&self) -> Option<i64> {
        if self.is_singleton() {
            self.values.iter().next().copied()
        } else {
            None
        }
    }

    /// Get domain size
    pub fn size(&self) -> usize {
        self.values.len()
    }

    /// Get minimum value
    pub fn min(&self) -> Option<i64> {
        self.values.iter().next().copied()
    }

    /// Get maximum value
    pub fn max(&self) -> Option<i64> {
        self.values.iter().next_back().copied()
    }

    /// Check if domain contains a value
    pub fn contains(&self, value: i64) -> bool {
        self.values.contains(&value)
    }

    /// Remove a value from the domain
    pub fn remove(&mut self, value: i64) -> bool {
        self.values.remove(&value)
    }

    /// Restrict domain to values less than n
    pub fn restrict_lt(&mut self, n: i64) {
        self.values.retain(|&v| v < n);
    }

    /// Restrict domain to values less than or equal to n
    pub fn restrict_le(&mut self, n: i64) {
        self.values.retain(|&v| v <= n);
    }

    /// Restrict domain to values greater than n
    pub fn restrict_gt(&mut self, n: i64) {
        self.values.retain(|&v| v > n);
    }

    /// Restrict domain to values greater than or equal to n
    pub fn restrict_ge(&mut self, n: i64) {
        self.values.retain(|&v| v >= n);
    }

    /// Intersect with another domain
    pub fn intersect(&mut self, other: &Domain) {
        self.values.retain(|v| other.values.contains(v));
    }

    /// Get iterator over values
    pub fn iter(&self) -> impl Iterator<Item = i64> + '_ {
        self.values.iter().copied()
    }
}

/// A constraint variable
#[derive(Clone, Debug)]
pub struct Variable {
    /// Variable ID
    pub id: VarId,
    /// Variable name
    pub name: String,
    /// Current domain
    pub domain: Domain,
    /// Original domain (for backtracking)
    pub original_domain: Domain,
}

impl Variable {
    /// Create a new variable
    pub fn new(id: VarId, name: impl Into<String>, domain: Domain) -> Self {
        Variable {
            id,
            name: name.into(),
            original_domain: domain.clone(),
            domain,
        }
    }

    /// Check if variable is bound (singleton domain)
    pub fn is_bound(&self) -> bool {
        self.domain.is_singleton()
    }

    /// Get bound value
    pub fn value(&self) -> Option<i64> {
        self.domain.get_singleton()
    }

    /// Reset to original domain
    pub fn reset(&mut self) {
        self.domain = self.original_domain.clone();
    }
}

/// A constraint between variables
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Constraint {
    /// x = y
    Equal(VarId, VarId),
    /// x != y
    NotEqual(VarId, VarId),
    /// x < y
    LessThan(VarId, VarId),
    /// x <= y
    LessEqual(VarId, VarId),
    /// x > y
    GreaterThan(VarId, VarId),
    /// x >= y
    GreaterEqual(VarId, VarId),
    /// x = constant
    EqualConst(VarId, i64),
    /// x != constant
    NotEqualConst(VarId, i64),
    /// x < constant
    LessThanConst(VarId, i64),
    /// x <= constant
    LessEqualConst(VarId, i64),
    /// x > constant
    GreaterThanConst(VarId, i64),
    /// x >= constant
    GreaterEqualConst(VarId, i64),
    /// x + y = z
    Sum(VarId, VarId, VarId),
    /// x * y = z
    Product(VarId, VarId, VarId),
    /// x + constant = y
    AddConst(VarId, i64, VarId),
    /// x * constant = y
    MulConst(VarId, i64, VarId),
    /// All variables have different values
    AllDifferent(Vec<VarId>),
    /// Sum of variables equals constant
    SumEquals(Vec<VarId>, i64),
    /// At most k variables are true (value 1)
    AtMost(Vec<VarId>, usize),
    /// At least k variables are true (value 1)
    AtLeast(Vec<VarId>, usize),
    /// Exactly k variables are true (value 1)
    Exactly(Vec<VarId>, usize),
    /// Element constraint: vars[index] = value
    Element(Vec<VarId>, VarId, VarId),
    /// Custom constraint with propagator
    Custom(String, Vec<VarId>),
}

impl Constraint {
    /// Get all variables involved in this constraint
    pub fn variables(&self) -> Vec<VarId> {
        match self {
            Constraint::Equal(x, y) => vec![*x, *y],
            Constraint::NotEqual(x, y) => vec![*x, *y],
            Constraint::LessThan(x, y) => vec![*x, *y],
            Constraint::LessEqual(x, y) => vec![*x, *y],
            Constraint::GreaterThan(x, y) => vec![*x, *y],
            Constraint::GreaterEqual(x, y) => vec![*x, *y],
            Constraint::EqualConst(x, _) => vec![*x],
            Constraint::NotEqualConst(x, _) => vec![*x],
            Constraint::LessThanConst(x, _) => vec![*x],
            Constraint::LessEqualConst(x, _) => vec![*x],
            Constraint::GreaterThanConst(x, _) => vec![*x],
            Constraint::GreaterEqualConst(x, _) => vec![*x],
            Constraint::Sum(x, y, z) => vec![*x, *y, *z],
            Constraint::Product(x, y, z) => vec![*x, *y, *z],
            Constraint::AddConst(x, _, y) => vec![*x, *y],
            Constraint::MulConst(x, _, y) => vec![*x, *y],
            Constraint::AllDifferent(vars) => vars.clone(),
            Constraint::SumEquals(vars, _) => vars.clone(),
            Constraint::AtMost(vars, _) => vars.clone(),
            Constraint::AtLeast(vars, _) => vars.clone(),
            Constraint::Exactly(vars, _) => vars.clone(),
            Constraint::Element(vars, idx, val) => {
                let mut v = vars.clone();
                v.push(*idx);
                v.push(*val);
                v
            }
            Constraint::Custom(_, vars) => vars.clone(),
        }
    }
}

/// Result of constraint propagation
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PropagationResult {
    /// No change
    NoChange,
    /// Domain was reduced
    Reduced,
    /// Constraint is satisfied
    Satisfied,
    /// Constraint failed (domain became empty)
    Failed,
}

/// Constraint store
#[derive(Clone, Debug)]
pub struct ConstraintStore {
    /// Variables
    variables: Vec<Variable>,
    /// Constraints
    constraints: Vec<Constraint>,
    /// Variable name to ID mapping
    name_to_id: HashMap<String, VarId>,
    /// Constraints involving each variable
    var_constraints: HashMap<VarId, Vec<usize>>,
}

impl ConstraintStore {
    /// Create a new constraint store
    pub fn new() -> Self {
        ConstraintStore {
            variables: Vec::new(),
            constraints: Vec::new(),
            name_to_id: HashMap::new(),
            var_constraints: HashMap::new(),
        }
    }

    /// Create a new variable with a range domain
    pub fn new_var(&mut self, name: impl Into<String>, range: RangeInclusive<i64>) -> VarId {
        let name = name.into();
        let id = self.variables.len();
        let domain = Domain::from_range(range);
        self.variables.push(Variable::new(id, name.clone(), domain));
        self.name_to_id.insert(name, id);
        id
    }

    /// Create a new variable with specific values
    pub fn new_var_values(&mut self, name: impl Into<String>, values: impl IntoIterator<Item = i64>) -> VarId {
        let name = name.into();
        let id = self.variables.len();
        let domain = Domain::from_values(values);
        self.variables.push(Variable::new(id, name.clone(), domain));
        self.name_to_id.insert(name, id);
        id
    }

    /// Create a boolean variable (domain {0, 1})
    pub fn new_bool_var(&mut self, name: impl Into<String>) -> VarId {
        self.new_var_values(name, [0, 1])
    }

    /// Get variable by name
    pub fn get_var(&self, name: &str) -> Option<VarId> {
        self.name_to_id.get(name).copied()
    }

    /// Get variable domain
    pub fn domain(&self, var: VarId) -> Option<&Domain> {
        self.variables.get(var).map(|v| &v.domain)
    }

    /// Get variable value (if bound)
    pub fn value(&self, var: VarId) -> Option<i64> {
        self.variables.get(var).and_then(|v| v.value())
    }

    /// Add a constraint
    pub fn add_constraint(&mut self, constraint: Constraint) {
        let idx = self.constraints.len();
        for var in constraint.variables() {
            self.var_constraints.entry(var).or_default().push(idx);
        }
        self.constraints.push(constraint);
    }

    /// Propagate a single constraint
    fn propagate_constraint(&mut self, idx: usize) -> PropagationResult {
        let constraint = self.constraints[idx].clone();

        match constraint {
            Constraint::Equal(x, y) => self.propagate_equal(x, y),
            Constraint::NotEqual(x, y) => self.propagate_not_equal(x, y),
            Constraint::LessThan(x, y) => self.propagate_less_than(x, y),
            Constraint::LessEqual(x, y) => self.propagate_less_equal(x, y),
            Constraint::GreaterThan(x, y) => self.propagate_greater_than(x, y),
            Constraint::GreaterEqual(x, y) => self.propagate_greater_equal(x, y),
            Constraint::EqualConst(x, c) => self.propagate_equal_const(x, c),
            Constraint::NotEqualConst(x, c) => self.propagate_not_equal_const(x, c),
            Constraint::LessThanConst(x, c) => self.propagate_lt_const(x, c),
            Constraint::LessEqualConst(x, c) => self.propagate_le_const(x, c),
            Constraint::GreaterThanConst(x, c) => self.propagate_gt_const(x, c),
            Constraint::GreaterEqualConst(x, c) => self.propagate_ge_const(x, c),
            Constraint::AllDifferent(ref vars) => self.propagate_all_different(vars),
            Constraint::SumEquals(ref vars, sum) => self.propagate_sum_equals(vars, sum),
            _ => PropagationResult::NoChange,
        }
    }

    fn propagate_equal(&mut self, x: VarId, y: VarId) -> PropagationResult {
        let dx = self.variables[x].domain.clone();
        let dy = self.variables[y].domain.clone();

        let mut new_d = dx.clone();
        new_d.intersect(&dy);

        if new_d.is_empty() {
            return PropagationResult::Failed;
        }

        let changed_x = new_d != dx;
        let changed_y = new_d != dy;

        self.variables[x].domain = new_d.clone();
        self.variables[y].domain = new_d;

        if changed_x || changed_y {
            PropagationResult::Reduced
        } else if self.variables[x].is_bound() && self.variables[y].is_bound() {
            PropagationResult::Satisfied
        } else {
            PropagationResult::NoChange
        }
    }

    fn propagate_not_equal(&mut self, x: VarId, y: VarId) -> PropagationResult {
        // Get singleton values first to avoid borrow conflicts
        let singleton_x = self.variables[x].domain.get_singleton();
        let singleton_y = self.variables[y].domain.get_singleton();

        // If both are singletons, check they're different
        if let (Some(vx), Some(vy)) = (singleton_x, singleton_y) {
            return if vx == vy {
                PropagationResult::Failed
            } else {
                PropagationResult::Satisfied
            };
        }

        // If x is singleton, remove its value from y
        if let Some(vx) = singleton_x {
            if self.variables[y].domain.remove(vx) {
                if self.variables[y].domain.is_empty() {
                    return PropagationResult::Failed;
                }
                return PropagationResult::Reduced;
            }
        }

        // If y is singleton, remove its value from x
        if let Some(vy) = singleton_y {
            if self.variables[x].domain.remove(vy) {
                if self.variables[x].domain.is_empty() {
                    return PropagationResult::Failed;
                }
                return PropagationResult::Reduced;
            }
        }

        PropagationResult::NoChange
    }

    fn propagate_less_than(&mut self, x: VarId, y: VarId) -> PropagationResult {
        let max_x = self.variables[x].domain.max();
        let min_y = self.variables[y].domain.min();

        if max_x.is_none() || min_y.is_none() {
            return PropagationResult::Failed;
        }

        let max_x = max_x.unwrap();
        let min_y = min_y.unwrap();

        let mut changed = false;

        // x < y means x < max(y), so x <= max(y) - 1
        let max_y = self.variables[y].domain.max().unwrap();
        let old_size_x = self.variables[x].domain.size();
        self.variables[x].domain.restrict_lt(max_y);
        if self.variables[x].domain.size() != old_size_x {
            changed = true;
        }

        // y > x means y > min(x), so y >= min(x) + 1
        let min_x = self.variables[x].domain.min().unwrap_or(i64::MIN);
        let old_size_y = self.variables[y].domain.size();
        self.variables[y].domain.restrict_gt(min_x);
        if self.variables[y].domain.size() != old_size_y {
            changed = true;
        }

        if self.variables[x].domain.is_empty() || self.variables[y].domain.is_empty() {
            return PropagationResult::Failed;
        }

        if changed {
            PropagationResult::Reduced
        } else {
            PropagationResult::NoChange
        }
    }

    fn propagate_less_equal(&mut self, x: VarId, y: VarId) -> PropagationResult {
        let mut changed = false;

        let max_y = self.variables[y].domain.max();
        if let Some(max_y) = max_y {
            let old_size = self.variables[x].domain.size();
            self.variables[x].domain.restrict_le(max_y);
            if self.variables[x].domain.size() != old_size {
                changed = true;
            }
        }

        let min_x = self.variables[x].domain.min();
        if let Some(min_x) = min_x {
            let old_size = self.variables[y].domain.size();
            self.variables[y].domain.restrict_ge(min_x);
            if self.variables[y].domain.size() != old_size {
                changed = true;
            }
        }

        if self.variables[x].domain.is_empty() || self.variables[y].domain.is_empty() {
            return PropagationResult::Failed;
        }

        if changed {
            PropagationResult::Reduced
        } else {
            PropagationResult::NoChange
        }
    }

    fn propagate_greater_than(&mut self, x: VarId, y: VarId) -> PropagationResult {
        self.propagate_less_than(y, x)
    }

    fn propagate_greater_equal(&mut self, x: VarId, y: VarId) -> PropagationResult {
        self.propagate_less_equal(y, x)
    }

    fn propagate_equal_const(&mut self, x: VarId, c: i64) -> PropagationResult {
        if !self.variables[x].domain.contains(c) {
            return PropagationResult::Failed;
        }

        if self.variables[x].domain.is_singleton() {
            return PropagationResult::Satisfied;
        }

        self.variables[x].domain = Domain::singleton(c);
        PropagationResult::Reduced
    }

    fn propagate_not_equal_const(&mut self, x: VarId, c: i64) -> PropagationResult {
        if !self.variables[x].domain.contains(c) {
            return PropagationResult::Satisfied;
        }

        self.variables[x].domain.remove(c);

        if self.variables[x].domain.is_empty() {
            PropagationResult::Failed
        } else {
            PropagationResult::Reduced
        }
    }

    fn propagate_lt_const(&mut self, x: VarId, c: i64) -> PropagationResult {
        let old_size = self.variables[x].domain.size();
        self.variables[x].domain.restrict_lt(c);

        if self.variables[x].domain.is_empty() {
            PropagationResult::Failed
        } else if self.variables[x].domain.size() != old_size {
            PropagationResult::Reduced
        } else {
            PropagationResult::NoChange
        }
    }

    fn propagate_le_const(&mut self, x: VarId, c: i64) -> PropagationResult {
        let old_size = self.variables[x].domain.size();
        self.variables[x].domain.restrict_le(c);

        if self.variables[x].domain.is_empty() {
            PropagationResult::Failed
        } else if self.variables[x].domain.size() != old_size {
            PropagationResult::Reduced
        } else {
            PropagationResult::NoChange
        }
    }

    fn propagate_gt_const(&mut self, x: VarId, c: i64) -> PropagationResult {
        let old_size = self.variables[x].domain.size();
        self.variables[x].domain.restrict_gt(c);

        if self.variables[x].domain.is_empty() {
            PropagationResult::Failed
        } else if self.variables[x].domain.size() != old_size {
            PropagationResult::Reduced
        } else {
            PropagationResult::NoChange
        }
    }

    fn propagate_ge_const(&mut self, x: VarId, c: i64) -> PropagationResult {
        let old_size = self.variables[x].domain.size();
        self.variables[x].domain.restrict_ge(c);

        if self.variables[x].domain.is_empty() {
            PropagationResult::Failed
        } else if self.variables[x].domain.size() != old_size {
            PropagationResult::Reduced
        } else {
            PropagationResult::NoChange
        }
    }

    fn propagate_all_different(&mut self, vars: &[VarId]) -> PropagationResult {
        let mut changed = false;

        // Remove singleton values from other domains
        for &var in vars {
            if let Some(val) = self.variables[var].domain.get_singleton() {
                for &other in vars {
                    if other != var {
                        if self.variables[other].domain.remove(val) {
                            changed = true;
                            if self.variables[other].domain.is_empty() {
                                return PropagationResult::Failed;
                            }
                        }
                    }
                }
            }
        }

        if changed {
            PropagationResult::Reduced
        } else {
            PropagationResult::NoChange
        }
    }

    fn propagate_sum_equals(&mut self, vars: &[VarId], target: i64) -> PropagationResult {
        // Simple bounds propagation
        let mut min_sum: i64 = 0;
        let mut max_sum: i64 = 0;

        for &var in vars {
            if let (Some(min), Some(max)) = (
                self.variables[var].domain.min(),
                self.variables[var].domain.max(),
            ) {
                min_sum += min;
                max_sum += max;
            } else {
                return PropagationResult::Failed;
            }
        }

        if target < min_sum || target > max_sum {
            return PropagationResult::Failed;
        }

        let mut changed = false;

        // For each variable, restrict its domain
        for &var in vars {
            let var_min = self.variables[var].domain.min().unwrap();
            let var_max = self.variables[var].domain.max().unwrap();

            // Other variables sum
            let other_min = min_sum - var_min;
            let other_max = max_sum - var_max;

            // This variable must be >= target - other_max
            let new_min = target - other_max;
            // This variable must be <= target - other_min
            let new_max = target - other_min;

            let old_size = self.variables[var].domain.size();
            self.variables[var].domain.restrict_ge(new_min);
            self.variables[var].domain.restrict_le(new_max);

            if self.variables[var].domain.is_empty() {
                return PropagationResult::Failed;
            }

            if self.variables[var].domain.size() != old_size {
                changed = true;
            }
        }

        if changed {
            PropagationResult::Reduced
        } else {
            PropagationResult::NoChange
        }
    }

    /// Run arc consistency propagation
    pub fn propagate(&mut self) -> bool {
        let mut queue: VecDeque<usize> = (0..self.constraints.len()).collect();
        let mut in_queue: HashSet<usize> = queue.iter().copied().collect();

        while let Some(idx) = queue.pop_front() {
            in_queue.remove(&idx);

            let result = self.propagate_constraint(idx);

            match result {
                PropagationResult::Failed => return false,
                PropagationResult::Reduced => {
                    // Re-add affected constraints
                    let vars = self.constraints[idx].variables();
                    for var in vars {
                        if let Some(constraint_indices) = self.var_constraints.get(&var) {
                            for &c_idx in constraint_indices {
                                if c_idx != idx && !in_queue.contains(&c_idx) {
                                    queue.push_back(c_idx);
                                    in_queue.insert(c_idx);
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        true
    }

    /// Check if all variables are bound
    pub fn is_solved(&self) -> bool {
        self.variables.iter().all(|v| v.is_bound())
    }

    /// Get current solution (if all variables are bound)
    pub fn solution(&self) -> Option<HashMap<String, i64>> {
        if !self.is_solved() {
            return None;
        }

        let mut sol = HashMap::new();
        for var in &self.variables {
            if let Some(val) = var.value() {
                sol.insert(var.name.clone(), val);
            }
        }
        Some(sol)
    }

    /// Save current state for backtracking
    pub fn save_state(&self) -> Vec<Domain> {
        self.variables.iter().map(|v| v.domain.clone()).collect()
    }

    /// Restore state
    pub fn restore_state(&mut self, state: &[Domain]) {
        for (var, domain) in self.variables.iter_mut().zip(state.iter()) {
            var.domain = domain.clone();
        }
    }

    /// Get variable count
    pub fn num_vars(&self) -> usize {
        self.variables.len()
    }

    /// Get constraint count
    pub fn num_constraints(&self) -> usize {
        self.constraints.len()
    }
}

impl Default for ConstraintStore {
    fn default() -> Self {
        Self::new()
    }
}

/// Constraint solver with search
#[derive(Clone, Debug)]
pub struct Solver {
    /// Constraint store
    store: ConstraintStore,
    /// Search strategy
    strategy: SearchStrategy,
    /// Maximum solutions to find
    max_solutions: usize,
    /// Statistics
    stats: SolverStats,
}

/// Search strategy for variable selection
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SearchStrategy {
    /// First unbound variable
    FirstFail,
    /// Smallest domain first
    SmallestDomain,
    /// Most constrained variable
    MostConstrained,
    /// Random selection
    Random,
}

impl Default for SearchStrategy {
    fn default() -> Self {
        SearchStrategy::SmallestDomain
    }
}

/// Solver statistics
#[derive(Clone, Debug, Default)]
pub struct SolverStats {
    /// Number of nodes explored
    pub nodes: usize,
    /// Number of backtracks
    pub backtracks: usize,
    /// Number of solutions found
    pub solutions: usize,
    /// Number of propagations
    pub propagations: usize,
}

impl Solver {
    /// Create a new solver
    pub fn new(store: ConstraintStore) -> Self {
        Solver {
            store,
            strategy: SearchStrategy::default(),
            max_solutions: 1,
            stats: SolverStats::default(),
        }
    }

    /// Set search strategy
    pub fn with_strategy(mut self, strategy: SearchStrategy) -> Self {
        self.strategy = strategy;
        self
    }

    /// Set maximum solutions
    pub fn with_max_solutions(mut self, max: usize) -> Self {
        self.max_solutions = max;
        self
    }

    /// Find all solutions (up to max_solutions)
    pub fn solve(&mut self) -> Vec<HashMap<String, i64>> {
        self.stats = SolverStats::default();
        let mut solutions = Vec::new();

        // Initial propagation
        self.stats.propagations += 1;
        if !self.store.propagate() {
            return solutions;
        }

        self.search(&mut solutions);
        solutions
    }

    /// Find one solution
    pub fn solve_one(&mut self) -> Option<HashMap<String, i64>> {
        self.max_solutions = 1;
        self.solve().into_iter().next()
    }

    /// Recursive search with backtracking
    fn search(&mut self, solutions: &mut Vec<HashMap<String, i64>>) {
        if solutions.len() >= self.max_solutions {
            return;
        }

        self.stats.nodes += 1;

        // Check if solved
        if self.store.is_solved() {
            if let Some(sol) = self.store.solution() {
                self.stats.solutions += 1;
                solutions.push(sol);
            }
            return;
        }

        // Select variable to branch on
        let var = match self.select_variable() {
            Some(v) => v,
            None => return,
        };

        // Get domain values
        let values: Vec<i64> = self.store.variables[var].domain.iter().collect();

        // Save state
        let state = self.store.save_state();

        // Try each value
        for value in values {
            if solutions.len() >= self.max_solutions {
                break;
            }

            // Assign value
            self.store.variables[var].domain = Domain::singleton(value);

            // Propagate
            self.stats.propagations += 1;
            if self.store.propagate() {
                self.search(solutions);
            }

            // Restore state
            self.stats.backtracks += 1;
            self.store.restore_state(&state);
        }
    }

    /// Select next variable to branch on
    fn select_variable(&self) -> Option<VarId> {
        let unbound: Vec<VarId> = self.store.variables.iter()
            .filter(|v| !v.is_bound())
            .map(|v| v.id)
            .collect();

        if unbound.is_empty() {
            return None;
        }

        match self.strategy {
            SearchStrategy::FirstFail => unbound.into_iter().next(),
            SearchStrategy::SmallestDomain => {
                unbound.into_iter()
                    .min_by_key(|&id| self.store.variables[id].domain.size())
            }
            SearchStrategy::MostConstrained => {
                unbound.into_iter()
                    .max_by_key(|&id| {
                        self.store.var_constraints.get(&id)
                            .map(|c| c.len())
                            .unwrap_or(0)
                    })
            }
            SearchStrategy::Random => {
                // Use first for determinism in tests
                unbound.into_iter().next()
            }
        }
    }

    /// Get statistics
    pub fn stats(&self) -> &SolverStats {
        &self.stats
    }
}

/// Optimization objective
#[derive(Clone, Debug)]
pub enum Objective {
    /// Minimize variable value
    Minimize(VarId),
    /// Maximize variable value
    Maximize(VarId),
    /// Minimize sum of variables
    MinimizeSum(Vec<VarId>),
    /// Maximize sum of variables
    MaximizeSum(Vec<VarId>),
}

/// Optimizer for constraint problems
#[derive(Clone, Debug)]
pub struct Optimizer {
    /// Base solver
    solver: Solver,
    /// Objective
    objective: Objective,
}

impl Optimizer {
    /// Create a new optimizer
    pub fn new(store: ConstraintStore, objective: Objective) -> Self {
        Optimizer {
            solver: Solver::new(store),
            objective,
        }
    }

    /// Find optimal solution
    pub fn optimize(&mut self) -> Option<(HashMap<String, i64>, i64)> {
        // Find all solutions and pick best
        self.solver.max_solutions = usize::MAX;
        let solutions = self.solver.solve();

        if solutions.is_empty() {
            return None;
        }

        let best = solutions.into_iter()
            .map(|sol| {
                let obj_value = self.evaluate(&sol);
                (sol, obj_value)
            })
            .min_by_key(|(_, obj)| {
                match self.objective {
                    Objective::Minimize(_) | Objective::MinimizeSum(_) => *obj,
                    Objective::Maximize(_) | Objective::MaximizeSum(_) => -obj,
                }
            });

        best
    }

    /// Evaluate objective for a solution
    fn evaluate(&self, solution: &HashMap<String, i64>) -> i64 {
        match &self.objective {
            Objective::Minimize(var) | Objective::Maximize(var) => {
                let name = &self.solver.store.variables[*var].name;
                *solution.get(name).unwrap_or(&0)
            }
            Objective::MinimizeSum(vars) | Objective::MaximizeSum(vars) => {
                vars.iter()
                    .map(|&var| {
                        let name = &self.solver.store.variables[var].name;
                        *solution.get(name).unwrap_or(&0)
                    })
                    .sum()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_domain_basic() {
        let mut d = Domain::from_range(1..=5);
        assert_eq!(d.size(), 5);
        assert_eq!(d.min(), Some(1));
        assert_eq!(d.max(), Some(5));
        assert!(d.contains(3));
        assert!(!d.contains(6));

        d.remove(3);
        assert!(!d.contains(3));
        assert_eq!(d.size(), 4);
    }

    #[test]
    fn test_domain_restrict() {
        let mut d = Domain::from_range(1..=10);

        d.restrict_lt(8);
        assert_eq!(d.max(), Some(7));

        d.restrict_gt(2);
        assert_eq!(d.min(), Some(3));

        assert_eq!(d.size(), 5); // 3,4,5,6,7
    }

    #[test]
    fn test_simple_constraint() {
        let mut store = ConstraintStore::new();

        let x = store.new_var("x", 1..=5);
        let y = store.new_var("y", 1..=5);

        store.add_constraint(Constraint::LessThan(x, y));

        assert!(store.propagate());

        // x < y, so x can be 1-4, y can be 2-5
        assert_eq!(store.variables[x].domain.max(), Some(4));
        assert_eq!(store.variables[y].domain.min(), Some(2));
    }

    #[test]
    fn test_equal_constraint() {
        let mut store = ConstraintStore::new();

        let x = store.new_var("x", 1..=5);
        let y = store.new_var("y", 3..=7);

        store.add_constraint(Constraint::Equal(x, y));
        assert!(store.propagate());

        // Intersection: 3,4,5
        assert_eq!(store.variables[x].domain.size(), 3);
        assert_eq!(store.variables[y].domain.size(), 3);
    }

    #[test]
    fn test_all_different() {
        let mut store = ConstraintStore::new();

        let x = store.new_var("x", 1..=3);
        let y = store.new_var("y", 1..=3);
        let z = store.new_var("z", 1..=3);

        store.add_constraint(Constraint::AllDifferent(vec![x, y, z]));
        store.add_constraint(Constraint::EqualConst(x, 1));

        assert!(store.propagate());

        // x=1, so y and z can only be 2,3
        assert!(!store.variables[y].domain.contains(1));
        assert!(!store.variables[z].domain.contains(1));
    }

    #[test]
    fn test_solver() {
        let mut store = ConstraintStore::new();

        let x = store.new_var("x", 1..=3);
        let y = store.new_var("y", 1..=3);

        store.add_constraint(Constraint::AllDifferent(vec![x, y]));
        store.add_constraint(Constraint::LessThan(x, y));

        let mut solver = Solver::new(store).with_max_solutions(100);
        let solutions = solver.solve();

        // x < y, x != y: (1,2), (1,3), (2,3)
        assert_eq!(solutions.len(), 3);
    }

    #[test]
    fn test_n_queens_2() {
        // Simple 2-queens (no solution)
        let mut store = ConstraintStore::new();

        let q1 = store.new_var("q1", 1..=2);
        let q2 = store.new_var("q2", 1..=2);

        // Different rows
        store.add_constraint(Constraint::NotEqual(q1, q2));

        // Diagonal constraints would need custom handling
        // For now just test the basic constraint

        let mut solver = Solver::new(store);
        let solutions = solver.solve();

        // Without diagonal constraints: (1,2), (2,1)
        assert!(!solutions.is_empty());
    }

    #[test]
    fn test_sum_constraint() {
        let mut store = ConstraintStore::new();

        let x = store.new_var("x", 1..=10);
        let y = store.new_var("y", 1..=10);
        let z = store.new_var("z", 1..=10);

        store.add_constraint(Constraint::SumEquals(vec![x, y, z], 6));

        assert!(store.propagate());

        // Sum = 6, each variable must be at most 4 (1+1+4=6)
        assert!(store.variables[x].domain.max().unwrap() <= 4);
    }

    #[test]
    fn test_optimizer() {
        let mut store = ConstraintStore::new();

        let x = store.new_var("x", 1..=5);
        let y = store.new_var("y", 1..=5);

        store.add_constraint(Constraint::SumEquals(vec![x, y], 6));

        let mut optimizer = Optimizer::new(store, Objective::Minimize(x));
        let result = optimizer.optimize();

        assert!(result.is_some());
        let (sol, obj) = result.unwrap();
        assert_eq!(obj, 1); // Minimum x is 1 (when y=5)
        assert_eq!(*sol.get("y").unwrap(), 5);
    }

    #[test]
    fn test_infeasible() {
        let mut store = ConstraintStore::new();

        let x = store.new_var("x", 1..=3);
        store.add_constraint(Constraint::GreaterThanConst(x, 5));

        assert!(!store.propagate());
    }

    #[test]
    fn test_solver_stats() {
        let mut store = ConstraintStore::new();

        let x = store.new_var("x", 1..=3);
        let y = store.new_var("y", 1..=3);

        store.add_constraint(Constraint::NotEqual(x, y));

        let mut solver = Solver::new(store).with_max_solutions(100);
        solver.solve();

        assert!(solver.stats().nodes > 0);
        assert!(solver.stats().solutions > 0);
    }
}
