//! DPLL SAT Solver
//!
//! Implementation of the Davis-Putnam-Logemann-Loveland algorithm for
//! propositional satisfiability (SAT) solving.
//!
//! Features:
//! - Unit propagation
//! - Pure literal elimination
//! - Chronological backtracking
//! - Variable selection heuristics

use std::collections::{HashMap, HashSet};

/// A propositional variable (positive integer)
pub type Var = u32;

/// A literal is a variable with a sign (positive or negative)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lit {
    /// The variable (1-indexed)
    var: Var,
    /// True if positive, false if negated
    sign: bool,
}

impl Lit {
    pub fn new(var: Var, sign: bool) -> Self {
        Lit { var, sign }
    }

    pub fn positive(var: Var) -> Self {
        Lit { var, sign: true }
    }

    pub fn negative(var: Var) -> Self {
        Lit { var, sign: false }
    }

    pub fn var(&self) -> Var {
        self.var
    }

    pub fn sign(&self) -> bool {
        self.sign
    }

    pub fn negated(&self) -> Self {
        Lit { var: self.var, sign: !self.sign }
    }

    /// Parse from DIMACS format (positive/negative integer)
    pub fn from_dimacs(val: i32) -> Self {
        if val > 0 {
            Lit::positive(val as Var)
        } else {
            Lit::negative((-val) as Var)
        }
    }

    pub fn to_dimacs(&self) -> i32 {
        if self.sign {
            self.var as i32
        } else {
            -(self.var as i32)
        }
    }
}

impl std::fmt::Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.sign {
            write!(f, "{}", self.var)
        } else {
            write!(f, "-{}", self.var)
        }
    }
}

/// A clause is a disjunction of literals
#[derive(Debug, Clone)]
pub struct SatClause {
    pub literals: Vec<Lit>,
}

impl SatClause {
    pub fn new(literals: Vec<Lit>) -> Self {
        SatClause { literals }
    }

    pub fn is_empty(&self) -> bool {
        self.literals.is_empty()
    }

    pub fn is_unit(&self) -> bool {
        self.literals.len() == 1
    }

    pub fn unit_literal(&self) -> Option<Lit> {
        if self.is_unit() {
            Some(self.literals[0])
        } else {
            None
        }
    }

    /// Parse from DIMACS format (space-separated integers ending with 0)
    pub fn from_dimacs(line: &str) -> Option<Self> {
        let parts: Vec<i32> = line
            .split_whitespace()
            .filter_map(|s| s.parse().ok())
            .collect();

        if parts.is_empty() || *parts.last().unwrap() != 0 {
            return None;
        }

        let literals: Vec<Lit> = parts[..parts.len() - 1]
            .iter()
            .map(|&v| Lit::from_dimacs(v))
            .collect();

        Some(SatClause::new(literals))
    }
}

impl std::fmt::Display for SatClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lits: Vec<String> = self.literals.iter().map(|l| l.to_string()).collect();
        write!(f, "({})", lits.join(" ∨ "))
    }
}

/// Assignment of truth values to variables
#[derive(Debug, Clone, Default)]
pub struct Assignment {
    values: HashMap<Var, bool>,
}

impl Assignment {
    pub fn new() -> Self {
        Assignment { values: HashMap::new() }
    }

    pub fn assign(&mut self, var: Var, value: bool) {
        self.values.insert(var, value);
    }

    pub fn unassign(&mut self, var: Var) {
        self.values.remove(&var);
    }

    pub fn get(&self, var: Var) -> Option<bool> {
        self.values.get(&var).copied()
    }

    pub fn is_assigned(&self, var: Var) -> bool {
        self.values.contains_key(&var)
    }

    pub fn eval_literal(&self, lit: Lit) -> Option<bool> {
        self.get(lit.var()).map(|v| if lit.sign() { v } else { !v })
    }

    pub fn assigned_vars(&self) -> impl Iterator<Item = Var> + '_ {
        self.values.keys().copied()
    }

    pub fn to_vec(&self) -> Vec<(Var, bool)> {
        let mut result: Vec<_> = self.values.iter().map(|(&v, &b)| (v, b)).collect();
        result.sort_by_key(|(v, _)| *v);
        result
    }
}

/// Result of SAT solving
#[derive(Debug, Clone)]
pub enum SatResult {
    /// Satisfiable with the given assignment
    Sat(Assignment),
    /// Unsatisfiable
    Unsat,
    /// Unknown (resource limit reached)
    Unknown(String),
}

/// DPLL SAT Solver configuration
#[derive(Debug, Clone)]
pub struct DpllConfig {
    /// Maximum number of decisions
    pub max_decisions: usize,
    /// Use pure literal elimination
    pub pure_literal_elimination: bool,
    /// Verbose output
    pub verbose: bool,
}

impl Default for DpllConfig {
    fn default() -> Self {
        DpllConfig {
            max_decisions: 1_000_000,
            pure_literal_elimination: true,
            verbose: false,
        }
    }
}

/// DPLL SAT Solver
pub struct DpllSolver {
    /// Original clauses
    clauses: Vec<SatClause>,
    /// Number of variables
    num_vars: Var,
    /// Current assignment
    assignment: Assignment,
    /// Configuration
    config: DpllConfig,
    /// Decision count
    decisions: usize,
    /// Propagation count
    propagations: usize,
}

impl DpllSolver {
    pub fn new() -> Self {
        Self::with_config(DpllConfig::default())
    }

    pub fn with_config(config: DpllConfig) -> Self {
        DpllSolver {
            clauses: Vec::new(),
            num_vars: 0,
            assignment: Assignment::new(),
            config,
            decisions: 0,
            propagations: 0,
        }
    }

    /// Add a clause to the solver
    pub fn add_clause(&mut self, clause: SatClause) {
        // Track maximum variable
        for lit in &clause.literals {
            if lit.var() > self.num_vars {
                self.num_vars = lit.var();
            }
        }
        self.clauses.push(clause);
    }

    /// Parse DIMACS CNF format
    pub fn parse_dimacs(&mut self, input: &str) -> Result<(), String> {
        for line in input.lines() {
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('c') {
                continue;
            }

            // Problem line
            if line.starts_with("p cnf") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 4 {
                    self.num_vars = parts[2].parse().map_err(|_| "Invalid num vars")?;
                }
                continue;
            }

            // Clause line
            if let Some(clause) = SatClause::from_dimacs(line) {
                self.add_clause(clause);
            }
        }

        Ok(())
    }

    /// Solve the SAT problem
    pub fn solve(&mut self) -> SatResult {
        self.assignment = Assignment::new();
        self.decisions = 0;
        self.propagations = 0;

        match self.dpll() {
            Some(true) => SatResult::Sat(self.assignment.clone()),
            Some(false) => SatResult::Unsat,
            None => SatResult::Unknown(format!(
                "Resource limit: {} decisions",
                self.decisions
            )),
        }
    }

    /// Core DPLL algorithm
    fn dpll(&mut self) -> Option<bool> {
        // Check resource limits
        if self.decisions > self.config.max_decisions {
            return None;
        }

        // Unit propagation
        if !self.unit_propagate() {
            return Some(false); // Conflict
        }

        // Pure literal elimination
        if self.config.pure_literal_elimination {
            self.eliminate_pure_literals();
        }

        // Check if all clauses satisfied
        if self.all_clauses_satisfied() {
            return Some(true);
        }

        // Check for empty clause (conflict)
        if self.has_empty_clause() {
            return Some(false);
        }

        // Choose a variable to branch on
        let var = match self.choose_variable() {
            Some(v) => v,
            None => return Some(true), // All variables assigned
        };

        self.decisions += 1;

        if self.config.verbose {
            eprintln!("Decision {}: trying {} = true", self.decisions, var);
        }

        // Try assigning true
        self.assignment.assign(var, true);
        if self.dpll() == Some(true) {
            return Some(true);
        }

        // Backtrack and try false
        self.assignment.assign(var, false);
        if self.dpll() == Some(true) {
            return Some(true);
        }

        // Backtrack completely
        self.assignment.unassign(var);
        Some(false)
    }

    /// Unit propagation: repeatedly assign unit clauses
    fn unit_propagate(&mut self) -> bool {
        loop {
            let mut found_unit = false;

            for clause in &self.clauses {
                // Skip satisfied clauses
                if self.is_clause_satisfied(clause) {
                    continue;
                }

                // Find unassigned literals
                let unassigned: Vec<Lit> = clause
                    .literals
                    .iter()
                    .filter(|l| self.assignment.eval_literal(**l).is_none())
                    .copied()
                    .collect();

                // Check for conflict (all literals false)
                let all_false = clause
                    .literals
                    .iter()
                    .all(|l| self.assignment.eval_literal(*l) == Some(false));

                if all_false {
                    return false; // Conflict
                }

                // Unit clause: exactly one unassigned literal, rest are false
                if unassigned.len() == 1 {
                    let unit_lit = unassigned[0];
                    let other_false = clause
                        .literals
                        .iter()
                        .filter(|l| **l != unit_lit)
                        .all(|l| self.assignment.eval_literal(*l) == Some(false));

                    if other_false {
                        self.propagations += 1;
                        if self.config.verbose {
                            eprintln!("Unit propagation: {} = {}", unit_lit.var(), unit_lit.sign());
                        }
                        self.assignment.assign(unit_lit.var(), unit_lit.sign());
                        found_unit = true;
                    }
                }
            }

            if !found_unit {
                break;
            }
        }

        true // No conflict
    }

    /// Pure literal elimination
    fn eliminate_pure_literals(&mut self) {
        let mut positive: HashSet<Var> = HashSet::new();
        let mut negative: HashSet<Var> = HashSet::new();

        for clause in &self.clauses {
            if self.is_clause_satisfied(clause) {
                continue;
            }

            for lit in &clause.literals {
                if self.assignment.is_assigned(lit.var()) {
                    continue;
                }

                if lit.sign() {
                    positive.insert(lit.var());
                } else {
                    negative.insert(lit.var());
                }
            }
        }

        // Pure literals appear only positive or only negative
        for var in 1..=self.num_vars {
            if self.assignment.is_assigned(var) {
                continue;
            }

            let is_positive = positive.contains(&var);
            let is_negative = negative.contains(&var);

            if is_positive && !is_negative {
                if self.config.verbose {
                    eprintln!("Pure literal: {} = true", var);
                }
                self.assignment.assign(var, true);
            } else if is_negative && !is_positive {
                if self.config.verbose {
                    eprintln!("Pure literal: {} = false", var);
                }
                self.assignment.assign(var, false);
            }
        }
    }

    /// Choose next variable to branch on (simple heuristic)
    fn choose_variable(&self) -> Option<Var> {
        // DLIS-like heuristic: choose variable appearing most in unsatisfied clauses
        let mut counts: HashMap<Var, usize> = HashMap::new();

        for clause in &self.clauses {
            if self.is_clause_satisfied(clause) {
                continue;
            }

            for lit in &clause.literals {
                if !self.assignment.is_assigned(lit.var()) {
                    *counts.entry(lit.var()).or_insert(0) += 1;
                }
            }
        }

        counts.into_iter().max_by_key(|(_, c)| *c).map(|(v, _)| v)
    }

    /// Check if a clause is satisfied
    fn is_clause_satisfied(&self, clause: &SatClause) -> bool {
        clause.literals.iter().any(|l| self.assignment.eval_literal(*l) == Some(true))
    }

    /// Check if all clauses are satisfied
    fn all_clauses_satisfied(&self) -> bool {
        self.clauses.iter().all(|c| self.is_clause_satisfied(c))
    }

    /// Check if any clause is empty (conflict)
    fn has_empty_clause(&self) -> bool {
        self.clauses.iter().any(|clause| {
            clause.literals.iter().all(|l| self.assignment.eval_literal(*l) == Some(false))
        })
    }

    /// Get statistics
    pub fn stats(&self) -> (usize, usize) {
        (self.decisions, self.propagations)
    }
}

impl Default for DpllSolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_sat() {
        let mut solver = DpllSolver::new();
        // (x1 ∨ x2) ∧ (¬x1 ∨ x2) ∧ (x1 ∨ ¬x2)
        solver.add_clause(SatClause::new(vec![Lit::positive(1), Lit::positive(2)]));
        solver.add_clause(SatClause::new(vec![Lit::negative(1), Lit::positive(2)]));
        solver.add_clause(SatClause::new(vec![Lit::positive(1), Lit::negative(2)]));

        match solver.solve() {
            SatResult::Sat(assignment) => {
                // x2 must be true
                assert_eq!(assignment.get(2), Some(true));
            }
            _ => panic!("Should be satisfiable"),
        }
    }

    #[test]
    fn test_simple_unsat() {
        let mut solver = DpllSolver::new();
        // (x1) ∧ (¬x1)
        solver.add_clause(SatClause::new(vec![Lit::positive(1)]));
        solver.add_clause(SatClause::new(vec![Lit::negative(1)]));

        match solver.solve() {
            SatResult::Unsat => {}
            _ => panic!("Should be unsatisfiable"),
        }
    }

    #[test]
    fn test_dimacs_parse() {
        let mut solver = DpllSolver::new();
        let dimacs = r#"
c This is a comment
p cnf 3 2
1 -2 3 0
-1 2 0
"#;
        solver.parse_dimacs(dimacs).unwrap();
        assert!(matches!(solver.solve(), SatResult::Sat(_)));
    }
}
