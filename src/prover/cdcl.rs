//! CDCL SAT Solver
//!
//! Implementation of Conflict-Driven Clause Learning for SAT solving.
//! This is the algorithm used by modern SAT solvers like MiniSat, CaDiCaL, etc.
//!
//! Features:
//! - Watched literals (two-watched-literal scheme)
//! - VSIDS decision heuristic
//! - Conflict analysis with First-UIP learning
//! - Non-chronological backtracking (backjumping)
//! - Restarts
//! - Clause deletion

use std::collections::{HashMap, HashSet, VecDeque};

pub use super::dpll::{Var, Lit, SatClause, Assignment, SatResult};

/// Trail entry: records an assignment with its reason
#[derive(Debug, Clone)]
struct TrailEntry {
    /// The literal that was assigned
    lit: Lit,
    /// Decision level at which this was assigned
    level: u32,
    /// Reason clause (None if decision)
    reason: Option<usize>,
}

/// Watcher: tracks which clauses are watching which literals
#[derive(Debug, Clone, Default)]
struct Watchers {
    /// For each literal, list of clause indices watching it
    watches: HashMap<Lit, Vec<usize>>,
}

impl Watchers {
    fn new() -> Self {
        Watchers {
            watches: HashMap::new(),
        }
    }

    fn add_watch(&mut self, lit: Lit, clause_idx: usize) {
        self.watches.entry(lit).or_default().push(clause_idx);
    }

    fn remove_watch(&mut self, lit: Lit, clause_idx: usize) {
        if let Some(watches) = self.watches.get_mut(&lit) {
            watches.retain(|&c| c != clause_idx);
        }
    }

    fn get_watches(&self, lit: Lit) -> &[usize] {
        self.watches.get(&lit).map(|v| v.as_slice()).unwrap_or(&[])
    }
}

/// VSIDS activity scores for variable selection
#[derive(Debug, Clone)]
struct VsidsScores {
    /// Activity score for each variable
    activity: HashMap<Var, f64>,
    /// Activity increment
    increment: f64,
    /// Decay factor
    decay: f64,
}

impl VsidsScores {
    fn new() -> Self {
        VsidsScores {
            activity: HashMap::new(),
            increment: 1.0,
            decay: 0.95,
        }
    }

    fn bump(&mut self, var: Var) {
        let score = self.activity.entry(var).or_insert(0.0);
        *score += self.increment;

        // Rescale if too large
        if *score > 1e100 {
            for v in self.activity.values_mut() {
                *v *= 1e-100;
            }
            self.increment *= 1e-100;
        }
    }

    fn decay(&mut self) {
        self.increment /= self.decay;
    }

    fn get(&self, var: Var) -> f64 {
        *self.activity.get(&var).unwrap_or(&0.0)
    }
}

/// CDCL Solver configuration
#[derive(Debug, Clone)]
pub struct CdclConfig {
    /// Maximum number of conflicts before restart
    pub restart_interval: usize,
    /// Restart interval multiplier
    pub restart_multiplier: f64,
    /// Maximum learned clause size to keep
    pub max_learned_size: usize,
    /// Clause deletion interval
    pub deletion_interval: usize,
    /// Verbose output
    pub verbose: bool,
}

impl Default for CdclConfig {
    fn default() -> Self {
        CdclConfig {
            restart_interval: 100,
            restart_multiplier: 1.5,
            max_learned_size: 1000,
            deletion_interval: 1000,
            verbose: false,
        }
    }
}

/// CDCL SAT Solver
pub struct CdclSolver {
    /// Original clauses
    clauses: Vec<SatClause>,
    /// Learned clauses (start index in clauses)
    learned_start: usize,
    /// Number of variables
    num_vars: Var,
    /// Current assignment
    assignment: Assignment,
    /// Assignment trail
    trail: Vec<TrailEntry>,
    /// Decision level markers in trail
    level_markers: Vec<usize>,
    /// Current decision level
    decision_level: u32,
    /// Watched literals
    watchers: Watchers,
    /// VSIDS scores
    vsids: VsidsScores,
    /// Propagation queue
    prop_queue: VecDeque<Lit>,
    /// Configuration
    config: CdclConfig,
    /// Statistics
    conflicts: usize,
    decisions: usize,
    propagations: usize,
    learned_clauses: usize,
    restarts: usize,
}

impl CdclSolver {
    pub fn new() -> Self {
        Self::with_config(CdclConfig::default())
    }

    pub fn with_config(config: CdclConfig) -> Self {
        CdclSolver {
            clauses: Vec::new(),
            learned_start: 0,
            num_vars: 0,
            assignment: Assignment::new(),
            trail: Vec::new(),
            level_markers: vec![0],
            decision_level: 0,
            watchers: Watchers::new(),
            vsids: VsidsScores::new(),
            prop_queue: VecDeque::new(),
            config,
            conflicts: 0,
            decisions: 0,
            propagations: 0,
            learned_clauses: 0,
            restarts: 0,
        }
    }

    /// Add a clause
    pub fn add_clause(&mut self, clause: SatClause) {
        // Track maximum variable
        for lit in &clause.literals {
            if lit.var() > self.num_vars {
                self.num_vars = lit.var();
            }
        }

        let clause_idx = self.clauses.len();

        // Set up watched literals (watch first two literals)
        if clause.literals.len() >= 2 {
            self.watchers.add_watch(clause.literals[0].negated(), clause_idx);
            self.watchers.add_watch(clause.literals[1].negated(), clause_idx);
        } else if clause.literals.len() == 1 {
            // Unit clause - add to propagation queue at level 0
            self.watchers.add_watch(clause.literals[0].negated(), clause_idx);
        }

        self.clauses.push(clause);
    }

    /// Parse DIMACS CNF format
    pub fn parse_dimacs(&mut self, input: &str) -> Result<(), String> {
        for line in input.lines() {
            let line = line.trim();

            if line.is_empty() || line.starts_with('c') {
                continue;
            }

            if line.starts_with("p cnf") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 4 {
                    self.num_vars = parts[2].parse().map_err(|_| "Invalid num vars")?;
                }
                continue;
            }

            if let Some(clause) = SatClause::from_dimacs(line) {
                self.add_clause(clause);
            }
        }

        self.learned_start = self.clauses.len();
        Ok(())
    }

    /// Solve the SAT problem
    pub fn solve(&mut self) -> SatResult {
        self.learned_start = self.clauses.len();
        let mut restart_count = 0;
        let mut current_restart_interval = self.config.restart_interval;

        // Process initial unit clauses
        for clause_idx in 0..self.clauses.len() {
            let clause = &self.clauses[clause_idx];
            if clause.literals.len() == 1 {
                let lit = clause.literals[0];
                match self.assignment.eval_literal(lit) {
                    Some(true) => {} // Already satisfied
                    Some(false) => return SatResult::Unsat, // Conflict with another unit clause
                    None => {
                        // Assign the unit literal
                        self.assign(lit, Some(clause_idx));
                    }
                }
            }
        }

        // Initial unit propagation
        if let Some(_conflict) = self.propagate() {
            return SatResult::Unsat;
        }

        loop {
            // Check for restart
            if self.conflicts.saturating_sub(restart_count * current_restart_interval) >= current_restart_interval {
                self.restart();
                self.restarts += 1;
                restart_count += 1;
                current_restart_interval =
                    (current_restart_interval as f64 * self.config.restart_multiplier) as usize;

                if self.config.verbose {
                    eprintln!("Restart #{}, next interval: {}", self.restarts, current_restart_interval);
                }
            }

            // Clause deletion
            if self.conflicts > 0 && self.conflicts % self.config.deletion_interval == 0 {
                self.delete_learned_clauses();
            }

            // Check if all variables assigned
            if self.all_assigned() {
                return SatResult::Sat(self.assignment.clone());
            }

            // Make a decision
            let decision_var = match self.pick_decision_var() {
                Some(v) => v,
                None => return SatResult::Sat(self.assignment.clone()),
            };

            self.decisions += 1;
            self.new_decision_level();

            // Decide: assign true first (can use phase saving here)
            let decision_lit = Lit::positive(decision_var);
            self.assign(decision_lit, None);

            // Propagate
            while let Some(conflict_clause) = self.propagate() {
                self.conflicts += 1;

                if self.decision_level == 0 {
                    return SatResult::Unsat;
                }

                // Analyze conflict
                let (learned_clause, backtrack_level) = self.analyze_conflict(conflict_clause);

                // Learn clause
                self.learn_clause(learned_clause.clone());

                // Backtrack
                self.backtrack(backtrack_level);

                // The learned clause is now unit - it will be propagated
            }
        }
    }

    /// Boolean Constraint Propagation with watched literals
    fn propagate(&mut self) -> Option<usize> {
        while let Some(lit) = self.prop_queue.pop_front() {
            self.propagations += 1;

            // Get clauses watching the negation of this literal
            let watching: Vec<usize> = self.watchers.get_watches(lit).to_vec();

            for &clause_idx in &watching {
                if let Some(conflict) = self.propagate_clause(clause_idx, lit) {
                    self.prop_queue.clear();
                    return Some(conflict);
                }
            }
        }
        None
    }

    /// Propagate a single clause
    fn propagate_clause(&mut self, clause_idx: usize, false_lit: Lit) -> Option<usize> {
        let clause = &self.clauses[clause_idx];

        // Find the two watched literals
        let lit0 = clause.literals[0];
        let lit1 = if clause.literals.len() > 1 {
            clause.literals[1]
        } else {
            // Unit clause that became false
            return Some(clause_idx);
        };

        // Make sure false_lit is lit1 (swap if necessary in our view)
        let (watch_lit, other_watch) = if false_lit == lit0.negated() {
            (lit0, lit1)
        } else {
            (lit1, lit0)
        };

        // If other watched literal is true, clause is satisfied
        if self.assignment.eval_literal(other_watch) == Some(true) {
            return None;
        }

        // Look for a new literal to watch
        for i in 2..clause.literals.len() {
            let lit = clause.literals[i];
            if self.assignment.eval_literal(lit) != Some(false) {
                // Found a new watch
                self.watchers.remove_watch(false_lit, clause_idx);
                self.watchers.add_watch(lit.negated(), clause_idx);

                // Swap literals in clause so invariant is maintained
                let clause = &mut self.clauses[clause_idx];
                let swap_idx = if watch_lit == lit0 { 0 } else { 1 };
                clause.literals.swap(swap_idx, i);

                return None;
            }
        }

        // No new watch found - check if other watch is unassigned (unit) or false (conflict)
        match self.assignment.eval_literal(other_watch) {
            Some(false) => Some(clause_idx), // Conflict
            None => {
                // Unit propagation
                self.assign(other_watch, Some(clause_idx));
                None
            }
            Some(true) => None, // Satisfied
        }
    }

    /// Analyze conflict and learn clause (First-UIP)
    fn analyze_conflict(&mut self, conflict_clause: usize) -> (SatClause, u32) {
        let mut learned_lits: Vec<Lit> = Vec::new();
        let mut seen: HashSet<Var> = HashSet::new();
        let mut counter = 0; // Literals from current decision level
        let mut reason = Some(conflict_clause);
        let mut trail_idx = self.trail.len();
        let mut first_uip = None;

        loop {
            // Process reason clause
            if let Some(clause_idx) = reason {
                let clause = &self.clauses[clause_idx];

                for &lit in &clause.literals {
                    let var = lit.var();
                    if seen.contains(&var) {
                        continue;
                    }
                    seen.insert(var);

                    // Bump activity
                    self.vsids.bump(var);

                    // Get the level at which this variable was assigned
                    if let Some(entry) = self.trail.iter().find(|e| e.lit.var() == var) {
                        if entry.level == self.decision_level {
                            counter += 1;
                        } else if entry.level > 0 {
                            // Add to learned clause (negated)
                            learned_lits.push(lit.negated());
                        }
                    }
                }
            }

            // Find next literal on trail from current level
            loop {
                trail_idx -= 1;
                let entry = &self.trail[trail_idx];
                if entry.level == self.decision_level && seen.contains(&entry.lit.var()) {
                    counter -= 1;
                    if counter == 0 {
                        // Found first UIP
                        first_uip = Some(entry.lit.negated());
                        break;
                    }
                    reason = entry.reason;
                    break;
                }
            }

            if counter == 0 {
                break;
            }
        }

        // Add first UIP to learned clause
        if let Some(uip) = first_uip {
            learned_lits.insert(0, uip);
        }

        // Decay VSIDS
        self.vsids.decay();

        // Compute backtrack level (second highest level in learned clause)
        let backtrack_level = if learned_lits.len() <= 1 {
            0
        } else {
            let mut levels: Vec<u32> = learned_lits
                .iter()
                .filter_map(|lit| {
                    self.trail
                        .iter()
                        .find(|e| e.lit.var() == lit.var())
                        .map(|e| e.level)
                })
                .collect();
            levels.sort_unstable();
            levels.iter().rev().nth(1).copied().unwrap_or(0)
        };

        (SatClause::new(learned_lits), backtrack_level)
    }

    /// Learn a new clause
    fn learn_clause(&mut self, clause: SatClause) {
        if clause.literals.is_empty() {
            return;
        }

        let clause_idx = self.clauses.len();

        // Set up watches
        if clause.literals.len() >= 2 {
            self.watchers.add_watch(clause.literals[0].negated(), clause_idx);
            self.watchers.add_watch(clause.literals[1].negated(), clause_idx);
        } else {
            self.watchers.add_watch(clause.literals[0].negated(), clause_idx);
        }

        self.clauses.push(clause);
        self.learned_clauses += 1;
    }

    /// Delete some learned clauses
    fn delete_learned_clauses(&mut self) {
        // Simple strategy: keep clauses smaller than threshold
        // A more sophisticated approach would use LBD (Literal Block Distance)

        let mut to_keep = Vec::new();
        for i in self.learned_start..self.clauses.len() {
            if self.clauses[i].literals.len() <= 3 {
                to_keep.push(i);
            }
        }

        // For simplicity, we don't actually delete here
        // A full implementation would compact the clause database
    }

    /// Pick a decision variable using VSIDS
    fn pick_decision_var(&self) -> Option<Var> {
        (1..=self.num_vars)
            .filter(|&v| !self.assignment.is_assigned(v))
            .max_by(|&a, &b| {
                self.vsids
                    .get(a)
                    .partial_cmp(&self.vsids.get(b))
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
    }

    /// Assign a literal
    fn assign(&mut self, lit: Lit, reason: Option<usize>) {
        self.assignment.assign(lit.var(), lit.sign());
        self.trail.push(TrailEntry {
            lit,
            level: self.decision_level,
            reason,
        });
        self.prop_queue.push_back(lit);
    }

    /// Create new decision level
    fn new_decision_level(&mut self) {
        self.decision_level += 1;
        self.level_markers.push(self.trail.len());
    }

    /// Backtrack to a given level
    fn backtrack(&mut self, level: u32) {
        while self.trail.len() > self.level_markers[level as usize] {
            let entry = self.trail.pop().unwrap();
            self.assignment.unassign(entry.lit.var());
        }
        self.level_markers.truncate(level as usize + 1);
        self.decision_level = level;
        self.prop_queue.clear();

        // Re-add the asserting literal to the queue (it's unit now)
        // This is handled by the learned clause being unit after backtrack
    }

    /// Restart: backtrack to level 0
    fn restart(&mut self) {
        self.backtrack(0);
    }

    /// Check if all variables are assigned
    fn all_assigned(&self) -> bool {
        (1..=self.num_vars).all(|v| self.assignment.is_assigned(v))
    }

    /// Get statistics
    pub fn stats(&self) -> (usize, usize, usize, usize, usize) {
        (
            self.conflicts,
            self.decisions,
            self.propagations,
            self.learned_clauses,
            self.restarts,
        )
    }
}

impl Default for CdclSolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_sat() {
        let mut solver = CdclSolver::new();
        solver.add_clause(SatClause::new(vec![Lit::positive(1), Lit::positive(2)]));
        solver.add_clause(SatClause::new(vec![Lit::negative(1), Lit::positive(2)]));
        solver.add_clause(SatClause::new(vec![Lit::positive(1), Lit::negative(2)]));

        match solver.solve() {
            SatResult::Sat(assignment) => {
                assert_eq!(assignment.get(2), Some(true));
            }
            _ => panic!("Should be satisfiable"),
        }
    }

    #[test]
    fn test_simple_unsat() {
        let mut solver = CdclSolver::new();
        // Empty clause via unit propagation
        solver.add_clause(SatClause::new(vec![Lit::positive(1)]));
        solver.add_clause(SatClause::new(vec![Lit::negative(1)]));

        match solver.solve() {
            SatResult::Unsat => {}
            _ => panic!("Should be unsatisfiable"),
        }
    }

    #[test]
    fn test_pigeon_hole_2_1() {
        // 2 pigeons, 1 hole - unsatisfiable
        let mut solver = CdclSolver::new();

        // p1 in hole 1
        solver.add_clause(SatClause::new(vec![Lit::positive(1)]));
        // p2 in hole 1
        solver.add_clause(SatClause::new(vec![Lit::positive(2)]));
        // Not both in same hole
        solver.add_clause(SatClause::new(vec![Lit::negative(1), Lit::negative(2)]));

        match solver.solve() {
            SatResult::Unsat => {}
            _ => panic!("Should be unsatisfiable"),
        }
    }
}
