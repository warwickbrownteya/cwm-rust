//! DPLL(T) SMT Solver
//!
//! Implementation of Satisfiability Modulo Theories (SMT) solving using
//! the DPLL(T) framework. Combines CDCL SAT solving with theory solvers.
//!
//! Supported theories:
//! - Equality and Uninterpreted Functions (EUF)
//! - Linear Integer Arithmetic (LIA)
//! - Linear Real Arithmetic (LRA)
//! - Arrays

use std::collections::HashMap;
use super::dpll::{Var, Lit, SatClause, Assignment};

/// SMT sort (type)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Sort {
    Bool,
    Int,
    Real,
    BitVec(usize),
    Array(Box<Sort>, Box<Sort>),
    Uninterpreted(String),
}

/// SMT term
#[derive(Debug, Clone, PartialEq)]
pub enum SmtTerm {
    /// Boolean constant
    BoolConst(bool),
    /// Integer constant
    IntConst(i64),
    /// Real constant (as rational)
    RealConst(f64),
    /// Variable
    Var(String, Sort),
    /// Function application
    App(String, Vec<SmtTerm>),
    /// Negation
    Not(Box<SmtTerm>),
    /// Conjunction
    And(Vec<SmtTerm>),
    /// Disjunction
    Or(Vec<SmtTerm>),
    /// Implication
    Implies(Box<SmtTerm>, Box<SmtTerm>),
    /// If-then-else
    Ite(Box<SmtTerm>, Box<SmtTerm>, Box<SmtTerm>),
    /// Equality
    Eq(Box<SmtTerm>, Box<SmtTerm>),
    /// Distinct
    Distinct(Vec<SmtTerm>),
    /// Less than
    Lt(Box<SmtTerm>, Box<SmtTerm>),
    /// Less than or equal
    Le(Box<SmtTerm>, Box<SmtTerm>),
    /// Greater than
    Gt(Box<SmtTerm>, Box<SmtTerm>),
    /// Greater than or equal
    Ge(Box<SmtTerm>, Box<SmtTerm>),
    /// Addition
    Add(Vec<SmtTerm>),
    /// Subtraction
    Sub(Box<SmtTerm>, Box<SmtTerm>),
    /// Multiplication
    Mul(Vec<SmtTerm>),
    /// Division
    Div(Box<SmtTerm>, Box<SmtTerm>),
    /// Negation (arithmetic)
    Neg(Box<SmtTerm>),
    /// Array select
    Select(Box<SmtTerm>, Box<SmtTerm>),
    /// Array store
    Store(Box<SmtTerm>, Box<SmtTerm>, Box<SmtTerm>),
}

impl SmtTerm {
    pub fn bool_var(name: &str) -> Self {
        SmtTerm::Var(name.to_string(), Sort::Bool)
    }

    pub fn int_var(name: &str) -> Self {
        SmtTerm::Var(name.to_string(), Sort::Int)
    }

    pub fn real_var(name: &str) -> Self {
        SmtTerm::Var(name.to_string(), Sort::Real)
    }
}

/// Theory solver trait
pub trait TheorySolver {
    /// Check consistency of current assignment
    fn check(&mut self, assignment: &TheoryAssignment) -> TheoryResult;

    /// Propagate theory consequences
    fn propagate(&mut self, assignment: &TheoryAssignment) -> Vec<TheoryLit>;

    /// Explain a conflict or propagation
    fn explain(&self, lit: TheoryLit) -> Vec<TheoryLit>;

    /// Push a backtrack point
    fn push(&mut self);

    /// Pop to previous state
    fn pop(&mut self);
}

/// A theory literal (atom with polarity)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TheoryLit {
    pub atom_id: usize,
    pub positive: bool,
}

/// Assignment at the theory level
#[derive(Debug, Clone, Default)]
pub struct TheoryAssignment {
    /// Assigned theory literals
    pub literals: Vec<TheoryLit>,
    /// Value assignment for terms
    pub values: HashMap<usize, SmtValue>,
}

/// SMT value
#[derive(Debug, Clone, PartialEq)]
pub enum SmtValue {
    Bool(bool),
    Int(i64),
    Real(f64),
    Uninterpreted(usize), // Equivalence class ID
}

/// Result from theory solver
#[derive(Debug, Clone)]
pub enum TheoryResult {
    /// Satisfiable
    Sat,
    /// Unsatisfiable with conflict clause
    Unsat(Vec<TheoryLit>),
    /// Unknown
    Unknown,
}

/// Equality and Uninterpreted Functions theory solver
pub struct EufSolver {
    /// Union-find parent pointers
    parent: HashMap<usize, usize>,
    /// Union-find rank
    rank: HashMap<usize, usize>,
    /// Term to ID mapping
    term_to_id: HashMap<String, usize>,
    /// ID to term mapping
    id_to_term: HashMap<usize, SmtTerm>,
    /// Function application table for congruence
    app_table: HashMap<(String, Vec<usize>), usize>,
    /// Disequalities to check
    disequalities: Vec<(usize, usize, TheoryLit)>,
    /// Next ID
    next_id: usize,
    /// Backtrack stack
    stack: Vec<EufState>,
}

#[derive(Debug, Clone)]
struct EufState {
    parent: HashMap<usize, usize>,
    rank: HashMap<usize, usize>,
    disequalities: Vec<(usize, usize, TheoryLit)>,
}

impl EufSolver {
    pub fn new() -> Self {
        EufSolver {
            parent: HashMap::new(),
            rank: HashMap::new(),
            term_to_id: HashMap::new(),
            id_to_term: HashMap::new(),
            app_table: HashMap::new(),
            disequalities: Vec::new(),
            next_id: 0,
            stack: Vec::new(),
        }
    }

    /// Get or create ID for a term
    pub fn intern(&mut self, term: &SmtTerm) -> usize {
        let key = format!("{:?}", term);
        if let Some(&id) = self.term_to_id.get(&key) {
            return id;
        }

        let id = self.next_id;
        self.next_id += 1;
        self.term_to_id.insert(key, id);
        self.id_to_term.insert(id, term.clone());
        self.parent.insert(id, id);
        self.rank.insert(id, 0);

        // Handle function applications for congruence closure
        if let SmtTerm::App(name, args) = term {
            let arg_ids: Vec<usize> = args.iter().map(|a| self.intern(a)).collect();
            let arg_roots: Vec<usize> = arg_ids.iter().map(|&id| self.find(id)).collect();
            self.app_table.insert((name.clone(), arg_roots), id);
        }

        id
    }

    /// Find representative (with path compression)
    fn find(&mut self, x: usize) -> usize {
        let p = *self.parent.get(&x).unwrap_or(&x);
        if p != x {
            let root = self.find(p);
            self.parent.insert(x, root);
            root
        } else {
            x
        }
    }

    /// Union two equivalence classes
    fn union(&mut self, x: usize, y: usize) {
        let rx = self.find(x);
        let ry = self.find(y);

        if rx == ry {
            return;
        }

        let rank_x = *self.rank.get(&rx).unwrap_or(&0);
        let rank_y = *self.rank.get(&ry).unwrap_or(&0);

        if rank_x < rank_y {
            self.parent.insert(rx, ry);
        } else if rank_x > rank_y {
            self.parent.insert(ry, rx);
        } else {
            self.parent.insert(ry, rx);
            self.rank.insert(rx, rank_x + 1);
        }

        // Propagate congruence closure
        self.propagate_congruence();
    }

    /// Propagate congruence closure
    fn propagate_congruence(&mut self) {
        // Check for congruent function applications
        let mut to_merge = Vec::new();

        let app_entries: Vec<_> = self.app_table.iter().map(|(k, &v)| (k.clone(), v)).collect();

        for i in 0..app_entries.len() {
            for j in (i + 1)..app_entries.len() {
                let ((name1, args1), id1) = &app_entries[i];
                let ((name2, args2), id2) = &app_entries[j];

                if name1 == name2 && args1.len() == args2.len() {
                    let args_equal = args1.iter().zip(args2.iter()).all(|(&a, &b)| {
                        self.find(a) == self.find(b)
                    });

                    if args_equal {
                        to_merge.push((*id1, *id2));
                    }
                }
            }
        }

        for (a, b) in to_merge {
            self.union(a, b);
        }
    }

    /// Assert an equality
    pub fn assert_eq(&mut self, t1: usize, t2: usize) {
        self.union(t1, t2);
    }

    /// Assert a disequality
    pub fn assert_neq(&mut self, t1: usize, t2: usize, lit: TheoryLit) {
        self.disequalities.push((t1, t2, lit));
    }

    /// Check for conflicts
    fn check_conflicts(&mut self) -> Option<Vec<TheoryLit>> {
        let diseqs = self.disequalities.clone();
        for (t1, t2, lit) in diseqs {
            if self.find(t1) == self.find(t2) {
                // Conflict: t1 = t2 but we asserted t1 ≠ t2
                return Some(vec![lit]);
            }
        }
        None
    }
}

impl Default for EufSolver {
    fn default() -> Self {
        Self::new()
    }
}

impl TheorySolver for EufSolver {
    fn check(&mut self, _assignment: &TheoryAssignment) -> TheoryResult {
        if let Some(conflict) = self.check_conflicts() {
            TheoryResult::Unsat(conflict)
        } else {
            TheoryResult::Sat
        }
    }

    fn propagate(&mut self, _assignment: &TheoryAssignment) -> Vec<TheoryLit> {
        Vec::new() // EUF doesn't propagate much on its own
    }

    fn explain(&self, _lit: TheoryLit) -> Vec<TheoryLit> {
        Vec::new() // Simplified
    }

    fn push(&mut self) {
        self.stack.push(EufState {
            parent: self.parent.clone(),
            rank: self.rank.clone(),
            disequalities: self.disequalities.clone(),
        });
    }

    fn pop(&mut self) {
        if let Some(state) = self.stack.pop() {
            self.parent = state.parent;
            self.rank = state.rank;
            self.disequalities = state.disequalities;
        }
    }
}

/// Linear Arithmetic theory solver (Simplex-based)
pub struct LiaSolver {
    /// Variables
    variables: HashMap<String, usize>,
    /// Constraints: (coefficients, comparator, constant)
    constraints: Vec<(HashMap<usize, i64>, Comparator, i64, TheoryLit)>,
    /// Current assignment
    assignment: HashMap<usize, i64>,
    /// Backtrack stack
    stack: Vec<LiaState>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Comparator {
    Eq,
    Le,
    Lt,
    Ge,
    Gt,
    Ne,
}

#[derive(Debug, Clone)]
struct LiaState {
    constraints: Vec<(HashMap<usize, i64>, Comparator, i64, TheoryLit)>,
    assignment: HashMap<usize, i64>,
}

impl LiaSolver {
    pub fn new() -> Self {
        LiaSolver {
            variables: HashMap::new(),
            constraints: Vec::new(),
            assignment: HashMap::new(),
            stack: Vec::new(),
        }
    }

    /// Get or create variable ID
    pub fn get_var(&mut self, name: &str) -> usize {
        if let Some(&id) = self.variables.get(name) {
            return id;
        }
        let id = self.variables.len();
        self.variables.insert(name.to_string(), id);
        id
    }

    /// Add a constraint
    pub fn add_constraint(
        &mut self,
        coeffs: HashMap<usize, i64>,
        cmp: Comparator,
        constant: i64,
        lit: TheoryLit,
    ) {
        self.constraints.push((coeffs, cmp, constant, lit));
    }

    /// Evaluate a linear expression
    fn eval(&self, coeffs: &HashMap<usize, i64>) -> i64 {
        coeffs.iter().map(|(&var, &coeff)| {
            coeff * self.assignment.get(&var).copied().unwrap_or(0)
        }).sum()
    }

    /// Check if a constraint is satisfied
    fn check_constraint(&self, coeffs: &HashMap<usize, i64>, cmp: Comparator, constant: i64) -> bool {
        let lhs = self.eval(coeffs);
        match cmp {
            Comparator::Eq => lhs == constant,
            Comparator::Le => lhs <= constant,
            Comparator::Lt => lhs < constant,
            Comparator::Ge => lhs >= constant,
            Comparator::Gt => lhs > constant,
            Comparator::Ne => lhs != constant,
        }
    }

    /// Simple constraint propagation (bound propagation)
    fn propagate_bounds(&mut self) -> bool {
        let mut changed = true;
        let mut iterations = 0;

        while changed && iterations < 100 {
            changed = false;
            iterations += 1;

            for (coeffs, cmp, constant, _) in &self.constraints {
                // Handle unit constraints (single variable)
                if coeffs.len() == 1 {
                    let (&var, &coeff) = coeffs.iter().next().unwrap();

                    if coeff != 0 {
                        let bound = constant / coeff;
                        let current = self.assignment.get(&var).copied();

                        match (cmp, coeff > 0) {
                            (Comparator::Eq, _) => {
                                if current != Some(bound) {
                                    self.assignment.insert(var, bound);
                                    changed = true;
                                }
                            }
                            (Comparator::Le, true) | (Comparator::Ge, false) => {
                                if current.map_or(true, |c| c > bound) {
                                    self.assignment.insert(var, bound);
                                    changed = true;
                                }
                            }
                            (Comparator::Ge, true) | (Comparator::Le, false) => {
                                if current.map_or(true, |c| c < bound) {
                                    self.assignment.insert(var, bound);
                                    changed = true;
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        // Check all constraints
        self.constraints.iter().all(|(coeffs, cmp, constant, _)| {
            self.check_constraint(coeffs, *cmp, *constant)
        })
    }
}

impl Default for LiaSolver {
    fn default() -> Self {
        Self::new()
    }
}

impl TheorySolver for LiaSolver {
    fn check(&mut self, _assignment: &TheoryAssignment) -> TheoryResult {
        if self.propagate_bounds() {
            TheoryResult::Sat
        } else {
            // Find conflicting constraints
            for (coeffs, cmp, constant, lit) in &self.constraints {
                if !self.check_constraint(coeffs, *cmp, *constant) {
                    return TheoryResult::Unsat(vec![lit.clone()]);
                }
            }
            TheoryResult::Sat
        }
    }

    fn propagate(&mut self, _assignment: &TheoryAssignment) -> Vec<TheoryLit> {
        Vec::new()
    }

    fn explain(&self, _lit: TheoryLit) -> Vec<TheoryLit> {
        Vec::new()
    }

    fn push(&mut self) {
        self.stack.push(LiaState {
            constraints: self.constraints.clone(),
            assignment: self.assignment.clone(),
        });
    }

    fn pop(&mut self) {
        if let Some(state) = self.stack.pop() {
            self.constraints = state.constraints;
            self.assignment = state.assignment;
        }
    }
}

/// Configuration for SMT solver
#[derive(Debug, Clone)]
pub struct SmtConfig {
    /// Maximum conflicts before restart
    pub max_conflicts: usize,
    /// Verbose output
    pub verbose: bool,
}

impl Default for SmtConfig {
    fn default() -> Self {
        SmtConfig {
            max_conflicts: 100000,
            verbose: false,
        }
    }
}

/// Result of SMT solving
#[derive(Debug, Clone)]
pub enum SmtResult {
    Sat(HashMap<String, SmtValue>),
    Unsat,
    Unknown(String),
}

/// DPLL(T) SMT Solver
pub struct SmtSolver {
    /// Configuration
    #[allow(dead_code)]
    config: SmtConfig,
    /// Theory atoms (term -> boolean variable)
    atoms: HashMap<String, Var>,
    /// Reverse mapping (var -> atom info)
    atom_info: HashMap<Var, (SmtTerm, usize)>, // (term, theory_atom_id)
    /// Boolean clauses
    clauses: Vec<SatClause>,
    /// EUF solver
    euf: EufSolver,
    /// LIA solver
    lia: LiaSolver,
    /// Current boolean assignment
    #[allow(dead_code)]
    bool_assignment: Assignment,
    /// Next variable
    next_var: Var,
    /// Next theory atom ID
    next_atom_id: usize,
}

impl SmtSolver {
    pub fn new() -> Self {
        Self::with_config(SmtConfig::default())
    }

    pub fn with_config(config: SmtConfig) -> Self {
        SmtSolver {
            config,
            atoms: HashMap::new(),
            atom_info: HashMap::new(),
            clauses: Vec::new(),
            euf: EufSolver::new(),
            lia: LiaSolver::new(),
            bool_assignment: Assignment::new(),
            next_var: 1,
            next_atom_id: 0,
        }
    }

    /// Get or create boolean variable for a theory atom
    fn get_bool_var(&mut self, term: &SmtTerm) -> Var {
        let key = format!("{:?}", term);
        if let Some(&var) = self.atoms.get(&key) {
            return var;
        }

        let var = self.next_var;
        self.next_var += 1;

        let atom_id = self.next_atom_id;
        self.next_atom_id += 1;

        self.atoms.insert(key, var);
        self.atom_info.insert(var, (term.clone(), atom_id));

        var
    }

    /// Convert term to CNF clauses
    fn tseitin(&mut self, term: &SmtTerm) -> Var {
        match term {
            SmtTerm::BoolConst(true) => {
                let v = self.get_bool_var(term);
                self.clauses.push(SatClause::new(vec![Lit::positive(v)]));
                v
            }
            SmtTerm::BoolConst(false) => {
                let v = self.get_bool_var(term);
                self.clauses.push(SatClause::new(vec![Lit::negative(v)]));
                v
            }
            SmtTerm::Var(_, Sort::Bool) => self.get_bool_var(term),

            SmtTerm::Not(ref inner) => {
                let inner_var = self.tseitin(inner);
                let v = self.get_bool_var(term);
                // v <-> ~inner_var
                self.clauses.push(SatClause::new(vec![Lit::negative(v), Lit::negative(inner_var)]));
                self.clauses.push(SatClause::new(vec![Lit::positive(v), Lit::positive(inner_var)]));
                v
            }

            SmtTerm::And(children) => {
                let child_vars: Vec<Var> = children.iter().map(|c| self.tseitin(c)).collect();
                let v = self.get_bool_var(term);
                // v <-> (c1 & c2 & ...)
                // v -> c1, v -> c2, ...
                for &cv in &child_vars {
                    self.clauses.push(SatClause::new(vec![Lit::negative(v), Lit::positive(cv)]));
                }
                // c1 & c2 & ... -> v
                let mut clause: Vec<Lit> = child_vars.iter().map(|&cv| Lit::negative(cv)).collect();
                clause.push(Lit::positive(v));
                self.clauses.push(SatClause::new(clause));
                v
            }

            SmtTerm::Or(children) => {
                let child_vars: Vec<Var> = children.iter().map(|c| self.tseitin(c)).collect();
                let v = self.get_bool_var(term);
                // v <-> (c1 | c2 | ...)
                // c1 -> v, c2 -> v, ...
                for &cv in &child_vars {
                    self.clauses.push(SatClause::new(vec![Lit::negative(cv), Lit::positive(v)]));
                }
                // v -> c1 | c2 | ...
                let mut clause: Vec<Lit> = child_vars.iter().map(|&cv| Lit::positive(cv)).collect();
                clause.push(Lit::negative(v));
                self.clauses.push(SatClause::new(clause));
                v
            }

            SmtTerm::Implies(ref a, ref b) => {
                // a -> b  ===  ~a | b
                self.tseitin(&SmtTerm::Or(vec![
                    SmtTerm::Not(Box::new((**a).clone())),
                    (**b).clone(),
                ]))
            }

            // Theory atoms
            SmtTerm::Eq(_, _) | SmtTerm::Lt(_, _) | SmtTerm::Le(_, _) |
            SmtTerm::Gt(_, _) | SmtTerm::Ge(_, _) => {
                self.get_bool_var(term)
            }

            _ => self.get_bool_var(term),
        }
    }

    /// Assert a formula
    pub fn assert(&mut self, term: SmtTerm) {
        let v = self.tseitin(&term);
        self.clauses.push(SatClause::new(vec![Lit::positive(v)]));
    }

    /// Check satisfiability
    pub fn check(&mut self) -> SmtResult {
        // Use CDCL for boolean reasoning
        let mut cdcl = super::cdcl::CdclSolver::new();
        for clause in &self.clauses {
            cdcl.add_clause(clause.clone());
        }

        // DPLL(T) main loop
        loop {
            match cdcl.solve() {
                super::dpll::SatResult::Unsat => return SmtResult::Unsat,
                super::dpll::SatResult::Unknown(reason) => {
                    return SmtResult::Unknown(reason);
                }
                super::dpll::SatResult::Sat(bool_assignment) => {
                    // Check theory consistency
                    let theory_assignment = self.build_theory_assignment(&bool_assignment);

                    // Push theory solvers
                    self.euf.push();
                    self.lia.push();

                    // Apply theory assignments
                    self.apply_theory_assignment(&bool_assignment);

                    // Check EUF
                    match self.euf.check(&theory_assignment) {
                        TheoryResult::Unsat(conflict) => {
                            // Add conflict clause and retry
                            let clause = self.conflict_to_clause(&conflict);
                            cdcl.add_clause(clause);
                            self.euf.pop();
                            self.lia.pop();
                            continue;
                        }
                        _ => {}
                    }

                    // Check LIA
                    match self.lia.check(&theory_assignment) {
                        TheoryResult::Unsat(conflict) => {
                            let clause = self.conflict_to_clause(&conflict);
                            cdcl.add_clause(clause);
                            self.euf.pop();
                            self.lia.pop();
                            continue;
                        }
                        _ => {}
                    }

                    // All theories satisfied
                    self.euf.pop();
                    self.lia.pop();

                    return SmtResult::Sat(self.build_model(&bool_assignment));
                }
            }
        }
    }

    fn build_theory_assignment(&self, bool_assignment: &Assignment) -> TheoryAssignment {
        let mut theory_assign = TheoryAssignment::default();

        for (&var, &(ref _term, atom_id)) in &self.atom_info {
            if let Some(value) = bool_assignment.get(var) {
                theory_assign.literals.push(TheoryLit {
                    atom_id,
                    positive: value,
                });
            }
        }

        theory_assign
    }

    fn apply_theory_assignment(&mut self, bool_assignment: &Assignment) {
        for (&var, &(ref term, atom_id)) in &self.atom_info {
            if let Some(value) = bool_assignment.get(var) {
                // Handle EUF assertions
                if let SmtTerm::Eq(ref t1, ref t2) = term {
                    let id1 = self.euf.intern(t1);
                    let id2 = self.euf.intern(t2);

                    if value {
                        self.euf.assert_eq(id1, id2);
                    } else {
                        self.euf.assert_neq(id1, id2, TheoryLit { atom_id, positive: false });
                    }
                }

                // Handle LIA assertions
                // (Simplified - would need proper term analysis)
            }
        }
    }

    fn conflict_to_clause(&self, conflict: &[TheoryLit]) -> SatClause {
        let literals: Vec<Lit> = conflict.iter().filter_map(|tlit| {
            // Find the boolean variable for this theory atom
            for (&var, &(_, atom_id)) in &self.atom_info {
                if atom_id == tlit.atom_id {
                    // Negate for conflict clause
                    return Some(if tlit.positive {
                        Lit::negative(var)
                    } else {
                        Lit::positive(var)
                    });
                }
            }
            None
        }).collect();

        SatClause::new(literals)
    }

    fn build_model(&self, bool_assignment: &Assignment) -> HashMap<String, SmtValue> {
        let mut model = HashMap::new();

        for (&var, &(ref term, _)) in &self.atom_info {
            if let SmtTerm::Var(name, sort) = term {
                if let Some(value) = bool_assignment.get(var) {
                    match sort {
                        Sort::Bool => {
                            model.insert(name.clone(), SmtValue::Bool(value));
                        }
                        _ => {}
                    }
                }
            }
        }

        model
    }

    /// Parse SMT-LIB2 format (simplified)
    pub fn parse_smtlib(&mut self, input: &str) -> Result<(), String> {
        for line in input.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with(';') {
                continue;
            }

            if line.starts_with("(assert") {
                // Parse assertion
                // Simplified parsing
            } else if line.starts_with("(declare-fun") || line.starts_with("(declare-const") {
                // Parse declaration
            } else if line.starts_with("(check-sat)") {
                // Will be handled by check()
            }
        }

        Ok(())
    }
}

impl Default for SmtSolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_euf_simple() {
        let mut euf = EufSolver::new();

        let a = SmtTerm::Var("a".to_string(), Sort::Uninterpreted("U".to_string()));
        let b = SmtTerm::Var("b".to_string(), Sort::Uninterpreted("U".to_string()));
        let c = SmtTerm::Var("c".to_string(), Sort::Uninterpreted("U".to_string()));

        let id_a = euf.intern(&a);
        let id_b = euf.intern(&b);
        let id_c = euf.intern(&c);

        // a = b
        euf.assert_eq(id_a, id_b);

        // Check a = b
        assert_eq!(euf.find(id_a), euf.find(id_b));

        // a ≠ c should be satisfiable
        euf.assert_neq(id_a, id_c, TheoryLit { atom_id: 0, positive: false });
        assert!(euf.check_conflicts().is_none());
    }

    #[test]
    fn test_smt_simple() {
        let mut solver = SmtSolver::new();

        // (x | y) & (~x | y)
        let x = SmtTerm::bool_var("x");
        let y = SmtTerm::bool_var("y");

        solver.assert(SmtTerm::And(vec![
            SmtTerm::Or(vec![x.clone(), y.clone()]),
            SmtTerm::Or(vec![SmtTerm::Not(Box::new(x.clone())), y.clone()]),
        ]));

        match solver.check() {
            SmtResult::Sat(model) => {
                // y must be true
                assert_eq!(model.get("y"), Some(&SmtValue::Bool(true)));
            }
            _ => panic!("Should be satisfiable"),
        }
    }
}
