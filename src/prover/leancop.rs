//! leanCoP - Lean Connection Prover
//!
//! A compact implementation of the connection calculus for first-order
//! theorem proving. Based on the original leanCoP by Jens Otten.
//!
//! Features:
//! - Connection-based proof search
//! - Iterative deepening
//! - Start clause selection
//! - Extension and reduction steps

use std::collections::HashMap;
use super::term::{FolTerm, Variable, Atom, Function, Predicate};
use super::clause::{Literal, Clause};
use super::unify::{Substitution, unify_atoms};

/// Configuration for leanCoP
#[derive(Debug, Clone)]
pub struct LeanCopConfig {
    /// Maximum proof depth (iterative deepening limit)
    pub max_depth: usize,
    /// Maximum number of inferences
    pub max_inferences: usize,
    /// Use iterative deepening
    pub iterative_deepening: bool,
    /// Verbose output
    pub verbose: bool,
}

impl Default for LeanCopConfig {
    fn default() -> Self {
        LeanCopConfig {
            max_depth: 100,
            max_inferences: 100000,
            iterative_deepening: true,
            verbose: false,
        }
    }
}

/// A matrix entry (clause in the matrix)
#[derive(Debug, Clone)]
struct MatrixClause {
    /// The literals in the clause
    literals: Vec<Literal>,
    /// Clause index
    #[allow(dead_code)]
    index: usize,
}

/// Connection proof state
#[derive(Debug, Clone)]
struct ProofState {
    /// Current path (open goals)
    path: Vec<Literal>,
    /// Active substitution
    substitution: Substitution,
    /// Lemmas (proven goals that can be reused)
    lemmas: Vec<Literal>,
    /// Current depth
    depth: usize,
}

impl ProofState {
    fn new() -> Self {
        ProofState {
            path: Vec::new(),
            substitution: Substitution::new(),
            lemmas: Vec::new(),
            depth: 0,
        }
    }
}

/// Result of leanCoP proof search
#[derive(Debug, Clone)]
pub enum LeanCopResult {
    /// Proof found
    Proved {
        /// Number of inferences
        inferences: usize,
        /// Proof depth
        depth: usize,
    },
    /// No proof found
    NotProved {
        reason: String,
    },
}

/// leanCoP Prover
pub struct LeanCop {
    /// The clause matrix
    matrix: Vec<MatrixClause>,
    /// Configuration
    config: LeanCopConfig,
    /// Inference counter
    inferences: usize,
    /// Variable counter for renaming
    var_counter: usize,
}

impl LeanCop {
    pub fn new() -> Self {
        Self::with_config(LeanCopConfig::default())
    }

    pub fn with_config(config: LeanCopConfig) -> Self {
        LeanCop {
            matrix: Vec::new(),
            config,
            inferences: 0,
            var_counter: 0,
        }
    }

    /// Add a clause to the matrix
    pub fn add_clause(&mut self, clause: Clause) {
        let index = self.matrix.len();
        self.matrix.push(MatrixClause {
            literals: clause.literals,
            index,
        });
    }

    /// Add an axiom (will be negated for refutation)
    pub fn add_axiom(&mut self, literals: Vec<Literal>) {
        let index = self.matrix.len();
        self.matrix.push(MatrixClause { literals, index });
    }

    /// Add a goal (negated conjecture)
    pub fn add_goal(&mut self, literals: Vec<Literal>) {
        // Goal literals are added as-is (already negated)
        let index = self.matrix.len();
        self.matrix.push(MatrixClause { literals, index });
    }

    /// Main prove function
    pub fn prove(&mut self) -> LeanCopResult {
        self.inferences = 0;
        self.var_counter = 1000; // Start variable renaming from 1000

        if self.matrix.is_empty() {
            return LeanCopResult::NotProved {
                reason: "Empty matrix".to_string(),
            };
        }

        if self.config.iterative_deepening {
            // Iterative deepening
            for depth in 1..=self.config.max_depth {
                if self.config.verbose {
                    eprintln!("leanCoP: trying depth {}", depth);
                }

                // Try each clause as start clause
                for start_idx in 0..self.matrix.len() {
                    let mut state = ProofState::new();

                    // Initialize path with literals from start clause
                    let lits = self.matrix[start_idx].literals.clone();
                    let start_clause = self.rename_clause(&lits);

                    for lit in start_clause {
                        state.path.push(lit);
                    }

                    if self.prove_path(&mut state, depth) {
                        return LeanCopResult::Proved {
                            inferences: self.inferences,
                            depth,
                        };
                    }

                    if self.inferences > self.config.max_inferences {
                        return LeanCopResult::NotProved {
                            reason: format!("Inference limit {} reached", self.config.max_inferences),
                        };
                    }
                }
            }

            LeanCopResult::NotProved {
                reason: format!("Depth limit {} reached", self.config.max_depth),
            }
        } else {
            // Fixed depth search
            for start_idx in 0..self.matrix.len() {
                let mut state = ProofState::new();
                let lits = self.matrix[start_idx].literals.clone();
                let start_clause = self.rename_clause(&lits);

                for lit in start_clause {
                    state.path.push(lit);
                }

                if self.prove_path(&mut state, self.config.max_depth) {
                    return LeanCopResult::Proved {
                        inferences: self.inferences,
                        depth: state.depth,
                    };
                }
            }

            LeanCopResult::NotProved {
                reason: "No proof found".to_string(),
            }
        }
    }

    /// Prove all literals on the path
    fn prove_path(&mut self, state: &mut ProofState, max_depth: usize) -> bool {
        // Check inference limit
        if self.inferences > self.config.max_inferences {
            return false;
        }

        // Base case: empty path means proof complete
        if state.path.is_empty() {
            return true;
        }

        // Depth limit check
        if state.depth >= max_depth {
            return false;
        }

        self.inferences += 1;

        // Select the first literal from path (goal to prove)
        let goal = state.path[0].clone();
        let remaining_path: Vec<Literal> = state.path[1..].to_vec();

        // Try reduction: unify with complementary literal on path
        if self.try_reduction(state, &goal, &remaining_path, max_depth) {
            return true;
        }

        // Try lemma reuse
        if self.try_lemma(state, &goal, &remaining_path, max_depth) {
            return true;
        }

        // Try extension: connect with clause from matrix
        if self.try_extension(state, &goal, &remaining_path, max_depth) {
            return true;
        }

        false
    }

    /// Reduction step: unify goal with complement of literal on path
    fn try_reduction(
        &mut self,
        state: &mut ProofState,
        goal: &Literal,
        remaining_path: &[Literal],
        max_depth: usize,
    ) -> bool {
        // Look for complementary literal in remaining path
        for (_i, path_lit) in remaining_path.iter().enumerate() {
            // Check for complementary literals (same predicate, opposite sign)
            if goal.negated != path_lit.negated
                && goal.atom.predicate == path_lit.atom.predicate
            {
                // Try to unify
                let goal_atom = state.substitution.apply_atom(&goal.atom);
                let path_atom = state.substitution.apply_atom(&path_lit.atom);

                if let Some(mgu) = unify_atoms(&goal_atom, &path_atom) {
                    // Apply substitution and continue
                    let mut new_state = state.clone();
                    new_state.substitution = new_state.substitution.compose(&mgu);
                    new_state.path = remaining_path.to_vec();
                    new_state.depth += 1;

                    if self.config.verbose {
                        eprintln!("  Reduction: {} with path literal", goal);
                    }

                    if self.prove_path(&mut new_state, max_depth) {
                        *state = new_state;
                        return true;
                    }
                }
            }
        }

        false
    }

    /// Lemma step: reuse a previously proven goal
    fn try_lemma(
        &mut self,
        state: &mut ProofState,
        goal: &Literal,
        remaining_path: &[Literal],
        max_depth: usize,
    ) -> bool {
        for lemma in &state.lemmas.clone() {
            // Lemma must match goal (same predicate and sign)
            if goal.negated == lemma.negated
                && goal.atom.predicate == lemma.atom.predicate
            {
                let goal_atom = state.substitution.apply_atom(&goal.atom);
                let lemma_atom = state.substitution.apply_atom(&lemma.atom);

                if let Some(mgu) = unify_atoms(&goal_atom, &lemma_atom) {
                    let mut new_state = state.clone();
                    new_state.substitution = new_state.substitution.compose(&mgu);
                    new_state.path = remaining_path.to_vec();

                    if self.config.verbose {
                        eprintln!("  Lemma: {} matches {}", goal, lemma);
                    }

                    if self.prove_path(&mut new_state, max_depth) {
                        *state = new_state;
                        return true;
                    }
                }
            }
        }

        false
    }

    /// Extension step: connect goal with clause from matrix
    fn try_extension(
        &mut self,
        state: &mut ProofState,
        goal: &Literal,
        remaining_path: &[Literal],
        max_depth: usize,
    ) -> bool {
        // Try each clause in the matrix
        for clause_idx in 0..self.matrix.len() {
            // Rename variables in clause to avoid capture
            let lits = self.matrix[clause_idx].literals.clone();
            let clause_lits = self.rename_clause(&lits);

            // Try to connect with each literal in the clause
            for (lit_idx, clause_lit) in clause_lits.iter().enumerate() {
                // Must be complementary (same predicate, opposite sign)
                if goal.negated != clause_lit.negated
                    && goal.atom.predicate == clause_lit.atom.predicate
                {
                    let goal_atom = state.substitution.apply_atom(&goal.atom);
                    let clause_atom = state.substitution.apply_atom(&clause_lit.atom);

                    if let Some(mgu) = unify_atoms(&goal_atom, &clause_atom) {
                        // Build new path: remaining literals from clause + remaining path
                        let mut new_path: Vec<Literal> = Vec::new();

                        // Add other literals from the clause (new subgoals)
                        for (i, lit) in clause_lits.iter().enumerate() {
                            if i != lit_idx {
                                new_path.push(lit.clone());
                            }
                        }

                        // Add remaining path
                        new_path.extend(remaining_path.iter().cloned());

                        let mut new_state = state.clone();
                        new_state.substitution = new_state.substitution.compose(&mgu);
                        new_state.path = new_path;
                        new_state.depth += 1;

                        // Add current goal as potential lemma
                        new_state.lemmas.push(goal.clone());

                        if self.config.verbose {
                            eprintln!(
                                "  Extension: {} with clause {} lit {}",
                                goal, clause_idx, lit_idx
                            );
                        }

                        if self.prove_path(&mut new_state, max_depth) {
                            *state = new_state;
                            return true;
                        }
                    }
                }
            }
        }

        false
    }

    /// Rename variables in a clause to fresh variables
    fn rename_clause(&mut self, literals: &[Literal]) -> Vec<Literal> {
        let mut var_map: HashMap<String, Variable> = HashMap::new();

        literals
            .iter()
            .map(|lit| {
                Literal {
                    atom: self.rename_atom(&lit.atom, &mut var_map),
                    negated: lit.negated,
                }
            })
            .collect()
    }

    fn rename_atom(&mut self, atom: &Atom, var_map: &mut HashMap<String, Variable>) -> Atom {
        Atom {
            predicate: atom.predicate.clone(),
            args: atom.args.iter().map(|t| self.rename_term(t, var_map)).collect(),
        }
    }

    fn rename_term(&mut self, term: &FolTerm, var_map: &mut HashMap<String, Variable>) -> FolTerm {
        match term {
            FolTerm::Var(v) => {
                let key = format!("{}_{}", v.name, v.id);
                let new_var = var_map.entry(key).or_insert_with(|| {
                    self.var_counter += 1;
                    Variable {
                        name: v.name.clone(),
                        id: self.var_counter,
                    }
                });
                FolTerm::Var(new_var.clone())
            }
            FolTerm::Func(f, args) => {
                FolTerm::Func(
                    f.clone(),
                    args.iter().map(|a| self.rename_term(a, var_map)).collect(),
                )
            }
        }
    }

    /// Parse input in a simple format
    pub fn parse_input(&mut self, input: &str) -> Result<(), String> {
        // Simple parser for clauses
        // Format: Each line is a clause
        // Literals separated by |
        // Negation with ~
        // Example: p(X) | ~q(X,Y)

        for line in input.lines() {
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('%') || line.starts_with('#') {
                continue;
            }

            // Skip Prover9 directives
            if line.starts_with("formulas(") || line.starts_with("end_of_list") {
                continue;
            }

            // Parse clause
            if let Some(clause) = self.parse_clause(line) {
                self.add_axiom(clause);
            }
        }

        Ok(())
    }

    fn parse_clause(&mut self, line: &str) -> Option<Vec<Literal>> {
        let line = line.trim().trim_end_matches('.');

        // Handle implication (A -> B becomes ~A | B)
        if line.contains("->") {
            let parts: Vec<&str> = line.split("->").collect();
            if parts.len() == 2 {
                let mut literals = Vec::new();

                // Negate antecedent
                let ant = parts[0].trim();
                if let Some(lit) = self.parse_literal(ant) {
                    literals.push(Literal {
                        atom: lit.atom,
                        negated: !lit.negated,
                    });
                }

                // Add consequent
                let cons = parts[1].trim();
                if let Some(lit) = self.parse_literal(cons) {
                    literals.push(lit);
                }

                return Some(literals);
            }
        }

        // Handle disjunction
        let parts: Vec<&str> = if line.contains('|') {
            line.split('|').collect()
        } else if line.contains(" & ") {
            // Conjunction in clause (shouldn't happen in CNF but handle it)
            line.split(" & ").collect()
        } else {
            vec![line]
        };

        let mut literals = Vec::new();
        for part in parts {
            if let Some(lit) = self.parse_literal(part.trim()) {
                literals.push(lit);
            }
        }

        if literals.is_empty() {
            None
        } else {
            Some(literals)
        }
    }

    fn parse_literal(&mut self, s: &str) -> Option<Literal> {
        let s = s.trim();
        if s.is_empty() {
            return None;
        }

        let (negated, rest) = if s.starts_with('~') || s.starts_with('-') || s.starts_with('¬') {
            (true, &s[1..])
        } else {
            (false, s)
        };

        let rest = rest.trim();

        // Parse predicate(args)
        if let Some(paren_pos) = rest.find('(') {
            let pred_name = &rest[..paren_pos];
            let args_str = &rest[paren_pos + 1..rest.len() - 1]; // Remove ( and )

            let args: Vec<FolTerm> = if args_str.is_empty() {
                Vec::new()
            } else {
                args_str
                    .split(',')
                    .map(|a| self.parse_term(a.trim()))
                    .collect()
            };

            Some(Literal {
                atom: Atom {
                    predicate: Predicate::new(pred_name, args.len()),
                    args,
                },
                negated,
            })
        } else {
            // Propositional atom (no arguments)
            Some(Literal {
                atom: Atom {
                    predicate: Predicate::new(rest, 0),
                    args: Vec::new(),
                },
                negated,
            })
        }
    }

    fn parse_term(&mut self, s: &str) -> FolTerm {
        let s = s.trim();

        // Variable: starts with uppercase or is X, Y, Z pattern
        if s.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
            self.var_counter += 1;
            FolTerm::Var(Variable {
                name: s.to_string(),
                id: self.var_counter,
            })
        } else if let Some(paren_pos) = s.find('(') {
            // Function term
            let func_name = &s[..paren_pos];
            let args_str = &s[paren_pos + 1..s.len() - 1];

            let args: Vec<FolTerm> = if args_str.is_empty() {
                Vec::new()
            } else {
                args_str
                    .split(',')
                    .map(|a| self.parse_term(a.trim()))
                    .collect()
            };

            FolTerm::Func(Function::new(func_name, args.len()), args)
        } else {
            // Constant (0-ary function)
            FolTerm::Func(Function::new(s, 0), Vec::new())
        }
    }

    /// Get statistics
    pub fn stats(&self) -> usize {
        self.inferences
    }
}

impl Default for LeanCop {
    fn default() -> Self {
        Self::new()
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_proof() {
        let mut prover = LeanCop::new();

        // p(a) and ~p(X) | q(X) and ~q(a)
        // Should derive contradiction

        // p(a)
        prover.add_axiom(vec![Literal {
            atom: Atom {
                predicate: Predicate::new("p", 1),
                args: vec![FolTerm::Func(Function::new("a", 0), vec![])],
            },
            negated: false,
        }]);

        // ~p(X) | q(X)
        prover.add_axiom(vec![
            Literal {
                atom: Atom {
                    predicate: Predicate::new("p", 1),
                    args: vec![FolTerm::Var(Variable { name: "X".to_string(), id: 1 })],
                },
                negated: true,
            },
            Literal {
                atom: Atom {
                    predicate: Predicate::new("q", 1),
                    args: vec![FolTerm::Var(Variable { name: "X".to_string(), id: 1 })],
                },
                negated: false,
            },
        ]);

        // ~q(a) (negated goal)
        prover.add_goal(vec![Literal {
            atom: Atom {
                predicate: Predicate::new("q", 1),
                args: vec![FolTerm::Func(Function::new("a", 0), vec![])],
            },
            negated: true,
        }]);

        match prover.prove() {
            LeanCopResult::Proved { .. } => {}
            LeanCopResult::NotProved { reason } => panic!("Should prove: {}", reason),
        }
    }
}
