//! nanoCoP - Non-clausal Connection Prover
//!
//! A compact implementation of the non-clausal connection calculus.
//! Unlike leanCoP, nanoCoP works directly on first-order formulas
//! without converting them to clause normal form.
//!
//! Features:
//! - Direct formula handling (no CNF conversion)
//! - Alpha/Beta rule decomposition
//! - Connection-based proof search
//! - Iterative deepening

use std::collections::HashMap;
use super::term::{FolTerm, Variable, Atom, Function, Predicate};
use super::unify::{Substitution, unify_atoms};

/// A formula tree node for non-clausal connection
#[derive(Debug, Clone)]
pub enum NcFormula {
    /// Atomic formula (literal)
    Literal {
        atom: Atom,
        negated: bool,
    },
    /// Alpha node (conjunctive)
    Alpha {
        children: Vec<NcFormula>,
    },
    /// Beta node (disjunctive)
    Beta {
        children: Vec<NcFormula>,
    },
}

impl NcFormula {
    /// Create a literal
    pub fn lit(atom: Atom, negated: bool) -> Self {
        NcFormula::Literal { atom, negated }
    }

    /// Create an alpha node
    pub fn alpha(children: Vec<NcFormula>) -> Self {
        NcFormula::Alpha { children }
    }

    /// Create a beta node
    pub fn beta(children: Vec<NcFormula>) -> Self {
        NcFormula::Beta { children }
    }

    /// Check if this is a literal
    pub fn is_literal(&self) -> bool {
        matches!(self, NcFormula::Literal { .. })
    }

    /// Get all literals in the formula tree
    pub fn collect_literals(&self) -> Vec<(Atom, bool)> {
        match self {
            NcFormula::Literal { atom, negated } => vec![(atom.clone(), *negated)],
            NcFormula::Alpha { children } | NcFormula::Beta { children } => {
                children.iter().flat_map(|c| c.collect_literals()).collect()
            }
        }
    }
}

/// Position in the formula tree (path from root)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Position(Vec<usize>);

impl Position {
    pub fn root() -> Self {
        Position(Vec::new())
    }

    pub fn child(&self, idx: usize) -> Self {
        let mut path = self.0.clone();
        path.push(idx);
        Position(path)
    }

    pub fn parent(&self) -> Option<Self> {
        if self.0.is_empty() {
            None
        } else {
            let mut path = self.0.clone();
            path.pop();
            Some(Position(path))
        }
    }
}

/// Configuration for nanoCoP
#[derive(Debug, Clone)]
pub struct NanoCopConfig {
    /// Maximum proof depth
    pub max_depth: usize,
    /// Maximum inferences
    pub max_inferences: usize,
    /// Use iterative deepening
    pub iterative_deepening: bool,
    /// Verbose output
    pub verbose: bool,
}

impl Default for NanoCopConfig {
    fn default() -> Self {
        NanoCopConfig {
            max_depth: 50,
            max_inferences: 100000,
            iterative_deepening: true,
            verbose: false,
        }
    }
}

/// Result of nanoCoP proof search
#[derive(Debug, Clone)]
pub enum NanoCopResult {
    /// Proof found
    Proved {
        inferences: usize,
        depth: usize,
    },
    /// No proof found
    NotProved {
        reason: String,
    },
}

/// Proof state for non-clausal connection
#[derive(Debug, Clone)]
struct NcProofState {
    /// Current substitution
    substitution: Substitution,
    /// Active path (open literals)
    path: Vec<(Atom, bool, Position)>,
    /// Proven lemmas
    lemmas: Vec<(Atom, bool)>,
    /// Current depth
    depth: usize,
}

impl NcProofState {
    fn new() -> Self {
        NcProofState {
            substitution: Substitution::new(),
            path: Vec::new(),
            lemmas: Vec::new(),
            depth: 0,
        }
    }
}

/// nanoCoP Prover
pub struct NanoCop {
    /// The formula matrix (list of formula trees)
    formulas: Vec<NcFormula>,
    /// Configuration
    config: NanoCopConfig,
    /// Inference counter
    inferences: usize,
    /// Variable counter
    var_counter: usize,
}

impl NanoCop {
    pub fn new() -> Self {
        Self::with_config(NanoCopConfig::default())
    }

    pub fn with_config(config: NanoCopConfig) -> Self {
        NanoCop {
            formulas: Vec::new(),
            config,
            inferences: 0,
            var_counter: 1000,
        }
    }

    /// Add an axiom formula
    pub fn add_axiom(&mut self, formula: NcFormula) {
        self.formulas.push(formula);
    }

    /// Add a goal (negated conjecture)
    pub fn add_goal(&mut self, formula: NcFormula) {
        // Goal is added as-is (should be negated already)
        self.formulas.push(formula);
    }

    /// Convert from standard formula representation
    pub fn from_formula(&mut self, formula: &super::tableau::Formula, negated: bool) -> NcFormula {
        use super::tableau::Formula;

        match formula {
            Formula::Atom(atom) => NcFormula::lit(atom.clone(), negated),

            Formula::Not(ref inner) => self.from_formula(inner, !negated),

            Formula::And(ref a, ref b) => {
                if negated {
                    // ¬(A ∧ B) = ¬A ∨ ¬B (beta)
                    NcFormula::beta(vec![
                        self.from_formula(a, true),
                        self.from_formula(b, true),
                    ])
                } else {
                    // A ∧ B (alpha)
                    NcFormula::alpha(vec![
                        self.from_formula(a, false),
                        self.from_formula(b, false),
                    ])
                }
            }

            Formula::Or(ref a, ref b) => {
                if negated {
                    // ¬(A ∨ B) = ¬A ∧ ¬B (alpha)
                    NcFormula::alpha(vec![
                        self.from_formula(a, true),
                        self.from_formula(b, true),
                    ])
                } else {
                    // A ∨ B (beta)
                    NcFormula::beta(vec![
                        self.from_formula(a, false),
                        self.from_formula(b, false),
                    ])
                }
            }

            Formula::Implies(ref a, ref b) => {
                if negated {
                    // ¬(A → B) = A ∧ ¬B (alpha)
                    NcFormula::alpha(vec![
                        self.from_formula(a, false),
                        self.from_formula(b, true),
                    ])
                } else {
                    // A → B = ¬A ∨ B (beta)
                    NcFormula::beta(vec![
                        self.from_formula(a, true),
                        self.from_formula(b, false),
                    ])
                }
            }

            Formula::Iff(ref a, ref b) => {
                // A ↔ B = (A → B) ∧ (B → A)
                let impl1 = Formula::Implies(Box::new((**a).clone()), Box::new((**b).clone()));
                let impl2 = Formula::Implies(Box::new((**b).clone()), Box::new((**a).clone()));
                self.from_formula(&Formula::And(Box::new(impl1), Box::new(impl2)), negated)
            }

            Formula::Forall(var, ref body) => {
                // For nanoCoP we handle quantifiers by instantiation during search
                // For now, treat as the body with the variable
                self.from_formula(body, negated)
            }

            Formula::Exists(var, ref body) => {
                // Similar to forall
                self.from_formula(body, negated)
            }
        }
    }

    /// Main prove function
    pub fn prove(&mut self) -> NanoCopResult {
        self.inferences = 0;

        if self.formulas.is_empty() {
            return NanoCopResult::NotProved {
                reason: "No formulas".to_string(),
            };
        }

        if self.config.iterative_deepening {
            for depth in 1..=self.config.max_depth {
                if self.config.verbose {
                    eprintln!("nanoCoP: trying depth {}", depth);
                }

                // Try each formula as start formula
                for start_idx in 0..self.formulas.len() {
                    let f = self.formulas[start_idx].clone();
                    let formula = self.rename_formula(&f);
                    let mut state = NcProofState::new();

                    if self.prove_formula(&formula, Position::root(), &mut state, depth) {
                        return NanoCopResult::Proved {
                            inferences: self.inferences,
                            depth,
                        };
                    }

                    if self.inferences > self.config.max_inferences {
                        return NanoCopResult::NotProved {
                            reason: format!("Inference limit {} reached", self.config.max_inferences),
                        };
                    }
                }
            }

            NanoCopResult::NotProved {
                reason: format!("Depth limit {} reached", self.config.max_depth),
            }
        } else {
            for start_idx in 0..self.formulas.len() {
                let f = self.formulas[start_idx].clone();
                let formula = self.rename_formula(&f);
                let mut state = NcProofState::new();

                if self.prove_formula(&formula, Position::root(), &mut state, self.config.max_depth) {
                    return NanoCopResult::Proved {
                        inferences: self.inferences,
                        depth: state.depth,
                    };
                }
            }

            NanoCopResult::NotProved {
                reason: "No proof found".to_string(),
            }
        }
    }

    /// Prove a formula at a given position
    fn prove_formula(
        &mut self,
        formula: &NcFormula,
        pos: Position,
        state: &mut NcProofState,
        max_depth: usize,
    ) -> bool {
        if state.depth > max_depth {
            return false;
        }

        if self.inferences > self.config.max_inferences {
            return false;
        }

        self.inferences += 1;

        match formula {
            NcFormula::Literal { atom, negated } => {
                self.prove_literal(atom, *negated, pos, state, max_depth)
            }

            NcFormula::Alpha { children } => {
                // Alpha: must prove ALL children
                let orig_state = state.clone();

                for (i, child) in children.iter().enumerate() {
                    let child_pos = pos.child(i);
                    if !self.prove_formula(child, child_pos, state, max_depth) {
                        *state = orig_state;
                        return false;
                    }
                }
                true
            }

            NcFormula::Beta { children } => {
                // Beta: must prove ONE child
                for (i, child) in children.iter().enumerate() {
                    let child_pos = pos.child(i);
                    let mut child_state = state.clone();

                    if self.prove_formula(child, child_pos, &mut child_state, max_depth) {
                        *state = child_state;
                        return true;
                    }
                }
                false
            }
        }
    }

    /// Prove a literal
    fn prove_literal(
        &mut self,
        atom: &Atom,
        negated: bool,
        pos: Position,
        state: &mut NcProofState,
        max_depth: usize,
    ) -> bool {
        let goal_atom = state.substitution.apply_atom(atom);

        // Try reduction: connect with complementary literal on path
        for (path_atom, path_neg, _path_pos) in &state.path {
            if negated != *path_neg && goal_atom.predicate == path_atom.predicate {
                let path_atom_subst = state.substitution.apply_atom(path_atom);
                if let Some(mgu) = unify_atoms(&goal_atom, &path_atom_subst) {
                    let mut new_state = state.clone();
                    new_state.substitution = new_state.substitution.compose(&mgu);

                    if self.config.verbose {
                        eprintln!("  Reduction: {:?} with path literal", atom);
                    }

                    *state = new_state;
                    return true;
                }
            }
        }

        // Try lemma
        for (lemma_atom, lemma_neg) in &state.lemmas.clone() {
            if negated == *lemma_neg && goal_atom.predicate == lemma_atom.predicate {
                let lemma_atom_subst = state.substitution.apply_atom(lemma_atom);
                if let Some(mgu) = unify_atoms(&goal_atom, &lemma_atom_subst) {
                    let mut new_state = state.clone();
                    new_state.substitution = new_state.substitution.compose(&mgu);

                    if self.config.verbose {
                        eprintln!("  Lemma: {:?}", atom);
                    }

                    *state = new_state;
                    return true;
                }
            }
        }

        // Try extension: connect with formula from matrix
        for formula_idx in 0..self.formulas.len() {
            let f = self.formulas[formula_idx].clone();
            let formula = self.rename_formula(&f);

            if self.try_extension(atom, negated, &formula, pos.clone(), state, max_depth) {
                return true;
            }
        }

        false
    }

    /// Try extension with a formula
    fn try_extension(
        &mut self,
        goal_atom: &Atom,
        goal_neg: bool,
        formula: &NcFormula,
        pos: Position,
        state: &mut NcProofState,
        max_depth: usize,
    ) -> bool {
        match formula {
            NcFormula::Literal { atom, negated } => {
                // Try to connect
                if goal_neg != *negated && goal_atom.predicate == atom.predicate {
                    let goal_subst = state.substitution.apply_atom(goal_atom);
                    let atom_subst = state.substitution.apply_atom(atom);

                    if let Some(mgu) = unify_atoms(&goal_subst, &atom_subst) {
                        let mut new_state = state.clone();
                        new_state.substitution = new_state.substitution.compose(&mgu);
                        new_state.lemmas.push((goal_atom.clone(), goal_neg));

                        if self.config.verbose {
                            eprintln!("  Extension: {:?} with {:?}", goal_atom, atom);
                        }

                        *state = new_state;
                        return true;
                    }
                }
                false
            }

            NcFormula::Alpha { children } => {
                // Find a child to connect with, then prove the rest
                for (i, child) in children.iter().enumerate() {
                    if let NcFormula::Literal { atom, negated } = child {
                        if goal_neg != *negated && goal_atom.predicate == atom.predicate {
                            let goal_subst = state.substitution.apply_atom(goal_atom);
                            let atom_subst = state.substitution.apply_atom(atom);

                            if let Some(mgu) = unify_atoms(&goal_subst, &atom_subst) {
                                let mut new_state = state.clone();
                                new_state.substitution = new_state.substitution.compose(&mgu);
                                new_state.depth += 1;

                                // Add goal to path
                                new_state.path.push((goal_atom.clone(), goal_neg, pos.clone()));

                                // Must prove other children
                                let mut success = true;
                                for (j, other_child) in children.iter().enumerate() {
                                    if j != i {
                                        let child_pos = pos.child(j);
                                        if !self.prove_formula(other_child, child_pos, &mut new_state, max_depth) {
                                            success = false;
                                            break;
                                        }
                                    }
                                }

                                if success {
                                    new_state.path.pop();
                                    new_state.lemmas.push((goal_atom.clone(), goal_neg));
                                    *state = new_state;
                                    return true;
                                }
                            }
                        }
                    }
                }

                // Recurse into alpha children
                for (i, child) in children.iter().enumerate() {
                    if self.try_extension(goal_atom, goal_neg, child, pos.child(i), state, max_depth) {
                        return true;
                    }
                }
                false
            }

            NcFormula::Beta { children } => {
                // Try any branch
                for (i, child) in children.iter().enumerate() {
                    if self.try_extension(goal_atom, goal_neg, child, pos.child(i), state, max_depth) {
                        return true;
                    }
                }
                false
            }
        }
    }

    /// Rename variables in a formula
    fn rename_formula(&mut self, formula: &NcFormula) -> NcFormula {
        let mut var_map: HashMap<String, Variable> = HashMap::new();
        self.rename_formula_inner(formula, &mut var_map)
    }

    fn rename_formula_inner(
        &mut self,
        formula: &NcFormula,
        var_map: &mut HashMap<String, Variable>,
    ) -> NcFormula {
        match formula {
            NcFormula::Literal { atom, negated } => {
                NcFormula::Literal {
                    atom: self.rename_atom(atom, var_map),
                    negated: *negated,
                }
            }
            NcFormula::Alpha { children } => {
                NcFormula::Alpha {
                    children: children
                        .iter()
                        .map(|c| self.rename_formula_inner(c, var_map))
                        .collect(),
                }
            }
            NcFormula::Beta { children } => {
                NcFormula::Beta {
                    children: children
                        .iter()
                        .map(|c| self.rename_formula_inner(c, var_map))
                        .collect(),
                }
            }
        }
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

    /// Parse a simple input format
    pub fn parse_input(&mut self, input: &str) -> Result<(), String> {
        for line in input.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('%') || line.starts_with('#') {
                continue;
            }

            // Skip Prover9 directives
            if line.starts_with("formulas(") || line.starts_with("end_of_list") {
                continue;
            }

            // Parse as clause-like input and convert to non-clausal form
            if let Some(formula) = self.parse_formula_line(line) {
                self.add_axiom(formula);
            }
        }

        Ok(())
    }

    fn parse_formula_line(&mut self, line: &str) -> Option<NcFormula> {
        let line = line.trim().trim_end_matches('.');

        // Handle implication
        if line.contains("->") {
            let parts: Vec<&str> = line.split("->").collect();
            if parts.len() == 2 {
                let ant = self.parse_literal(parts[0].trim())?;
                let cons = self.parse_literal(parts[1].trim())?;

                // A -> B as beta(¬A, B)
                let neg_ant = match ant {
                    NcFormula::Literal { atom, negated } => {
                        NcFormula::Literal { atom, negated: !negated }
                    }
                    _ => return None,
                };

                return Some(NcFormula::beta(vec![neg_ant, cons]));
            }
        }

        // Handle disjunction
        if line.contains('|') {
            let parts: Vec<&str> = line.split('|').collect();
            let children: Vec<NcFormula> = parts
                .iter()
                .filter_map(|p| self.parse_literal(p.trim()))
                .collect();
            if children.is_empty() {
                return None;
            }
            return Some(NcFormula::beta(children));
        }

        // Handle conjunction
        if line.contains(" & ") {
            let parts: Vec<&str> = line.split(" & ").collect();
            let children: Vec<NcFormula> = parts
                .iter()
                .filter_map(|p| self.parse_literal(p.trim()))
                .collect();
            if children.is_empty() {
                return None;
            }
            return Some(NcFormula::alpha(children));
        }

        // Single literal
        self.parse_literal(line)
    }

    fn parse_literal(&mut self, s: &str) -> Option<NcFormula> {
        let s = s.trim();
        if s.is_empty() {
            return None;
        }

        let (negated, rest) = if s.starts_with('~') || s.starts_with('-') {
            (true, s[1..].trim())
        } else {
            (false, s)
        };

        // Parse predicate(args)
        if let Some(paren_pos) = rest.find('(') {
            let pred_name = &rest[..paren_pos];
            let args_str = &rest[paren_pos + 1..rest.len() - 1];

            let args: Vec<FolTerm> = if args_str.is_empty() {
                Vec::new()
            } else {
                args_str.split(',').map(|a| self.parse_term(a.trim())).collect()
            };

            Some(NcFormula::lit(
                Atom {
                    predicate: Predicate::new(pred_name, args.len()),
                    args,
                },
                negated,
            ))
        } else {
            // Propositional
            Some(NcFormula::lit(
                Atom {
                    predicate: Predicate::new(rest, 0),
                    args: Vec::new(),
                },
                negated,
            ))
        }
    }

    fn parse_term(&mut self, s: &str) -> FolTerm {
        let s = s.trim();

        if s.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
            self.var_counter += 1;
            FolTerm::Var(Variable {
                name: s.to_string(),
                id: self.var_counter,
            })
        } else if let Some(paren_pos) = s.find('(') {
            let func_name = &s[..paren_pos];
            let args_str = &s[paren_pos + 1..s.len() - 1];
            let args: Vec<FolTerm> = if args_str.is_empty() {
                Vec::new()
            } else {
                args_str.split(',').map(|a| self.parse_term(a.trim())).collect()
            };
            FolTerm::Func(Function::new(func_name, args.len()), args)
        } else {
            FolTerm::Func(Function::new(s, 0), Vec::new())
        }
    }

    /// Get statistics
    pub fn stats(&self) -> usize {
        self.inferences
    }
}

impl Default for NanoCop {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_proof() {
        let mut prover = NanoCop::new();

        // p(a) and ~p(X) | q(X) and ~q(a)
        // Should find proof

        // p(a)
        prover.add_axiom(NcFormula::lit(
            Atom {
                predicate: Predicate::new("p", 1),
                args: vec![FolTerm::Func(Function::new("a", 0), vec![])],
            },
            false,
        ));

        // ~p(X) | q(X) as beta
        prover.add_axiom(NcFormula::beta(vec![
            NcFormula::lit(
                Atom {
                    predicate: Predicate::new("p", 1),
                    args: vec![FolTerm::Var(Variable { name: "X".to_string(), id: 1 })],
                },
                true,
            ),
            NcFormula::lit(
                Atom {
                    predicate: Predicate::new("q", 1),
                    args: vec![FolTerm::Var(Variable { name: "X".to_string(), id: 1 })],
                },
                false,
            ),
        ]));

        // ~q(a) (negated goal)
        prover.add_goal(NcFormula::lit(
            Atom {
                predicate: Predicate::new("q", 1),
                args: vec![FolTerm::Func(Function::new("a", 0), vec![])],
            },
            true,
        ));

        match prover.prove() {
            NanoCopResult::Proved { .. } => {}
            NanoCopResult::NotProved { reason } => panic!("Should prove: {}", reason),
        }
    }
}
