//! Analytic Tableau Prover
//!
//! Implementation of the semantic tableau method for first-order logic
//! theorem proving. Also known as the truth tree method.
//!
//! Features:
//! - Alpha/Beta rules for propositional connectives
//! - Gamma/Delta rules for quantifiers
//! - Free-variable tableau with unification
//! - Branch closure detection

use std::collections::VecDeque;
use super::term::{FolTerm, Variable, Atom, Function, Predicate};
use super::unify::unify_atoms;

/// A formula in the tableau
#[derive(Debug, Clone, PartialEq)]
pub enum Formula {
    /// Atomic formula
    Atom(Atom),
    /// Negation
    Not(Box<Formula>),
    /// Conjunction
    And(Box<Formula>, Box<Formula>),
    /// Disjunction
    Or(Box<Formula>, Box<Formula>),
    /// Implication
    Implies(Box<Formula>, Box<Formula>),
    /// Biconditional
    Iff(Box<Formula>, Box<Formula>),
    /// Universal quantifier
    Forall(Variable, Box<Formula>),
    /// Existential quantifier
    Exists(Variable, Box<Formula>),
}

impl Formula {
    /// Check if formula is a literal (atom or negated atom)
    pub fn is_literal(&self) -> bool {
        match self {
            Formula::Atom(_) => true,
            Formula::Not(inner) => matches!(**inner, Formula::Atom(_)),
            _ => false,
        }
    }

    /// Get the sign of a literal (true = positive, false = negative)
    pub fn literal_sign(&self) -> Option<bool> {
        match self {
            Formula::Atom(_) => Some(true),
            Formula::Not(inner) if matches!(**inner, Formula::Atom(_)) => Some(false),
            _ => None,
        }
    }

    /// Get the atom from a literal
    pub fn literal_atom(&self) -> Option<&Atom> {
        match self {
            Formula::Atom(a) => Some(a),
            Formula::Not(inner) => {
                if let Formula::Atom(a) = &**inner {
                    Some(a)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

/// A signed formula (T or F prefix)
#[derive(Debug, Clone)]
pub struct SignedFormula {
    /// True = T (formula is true), False = F (formula is false)
    pub sign: bool,
    /// The formula
    pub formula: Formula,
    /// Source branch node ID
    pub source: usize,
}

impl SignedFormula {
    pub fn new(sign: bool, formula: Formula, source: usize) -> Self {
        SignedFormula { sign, formula, source }
    }

    /// Check if this is a signed literal
    pub fn is_literal(&self) -> bool {
        self.formula.is_literal()
    }

    /// Get effective sign (considering formula negation)
    pub fn effective_sign(&self) -> Option<bool> {
        match &self.formula {
            Formula::Atom(_) => Some(self.sign),
            Formula::Not(inner) if matches!(**inner, Formula::Atom(_)) => Some(!self.sign),
            _ => None,
        }
    }

    /// Get atom from signed literal
    pub fn get_atom(&self) -> Option<&Atom> {
        self.formula.literal_atom()
    }
}

/// Classification of signed formulas
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FormulaType {
    /// Alpha: conjunctive (T∧, F∨, F→)
    Alpha,
    /// Beta: disjunctive (F∧, T∨, T→)
    Beta,
    /// Gamma: universal (T∀, F∃)
    Gamma,
    /// Delta: existential (F∀, T∃)
    Delta,
    /// Literal (atom or negated atom)
    Literal,
    /// Double negation
    DoubleNegation,
}

impl SignedFormula {
    /// Classify the signed formula
    pub fn classify(&self) -> FormulaType {
        match (&self.formula, self.sign) {
            // Literals
            (Formula::Atom(_), _) => FormulaType::Literal,
            (Formula::Not(inner), _) if matches!(**inner, Formula::Atom(_)) => FormulaType::Literal,

            // Double negation
            (Formula::Not(inner), _) if matches!(**inner, Formula::Not(_)) => FormulaType::DoubleNegation,

            // Alpha rules (conjunctive)
            (Formula::And(_, _), true) => FormulaType::Alpha,
            (Formula::Or(_, _), false) => FormulaType::Alpha,
            (Formula::Implies(_, _), false) => FormulaType::Alpha,
            (Formula::Not(inner), true) if matches!(**inner, Formula::Or(_, _)) => FormulaType::Alpha,
            (Formula::Not(inner), true) if matches!(**inner, Formula::Implies(_, _)) => FormulaType::Alpha,

            // Beta rules (disjunctive)
            (Formula::And(_, _), false) => FormulaType::Beta,
            (Formula::Or(_, _), true) => FormulaType::Beta,
            (Formula::Implies(_, _), true) => FormulaType::Beta,
            (Formula::Not(inner), true) if matches!(**inner, Formula::And(_, _)) => FormulaType::Beta,

            // Gamma rules (universal)
            (Formula::Forall(_, _), true) => FormulaType::Gamma,
            (Formula::Exists(_, _), false) => FormulaType::Gamma,
            (Formula::Not(inner), true) if matches!(**inner, Formula::Exists(_, _)) => FormulaType::Gamma,

            // Delta rules (existential)
            (Formula::Forall(_, _), false) => FormulaType::Delta,
            (Formula::Exists(_, _), true) => FormulaType::Delta,
            (Formula::Not(inner), true) if matches!(**inner, Formula::Forall(_, _)) => FormulaType::Delta,

            // Biconditional (treated as conjunction of implications)
            (Formula::Iff(_, _), true) => FormulaType::Alpha,
            (Formula::Iff(_, _), false) => FormulaType::Beta,

            // Other negations
            (Formula::Not(_), _) => FormulaType::DoubleNegation,
        }
    }
}

/// A node in the tableau tree
#[derive(Debug, Clone)]
pub struct TableauNode {
    /// Node ID
    pub id: usize,
    /// Signed formula at this node
    pub formula: SignedFormula,
    /// Parent node ID (None for root)
    pub parent: Option<usize>,
    /// Whether this branch is closed
    pub closed: bool,
}

/// Configuration for the tableau prover
#[derive(Debug, Clone)]
pub struct TableauConfig {
    /// Maximum tableau size
    pub max_nodes: usize,
    /// Maximum gamma rule applications per formula
    pub max_gamma_instances: usize,
    /// Use free-variable tableau
    pub free_variable: bool,
    /// Verbose output
    pub verbose: bool,
}

impl Default for TableauConfig {
    fn default() -> Self {
        TableauConfig {
            max_nodes: 10000,
            max_gamma_instances: 5,
            free_variable: true,
            verbose: false,
        }
    }
}

/// Result of tableau proof search
#[derive(Debug, Clone)]
pub enum TableauResult {
    /// Tableau closed (formula is valid / negation unsatisfiable)
    Closed {
        nodes: usize,
    },
    /// Open branch found (formula not valid)
    Open {
        reason: String,
    },
    /// Resource limit reached
    Unknown {
        reason: String,
    },
}

/// Tableau Prover
pub struct TableauProver {
    /// Tableau nodes
    nodes: Vec<TableauNode>,
    /// Unexpanded formulas queue
    unexpanded: VecDeque<usize>,
    /// Gamma formulas (can be reused)
    gamma_formulas: Vec<(usize, usize)>, // (node_id, instance_count)
    /// Configuration
    config: TableauConfig,
    /// Variable counter for Skolem/instantiation
    var_counter: usize,
    /// Skolem function counter
    skolem_counter: usize,
}

impl TableauProver {
    pub fn new() -> Self {
        Self::with_config(TableauConfig::default())
    }

    pub fn with_config(config: TableauConfig) -> Self {
        TableauProver {
            nodes: Vec::new(),
            unexpanded: VecDeque::new(),
            gamma_formulas: Vec::new(),
            config,
            var_counter: 0,
            skolem_counter: 0,
        }
    }

    /// Add a formula to prove (will negate for refutation)
    pub fn add_formula(&mut self, formula: Formula) {
        // To prove formula, we show its negation leads to contradiction
        // So add F(formula) to the tableau
        let node = TableauNode {
            id: self.nodes.len(),
            formula: SignedFormula::new(false, formula, 0),
            parent: None,
            closed: false,
        };
        self.unexpanded.push_back(node.id);
        self.nodes.push(node);
    }

    /// Add an axiom (assumed true)
    pub fn add_axiom(&mut self, formula: Formula) {
        let node = TableauNode {
            id: self.nodes.len(),
            formula: SignedFormula::new(true, formula, 0),
            parent: None,
            closed: false,
        };
        self.unexpanded.push_back(node.id);
        self.nodes.push(node);
    }

    /// Main prove function
    pub fn prove(&mut self) -> TableauResult {
        // Use a priority-based approach: process non-branching rules before branching rules
        // This ensures that all formulas on a branch are fully expanded before splitting
        loop {
            // Check limits
            if self.nodes.len() > self.config.max_nodes {
                return TableauResult::Unknown {
                    reason: format!("Node limit {} reached", self.config.max_nodes),
                };
            }

            // First, try to find a non-branching rule to apply (alpha, delta, double-negation, literal)
            let non_branching_idx = self.unexpanded.iter().position(|&node_id| {
                if self.is_branch_closed(node_id) {
                    return false;
                }
                let formula_type = self.nodes[node_id].formula.classify();
                matches!(
                    formula_type,
                    FormulaType::Alpha
                        | FormulaType::Delta
                        | FormulaType::DoubleNegation
                        | FormulaType::Literal
                )
            });

            // If no non-branching rule, try branching rules (beta, gamma)
            let node_id = if let Some(idx) = non_branching_idx {
                self.unexpanded.remove(idx).unwrap()
            } else {
                // Try to find a branching rule
                let branching_idx = self.unexpanded.iter().position(|&node_id| {
                    !self.is_branch_closed(node_id)
                });
                if let Some(idx) = branching_idx {
                    self.unexpanded.remove(idx).unwrap()
                } else {
                    // No more unexpanded formulas
                    break;
                }
            };

            // Skip if branch already closed
            if self.is_branch_closed(node_id) {
                continue;
            }

            let formula_type = self.nodes[node_id].formula.classify();

            if self.config.verbose {
                eprintln!(
                    "Expanding node {}: {:?} (type: {:?})",
                    node_id, self.nodes[node_id].formula, formula_type
                );
            }

            match formula_type {
                FormulaType::Literal => {
                    // Check for branch closure
                    if self.check_closure(node_id) {
                        self.close_branch(node_id);
                    }
                }
                FormulaType::Alpha => {
                    self.apply_alpha(node_id);
                }
                FormulaType::Beta => {
                    self.apply_beta(node_id);
                }
                FormulaType::Gamma => {
                    self.apply_gamma(node_id);
                }
                FormulaType::Delta => {
                    self.apply_delta(node_id);
                }
                FormulaType::DoubleNegation => {
                    self.apply_double_negation(node_id);
                }
            }
        }

        // Try gamma formulas again if there are open branches
        if !self.all_branches_closed() && !self.gamma_formulas.is_empty() {
            for (gamma_id, count) in &mut self.gamma_formulas.clone() {
                if *count < self.config.max_gamma_instances {
                    self.apply_gamma(*gamma_id);
                    // Update count in the original vector
                    if let Some(entry) = self.gamma_formulas.iter_mut().find(|(id, _)| *id == *gamma_id) {
                        entry.1 += 1;
                    }
                }
            }
        }

        if self.all_branches_closed() {
            TableauResult::Closed {
                nodes: self.nodes.len(),
            }
        } else {
            TableauResult::Open {
                reason: "Open branch exists".to_string(),
            }
        }
    }

    /// Apply alpha rule (conjunctive - add both components sequentially)
    fn apply_alpha(&mut self, node_id: usize) {
        let node = &self.nodes[node_id];
        let (f1, f2) = self.get_alpha_components(&node.formula);

        // Add both formulas to the same branch (f2 after f1, sequentially)
        let id1 = self.add_node(f1, Some(node_id));
        let id2 = self.add_node(f2, Some(id1)); // Chain: f2 is child of f1

        self.unexpanded.push_back(id1);
        self.unexpanded.push_back(id2);
    }

    /// Apply beta rule (disjunctive - branch)
    fn apply_beta(&mut self, node_id: usize) {
        let node = &self.nodes[node_id];
        let (f1, f2) = self.get_beta_components(&node.formula);

        // Find all leaf nodes that descend from this node (or the node itself if it's a leaf)
        let leaves = self.find_leaf_descendants(node_id);

        if leaves.is_empty() {
            // Node is a leaf - create two branches from it directly
            let id1 = self.add_node(f1, Some(node_id));
            let id2 = self.add_node(f2, Some(node_id));
            self.unexpanded.push_back(id1);
            self.unexpanded.push_back(id2);
        } else {
            // Create branches from each leaf
            for leaf_id in leaves {
                let id1 = self.add_node(f1.clone(), Some(leaf_id));
                let id2 = self.add_node(f2.clone(), Some(leaf_id));
                self.unexpanded.push_back(id1);
                self.unexpanded.push_back(id2);
            }
        }
    }

    /// Find all leaf nodes that are descendants of the given node
    fn find_leaf_descendants(&self, node_id: usize) -> Vec<usize> {
        let mut leaves = Vec::new();
        let mut to_visit = vec![node_id];

        while let Some(current) = to_visit.pop() {
            // Find children of current node
            let children: Vec<usize> = self.nodes.iter()
                .filter(|n| n.parent == Some(current))
                .map(|n| n.id)
                .collect();

            if children.is_empty() {
                // current is a leaf, but only include it if it's not the original node
                if current != node_id {
                    leaves.push(current);
                }
            } else {
                to_visit.extend(children);
            }
        }

        leaves
    }

    /// Apply gamma rule (universal - instantiate)
    fn apply_gamma(&mut self, node_id: usize) {
        let node = &self.nodes[node_id];
        let (var, body, sign) = self.get_gamma_components(&node.formula);

        // Track gamma formula for reuse
        if !self.gamma_formulas.iter().any(|(id, _)| *id == node_id) {
            self.gamma_formulas.push((node_id, 1));
        }

        if self.config.free_variable {
            // Free-variable tableau: use a fresh free variable
            self.var_counter += 1;
            let new_var = Variable {
                name: format!("_X{}", self.var_counter),
                id: self.var_counter,
            };
            let instantiated = self.substitute_var(&body, &var, &FolTerm::Var(new_var));
            let sf = SignedFormula::new(sign, instantiated, node_id);
            let id = self.add_node(sf, Some(node_id));
            self.unexpanded.push_back(id);
        } else {
            // Ground tableau: use a constant
            self.var_counter += 1;
            let constant = FolTerm::Func(
                Function::new(&format!("c{}", self.var_counter), 0),
                Vec::new(),
            );
            let instantiated = self.substitute_var(&body, &var, &constant);
            let sf = SignedFormula::new(sign, instantiated, node_id);
            let id = self.add_node(sf, Some(node_id));
            self.unexpanded.push_back(id);
        }
    }

    /// Apply delta rule (existential - Skolemize)
    fn apply_delta(&mut self, node_id: usize) {
        let node = &self.nodes[node_id];
        let (var, body, sign) = self.get_delta_components(&node.formula);

        // Collect free variables in the branch
        let free_vars = self.collect_free_vars(node_id);

        // Create Skolem term
        self.skolem_counter += 1;
        let skolem_term = if free_vars.is_empty() {
            // Skolem constant
            FolTerm::Func(
                Function::new(&format!("sk{}", self.skolem_counter), 0),
                Vec::new(),
            )
        } else {
            // Skolem function
            FolTerm::Func(
                Function::new(&format!("sk{}", self.skolem_counter), free_vars.len()),
                free_vars.into_iter().map(FolTerm::Var).collect(),
            )
        };

        let instantiated = self.substitute_var(&body, &var, &skolem_term);
        let sf = SignedFormula::new(sign, instantiated, node_id);
        let id = self.add_node(sf, Some(node_id));
        self.unexpanded.push_back(id);
    }

    /// Apply double negation elimination
    fn apply_double_negation(&mut self, node_id: usize) {
        let node = &self.nodes[node_id];
        if let Formula::Not(ref outer) = &node.formula.formula {
            if let Formula::Not(ref inner) = **outer {
                let sf = SignedFormula::new(node.formula.sign, (**inner).clone(), node_id);
                let id = self.add_node(sf, Some(node_id));
                self.unexpanded.push_back(id);
            }
        }
    }

    /// Get alpha rule components
    fn get_alpha_components(&self, sf: &SignedFormula) -> (SignedFormula, SignedFormula) {
        let source = sf.source;
        match (&sf.formula, sf.sign) {
            (Formula::And(ref a, ref b), true) => (
                SignedFormula::new(true, (**a).clone(), source),
                SignedFormula::new(true, (**b).clone(), source),
            ),
            (Formula::Or(ref a, ref b), false) => (
                SignedFormula::new(false, (**a).clone(), source),
                SignedFormula::new(false, (**b).clone(), source),
            ),
            (Formula::Implies(ref a, ref b), false) => (
                SignedFormula::new(true, (**a).clone(), source),
                SignedFormula::new(false, (**b).clone(), source),
            ),
            (Formula::Iff(ref a, ref b), true) => (
                SignedFormula::new(true, Formula::Implies(Box::new((**a).clone()), Box::new((**b).clone())), source),
                SignedFormula::new(true, Formula::Implies(Box::new((**b).clone()), Box::new((**a).clone())), source),
            ),
            _ => panic!("Not an alpha formula"),
        }
    }

    /// Get beta rule components
    fn get_beta_components(&self, sf: &SignedFormula) -> (SignedFormula, SignedFormula) {
        let source = sf.source;
        match (&sf.formula, sf.sign) {
            (Formula::And(ref a, ref b), false) => (
                SignedFormula::new(false, (**a).clone(), source),
                SignedFormula::new(false, (**b).clone(), source),
            ),
            (Formula::Or(ref a, ref b), true) => (
                SignedFormula::new(true, (**a).clone(), source),
                SignedFormula::new(true, (**b).clone(), source),
            ),
            (Formula::Implies(ref a, ref b), true) => (
                SignedFormula::new(false, (**a).clone(), source),
                SignedFormula::new(true, (**b).clone(), source),
            ),
            (Formula::Iff(ref a, ref b), false) => (
                SignedFormula::new(false, Formula::Implies(Box::new((**a).clone()), Box::new((**b).clone())), source),
                SignedFormula::new(false, Formula::Implies(Box::new((**b).clone()), Box::new((**a).clone())), source),
            ),
            _ => panic!("Not a beta formula"),
        }
    }

    /// Get gamma rule components
    fn get_gamma_components(&self, sf: &SignedFormula) -> (Variable, Formula, bool) {
        match (&sf.formula, sf.sign) {
            (Formula::Forall(v, ref body), true) => (v.clone(), (**body).clone(), true),
            (Formula::Exists(v, ref body), false) => (v.clone(), (**body).clone(), false),
            _ => panic!("Not a gamma formula"),
        }
    }

    /// Get delta rule components
    fn get_delta_components(&self, sf: &SignedFormula) -> (Variable, Formula, bool) {
        match (&sf.formula, sf.sign) {
            (Formula::Forall(v, ref body), false) => (v.clone(), (**body).clone(), false),
            (Formula::Exists(v, ref body), true) => (v.clone(), (**body).clone(), true),
            _ => panic!("Not a delta formula"),
        }
    }

    /// Substitute a variable in a formula
    fn substitute_var(&self, formula: &Formula, var: &Variable, term: &FolTerm) -> Formula {
        match formula {
            Formula::Atom(atom) => {
                Formula::Atom(Atom {
                    predicate: atom.predicate.clone(),
                    args: atom.args.iter().map(|a| self.subst_term(a, var, term)).collect(),
                })
            }
            Formula::Not(ref f) => Formula::Not(Box::new(self.substitute_var(f, var, term))),
            Formula::And(ref a, ref b) => Formula::And(
                Box::new(self.substitute_var(a, var, term)),
                Box::new(self.substitute_var(b, var, term)),
            ),
            Formula::Or(ref a, ref b) => Formula::Or(
                Box::new(self.substitute_var(a, var, term)),
                Box::new(self.substitute_var(b, var, term)),
            ),
            Formula::Implies(ref a, ref b) => Formula::Implies(
                Box::new(self.substitute_var(a, var, term)),
                Box::new(self.substitute_var(b, var, term)),
            ),
            Formula::Iff(ref a, ref b) => Formula::Iff(
                Box::new(self.substitute_var(a, var, term)),
                Box::new(self.substitute_var(b, var, term)),
            ),
            Formula::Forall(v, ref body) => {
                if v == var {
                    formula.clone() // Variable is bound
                } else {
                    Formula::Forall(v.clone(), Box::new(self.substitute_var(body, var, term)))
                }
            }
            Formula::Exists(v, ref body) => {
                if v == var {
                    formula.clone() // Variable is bound
                } else {
                    Formula::Exists(v.clone(), Box::new(self.substitute_var(body, var, term)))
                }
            }
        }
    }

    fn subst_term(&self, t: &FolTerm, var: &Variable, replacement: &FolTerm) -> FolTerm {
        match t {
            FolTerm::Var(v) if v == var => replacement.clone(),
            FolTerm::Var(_) => t.clone(),
            FolTerm::Func(f, args) => {
                FolTerm::Func(
                    f.clone(),
                    args.iter().map(|a| self.subst_term(a, var, replacement)).collect(),
                )
            }
        }
    }

    /// Collect free variables on a branch
    fn collect_free_vars(&self, node_id: usize) -> Vec<Variable> {
        let mut vars = Vec::new();
        let mut current = Some(node_id);

        while let Some(id) = current {
            let node = &self.nodes[id];
            self.collect_vars_formula(&node.formula.formula, &mut vars);
            current = node.parent;
        }

        vars
    }

    fn collect_vars_formula(&self, formula: &Formula, vars: &mut Vec<Variable>) {
        match formula {
            Formula::Atom(atom) => {
                for arg in &atom.args {
                    self.collect_vars_term(arg, vars);
                }
            }
            Formula::Not(ref f) => self.collect_vars_formula(f, vars),
            Formula::And(ref a, ref b)
            | Formula::Or(ref a, ref b)
            | Formula::Implies(ref a, ref b)
            | Formula::Iff(ref a, ref b) => {
                self.collect_vars_formula(a, vars);
                self.collect_vars_formula(b, vars);
            }
            Formula::Forall(_, ref body) | Formula::Exists(_, ref body) => {
                self.collect_vars_formula(body, vars);
            }
        }
    }

    fn collect_vars_term(&self, term: &FolTerm, vars: &mut Vec<Variable>) {
        match term {
            FolTerm::Var(v) => {
                if !vars.iter().any(|x| x == v) {
                    vars.push(v.clone());
                }
            }
            FolTerm::Func(_, args) => {
                for arg in args {
                    self.collect_vars_term(arg, vars);
                }
            }
        }
    }

    /// Add a node to the tableau
    fn add_node(&mut self, formula: SignedFormula, parent: Option<usize>) -> usize {
        let id = self.nodes.len();
        self.nodes.push(TableauNode {
            id,
            formula,
            parent,
            closed: false,
        });
        id
    }

    /// Check if a branch can be closed (complementary literals)
    fn check_closure(&self, node_id: usize) -> bool {
        let node = &self.nodes[node_id];

        if let (Some(sign), Some(atom)) = (node.formula.effective_sign(), node.formula.get_atom()) {
            // Look for complementary literal on the branch
            let mut current = node.parent;
            while let Some(pid) = current {
                let pnode = &self.nodes[pid];
                if let (Some(psign), Some(patom)) =
                    (pnode.formula.effective_sign(), pnode.formula.get_atom())
                {
                    if sign != psign && atom.predicate == patom.predicate {
                        // Check if atoms unify (for free-variable tableau)
                        if self.config.free_variable {
                            if unify_atoms(atom, patom).is_some() {
                                return true;
                            }
                        } else if atom == patom {
                            return true;
                        }
                    }
                }
                current = pnode.parent;
            }
        }

        false
    }

    /// Mark a branch as closed
    fn close_branch(&mut self, node_id: usize) {
        let mut current = Some(node_id);
        while let Some(id) = current {
            self.nodes[id].closed = true;
            current = self.nodes[id].parent;
        }
    }

    /// Check if a branch is already closed
    fn is_branch_closed(&self, node_id: usize) -> bool {
        self.nodes[node_id].closed
    }

    /// Check if all branches are closed
    fn all_branches_closed(&self) -> bool {
        // Find leaf nodes and check if they're all closed
        let leaves: Vec<usize> = self
            .nodes
            .iter()
            .enumerate()
            .filter(|(id, _)| !self.nodes.iter().any(|n| n.parent == Some(*id)))
            .map(|(id, _)| id)
            .collect();

        leaves.iter().all(|&id| self.nodes[id].closed)
    }

    /// Parse a simple formula format
    pub fn parse_formula(&mut self, input: &str) -> Result<Formula, String> {
        let input = input.trim();
        self.parse_formula_inner(input)
    }

    fn parse_formula_inner(&mut self, input: &str) -> Result<Formula, String> {
        let input = input.trim();

        // Handle quantifiers
        if input.starts_with("forall ") || input.starts_with("∀") {
            let rest = input.strip_prefix("forall ").or_else(|| input.strip_prefix("∀")).unwrap();
            let dot_pos = rest.find('.').ok_or("Missing . in forall")?;
            let var_name = rest[..dot_pos].trim();
            let body = &rest[dot_pos + 1..];
            self.var_counter += 1;
            return Ok(Formula::Forall(
                Variable { name: var_name.to_string(), id: self.var_counter },
                Box::new(self.parse_formula_inner(body)?),
            ));
        }

        if input.starts_with("exists ") || input.starts_with("∃") {
            let rest = input.strip_prefix("exists ").or_else(|| input.strip_prefix("∃")).unwrap();
            let dot_pos = rest.find('.').ok_or("Missing . in exists")?;
            let var_name = rest[..dot_pos].trim();
            let body = &rest[dot_pos + 1..];
            self.var_counter += 1;
            return Ok(Formula::Exists(
                Variable { name: var_name.to_string(), id: self.var_counter },
                Box::new(self.parse_formula_inner(body)?),
            ));
        }

        // Handle binary connectives (right to left precedence)
        if let Some(pos) = self.find_connective(input, "<->") {
            let left = &input[..pos];
            let right = &input[pos + 3..];
            return Ok(Formula::Iff(
                Box::new(self.parse_formula_inner(left)?),
                Box::new(self.parse_formula_inner(right)?),
            ));
        }

        if let Some(pos) = self.find_connective(input, "->") {
            let left = &input[..pos];
            let right = &input[pos + 2..];
            return Ok(Formula::Implies(
                Box::new(self.parse_formula_inner(left)?),
                Box::new(self.parse_formula_inner(right)?),
            ));
        }

        if let Some(pos) = self.find_connective(input, "|") {
            let left = &input[..pos];
            let right = &input[pos + 1..];
            return Ok(Formula::Or(
                Box::new(self.parse_formula_inner(left)?),
                Box::new(self.parse_formula_inner(right)?),
            ));
        }

        if let Some(pos) = self.find_connective(input, "&") {
            let left = &input[..pos];
            let right = &input[pos + 1..];
            return Ok(Formula::And(
                Box::new(self.parse_formula_inner(left)?),
                Box::new(self.parse_formula_inner(right)?),
            ));
        }

        // Handle negation
        if input.starts_with('~') || input.starts_with('¬') || input.starts_with('-') {
            return Ok(Formula::Not(Box::new(self.parse_formula_inner(&input[1..])?)));
        }

        // Handle parentheses
        if input.starts_with('(') && input.ends_with(')') {
            return self.parse_formula_inner(&input[1..input.len() - 1]);
        }

        // Atomic formula
        self.parse_atom(input).map(Formula::Atom)
    }

    fn find_connective(&self, input: &str, conn: &str) -> Option<usize> {
        let mut depth = 0;
        let chars: Vec<char> = input.chars().collect();

        for i in 0..chars.len() {
            match chars[i] {
                '(' => depth += 1,
                ')' => depth -= 1,
                _ if depth == 0 && input[i..].starts_with(conn) => return Some(i),
                _ => {}
            }
        }
        None
    }

    fn parse_atom(&self, input: &str) -> Result<Atom, String> {
        let input = input.trim();

        if let Some(paren_pos) = input.find('(') {
            let pred_name = &input[..paren_pos];
            let args_str = &input[paren_pos + 1..input.len() - 1];

            let args: Vec<FolTerm> = if args_str.is_empty() {
                Vec::new()
            } else {
                args_str
                    .split(',')
                    .map(|a| self.parse_term(a.trim()))
                    .collect::<Result<Vec<_>, _>>()?
            };

            Ok(Atom {
                predicate: Predicate::new(pred_name, args.len()),
                args,
            })
        } else {
            // Propositional atom
            Ok(Atom {
                predicate: Predicate::new(input, 0),
                args: Vec::new(),
            })
        }
    }

    fn parse_term(&self, input: &str) -> Result<FolTerm, String> {
        let input = input.trim();

        if input.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
            Ok(FolTerm::Var(Variable {
                name: input.to_string(),
                id: 0,
            }))
        } else if let Some(paren_pos) = input.find('(') {
            let func_name = &input[..paren_pos];
            let args_str = &input[paren_pos + 1..input.len() - 1];

            let args: Vec<FolTerm> = if args_str.is_empty() {
                Vec::new()
            } else {
                args_str
                    .split(',')
                    .map(|a| self.parse_term(a.trim()))
                    .collect::<Result<Vec<_>, _>>()?
            };

            Ok(FolTerm::Func(Function::new(func_name, args.len()), args))
        } else {
            // Constant
            Ok(FolTerm::Func(Function::new(input, 0), Vec::new()))
        }
    }

    /// Get statistics
    pub fn stats(&self) -> usize {
        self.nodes.len()
    }
}

impl Default for TableauProver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_propositional() {
        let mut prover = TableauProver::new();

        // Prove: (p -> q) -> (~q -> ~p)
        let formula = Formula::Implies(
            Box::new(Formula::Implies(
                Box::new(Formula::Atom(Atom {
                    predicate: Predicate::new("p", 0),
                    args: vec![],
                })),
                Box::new(Formula::Atom(Atom {
                    predicate: Predicate::new("q", 0),
                    args: vec![],
                })),
            )),
            Box::new(Formula::Implies(
                Box::new(Formula::Not(Box::new(Formula::Atom(Atom {
                    predicate: Predicate::new("q", 0),
                    args: vec![],
                })))),
                Box::new(Formula::Not(Box::new(Formula::Atom(Atom {
                    predicate: Predicate::new("p", 0),
                    args: vec![],
                })))),
            )),
        );

        prover.add_formula(formula);
        match prover.prove() {
            TableauResult::Closed { .. } => {}
            _ => panic!("Should be valid"),
        }
    }
}
