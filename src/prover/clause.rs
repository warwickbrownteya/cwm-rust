//! Clause representation and CNF conversion
//!
//! A clause is a disjunction of literals.
//! A literal is a possibly negated atom.

use std::collections::{HashMap, HashSet};
use std::fmt;
use super::term::{FolTerm, Variable, Atom};
use super::unify::Substitution;

/// A literal is a possibly negated atom
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Literal {
    /// The underlying atom
    pub atom: Atom,
    /// Whether this literal is negated
    pub negated: bool,
}

impl Literal {
    /// Create a positive literal
    pub fn positive(atom: Atom) -> Self {
        Literal {
            atom,
            negated: false,
        }
    }

    /// Create a negative literal
    pub fn negative(atom: Atom) -> Self {
        Literal {
            atom,
            negated: true,
        }
    }

    /// Create a literal from an atom and sign
    pub fn new(atom: Atom, negated: bool) -> Self {
        Literal { atom, negated }
    }

    /// Negate this literal
    pub fn negate(&self) -> Literal {
        Literal {
            atom: self.atom.clone(),
            negated: !self.negated,
        }
    }

    /// Check if this is a positive literal
    pub fn is_positive(&self) -> bool {
        !self.negated
    }

    /// Check if this is a negative literal
    pub fn is_negative(&self) -> bool {
        self.negated
    }

    /// Check if this literal is ground
    pub fn is_ground(&self) -> bool {
        self.atom.is_ground()
    }

    /// Get all variables in this literal
    pub fn variables(&self) -> HashSet<Variable> {
        self.atom.variables()
    }

    /// Apply a substitution to this literal
    pub fn apply_substitution(&self, subst: &Substitution) -> Literal {
        Literal {
            atom: subst.apply_atom(&self.atom),
            negated: self.negated,
        }
    }

    /// Rename variables
    pub fn rename_variables(&self, suffix: &str) -> Literal {
        Literal {
            atom: self.atom.rename_variables(suffix),
            negated: self.negated,
        }
    }

    /// Standardize variables apart
    pub fn standardize_apart(&self, counter: &mut usize, mapping: &mut HashMap<Variable, Variable>) -> Literal {
        Literal {
            atom: self.atom.standardize_apart(counter, mapping),
            negated: self.negated,
        }
    }

    /// Check if this literal complements another
    pub fn complements(&self, other: &Literal) -> bool {
        self.negated != other.negated && self.atom.predicate == other.atom.predicate
    }

    /// Get the weight of this literal
    pub fn weight(&self) -> usize {
        self.atom.weight()
    }

    /// Check if this is an equality literal
    pub fn is_equality(&self) -> bool {
        self.atom.predicate.is_equality()
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.negated {
            write!(f, "-{}", self.atom)
        } else {
            write!(f, "{}", self.atom)
        }
    }
}

/// A clause is a disjunction of literals
#[derive(Debug, Clone)]
pub struct Clause {
    /// The literals in this clause
    pub literals: Vec<Literal>,
    /// Unique identifier
    pub id: usize,
    /// Weight for clause selection
    pub weight: usize,
    /// How this clause was derived
    pub derivation: Option<ClauseDerivation>,
    /// Whether this is a goal clause (from negated conjecture)
    pub is_goal: bool,
}

/// How a clause was derived
#[derive(Debug, Clone)]
pub enum ClauseDerivation {
    Input,
    Resolution {
        clause1: usize,
        clause2: usize,
        lit1_idx: usize,
        lit2_idx: usize,
    },
    Factor {
        clause: usize,
        lit1_idx: usize,
        lit2_idx: usize,
    },
    Paramodulation {
        from: usize,
        into: usize,
    },
}

impl Clause {
    /// Create a new clause with literals
    pub fn new(literals: Vec<Literal>, id: usize) -> Self {
        let weight = literals.iter().map(|l| l.weight()).sum();
        Clause {
            literals,
            id,
            weight,
            derivation: None,
            is_goal: false,
        }
    }

    /// Create an empty (false) clause
    pub fn empty(id: usize) -> Self {
        Clause {
            literals: vec![],
            id,
            weight: 0,
            derivation: None,
            is_goal: false,
        }
    }

    /// Create a unit clause (single literal)
    pub fn unit(literal: Literal, id: usize) -> Self {
        let weight = literal.weight();
        Clause {
            literals: vec![literal],
            id,
            weight,
            derivation: None,
            is_goal: false,
        }
    }

    /// Check if this is the empty clause
    pub fn is_empty(&self) -> bool {
        self.literals.is_empty()
    }

    /// Check if this is a unit clause
    pub fn is_unit(&self) -> bool {
        self.literals.len() == 1
    }

    /// Check if this is a Horn clause (at most one positive literal)
    pub fn is_horn(&self) -> bool {
        self.literals.iter().filter(|l| l.is_positive()).count() <= 1
    }

    /// Check if this is a goal clause (all negative literals)
    pub fn is_goal_clause(&self) -> bool {
        self.literals.iter().all(|l| l.is_negative())
    }

    /// Check if this clause is ground
    pub fn is_ground(&self) -> bool {
        self.literals.iter().all(|l| l.is_ground())
    }

    /// Get all variables in this clause
    pub fn variables(&self) -> HashSet<Variable> {
        let mut vars = HashSet::new();
        for lit in &self.literals {
            for v in lit.variables() {
                vars.insert(v);
            }
        }
        vars
    }

    /// Apply a substitution to this clause
    pub fn apply_substitution(&self, subst: &Substitution) -> Clause {
        let literals: Vec<Literal> = self.literals
            .iter()
            .map(|l| l.apply_substitution(subst))
            .collect();
        let weight = literals.iter().map(|l| l.weight()).sum();

        Clause {
            literals,
            id: self.id,
            weight,
            derivation: self.derivation.clone(),
            is_goal: self.is_goal,
        }
    }

    /// Rename variables in this clause
    pub fn rename_variables(&self, suffix: &str) -> Clause {
        Clause {
            literals: self.literals.iter().map(|l| l.rename_variables(suffix)).collect(),
            id: self.id,
            weight: self.weight,
            derivation: self.derivation.clone(),
            is_goal: self.is_goal,
        }
    }

    /// Standardize variables apart
    pub fn standardize_apart(&self, counter: &mut usize) -> Clause {
        let mut mapping = HashMap::new();
        Clause {
            literals: self.literals
                .iter()
                .map(|l| l.standardize_apart(counter, &mut mapping))
                .collect(),
            id: self.id,
            weight: self.weight,
            derivation: self.derivation.clone(),
            is_goal: self.is_goal,
        }
    }

    /// Check if this clause subsumes another (is more general)
    pub fn subsumes(&self, other: &Clause) -> bool {
        if self.literals.len() > other.literals.len() {
            return false;
        }

        // Try to find a substitution that makes self a subset of other
        self.try_subsumption(&self.literals, &other.literals, &Substitution::new())
    }

    fn try_subsumption(&self, remaining: &[Literal], targets: &[Literal], subst: &Substitution) -> bool {
        if remaining.is_empty() {
            return true;
        }

        let lit = &remaining[0];
        let rest = &remaining[1..];

        for (i, target) in targets.iter().enumerate() {
            if lit.negated == target.negated {
                let lit_atom = subst.apply_atom(&lit.atom);
                if let Some(new_subst) = super::unify::match_term(
                    &FolTerm::func("_pred_", lit_atom.args.clone()),
                    &FolTerm::func("_pred_", target.atom.args.clone()),
                ) {
                    if lit.atom.predicate == target.atom.predicate {
                        let composed = subst.compose(&new_subst);
                        let remaining_targets: Vec<Literal> = targets.iter()
                            .enumerate()
                            .filter(|(j, _)| *j != i)
                            .map(|(_, l)| l.clone())
                            .collect();

                        if self.try_subsumption(rest, &remaining_targets, &composed) {
                            return true;
                        }
                    }
                }
            }
        }

        false
    }

    /// Remove duplicate literals
    pub fn remove_duplicates(&mut self) {
        let mut seen = HashSet::new();
        self.literals.retain(|l| {
            let key = format!("{}", l);
            if seen.contains(&key) {
                false
            } else {
                seen.insert(key);
                true
            }
        });
        self.weight = self.literals.iter().map(|l| l.weight()).sum();
    }

    /// Check for tautology (complementary literals)
    /// A clause is a tautology only if it contains a literal and its exact negation
    /// e.g., {P(a), -P(a)} is a tautology, but {P(x), -P(y)} is not
    pub fn is_tautology(&self) -> bool {
        for (i, lit1) in self.literals.iter().enumerate() {
            for lit2 in self.literals.iter().skip(i + 1) {
                if lit1.complements(lit2) {
                    // Only a tautology if atoms are identical, not just unifiable
                    if lit1.atom == lit2.atom {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Get the positive literals
    pub fn positive_literals(&self) -> impl Iterator<Item = &Literal> {
        self.literals.iter().filter(|l| l.is_positive())
    }

    /// Get the negative literals
    pub fn negative_literals(&self) -> impl Iterator<Item = &Literal> {
        self.literals.iter().filter(|l| l.is_negative())
    }

    /// Get the maximal literal (for ordered resolution)
    pub fn maximal_literal(&self) -> Option<&Literal> {
        self.literals.iter().max_by_key(|l| l.weight())
    }
}

impl fmt::Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.literals.is_empty() {
            write!(f, "$false")
        } else {
            for (i, lit) in self.literals.iter().enumerate() {
                if i > 0 {
                    write!(f, " | ")?;
                }
                write!(f, "{}", lit)?;
            }
            Ok(())
        }
    }
}

impl PartialEq for Clause {
    fn eq(&self, other: &Self) -> bool {
        if self.literals.len() != other.literals.len() {
            return false;
        }
        // Check if same literals (order independent)
        let self_set: HashSet<_> = self.literals.iter().map(|l| format!("{}", l)).collect();
        let other_set: HashSet<_> = other.literals.iter().map(|l| format!("{}", l)).collect();
        self_set == other_set
    }
}

impl Eq for Clause {}

/// A set of clauses
#[derive(Debug, Clone)]
pub struct ClauseSet {
    /// All clauses
    pub clauses: Vec<Clause>,
    /// Next clause ID
    next_id: usize,
}

impl ClauseSet {
    pub fn new() -> Self {
        ClauseSet {
            clauses: vec![],
            next_id: 1,
        }
    }

    /// Add a clause and return its ID
    pub fn add(&mut self, mut clause: Clause) -> usize {
        clause.id = self.next_id;
        self.next_id += 1;
        let id = clause.id;
        self.clauses.push(clause);
        id
    }

    /// Add input clauses (axioms)
    pub fn add_axiom(&mut self, literals: Vec<Literal>) -> usize {
        let mut clause = Clause::new(literals, 0);
        clause.derivation = Some(ClauseDerivation::Input);
        self.add(clause)
    }

    /// Add goal clauses (negated conjecture)
    pub fn add_goal(&mut self, literals: Vec<Literal>) -> usize {
        let mut clause = Clause::new(literals, 0);
        clause.derivation = Some(ClauseDerivation::Input);
        clause.is_goal = true;
        self.add(clause)
    }

    /// Get a clause by ID
    pub fn get(&self, id: usize) -> Option<&Clause> {
        self.clauses.iter().find(|c| c.id == id)
    }

    /// Check if the set contains the empty clause
    pub fn contains_empty(&self) -> bool {
        self.clauses.iter().any(|c| c.is_empty())
    }

    /// Get the number of clauses
    pub fn len(&self) -> usize {
        self.clauses.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.clauses.is_empty()
    }

    /// Remove subsumed clauses
    pub fn remove_subsumed(&mut self) {
        let mut to_remove = HashSet::new();

        for i in 0..self.clauses.len() {
            if to_remove.contains(&i) {
                continue;
            }
            for j in (i + 1)..self.clauses.len() {
                if to_remove.contains(&j) {
                    continue;
                }
                if self.clauses[i].subsumes(&self.clauses[j]) {
                    to_remove.insert(j);
                } else if self.clauses[j].subsumes(&self.clauses[i]) {
                    to_remove.insert(i);
                    break;
                }
            }
        }

        let mut indices: Vec<_> = to_remove.into_iter().collect();
        indices.sort_by(|a, b| b.cmp(a)); // Sort descending

        for i in indices {
            self.clauses.remove(i);
        }
    }

    /// Remove tautologies
    pub fn remove_tautologies(&mut self) {
        self.clauses.retain(|c| !c.is_tautology());
    }
}

impl Default for ClauseSet {
    fn default() -> Self {
        Self::new()
    }
}

/// First-order formula for CNF conversion
#[derive(Debug, Clone)]
pub enum Formula {
    Atom(Atom),
    Not(Box<Formula>),
    And(Box<Formula>, Box<Formula>),
    Or(Box<Formula>, Box<Formula>),
    Implies(Box<Formula>, Box<Formula>),
    Iff(Box<Formula>, Box<Formula>),
    ForAll(Variable, Box<Formula>),
    Exists(Variable, Box<Formula>),
}

impl Formula {
    /// Convert to negation normal form (NNF)
    pub fn to_nnf(&self) -> Formula {
        match self {
            Formula::Atom(a) => Formula::Atom(a.clone()),

            Formula::Not(inner) => match inner.as_ref() {
                // Double negation elimination
                Formula::Not(f) => f.to_nnf(),

                // De Morgan's laws
                Formula::And(a, b) => Formula::Or(
                    Box::new(Formula::Not(a.clone()).to_nnf()),
                    Box::new(Formula::Not(b.clone()).to_nnf()),
                ),
                Formula::Or(a, b) => Formula::And(
                    Box::new(Formula::Not(a.clone()).to_nnf()),
                    Box::new(Formula::Not(b.clone()).to_nnf()),
                ),

                // Quantifier negation
                Formula::ForAll(v, f) => Formula::Exists(
                    v.clone(),
                    Box::new(Formula::Not(f.clone()).to_nnf()),
                ),
                Formula::Exists(v, f) => Formula::ForAll(
                    v.clone(),
                    Box::new(Formula::Not(f.clone()).to_nnf()),
                ),

                // Implication and biconditional
                Formula::Implies(a, b) => Formula::And(
                    Box::new(a.to_nnf()),
                    Box::new(Formula::Not(b.clone()).to_nnf()),
                ),
                Formula::Iff(a, b) => Formula::Or(
                    Box::new(Formula::And(
                        Box::new(a.to_nnf()),
                        Box::new(Formula::Not(b.clone()).to_nnf()),
                    )),
                    Box::new(Formula::And(
                        Box::new(Formula::Not(a.clone()).to_nnf()),
                        Box::new(b.to_nnf()),
                    )),
                ),

                // Atom
                Formula::Atom(a) => Formula::Not(Box::new(Formula::Atom(a.clone()))),
            },

            Formula::And(a, b) => Formula::And(
                Box::new(a.to_nnf()),
                Box::new(b.to_nnf()),
            ),

            Formula::Or(a, b) => Formula::Or(
                Box::new(a.to_nnf()),
                Box::new(b.to_nnf()),
            ),

            Formula::Implies(a, b) => Formula::Or(
                Box::new(Formula::Not(a.clone()).to_nnf()),
                Box::new(b.to_nnf()),
            ),

            Formula::Iff(a, b) => Formula::And(
                Box::new(Formula::Or(
                    Box::new(Formula::Not(a.clone()).to_nnf()),
                    Box::new(b.to_nnf()),
                )),
                Box::new(Formula::Or(
                    Box::new(Formula::Not(b.clone()).to_nnf()),
                    Box::new(a.to_nnf()),
                )),
            ),

            Formula::ForAll(v, f) => Formula::ForAll(v.clone(), Box::new(f.to_nnf())),
            Formula::Exists(v, f) => Formula::Exists(v.clone(), Box::new(f.to_nnf())),
        }
    }

    /// Skolemize existential quantifiers
    pub fn skolemize(&self, counter: &mut usize, universal_vars: &[Variable]) -> Formula {
        match self {
            Formula::Atom(a) => Formula::Atom(a.clone()),
            Formula::Not(f) => Formula::Not(Box::new(f.skolemize(counter, universal_vars))),
            Formula::And(a, b) => Formula::And(
                Box::new(a.skolemize(counter, universal_vars)),
                Box::new(b.skolemize(counter, universal_vars)),
            ),
            Formula::Or(a, b) => Formula::Or(
                Box::new(a.skolemize(counter, universal_vars)),
                Box::new(b.skolemize(counter, universal_vars)),
            ),
            Formula::ForAll(v, f) => {
                let mut new_vars = universal_vars.to_vec();
                new_vars.push(v.clone());
                Formula::ForAll(v.clone(), Box::new(f.skolemize(counter, &new_vars)))
            }
            Formula::Exists(v, f) => {
                // Replace with Skolem function
                *counter += 1;
                let skolem_name = format!("sk{}", counter);

                let skolem_term = if universal_vars.is_empty() {
                    FolTerm::constant(&skolem_name)
                } else {
                    FolTerm::func(
                        &skolem_name,
                        universal_vars.iter().map(|uv| FolTerm::Var(uv.clone())).collect(),
                    )
                };

                // Substitute and continue
                let substituted = f.substitute_var(v, &skolem_term);
                substituted.skolemize(counter, universal_vars)
            }
            Formula::Implies(_, _) | Formula::Iff(_, _) => {
                panic!("Implications should be eliminated before skolemization")
            }
        }
    }

    /// Substitute a variable with a term
    fn substitute_var(&self, var: &Variable, term: &FolTerm) -> Formula {
        match self {
            Formula::Atom(a) => {
                let new_args: Vec<FolTerm> = a.args
                    .iter()
                    .map(|arg| substitute_term(arg, var, term))
                    .collect();
                Formula::Atom(Atom {
                    predicate: a.predicate.clone(),
                    args: new_args,
                })
            }
            Formula::Not(f) => Formula::Not(Box::new(f.substitute_var(var, term))),
            Formula::And(a, b) => Formula::And(
                Box::new(a.substitute_var(var, term)),
                Box::new(b.substitute_var(var, term)),
            ),
            Formula::Or(a, b) => Formula::Or(
                Box::new(a.substitute_var(var, term)),
                Box::new(b.substitute_var(var, term)),
            ),
            Formula::ForAll(v, f) if v != var => {
                Formula::ForAll(v.clone(), Box::new(f.substitute_var(var, term)))
            }
            Formula::Exists(v, f) if v != var => {
                Formula::Exists(v.clone(), Box::new(f.substitute_var(var, term)))
            }
            Formula::ForAll(_, _) | Formula::Exists(_, _) => self.clone(),
            Formula::Implies(a, b) => Formula::Implies(
                Box::new(a.substitute_var(var, term)),
                Box::new(b.substitute_var(var, term)),
            ),
            Formula::Iff(a, b) => Formula::Iff(
                Box::new(a.substitute_var(var, term)),
                Box::new(b.substitute_var(var, term)),
            ),
        }
    }

    /// Drop universal quantifiers (after skolemization)
    pub fn drop_quantifiers(&self) -> Formula {
        match self {
            Formula::ForAll(_, f) | Formula::Exists(_, f) => f.drop_quantifiers(),
            Formula::And(a, b) => Formula::And(
                Box::new(a.drop_quantifiers()),
                Box::new(b.drop_quantifiers()),
            ),
            Formula::Or(a, b) => Formula::Or(
                Box::new(a.drop_quantifiers()),
                Box::new(b.drop_quantifiers()),
            ),
            Formula::Not(f) => Formula::Not(Box::new(f.drop_quantifiers())),
            _ => self.clone(),
        }
    }

    /// Convert to CNF (after NNF and skolemization)
    pub fn to_cnf(&self) -> Vec<Vec<Literal>> {
        match self {
            Formula::Atom(a) => vec![vec![Literal::positive(a.clone())]],
            Formula::Not(f) => {
                if let Formula::Atom(a) = f.as_ref() {
                    vec![vec![Literal::negative(a.clone())]]
                } else {
                    panic!("Not should only appear before atoms in NNF")
                }
            }
            Formula::And(a, b) => {
                let mut clauses = a.to_cnf();
                clauses.extend(b.to_cnf());
                clauses
            }
            Formula::Or(a, b) => {
                // Distribute: (A ∨ B) where A and B are in CNF
                let a_clauses = a.to_cnf();
                let b_clauses = b.to_cnf();

                let mut result = vec![];
                for ac in &a_clauses {
                    for bc in &b_clauses {
                        let mut new_clause = ac.clone();
                        new_clause.extend(bc.clone());
                        result.push(new_clause);
                    }
                }
                result
            }
            _ => panic!("Unexpected formula in CNF conversion: {:?}", self),
        }
    }

    /// Full conversion to clauses
    pub fn to_clauses(&self, counter: &mut usize) -> Vec<Vec<Literal>> {
        let nnf = self.to_nnf();
        let skolemized = nnf.skolemize(counter, &[]);
        let quantifier_free = skolemized.drop_quantifiers();
        quantifier_free.to_cnf()
    }
}

/// Substitute a variable in a term
fn substitute_term(term: &FolTerm, var: &Variable, replacement: &FolTerm) -> FolTerm {
    match term {
        FolTerm::Var(v) if v == var => replacement.clone(),
        FolTerm::Var(_) => term.clone(),
        FolTerm::Func(f, args) => {
            FolTerm::Func(
                f.clone(),
                args.iter().map(|a| substitute_term(a, var, replacement)).collect(),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal_creation() {
        let atom = Atom::new("p", vec![FolTerm::constant("a")]);
        let pos = Literal::positive(atom.clone());
        let neg = Literal::negative(atom.clone());

        assert!(pos.is_positive());
        assert!(neg.is_negative());
        assert!(pos.complements(&neg));
    }

    #[test]
    fn test_clause_creation() {
        let lit1 = Literal::positive(Atom::new("p", vec![FolTerm::var("x", 1)]));
        let lit2 = Literal::negative(Atom::new("q", vec![FolTerm::var("x", 1)]));

        let clause = Clause::new(vec![lit1, lit2], 1);
        assert!(!clause.is_empty());
        assert!(!clause.is_unit());
        assert!(clause.is_horn()); // One positive, one negative
    }

    #[test]
    fn test_empty_clause() {
        let clause = Clause::empty(1);
        assert!(clause.is_empty());
    }

    #[test]
    fn test_tautology() {
        let atom = Atom::new("p", vec![FolTerm::var("x", 1)]);
        let clause = Clause::new(
            vec![Literal::positive(atom.clone()), Literal::negative(atom)],
            1,
        );
        assert!(clause.is_tautology());
    }

    #[test]
    fn test_formula_to_nnf() {
        // ¬(p ∧ q) should become (¬p ∨ ¬q)
        let p = Formula::Atom(Atom::new("p", vec![]));
        let q = Formula::Atom(Atom::new("q", vec![]));
        let formula = Formula::Not(Box::new(Formula::And(Box::new(p), Box::new(q))));

        let nnf = formula.to_nnf();
        // Check structure
        if let Formula::Or(a, b) = nnf {
            assert!(matches!(a.as_ref(), Formula::Not(_)));
            assert!(matches!(b.as_ref(), Formula::Not(_)));
        } else {
            panic!("Expected Or");
        }
    }

    #[test]
    fn test_formula_to_cnf() {
        // (p ∨ q) ∧ r should give [[p, q], [r]]
        let p = Formula::Atom(Atom::new("p", vec![]));
        let q = Formula::Atom(Atom::new("q", vec![]));
        let r = Formula::Atom(Atom::new("r", vec![]));

        let formula = Formula::And(
            Box::new(Formula::Or(Box::new(p), Box::new(q))),
            Box::new(r),
        );

        let clauses = formula.to_cnf();
        assert_eq!(clauses.len(), 2);
    }
}
