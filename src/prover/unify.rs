//! Unification algorithm for first-order terms
//!
//! Implements the Martelli-Montanari unification algorithm with occurs check.
//! This is the foundation of resolution-based theorem proving.

use std::collections::HashMap;
use super::term::{FolTerm, Variable, Atom};
use super::clause::Literal;

/// A substitution mapping variables to terms
#[derive(Debug, Clone, Default)]
pub struct Substitution {
    bindings: HashMap<Variable, FolTerm>,
}

impl Substitution {
    /// Create an empty substitution
    pub fn new() -> Self {
        Substitution {
            bindings: HashMap::new(),
        }
    }

    /// Create a substitution with a single binding
    pub fn singleton(var: Variable, term: FolTerm) -> Self {
        let mut s = Substitution::new();
        s.bindings.insert(var, term);
        s
    }

    /// Add a binding to the substitution
    pub fn bind(&mut self, var: Variable, term: FolTerm) {
        // Apply current substitution to the term
        let term = self.apply_term(&term);

        // Apply this binding to all existing bindings
        let new_bindings: HashMap<Variable, FolTerm> = self.bindings
            .iter()
            .map(|(v, t)| {
                let new_t = apply_single_binding(t, &var, &term);
                (v.clone(), new_t)
            })
            .collect();

        self.bindings = new_bindings;
        self.bindings.insert(var, term);
    }

    /// Look up a variable
    pub fn get(&self, var: &Variable) -> Option<&FolTerm> {
        self.bindings.get(var)
    }

    /// Check if the substitution is empty
    pub fn is_empty(&self) -> bool {
        self.bindings.is_empty()
    }

    /// Get the domain (variables) of the substitution
    pub fn domain(&self) -> impl Iterator<Item = &Variable> {
        self.bindings.keys()
    }

    /// Apply this substitution to a term
    pub fn apply_term(&self, term: &FolTerm) -> FolTerm {
        self.apply_term_depth(term, 0)
    }

    fn apply_term_depth(&self, term: &FolTerm, depth: usize) -> FolTerm {
        // Prevent infinite recursion from cyclic substitutions
        if depth > 100 {
            return term.clone();
        }

        match term {
            FolTerm::Var(v) => {
                if let Some(t) = self.bindings.get(v) {
                    // Recursively apply in case of chained bindings
                    self.apply_term_depth(t, depth + 1)
                } else {
                    term.clone()
                }
            }
            FolTerm::Func(f, args) => {
                FolTerm::Func(
                    f.clone(),
                    args.iter().map(|a| self.apply_term_depth(a, depth + 1)).collect(),
                )
            }
        }
    }

    /// Apply this substitution to an atom
    pub fn apply_atom(&self, atom: &Atom) -> Atom {
        Atom {
            predicate: atom.predicate.clone(),
            args: atom.args.iter().map(|a| self.apply_term(a)).collect(),
        }
    }

    /// Apply this substitution to a literal
    pub fn apply_literal(&self, lit: &Literal) -> Literal {
        Literal {
            atom: self.apply_atom(&lit.atom),
            negated: lit.negated,
        }
    }

    /// Compose two substitutions: self ∘ other
    /// (apply other first, then self)
    pub fn compose(&self, other: &Substitution) -> Substitution {
        let mut result = Substitution::new();

        // Apply self to all bindings in other
        for (var, term) in &other.bindings {
            result.bindings.insert(var.clone(), self.apply_term(term));
        }

        // Add bindings from self that aren't in other's domain
        for (var, term) in &self.bindings {
            if !other.bindings.contains_key(var) {
                result.bindings.insert(var.clone(), term.clone());
            }
        }

        result
    }

    /// Restrict substitution to given variables
    pub fn restrict(&self, vars: &std::collections::HashSet<Variable>) -> Substitution {
        Substitution {
            bindings: self.bindings
                .iter()
                .filter(|(v, _)| vars.contains(v))
                .map(|(v, t)| (v.clone(), t.clone()))
                .collect(),
        }
    }
}

/// Apply a single binding to a term
fn apply_single_binding(term: &FolTerm, var: &Variable, replacement: &FolTerm) -> FolTerm {
    match term {
        FolTerm::Var(v) if v == var => replacement.clone(),
        FolTerm::Var(_) => term.clone(),
        FolTerm::Func(f, args) => {
            FolTerm::Func(
                f.clone(),
                args.iter().map(|a| apply_single_binding(a, var, replacement)).collect(),
            )
        }
    }
}

/// Apply a substitution to a term (convenience function)
pub fn apply_substitution(term: &FolTerm, subst: &Substitution) -> FolTerm {
    subst.apply_term(term)
}

/// Apply a substitution to an atom (convenience function)
pub fn apply_substitution_atom(atom: &Atom, subst: &Substitution) -> Atom {
    subst.apply_atom(atom)
}

/// Unify two terms, returning the most general unifier (MGU)
/// Returns None if the terms are not unifiable
pub fn unify(term1: &FolTerm, term2: &FolTerm) -> Option<Substitution> {
    let mut equations = vec![(term1.clone(), term2.clone())];
    let mut subst = Substitution::new();

    while let Some((t1, t2)) = equations.pop() {
        // Apply current substitution
        let t1 = subst.apply_term(&t1);
        let t2 = subst.apply_term(&t2);

        if t1 == t2 {
            // Delete rule: identical terms
            continue;
        }

        match (&t1, &t2) {
            // Orient: put variable on left
            (FolTerm::Func(_, _), FolTerm::Var(_)) => {
                equations.push((t2, t1));
            }

            // Eliminate: variable not in term
            (FolTerm::Var(v), t) => {
                // Occurs check
                if t.contains_var(v) {
                    return None; // Infinite term
                }
                subst.bind(v.clone(), t.clone());
            }

            // Decompose: same function symbol
            (FolTerm::Func(f1, args1), FolTerm::Func(f2, args2)) => {
                if f1.name != f2.name || f1.arity != f2.arity {
                    return None; // Clash
                }
                // Add equations for arguments
                for (a1, a2) in args1.iter().zip(args2.iter()) {
                    equations.push((a1.clone(), a2.clone()));
                }
            }
        }
    }

    Some(subst)
}

/// Unify two atoms
pub fn unify_atoms(atom1: &Atom, atom2: &Atom) -> Option<Substitution> {
    if atom1.predicate != atom2.predicate {
        return None;
    }

    let mut subst = Substitution::new();

    for (t1, t2) in atom1.args.iter().zip(atom2.args.iter()) {
        let t1 = subst.apply_term(t1);
        let t2 = subst.apply_term(t2);

        match unify(&t1, &t2) {
            Some(s) => {
                subst = subst.compose(&s);
            }
            None => return None,
        }
    }

    Some(subst)
}

/// Check if two terms are unifiable (without computing the MGU)
pub fn unifiable(term1: &FolTerm, term2: &FolTerm) -> bool {
    unify(term1, term2).is_some()
}

/// Check if two atoms are unifiable
pub fn unifiable_atoms(atom1: &Atom, atom2: &Atom) -> bool {
    unify_atoms(atom1, atom2).is_some()
}

/// Match a pattern against a term (one-way unification)
/// Variables in the pattern can be bound, but variables in the term are treated as constants
pub fn match_term(pattern: &FolTerm, term: &FolTerm) -> Option<Substitution> {
    let mut equations = vec![(pattern.clone(), term.clone())];
    let mut subst = Substitution::new();

    while let Some((p, t)) = equations.pop() {
        let p = subst.apply_term(&p);

        match (&p, &t) {
            (FolTerm::Var(v), _) => {
                if let Some(existing) = subst.get(v) {
                    if existing != &t {
                        return None;
                    }
                } else {
                    subst.bind(v.clone(), t.clone());
                }
            }
            (FolTerm::Func(f1, args1), FolTerm::Func(f2, args2)) => {
                if f1.name != f2.name || f1.arity != f2.arity {
                    return None;
                }
                for (a1, a2) in args1.iter().zip(args2.iter()) {
                    equations.push((a1.clone(), a2.clone()));
                }
            }
            _ => return None,
        }
    }

    Some(subst)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unify_identical() {
        let t = FolTerm::constant("a");
        let result = unify(&t, &t);
        assert!(result.is_some());
        assert!(result.unwrap().is_empty());
    }

    #[test]
    fn test_unify_var_const() {
        let x = FolTerm::var("x", 1);
        let a = FolTerm::constant("a");

        let result = unify(&x, &a);
        assert!(result.is_some());

        let subst = result.unwrap();
        assert_eq!(subst.apply_term(&x), a);
    }

    #[test]
    fn test_unify_clash() {
        let a = FolTerm::constant("a");
        let b = FolTerm::constant("b");

        let result = unify(&a, &b);
        assert!(result.is_none());
    }

    #[test]
    fn test_unify_occurs_check() {
        let x = FolTerm::var("x", 1);
        let fx = FolTerm::func("f", vec![x.clone()]);

        let result = unify(&x, &fx);
        assert!(result.is_none());
    }

    #[test]
    fn test_unify_function() {
        let x = FolTerm::var("x", 1);
        let y = FolTerm::var("y", 2);
        let a = FolTerm::constant("a");
        let b = FolTerm::constant("b");

        let t1 = FolTerm::func("f", vec![x.clone(), a.clone()]);
        let t2 = FolTerm::func("f", vec![b.clone(), y.clone()]);

        let result = unify(&t1, &t2);
        assert!(result.is_some());

        let subst = result.unwrap();
        assert_eq!(subst.apply_term(&x), b);
        assert_eq!(subst.apply_term(&y), a);
    }

    #[test]
    fn test_unify_atoms() {
        let x = FolTerm::var("x", 1);
        let a = FolTerm::constant("a");

        let atom1 = Atom::new("p", vec![x.clone(), a.clone()]);
        let atom2 = Atom::new("p", vec![a.clone(), a.clone()]);

        let result = unify_atoms(&atom1, &atom2);
        assert!(result.is_some());
    }

    #[test]
    fn test_unify_atoms_different_predicate() {
        let a = FolTerm::constant("a");

        let atom1 = Atom::new("p", vec![a.clone()]);
        let atom2 = Atom::new("q", vec![a.clone()]);

        let result = unify_atoms(&atom1, &atom2);
        assert!(result.is_none());
    }

    #[test]
    fn test_substitution_compose() {
        let x = FolTerm::var("x", 1);
        let y = FolTerm::var("y", 2);
        let a = FolTerm::constant("a");

        let mut s1 = Substitution::new();
        s1.bind(Variable::new("x", 1), y.clone());

        let mut s2 = Substitution::new();
        s2.bind(Variable::new("y", 2), a.clone());

        let composed = s2.compose(&s1);
        assert_eq!(composed.apply_term(&x), a);
    }
}
