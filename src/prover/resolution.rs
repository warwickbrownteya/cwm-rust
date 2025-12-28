//! Resolution inference rules
//!
//! Implements binary resolution, factoring, and paramodulation.

use super::term::{FolTerm, Atom};
use super::clause::{Literal, Clause, ClauseDerivation};
use super::unify::{Substitution, unify_atoms};

/// Perform binary resolution between two clauses
///
/// Given clauses C1 = {L1, ...} and C2 = {L2, ...} where L1 and ¬L2 unify,
/// produces the resolvent (C1 - {L1}) ∪ σ(C2 - {L2}) where σ is the MGU.
pub fn resolve(
    clause1: &Clause,
    lit1_idx: usize,
    clause2: &Clause,
    lit2_idx: usize,
    new_id: usize,
) -> Option<Clause> {
    if lit1_idx >= clause1.literals.len() || lit2_idx >= clause2.literals.len() {
        return None;
    }

    let lit1 = &clause1.literals[lit1_idx];
    let lit2 = &clause2.literals[lit2_idx];

    // Check if literals are complementary
    if lit1.negated == lit2.negated {
        return None;
    }

    // Try to unify the atoms
    let mgu = unify_atoms(&lit1.atom, &lit2.atom)?;

    // Build the resolvent
    let mut new_literals: Vec<Literal> = Vec::new();

    // Add literals from clause1, except the resolved one
    for (i, lit) in clause1.literals.iter().enumerate() {
        if i != lit1_idx {
            new_literals.push(lit.apply_substitution(&mgu));
        }
    }

    // Add literals from clause2, except the resolved one
    for (i, lit) in clause2.literals.iter().enumerate() {
        if i != lit2_idx {
            let applied = lit.apply_substitution(&mgu);
            // Avoid adding duplicates
            if !new_literals.contains(&applied) {
                new_literals.push(applied);
            }
        }
    }

    let weight = new_literals.iter().map(|l| l.weight()).sum();

    Some(Clause {
        literals: new_literals,
        id: new_id,
        weight,
        derivation: Some(ClauseDerivation::Resolution {
            clause1: clause1.id,
            clause2: clause2.id,
            lit1_idx,
            lit2_idx,
        }),
        is_goal: clause1.is_goal || clause2.is_goal,
    })
}

/// Perform all possible resolutions between two clauses
pub fn resolve_all(
    clause1: &Clause,
    clause2: &Clause,
    next_id: &mut usize,
) -> Vec<Clause> {
    let mut resolvents = Vec::new();

    // Standardize clause2 apart from clause1
    let clause2_renamed = clause2.standardize_apart(next_id);

    for (i, lit1) in clause1.literals.iter().enumerate() {
        for (j, lit2) in clause2_renamed.literals.iter().enumerate() {
            // Look for complementary literals
            if lit1.negated != lit2.negated && lit1.atom.predicate == lit2.atom.predicate {
                if let Some(resolvent) = resolve(clause1, i, &clause2_renamed, j, *next_id) {
                    *next_id += 1;
                    resolvents.push(resolvent);
                }
            }
        }
    }

    resolvents
}

/// Perform factoring within a clause
///
/// Given clause C = {L1, L2, ...} where L1 and L2 unify with MGU σ,
/// produces σ(C - {L2})
pub fn factor(clause: &Clause, lit1_idx: usize, lit2_idx: usize, new_id: usize) -> Option<Clause> {
    if lit1_idx >= clause.literals.len() || lit2_idx >= clause.literals.len() {
        return None;
    }

    if lit1_idx == lit2_idx {
        return None;
    }

    let lit1 = &clause.literals[lit1_idx];
    let lit2 = &clause.literals[lit2_idx];

    // Both literals must have the same sign
    if lit1.negated != lit2.negated {
        return None;
    }

    // Try to unify the atoms
    let mgu = unify_atoms(&lit1.atom, &lit2.atom)?;

    // Build the factor (remove lit2)
    let mut new_literals: Vec<Literal> = Vec::new();

    for (i, lit) in clause.literals.iter().enumerate() {
        if i != lit2_idx {
            let applied = lit.apply_substitution(&mgu);
            if !new_literals.contains(&applied) {
                new_literals.push(applied);
            }
        }
    }

    let weight = new_literals.iter().map(|l| l.weight()).sum();

    Some(Clause {
        literals: new_literals,
        id: new_id,
        weight,
        derivation: Some(ClauseDerivation::Factor {
            clause: clause.id,
            lit1_idx,
            lit2_idx,
        }),
        is_goal: clause.is_goal,
    })
}

/// Find all factors of a clause
pub fn factor_all(clause: &Clause, next_id: &mut usize) -> Vec<Clause> {
    let mut factors = Vec::new();

    for i in 0..clause.literals.len() {
        for j in (i + 1)..clause.literals.len() {
            if let Some(f) = factor(clause, i, j, *next_id) {
                *next_id += 1;
                factors.push(f);
            }
        }
    }

    factors
}

/// Perform hyperresolution
///
/// Given a nucleus (clause with exactly one positive literal) and
/// satellites (clauses with at least one positive literal that resolves with negative literals),
/// produces a clause with all negative literals resolved.
pub fn hyperresolve(
    nucleus: &Clause,
    satellites: &[&Clause],
    next_id: &mut usize,
) -> Option<Clause> {
    // The nucleus should have exactly one positive literal
    let positive_lits: Vec<_> = nucleus.literals.iter()
        .enumerate()
        .filter(|(_, l)| l.is_positive())
        .collect();

    if positive_lits.len() != 1 {
        return None;
    }

    let negative_lits: Vec<_> = nucleus.literals.iter()
        .enumerate()
        .filter(|(_, l)| l.is_negative())
        .collect();

    if negative_lits.len() != satellites.len() {
        return None;
    }

    let mut current = nucleus.clone();
    let mut subst = Substitution::new();

    for (neg_idx, neg_lit) in negative_lits {
        let satellite = satellites[neg_idx];

        // Find a positive literal in the satellite that unifies with the negative literal
        let mut found = false;
        for (sat_idx, sat_lit) in satellite.literals.iter().enumerate() {
            if sat_lit.is_positive() && sat_lit.atom.predicate == neg_lit.atom.predicate {
                let neg_atom = subst.apply_atom(&neg_lit.atom);
                let sat_atom = subst.apply_atom(&sat_lit.atom);

                if let Some(mgu) = unify_atoms(&neg_atom, &sat_atom) {
                    // Resolve
                    if let Some(resolvent) = resolve(&current, neg_idx, satellite, sat_idx, *next_id) {
                        *next_id += 1;
                        current = resolvent;
                        subst = subst.compose(&mgu);
                        found = true;
                        break;
                    }
                }
            }
        }

        if !found {
            return None;
        }
    }

    // The result should have only positive literals
    if current.literals.iter().all(|l| l.is_positive()) {
        Some(current)
    } else {
        None
    }
}

/// Perform paramodulation (equality reasoning)
///
/// Given clause C1 containing L = R (or R = L) and clause C2 containing a term t,
/// if L unifies with a subterm of t, produces a clause where that subterm is replaced by σ(R).
pub fn paramodulate(
    from_clause: &Clause,
    eq_lit_idx: usize,
    into_clause: &Clause,
    target_lit_idx: usize,
    next_id: usize,
) -> Vec<Clause> {
    let mut results = Vec::new();

    if eq_lit_idx >= from_clause.literals.len() || target_lit_idx >= into_clause.literals.len() {
        return results;
    }

    let eq_lit = &from_clause.literals[eq_lit_idx];

    // The equality literal must be positive and an equality
    if !eq_lit.is_positive() || !eq_lit.is_equality() {
        return results;
    }

    let left = &eq_lit.atom.args[0];
    let right = &eq_lit.atom.args[1];

    let target_lit = &into_clause.literals[target_lit_idx];

    // Try paramodulating L = R into each position
    for (arg_idx, arg) in target_lit.atom.args.iter().enumerate() {
        // Try left -> right
        if let Some(new_term) = try_paramodulate_term(arg, left, right) {
            if let Some(clause) = build_paramodulated_clause(
                from_clause,
                eq_lit_idx,
                into_clause,
                target_lit_idx,
                arg_idx,
                &new_term,
                next_id,
            ) {
                results.push(clause);
            }
        }

        // Try right -> left
        if let Some(new_term) = try_paramodulate_term(arg, right, left) {
            if let Some(clause) = build_paramodulated_clause(
                from_clause,
                eq_lit_idx,
                into_clause,
                target_lit_idx,
                arg_idx,
                &new_term,
                next_id,
            ) {
                results.push(clause);
            }
        }
    }

    results
}

/// Try to paramodulate at any position in a term
fn try_paramodulate_term(term: &FolTerm, from: &FolTerm, to: &FolTerm) -> Option<FolTerm> {
    // Try at the root
    if let Some(subst) = super::unify::unify(term, from) {
        return Some(subst.apply_term(to));
    }

    // Try in subterms
    if let FolTerm::Func(f, args) = term {
        for (i, arg) in args.iter().enumerate() {
            if let Some(new_arg) = try_paramodulate_term(arg, from, to) {
                let mut new_args = args.clone();
                new_args[i] = new_arg;
                return Some(FolTerm::Func(f.clone(), new_args));
            }
        }
    }

    None
}

/// Build the result of paramodulation
fn build_paramodulated_clause(
    from_clause: &Clause,
    eq_lit_idx: usize,
    into_clause: &Clause,
    target_lit_idx: usize,
    arg_idx: usize,
    new_term: &FolTerm,
    new_id: usize,
) -> Option<Clause> {
    let target_lit = &into_clause.literals[target_lit_idx];

    // Build new literal with replaced term
    let mut new_args = target_lit.atom.args.clone();
    new_args[arg_idx] = new_term.clone();

    let new_lit = Literal {
        atom: Atom {
            predicate: target_lit.atom.predicate.clone(),
            args: new_args,
        },
        negated: target_lit.negated,
    };

    // Build new clause
    let mut new_literals: Vec<Literal> = Vec::new();

    // Add literals from from_clause except the equality
    for (i, lit) in from_clause.literals.iter().enumerate() {
        if i != eq_lit_idx {
            if !new_literals.contains(lit) {
                new_literals.push(lit.clone());
            }
        }
    }

    // Add literals from into_clause with the modified one
    for (i, lit) in into_clause.literals.iter().enumerate() {
        if i == target_lit_idx {
            if !new_literals.contains(&new_lit) {
                new_literals.push(new_lit.clone());
            }
        } else if !new_literals.contains(lit) {
            new_literals.push(lit.clone());
        }
    }

    let weight = new_literals.iter().map(|l| l.weight()).sum();

    Some(Clause {
        literals: new_literals,
        id: new_id,
        weight,
        derivation: Some(ClauseDerivation::Paramodulation {
            from: from_clause.id,
            into: into_clause.id,
        }),
        is_goal: into_clause.is_goal,
    })
}

/// Demodulate a clause using a set of equalities
///
/// Rewrites terms using oriented equalities (demodulators)
pub fn demodulate(clause: &Clause, demodulators: &[Clause]) -> Clause {
    let mut current = clause.clone();
    let mut changed = true;

    while changed {
        changed = false;

        for demod in demodulators {
            // Find an equality literal
            for lit in &demod.literals {
                if lit.is_positive() && lit.is_equality() && demod.literals.len() == 1 {
                    let left = &lit.atom.args[0];
                    let right = &lit.atom.args[1];

                    // Orient the equality (larger term on left)
                    let (from, to) = if left.size() >= right.size() {
                        (left, right)
                    } else {
                        (right, left)
                    };

                    // Try to apply to each literal - collect changes first
                    let mut replacements: Vec<(usize, Literal)> = Vec::new();
                    for (i, target_lit) in current.literals.iter().enumerate() {
                        for (j, arg) in target_lit.atom.args.iter().enumerate() {
                            if let Some(new_term) = try_demodulate_term(arg, from, to) {
                                let mut new_args = target_lit.atom.args.clone();
                                new_args[j] = new_term;

                                let new_lit = Literal {
                                    atom: Atom {
                                        predicate: target_lit.atom.predicate.clone(),
                                        args: new_args,
                                    },
                                    negated: target_lit.negated,
                                };

                                replacements.push((i, new_lit));
                            }
                        }
                    }
                    // Apply collected changes
                    for (i, new_lit) in replacements {
                        current.literals[i] = new_lit;
                        changed = true;
                    }
                }
            }
        }
    }

    current.weight = current.literals.iter().map(|l| l.weight()).sum();
    current
}

/// Try to demodulate a term (one-way matching)
fn try_demodulate_term(term: &FolTerm, from: &FolTerm, to: &FolTerm) -> Option<FolTerm> {
    // Try at the root using matching (not unification)
    if let Some(subst) = super::unify::match_term(from, term) {
        return Some(subst.apply_term(to));
    }

    // Try in subterms
    if let FolTerm::Func(f, args) = term {
        for (i, arg) in args.iter().enumerate() {
            if let Some(new_arg) = try_demodulate_term(arg, from, to) {
                let mut new_args = args.clone();
                new_args[i] = new_arg;
                return Some(FolTerm::Func(f.clone(), new_args));
            }
        }
    }

    None
}

/// Ordered resolution - only resolve on maximal literals
pub fn ordered_resolve(
    clause1: &Clause,
    clause2: &Clause,
    next_id: &mut usize,
) -> Vec<Clause> {
    let mut resolvents = Vec::new();

    let clause2_renamed = clause2.standardize_apart(next_id);

    // Find maximal literals
    let max1 = clause1.literals.iter().map(|l| l.weight()).max().unwrap_or(0);
    let max2 = clause2_renamed.literals.iter().map(|l| l.weight()).max().unwrap_or(0);

    for (i, lit1) in clause1.literals.iter().enumerate() {
        // Only resolve on maximal literals
        if lit1.weight() < max1 {
            continue;
        }

        for (j, lit2) in clause2_renamed.literals.iter().enumerate() {
            if lit2.weight() < max2 {
                continue;
            }

            if lit1.negated != lit2.negated && lit1.atom.predicate == lit2.atom.predicate {
                if let Some(resolvent) = resolve(clause1, i, &clause2_renamed, j, *next_id) {
                    *next_id += 1;
                    resolvents.push(resolvent);
                }
            }
        }
    }

    resolvents
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binary_resolution() {
        // P(a) and -P(a) | Q(b) should give Q(b)
        let p_a = Literal::positive(Atom::new("P", vec![FolTerm::constant("a")]));
        let not_p_a = Literal::negative(Atom::new("P", vec![FolTerm::constant("a")]));
        let q_b = Literal::positive(Atom::new("Q", vec![FolTerm::constant("b")]));

        let clause1 = Clause::new(vec![p_a], 1);
        let clause2 = Clause::new(vec![not_p_a, q_b.clone()], 2);

        let resolvent = resolve(&clause1, 0, &clause2, 0, 3);
        assert!(resolvent.is_some());

        let r = resolvent.unwrap();
        assert_eq!(r.literals.len(), 1);
        assert_eq!(format!("{}", r.literals[0]), "Q(b)");
    }

    #[test]
    fn test_resolution_with_unification() {
        // P(x) and -P(a) | Q(x) should give Q(a)
        let p_x = Literal::positive(Atom::new("P", vec![FolTerm::var("x", 1)]));
        let not_p_a = Literal::negative(Atom::new("P", vec![FolTerm::constant("a")]));
        let q_x = Literal::positive(Atom::new("Q", vec![FolTerm::var("x", 1)]));

        let clause1 = Clause::new(vec![p_x], 1);
        let clause2 = Clause::new(vec![not_p_a, q_x], 2);

        let resolvent = resolve(&clause1, 0, &clause2, 0, 3);
        assert!(resolvent.is_some());

        let r = resolvent.unwrap();
        assert_eq!(r.literals.len(), 1);
        assert_eq!(format!("{}", r.literals[0]), "Q(a)");
    }

    #[test]
    fn test_factoring() {
        // P(x) | P(a) should factor to P(a)
        let p_x = Literal::positive(Atom::new("P", vec![FolTerm::var("x", 1)]));
        let p_a = Literal::positive(Atom::new("P", vec![FolTerm::constant("a")]));

        let clause = Clause::new(vec![p_x, p_a], 1);
        let factored = factor(&clause, 0, 1, 2);

        assert!(factored.is_some());
        let f = factored.unwrap();
        assert_eq!(f.literals.len(), 1);
    }

    #[test]
    fn test_derive_empty_clause() {
        // P(a) and -P(a) should give empty clause
        let p_a = Literal::positive(Atom::new("P", vec![FolTerm::constant("a")]));
        let not_p_a = Literal::negative(Atom::new("P", vec![FolTerm::constant("a")]));

        let clause1 = Clause::new(vec![p_a], 1);
        let clause2 = Clause::new(vec![not_p_a], 2);

        let resolvent = resolve(&clause1, 0, &clause2, 0, 3);
        assert!(resolvent.is_some());
        assert!(resolvent.unwrap().is_empty());
    }
}
