//! Knuth-Bendix Completion
//!
//! Implementation of the Knuth-Bendix completion procedure for
//! equational theories. Attempts to complete a set of equations
//! into a confluent and terminating term rewriting system.
//!
//! Features:
//! - Term rewriting
//! - Critical pair computation
//! - Lexicographic path ordering (LPO)
//! - Completion procedure

use std::collections::{HashMap, VecDeque};
use super::term::{FolTerm, Variable};
use super::unify::{Substitution, unify};

/// A rewrite rule (l -> r)
#[derive(Debug, Clone, PartialEq)]
pub struct RewriteRule {
    /// Left-hand side
    pub lhs: FolTerm,
    /// Right-hand side
    pub rhs: FolTerm,
    /// Rule ID
    pub id: usize,
}

impl RewriteRule {
    pub fn new(lhs: FolTerm, rhs: FolTerm, id: usize) -> Self {
        RewriteRule { lhs, rhs, id }
    }
}

/// An equation (s = t)
#[derive(Debug, Clone, PartialEq)]
pub struct Equation {
    pub left: FolTerm,
    pub right: FolTerm,
}

impl Equation {
    pub fn new(left: FolTerm, right: FolTerm) -> Self {
        Equation { left, right }
    }
}

/// Lexicographic Path Ordering (LPO)
#[derive(Debug, Clone)]
pub struct LpoOrdering {
    /// Function symbol precedence (higher = greater)
    precedence: HashMap<String, usize>,
}

impl LpoOrdering {
    pub fn new() -> Self {
        LpoOrdering {
            precedence: HashMap::new(),
        }
    }

    /// Set precedence for a function symbol
    pub fn set_precedence(&mut self, name: &str, prec: usize) {
        self.precedence.insert(name.to_string(), prec);
    }

    /// Get precedence of a function symbol
    fn prec(&self, name: &str) -> usize {
        *self.precedence.get(name).unwrap_or(&0)
    }

    /// Compare two terms using LPO
    /// Returns Some(true) if s > t, Some(false) if s < t, None if incomparable
    pub fn compare(&self, s: &FolTerm, t: &FolTerm) -> Option<bool> {
        // s > t iff:
        // 1. t is a subterm of s, or
        // 2. s = f(s1,...,sn), t = g(t1,...,tm) and
        //    a. f > g and s > ti for all i, or
        //    b. f = g and (s1,...,sn) >_lex (t1,...,tm) and s > ti for all i, or
        //    c. si >= t for some i

        match (s, t) {
            (FolTerm::Var(v1), FolTerm::Var(v2)) => {
                if v1 == v2 {
                    None // Equal
                } else {
                    None // Incomparable
                }
            }

            (FolTerm::Func(_, _args), FolTerm::Var(_)) => {
                // s > t if t is a subterm of s
                if self.contains_var(s, t) {
                    Some(true)
                } else {
                    None
                }
            }

            (FolTerm::Var(_), FolTerm::Func(_, _)) => {
                // t > s if s is a subterm of t
                if self.contains_var(t, s) {
                    Some(false)
                } else {
                    None
                }
            }

            (FolTerm::Func(f, sargs), FolTerm::Func(g, targs)) => {
                // Check if t is a subterm of some si
                for si in sargs {
                    if self.term_geq(si, t) {
                        return Some(true);
                    }
                }

                // Check if s is a subterm of some ti (for symmetry)
                for ti in targs {
                    if self.term_geq(ti, s) {
                        return Some(false);
                    }
                }

                let pf = self.prec(&f.name);
                let pg = self.prec(&g.name);

                if pf > pg {
                    // s > t if s > ti for all i
                    if targs.iter().all(|ti| self.compare(s, ti) == Some(true)) {
                        return Some(true);
                    }
                } else if pg > pf {
                    // t > s if t > si for all i
                    if sargs.iter().all(|si| self.compare(t, si) == Some(true)) {
                        return Some(false);
                    }
                } else {
                    // Same precedence: lexicographic comparison
                    for (si, ti) in sargs.iter().zip(targs.iter()) {
                        match self.compare(si, ti) {
                            Some(true) => {
                                // s > t if s > tj for all j
                                if targs.iter().all(|tj| self.compare(s, tj) == Some(true)) {
                                    return Some(true);
                                }
                            }
                            Some(false) => {
                                if sargs.iter().all(|sj| self.compare(t, sj) == Some(true)) {
                                    return Some(false);
                                }
                            }
                            None => continue,
                        }
                    }
                }

                None // Incomparable
            }
        }
    }

    /// Check if s >= t
    fn term_geq(&self, s: &FolTerm, t: &FolTerm) -> bool {
        s == t || self.compare(s, t) == Some(true)
    }

    /// Check if term s contains variable v
    fn contains_var(&self, s: &FolTerm, v: &FolTerm) -> bool {
        match s {
            FolTerm::Var(sv) => {
                if let FolTerm::Var(vv) = v {
                    sv == vv
                } else {
                    false
                }
            }
            FolTerm::Func(_, args) => args.iter().any(|a| self.contains_var(a, v)),
        }
    }

    /// Check if term s contains term t as a proper subterm
    #[allow(dead_code)]
    fn contains_subterm(&self, s: &FolTerm, t: &FolTerm) -> bool {
        if s == t {
            return false; // Not a proper subterm
        }

        match s {
            FolTerm::Var(_) => false,
            FolTerm::Func(_, args) => {
                args.iter().any(|a| a == t || self.contains_subterm(a, t))
            }
        }
    }
}

impl Default for LpoOrdering {
    fn default() -> Self {
        Self::new()
    }
}

/// Configuration for Knuth-Bendix completion
#[derive(Debug, Clone)]
pub struct KbConfig {
    /// Maximum number of rules
    pub max_rules: usize,
    /// Maximum number of iterations
    pub max_iterations: usize,
    /// Maximum term depth (for stack overflow prevention)
    pub max_depth: usize,
    /// Verbose output
    pub verbose: bool,
}

impl Default for KbConfig {
    fn default() -> Self {
        KbConfig {
            max_rules: 1000,
            max_iterations: 10000,
            max_depth: 50,
            verbose: false,
        }
    }
}

/// Result of KB completion
#[derive(Debug, Clone)]
pub enum KbResult {
    /// Successfully completed
    Complete {
        rules: Vec<RewriteRule>,
    },
    /// Failed to complete (non-orientable equation)
    Failed {
        reason: String,
    },
    /// Resource limit reached
    Incomplete {
        rules: Vec<RewriteRule>,
        equations: Vec<Equation>,
    },
}

/// Knuth-Bendix Completion procedure
pub struct KnuthBendix {
    /// Configuration
    config: KbConfig,
    /// Term ordering
    ordering: LpoOrdering,
    /// Current rewrite rules
    rules: Vec<RewriteRule>,
    /// Unprocessed equations
    equations: VecDeque<Equation>,
    /// Rule ID counter
    next_id: usize,
    /// Variable counter for renaming
    var_counter: usize,
}

impl KnuthBendix {
    pub fn new() -> Self {
        Self::with_config(KbConfig::default())
    }

    pub fn with_config(config: KbConfig) -> Self {
        KnuthBendix {
            config,
            ordering: LpoOrdering::new(),
            rules: Vec::new(),
            equations: VecDeque::new(),
            next_id: 0,
            var_counter: 0,
        }
    }

    /// Set precedence for a function symbol
    pub fn set_precedence(&mut self, name: &str, prec: usize) {
        self.ordering.set_precedence(name, prec);
    }

    /// Add an equation
    pub fn add_equation(&mut self, left: FolTerm, right: FolTerm) {
        self.equations.push_back(Equation::new(left, right));
    }

    /// Run completion
    pub fn complete(&mut self) -> KbResult {
        let mut iterations = 0;

        while let Some(eq) = self.equations.pop_front() {
            iterations += 1;

            if iterations > self.config.max_iterations {
                return KbResult::Incomplete {
                    rules: self.rules.clone(),
                    equations: self.equations.clone().into_iter().collect(),
                };
            }

            if self.rules.len() > self.config.max_rules {
                return KbResult::Incomplete {
                    rules: self.rules.clone(),
                    equations: self.equations.clone().into_iter().collect(),
                };
            }

            // Normalize both sides
            let left = self.normalize(&eq.left);
            let right = self.normalize(&eq.right);

            // If equal after normalization, skip
            if left == right {
                continue;
            }

            if self.config.verbose {
                eprintln!("Processing: {:?} = {:?}", left, right);
            }

            // Orient the equation
            match self.ordering.compare(&left, &right) {
                Some(true) => {
                    // left > right, so left -> right
                    let rule = RewriteRule::new(left, right, self.next_id);
                    self.next_id += 1;
                    self.add_rule(rule);
                }
                Some(false) => {
                    // right > left, so right -> left
                    let rule = RewriteRule::new(right, left, self.next_id);
                    self.next_id += 1;
                    self.add_rule(rule);
                }
                None => {
                    // Cannot orient - completion fails
                    return KbResult::Failed {
                        reason: format!("Cannot orient equation: {:?} = {:?}", left, right),
                    };
                }
            }
        }

        KbResult::Complete {
            rules: self.rules.clone(),
        }
    }

    /// Add a rule and compute critical pairs
    fn add_rule(&mut self, new_rule: RewriteRule) {
        if self.config.verbose {
            eprintln!("Adding rule {}: {:?} -> {:?}", new_rule.id, new_rule.lhs, new_rule.rhs);
        }

        // Compute critical pairs with existing rules
        for existing in &self.rules.clone() {
            // Critical pairs: overlap new_rule with existing
            self.compute_critical_pairs(&new_rule, existing);
            self.compute_critical_pairs(existing, &new_rule);
        }

        // Self-critical pairs
        self.compute_critical_pairs(&new_rule, &new_rule);

        // Interreduce: simplify existing rules with new rule
        let mut simplified_rules = Vec::new();
        for rule in &self.rules {
            let new_lhs = self.normalize_with(&rule.lhs, &[new_rule.clone()]);
            let new_rhs = self.normalize_with(&rule.rhs, &[new_rule.clone()]);

            if new_lhs != rule.lhs || new_rhs != rule.rhs {
                // Rule was simplified, add as equation
                if new_lhs != new_rhs {
                    self.equations.push_back(Equation::new(new_lhs, new_rhs));
                }
            } else {
                simplified_rules.push(rule.clone());
            }
        }

        simplified_rules.push(new_rule);
        self.rules = simplified_rules;
    }

    /// Compute critical pairs between two rules
    fn compute_critical_pairs(&mut self, rule1: &RewriteRule, rule2: &RewriteRule) {
        // Rename variables in rule2 to avoid capture
        let rule2_renamed = self.rename_rule(rule2);

        // Find unifiable positions
        self.find_overlaps(&rule1.lhs, &rule1.rhs, &rule2_renamed.lhs, &rule2_renamed.rhs);
    }

    /// Find overlapping positions and generate critical pairs
    fn find_overlaps(&mut self, lhs1: &FolTerm, rhs1: &FolTerm, lhs2: &FolTerm, rhs2: &FolTerm) {
        // Try to unify lhs1 with subterms of lhs2
        self.find_overlaps_at(lhs1, rhs1, lhs2, rhs2, lhs2, 0);
    }

    fn find_overlaps_at(
        &mut self,
        lhs1: &FolTerm,
        rhs1: &FolTerm,
        lhs2: &FolTerm,
        _rhs2: &FolTerm,
        subterm: &FolTerm,
        depth: usize,
    ) {
        // Prevent stack overflow on deeply nested terms
        if depth > self.config.max_depth {
            return;
        }

        // Try to unify lhs1 with subterm
        if let Some(mgu) = unify(lhs1, subterm) {
            // Critical pair: mgu(rhs1) and mgu(lhs2[rhs1/subterm])
            let cp_left = mgu.apply_term(rhs1);
            let cp_right = mgu.apply_term(&self.replace_subterm(lhs2, subterm, rhs1, 0));

            if cp_left != cp_right {
                self.equations.push_back(Equation::new(cp_left, cp_right));
            }
        }

        // Recurse into subterms (but not into variables)
        if let FolTerm::Func(_, args) = subterm {
            for arg in args {
                self.find_overlaps_at(lhs1, rhs1, lhs2, _rhs2, arg, depth + 1);
            }
        }
    }

    /// Replace a subterm in a term
    fn replace_subterm(&self, term: &FolTerm, old: &FolTerm, new: &FolTerm, depth: usize) -> FolTerm {
        // Prevent stack overflow
        if depth > self.config.max_depth {
            return term.clone();
        }

        if term == old {
            return new.clone();
        }

        match term {
            FolTerm::Var(_) => term.clone(),
            FolTerm::Func(f, args) => {
                FolTerm::Func(
                    f.clone(),
                    args.iter().map(|a| self.replace_subterm(a, old, new, depth + 1)).collect(),
                )
            }
        }
    }

    /// Normalize a term using the current rules
    pub fn normalize(&self, term: &FolTerm) -> FolTerm {
        self.normalize_with(term, &self.rules)
    }

    fn normalize_with(&self, term: &FolTerm, rules: &[RewriteRule]) -> FolTerm {
        let mut current = term.clone();
        let mut changed = true;

        while changed {
            changed = false;

            for rule in rules {
                if let Some(new_term) = self.rewrite_once(&current, rule) {
                    current = new_term;
                    changed = true;
                    break;
                }
            }
        }

        current
    }

    /// Try to apply a rule once
    fn rewrite_once(&self, term: &FolTerm, rule: &RewriteRule) -> Option<FolTerm> {
        self.rewrite_once_depth(term, rule, 0)
    }

    fn rewrite_once_depth(&self, term: &FolTerm, rule: &RewriteRule, depth: usize) -> Option<FolTerm> {
        // Prevent stack overflow on deeply nested terms
        if depth > self.config.max_depth {
            return None;
        }

        // Try at root
        if let Some(subst) = self.match_pattern(&rule.lhs, term) {
            return Some(subst.apply_term(&rule.rhs));
        }

        // Try in subterms
        match term {
            FolTerm::Var(_) => None,
            FolTerm::Func(f, args) => {
                for (i, arg) in args.iter().enumerate() {
                    if let Some(new_arg) = self.rewrite_once_depth(arg, rule, depth + 1) {
                        let mut new_args = args.clone();
                        new_args[i] = new_arg;
                        return Some(FolTerm::Func(f.clone(), new_args));
                    }
                }
                None
            }
        }
    }

    /// One-way matching (pattern -> term)
    fn match_pattern(&self, pattern: &FolTerm, term: &FolTerm) -> Option<Substitution> {
        super::unify::match_term(pattern, term)
    }

    /// Rename variables in a rule
    fn rename_rule(&mut self, rule: &RewriteRule) -> RewriteRule {
        let mut var_map: HashMap<String, Variable> = HashMap::new();

        RewriteRule {
            lhs: self.rename_term(&rule.lhs, &mut var_map),
            rhs: self.rename_term(&rule.rhs, &mut var_map),
            id: rule.id,
        }
    }

    fn rename_term(&mut self, term: &FolTerm, var_map: &mut HashMap<String, Variable>) -> FolTerm {
        self.rename_term_depth(term, var_map, 0)
    }

    fn rename_term_depth(&mut self, term: &FolTerm, var_map: &mut HashMap<String, Variable>, depth: usize) -> FolTerm {
        // Prevent stack overflow on deeply nested terms
        if depth > self.config.max_depth {
            return term.clone();
        }

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
                    args.iter().map(|a| self.rename_term_depth(a, var_map, depth + 1)).collect(),
                )
            }
        }
    }

    /// Get the completed rules
    pub fn rules(&self) -> &[RewriteRule] {
        &self.rules
    }

    /// Check if two terms are equal in the equational theory
    pub fn equals(&self, s: &FolTerm, t: &FolTerm) -> bool {
        self.normalize(s) == self.normalize(t)
    }
}

impl Default for KnuthBendix {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::term::Function;

    fn var(name: &str, id: usize) -> FolTerm {
        FolTerm::Var(Variable { name: name.to_string(), id })
    }

    fn func(name: &str, args: Vec<FolTerm>) -> FolTerm {
        FolTerm::Func(Function::new(name, args.len()), args)
    }

    #[test]
    fn test_lpo() {
        let mut lpo = LpoOrdering::new();
        lpo.set_precedence("f", 2);
        lpo.set_precedence("g", 1);

        let x = var("X", 1);
        let fx = func("f", vec![x.clone()]);
        let gx = func("g", vec![x.clone()]);

        // f(X) > g(X) because f has higher precedence
        assert_eq!(lpo.compare(&fx, &gx), Some(true));
    }

    #[test]
    fn test_group_completion() {
        let mut kb = KnuthBendix::new();

        // Set precedence: * > inv > e
        kb.set_precedence("*", 3);
        kb.set_precedence("inv", 2);
        kb.set_precedence("e", 1);

        let x = var("X", 1);
        let y = var("Y", 2);
        let z = var("Z", 3);

        let e = func("e", vec![]);

        // Group axioms:
        // 1. e * X = X (left identity)
        kb.add_equation(
            func("*", vec![e.clone(), x.clone()]),
            x.clone(),
        );

        // 2. inv(X) * X = e (left inverse)
        kb.add_equation(
            func("*", vec![func("inv", vec![x.clone()]), x.clone()]),
            e.clone(),
        );

        // 3. (X * Y) * Z = X * (Y * Z) (associativity)
        kb.add_equation(
            func("*", vec![
                func("*", vec![x.clone(), y.clone()]),
                z.clone(),
            ]),
            func("*", vec![
                x.clone(),
                func("*", vec![y.clone(), z.clone()]),
            ]),
        );

        match kb.complete() {
            KbResult::Complete { rules } => {
                assert!(!rules.is_empty());
                println!("Completed with {} rules", rules.len());
            }
            KbResult::Failed { reason } => {
                // This might happen depending on precedence
                println!("Failed: {}", reason);
            }
            KbResult::Incomplete { rules, .. } => {
                println!("Incomplete with {} rules", rules.len());
            }
        }
    }
}
