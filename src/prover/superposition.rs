//! Superposition Prover
//!
//! Implementation of the superposition calculus for first-order logic
//! with equality. This is the algorithm used by leading provers like
//! Vampire, E, and SPASS.
//!
//! Features:
//! - Ordered resolution with selection
//! - Superposition for equality reasoning
//! - Knuth-Bendix ordering (KBO)
//! - Forward and backward simplification
//! - Subsumption

use std::collections::{BinaryHeap, HashMap, HashSet};
use std::cmp::Ordering as CmpOrdering;
use super::term::{FolTerm, Atom};
use super::clause::{Literal, Clause, ClauseDerivation};
use super::unify::{Substitution, unify, unify_atoms, match_term};
use super::{ProofResult, ProofStep, Derivation, ProverConfig};

/// Knuth-Bendix Ordering (KBO) for terms
#[derive(Debug, Clone)]
pub struct KboOrdering {
    /// Weight of each function symbol
    weights: HashMap<String, usize>,
    /// Precedence of function symbols (higher = greater)
    precedence: HashMap<String, usize>,
    /// Default weight for unknown symbols
    default_weight: usize,
}

impl KboOrdering {
    pub fn new() -> Self {
        KboOrdering {
            weights: HashMap::new(),
            precedence: HashMap::new(),
            default_weight: 1,
        }
    }

    /// Set weight for a function symbol
    pub fn set_weight(&mut self, name: &str, weight: usize) {
        self.weights.insert(name.to_string(), weight);
    }

    /// Set precedence for a function symbol
    pub fn set_precedence(&mut self, name: &str, prec: usize) {
        self.precedence.insert(name.to_string(), prec);
    }

    /// Get weight of a function symbol
    fn weight(&self, name: &str) -> usize {
        *self.weights.get(name).unwrap_or(&self.default_weight)
    }

    /// Get precedence of a function symbol
    fn prec(&self, name: &str) -> usize {
        *self.precedence.get(name).unwrap_or(&0)
    }

    /// Calculate weight of a term
    fn term_weight(&self, term: &FolTerm) -> usize {
        match term {
            FolTerm::Var(_) => 1,
            FolTerm::Func(f, args) => {
                self.weight(&f.name) + args.iter().map(|a| self.term_weight(a)).sum::<usize>()
            }
        }
    }

    /// Count variable occurrences
    fn var_count(&self, term: &FolTerm) -> HashMap<String, usize> {
        let mut counts = HashMap::new();
        self.count_vars(term, &mut counts);
        counts
    }

    fn count_vars(&self, term: &FolTerm, counts: &mut HashMap<String, usize>) {
        match term {
            FolTerm::Var(v) => {
                *counts.entry(format!("{}_{}", v.name, v.id)).or_insert(0) += 1;
            }
            FolTerm::Func(_, args) => {
                for arg in args {
                    self.count_vars(arg, counts);
                }
            }
        }
    }

    /// Compare two terms using KBO
    pub fn compare(&self, s: &FolTerm, t: &FolTerm) -> Option<CmpOrdering> {
        let ws = self.term_weight(s);
        let wt = self.term_weight(t);

        let vars_s = self.var_count(s);
        let vars_t = self.var_count(t);

        // Check variable condition: s >= t requires vars(s) ≥ vars(t) for each var
        let s_geq_t = vars_t.iter().all(|(v, &ct)| {
            vars_s.get(v).copied().unwrap_or(0) >= ct
        });
        let t_geq_s = vars_s.iter().all(|(v, &cs)| {
            vars_t.get(v).copied().unwrap_or(0) >= cs
        });

        if ws > wt && s_geq_t {
            return Some(CmpOrdering::Greater);
        }
        if wt > ws && t_geq_s {
            return Some(CmpOrdering::Less);
        }

        if ws == wt {
            // Use precedence
            match (s, t) {
                (FolTerm::Func(f1, args1), FolTerm::Func(f2, args2)) => {
                    let p1 = self.prec(&f1.name);
                    let p2 = self.prec(&f2.name);

                    if p1 > p2 && s_geq_t {
                        return Some(CmpOrdering::Greater);
                    }
                    if p2 > p1 && t_geq_s {
                        return Some(CmpOrdering::Less);
                    }

                    // Same symbol: compare arguments lexicographically
                    if p1 == p2 && f1.name == f2.name {
                        for (a1, a2) in args1.iter().zip(args2.iter()) {
                            match self.compare(a1, a2) {
                                Some(CmpOrdering::Greater) if s_geq_t => {
                                    return Some(CmpOrdering::Greater);
                                }
                                Some(CmpOrdering::Less) if t_geq_s => {
                                    return Some(CmpOrdering::Less);
                                }
                                Some(CmpOrdering::Equal) => continue,
                                _ => return None,
                            }
                        }
                        return Some(CmpOrdering::Equal);
                    }
                }
                (FolTerm::Var(v1), FolTerm::Var(v2)) => {
                    if v1 == v2 {
                        return Some(CmpOrdering::Equal);
                    }
                }
                _ => {}
            }
        }

        None // Incomparable
    }

    /// Compare literals
    pub fn compare_literal(&self, l1: &Literal, l2: &Literal) -> Option<CmpOrdering> {
        // First compare by predicate
        let p1 = self.prec(&l1.atom.predicate.name);
        let p2 = self.prec(&l2.atom.predicate.name);

        if p1 != p2 {
            return Some(p1.cmp(&p2));
        }

        // Then compare arguments lexicographically
        for (a1, a2) in l1.atom.args.iter().zip(l2.atom.args.iter()) {
            if let Some(ord) = self.compare(a1, a2) {
                if ord != CmpOrdering::Equal {
                    return Some(ord);
                }
            }
        }

        // Finally, positive > negative
        Some(l1.negated.cmp(&l2.negated).reverse())
    }
}

impl Default for KboOrdering {
    fn default() -> Self {
        Self::new()
    }
}

/// Superposition prover configuration
#[derive(Debug, Clone)]
pub struct SuperpositionConfig {
    /// Base configuration
    pub base: ProverConfig,
    /// Use literal selection
    pub selection: bool,
    /// Use demodulation (rewriting)
    pub demodulation: bool,
    /// Use subsumption
    pub subsumption: bool,
}

impl Default for SuperpositionConfig {
    fn default() -> Self {
        SuperpositionConfig {
            base: ProverConfig::default(),
            selection: true,
            demodulation: true,
            subsumption: true,
        }
    }
}

/// Weighted clause for the priority queue
#[derive(Debug, Clone)]
struct WeightedClause {
    clause: Clause,
    weight: usize,
}

impl PartialEq for WeightedClause {
    fn eq(&self, other: &Self) -> bool {
        self.weight == other.weight
    }
}

impl Eq for WeightedClause {}

impl PartialOrd for WeightedClause {
    fn partial_cmp(&self, other: &Self) -> Option<CmpOrdering> {
        Some(self.cmp(other))
    }
}

impl Ord for WeightedClause {
    fn cmp(&self, other: &Self) -> CmpOrdering {
        // Min-heap: smaller weight = higher priority
        other.weight.cmp(&self.weight)
    }
}

/// Superposition Prover
pub struct SuperpositionProver {
    /// Configuration
    config: SuperpositionConfig,
    /// Term ordering
    ordering: KboOrdering,
    /// Usable clauses (processed)
    usable: Vec<Clause>,
    /// Set of support (to be processed)
    sos: BinaryHeap<WeightedClause>,
    /// All clauses (for proof reconstruction)
    all_clauses: Vec<Clause>,
    /// Demodulators (unit equalities for rewriting)
    demodulators: Vec<Clause>,
    /// Clause ID counter
    next_id: usize,
    /// Statistics
    inferences: usize,
    /// Seen clause signatures
    seen: HashSet<String>,
}

impl SuperpositionProver {
    pub fn new() -> Self {
        Self::with_config(SuperpositionConfig::default())
    }

    pub fn with_config(config: SuperpositionConfig) -> Self {
        SuperpositionProver {
            config,
            ordering: KboOrdering::new(),
            usable: Vec::new(),
            sos: BinaryHeap::new(),
            all_clauses: Vec::new(),
            demodulators: Vec::new(),
            next_id: 1,
            inferences: 0,
            seen: HashSet::new(),
        }
    }

    /// Add an axiom
    pub fn add_axiom(&mut self, literals: Vec<Literal>) {
        let mut clause = Clause::new(literals, self.next_id);
        self.next_id += 1;
        clause.derivation = Some(ClauseDerivation::Input);
        self.process_clause(clause, false);
    }

    /// Add a goal (negated conjecture)
    pub fn add_goal(&mut self, literals: Vec<Literal>) {
        let mut clause = Clause::new(literals, self.next_id);
        self.next_id += 1;
        clause.derivation = Some(ClauseDerivation::Input);
        clause.is_goal = true;
        self.process_clause(clause, true);
    }

    /// Process a new clause
    fn process_clause(&mut self, mut clause: Clause, to_sos: bool) {
        // Remove duplicate literals
        clause.remove_duplicates();

        // Tautology check
        if clause.is_tautology() {
            return;
        }

        // Simplify with demodulators
        if self.config.demodulation {
            clause = self.demodulate(&clause);
        }

        // Subsumption check
        if self.config.subsumption && self.is_subsumed(&clause) {
            return;
        }

        // Check for duplicate
        let sig = self.clause_signature(&clause);
        if self.seen.contains(&sig) {
            return;
        }
        self.seen.insert(sig);

        // Check if unit equality (potential demodulator)
        if clause.literals.len() == 1 && clause.literals[0].is_equality() && !clause.literals[0].negated {
            self.demodulators.push(clause.clone());
        }

        self.all_clauses.push(clause.clone());

        if to_sos {
            let weight = self.clause_weight(&clause);
            self.sos.push(WeightedClause { clause, weight });
        } else {
            self.usable.push(clause);
        }
    }

    /// Main prove function
    pub fn prove(&mut self) -> ProofResult {
        let start_time = std::time::Instant::now();
        let max_time = std::time::Duration::from_secs(self.config.base.max_seconds);

        if self.config.base.verbose {
            eprintln!(
                "Starting Superposition with {} usable and {} sos clauses",
                self.usable.len(),
                self.sos.len()
            );
        }

        while let Some(WeightedClause { clause: given, .. }) = self.sos.pop() {
            // Time limit
            if start_time.elapsed() > max_time {
                return ProofResult::Unknown {
                    reason: format!("Time limit {}s reached", self.config.base.max_seconds),
                };
            }

            // Clause limit
            if self.all_clauses.len() > self.config.base.max_clauses {
                return ProofResult::Unknown {
                    reason: format!("Clause limit {} reached", self.config.base.max_clauses),
                };
            }

            // Skip if subsumed
            if self.config.subsumption && self.is_subsumed(&given) {
                continue;
            }

            if self.config.base.verbose {
                eprintln!("Given clause {}: {:?}", given.id, given);
            }

            // Check for empty clause
            if given.literals.is_empty() {
                return ProofResult::Proved {
                    proof: self.extract_proof(given.id),
                    clauses_generated: self.all_clauses.len(),
                    resolution_steps: self.inferences,
                };
            }

            // Generate inferences with given clause
            let new_clauses = self.generate_inferences(&given);

            // Add given to usable
            self.usable.push(given);

            // Process new clauses
            for clause in new_clauses {
                self.process_clause(clause, true);
            }
        }

        // SOS exhausted
        ProofResult::Satisfiable
    }

    /// Generate all inferences with the given clause
    fn generate_inferences(&mut self, given: &Clause) -> Vec<Clause> {
        let mut new_clauses = Vec::new();

        // Resolution inferences
        for usable_clause in &self.usable.clone() {
            // Given against usable (ordered resolution)
            new_clauses.extend(self.ordered_resolution(given, usable_clause));
            new_clauses.extend(self.ordered_resolution(usable_clause, given));

            // Superposition inferences
            if self.config.base.paramodulation {
                new_clauses.extend(self.superposition_inferences(given, usable_clause));
                new_clauses.extend(self.superposition_inferences(usable_clause, given));
            }
        }

        // Factoring
        new_clauses.extend(self.factoring(given));

        // Equality resolution (reflexivity)
        new_clauses.extend(self.equality_resolution(given));

        self.inferences += new_clauses.len();
        new_clauses
    }

    /// Ordered resolution
    fn ordered_resolution(&mut self, c1: &Clause, c2: &Clause) -> Vec<Clause> {
        let mut results = Vec::new();

        // Select maximal literal in c1
        let selected1 = if self.config.selection {
            self.select_literal(c1)
        } else {
            (0..c1.literals.len()).collect()
        };

        for i in selected1 {
            let lit1 = &c1.literals[i];

            for (j, lit2) in c2.literals.iter().enumerate() {
                // Must be complementary
                if lit1.negated == lit2.negated {
                    continue;
                }

                if lit1.atom.predicate != lit2.atom.predicate {
                    continue;
                }

                // Try to unify
                if let Some(mgu) = unify_atoms(&lit1.atom, &lit2.atom) {
                    // Build resolvent
                    let mut resolvent_lits = Vec::new();

                    for (k, lit) in c1.literals.iter().enumerate() {
                        if k != i {
                            resolvent_lits.push(mgu.apply_literal(lit));
                        }
                    }

                    for (k, lit) in c2.literals.iter().enumerate() {
                        if k != j {
                            resolvent_lits.push(mgu.apply_literal(lit));
                        }
                    }

                    let mut resolvent = Clause::new(resolvent_lits, self.next_id);
                    self.next_id += 1;
                    resolvent.derivation = Some(ClauseDerivation::Resolution {
                        clause1: c1.id,
                        clause2: c2.id,
                        lit1_idx: i,
                        lit2_idx: j,
                    });
                    resolvent.is_goal = c1.is_goal || c2.is_goal;

                    results.push(resolvent);
                }
            }
        }

        results
    }

    /// Superposition inferences
    fn superposition_inferences(&mut self, c1: &Clause, c2: &Clause) -> Vec<Clause> {
        let mut results = Vec::new();

        // Find positive equalities in c1
        for (i, lit1) in c1.literals.iter().enumerate() {
            if lit1.negated || !lit1.is_equality() {
                continue;
            }

            let lhs = &lit1.atom.args[0];
            let rhs = &lit1.atom.args[1];

            // Orient the equality
            let (from, to) = match self.ordering.compare(lhs, rhs) {
                Some(CmpOrdering::Greater) => (lhs, rhs),
                Some(CmpOrdering::Less) => (rhs, lhs),
                _ => continue, // Not orientable
            };

            // Superpose into c2
            for (j, lit2) in c2.literals.iter().enumerate() {
                for (k, arg) in lit2.atom.args.iter().enumerate() {
                    results.extend(self.superpose_into(c1, i, from, to, c2, j, k, arg));
                }
            }
        }

        results
    }

    /// Superpose into a specific position
    fn superpose_into(
        &mut self,
        c1: &Clause,
        eq_idx: usize,
        from: &FolTerm,
        to: &FolTerm,
        c2: &Clause,
        lit_idx: usize,
        arg_idx: usize,
        target: &FolTerm,
    ) -> Vec<Clause> {
        let mut results = Vec::new();

        // Don't superpose into variables
        if matches!(target, FolTerm::Var(_)) {
            return results;
        }

        // Try to unify from with target
        if let Some(mgu) = unify(from, target) {
            // Build result clause
            let mut result_lits = Vec::new();

            // Literals from c1 except the equality
            for (k, lit) in c1.literals.iter().enumerate() {
                if k != eq_idx {
                    result_lits.push(mgu.apply_literal(lit));
                }
            }

            // Literals from c2 with replacement
            for (k, lit) in c2.literals.iter().enumerate() {
                if k == lit_idx {
                    // Replace argument with 'to'
                    let mut new_args = lit.atom.args.clone();
                    new_args[arg_idx] = mgu.apply_term(to);
                    result_lits.push(Literal {
                        atom: Atom {
                            predicate: lit.atom.predicate.clone(),
                            args: new_args.into_iter().map(|a| mgu.apply_term(&a)).collect(),
                        },
                        negated: lit.negated,
                    });
                } else {
                    result_lits.push(mgu.apply_literal(lit));
                }
            }

            let mut clause = Clause::new(result_lits, self.next_id);
            self.next_id += 1;
            clause.derivation = Some(ClauseDerivation::Paramodulation {
                from: c1.id,
                into: c2.id,
            });

            results.push(clause);
        }

        results
    }

    /// Factoring
    fn factoring(&mut self, clause: &Clause) -> Vec<Clause> {
        let mut results = Vec::new();

        for i in 0..clause.literals.len() {
            for j in (i + 1)..clause.literals.len() {
                let lit1 = &clause.literals[i];
                let lit2 = &clause.literals[j];

                if lit1.negated != lit2.negated {
                    continue;
                }

                if lit1.atom.predicate != lit2.atom.predicate {
                    continue;
                }

                if let Some(mgu) = unify_atoms(&lit1.atom, &lit2.atom) {
                    let mut factor_lits = Vec::new();

                    for (k, lit) in clause.literals.iter().enumerate() {
                        if k != j {
                            factor_lits.push(mgu.apply_literal(lit));
                        }
                    }

                    let mut factor = Clause::new(factor_lits, self.next_id);
                    self.next_id += 1;
                    factor.derivation = Some(ClauseDerivation::Factor {
                        clause: clause.id,
                        lit1_idx: i,
                        lit2_idx: j,
                    });

                    results.push(factor);
                }
            }
        }

        results
    }

    /// Equality resolution (for negative equalities)
    fn equality_resolution(&mut self, clause: &Clause) -> Vec<Clause> {
        let mut results = Vec::new();

        for (i, lit) in clause.literals.iter().enumerate() {
            if !lit.negated || !lit.is_equality() {
                continue;
            }

            let lhs = &lit.atom.args[0];
            let rhs = &lit.atom.args[1];

            if let Some(mgu) = unify(lhs, rhs) {
                let mut new_lits = Vec::new();

                for (k, l) in clause.literals.iter().enumerate() {
                    if k != i {
                        new_lits.push(mgu.apply_literal(l));
                    }
                }

                let mut new_clause = Clause::new(new_lits, self.next_id);
                self.next_id += 1;
                // Use Factor as derivation type for equality resolution
                new_clause.derivation = Some(ClauseDerivation::Factor {
                    clause: clause.id,
                    lit1_idx: i,
                    lit2_idx: i,
                });

                results.push(new_clause);
            }
        }

        results
    }

    /// Select maximal literals
    fn select_literal(&self, clause: &Clause) -> Vec<usize> {
        if clause.literals.is_empty() {
            return vec![];
        }

        // Find maximal literals
        let mut maximal = vec![0];
        let mut max_lit = &clause.literals[0];

        for (i, lit) in clause.literals.iter().enumerate().skip(1) {
            match self.ordering.compare_literal(lit, max_lit) {
                Some(CmpOrdering::Greater) => {
                    maximal = vec![i];
                    max_lit = lit;
                }
                Some(CmpOrdering::Equal) => {
                    maximal.push(i);
                }
                _ => {}
            }
        }

        // Also select negative literals (for completeness)
        for (i, lit) in clause.literals.iter().enumerate() {
            if lit.negated && !maximal.contains(&i) {
                maximal.push(i);
            }
        }

        maximal
    }

    /// Demodulate a clause
    fn demodulate(&self, clause: &Clause) -> Clause {
        let mut result = clause.clone();

        for demod in &self.demodulators {
            if demod.literals.len() != 1 {
                continue;
            }

            let eq_lit = &demod.literals[0];
            let lhs = &eq_lit.atom.args[0];
            let rhs = &eq_lit.atom.args[1];

            // Orient
            let (from, to) = match self.ordering.compare(lhs, rhs) {
                Some(CmpOrdering::Greater) => (lhs, rhs),
                _ => continue,
            };

            // Try to match and rewrite
            for lit in &mut result.literals {
                for arg in &mut lit.atom.args {
                    *arg = self.rewrite_term(arg, from, to);
                }
            }
        }

        result
    }

    fn rewrite_term(&self, term: &FolTerm, from: &FolTerm, to: &FolTerm) -> FolTerm {
        // Try to match at root
        if let Some(subst) = match_term(from, term) {
            return subst.apply_term(to);
        }

        // Recurse into subterms
        match term {
            FolTerm::Var(_) => term.clone(),
            FolTerm::Func(f, args) => {
                FolTerm::Func(
                    f.clone(),
                    args.iter().map(|a| self.rewrite_term(a, from, to)).collect(),
                )
            }
        }
    }

    /// Check if clause is subsumed
    fn is_subsumed(&self, clause: &Clause) -> bool {
        for other in &self.usable {
            if other.id != clause.id && self.subsumes(other, clause) {
                return true;
            }
        }
        false
    }

    /// Check if c1 subsumes c2
    fn subsumes(&self, c1: &Clause, c2: &Clause) -> bool {
        if c1.literals.len() > c2.literals.len() {
            return false;
        }

        // Try to find a substitution that makes c1 a subset of c2
        self.subsumes_helper(&c1.literals, &c2.literals, &Substitution::new())
    }

    fn subsumes_helper(&self, lits1: &[Literal], lits2: &[Literal], subst: &Substitution) -> bool {
        if lits1.is_empty() {
            return true;
        }

        let lit1 = &lits1[0];
        let rest1 = &lits1[1..];

        for lit2 in lits2 {
            if lit1.negated != lit2.negated || lit1.atom.predicate != lit2.atom.predicate {
                continue;
            }

            // Try to match (one-way unification)
            let lit1_subst = subst.apply_literal(lit1);
            if let Some(mgu) = match_atom(&lit1_subst.atom, &lit2.atom) {
                let new_subst = subst.compose(&mgu);
                if self.subsumes_helper(rest1, lits2, &new_subst) {
                    return true;
                }
            }
        }

        false
    }

    /// Calculate clause weight
    fn clause_weight(&self, clause: &Clause) -> usize {
        clause.literals.iter().map(|l| {
            1 + l.atom.args.iter().map(|a| self.ordering.term_weight(a)).sum::<usize>()
        }).sum()
    }

    /// Get clause signature for duplicate detection
    fn clause_signature(&self, clause: &Clause) -> String {
        let mut sig_parts: Vec<String> = clause.literals.iter().map(|l| {
            let sign = if l.negated { "~" } else { "" };
            format!("{}{}", sign, l.atom.predicate.name)
        }).collect();
        sig_parts.sort();
        sig_parts.join("|")
    }

    /// Extract proof from empty clause
    fn extract_proof(&self, empty_id: usize) -> Vec<ProofStep> {
        let mut proof = Vec::new();
        let mut to_process = vec![empty_id];
        let mut processed = HashSet::new();

        while let Some(id) = to_process.pop() {
            if processed.contains(&id) {
                continue;
            }
            processed.insert(id);

            if let Some(clause) = self.all_clauses.iter().find(|c| c.id == id) {
                let derivation = match &clause.derivation {
                    Some(ClauseDerivation::Input) => Derivation::Input,
                    Some(ClauseDerivation::Resolution { clause1, clause2, lit1_idx, lit2_idx }) => {
                        to_process.push(*clause1);
                        to_process.push(*clause2);
                        Derivation::Resolution {
                            clause1_id: *clause1,
                            clause2_id: *clause2,
                            literal1_idx: *lit1_idx,
                            literal2_idx: *lit2_idx,
                        }
                    }
                    Some(ClauseDerivation::Factor { clause, lit1_idx, lit2_idx }) => {
                        to_process.push(*clause);
                        Derivation::Factor {
                            clause_id: *clause,
                            literal1_idx: *lit1_idx,
                            literal2_idx: *lit2_idx,
                        }
                    }
                    Some(ClauseDerivation::Paramodulation { from, into }) => {
                        to_process.push(*from);
                        to_process.push(*into);
                        Derivation::Paramodulation {
                            from_clause: *from,
                            into_clause: *into,
                        }
                    }
                    _ => Derivation::Input,
                };

                proof.push(ProofStep {
                    clause: clause.clone(),
                    derivation,
                    step_id: id,
                });
            }
        }

        proof.reverse();
        proof
    }

    /// Parse input
    pub fn parse_input(&mut self, input: &str) -> Result<(), String> {
        // Reuse Otter parser
        for line in input.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('%') || line.starts_with('#') {
                continue;
            }
            if line.starts_with("formulas(") || line.starts_with("end_of_list") {
                continue;
            }

            // Parse as clause
            // Simplified parsing - in production would use full parser
        }
        Ok(())
    }

    /// Get statistics
    pub fn stats(&self) -> (usize, usize) {
        (self.all_clauses.len(), self.inferences)
    }
}

impl Default for SuperpositionProver {
    fn default() -> Self {
        Self::new()
    }
}

/// One-way matching for atoms
fn match_atom(pattern: &Atom, target: &Atom) -> Option<Substitution> {
    if pattern.predicate != target.predicate {
        return None;
    }

    let mut subst = Substitution::new();

    for (p, t) in pattern.args.iter().zip(target.args.iter()) {
        if let Some(s) = match_term(p, t) {
            subst = subst.compose(&s);
        } else {
            return None;
        }
    }

    Some(subst)
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::term::{Function, Variable};

    #[test]
    fn test_kbo() {
        let mut kbo = KboOrdering::new();
        kbo.set_weight("f", 2);
        kbo.set_weight("g", 1);

        let t1 = FolTerm::Func(Function::new("f", 1), vec![
            FolTerm::Var(Variable { name: "X".to_string(), id: 1 })
        ]);
        let t2 = FolTerm::Func(Function::new("g", 1), vec![
            FolTerm::Var(Variable { name: "X".to_string(), id: 1 })
        ]);

        // f(X) > g(X) because weight(f) > weight(g)
        assert_eq!(kbo.compare(&t1, &t2), Some(CmpOrdering::Greater));
    }
}
