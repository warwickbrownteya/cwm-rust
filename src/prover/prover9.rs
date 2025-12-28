//! Prover9-style theorem prover
//!
//! Extends the Otter algorithm with additional features:
//! - Better clause selection (pick-given ratio)
//! - More sophisticated term ordering (LPO)
//! - Hints mechanism
//! - Better weight calculation

use std::collections::{BinaryHeap, HashSet, HashMap};
use std::cmp::Ordering;
use std::time::{Instant, Duration};

use super::{ProofResult, ProofStep, Derivation, ProverConfig};
use super::clause::{Clause, Literal, ClauseDerivation};
use super::resolution::{resolve_all, factor_all, ordered_resolve, paramodulate, demodulate};
use super::term::{FolTerm, Atom};

/// Term ordering for Prover9 (Lexicographic Path Ordering)
#[derive(Debug, Clone)]
pub struct TermOrdering {
    /// Symbol precedence (higher = greater)
    precedence: HashMap<String, usize>,
}

impl TermOrdering {
    pub fn new() -> Self {
        TermOrdering {
            precedence: HashMap::new(),
        }
    }

    /// Set precedence for a symbol
    pub fn set_precedence(&mut self, symbol: &str, prec: usize) {
        self.precedence.insert(symbol.to_string(), prec);
    }

    /// Get precedence for a symbol (defaults to arity)
    pub fn get_precedence(&self, symbol: &str, arity: usize) -> usize {
        *self.precedence.get(symbol).unwrap_or(&arity)
    }

    /// Compare two terms using LPO
    pub fn compare(&self, t1: &FolTerm, t2: &FolTerm) -> Ordering {
        // Variables are incomparable
        match (t1, t2) {
            (FolTerm::Var(_), FolTerm::Var(_)) => Ordering::Equal,
            (FolTerm::Var(v), FolTerm::Func(_, _)) => {
                if t2.contains_var(v) {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            }
            (FolTerm::Func(_, _), FolTerm::Var(v)) => {
                if t1.contains_var(v) {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            }
            (FolTerm::Func(f1, args1), FolTerm::Func(f2, args2)) => {
                // LPO comparison
                let prec1 = self.get_precedence(&f1.name, f1.arity);
                let prec2 = self.get_precedence(&f2.name, f2.arity);

                if prec1 != prec2 {
                    return prec1.cmp(&prec2);
                }

                // Same precedence, compare arguments lexicographically
                for (a1, a2) in args1.iter().zip(args2.iter()) {
                    match self.compare(a1, a2) {
                        Ordering::Equal => continue,
                        other => return other,
                    }
                }

                args1.len().cmp(&args2.len())
            }
        }
    }

    /// Check if t1 > t2
    pub fn greater(&self, t1: &FolTerm, t2: &FolTerm) -> bool {
        self.compare(t1, t2) == Ordering::Greater
    }
}

impl Default for TermOrdering {
    fn default() -> Self {
        Self::new()
    }
}

/// Clause with age for pick-given selection
#[derive(Debug, Clone)]
struct AgedClause {
    clause: Clause,
    age: usize,
}

impl PartialEq for AgedClause {
    fn eq(&self, other: &Self) -> bool {
        self.clause.id == other.clause.id
    }
}

impl Eq for AgedClause {}

/// Weight-based clause selection
#[derive(Debug, Clone)]
struct WeightedClause {
    clause: Clause,
    adjusted_weight: usize,
}

impl PartialEq for WeightedClause {
    fn eq(&self, other: &Self) -> bool {
        self.clause.id == other.clause.id
    }
}

impl Eq for WeightedClause {}

impl PartialOrd for WeightedClause {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WeightedClause {
    fn cmp(&self, other: &Self) -> Ordering {
        other.adjusted_weight.cmp(&self.adjusted_weight)
            .then_with(|| other.clause.id.cmp(&self.clause.id))
    }
}

/// Prover9 configuration
#[derive(Debug, Clone)]
pub struct Prover9Config {
    /// Base prover configuration
    pub base: ProverConfig,
    /// Pick-given ratio (weight:age)
    pub pick_given_ratio: usize,
    /// Age weight multiplier
    pub age_weight: f64,
    /// Symbol weight multiplier
    pub symbol_weight: usize,
    /// Variable weight
    pub var_weight: usize,
    /// Use hints
    pub use_hints: bool,
    /// Hint bonus (weight reduction)
    pub hint_bonus: usize,
}

impl Default for Prover9Config {
    fn default() -> Self {
        Prover9Config {
            base: ProverConfig::default(),
            pick_given_ratio: 5,
            age_weight: 0.5,
            symbol_weight: 1,
            var_weight: 1,
            use_hints: false,
            hint_bonus: 100,
        }
    }
}

/// The Prover9 theorem prover
pub struct Prover9 {
    /// Configuration
    config: Prover9Config,
    /// Processed clauses
    usable: Vec<Clause>,
    /// Unprocessed clauses by weight
    sos_by_weight: BinaryHeap<WeightedClause>,
    /// Unprocessed clauses by age
    sos_by_age: Vec<AgedClause>,
    /// All clauses by ID
    all_clauses: Vec<Clause>,
    /// Demodulators
    demodulators: Vec<Clause>,
    /// Hints (patterns to match)
    hints: Vec<Clause>,
    /// Term ordering
    ordering: TermOrdering,
    /// Next clause ID
    next_id: usize,
    /// Current age counter
    age_counter: usize,
    /// Resolution steps
    resolution_steps: usize,
    /// Given clause counter (for pick-given ratio)
    given_counter: usize,
    /// Seen clause signatures
    seen: HashSet<String>,
}

impl Prover9 {
    /// Create a new Prover9 prover
    pub fn new() -> Self {
        Self::with_config(Prover9Config::default())
    }

    /// Create with custom configuration
    pub fn with_config(config: Prover9Config) -> Self {
        Prover9 {
            config,
            usable: Vec::new(),
            sos_by_weight: BinaryHeap::new(),
            sos_by_age: Vec::new(),
            all_clauses: Vec::new(),
            demodulators: Vec::new(),
            hints: Vec::new(),
            ordering: TermOrdering::new(),
            next_id: 1,
            age_counter: 0,
            resolution_steps: 0,
            given_counter: 0,
            seen: HashSet::new(),
        }
    }

    /// Add a hint clause
    pub fn add_hint(&mut self, literals: Vec<Literal>) {
        let clause = Clause::new(literals, 0);
        self.hints.push(clause);
    }

    /// Calculate the weight of a clause
    fn calculate_weight(&self, clause: &Clause) -> usize {
        let mut weight = 0;

        for lit in &clause.literals {
            weight += self.config.symbol_weight; // predicate symbol

            for arg in &lit.atom.args {
                weight += self.term_weight(arg);
            }
        }

        // Apply hint bonus
        if self.config.use_hints {
            for hint in &self.hints {
                if self.matches_hint(clause, hint) {
                    weight = weight.saturating_sub(self.config.hint_bonus);
                    break;
                }
            }
        }

        weight
    }

    /// Calculate the weight of a term
    fn term_weight(&self, term: &FolTerm) -> usize {
        match term {
            FolTerm::Var(_) => self.config.var_weight,
            FolTerm::Func(_, args) => {
                self.config.symbol_weight + args.iter().map(|a| self.term_weight(a)).sum::<usize>()
            }
        }
    }

    /// Check if a clause matches a hint
    fn matches_hint(&self, clause: &Clause, hint: &Clause) -> bool {
        // Simple check: same number of literals and predicates match
        if clause.literals.len() != hint.literals.len() {
            return false;
        }

        for (c_lit, h_lit) in clause.literals.iter().zip(hint.literals.iter()) {
            if c_lit.negated != h_lit.negated {
                return false;
            }
            if c_lit.atom.predicate != h_lit.atom.predicate {
                return false;
            }
        }

        true
    }

    /// Add an axiom clause
    pub fn add_axiom(&mut self, literals: Vec<Literal>) {
        let mut clause = Clause::new(literals, self.next_id);
        self.next_id += 1;
        clause.derivation = Some(ClauseDerivation::Input);

        self.process_new_clause(clause, false);
    }

    /// Add a goal clause
    pub fn add_goal(&mut self, literals: Vec<Literal>) {
        let mut clause = Clause::new(literals, self.next_id);
        self.next_id += 1;
        clause.derivation = Some(ClauseDerivation::Input);
        clause.is_goal = true;

        self.process_new_clause(clause, true);
    }

    /// Process a new clause
    fn process_new_clause(&mut self, mut clause: Clause, to_sos: bool) {
        // Remove duplicates
        clause.remove_duplicates();

        // Skip tautologies
        if clause.is_tautology() {
            return;
        }

        // Check weight limit
        let weight = self.calculate_weight(&clause);
        if weight > self.config.base.max_weight {
            return;
        }

        // Demodulation
        if self.config.base.demodulation && !self.demodulators.is_empty() {
            clause = demodulate(&clause, &self.demodulators);
        }

        // Orient equalities using LPO
        self.orient_equalities(&mut clause);

        // Duplicate check
        let sig = format!("{}", clause);
        if self.seen.contains(&sig) {
            return;
        }
        self.seen.insert(sig);

        // Subsumption
        for c in &self.usable {
            if c.subsumes(&clause) {
                return;
            }
        }

        self.usable.retain(|c| !clause.subsumes(c));

        // Check for demodulator
        if self.config.base.demodulation && clause.is_unit() {
            if let Some(lit) = clause.literals.first() {
                if lit.is_positive() && lit.is_equality() {
                    // Check if it's a good demodulator (oriented)
                    let left = &lit.atom.args[0];
                    let right = &lit.atom.args[1];
                    if self.ordering.greater(left, right) || left.is_ground() {
                        self.demodulators.push(clause.clone());
                    }
                }
            }
        }

        // Store clause
        self.all_clauses.push(clause.clone());

        // Add to SOS
        if to_sos || self.config.base.set_of_support {
            self.age_counter += 1;

            let adjusted_weight = weight + (self.age_counter as f64 * self.config.age_weight) as usize;

            self.sos_by_weight.push(WeightedClause {
                clause: clause.clone(),
                adjusted_weight,
            });

            self.sos_by_age.push(AgedClause {
                clause,
                age: self.age_counter,
            });
        } else {
            self.usable.push(clause);
        }
    }

    /// Orient equalities in a clause using LPO
    fn orient_equalities(&self, clause: &mut Clause) {
        for lit in &mut clause.literals {
            if lit.is_equality() && lit.atom.args.len() == 2 {
                let left = &lit.atom.args[0];
                let right = &lit.atom.args[1];

                // Orient so larger term is on the left
                if self.ordering.compare(left, right) == Ordering::Less {
                    lit.atom.args.swap(0, 1);
                }
            }
        }
    }

    /// Select the next given clause using pick-given ratio
    fn select_given(&mut self) -> Option<Clause> {
        self.given_counter += 1;

        // Use age every N clauses, weight otherwise
        let use_age = self.given_counter % self.config.pick_given_ratio == 0;

        if use_age && !self.sos_by_age.is_empty() {
            // Pick oldest clause
            let aged = self.sos_by_age.remove(0);

            // Remove from weight heap
            self.sos_by_weight = self.sos_by_weight
                .drain()
                .filter(|wc| wc.clause.id != aged.clause.id)
                .collect();

            Some(aged.clause)
        } else if let Some(weighted) = self.sos_by_weight.pop() {
            // Remove from age list
            self.sos_by_age.retain(|ac| ac.clause.id != weighted.clause.id);

            Some(weighted.clause)
        } else if !self.sos_by_age.is_empty() {
            // Fallback to age if weight heap is empty
            let aged = self.sos_by_age.remove(0);
            Some(aged.clause)
        } else {
            None
        }
    }

    /// Run the prover
    pub fn prove(&mut self) -> ProofResult {
        let start_time = Instant::now();
        let timeout = Duration::from_secs(self.config.base.max_seconds);

        if self.config.base.verbose {
            eprintln!("Starting Prover9 with {} usable and {} sos clauses",
                self.usable.len(), self.sos_by_weight.len());
        }

        while let Some(given) = self.select_given() {
            // Check limits
            if start_time.elapsed() > timeout {
                return ProofResult::Unknown {
                    reason: format!("Time limit of {} seconds exceeded", self.config.base.max_seconds),
                };
            }

            if self.all_clauses.len() > self.config.base.max_clauses {
                return ProofResult::Unknown {
                    reason: format!("Clause limit of {} exceeded", self.config.base.max_clauses),
                };
            }

            // Check for empty clause
            if given.is_empty() {
                return self.build_proof(&given);
            }

            if self.config.base.verbose && self.resolution_steps % 1000 == 0 {
                eprintln!("Step {}: given clause {}: {}",
                    self.resolution_steps, given.id, given);
            }

            // Perform inferences
            let new_clauses = self.infer(&given);

            // Move to usable
            self.usable.push(given);

            // Process new clauses
            for clause in new_clauses {
                if clause.is_empty() {
                    return self.build_proof(&clause);
                }
                self.process_new_clause(clause, true);
            }
        }

        ProofResult::Satisfiable
    }

    /// Perform inferences
    fn infer(&mut self, given: &Clause) -> Vec<Clause> {
        let mut results = Vec::new();

        // Binary resolution
        for usable in &self.usable {
            let resolvents = if self.config.base.ordered_resolution {
                ordered_resolve(given, usable, &mut self.next_id)
            } else {
                resolve_all(given, usable, &mut self.next_id)
            };

            self.resolution_steps += resolvents.len();
            results.extend(resolvents);

            let resolvents2 = if self.config.base.ordered_resolution {
                ordered_resolve(usable, given, &mut self.next_id)
            } else {
                resolve_all(usable, given, &mut self.next_id)
            };

            self.resolution_steps += resolvents2.len();
            results.extend(resolvents2);
        }

        // Factoring
        let factors = factor_all(given, &mut self.next_id);
        results.extend(factors);

        // Paramodulation
        if self.config.base.paramodulation {
            for (i, lit) in given.literals.iter().enumerate() {
                if lit.is_positive() && lit.is_equality() {
                    for usable in &self.usable {
                        for j in 0..usable.literals.len() {
                            let paras = paramodulate(given, i, usable, j, self.next_id);
                            self.next_id += paras.len();
                            results.extend(paras);
                        }
                    }
                }
            }

            for usable in &self.usable {
                for (i, lit) in usable.literals.iter().enumerate() {
                    if lit.is_positive() && lit.is_equality() {
                        for j in 0..given.literals.len() {
                            let paras = paramodulate(usable, i, given, j, self.next_id);
                            self.next_id += paras.len();
                            results.extend(paras);
                        }
                    }
                }
            }
        }

        results
    }

    /// Build proof
    fn build_proof(&self, empty_clause: &Clause) -> ProofResult {
        let mut proof_steps = Vec::new();
        let mut to_trace = vec![empty_clause.id];
        let mut traced = HashSet::new();

        while let Some(id) = to_trace.pop() {
            if traced.contains(&id) {
                continue;
            }
            traced.insert(id);

            if let Some(clause) = self.all_clauses.iter().find(|c| c.id == id) {
                let derivation = match &clause.derivation {
                    Some(ClauseDerivation::Input) => Derivation::Input,
                    Some(ClauseDerivation::Resolution { clause1, clause2, lit1_idx, lit2_idx }) => {
                        to_trace.push(*clause1);
                        to_trace.push(*clause2);
                        Derivation::Resolution {
                            clause1_id: *clause1,
                            clause2_id: *clause2,
                            literal1_idx: *lit1_idx,
                            literal2_idx: *lit2_idx,
                        }
                    }
                    Some(ClauseDerivation::Factor { clause, lit1_idx, lit2_idx }) => {
                        to_trace.push(*clause);
                        Derivation::Factor {
                            clause_id: *clause,
                            literal1_idx: *lit1_idx,
                            literal2_idx: *lit2_idx,
                        }
                    }
                    Some(ClauseDerivation::Paramodulation { from, into }) => {
                        to_trace.push(*from);
                        to_trace.push(*into);
                        Derivation::Paramodulation {
                            from_clause: *from,
                            into_clause: *into,
                        }
                    }
                    None => Derivation::Input,
                };

                proof_steps.push(ProofStep {
                    clause: clause.clone(),
                    derivation,
                    step_id: clause.id,
                });
            }
        }

        proof_steps.sort_by_key(|s| s.step_id);

        ProofResult::Proved {
            proof: proof_steps,
            clauses_generated: self.all_clauses.len(),
            resolution_steps: self.resolution_steps,
        }
    }

    /// Parse Prover9 format input
    pub fn parse_input(&mut self, input: &str) -> Result<(), String> {
        let mut in_formulas = false;
        let mut in_goals = false;
        let mut in_hints = false;
        let mut current_formula = String::new();

        for line in input.lines() {
            let line = line.trim();

            // Skip comments
            if line.starts_with('%') || line.starts_with('#') {
                continue;
            }

            // Check for section markers
            if line.contains("formulas(assumptions)") || line.contains("formulas(sos)") {
                in_formulas = true;
                in_goals = false;
                in_hints = false;
                continue;
            }
            if line.contains("formulas(goals)") {
                in_formulas = true;
                in_goals = true;
                in_hints = false;
                continue;
            }
            if line.contains("formulas(hints)") {
                in_formulas = true;
                in_goals = false;
                in_hints = true;
                continue;
            }
            if line.contains("end_of_list") {
                in_formulas = false;
                continue;
            }

            // Parse set commands
            if line.starts_with("set(") {
                if line.contains("hyperresolution") {
                    self.config.base.hyperresolution = true;
                }
                continue;
            }

            if in_formulas {
                current_formula.push_str(line);
                current_formula.push(' ');

                if line.ends_with('.') {
                    let formula = current_formula.trim().trim_end_matches('.');
                    if !formula.is_empty() {
                        self.parse_clause(formula, in_goals, in_hints)?;
                    }
                    current_formula.clear();
                }
            }
        }

        Ok(())
    }

    /// Parse a clause
    fn parse_clause(&mut self, input: &str, is_goal: bool, is_hint: bool) -> Result<(), String> {
        let input = input.trim();

        // Handle implication
        if input.contains("->") {
            let parts: Vec<&str> = input.splitn(2, "->").collect();
            if parts.len() == 2 {
                let antecedent = self.parse_literal(parts[0].trim())?;
                let consequent = self.parse_literal(parts[1].trim())?;

                let literals = vec![antecedent.negate(), consequent];
                if is_hint {
                    self.add_hint(literals);
                } else if is_goal {
                    self.add_goal(literals);
                } else {
                    self.add_axiom(literals);
                }
                return Ok(());
            }
        }

        // Handle disjunction
        let literal_strs: Vec<&str> = input.split('|').map(|s| s.trim()).collect();
        let mut literals = Vec::new();

        for lit_str in literal_strs {
            if !lit_str.is_empty() {
                literals.push(self.parse_literal(lit_str)?);
            }
        }

        if !literals.is_empty() {
            if is_hint {
                self.add_hint(literals);
            } else if is_goal {
                let negated: Vec<Literal> = literals.iter().map(|l| l.negate()).collect();
                self.add_goal(negated);
            } else {
                self.add_axiom(literals);
            }
        }

        Ok(())
    }

    fn parse_literal(&mut self, input: &str) -> Result<Literal, String> {
        let input = input.trim();

        let (negated, atom_str) = if input.starts_with('-') || input.starts_with('~') {
            (true, &input[1..])
        } else {
            (false, input)
        };

        let atom = self.parse_atom(atom_str.trim())?;
        Ok(Literal::new(atom, negated))
    }

    fn parse_atom(&mut self, input: &str) -> Result<Atom, String> {
        let input = input.trim();

        if input.contains('=') {
            let parts: Vec<&str> = input.splitn(2, '=').collect();
            if parts.len() == 2 {
                let left = self.parse_term(parts[0].trim())?;
                let right = self.parse_term(parts[1].trim())?;
                return Ok(Atom::equality(left, right));
            }
        }

        if let Some(paren_pos) = input.find('(') {
            let pred_name = &input[..paren_pos];
            let args_str = &input[paren_pos + 1..input.len() - 1];
            let args = self.parse_term_list(args_str)?;
            Ok(Atom::new(pred_name, args))
        } else {
            Ok(Atom::new(input, vec![]))
        }
    }

    fn parse_term(&mut self, input: &str) -> Result<FolTerm, String> {
        let input = input.trim();

        if input.chars().next().map(|c| c.is_uppercase() || "xyzuvw".contains(c)).unwrap_or(false)
            && !input.contains('(')
        {
            return Ok(FolTerm::var(input, self.next_id));
        }

        if let Some(paren_pos) = input.find('(') {
            let func_name = &input[..paren_pos];
            let args_str = &input[paren_pos + 1..input.len() - 1];
            let args = self.parse_term_list(args_str)?;
            Ok(FolTerm::func(func_name, args))
        } else {
            Ok(FolTerm::constant(input))
        }
    }

    fn parse_term_list(&mut self, input: &str) -> Result<Vec<FolTerm>, String> {
        if input.trim().is_empty() {
            return Ok(vec![]);
        }

        let mut terms = Vec::new();
        let mut current = String::new();
        let mut depth = 0;

        for c in input.chars() {
            match c {
                '(' => { depth += 1; current.push(c); }
                ')' => { depth -= 1; current.push(c); }
                ',' if depth == 0 => {
                    if !current.trim().is_empty() {
                        terms.push(self.parse_term(&current)?);
                    }
                    current.clear();
                }
                _ => current.push(c),
            }
        }

        if !current.trim().is_empty() {
            terms.push(self.parse_term(&current)?);
        }

        Ok(terms)
    }
}

impl Default for Prover9 {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prover9_simple() {
        let mut prover = Prover9::new();

        prover.add_axiom(vec![
            Literal::positive(Atom::new("P", vec![FolTerm::constant("a")]))
        ]);

        prover.add_axiom(vec![
            Literal::negative(Atom::new("P", vec![FolTerm::var("x", 1)])),
            Literal::positive(Atom::new("Q", vec![FolTerm::var("x", 1)])),
        ]);

        prover.add_goal(vec![
            Literal::negative(Atom::new("Q", vec![FolTerm::constant("a")]))
        ]);

        let result = prover.prove();
        assert!(matches!(result, ProofResult::Proved { .. }));
    }

    #[test]
    fn test_term_ordering() {
        let ordering = TermOrdering::new();

        let f_a = FolTerm::func("f", vec![FolTerm::constant("a")]);
        let a = FolTerm::constant("a");

        // f(a) > a because f(a) has greater depth
        assert!(ordering.greater(&f_a, &a));
    }

    #[test]
    fn test_prover9_equality() {
        let mut prover = Prover9::new();

        // f(a) = b
        prover.add_axiom(vec![
            Literal::positive(Atom::equality(
                FolTerm::func("f", vec![FolTerm::constant("a")]),
                FolTerm::constant("b"),
            ))
        ]);

        // P(f(a))
        prover.add_axiom(vec![
            Literal::positive(Atom::new("P", vec![
                FolTerm::func("f", vec![FolTerm::constant("a")])
            ]))
        ]);

        // Goal: P(b)
        prover.add_goal(vec![
            Literal::negative(Atom::new("P", vec![FolTerm::constant("b")]))
        ]);

        let result = prover.prove();
        assert!(matches!(result, ProofResult::Proved { .. }));
    }
}
