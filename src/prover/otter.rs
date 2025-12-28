//! Otter-style theorem prover
//!
//! Implements the "given clause" algorithm used by Otter.
//! This is a saturation-based approach that systematically explores
//! all possible resolutions.

use std::collections::{BinaryHeap, HashSet};
use std::cmp::Ordering;
use std::time::{Instant, Duration};

use super::{ProofResult, ProofStep, Derivation, ProverConfig};
use super::clause::{Clause, Literal, ClauseDerivation};
use super::resolution::{resolve_all, factor_all, ordered_resolve, paramodulate, demodulate};
use super::term::{FolTerm, Atom};

/// A wrapper for clauses that implements ordering by weight (min-heap)
#[derive(Debug, Clone)]
struct WeightedClause {
    clause: Clause,
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
        // Reverse ordering for min-heap (smaller weight = higher priority)
        other.clause.weight.cmp(&self.clause.weight)
            .then_with(|| other.clause.id.cmp(&self.clause.id))
    }
}

/// The Otter theorem prover
pub struct OtterProver {
    /// Configuration
    config: ProverConfig,
    /// Processed clauses (usable list)
    usable: Vec<Clause>,
    /// Unprocessed clauses (sos - set of support)
    sos: BinaryHeap<WeightedClause>,
    /// All clauses by ID
    all_clauses: Vec<Clause>,
    /// Demodulators (unit equalities)
    demodulators: Vec<Clause>,
    /// Next clause ID
    next_id: usize,
    /// Number of resolution steps
    resolution_steps: usize,
    /// Seen clause signatures (for duplicate detection)
    seen: HashSet<String>,
}

impl OtterProver {
    /// Create a new Otter prover with default configuration
    pub fn new() -> Self {
        Self::with_config(ProverConfig::default())
    }

    /// Create a new Otter prover with custom configuration
    pub fn with_config(config: ProverConfig) -> Self {
        OtterProver {
            config,
            usable: Vec::new(),
            sos: BinaryHeap::new(),
            all_clauses: Vec::new(),
            demodulators: Vec::new(),
            next_id: 1,
            resolution_steps: 0,
            seen: HashSet::new(),
        }
    }

    /// Add an axiom clause
    pub fn add_axiom(&mut self, literals: Vec<Literal>) {
        let mut clause = Clause::new(literals, self.next_id);
        self.next_id += 1;
        clause.derivation = Some(ClauseDerivation::Input);

        self.process_new_clause(clause, false);
    }

    /// Add a goal clause (negated conjecture)
    pub fn add_goal(&mut self, literals: Vec<Literal>) {
        let mut clause = Clause::new(literals, self.next_id);
        self.next_id += 1;
        clause.derivation = Some(ClauseDerivation::Input);
        clause.is_goal = true;

        self.process_new_clause(clause, true);
    }

    /// Process a new clause (simplification, subsumption, etc.)
    fn process_new_clause(&mut self, mut clause: Clause, to_sos: bool) {
        // Remove duplicate literals
        clause.remove_duplicates();

        // Check for tautology
        if clause.is_tautology() {
            return;
        }

        // Check weight limit
        if clause.weight > self.config.max_weight {
            return;
        }

        // Demodulate if enabled
        if self.config.demodulation && !self.demodulators.is_empty() {
            clause = demodulate(&clause, &self.demodulators);
        }

        // Check for duplicate
        let sig = format!("{}", clause);
        if self.seen.contains(&sig) {
            return;
        }
        self.seen.insert(sig);

        // Forward subsumption: check if this clause is subsumed by existing ones
        for c in &self.usable {
            if c.subsumes(&clause) {
                return;
            }
        }

        // Backward subsumption: remove existing clauses subsumed by this one
        self.usable.retain(|c| !clause.subsumes(c));

        // Check if it's a demodulator
        if self.config.demodulation && clause.is_unit() {
            if let Some(lit) = clause.literals.first() {
                if lit.is_positive() && lit.is_equality() {
                    self.demodulators.push(clause.clone());
                }
            }
        }

        // Store the clause
        self.all_clauses.push(clause.clone());

        // Add to appropriate set
        if to_sos || self.config.set_of_support {
            self.sos.push(WeightedClause { clause });
        } else {
            self.usable.push(clause);
        }
    }

    /// Run the prover
    pub fn prove(&mut self) -> ProofResult {
        let start_time = Instant::now();
        let timeout = Duration::from_secs(self.config.max_seconds);

        if self.config.verbose {
            eprintln!("Starting Otter prover with {} usable and {} sos clauses",
                self.usable.len(), self.sos.len());
        }

        // Main given-clause loop
        while let Some(WeightedClause { clause: given }) = self.sos.pop() {
            // Check resource limits
            if start_time.elapsed() > timeout {
                return ProofResult::Unknown {
                    reason: format!("Time limit of {} seconds exceeded", self.config.max_seconds),
                };
            }

            if self.all_clauses.len() > self.config.max_clauses {
                return ProofResult::Unknown {
                    reason: format!("Clause limit of {} exceeded", self.config.max_clauses),
                };
            }

            // Check if we derived the empty clause
            if given.is_empty() {
                return self.build_proof(&given);
            }

            if self.config.verbose && self.resolution_steps % 1000 == 0 {
                eprintln!("Step {}: given clause {}: {}",
                    self.resolution_steps, given.id, given);
            }

            // Perform inferences with the given clause
            let new_clauses = self.infer(&given);

            // Move given clause to usable
            self.usable.push(given);

            // Process new clauses
            for clause in new_clauses {
                if clause.is_empty() {
                    return self.build_proof(&clause);
                }
                self.process_new_clause(clause, true);
            }
        }

        // SOS is empty and no proof found
        ProofResult::Satisfiable
    }

    /// Perform all inferences with the given clause
    fn infer(&mut self, given: &Clause) -> Vec<Clause> {
        let mut results = Vec::new();

        // Binary resolution with usable clauses
        for usable in &self.usable {
            let resolvents = if self.config.ordered_resolution {
                ordered_resolve(given, usable, &mut self.next_id)
            } else {
                resolve_all(given, usable, &mut self.next_id)
            };

            self.resolution_steps += resolvents.len();
            results.extend(resolvents);

            // Also try the other direction
            let resolvents2 = if self.config.ordered_resolution {
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

        // Paramodulation if enabled
        if self.config.paramodulation {
            for (i, lit) in given.literals.iter().enumerate() {
                if lit.is_positive() && lit.is_equality() {
                    // Paramodulate into usable clauses
                    for usable in &self.usable {
                        for j in 0..usable.literals.len() {
                            let paras = paramodulate(given, i, usable, j, self.next_id);
                            self.next_id += paras.len();
                            results.extend(paras);
                        }
                    }
                }
            }

            // Paramodulate from usable equalities into given
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

    /// Build a proof from the empty clause
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

        // Sort proof steps by ID
        proof_steps.sort_by_key(|s| s.step_id);

        ProofResult::Proved {
            proof: proof_steps,
            clauses_generated: self.all_clauses.len(),
            resolution_steps: self.resolution_steps,
        }
    }

    /// Parse Otter/Prover9 format input
    pub fn parse_input(&mut self, input: &str) -> Result<(), String> {
        let mut in_formulas = false;
        let mut in_goals = false;
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
                continue;
            }
            if line.contains("formulas(goals)") {
                in_formulas = true;
                in_goals = true;
                continue;
            }
            if line.contains("end_of_list") {
                in_formulas = false;
                continue;
            }

            if in_formulas {
                current_formula.push_str(line);
                current_formula.push(' ');

                if line.ends_with('.') {
                    // Parse the formula
                    let formula = current_formula.trim().trim_end_matches('.');
                    if !formula.is_empty() {
                        self.parse_clause(formula, in_goals)?;
                    }
                    current_formula.clear();
                }
            }
        }

        Ok(())
    }

    /// Parse a single clause in Otter/Prover9 format
    fn parse_clause(&mut self, input: &str, is_goal: bool) -> Result<(), String> {
        let input = input.trim();

        // Handle implication: A -> B becomes -A | B
        if input.contains("->") {
            let parts: Vec<&str> = input.splitn(2, "->").collect();
            if parts.len() == 2 {
                let antecedent = self.parse_literal(parts[0].trim())?;
                let consequent = self.parse_literal(parts[1].trim())?;

                let literals = vec![antecedent.negate(), consequent];
                if is_goal {
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
            if is_goal {
                // Negate the goal
                let negated: Vec<Literal> = literals.iter().map(|l| l.negate()).collect();
                self.add_goal(negated);
            } else {
                self.add_axiom(literals);
            }
        }

        Ok(())
    }

    /// Parse a single literal
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

    /// Parse an atom
    fn parse_atom(&mut self, input: &str) -> Result<Atom, String> {
        let input = input.trim();

        // Check for equality
        if input.contains('=') {
            let parts: Vec<&str> = input.splitn(2, '=').collect();
            if parts.len() == 2 {
                let left = self.parse_term(parts[0].trim())?;
                let right = self.parse_term(parts[1].trim())?;
                return Ok(Atom::equality(left, right));
            }
        }

        // Check for function application
        if let Some(paren_pos) = input.find('(') {
            let pred_name = &input[..paren_pos];
            let args_str = &input[paren_pos + 1..input.len() - 1];

            let args = self.parse_term_list(args_str)?;
            Ok(Atom::new(pred_name, args))
        } else {
            // Propositional atom
            Ok(Atom::new(input, vec![]))
        }
    }

    /// Parse a term
    fn parse_term(&mut self, input: &str) -> Result<FolTerm, String> {
        let input = input.trim();

        // Check for variable (starts with uppercase or x, y, z, u, v, w)
        if input.chars().next().map(|c| c.is_uppercase() || "xyzuvw".contains(c)).unwrap_or(false)
            && !input.contains('(')
        {
            return Ok(FolTerm::var(input, self.next_id));
        }

        // Check for function application
        if let Some(paren_pos) = input.find('(') {
            let func_name = &input[..paren_pos];
            let args_str = &input[paren_pos + 1..input.len() - 1];

            let args = self.parse_term_list(args_str)?;
            Ok(FolTerm::func(func_name, args))
        } else {
            // Constant
            Ok(FolTerm::constant(input))
        }
    }

    /// Parse a comma-separated list of terms
    fn parse_term_list(&mut self, input: &str) -> Result<Vec<FolTerm>, String> {
        if input.trim().is_empty() {
            return Ok(vec![]);
        }

        let mut terms = Vec::new();
        let mut current = String::new();
        let mut depth = 0;

        for c in input.chars() {
            match c {
                '(' => {
                    depth += 1;
                    current.push(c);
                }
                ')' => {
                    depth -= 1;
                    current.push(c);
                }
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

impl Default for OtterProver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_proof() {
        let mut prover = OtterProver::new();

        // P(a)
        prover.add_axiom(vec![
            Literal::positive(Atom::new("P", vec![FolTerm::constant("a")]))
        ]);

        // -P(x) | Q(x)
        prover.add_axiom(vec![
            Literal::negative(Atom::new("P", vec![FolTerm::var("x", 1)])),
            Literal::positive(Atom::new("Q", vec![FolTerm::var("x", 1)])),
        ]);

        // Goal: Q(a) (negated as -Q(a))
        prover.add_goal(vec![
            Literal::negative(Atom::new("Q", vec![FolTerm::constant("a")]))
        ]);

        let result = prover.prove();
        assert!(matches!(result, ProofResult::Proved { .. }));
    }

    #[test]
    fn test_transitive_proof() {
        let mut prover = OtterProver::new();

        // R(a, b)
        prover.add_axiom(vec![
            Literal::positive(Atom::new("R", vec![
                FolTerm::constant("a"),
                FolTerm::constant("b"),
            ]))
        ]);

        // R(b, c)
        prover.add_axiom(vec![
            Literal::positive(Atom::new("R", vec![
                FolTerm::constant("b"),
                FolTerm::constant("c"),
            ]))
        ]);

        // Transitivity: -R(x,y) | -R(y,z) | R(x,z)
        prover.add_axiom(vec![
            Literal::negative(Atom::new("R", vec![
                FolTerm::var("x", 1),
                FolTerm::var("y", 2),
            ])),
            Literal::negative(Atom::new("R", vec![
                FolTerm::var("y", 2),
                FolTerm::var("z", 3),
            ])),
            Literal::positive(Atom::new("R", vec![
                FolTerm::var("x", 1),
                FolTerm::var("z", 3),
            ])),
        ]);

        // Goal: R(a, c)
        prover.add_goal(vec![
            Literal::negative(Atom::new("R", vec![
                FolTerm::constant("a"),
                FolTerm::constant("c"),
            ]))
        ]);

        let result = prover.prove();
        assert!(matches!(result, ProofResult::Proved { .. }));
    }

    #[test]
    fn test_parse_input() {
        let input = r#"
            formulas(assumptions).
            P(a).
            -P(x) | Q(x).
            end_of_list.

            formulas(goals).
            Q(a).
            end_of_list.
        "#;

        let mut prover = OtterProver::new();
        prover.parse_input(input).unwrap();

        let result = prover.prove();
        assert!(matches!(result, ProofResult::Proved { .. }));
    }
}
