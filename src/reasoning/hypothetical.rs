//! Hypothetical Reasoning Module
//!
//! This module provides hypothetical and counterfactual reasoning capabilities:
//! - Hypothetical worlds (branching contexts)
//! - Assumption-based reasoning
//! - What-if analysis
//! - Truth maintenance across hypotheses
//! - Counterfactual inference

use crate::term::{Term, Triple, Uri, Variable as TermVariable};
use crate::reasoner::Rule;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

/// Unique identifier for hypothetical worlds
pub type WorldId = u64;

static WORLD_COUNTER: AtomicU64 = AtomicU64::new(0);

fn next_world_id() -> WorldId {
    WORLD_COUNTER.fetch_add(1, Ordering::SeqCst)
}

/// An assumption in hypothetical reasoning
#[derive(Debug, Clone)]
pub struct Assumption {
    /// Unique identifier
    pub id: u64,
    /// The assumed triple
    pub triple: Triple,
    /// Whether this assumption is currently active
    pub active: bool,
    /// Justification for the assumption
    pub justification: Option<String>,
}

impl Assumption {
    pub fn new(triple: Triple, justification: Option<String>) -> Self {
        static ASSUMPTION_COUNTER: AtomicU64 = AtomicU64::new(0);
        Self {
            id: ASSUMPTION_COUNTER.fetch_add(1, Ordering::SeqCst),
            triple,
            active: true,
            justification,
        }
    }
}

/// A hypothetical world representing a set of assumptions and derived facts
#[derive(Debug, Clone)]
pub struct HypotheticalWorld {
    /// World identifier
    pub id: WorldId,
    /// Parent world (None for base world)
    pub parent: Option<WorldId>,
    /// Active assumptions in this world
    pub assumptions: Vec<Assumption>,
    /// Facts derived in this world (not inherited from parent)
    pub local_facts: HashSet<Triple>,
    /// Child worlds branching from this one
    pub children: Vec<WorldId>,
    /// Whether this world is consistent
    pub consistent: bool,
    /// Label for this world
    pub label: Option<String>,
}

impl HypotheticalWorld {
    pub fn new(parent: Option<WorldId>, label: Option<String>) -> Self {
        Self {
            id: next_world_id(),
            parent,
            assumptions: Vec::new(),
            local_facts: HashSet::new(),
            children: Vec::new(),
            consistent: true,
            label,
        }
    }

    /// Add an assumption to this world
    pub fn assume(&mut self, triple: Triple, justification: Option<String>) -> u64 {
        let assumption = Assumption::new(triple.clone(), justification);
        let id = assumption.id;
        self.assumptions.push(assumption);
        self.local_facts.insert(triple);
        id
    }

    /// Retract an assumption
    pub fn retract(&mut self, assumption_id: u64) -> bool {
        if let Some(pos) = self.assumptions.iter().position(|a| a.id == assumption_id) {
            let assumption = self.assumptions.remove(pos);
            self.local_facts.remove(&assumption.triple);
            true
        } else {
            false
        }
    }

    /// Get all active assumptions
    pub fn active_assumptions(&self) -> Vec<&Assumption> {
        self.assumptions.iter().filter(|a| a.active).collect()
    }
}

/// Dependency tracking for truth maintenance
#[derive(Debug, Clone)]
pub struct Dependency {
    /// The derived fact
    pub fact: Triple,
    /// Assumptions this fact depends on
    pub assumptions: HashSet<u64>,
    /// Rules used to derive this fact
    pub rules: Vec<usize>,
}

/// Truth Maintenance System for tracking assumption dependencies
#[derive(Debug, Clone, Default)]
pub struct TruthMaintenanceSystem {
    /// Dependencies for derived facts
    dependencies: HashMap<Triple, Dependency>,
    /// Reverse index: assumption -> facts depending on it
    dependents: HashMap<u64, HashSet<Triple>>,
}

impl TruthMaintenanceSystem {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a dependency
    pub fn record_dependency(
        &mut self,
        fact: Triple,
        assumptions: HashSet<u64>,
        rule_idx: Option<usize>,
    ) {
        let dep = self.dependencies.entry(fact.clone()).or_insert(Dependency {
            fact: fact.clone(),
            assumptions: HashSet::new(),
            rules: Vec::new(),
        });

        for &assumption_id in &assumptions {
            dep.assumptions.insert(assumption_id);
            self.dependents
                .entry(assumption_id)
                .or_default()
                .insert(fact.clone());
        }

        if let Some(idx) = rule_idx {
            if !dep.rules.contains(&idx) {
                dep.rules.push(idx);
            }
        }
    }

    /// Get facts that depend on an assumption
    pub fn get_dependents(&self, assumption_id: u64) -> HashSet<Triple> {
        self.dependents
            .get(&assumption_id)
            .cloned()
            .unwrap_or_default()
    }

    /// Get assumptions a fact depends on
    pub fn get_dependencies(&self, fact: &Triple) -> HashSet<u64> {
        self.dependencies
            .get(fact)
            .map(|d| d.assumptions.clone())
            .unwrap_or_default()
    }

    /// Remove dependencies for retracted assumption
    pub fn retract_assumption(&mut self, assumption_id: u64) -> HashSet<Triple> {
        let affected = self.get_dependents(assumption_id);
        self.dependents.remove(&assumption_id);

        for fact in &affected {
            if let Some(dep) = self.dependencies.get_mut(fact) {
                dep.assumptions.remove(&assumption_id);
            }
        }

        affected
    }
}

/// Result of hypothetical analysis
#[derive(Debug, Clone)]
pub struct HypotheticalResult {
    /// The world that was analyzed
    pub world_id: WorldId,
    /// Facts that hold under the hypotheses
    pub derived_facts: HashSet<Triple>,
    /// Whether the hypotheses are consistent
    pub consistent: bool,
    /// Detected contradictions if any
    pub contradictions: Vec<(Triple, Triple)>,
    /// Assumptions used
    pub assumptions: Vec<Assumption>,
}

/// Counterfactual query result
#[derive(Debug, Clone)]
pub struct CounterfactualResult {
    /// The counterfactual hypothesis
    pub hypothesis: Triple,
    /// Facts that would be true under the hypothesis
    pub would_be_true: HashSet<Triple>,
    /// Facts that would become false under the hypothesis
    pub would_become_false: HashSet<Triple>,
    /// New facts derived under the hypothesis
    pub newly_derived: HashSet<Triple>,
    /// Whether the counterfactual is coherent
    pub coherent: bool,
}

/// Hypothetical Reasoner for what-if analysis
pub struct HypotheticalReasoner {
    /// All worlds, keyed by ID
    worlds: HashMap<WorldId, HypotheticalWorld>,
    /// The base (actual) world ID
    base_world_id: WorldId,
    /// Current active world
    current_world_id: WorldId,
    /// Rules for inference
    rules: Vec<Rule>,
    /// Base facts (ground truth)
    base_facts: HashSet<Triple>,
    /// Truth maintenance system
    tms: TruthMaintenanceSystem,
}

impl HypotheticalReasoner {
    pub fn new() -> Self {
        let base_world = HypotheticalWorld::new(None, Some("base".to_string()));
        let base_id = base_world.id;

        let mut worlds = HashMap::new();
        worlds.insert(base_id, base_world);

        Self {
            worlds,
            base_world_id: base_id,
            current_world_id: base_id,
            rules: Vec::new(),
            base_facts: HashSet::new(),
            tms: TruthMaintenanceSystem::new(),
        }
    }

    /// Add a rule for inference
    pub fn add_rule(&mut self, rule: Rule) {
        self.rules.push(rule);
    }

    /// Add base facts (ground truth)
    pub fn add_base_fact(&mut self, triple: Triple) {
        self.base_facts.insert(triple);
    }

    /// Add multiple base facts
    pub fn add_base_facts(&mut self, triples: impl IntoIterator<Item = Triple>) {
        self.base_facts.extend(triples);
    }

    /// Create a new hypothetical world branching from the current world
    pub fn branch(&mut self, label: Option<String>) -> WorldId {
        let parent_id = self.current_world_id;
        let new_world = HypotheticalWorld::new(Some(parent_id), label);
        let new_id = new_world.id;

        self.worlds.insert(new_id, new_world);

        if let Some(parent) = self.worlds.get_mut(&parent_id) {
            parent.children.push(new_id);
        }

        new_id
    }

    /// Switch to a different world
    pub fn switch_to(&mut self, world_id: WorldId) -> bool {
        if self.worlds.contains_key(&world_id) {
            self.current_world_id = world_id;
            true
        } else {
            false
        }
    }

    /// Get the current world
    pub fn current_world(&self) -> Option<&HypotheticalWorld> {
        self.worlds.get(&self.current_world_id)
    }

    /// Get a world by ID
    pub fn get_world(&self, world_id: WorldId) -> Option<&HypotheticalWorld> {
        self.worlds.get(&world_id)
    }

    /// Assume a fact in the current world
    pub fn assume(&mut self, triple: Triple, justification: Option<String>) -> Option<u64> {
        let world = self.worlds.get_mut(&self.current_world_id)?;
        Some(world.assume(triple, justification))
    }

    /// Retract an assumption from the current world
    pub fn retract(&mut self, assumption_id: u64) -> bool {
        if let Some(world) = self.worlds.get_mut(&self.current_world_id) {
            if world.retract(assumption_id) {
                // Update TMS and remove dependent facts
                let affected = self.tms.retract_assumption(assumption_id);
                for fact in affected {
                    world.local_facts.remove(&fact);
                }
                return true;
            }
        }
        false
    }

    /// Get all facts visible in a world (including inherited facts)
    pub fn facts_in_world(&self, world_id: WorldId) -> HashSet<Triple> {
        let mut facts = self.base_facts.clone();
        let mut current = Some(world_id);

        // Walk up the parent chain, collecting facts
        while let Some(wid) = current {
            if let Some(world) = self.worlds.get(&wid) {
                facts.extend(world.local_facts.iter().cloned());
                current = world.parent;
            } else {
                break;
            }
        }

        facts
    }

    /// Get facts in current world
    pub fn current_facts(&self) -> HashSet<Triple> {
        self.facts_in_world(self.current_world_id)
    }

    /// Perform forward chaining inference in a world
    pub fn infer_in_world(&mut self, world_id: WorldId) -> HashSet<Triple> {
        let mut facts = self.facts_in_world(world_id);
        let mut new_facts = HashSet::new();
        let mut iteration = 0;
        let max_iterations = 100;

        // Collect assumptions from this world for dependency tracking
        let assumptions: HashSet<u64> = self
            .worlds
            .get(&world_id)
            .map(|w| w.assumptions.iter().map(|a| a.id).collect())
            .unwrap_or_default();

        loop {
            let mut changed = false;
            iteration += 1;
            if iteration > max_iterations {
                break;
            }

            for (rule_idx, rule) in self.rules.iter().enumerate() {
                // Try to match rule antecedent (body)
                let bindings = self.match_rule_body(&rule.antecedent, &facts);

                for binding in bindings {
                    // Generate consequent (head) instantiation
                    if let Some(derived) = self.instantiate_head(&rule.consequent, &binding) {
                        if !facts.contains(&derived) {
                            facts.insert(derived.clone());
                            new_facts.insert(derived.clone());
                            changed = true;

                            // Record dependency
                            self.tms.record_dependency(
                                derived.clone(),
                                assumptions.clone(),
                                Some(rule_idx),
                            );
                        }
                    }
                }
            }

            if !changed {
                break;
            }
        }

        // Add new facts to the world
        if let Some(world) = self.worlds.get_mut(&world_id) {
            world.local_facts.extend(new_facts.iter().cloned());
        }

        new_facts
    }

    /// Perform inference in current world
    pub fn infer(&mut self) -> HashSet<Triple> {
        self.infer_in_world(self.current_world_id)
    }

    /// Match rule body against facts
    fn match_rule_body(
        &self,
        body: &[Triple],
        facts: &HashSet<Triple>,
    ) -> Vec<HashMap<String, Term>> {
        if body.is_empty() {
            return vec![HashMap::new()];
        }

        let mut all_bindings = vec![HashMap::new()];

        for pattern in body {
            let mut new_bindings = Vec::new();

            for binding in &all_bindings {
                let instantiated = self.apply_binding_to_triple(pattern, binding);

                for fact in facts {
                    if let Some(new_binding) = self.unify_triple(&instantiated, fact, binding) {
                        new_bindings.push(new_binding);
                    }
                }
            }

            all_bindings = new_bindings;
            if all_bindings.is_empty() {
                break;
            }
        }

        all_bindings
    }

    /// Apply binding to a triple
    fn apply_binding_to_triple(&self, triple: &Triple, binding: &HashMap<String, Term>) -> Triple {
        Triple {
            subject: self.apply_binding_to_term(&triple.subject, binding),
            predicate: self.apply_binding_to_term(&triple.predicate, binding),
            object: self.apply_binding_to_term(&triple.object, binding),
        }
    }

    /// Apply binding to a term
    fn apply_binding_to_term(&self, term: &Term, binding: &HashMap<String, Term>) -> Term {
        match term {
            Term::Variable(v) => binding.get(v.name()).cloned().unwrap_or_else(|| term.clone()),
            _ => term.clone(),
        }
    }

    /// Unify two triples
    fn unify_triple(
        &self,
        pattern: &Triple,
        fact: &Triple,
        initial_binding: &HashMap<String, Term>,
    ) -> Option<HashMap<String, Term>> {
        let mut binding = initial_binding.clone();

        if !self.unify_term(&pattern.subject, &fact.subject, &mut binding) {
            return None;
        }
        if !self.unify_term(&pattern.predicate, &fact.predicate, &mut binding) {
            return None;
        }
        if !self.unify_term(&pattern.object, &fact.object, &mut binding) {
            return None;
        }

        Some(binding)
    }

    /// Unify two terms
    fn unify_term(&self, pattern: &Term, fact: &Term, binding: &mut HashMap<String, Term>) -> bool {
        match pattern {
            Term::Variable(v) => {
                if let Some(bound) = binding.get(v.name()) {
                    bound == fact
                } else {
                    binding.insert(v.name().to_string(), fact.clone());
                    true
                }
            }
            _ => pattern == fact,
        }
    }

    /// Instantiate rule head with binding
    fn instantiate_head(&self, head: &[Triple], binding: &HashMap<String, Term>) -> Option<Triple> {
        if head.is_empty() {
            return None;
        }

        let instantiated = self.apply_binding_to_triple(&head[0], binding);

        // Check that all variables are bound (no free variables in result)
        if self.has_free_variables(&instantiated) {
            return None;
        }

        Some(instantiated)
    }

    /// Check if triple has free (unbound) variables
    fn has_free_variables(&self, triple: &Triple) -> bool {
        self.term_is_variable(&triple.subject)
            || self.term_is_variable(&triple.predicate)
            || self.term_is_variable(&triple.object)
    }

    fn term_is_variable(&self, term: &Term) -> bool {
        matches!(term, Term::Variable(_))
    }

    /// Perform what-if analysis: hypothesize facts and see what follows
    pub fn what_if(&mut self, hypotheses: Vec<Triple>) -> HypotheticalResult {
        // Create a new branch
        let world_id = self.branch(Some("what-if".to_string()));
        self.switch_to(world_id);

        // Add hypotheses as assumptions
        let mut assumptions = Vec::new();
        for hypothesis in hypotheses {
            if let Some(world) = self.worlds.get_mut(&world_id) {
                let assumption =
                    Assumption::new(hypothesis.clone(), Some("what-if hypothesis".to_string()));
                assumptions.push(assumption.clone());
                world.assumptions.push(assumption);
                world.local_facts.insert(hypothesis);
            }
        }

        // Perform inference
        self.infer();

        // Check consistency
        let (consistent, contradictions) = self.check_consistency(world_id);

        if let Some(world) = self.worlds.get_mut(&world_id) {
            world.consistent = consistent;
        }

        let derived_facts = self.facts_in_world(world_id);

        // Return to base world
        self.switch_to(self.base_world_id);

        HypotheticalResult {
            world_id,
            derived_facts,
            consistent,
            contradictions,
            assumptions,
        }
    }

    /// Check consistency of a world
    fn check_consistency(&self, world_id: WorldId) -> (bool, Vec<(Triple, Triple)>) {
        let facts = self.facts_in_world(world_id);
        let mut contradictions = Vec::new();

        // Look for explicit contradictions (owl:complementOf, negation as failure, etc.)
        // This is a simplified check - real implementation would be more sophisticated
        for fact in &facts {
            // Check for owl:sameAs / owl:differentFrom contradiction
            if let Term::Uri(p) = &fact.predicate {
                if p.as_str().contains("differentFrom") {
                    // Check if sameAs also exists
                    let same_as_check = Triple {
                        subject: fact.subject.clone(),
                        predicate: Term::uri(p.as_str().replace("differentFrom", "sameAs")),
                        object: fact.object.clone(),
                    };
                    if facts.contains(&same_as_check) {
                        contradictions.push((fact.clone(), same_as_check));
                    }
                }
            }
        }

        (contradictions.is_empty(), contradictions)
    }

    /// Counterfactual reasoning: what would be true if something were different
    pub fn counterfactual(&mut self, hypothesis: Triple) -> CounterfactualResult {
        // Get current facts
        let original_facts = self.current_facts();

        // Create counterfactual branch
        let world_id = self.branch(Some("counterfactual".to_string()));
        self.switch_to(world_id);

        // Add the counterfactual hypothesis
        self.assume(hypothesis.clone(), Some("counterfactual".to_string()));

        // Infer in this world
        self.infer();
        let counterfactual_facts = self.facts_in_world(world_id);

        // Compute differences
        let would_be_true: HashSet<Triple> = counterfactual_facts.clone();
        let would_become_false: HashSet<Triple> = original_facts
            .difference(&counterfactual_facts)
            .cloned()
            .collect();
        let newly_derived: HashSet<Triple> = counterfactual_facts
            .difference(&original_facts)
            .cloned()
            .collect();

        // Check coherence
        let (coherent, _) = self.check_consistency(world_id);

        // Return to original world
        self.switch_to(self.base_world_id);

        CounterfactualResult {
            hypothesis,
            would_be_true,
            would_become_false,
            newly_derived,
            coherent,
        }
    }

    /// Compare two worlds
    pub fn compare_worlds(&self, world_a: WorldId, world_b: WorldId) -> WorldComparison {
        let facts_a = self.facts_in_world(world_a);
        let facts_b = self.facts_in_world(world_b);

        let common: HashSet<Triple> = facts_a.intersection(&facts_b).cloned().collect();
        let only_a: HashSet<Triple> = facts_a.difference(&facts_b).cloned().collect();
        let only_b: HashSet<Triple> = facts_b.difference(&facts_a).cloned().collect();

        WorldComparison {
            world_a,
            world_b,
            common_facts: common,
            only_in_a: only_a,
            only_in_b: only_b,
        }
    }

    /// Get the world tree structure
    pub fn world_tree(&self) -> WorldTree {
        self.build_world_tree(self.base_world_id)
    }

    fn build_world_tree(&self, world_id: WorldId) -> WorldTree {
        let world = self.worlds.get(&world_id);
        let label = world.and_then(|w| w.label.clone());
        let fact_count = world.map(|w| w.local_facts.len()).unwrap_or(0);
        let consistent = world.map(|w| w.consistent).unwrap_or(true);

        let children: Vec<WorldTree> = world
            .map(|w| {
                w.children
                    .iter()
                    .map(|&child_id| self.build_world_tree(child_id))
                    .collect()
            })
            .unwrap_or_default();

        WorldTree {
            world_id,
            label,
            local_fact_count: fact_count,
            consistent,
            children,
        }
    }

    /// Find minimal assumption sets that support a fact
    pub fn find_support(&self, fact: &Triple, _world_id: WorldId) -> Vec<HashSet<u64>> {
        let deps = self.tms.get_dependencies(fact);
        if deps.is_empty() {
            // Fact is base fact or assumed directly
            return vec![HashSet::new()];
        }

        // For now, return the single dependency set
        vec![deps]
    }

    /// Check if a fact is necessarily true (true in all consistent worlds)
    pub fn necessarily_true(&self, fact: &Triple) -> bool {
        for (_, world) in &self.worlds {
            if world.consistent {
                let facts = self.facts_in_world(world.id);
                if !facts.contains(fact) {
                    return false;
                }
            }
        }
        true
    }

    /// Check if a fact is possibly true (true in some consistent world)
    pub fn possibly_true(&self, fact: &Triple) -> bool {
        for (_, world) in &self.worlds {
            if world.consistent {
                let facts = self.facts_in_world(world.id);
                if facts.contains(fact) {
                    return true;
                }
            }
        }
        false
    }

    /// Get all consistent worlds
    pub fn consistent_worlds(&self) -> Vec<WorldId> {
        self.worlds
            .iter()
            .filter(|(_, w)| w.consistent)
            .map(|(&id, _)| id)
            .collect()
    }

    /// Delete a world and its descendants
    pub fn delete_world(&mut self, world_id: WorldId) -> bool {
        if world_id == self.base_world_id {
            return false; // Cannot delete base world
        }

        // Get children to delete recursively
        let children: Vec<WorldId> = self
            .worlds
            .get(&world_id)
            .map(|w| w.children.clone())
            .unwrap_or_default();

        // Delete children first
        for child in children {
            self.delete_world(child);
        }

        // Remove from parent's children list
        if let Some(world) = self.worlds.get(&world_id) {
            if let Some(parent_id) = world.parent {
                if let Some(parent) = self.worlds.get_mut(&parent_id) {
                    parent.children.retain(|&c| c != world_id);
                }
            }
        }

        // Remove the world
        self.worlds.remove(&world_id);

        // Switch to base if current was deleted
        if self.current_world_id == world_id {
            self.current_world_id = self.base_world_id;
        }

        true
    }
}

impl Default for HypotheticalReasoner {
    fn default() -> Self {
        Self::new()
    }
}

/// Comparison between two worlds
#[derive(Debug, Clone)]
pub struct WorldComparison {
    pub world_a: WorldId,
    pub world_b: WorldId,
    pub common_facts: HashSet<Triple>,
    pub only_in_a: HashSet<Triple>,
    pub only_in_b: HashSet<Triple>,
}

/// Tree structure of hypothetical worlds
#[derive(Debug, Clone)]
pub struct WorldTree {
    pub world_id: WorldId,
    pub label: Option<String>,
    pub local_fact_count: usize,
    pub consistent: bool,
    pub children: Vec<WorldTree>,
}

/// Abductive reasoning support
pub struct AbductiveReasoner {
    /// Base reasoner
    hypothetical: HypotheticalReasoner,
    /// Abducibles (facts that can be hypothesized)
    abducibles: HashSet<Triple>,
}

impl AbductiveReasoner {
    pub fn new() -> Self {
        Self {
            hypothetical: HypotheticalReasoner::new(),
            abducibles: HashSet::new(),
        }
    }

    /// Mark a triple as abducible
    pub fn add_abducible(&mut self, triple: Triple) {
        self.abducibles.insert(triple);
    }

    /// Add a rule
    pub fn add_rule(&mut self, rule: Rule) {
        self.hypothetical.add_rule(rule);
    }

    /// Add a base fact
    pub fn add_fact(&mut self, triple: Triple) {
        self.hypothetical.add_base_fact(triple);
    }

    /// Find explanations for an observation
    pub fn explain(&mut self, observation: Triple) -> Vec<Explanation> {
        let mut explanations = Vec::new();

        // Try each subset of abducibles
        let abducibles_vec: Vec<Triple> = self.abducibles.iter().cloned().collect();

        // For simplicity, try single abducibles first
        for abducible in &abducibles_vec {
            let result = self.hypothetical.what_if(vec![abducible.clone()]);
            if result.consistent && result.derived_facts.contains(&observation) {
                explanations.push(Explanation {
                    hypotheses: vec![abducible.clone()],
                    derived_facts: result.derived_facts,
                    world_id: result.world_id,
                });
            }
        }

        // Try pairs if no single explanation found
        if explanations.is_empty() && abducibles_vec.len() >= 2 {
            for i in 0..abducibles_vec.len() {
                for j in (i + 1)..abducibles_vec.len() {
                    let hypotheses = vec![abducibles_vec[i].clone(), abducibles_vec[j].clone()];
                    let result = self.hypothetical.what_if(hypotheses.clone());
                    if result.consistent && result.derived_facts.contains(&observation) {
                        explanations.push(Explanation {
                            hypotheses,
                            derived_facts: result.derived_facts,
                            world_id: result.world_id,
                        });
                    }
                }
            }
        }

        // Sort by hypothesis count (prefer simpler explanations)
        explanations.sort_by_key(|e| e.hypotheses.len());

        explanations
    }
}

impl Default for AbductiveReasoner {
    fn default() -> Self {
        Self::new()
    }
}

/// An explanation for an observation
#[derive(Debug, Clone)]
pub struct Explanation {
    /// The hypothesized facts
    pub hypotheses: Vec<Triple>,
    /// All derived facts under this explanation
    pub derived_facts: HashSet<Triple>,
    /// The world in which this explanation holds
    pub world_id: WorldId,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_triple(s: &str, p: &str, o: &str) -> Triple {
        Triple {
            subject: Term::uri(s),
            predicate: Term::uri(p),
            object: Term::uri(o),
        }
    }

    #[test]
    fn test_hypothetical_world_creation() {
        let mut reasoner = HypotheticalReasoner::new();

        // Add base fact
        reasoner.add_base_fact(make_triple(":Alice", ":knows", ":Bob"));

        // Create branch
        let branch = reasoner.branch(Some("test".to_string()));
        reasoner.switch_to(branch);

        // Assume something
        reasoner.assume(
            make_triple(":Bob", ":knows", ":Charlie"),
            Some("test assumption".to_string()),
        );

        let facts = reasoner.current_facts();
        assert!(facts.contains(&make_triple(":Alice", ":knows", ":Bob")));
        assert!(facts.contains(&make_triple(":Bob", ":knows", ":Charlie")));
    }

    #[test]
    fn test_what_if_analysis() {
        let mut reasoner = HypotheticalReasoner::new();

        // Add rule: knows(X, Y) & knows(Y, Z) => knows(X, Z)
        reasoner.add_rule(Rule::new(
            vec![
                Triple {
                    subject: Term::universal("X"),
                    predicate: Term::uri(":knows"),
                    object: Term::universal("Y"),
                },
                Triple {
                    subject: Term::universal("Y"),
                    predicate: Term::uri(":knows"),
                    object: Term::universal("Z"),
                },
            ],
            vec![Triple {
                subject: Term::universal("X"),
                predicate: Term::uri(":knows"),
                object: Term::universal("Z"),
            }],
        ));

        // Add base fact
        reasoner.add_base_fact(make_triple(":Alice", ":knows", ":Bob"));

        // What if Bob knows Charlie?
        let result = reasoner.what_if(vec![make_triple(":Bob", ":knows", ":Charlie")]);

        assert!(result.consistent);
        // Should derive Alice knows Charlie
        assert!(result
            .derived_facts
            .contains(&make_triple(":Alice", ":knows", ":Charlie")));
    }

    #[test]
    fn test_counterfactual() {
        let mut reasoner = HypotheticalReasoner::new();

        reasoner.add_base_fact(make_triple(":Bird", ":canFly", ":true"));

        let result = reasoner.counterfactual(make_triple(":Bird", ":hasBrokenWing", ":true"));

        assert!(result.coherent);
        assert!(result
            .newly_derived
            .contains(&make_triple(":Bird", ":hasBrokenWing", ":true")));
    }

    #[test]
    fn test_world_comparison() {
        let mut reasoner = HypotheticalReasoner::new();

        reasoner.add_base_fact(make_triple(":A", ":p", ":B"));

        // Branch 1
        let world1 = reasoner.branch(Some("world1".to_string()));
        reasoner.switch_to(world1);
        reasoner.assume(make_triple(":C", ":p", ":D"), None);

        // Branch 2 from base
        reasoner.switch_to(reasoner.base_world_id);
        let world2 = reasoner.branch(Some("world2".to_string()));
        reasoner.switch_to(world2);
        reasoner.assume(make_triple(":E", ":p", ":F"), None);

        let comparison = reasoner.compare_worlds(world1, world2);

        assert!(comparison.common_facts.contains(&make_triple(":A", ":p", ":B")));
        assert!(comparison.only_in_a.contains(&make_triple(":C", ":p", ":D")));
        assert!(comparison.only_in_b.contains(&make_triple(":E", ":p", ":F")));
    }

    #[test]
    fn test_assumption_retraction() {
        let mut reasoner = HypotheticalReasoner::new();

        let branch = reasoner.branch(None);
        reasoner.switch_to(branch);

        let assumption_id = reasoner
            .assume(make_triple(":A", ":p", ":B"), None)
            .unwrap();
        assert!(reasoner
            .current_facts()
            .contains(&make_triple(":A", ":p", ":B")));

        reasoner.retract(assumption_id);
        assert!(!reasoner
            .current_facts()
            .contains(&make_triple(":A", ":p", ":B")));
    }

    #[test]
    fn test_necessarily_possibly() {
        let mut reasoner = HypotheticalReasoner::new();

        // Base fact is necessarily true
        reasoner.add_base_fact(make_triple(":A", ":p", ":B"));

        assert!(reasoner.necessarily_true(&make_triple(":A", ":p", ":B")));
        assert!(reasoner.possibly_true(&make_triple(":A", ":p", ":B")));

        // Create a world with another fact
        let world1 = reasoner.branch(None);
        reasoner.switch_to(world1);
        reasoner.assume(make_triple(":C", ":p", ":D"), None);

        // C p D is possibly true
        assert!(reasoner.possibly_true(&make_triple(":C", ":p", ":D")));
    }

    #[test]
    fn test_abductive_reasoning() {
        let mut reasoner = AbductiveReasoner::new();

        // Rule: hasFlu => hasHeadache
        reasoner.add_rule(Rule::new(
            vec![make_triple(":Patient", ":has", ":Flu")],
            vec![make_triple(":Patient", ":has", ":Headache")],
        ));

        // Mark hasFlu as abducible
        reasoner.add_abducible(make_triple(":Patient", ":has", ":Flu"));

        // Try to explain headache
        let explanations = reasoner.explain(make_triple(":Patient", ":has", ":Headache"));

        assert!(!explanations.is_empty());
        assert!(explanations[0]
            .hypotheses
            .contains(&make_triple(":Patient", ":has", ":Flu")));
    }

    #[test]
    fn test_world_tree() {
        let mut reasoner = HypotheticalReasoner::new();

        let world1 = reasoner.branch(Some("w1".to_string()));
        let _world2 = reasoner.branch(Some("w2".to_string()));

        reasoner.switch_to(world1);
        let _world3 = reasoner.branch(Some("w3".to_string()));

        let tree = reasoner.world_tree();

        assert_eq!(tree.children.len(), 2); // w1 and w2
        let w1_tree = tree.children.iter().find(|c| c.label == Some("w1".to_string()));
        assert!(w1_tree.is_some());
        assert_eq!(w1_tree.unwrap().children.len(), 1); // w3
    }

    #[test]
    fn test_delete_world() {
        let mut reasoner = HypotheticalReasoner::new();

        let world1 = reasoner.branch(Some("w1".to_string()));
        reasoner.switch_to(world1);
        let world2 = reasoner.branch(Some("w2".to_string()));

        assert!(reasoner.delete_world(world1));
        assert!(reasoner.get_world(world1).is_none());
        assert!(reasoner.get_world(world2).is_none()); // Child also deleted
    }

    #[test]
    fn test_tms_dependencies() {
        let mut tms = TruthMaintenanceSystem::new();

        let fact = make_triple(":A", ":p", ":B");
        let mut deps = HashSet::new();
        deps.insert(1);
        deps.insert(2);

        tms.record_dependency(fact.clone(), deps.clone(), Some(0));

        let retrieved = tms.get_dependencies(&fact);
        assert!(retrieved.contains(&1));
        assert!(retrieved.contains(&2));

        let dependents = tms.get_dependents(1);
        assert!(dependents.contains(&fact));
    }
}
