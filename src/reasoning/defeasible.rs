//! Defeasible Reasoning for N3
//!
//! Non-monotonic logic, argumentation frameworks, and defeat relations.
//!
//! # Features
//!
//! - Defeasible rules with priorities
//! - Argumentation frameworks (Dung-style)
//! - Attack/defeat relations
//! - Multiple semantics (grounded, preferred, stable)
//! - Skeptical and credulous reasoning
//!
//! # Example
//!
//! ```ignore
//! use cwm::reasoning::defeasible::{DefeasibleReasoner, DefeasibleRule, Argument};
//!
//! let mut reasoner = DefeasibleReasoner::new();
//!
//! // Add defeasible rules
//! reasoner.add_rule(DefeasibleRule::strict(rule1));
//! reasoner.add_rule(DefeasibleRule::defeasible(rule2, 1));
//!
//! // Compute justified conclusions
//! let conclusions = reasoner.compute_extensions(&store);
//! ```

use std::collections::{HashMap, HashSet, VecDeque};
use crate::term::{Term, Triple, Bindings, substitute_triple};
use crate::store::Store;
use crate::reasoner::Rule;

/// A defeasible rule
#[derive(Clone, Debug)]
pub struct DefeasibleRule {
    /// Underlying rule
    pub rule: Rule,
    /// Rule type
    pub rule_type: RuleType,
    /// Priority (higher = stronger)
    pub priority: i32,
    /// Rule name/identifier
    pub name: String,
}

impl DefeasibleRule {
    /// Create a strict rule (cannot be defeated)
    pub fn strict(rule: Rule) -> Self {
        let name = rule.name.clone().unwrap_or_else(|| "strict".to_string());
        DefeasibleRule {
            rule,
            rule_type: RuleType::Strict,
            priority: i32::MAX,
            name,
        }
    }

    /// Create a defeasible rule with priority
    pub fn defeasible(rule: Rule, priority: i32) -> Self {
        let name = rule.name.clone().unwrap_or_else(|| format!("def_{}", priority));
        DefeasibleRule {
            rule,
            rule_type: RuleType::Defeasible,
            priority,
            name,
        }
    }

    /// Create a defeater (only defeats, doesn't conclude)
    pub fn defeater(rule: Rule, priority: i32) -> Self {
        let name = rule.name.clone().unwrap_or_else(|| "defeater".to_string());
        DefeasibleRule {
            rule,
            rule_type: RuleType::Defeater,
            priority,
            name,
        }
    }

    /// Set rule name
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = name.into();
        self
    }
}

/// Type of defeasible rule
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RuleType {
    /// Strict rule - cannot be defeated
    Strict,
    /// Defeasible rule - can be defeated by stronger rules
    Defeasible,
    /// Defeater - only defeats, doesn't derive conclusions
    Defeater,
}

/// An argument in the argumentation framework
#[derive(Clone, Debug)]
pub struct Argument {
    /// Argument identifier
    pub id: usize,
    /// Conclusion of the argument
    pub conclusion: Triple,
    /// Rules used in the argument
    pub rules: Vec<usize>,
    /// Premises (supporting arguments)
    pub premises: Vec<usize>,
    /// Whether argument is strict (uses only strict rules)
    pub is_strict: bool,
    /// Strength (based on rule priorities)
    pub strength: i32,
}

impl Argument {
    /// Create a new argument
    pub fn new(id: usize, conclusion: Triple) -> Self {
        Argument {
            id,
            conclusion,
            rules: Vec::new(),
            premises: Vec::new(),
            is_strict: true,
            strength: 0,
        }
    }

    /// Add a rule to the argument
    pub fn with_rule(mut self, rule_idx: usize, priority: i32, is_strict: bool) -> Self {
        self.rules.push(rule_idx);
        self.strength = self.strength.max(priority);
        if !is_strict {
            self.is_strict = false;
        }
        self
    }

    /// Add a premise
    pub fn with_premise(mut self, premise: usize) -> Self {
        self.premises.push(premise);
        self
    }
}

/// Attack relation between arguments
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attack {
    /// Attacking argument
    pub attacker: usize,
    /// Attacked argument
    pub target: usize,
    /// Type of attack
    pub attack_type: AttackType,
}

/// Type of attack
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AttackType {
    /// Rebut - attacks the conclusion
    Rebut,
    /// Undercut - attacks the inference step
    Undercut,
    /// Undermine - attacks a premise
    Undermine,
}

/// Argumentation framework
#[derive(Clone, Debug)]
pub struct ArgumentationFramework {
    /// Arguments
    arguments: Vec<Argument>,
    /// Attack relations
    attacks: Vec<Attack>,
    /// Arguments by conclusion signature
    by_conclusion: HashMap<String, Vec<usize>>,
}

impl ArgumentationFramework {
    /// Create a new framework
    pub fn new() -> Self {
        ArgumentationFramework {
            arguments: Vec::new(),
            attacks: Vec::new(),
            by_conclusion: HashMap::new(),
        }
    }

    /// Add an argument
    pub fn add_argument(&mut self, arg: Argument) -> usize {
        let id = arg.id;
        let sig = format!("{:?}", arg.conclusion);
        self.by_conclusion.entry(sig).or_default().push(id);
        self.arguments.push(arg);
        id
    }

    /// Add an attack
    pub fn add_attack(&mut self, attack: Attack) {
        self.attacks.push(attack);
    }

    /// Get arguments attacking a given argument
    pub fn attackers(&self, target: usize) -> Vec<usize> {
        self.attacks.iter()
            .filter(|a| a.target == target)
            .map(|a| a.attacker)
            .collect()
    }

    /// Get arguments attacked by a given argument
    pub fn attacked_by(&self, attacker: usize) -> Vec<usize> {
        self.attacks.iter()
            .filter(|a| a.attacker == attacker)
            .map(|a| a.target)
            .collect()
    }

    /// Check if an argument is attacked
    pub fn is_attacked(&self, arg: usize) -> bool {
        self.attacks.iter().any(|a| a.target == arg)
    }

    /// Get all arguments
    pub fn arguments(&self) -> &[Argument] {
        &self.arguments
    }

    /// Get argument by ID
    pub fn get_argument(&self, id: usize) -> Option<&Argument> {
        self.arguments.get(id)
    }

    /// Get number of arguments
    pub fn num_arguments(&self) -> usize {
        self.arguments.len()
    }

    /// Get number of attacks
    pub fn num_attacks(&self) -> usize {
        self.attacks.len()
    }

    /// Compute grounded extension (most skeptical)
    pub fn grounded_extension(&self) -> HashSet<usize> {
        let mut grounded: HashSet<usize> = HashSet::new();
        let mut defeated: HashSet<usize> = HashSet::new();

        loop {
            let mut changed = false;

            for arg in &self.arguments {
                if grounded.contains(&arg.id) || defeated.contains(&arg.id) {
                    continue;
                }

                let attackers = self.attackers(arg.id);

                // Check if all attackers are defeated
                let all_defeated = attackers.iter()
                    .all(|a| defeated.contains(a));

                if all_defeated {
                    grounded.insert(arg.id);
                    changed = true;

                    // Defeat all arguments attacked by this one
                    for target in self.attacked_by(arg.id) {
                        if !defeated.contains(&target) {
                            defeated.insert(target);
                            changed = true;
                        }
                    }
                }
            }

            if !changed {
                break;
            }
        }

        grounded
    }

    /// Check if a set is conflict-free
    pub fn is_conflict_free(&self, set: &HashSet<usize>) -> bool {
        for &arg in set {
            for target in self.attacked_by(arg) {
                if set.contains(&target) {
                    return false;
                }
            }
        }
        true
    }

    /// Check if a set defends an argument
    pub fn defends(&self, set: &HashSet<usize>, arg: usize) -> bool {
        for attacker in self.attackers(arg) {
            // Check if set attacks the attacker
            let defended = set.iter()
                .any(|&defender| {
                    self.attacked_by(defender).contains(&attacker)
                });

            if !defended {
                return false;
            }
        }
        true
    }

    /// Check if a set is admissible
    pub fn is_admissible(&self, set: &HashSet<usize>) -> bool {
        if !self.is_conflict_free(set) {
            return false;
        }

        // Every argument in set must be defended by set
        for &arg in set {
            if !self.defends(set, arg) {
                return false;
            }
        }

        true
    }

    /// Compute preferred extensions
    pub fn preferred_extensions(&self) -> Vec<HashSet<usize>> {
        let admissible = self.all_admissible_sets();

        // Preferred = maximal admissible
        let mut preferred = Vec::new();

        for set in &admissible {
            let is_maximal = !admissible.iter()
                .any(|other| other != set && set.is_subset(other));

            if is_maximal {
                preferred.push(set.clone());
            }
        }

        preferred
    }

    /// Compute all admissible sets
    fn all_admissible_sets(&self) -> Vec<HashSet<usize>> {
        let mut admissible = Vec::new();
        let n = self.arguments.len();

        // Generate all subsets (exponential but complete)
        for mask in 0..(1u64 << n) {
            let set: HashSet<usize> = (0..n)
                .filter(|&i| (mask >> i) & 1 == 1)
                .collect();

            if self.is_admissible(&set) {
                admissible.push(set);
            }
        }

        admissible
    }

    /// Compute stable extensions
    pub fn stable_extensions(&self) -> Vec<HashSet<usize>> {
        let preferred = self.preferred_extensions();

        // Stable = preferred that attacks all non-members
        preferred.into_iter()
            .filter(|ext| {
                for arg in &self.arguments {
                    if !ext.contains(&arg.id) {
                        // Must be attacked by extension
                        let is_attacked = ext.iter()
                            .any(|&attacker| {
                                self.attacked_by(attacker).contains(&arg.id)
                            });

                        if !is_attacked {
                            return false;
                        }
                    }
                }
                true
            })
            .collect()
    }

    /// Skeptical acceptance (in all extensions)
    pub fn skeptically_accepted(&self, semantics: Semantics) -> HashSet<usize> {
        let extensions = match semantics {
            Semantics::Grounded => vec![self.grounded_extension()],
            Semantics::Preferred => self.preferred_extensions(),
            Semantics::Stable => self.stable_extensions(),
        };

        if extensions.is_empty() {
            return HashSet::new();
        }

        // Intersection of all extensions
        let mut result = extensions[0].clone();
        for ext in &extensions[1..] {
            result.retain(|a| ext.contains(a));
        }

        result
    }

    /// Credulous acceptance (in some extension)
    pub fn credulously_accepted(&self, semantics: Semantics) -> HashSet<usize> {
        let extensions = match semantics {
            Semantics::Grounded => vec![self.grounded_extension()],
            Semantics::Preferred => self.preferred_extensions(),
            Semantics::Stable => self.stable_extensions(),
        };

        // Union of all extensions
        let mut result = HashSet::new();
        for ext in extensions {
            result.extend(ext);
        }

        result
    }
}

impl Default for ArgumentationFramework {
    fn default() -> Self {
        Self::new()
    }
}

/// Argumentation semantics
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Semantics {
    /// Grounded semantics (most skeptical)
    Grounded,
    /// Preferred semantics
    Preferred,
    /// Stable semantics
    Stable,
}

impl Default for Semantics {
    fn default() -> Self {
        Semantics::Grounded
    }
}

/// Configuration for defeasible reasoning
#[derive(Clone, Debug)]
pub struct DefeasibleConfig {
    /// Semantics to use
    pub semantics: Semantics,
    /// Whether to use skeptical reasoning
    pub skeptical: bool,
    /// Maximum inference depth
    pub max_depth: usize,
    /// Whether to consider rule priorities
    pub use_priorities: bool,
}

impl Default for DefeasibleConfig {
    fn default() -> Self {
        DefeasibleConfig {
            semantics: Semantics::Grounded,
            skeptical: true,
            max_depth: 100,
            use_priorities: true,
        }
    }
}

/// Statistics for defeasible reasoning
#[derive(Clone, Debug, Default)]
pub struct DefeasibleStats {
    /// Number of arguments constructed
    pub arguments: usize,
    /// Number of attacks identified
    pub attacks: usize,
    /// Number of justified conclusions
    pub justified: usize,
    /// Number of defeated conclusions
    pub defeated: usize,
}

/// Defeasible reasoner
#[derive(Clone, Debug)]
pub struct DefeasibleReasoner {
    /// Defeasible rules
    rules: Vec<DefeasibleRule>,
    /// Configuration
    config: DefeasibleConfig,
    /// Statistics
    stats: DefeasibleStats,
}

impl DefeasibleReasoner {
    /// Create a new defeasible reasoner
    pub fn new() -> Self {
        DefeasibleReasoner {
            rules: Vec::new(),
            config: DefeasibleConfig::default(),
            stats: DefeasibleStats::default(),
        }
    }

    /// Create with configuration
    pub fn with_config(config: DefeasibleConfig) -> Self {
        DefeasibleReasoner {
            rules: Vec::new(),
            config,
            stats: DefeasibleStats::default(),
        }
    }

    /// Add a rule
    pub fn add_rule(&mut self, rule: DefeasibleRule) {
        self.rules.push(rule);
    }

    /// Add a strict rule
    pub fn add_strict(&mut self, rule: Rule) {
        self.rules.push(DefeasibleRule::strict(rule));
    }

    /// Add a defeasible rule
    pub fn add_defeasible(&mut self, rule: Rule, priority: i32) {
        self.rules.push(DefeasibleRule::defeasible(rule, priority));
    }

    /// Compute justified conclusions
    pub fn reason(&mut self, store: &Store) -> Vec<Triple> {
        self.stats = DefeasibleStats::default();

        // Build argumentation framework
        let framework = self.build_framework(store);

        self.stats.arguments = framework.num_arguments();
        self.stats.attacks = framework.num_attacks();

        // Compute accepted arguments
        let accepted = if self.config.skeptical {
            framework.skeptically_accepted(self.config.semantics)
        } else {
            framework.credulously_accepted(self.config.semantics)
        };

        self.stats.justified = accepted.len();
        self.stats.defeated = framework.num_arguments() - accepted.len();

        // Extract conclusions
        accepted.iter()
            .filter_map(|&id| {
                framework.get_argument(id).map(|a| a.conclusion.clone())
            })
            .collect()
    }

    /// Build argumentation framework from rules and facts
    fn build_framework(&self, store: &Store) -> ArgumentationFramework {
        let mut framework = ArgumentationFramework::new();
        let mut arg_id = 0;

        // Map from triple signature to argument IDs
        let mut triple_to_args: HashMap<String, Vec<usize>> = HashMap::new();

        // Build arguments from rule applications
        for (rule_idx, drule) in self.rules.iter().enumerate() {
            // Skip defeaters for conclusion generation
            if drule.rule_type == RuleType::Defeater {
                continue;
            }

            // Find matches for antecedent
            let matches = store.query(&drule.rule.antecedent);

            for bindings in matches {
                // Generate consequent triples
                for consequent in &drule.rule.consequent {
                    let triple = substitute_triple(consequent, &bindings);

                    if !triple.is_ground() {
                        continue;
                    }

                    let sig = format!("{:?}", triple);

                    // Create argument
                    let arg = Argument::new(arg_id, triple)
                        .with_rule(rule_idx, drule.priority, drule.rule_type == RuleType::Strict);

                    framework.add_argument(arg);
                    triple_to_args.entry(sig).or_default().push(arg_id);
                    arg_id += 1;
                }
            }
        }

        // Add arguments for base facts
        for triple in store.iter() {
            let sig = format!("{:?}", triple);
            if !triple_to_args.contains_key(&sig) {
                let arg = Argument::new(arg_id, triple.clone());
                framework.add_argument(arg);
                triple_to_args.entry(sig).or_default().push(arg_id);
                arg_id += 1;
            }
        }

        // Identify attacks (rebuttals)
        self.identify_attacks(&mut framework, &triple_to_args);

        framework
    }

    /// Identify attack relations
    fn identify_attacks(
        &self,
        framework: &mut ArgumentationFramework,
        triple_to_args: &HashMap<String, Vec<usize>>,
    ) {
        let arguments: Vec<Argument> = framework.arguments().to_vec();

        for arg in &arguments {
            // Find contradicting conclusions
            let neg_sig = self.negate_signature(&arg.conclusion);

            if let Some(conflicting) = triple_to_args.get(&neg_sig) {
                for &other_id in conflicting {
                    if other_id != arg.id {
                        // Determine attack direction based on priority
                        let other = &arguments[other_id];

                        if self.config.use_priorities {
                            // Higher priority defeats lower
                            if arg.strength > other.strength {
                                framework.add_attack(Attack {
                                    attacker: arg.id,
                                    target: other_id,
                                    attack_type: AttackType::Rebut,
                                });
                            } else if other.strength > arg.strength {
                                framework.add_attack(Attack {
                                    attacker: other_id,
                                    target: arg.id,
                                    attack_type: AttackType::Rebut,
                                });
                            } else {
                                // Equal priority: mutual attack
                                framework.add_attack(Attack {
                                    attacker: arg.id,
                                    target: other_id,
                                    attack_type: AttackType::Rebut,
                                });
                                framework.add_attack(Attack {
                                    attacker: other_id,
                                    target: arg.id,
                                    attack_type: AttackType::Rebut,
                                });
                            }
                        } else {
                            // Mutual attack
                            framework.add_attack(Attack {
                                attacker: arg.id,
                                target: other_id,
                                attack_type: AttackType::Rebut,
                            });
                        }
                    }
                }
            }
        }
    }

    /// Generate negation signature for a triple
    fn negate_signature(&self, triple: &Triple) -> String {
        // Simple negation: add/remove "not" prefix
        let sig = format!("{:?}", triple);
        if sig.starts_with("not_") {
            sig[4..].to_string()
        } else {
            format!("not_{}", sig)
        }
    }

    /// Get statistics
    pub fn stats(&self) -> &DefeasibleStats {
        &self.stats
    }

    /// Get framework for a store (for inspection)
    pub fn get_framework(&self, store: &Store) -> ArgumentationFramework {
        self.build_framework(store)
    }
}

impl Default for DefeasibleReasoner {
    fn default() -> Self {
        Self::new()
    }
}

/// Preference ordering between rules
#[derive(Clone, Debug)]
pub struct PreferenceOrdering {
    /// Preferences: (stronger, weaker)
    preferences: Vec<(String, String)>,
}

impl PreferenceOrdering {
    /// Create a new preference ordering
    pub fn new() -> Self {
        PreferenceOrdering {
            preferences: Vec::new(),
        }
    }

    /// Add a preference (rule1 > rule2)
    pub fn prefer(&mut self, stronger: impl Into<String>, weaker: impl Into<String>) {
        self.preferences.push((stronger.into(), weaker.into()));
    }

    /// Check if rule1 is preferred over rule2
    pub fn is_preferred(&self, rule1: &str, rule2: &str) -> bool {
        self.preferences.iter()
            .any(|(s, w)| s == rule1 && w == rule2)
    }

    /// Compute transitive closure
    pub fn transitive_closure(&self) -> PreferenceOrdering {
        let mut result = self.clone();
        let mut changed = true;

        while changed {
            changed = false;
            let current = result.preferences.clone();

            for (a, b) in &current {
                for (c, d) in &current {
                    if b == c && !result.is_preferred(a, d) {
                        result.prefer(a.clone(), d.clone());
                        changed = true;
                    }
                }
            }
        }

        result
    }
}

impl Default for PreferenceOrdering {
    fn default() -> Self {
        Self::new()
    }
}

/// Dialectical tree for argumentation
#[derive(Clone, Debug)]
pub struct DialecticalTree {
    /// Root argument
    root: usize,
    /// Children (counter-arguments)
    children: HashMap<usize, Vec<usize>>,
    /// Marking (defeated or undefeated)
    marking: HashMap<usize, bool>,
}

impl DialecticalTree {
    /// Create a new dialectical tree
    pub fn new(root: usize) -> Self {
        DialecticalTree {
            root,
            children: HashMap::new(),
            marking: HashMap::new(),
        }
    }

    /// Add a counter-argument
    pub fn add_counter(&mut self, parent: usize, counter: usize) {
        self.children.entry(parent).or_default().push(counter);
    }

    /// Mark the tree (bottom-up)
    pub fn mark(&mut self) {
        self.mark_recursive(self.root);
    }

    fn mark_recursive(&mut self, node: usize) -> bool {
        let children = self.children.get(&node).cloned().unwrap_or_default();

        if children.is_empty() {
            // Leaf node - undefeated
            self.marking.insert(node, true);
            return true;
        }

        // Mark children first
        for &child in &children {
            self.mark_recursive(child);
        }

        // Node is undefeated if all children are defeated
        let undefeated = children.iter()
            .all(|&child| !self.marking.get(&child).copied().unwrap_or(false));

        self.marking.insert(node, undefeated);
        undefeated
    }

    /// Check if root is warranted
    pub fn is_warranted(&self) -> bool {
        *self.marking.get(&self.root).unwrap_or(&false)
    }

    /// Get root
    pub fn root(&self) -> usize {
        self.root
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_argument_framework_basic() {
        let mut af = ArgumentationFramework::new();

        let a = af.add_argument(Argument::new(0, Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        )));

        let b = af.add_argument(Argument::new(1, Triple::new(
            Term::uri("http://example.org/c"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/d"),
        )));

        af.add_attack(Attack {
            attacker: a,
            target: b,
            attack_type: AttackType::Rebut,
        });

        assert!(af.is_attacked(b));
        assert!(!af.is_attacked(a));
    }

    #[test]
    fn test_grounded_extension() {
        let mut af = ArgumentationFramework::new();

        // A attacks B, B attacks C
        let a = af.add_argument(Argument::new(0, Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::literal("a"),
        )));

        let b = af.add_argument(Argument::new(1, Triple::new(
            Term::uri("http://example.org/b"),
            Term::uri("http://example.org/p"),
            Term::literal("b"),
        )));

        let c = af.add_argument(Argument::new(2, Triple::new(
            Term::uri("http://example.org/c"),
            Term::uri("http://example.org/p"),
            Term::literal("c"),
        )));

        af.add_attack(Attack { attacker: a, target: b, attack_type: AttackType::Rebut });
        af.add_attack(Attack { attacker: b, target: c, attack_type: AttackType::Rebut });

        let grounded = af.grounded_extension();

        // A is unattacked, so it's in grounded
        // A defeats B
        // C is defended by A (because A defeats B)
        assert!(grounded.contains(&a));
        assert!(!grounded.contains(&b));
        assert!(grounded.contains(&c));
    }

    #[test]
    fn test_conflict_free() {
        let mut af = ArgumentationFramework::new();

        let a = af.add_argument(Argument::new(0, Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::literal("a"),
        )));

        let b = af.add_argument(Argument::new(1, Triple::new(
            Term::uri("http://example.org/b"),
            Term::uri("http://example.org/p"),
            Term::literal("b"),
        )));

        af.add_attack(Attack { attacker: a, target: b, attack_type: AttackType::Rebut });

        let mut set1 = HashSet::new();
        set1.insert(a);
        assert!(af.is_conflict_free(&set1));

        let mut set2 = HashSet::new();
        set2.insert(a);
        set2.insert(b);
        assert!(!af.is_conflict_free(&set2));
    }

    #[test]
    fn test_admissible() {
        let mut af = ArgumentationFramework::new();

        let a = af.add_argument(Argument::new(0, Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::literal("a"),
        )));

        // Unattacked argument is admissible alone
        let mut set = HashSet::new();
        set.insert(a);
        assert!(af.is_admissible(&set));
    }

    #[test]
    fn test_defeasible_rule() {
        let rule = Rule::new(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/bird"),
                Term::literal("true"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/flies"),
                Term::literal("true"),
            )],
        );

        let drule = DefeasibleRule::defeasible(rule, 1);

        assert_eq!(drule.rule_type, RuleType::Defeasible);
        assert_eq!(drule.priority, 1);
    }

    #[test]
    fn test_defeasible_reasoner() {
        let mut reasoner = DefeasibleReasoner::new();

        // Birds fly (defeasible)
        let bird_flies = Rule::named(
            "bird_flies",
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Bird"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/flies"),
                Term::literal("true"),
            )],
        );

        reasoner.add_defeasible(bird_flies, 1);

        // Add a bird
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/tweety"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Bird"),
        ));

        let conclusions = reasoner.reason(&store);

        // Should conclude that tweety flies
        assert!(!conclusions.is_empty());
    }

    #[test]
    fn test_preference_ordering() {
        let mut prefs = PreferenceOrdering::new();

        prefs.prefer("r1", "r2");
        prefs.prefer("r2", "r3");

        assert!(prefs.is_preferred("r1", "r2"));
        assert!(!prefs.is_preferred("r1", "r3")); // Not transitive yet

        let closed = prefs.transitive_closure();
        assert!(closed.is_preferred("r1", "r3"));
    }

    #[test]
    fn test_dialectical_tree() {
        let mut tree = DialecticalTree::new(0);

        // Root has one counter-argument
        tree.add_counter(0, 1);
        // Counter has its own counter
        tree.add_counter(1, 2);

        tree.mark();

        // Node 2 is leaf -> undefeated
        // Node 1 has undefeated child -> defeated
        // Node 0 has defeated child -> undefeated
        assert!(tree.is_warranted());
    }

    #[test]
    fn test_stable_extension() {
        let mut af = ArgumentationFramework::new();

        // Self-attacking argument
        let a = af.add_argument(Argument::new(0, Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::literal("a"),
        )));

        af.add_attack(Attack { attacker: a, target: a, attack_type: AttackType::Rebut });

        let stable = af.stable_extensions();

        // For a single self-attacking argument, there's no stable extension:
        // - Empty set {} cannot attack 'a', so it's not stable
        // - {a} is not conflict-free (a attacks itself), so it's not admissible, thus not preferred
        assert!(stable.is_empty());
    }
}
