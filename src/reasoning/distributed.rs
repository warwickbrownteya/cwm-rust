//! Distributed Reasoning for N3
//!
//! Partitioning strategies, distributed inference, and coordination for large-scale reasoning.
//!
//! # Features
//!
//! - Graph partitioning strategies
//! - Distributed inference coordination
//! - Message passing between partitions
//! - Partition-local reasoning
//! - Result aggregation
//!
//! # Example
//!
//! ```ignore
//! use cwm::reasoning::distributed::{DistributedReasoner, PartitionStrategy, Partition};
//!
//! let mut distributed = DistributedReasoner::new(PartitionStrategy::HashBased { num_partitions: 4 });
//!
//! // Add data to partitions
//! distributed.add(triple);
//!
//! // Run distributed inference
//! let results = distributed.infer(&rules);
//! ```

use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::{Arc, Mutex, RwLock};
use std::hash::{Hash, Hasher};
use crate::term::{Term, Triple, Bindings, substitute_triple};
use crate::store::Store;
use crate::reasoner::{Rule, Reasoner, ReasonerConfig};

/// Strategy for partitioning data
#[derive(Clone, Debug)]
pub enum PartitionStrategy {
    /// Hash-based partitioning on subject
    HashBased {
        num_partitions: usize,
    },
    /// Predicate-based partitioning
    PredicateBased {
        predicates: HashMap<String, usize>,
        default_partition: usize,
    },
    /// Range-based partitioning on subject URI
    RangeBased {
        ranges: Vec<(String, String, usize)>,
    },
    /// Graph-based partitioning (minimize edge cuts)
    GraphBased {
        num_partitions: usize,
    },
    /// Manual assignment
    Manual,
}

impl Default for PartitionStrategy {
    fn default() -> Self {
        PartitionStrategy::HashBased { num_partitions: 4 }
    }
}

/// A partition of the knowledge graph
#[derive(Clone)]
pub struct Partition {
    /// Partition ID
    pub id: usize,
    /// Local store
    pub store: Store,
    /// Rules applicable to this partition
    pub rules: Vec<Rule>,
    /// Border triples (shared with other partitions)
    border: HashSet<String>,
    /// Inbound messages from other partitions
    inbox: Arc<Mutex<VecDeque<Message>>>,
}

impl Partition {
    /// Create a new partition
    pub fn new(id: usize) -> Self {
        Partition {
            id,
            store: Store::new(),
            rules: Vec::new(),
            border: HashSet::new(),
            inbox: Arc::new(Mutex::new(VecDeque::new())),
        }
    }

    /// Add a triple to this partition
    pub fn add(&mut self, triple: Triple) {
        self.store.add(triple);
    }

    /// Check if partition contains a triple
    pub fn contains(&self, triple: &Triple) -> bool {
        self.store.contains(triple)
    }

    /// Get number of triples
    pub fn len(&self) -> usize {
        self.store.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.store.is_empty()
    }

    /// Mark a triple as border triple
    pub fn mark_border(&mut self, triple: &Triple) {
        self.border.insert(format!("{:?}", triple));
    }

    /// Check if a triple is a border triple
    pub fn is_border(&self, triple: &Triple) -> bool {
        self.border.contains(&format!("{:?}", triple))
    }

    /// Get inbox for receiving messages
    pub fn inbox(&self) -> Arc<Mutex<VecDeque<Message>>> {
        self.inbox.clone()
    }

    /// Receive a message
    pub fn receive(&self, msg: Message) {
        let mut inbox = self.inbox.lock().unwrap();
        inbox.push_back(msg);
    }

    /// Process inbox messages
    pub fn process_inbox(&mut self) -> usize {
        let mut count = 0;
        let mut inbox = self.inbox.lock().unwrap();

        while let Some(msg) = inbox.pop_front() {
            match msg {
                Message::Triple(triple) => {
                    if !self.store.contains(&triple) {
                        self.store.add(triple);
                        count += 1;
                    }
                }
                Message::Bindings(_, bindings) => {
                    // Apply bindings to generate new triples
                    for rule in &self.rules {
                        for consequent in &rule.consequent {
                            let triple = substitute_triple(consequent, &bindings);
                            if triple.is_ground() && !self.store.contains(&triple) {
                                self.store.add(triple);
                                count += 1;
                            }
                        }
                    }
                }
                Message::Query(_) => {
                    // Queries handled separately
                }
            }
        }

        count
    }

    /// Run local inference
    pub fn infer(&mut self, config: &ReasonerConfig) -> InferenceResult {
        let start_count = self.store.len();

        let mut reasoner = Reasoner::with_config(config.clone());
        for rule in &self.rules {
            reasoner.add_rule(rule.clone());
        }

        reasoner.run(&mut self.store);

        let end_count = self.store.len();

        InferenceResult {
            partition_id: self.id,
            triples_before: start_count,
            triples_after: end_count,
            triples_derived: end_count - start_count,
        }
    }
}

impl std::fmt::Debug for Partition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Partition")
            .field("id", &self.id)
            .field("triples", &self.store.len())
            .field("rules", &self.rules.len())
            .field("border", &self.border.len())
            .finish()
    }
}

/// Message between partitions
#[derive(Clone, Debug)]
pub enum Message {
    /// A derived triple
    Triple(Triple),
    /// Bindings for a rule
    Bindings(usize, Bindings),
    /// Query request
    Query(QueryMessage),
}

/// Query message between partitions
#[derive(Clone, Debug)]
pub struct QueryMessage {
    /// Query ID
    pub id: u64,
    /// Pattern to match
    pub pattern: Triple,
    /// Requesting partition
    pub from: usize,
}

/// Result of partition-local inference
#[derive(Clone, Debug)]
pub struct InferenceResult {
    /// Partition ID
    pub partition_id: usize,
    /// Triples before inference
    pub triples_before: usize,
    /// Triples after inference
    pub triples_after: usize,
    /// Triples derived
    pub triples_derived: usize,
}

/// Configuration for distributed reasoning
#[derive(Clone, Debug)]
pub struct DistributedConfig {
    /// Number of partitions
    pub num_partitions: usize,
    /// Maximum rounds of message passing
    pub max_rounds: usize,
    /// Partition strategy
    pub strategy: PartitionStrategy,
    /// Maximum inference depth per partition
    pub max_local_depth: usize,
    /// Whether to replicate rules to all partitions
    pub replicate_rules: bool,
}

impl Default for DistributedConfig {
    fn default() -> Self {
        DistributedConfig {
            num_partitions: 4,
            max_rounds: 10,
            strategy: PartitionStrategy::default(),
            max_local_depth: 100,
            replicate_rules: true,
        }
    }
}

/// Statistics for distributed reasoning
#[derive(Clone, Debug, Default)]
pub struct DistributedStats {
    /// Total triples before
    pub total_triples_before: usize,
    /// Total triples after
    pub total_triples_after: usize,
    /// Total triples derived
    pub total_derived: usize,
    /// Messages exchanged
    pub messages_exchanged: usize,
    /// Rounds of coordination
    pub rounds: usize,
    /// Per-partition statistics
    pub partition_stats: Vec<InferenceResult>,
}

/// Distributed reasoner coordinator
pub struct DistributedReasoner {
    /// Partitions
    partitions: Vec<Arc<RwLock<Partition>>>,
    /// Global rules (not partitioned)
    global_rules: Vec<Rule>,
    /// Configuration
    config: DistributedConfig,
    /// Statistics
    stats: DistributedStats,
    /// Cross-partition dependencies
    dependencies: HashMap<usize, HashSet<usize>>,
}

impl DistributedReasoner {
    /// Create a new distributed reasoner
    pub fn new(strategy: PartitionStrategy) -> Self {
        let num_partitions = match &strategy {
            PartitionStrategy::HashBased { num_partitions } => *num_partitions,
            PartitionStrategy::PredicateBased { predicates, .. } => {
                predicates.values().max().map(|m| m + 1).unwrap_or(1)
            }
            PartitionStrategy::RangeBased { ranges } => {
                ranges.iter().map(|(_, _, p)| *p).max().map(|m| m + 1).unwrap_or(1)
            }
            PartitionStrategy::GraphBased { num_partitions } => *num_partitions,
            PartitionStrategy::Manual => 1,
        };

        let config = DistributedConfig {
            num_partitions,
            strategy,
            ..Default::default()
        };

        let partitions: Vec<Arc<RwLock<Partition>>> = (0..num_partitions)
            .map(|id| Arc::new(RwLock::new(Partition::new(id))))
            .collect();

        DistributedReasoner {
            partitions,
            global_rules: Vec::new(),
            config,
            stats: DistributedStats::default(),
            dependencies: HashMap::new(),
        }
    }

    /// Create with configuration
    pub fn with_config(config: DistributedConfig) -> Self {
        let partitions: Vec<Arc<RwLock<Partition>>> = (0..config.num_partitions)
            .map(|id| Arc::new(RwLock::new(Partition::new(id))))
            .collect();

        DistributedReasoner {
            partitions,
            global_rules: Vec::new(),
            config,
            stats: DistributedStats::default(),
            dependencies: HashMap::new(),
        }
    }

    /// Add a triple (routes to appropriate partition)
    pub fn add(&mut self, triple: Triple) {
        let partition_id = self.route(&triple);
        let partition = &self.partitions[partition_id];
        partition.write().unwrap().add(triple);
    }

    /// Route a triple to a partition
    fn route(&self, triple: &Triple) -> usize {
        match &self.config.strategy {
            PartitionStrategy::HashBased { num_partitions } => {
                let hash = hash_term(&triple.subject);
                (hash as usize) % num_partitions
            }
            PartitionStrategy::PredicateBased { predicates, default_partition } => {
                if let Term::Uri(uri) = &triple.predicate {
                    predicates.get(uri.as_str())
                        .copied()
                        .unwrap_or(*default_partition)
                } else {
                    *default_partition
                }
            }
            PartitionStrategy::RangeBased { ranges } => {
                if let Term::Uri(uri) = &triple.subject {
                    let uri_str = uri.as_str();
                    for (start, end, partition) in ranges {
                        if uri_str >= start.as_str() && uri_str < end.as_str() {
                            return *partition;
                        }
                    }
                }
                0
            }
            PartitionStrategy::GraphBased { .. } => {
                // Simple hash-based fallback
                let hash = hash_term(&triple.subject);
                (hash as usize) % self.config.num_partitions
            }
            PartitionStrategy::Manual => 0,
        }
    }

    /// Add a rule (replicates to all partitions if configured)
    pub fn add_rule(&mut self, rule: Rule) {
        self.global_rules.push(rule.clone());

        if self.config.replicate_rules {
            for partition in &self.partitions {
                partition.write().unwrap().rules.push(rule.clone());
            }
        }
    }

    /// Add a rule to a specific partition
    pub fn add_rule_to_partition(&mut self, partition_id: usize, rule: Rule) {
        if partition_id < self.partitions.len() {
            self.partitions[partition_id].write().unwrap().rules.push(rule);
        }
    }

    /// Run distributed inference
    pub fn infer(&mut self) -> &DistributedStats {
        self.stats = DistributedStats::default();

        // Count initial triples
        for partition in &self.partitions {
            self.stats.total_triples_before += partition.read().unwrap().len();
        }

        // Iterative coordination
        for round in 0..self.config.max_rounds {
            self.stats.rounds = round + 1;

            // Run local inference on each partition
            let results = self.run_local_inference();
            self.stats.partition_stats = results;

            // Exchange border triples
            let messages = self.exchange_borders();
            self.stats.messages_exchanged += messages;

            // Process inbox messages
            let processed = self.process_inboxes();

            // Check for fixpoint
            if messages == 0 && processed == 0 {
                break;
            }
        }

        // Count final triples
        for partition in &self.partitions {
            self.stats.total_triples_after += partition.read().unwrap().len();
        }
        self.stats.total_derived = self.stats.total_triples_after - self.stats.total_triples_before;

        &self.stats
    }

    /// Run local inference on all partitions
    fn run_local_inference(&self) -> Vec<InferenceResult> {
        let config = ReasonerConfig {
            max_steps: self.config.max_local_depth,
            recursive: true,
            filter: false,
            generate_proof: false,
            enable_tabling: true,
            enable_crypto: false,
        };

        self.partitions.iter()
            .map(|p| {
                p.write().unwrap().infer(&config)
            })
            .collect()
    }

    /// Exchange border triples between partitions
    fn exchange_borders(&mut self) -> usize {
        let mut messages = 0;

        // Identify triples that should be replicated
        let cross_partition: Vec<(usize, Triple)> = self.find_cross_partition_triples();

        for (from_partition, triple) in cross_partition {
            let target = self.route(&triple);

            if target != from_partition {
                let msg = Message::Triple(triple);
                self.partitions[target].read().unwrap().receive(msg);
                messages += 1;
            }
        }

        messages
    }

    /// Find triples that reference entities in other partitions
    fn find_cross_partition_triples(&self) -> Vec<(usize, Triple)> {
        let mut cross = Vec::new();

        // Collect all triples from all partitions
        let mut all_triples: Vec<(usize, Triple)> = Vec::new();
        for (id, p) in self.partitions.iter().enumerate() {
            let triples: Vec<Triple> = p.read().unwrap().store.iter().cloned().collect();
            for t in triples {
                all_triples.push((id, t));
            }
        }

        // Check for cross-partition references
        for (partition_id, triple) in &all_triples {
            // Check if object references an entity in another partition
            if let Term::Uri(_) = &triple.object {
                let object_partition = self.route_term(&triple.object);
                if object_partition != *partition_id {
                    cross.push((*partition_id, triple.clone()));
                }
            }
        }

        cross
    }

    /// Route a term to a partition
    fn route_term(&self, term: &Term) -> usize {
        // Create a dummy triple for routing
        let dummy = Triple::new(
            term.clone(),
            Term::uri("http://example.org/dummy"),
            Term::uri("http://example.org/dummy"),
        );
        self.route(&dummy)
    }

    /// Process inbox messages on all partitions
    fn process_inboxes(&self) -> usize {
        self.partitions.iter()
            .map(|p| p.write().unwrap().process_inbox())
            .sum()
    }

    /// Query across all partitions
    pub fn query(&self, patterns: &[Triple]) -> Vec<Bindings> {
        let mut all_results = Vec::new();

        for partition in &self.partitions {
            let results = partition.read().unwrap().store.query(patterns);
            all_results.extend(results);
        }

        // Deduplicate
        let mut seen = HashSet::new();
        all_results.retain(|b| {
            let key = format!("{:?}", b);
            if seen.contains(&key) {
                false
            } else {
                seen.insert(key);
                true
            }
        });

        all_results
    }

    /// Get partition by ID
    pub fn partition(&self, id: usize) -> Option<Arc<RwLock<Partition>>> {
        self.partitions.get(id).cloned()
    }

    /// Get number of partitions
    pub fn num_partitions(&self) -> usize {
        self.partitions.len()
    }

    /// Get total triple count
    pub fn total_triples(&self) -> usize {
        self.partitions.iter()
            .map(|p| p.read().unwrap().len())
            .sum()
    }

    /// Get statistics
    pub fn stats(&self) -> &DistributedStats {
        &self.stats
    }

    /// Merge all partitions into a single store
    pub fn merge(&self) -> Store {
        let mut store = Store::new();

        for partition in &self.partitions {
            for triple in partition.read().unwrap().store.iter() {
                store.add(triple.clone());
            }
        }

        store
    }

    /// Repartition data according to a new strategy
    pub fn repartition(&mut self, strategy: PartitionStrategy) {
        // Collect all triples
        let all_triples: Vec<Triple> = self.partitions.iter()
            .flat_map(|p| p.read().unwrap().store.iter().cloned().collect::<Vec<_>>())
            .collect();

        // Update strategy
        let num_partitions = match &strategy {
            PartitionStrategy::HashBased { num_partitions } => *num_partitions,
            PartitionStrategy::PredicateBased { predicates, .. } => {
                predicates.values().max().map(|m| m + 1).unwrap_or(1)
            }
            PartitionStrategy::RangeBased { ranges } => {
                ranges.iter().map(|(_, _, p)| *p).max().map(|m| m + 1).unwrap_or(1)
            }
            PartitionStrategy::GraphBased { num_partitions } => *num_partitions,
            PartitionStrategy::Manual => self.config.num_partitions,
        };

        self.config.strategy = strategy;
        self.config.num_partitions = num_partitions;

        // Create new partitions
        self.partitions = (0..num_partitions)
            .map(|id| Arc::new(RwLock::new(Partition::new(id))))
            .collect();

        // Replicate rules if needed
        if self.config.replicate_rules {
            for rule in &self.global_rules {
                for partition in &self.partitions {
                    partition.write().unwrap().rules.push(rule.clone());
                }
            }
        }

        // Re-add all triples
        for triple in all_triples {
            self.add(triple);
        }
    }
}

impl std::fmt::Debug for DistributedReasoner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DistributedReasoner")
            .field("partitions", &self.partitions.len())
            .field("global_rules", &self.global_rules.len())
            .field("total_triples", &self.total_triples())
            .field("stats", &self.stats)
            .finish()
    }
}

impl Default for DistributedReasoner {
    fn default() -> Self {
        Self::new(PartitionStrategy::default())
    }
}

/// Hash a term for partitioning
fn hash_term(term: &Term) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    let mut hasher = DefaultHasher::new();
    term.hash(&mut hasher);
    hasher.finish()
}

/// Partition analysis for load balancing
#[derive(Clone, Debug)]
pub struct PartitionAnalysis {
    /// Partition sizes (triple counts)
    pub sizes: Vec<usize>,
    /// Maximum partition size
    pub max_size: usize,
    /// Minimum partition size
    pub min_size: usize,
    /// Average partition size
    pub avg_size: f64,
    /// Standard deviation
    pub std_dev: f64,
    /// Imbalance factor (max/avg)
    pub imbalance: f64,
}

impl PartitionAnalysis {
    /// Analyze partition distribution
    pub fn analyze(reasoner: &DistributedReasoner) -> Self {
        let sizes: Vec<usize> = reasoner.partitions.iter()
            .map(|p| p.read().unwrap().len())
            .collect();

        let max_size = *sizes.iter().max().unwrap_or(&0);
        let min_size = *sizes.iter().min().unwrap_or(&0);
        let sum: usize = sizes.iter().sum();
        let avg_size = if sizes.is_empty() { 0.0 } else { sum as f64 / sizes.len() as f64 };

        let variance: f64 = sizes.iter()
            .map(|&s| (s as f64 - avg_size).powi(2))
            .sum::<f64>() / sizes.len().max(1) as f64;
        let std_dev = variance.sqrt();

        let imbalance = if avg_size > 0.0 { max_size as f64 / avg_size } else { 1.0 };

        PartitionAnalysis {
            sizes,
            max_size,
            min_size,
            avg_size,
            std_dev,
            imbalance,
        }
    }

    /// Check if partitions are reasonably balanced
    pub fn is_balanced(&self, threshold: f64) -> bool {
        self.imbalance <= threshold
    }
}

/// Graph-based partitioner using simple min-cut heuristic
#[derive(Clone, Debug)]
pub struct GraphPartitioner {
    /// Number of partitions
    num_partitions: usize,
    /// Current partition assignments
    assignments: HashMap<String, usize>,
}

impl GraphPartitioner {
    /// Create a new graph partitioner
    pub fn new(num_partitions: usize) -> Self {
        GraphPartitioner {
            num_partitions,
            assignments: HashMap::new(),
        }
    }

    /// Partition a store
    pub fn partition(&mut self, store: &Store) -> Vec<Vec<Triple>> {
        let mut partitions: Vec<Vec<Triple>> = (0..self.num_partitions)
            .map(|_| Vec::new())
            .collect();

        // Build adjacency information
        let edges = self.build_edges(store);

        // Assign vertices using greedy algorithm
        for (entity, neighbors) in &edges {
            let partition = self.best_partition(entity, neighbors);
            self.assignments.insert(entity.clone(), partition);
        }

        // Distribute triples
        for triple in store.iter() {
            let partition = self.get_triple_partition(&triple);
            partitions[partition].push(triple.clone());
        }

        partitions
    }

    /// Build edge map (entity -> neighbors)
    fn build_edges(&self, store: &Store) -> HashMap<String, HashSet<String>> {
        let mut edges: HashMap<String, HashSet<String>> = HashMap::new();

        for triple in store.iter() {
            if let (Term::Uri(subj), Term::Uri(obj)) = (&triple.subject, &triple.object) {
                let s = subj.as_str().to_string();
                let o = obj.as_str().to_string();

                edges.entry(s.clone()).or_default().insert(o.clone());
                edges.entry(o).or_default().insert(s);
            }
        }

        edges
    }

    /// Find best partition for an entity
    fn best_partition(&self, _entity: &str, neighbors: &HashSet<String>) -> usize {
        let mut partition_counts = vec![0; self.num_partitions];

        // Count neighbors in each partition
        for neighbor in neighbors {
            if let Some(&p) = self.assignments.get(neighbor) {
                partition_counts[p] += 1;
            }
        }

        // Find partition with most neighbors
        let (best, count) = partition_counts.iter()
            .enumerate()
            .max_by_key(|(_, &c)| c)
            .unwrap_or((0, &0));

        if *count > 0 {
            best
        } else {
            // Assign to least loaded partition
            partition_counts.iter()
                .enumerate()
                .min_by_key(|(_, &c)| c)
                .map(|(i, _)| i)
                .unwrap_or(0)
        }
    }

    /// Get partition for a triple
    fn get_triple_partition(&self, triple: &Triple) -> usize {
        if let Term::Uri(subj) = &triple.subject {
            self.assignments.get(subj.as_str())
                .copied()
                .unwrap_or(0)
        } else {
            0
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_partition_basic() {
        let mut partition = Partition::new(0);

        let triple = Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        );

        partition.add(triple.clone());
        assert!(partition.contains(&triple));
        assert_eq!(partition.len(), 1);
    }

    #[test]
    fn test_distributed_reasoner() {
        let mut reasoner = DistributedReasoner::new(
            PartitionStrategy::HashBased { num_partitions: 2 }
        );

        // Add triples
        for i in 0..10 {
            reasoner.add(Triple::new(
                Term::uri(format!("http://example.org/entity{}", i)),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Thing"),
            ));
        }

        assert_eq!(reasoner.total_triples(), 10);
    }

    #[test]
    fn test_distributed_inference() {
        let mut reasoner = DistributedReasoner::new(
            PartitionStrategy::HashBased { num_partitions: 2 }
        );

        // Add data
        reasoner.add(Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        ));

        // Add rule
        reasoner.add_rule(Rule::new(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Human"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Mortal"),
            )],
        ));

        reasoner.infer();

        // Check if inference worked
        let store = reasoner.merge();
        let mortal = Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Mortal"),
        );

        assert!(store.contains(&mortal));
    }

    #[test]
    fn test_predicate_based_partitioning() {
        let mut predicates = HashMap::new();
        predicates.insert("http://example.org/knows".to_string(), 0);
        predicates.insert("http://example.org/works".to_string(), 1);

        let mut reasoner = DistributedReasoner::new(
            PartitionStrategy::PredicateBased {
                predicates,
                default_partition: 0,
            }
        );

        reasoner.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/bob"),
        ));

        reasoner.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/works"),
            Term::uri("http://example.org/company"),
        ));

        // Check partition distribution
        let p0 = reasoner.partition(0).unwrap();
        let p1 = reasoner.partition(1).unwrap();

        assert_eq!(p0.read().unwrap().len(), 1);
        assert_eq!(p1.read().unwrap().len(), 1);
    }

    #[test]
    fn test_query_across_partitions() {
        let mut reasoner = DistributedReasoner::new(
            PartitionStrategy::HashBased { num_partitions: 2 }
        );

        // Add triples to different partitions
        for i in 0..10 {
            reasoner.add(Triple::new(
                Term::uri(format!("http://example.org/s{}", i)),
                Term::uri("http://example.org/p"),
                Term::uri("http://example.org/o"),
            ));
        }

        // Query across partitions
        let pattern = Triple::new(
            Term::universal("x"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/o"),
        );

        let results = reasoner.query(&[pattern]);
        assert_eq!(results.len(), 10);
    }

    #[test]
    fn test_partition_analysis() {
        let mut reasoner = DistributedReasoner::new(
            PartitionStrategy::HashBased { num_partitions: 4 }
        );

        // Add triples
        for i in 0..100 {
            reasoner.add(Triple::new(
                Term::uri(format!("http://example.org/entity{}", i)),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Thing"),
            ));
        }

        let analysis = PartitionAnalysis::analyze(&reasoner);

        assert_eq!(analysis.sizes.len(), 4);
        assert!(analysis.avg_size > 0.0);
        assert!(analysis.imbalance >= 1.0);
    }

    #[test]
    fn test_repartition() {
        let mut reasoner = DistributedReasoner::new(
            PartitionStrategy::HashBased { num_partitions: 2 }
        );

        for i in 0..10 {
            reasoner.add(Triple::new(
                Term::uri(format!("http://example.org/entity{}", i)),
                Term::uri("http://example.org/p"),
                Term::uri("http://example.org/o"),
            ));
        }

        assert_eq!(reasoner.num_partitions(), 2);

        // Repartition to 4
        reasoner.repartition(PartitionStrategy::HashBased { num_partitions: 4 });

        assert_eq!(reasoner.num_partitions(), 4);
        assert_eq!(reasoner.total_triples(), 10);
    }

    #[test]
    fn test_message_passing() {
        let partition = Partition::new(0);

        let triple = Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        );

        partition.receive(Message::Triple(triple.clone()));

        let inbox = partition.inbox();
        assert_eq!(inbox.lock().unwrap().len(), 1);
    }
}
