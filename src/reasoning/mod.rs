//! Pluggable reasoning strategies for N3 inference
//!
//! This module provides abstract reasoning strategies that can be composed
//! with different stores and builtin registries.
//!
//! # Available Strategies
//!
//! - `ForwardChaining`: Bottom-up, data-driven inference (default)
//! - `BackwardChaining`: Top-down, goal-directed query answering
//!
//! # Usage
//!
//! ```ignore
//! use cwm::{Store, Rule, BuiltinRegistry, ForwardChaining, BackwardChaining, ReasoningStrategy};
//!
//! // Forward chaining: derive all consequences
//! let mut fc = ForwardChaining::new();
//! fc.infer(&mut store, &rules, &builtins, &config);
//!
//! // Backward chaining: answer specific queries
//! let bc = BackwardChaining::new();
//! let results = bc.query(&store, &goals, &rules, &builtins);
//! ```
//!
//! # Architecture
//!
//! Each strategy implements the `ReasoningStrategy` trait, allowing:
//! - Swappable inference algorithms
//! - Custom optimizations per strategy
//! - Hybrid approaches combining multiple strategies

mod strategy;
mod forward;
mod backward;
pub mod rdfs;
pub mod owl2rl;
pub mod consistency;
pub mod materialization;
pub mod temporal;
pub mod probabilistic;
pub mod reactive;
pub mod versioning;
pub mod distributed;
pub mod ml;
pub mod nlp;
pub mod clp;
pub mod defeasible;
pub mod hypothetical;
pub mod fuzzy;
pub mod kgc;
pub mod caching;
pub mod ipc;
pub mod cluster;

pub use strategy::{ReasoningStrategy, StrategyConfig, InferenceStats};
pub use forward::ForwardChaining;
pub use backward::BackwardChaining;
pub use rdfs::RdfsRules;
pub use owl2rl::Owl2RlRules;
pub use consistency::{ConsistencyChecker, ConsistencyConfig, ConsistencyResult, ConsistencyStats, Violation, ViolationType};
pub use materialization::{MaterializationEngine, MaterializationStrategy, MaterializationStats, MaterializationCost, PatternUsage, MaterializedView};
pub use temporal::{TemporalReasoner, TemporalStore, TemporalTriple, TimeInterval, Instant, IntervalRelation, compute_relation};
pub use probabilistic::{
    Probability, BeliefMass, ProbabilisticTriple, ProbabilitySource,
    ProbabilisticRule, CombinationMethod, ProbabilisticConfig, AggregationMethod,
    ProbabilisticStore, ProbabilisticStats, ProbabilisticReasoner,
    BayesianNode, BayesianNetwork,
};
pub use reactive::{
    Event, SubscriptionId, Subscription, EventHandler, ReactiveConfig, ReactiveStats,
    EventQueue, ReactiveReasoner, ReactiveQuery, EventStream,
};
pub use versioning::{
    SemanticVersion, Change, ChangeCategory, ChangeSet, ChangeStats,
    VersionSnapshot, VersionHistory, VersionedStore, VersionError,
    Migration, MigrationStep, CompatibilityReport, compute_diff,
};
pub use distributed::{
    PartitionStrategy, Partition, Message, QueryMessage, InferenceResult as DistributedInferenceResult,
    DistributedConfig, DistributedStats, DistributedReasoner, PartitionAnalysis, GraphPartitioner,
};
pub use ml::{
    Embedding, EmbeddingConfig, EmbeddingSpace, TrainingStats,
    RulePrioritizer, PatternLearner, LearnedPattern,
    MLReasoner, MLReasonerConfig, MLInferenceStats,
};
pub use nlp::{
    NLInterface, QueryParser, QueryPattern as NLQueryPattern, QueryTemplate,
    RuleVerbalizer, NLExplainer, Vocabulary, VocabularyEntry,
    NLError, EntityMention, EntityLinker,
};
pub use clp::{
    Domain, Variable as CLPVariable, Constraint, ConstraintStore,
    Solver, SolverStats, Optimizer, Objective, SearchStrategy,
    PropagationResult,
};
pub use defeasible::{
    DefeasibleRule, RuleType, Argument, Attack, AttackType,
    ArgumentationFramework, Semantics, DefeasibleConfig, DefeasibleStats,
    DefeasibleReasoner, PreferenceOrdering, DialecticalTree,
};
pub use hypothetical::{
    Assumption, HypotheticalWorld, TruthMaintenanceSystem, Dependency,
    HypotheticalResult, CounterfactualResult, HypotheticalReasoner,
    WorldComparison, WorldTree, AbductiveReasoner, Explanation as HypotheticalExplanation,
};
pub use fuzzy::{
    FuzzyValue, MembershipFunction, FuzzySet, LinguisticVariable,
    InferenceMethod, DefuzzificationMethod, Hedge,
    FuzzyAntecedent, FuzzyConsequent, FuzzyRule as FuzzyLogicRule,
    FuzzyInferenceSystem, FuzzyTriple, TNorm, TConorm, FuzzyN3Reasoner,
};
pub use kgc::{
    Confidence, Prediction, Evidence, PredictionMethod, Pattern as KGCPattern,
    EntityStats, RelationStats, KGCompletionEngine, KGCConfig,
    LearnedRule as KGCLearnedRule, PathPattern, GraphStatistics,
};
pub use caching::{
    SemanticHash, QueryPattern as CacheQueryPattern, Filter as CacheFilter, QueryResult as CacheQueryResult,
    CacheEntry, EvictionStrategy, CacheConfig as SemanticCacheConfig, SemanticCache, CacheStats as SemanticCacheStats,
    MaterializedView as CacheMaterializedView, ViewManager, ResultDiff, IncrementalMaintainer,
};
pub use ipc::{
    Transport, IpcMessage, SerializationFormat, NodeConfig, ConnectionState,
    Connection, Node, NodeStats,
};
pub use cluster::{
    ClusterConfig, NodeState, RaftRole, ClusterMember, RaftState,
    Barrier, DistributedLock, ClusterStats, Cluster, ClusterHandlers, ClusterBuilder,
};
