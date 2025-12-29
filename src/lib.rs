//! CWM - Closed World Machine
//!
//! A Rust implementation of the cwm N3 reasoner and RDF processor.
//!
//! # Architecture
//!
//! The crate is organized around core trait abstractions that enable extensibility:
//!
//! - [`core::BuiltinPredicate`] - Interface for built-in predicate implementations
//! - [`core::PredicateRegistry`] - Registry of built-in predicates
//! - [`core::TripleStore`] - Abstraction for triple storage backends
//! - [`core::InferenceEngine`] - Interface for reasoning strategies
//!
//! # Features
//!
//! - Parse RDF/N3 (Notation3) syntax
//! - Store and query RDF triples
//! - Forward-chaining inference with N3 rules
//! - 266+ built-in predicates for math, string, list, crypto, time operations
//! - Bidirectional constraint solving
//! - Tabling/memoization for cycle prevention
//! - Stratification analysis for negation
//! - Proof generation with W3C SWAP reason vocabulary
//!
//! # Example
//!
//! ```rust,ignore
//! use cwm::{Store, Reasoner, Rule, Term, Triple};
//!
//! let mut store = Store::new();
//! store.add(Triple::new(
//!     Term::uri("http://example.org/socrates"),
//!     Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
//!     Term::uri("http://example.org/Human"),
//! ));
//!
//! let rule = Rule::new(
//!     vec![Triple::new(
//!         Term::universal("x"),
//!         Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
//!         Term::uri("http://example.org/Human"),
//!     )],
//!     vec![Triple::new(
//!         Term::universal("x"),
//!         Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
//!         Term::uri("http://example.org/Mortal"),
//!     )],
//! );
//!
//! let mut reasoner = Reasoner::new();
//! reasoner.add_rule(rule);
//! reasoner.run(&mut store);
//! ```

// Core abstractions (Phase 1 of refactoring)
pub mod core;
pub mod config;
pub mod error;
pub mod term;
pub mod parser;
pub mod store;
pub mod reasoner;
pub mod reasoning;
pub mod builtins;
pub mod sparql;
pub mod fuseki;
pub mod prover;
pub mod server;
pub mod http_client;

// Re-export core traits and types
pub use crate::core::{
    EvalResult, BuiltinPredicate, PredicateRegistry, MutableRegistry, BuiltinNamespace,
    TripleStore, TripleStoreExt, PatternMatcher, InferenceEngine, InferenceResult, InferenceConfig,
    ReasonerHook, Unifier, UnifyResult,
    term_variables, triple_variables,
    ns,
};

// Re-export term types
pub use term::{Term, Triple, Bindings, Uri, Literal, Datatype, BlankNode, Variable, List, FormulaRef, set_run_prefix, clear_run_prefix};

// Re-export parser types
pub use parser::{parse, parse_kif, parse_rdfxml, ParseResult, ParseError, ParserState, Formula, N3Parser};

// Re-export store types
pub use store::{Store, SqliteStore, SqliteStoreError, SqliteStoreStats, SqliteResult};
pub use store::{Dataset, Quad};

// Re-export reasoner types
pub use reasoner::{Reasoner, ReasonerBuilder, Rule, ReasonerConfig, ReasonerStats, Proof, ProofStep, TablingState, StratificationResult};
pub use reasoner::proof::{ProofFormatter, ProofValidation, ProofStats, validate_proof};
pub use reasoner::parallel::{ParallelReasoner, ParallelConfig, ParallelStats};
pub use reasoner::incremental::{IncrementalReasoner, DependencyGraph, DependencyEntry, DependencyStats, TripleOrigin, TripleId, Delta, IncrementalStats};
pub use reasoner::explanation::{ExplanationGenerator, Explanation, ExplanationStep, ExplanationReason, ExplanationLevel, ExplanationConfig, WhyNotGenerator, WhyNotExplanation, WhyNotReason, NearMissRule};

// Re-export builtins (legacy compatibility)
pub use builtins::{BuiltinRegistry, BuiltinResult};

// Re-export SPARQL types
pub use sparql::{execute_sparql, QueryResult, format_results_xml, format_results_json};
pub use sparql::{QueryCache, CacheConfig, CacheStats, SharedQueryCache, create_shared_cache};
pub use sparql::{PathEvaluator, PathConfig, PathStats, PathAnalysis};
pub use sparql::{DatasetEngine, execute_sparql_dataset};
pub use sparql::{Update, UpdateEngine, UpdateResult, QuadData, GraphTarget, parse_update};
pub use sparql::{FederatedEngine, FederatedConfig, FederatedStats, ServiceQuery, ServiceResult};
pub use sparql::{QueryOptimizer, OptimizerConfig, DataStatistics, QueryPlan, PlanNode, JoinType, IndexHint, OptimizerStats, OptTriplePattern, PatternTerm};

// Re-export reasoning strategies
pub use reasoning::{ReasoningStrategy, StrategyConfig, InferenceStats, ForwardChaining, BackwardChaining, RdfsRules, Owl2RlRules};
pub use reasoning::{ConsistencyChecker, ConsistencyConfig, ConsistencyResult, ConsistencyStats, Violation, ViolationType};
pub use reasoning::{MaterializationEngine, MaterializationStrategy, MaterializationStats, MaterializationCost, PatternUsage, MaterializedView};
pub use reasoning::{TemporalReasoner, TemporalStore, TemporalTriple, TimeInterval, Instant as TemporalInstant, IntervalRelation, compute_relation};

// Re-export Tier 5: Probabilistic reasoning
pub use reasoning::{
    Probability, BeliefMass, ProbabilisticTriple, ProbabilitySource,
    ProbabilisticRule, CombinationMethod, ProbabilisticConfig, AggregationMethod,
    ProbabilisticStore, ProbabilisticStats, ProbabilisticReasoner,
    BayesianNode, BayesianNetwork,
};

// Re-export Tier 5: Reactive reasoning
pub use reasoning::{
    Event, SubscriptionId, Subscription, ReactiveConfig, ReactiveStats,
    EventQueue, ReactiveReasoner, ReactiveQuery, EventStream,
};

// Re-export Tier 5: Ontology versioning
pub use reasoning::{
    SemanticVersion, Change, ChangeCategory, ChangeSet, ChangeStats,
    VersionSnapshot, VersionHistory, VersionedStore, VersionError,
    Migration, MigrationStep, CompatibilityReport, compute_diff,
};

// Re-export Tier 5: Distributed reasoning
pub use reasoning::{
    PartitionStrategy, Partition, Message, QueryMessage, DistributedInferenceResult,
    DistributedConfig, DistributedStats, DistributedReasoner, PartitionAnalysis, GraphPartitioner,
};

// Re-export Tier 5: ML integration
pub use reasoning::{
    Embedding, EmbeddingConfig, EmbeddingSpace, TrainingStats,
    RulePrioritizer, PatternLearner, LearnedPattern,
    MLReasoner, MLReasonerConfig, MLInferenceStats,
};

// Re-export Tier 5: Natural language interface
pub use reasoning::{
    NLInterface, QueryParser, NLQueryPattern, QueryTemplate,
    RuleVerbalizer, NLExplainer, Vocabulary, VocabularyEntry,
    NLError, EntityMention, EntityLinker,
};

// Re-export Tier 6: Constraint Logic Programming
pub use reasoning::{
    Domain, CLPVariable, Constraint, ConstraintStore,
    Solver, SolverStats, Optimizer, Objective, SearchStrategy,
    PropagationResult,
};

// Re-export Tier 6: Defeasible Reasoning
pub use reasoning::{
    DefeasibleRule, RuleType, Argument, Attack, AttackType,
    ArgumentationFramework, Semantics, DefeasibleConfig, DefeasibleStats,
    DefeasibleReasoner, PreferenceOrdering, DialecticalTree,
};

// Re-export Tier 6: Hypothetical Reasoning
pub use reasoning::{
    Assumption, HypotheticalWorld, TruthMaintenanceSystem, Dependency,
    HypotheticalResult, CounterfactualResult, HypotheticalReasoner,
    WorldComparison, WorldTree, AbductiveReasoner, HypotheticalExplanation,
};

// Re-export Tier 6: Fuzzy Logic Integration
pub use reasoning::{
    FuzzyValue, MembershipFunction, FuzzySet, LinguisticVariable,
    InferenceMethod, DefuzzificationMethod, Hedge,
    FuzzyAntecedent, FuzzyConsequent, FuzzyLogicRule,
    FuzzyInferenceSystem, FuzzyTriple, TNorm, TConorm, FuzzyN3Reasoner,
};

// Re-export Tier 6: Knowledge Graph Completion
pub use reasoning::{
    Confidence, Prediction, Evidence, PredictionMethod, KGCPattern,
    EntityStats, RelationStats, KGCompletionEngine, KGCConfig,
    KGCLearnedRule, PathPattern, GraphStatistics,
};

// Re-export Tier 6: Semantic Caching
pub use reasoning::{
    SemanticHash, CacheQueryPattern, CacheFilter, CacheQueryResult,
    CacheEntry, EvictionStrategy, SemanticCacheConfig, SemanticCache, SemanticCacheStats,
    CacheMaterializedView, ViewManager, ResultDiff, IncrementalMaintainer,
};

// Re-export Tier 7: Inter-Process Communication
pub use reasoning::{
    Transport, IpcMessage, SerializationFormat, NodeConfig, ConnectionState,
    Connection, Node, NodeStats,
};

// Re-export Tier 7: Cluster Coordination
pub use reasoning::{
    ClusterConfig, NodeState, RaftRole, ClusterMember, RaftState,
    Barrier, DistributedLock, ClusterStats, Cluster, ClusterHandlers, ClusterBuilder,
};

// Re-export async server types
pub use server::{ServerConfig, AppState, run_server, create_router};

// Re-export configuration types
pub use config::{
    CwmConfig, ConfigError,
    GeneralConfig, ReasoningConfig, StoreConfig, SecurityConfig, ProfileConfig,
    OutputFormat, LogLevel, ReasoningProfile, StoreBackend,
};

// Re-export HTTP client types
pub use http_client::{
    get_sync_client, get_async_client, create_sync_client, create_async_client,
    HttpClientConfig, HttpError, is_domain_allowed,
};

// Re-export error types
pub use error::{
    CwmError, CwmResult, ErrorCode, ErrorContext, ErrorResponse,
};
