# CWM-Rust Refactoring Architecture

## Executive Summary

This document outlines a comprehensive refactoring strategy to transform cwm-rust from a working implementation into an extensible, composable reasoning framework. The refactoring applies SOLID principles, dependency inversion, and plugin architecture patterns while maintaining backward compatibility.

## Current State Analysis

### Strengths
- **266 working builtins** across 8 namespaces
- **Forward chaining** with tabling, proof generation, stratification
- **Comprehensive N3/Turtle parsing**
- **SPARQL query support**

### Pain Points
| Issue | Impact | Location |
|-------|--------|----------|
| Monolithic builtins | Hard to extend/test | `builtins/mod.rs` (4,737 lines) |
| Circular deps | Coupling | builtins ↔ parser |
| No trait abstractions | No pluggability | Throughout |
| String-based dispatch | No compile-time checks | BuiltinRegistry |
| O(n) store lookup | Performance | store/mod.rs |
| Mixed concerns in Reasoner | Rigidity | reasoner/mod.rs |

---

## Target Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                           cwm-rust                                   │
├─────────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌────────────┐ │
│  │   Parser    │  │   Store     │  │  Reasoner   │  │  Output    │ │
│  │  Pipeline   │  │  Backends   │  │  Strategies │  │ Formatters │ │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘  └─────┬──────┘ │
│         │                │                │                │        │
│  ┌──────┴────────────────┴────────────────┴────────────────┴──────┐ │
│  │                     Core Traits Layer                          │ │
│  │  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────────────┐  │ │
│  │  │TermStore │ │BuiltinEv │ │InferenceE│ │PatternMatcher    │  │ │
│  │  └──────────┘ └──────────┘ └──────────┘ └──────────────────┘  │ │
│  └────────────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │                     Core Types Layer                           │ │
│  │  Term, Triple, Bindings, Rule, Formula, Uri, Literal, etc.    │ │
│  └────────────────────────────────────────────────────────────────┘ │
├─────────────────────────────────────────────────────────────────────┤
│                        Builtin Plugins                              │
│  ┌─────┐ ┌──────┐ ┌────┐ ┌───┐ ┌────┐ ┌──────┐ ┌────┐ ┌─────┐    │
│  │Math │ │String│ │List│ │Log│ │Time│ │Crypto│ │ OS │ │Graph│    │
│  └─────┘ └──────┘ └────┘ └───┘ └────┘ └──────┘ └────┘ └─────┘    │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Phase 1: Core Traits (Non-Breaking)

### 1.1 Create `core/traits.rs`

```rust
//! Core abstractions for cwm-rust extensibility

use crate::term::{Term, Triple, Bindings, Variable};
use std::fmt::Debug;

/// Result of evaluating a built-in predicate
#[derive(Clone, Debug)]
pub enum EvalResult {
    /// Predicate succeeded with new/extended bindings
    Success(Bindings),
    /// Predicate failed (no match possible)
    Failure,
    /// Predicate cannot be evaluated yet (needs more bindings)
    NotReady,
    /// Predicate produced multiple solutions (for generators)
    MultiSuccess(Vec<Bindings>),
}

/// A built-in predicate evaluator
pub trait BuiltinPredicate: Send + Sync + Debug {
    /// The full URI of this predicate
    fn uri(&self) -> &str;

    /// Evaluate the predicate with given subject, object, and current bindings
    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult;

    /// Whether this predicate supports inverse/backward evaluation
    fn is_bidirectional(&self) -> bool { false }

    /// Human-readable description
    fn description(&self) -> &str { "" }
}

/// Registry of built-in predicates
pub trait PredicateRegistry: Send + Sync {
    /// Check if a predicate URI is registered
    fn is_registered(&self, uri: &str) -> bool;

    /// Get a predicate by URI
    fn get(&self, uri: &str) -> Option<&dyn BuiltinPredicate>;

    /// Evaluate a predicate (convenience method)
    fn evaluate(&self, uri: &str, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult;

    /// List all registered predicate URIs
    fn predicates(&self) -> Vec<&str>;
}

/// A triple store abstraction
pub trait TripleStore: Send + Sync {
    /// Add a triple to the store
    fn add(&mut self, triple: Triple);

    /// Remove a triple from the store
    fn remove(&mut self, triple: &Triple) -> bool;

    /// Check if a triple exists
    fn contains(&self, triple: &Triple) -> bool;

    /// Get the number of triples
    fn len(&self) -> usize;

    /// Check if empty
    fn is_empty(&self) -> bool { self.len() == 0 }

    /// Iterate over all triples
    fn iter(&self) -> Box<dyn Iterator<Item = &Triple> + '_>;

    /// Find triples matching a pattern (with variables)
    fn match_pattern(&self, pattern: &Triple) -> Vec<Bindings>;

    /// Clear all triples
    fn clear(&mut self);
}

/// Pattern matching strategy
pub trait PatternMatcher: Send + Sync {
    /// Match a pattern against a store, returning all valid bindings
    fn match_pattern(&self, store: &dyn TripleStore, pattern: &Triple) -> Vec<Bindings>;

    /// Match multiple patterns conjunctively
    fn match_patterns(&self, store: &dyn TripleStore, patterns: &[Triple]) -> Vec<Bindings>;
}

/// Inference engine abstraction
pub trait InferenceEngine: Send + Sync {
    /// Run inference on a store with given rules
    fn infer(
        &mut self,
        store: &mut dyn TripleStore,
        rules: &[crate::reasoner::Rule],
        registry: &dyn PredicateRegistry,
    ) -> InferenceResult;
}

/// Result of running inference
#[derive(Clone, Debug, Default)]
pub struct InferenceResult {
    pub steps: usize,
    pub rules_fired: usize,
    pub triples_derived: usize,
    pub converged: bool,
}

/// Hook for observing reasoning events
pub trait ReasonerHook: Send + Sync {
    fn on_triple_derived(&self, _triple: &Triple, _rule_index: usize) {}
    fn on_rule_fired(&self, _rule_index: usize, _bindings: &Bindings) {}
    fn on_builtin_evaluated(&self, _uri: &str, _result: &EvalResult) {}
    fn on_step_complete(&self, _step: usize, _derived: usize) {}
    fn on_inference_complete(&self, _result: &InferenceResult) {}
}
```

### 1.2 Create Namespace Constants

```rust
// core/namespaces.rs
pub mod ns {
    pub const RDF: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    pub const RDFS: &str = "http://www.w3.org/2000/01/rdf-schema#";
    pub const XSD: &str = "http://www.w3.org/2001/XMLSchema#";
    pub const OWL: &str = "http://www.w3.org/2002/07/owl#";
    pub const LOG: &str = "http://www.w3.org/2000/10/swap/log#";
    pub const MATH: &str = "http://www.w3.org/2000/10/swap/math#";
    pub const STRING: &str = "http://www.w3.org/2000/10/swap/string#";
    pub const LIST: &str = "http://www.w3.org/2000/10/swap/list#";
    pub const TIME: &str = "http://www.w3.org/2000/10/swap/time#";
    pub const CRYPTO: &str = "http://www.w3.org/2000/10/swap/crypto#";
    pub const OS: &str = "http://www.w3.org/2000/10/swap/os#";
    pub const GRAPH: &str = "http://www.w3.org/2000/10/swap/graph#";
}
```

---

## Phase 2: Modular Builtins

### 2.1 New Directory Structure

```
src/builtins/
├── mod.rs              # Registry + re-exports
├── registry.rs         # PluginRegistry implementation
├── helpers.rs          # Shared utilities (get_number, get_string, etc.)
├── math/
│   ├── mod.rs          # MathNamespace
│   ├── arithmetic.rs   # sum, difference, product, quotient
│   ├── comparison.rs   # lessThan, greaterThan, etc.
│   ├── functions.rs    # sin, cos, sqrt, log, etc.
│   └── rounding.rs     # floor, ceiling, rounded
├── string/
│   ├── mod.rs          # StringNamespace
│   ├── basic.rs        # concatenation, length, substring
│   ├── case.rs         # upperCase, lowerCase
│   ├── search.rs       # contains, startsWith, endsWith
│   ├── regex.rs        # matches, replace, split
│   └── encoding.rs     # xmlEscape, urlEncode, etc.
├── list/
│   ├── mod.rs          # ListNamespace
│   ├── access.rs       # first, rest, nth, member
│   ├── construct.rs    # append, cons, range
│   ├── transform.rs    # reverse, sort, unique, flatten
│   └── aggregate.rs    # length, sum, min, max, average
├── log/
│   ├── mod.rs          # LogNamespace
│   ├── equality.rs     # equalTo, notEqualTo
│   ├── formula.rs      # includes, notIncludes, conjunction
│   ├── semantics.rs    # semantics, parsedAsN3
│   └── control.rs      # ifThenElseIn, etc.
├── time/
│   ├── mod.rs          # TimeNamespace
│   └── datetime.rs     # now, inSeconds, date components
├── crypto/
│   ├── mod.rs          # CryptoNamespace
│   ├── hash.rs         # sha256, sha512, md5
│   └── encoding.rs     # base64Encode/Decode, hexEncode/Decode
├── os/
│   ├── mod.rs          # OsNamespace
│   └── system.rs       # environ, argv, cwd
└── graph/
    ├── mod.rs          # GraphNamespace
    └── operations.rs   # difference, union, intersection
```

### 2.2 Namespace Trait

```rust
// builtins/mod.rs
pub trait BuiltinNamespace: Send + Sync {
    /// Namespace URI prefix (e.g., "http://www.w3.org/2000/10/swap/math#")
    fn prefix(&self) -> &str;

    /// Register all predicates from this namespace
    fn register(&self, registry: &mut dyn MutableRegistry);

    /// Number of predicates in this namespace
    fn predicate_count(&self) -> usize;
}

pub trait MutableRegistry {
    fn register_predicate(&mut self, predicate: Box<dyn BuiltinPredicate>);
}
```

### 2.3 Example: Refactored Math Builtin

```rust
// builtins/math/arithmetic.rs
use crate::core::traits::{BuiltinPredicate, EvalResult};
use crate::term::{Term, Bindings};
use super::helpers::get_number;

/// math:sum - Bidirectional addition
#[derive(Debug)]
pub struct Sum;

impl BuiltinPredicate for Sum {
    fn uri(&self) -> &str {
        "http://www.w3.org/2000/10/swap/math#sum"
    }

    fn is_bidirectional(&self) -> bool { true }

    fn description(&self) -> &str {
        "(a b) math:sum c means a + b = c. Can solve for any one unknown."
    }

    fn evaluate(&self, subject: &Term, object: &Term, bindings: &Bindings) -> EvalResult {
        let Term::List(list) = subject else {
            return EvalResult::NotReady;
        };

        let items = list.to_vec();
        if items.len() != 2 {
            return EvalResult::NotReady;
        }

        let a = get_number(&items[0]);
        let b = get_number(&items[1]);
        let c = get_number(object);

        match (a, b, c) {
            // Forward: a + b = ?c
            (Some(a), Some(b), None) => {
                if let Term::Variable(var) = object {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), Term::decimal(a + b));
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            // Forward: verify a + b = c
            (Some(a), Some(b), Some(c)) => {
                if (a + b - c).abs() < 1e-10 {
                    EvalResult::Success(bindings.clone())
                } else {
                    EvalResult::Failure
                }
            }
            // Backward: ?a + b = c
            (None, Some(b), Some(c)) => {
                if let Term::Variable(var) = &items[0] {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), Term::decimal(c - b));
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            // Backward: a + ?b = c
            (Some(a), None, Some(c)) => {
                if let Term::Variable(var) = &items[1] {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var.clone(), Term::decimal(c - a));
                    EvalResult::Success(new_bindings)
                } else {
                    EvalResult::Failure
                }
            }
            _ => EvalResult::NotReady,
        }
    }
}
```

---

## Phase 3: Pluggable Store

### 3.1 Store Implementations

```rust
// store/mod.rs
mod memory;
mod indexed;

pub use memory::MemoryStore;
pub use indexed::IndexedStore;

// Default implementation
pub type Store = IndexedStore;
```

### 3.2 Indexed Store with SPO Indexes

```rust
// store/indexed.rs
use crate::core::traits::TripleStore;
use crate::term::{Term, Triple, Bindings};
use std::collections::{HashMap, HashSet};

/// Triple store with subject, predicate, object indexes for O(1) lookup
pub struct IndexedStore {
    triples: HashSet<Triple>,
    by_subject: HashMap<Term, HashSet<usize>>,
    by_predicate: HashMap<Term, HashSet<usize>>,
    by_object: HashMap<Term, HashSet<usize>>,
    triple_vec: Vec<Triple>, // For index-based access
}

impl TripleStore for IndexedStore {
    fn add(&mut self, triple: Triple) {
        if self.triples.insert(triple.clone()) {
            let idx = self.triple_vec.len();
            self.triple_vec.push(triple.clone());

            self.by_subject
                .entry(triple.subject.clone())
                .or_default()
                .insert(idx);
            self.by_predicate
                .entry(triple.predicate.clone())
                .or_default()
                .insert(idx);
            self.by_object
                .entry(triple.object.clone())
                .or_default()
                .insert(idx);
        }
    }

    fn match_pattern(&self, pattern: &Triple) -> Vec<Bindings> {
        // Use indexes for efficient matching
        let candidates = self.find_candidates(pattern);
        candidates
            .iter()
            .filter_map(|triple| self.unify(pattern, triple))
            .collect()
    }

    // ... other methods
}
```

---

## Phase 4: Reasoning Strategies

### 4.1 Strategy Trait

```rust
// reasoning/strategy.rs
use crate::core::traits::{TripleStore, PredicateRegistry, InferenceResult};
use crate::reasoner::Rule;

pub trait ReasoningStrategy: Send + Sync {
    fn name(&self) -> &str;

    fn infer(
        &mut self,
        store: &mut dyn TripleStore,
        rules: &[Rule],
        registry: &dyn PredicateRegistry,
        config: &ReasoningConfig,
    ) -> InferenceResult;
}

#[derive(Clone, Debug)]
pub struct ReasoningConfig {
    pub max_steps: usize,
    pub enable_tabling: bool,
    pub enable_stratification: bool,
    pub generate_proof: bool,
}
```

### 4.2 Strategy Implementations

```rust
// reasoning/forward.rs
pub struct ForwardChaining {
    tabling: Option<TablingState>,
    proof: Option<Proof>,
}

impl ReasoningStrategy for ForwardChaining {
    fn name(&self) -> &str { "forward-chaining" }
    // ... implementation
}

// reasoning/backward.rs (future)
pub struct BackwardChaining {
    goal_stack: Vec<Goal>,
    solutions: Vec<Bindings>,
}

impl ReasoningStrategy for BackwardChaining {
    fn name(&self) -> &str { "backward-chaining" }
    // ... implementation
}

// reasoning/hybrid.rs (future)
pub struct HybridChaining {
    forward: ForwardChaining,
    backward: BackwardChaining,
}
```

---

## Phase 5: Composable Reasoner Builder

### 5.1 Builder Pattern

```rust
// reasoner/builder.rs
pub struct ReasonerBuilder {
    store: Option<Box<dyn TripleStore>>,
    registry: Option<Box<dyn PredicateRegistry>>,
    strategy: Option<Box<dyn ReasoningStrategy>>,
    hooks: Vec<Box<dyn ReasonerHook>>,
    config: ReasoningConfig,
}

impl ReasonerBuilder {
    pub fn new() -> Self {
        Self {
            store: None,
            registry: None,
            strategy: None,
            hooks: Vec::new(),
            config: ReasoningConfig::default(),
        }
    }

    pub fn with_store<S: TripleStore + 'static>(mut self, store: S) -> Self {
        self.store = Some(Box::new(store));
        self
    }

    pub fn with_builtins<R: PredicateRegistry + 'static>(mut self, registry: R) -> Self {
        self.registry = Some(Box::new(registry));
        self
    }

    pub fn with_strategy<S: ReasoningStrategy + 'static>(mut self, strategy: S) -> Self {
        self.strategy = Some(Box::new(strategy));
        self
    }

    pub fn with_hook<H: ReasonerHook + 'static>(mut self, hook: H) -> Self {
        self.hooks.push(Box::new(hook));
        self
    }

    pub fn with_tabling(mut self, enabled: bool) -> Self {
        self.config.enable_tabling = enabled;
        self
    }

    pub fn with_proof_generation(mut self, enabled: bool) -> Self {
        self.config.generate_proof = enabled;
        self
    }

    pub fn build(self) -> Reasoner {
        Reasoner {
            store: self.store.unwrap_or_else(|| Box::new(IndexedStore::new())),
            registry: self.registry.unwrap_or_else(|| Box::new(StandardRegistry::new())),
            strategy: self.strategy.unwrap_or_else(|| Box::new(ForwardChaining::new())),
            hooks: self.hooks,
            config: self.config,
            rules: Vec::new(),
        }
    }
}
```

### 5.2 Usage Example

```rust
// Example usage
let reasoner = ReasonerBuilder::new()
    .with_store(IndexedStore::new())
    .with_builtins(StandardRegistry::new())
    .with_strategy(ForwardChaining::new())
    .with_tabling(true)
    .with_proof_generation(true)
    .with_hook(LoggingHook::new())
    .build();

reasoner.add_rule(rule);
let result = reasoner.run();
```

---

## Phase 6: Feature Flags

### 6.1 Cargo.toml Features

```toml
[features]
default = ["std-builtins", "forward-chaining", "indexed-store"]

# Builtin namespaces
std-builtins = ["math-builtins", "string-builtins", "list-builtins", "log-builtins"]
math-builtins = []
string-builtins = ["dep:regex"]
list-builtins = []
log-builtins = []
time-builtins = ["dep:chrono"]
crypto-builtins = ["dep:sha2", "dep:hmac", "dep:base64"]
os-builtins = []
graph-builtins = []

# Reasoning strategies
forward-chaining = []
backward-chaining = []
hybrid-chaining = ["forward-chaining", "backward-chaining"]

# Store backends
indexed-store = []
persistent-store = ["dep:sled"]

# Optional features
sparql = []
proof-generation = []
profiling = ["dep:tracing"]
```

---

## Migration Strategy

### Phase 1: Extract Traits (Week 1) - Non-Breaking
1. Create `core/traits.rs` with all trait definitions
2. Create `core/namespaces.rs` with URI constants
3. Update existing code to use namespace constants
4. Add trait implementations to existing types

### Phase 2: Split Builtins (Week 2) - Non-Breaking
1. Create modular directory structure
2. Extract builtins one namespace at a time
3. Implement `BuiltinNamespace` for each
4. Keep old `BuiltinRegistry` as facade delegating to new modules
5. Add comprehensive tests for each namespace

### Phase 3: Store Abstraction (Week 3) - Non-Breaking
1. Create `IndexedStore` with proper indexes
2. Implement `TripleStore` trait for both old and new stores
3. Benchmark and validate performance
4. Switch default to `IndexedStore`

### Phase 4: Reasoning Strategies (Week 4) - Breaking
1. Extract `ForwardChaining` strategy
2. Create `ReasonerBuilder`
3. Update `Reasoner` to use traits
4. Migrate CLI to use builder pattern
5. Update all tests

### Phase 5: Backward Chaining (Future)
1. Design goal-directed evaluation
2. Implement `BackwardChaining` strategy
3. Create `HybridChaining` combining both
4. Add query answering mode to CLI

---

## Benefits of This Architecture

| Aspect | Before | After |
|--------|--------|-------|
| **Extensibility** | Modify core code | Add plugin |
| **Testing** | Monolithic tests | Unit tests per module |
| **Builtin count** | Hard to track | Self-documenting per namespace |
| **Store performance** | O(n) lookup | O(1) with indexes |
| **Reasoning strategies** | Only forward | Pluggable strategies |
| **Configuration** | Scattered flags | Centralized config |
| **Compile times** | Full rebuild | Incremental per module |
| **Feature flags** | None | Fine-grained control |

---

## Appendix: File Size Targets

| Module | Current | Target |
|--------|---------|--------|
| `builtins/mod.rs` | 4,737 lines | ~200 lines |
| `builtins/math/` | (new) | ~800 lines total |
| `builtins/string/` | (new) | ~600 lines total |
| `builtins/list/` | (new) | ~500 lines total |
| `builtins/log/` | (new) | ~400 lines total |
| `reasoner/mod.rs` | 771 lines | ~300 lines |
| `reasoning/forward.rs` | (new) | ~300 lines |
| `store/indexed.rs` | (new) | ~400 lines |

Total: Similar LOC, but distributed for maintainability.
