# CWM-Rust Reasoning Engines & Strategies

This document provides comprehensive documentation for the 25 reasoning strategies and 12 theorem proving engines available in cwm-rust.

## Table of Contents

- [Overview](#overview)
- [Core Reasoning Strategies](#core-reasoning-strategies)
- [Theorem Proving Engines](#theorem-proving-engines)
- [Reasoning Profiles](#reasoning-profiles)
- [Advanced Reasoning Features](#advanced-reasoning-features)
- [Configuration](#configuration)
- [Examples](#examples)

---

## Overview

cwm-rust implements a modular reasoning architecture with:

- **25 specialized reasoning modules** for different inference paradigms
- **12 theorem proving engines** for formal verification
- **7 pre-configured profiles** for common use cases
- **Pluggable strategy composition** for custom reasoning pipelines

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Reasoning Controller                      │
├─────────────────────────────────────────────────────────────┤
│  Strategy Selection  │  Profile Management  │  Engine Router │
├──────────────────────┴──────────────────────┴───────────────┤
│                     Strategy Modules                         │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐           │
│  │ Forward │ │Backward │ │  RDFS   │ │ OWL2RL  │  ...      │
│  └─────────┘ └─────────┘ └─────────┘ └─────────┘           │
├─────────────────────────────────────────────────────────────┤
│                     Prover Engines                           │
│  ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐              │
│  │Otter │ │ DPLL │ │ CDCL │ │Tableau│ │ SMT  │  ...        │
│  └──────┘ └──────┘ └──────┘ └──────┘ └──────┘              │
└─────────────────────────────────────────────────────────────┘
```

---

## Core Reasoning Strategies

### 1. Forward Chaining (`--think`)

**Module**: `reasoning/forward.rs`

Bottom-up, data-driven inference. Starts with known facts and applies rules to derive new facts until no more can be inferred (fixpoint).

```bash
cwm data.n3 rules.n3 --think
```

**Characteristics**:
- Exhaustive materialization of all derivable facts
- Best for: Complete inference, data enrichment
- Termination: Guaranteed with finite data and safe rules

**Example**:
```n3
# data.n3
@prefix ex: <http://example.org/> .
ex:socrates a ex:Human .

# rules.n3
{ ?x a ex:Human } => { ?x a ex:Mortal } .

# Result after --think:
ex:socrates a ex:Mortal .
```

### 2. Backward Chaining (Goal-Directed)

**Module**: `reasoning/backward.rs`

Top-down, goal-directed query answering. Starts with a goal and works backward to find supporting facts.

**Characteristics**:
- Lazy evaluation - only computes what's needed
- Best for: Query answering, proof construction
- Uses: SLD-resolution with tabling

**Example**:
```n3
# Query: Is Socrates mortal?
{ ex:socrates a ex:Mortal } log:conclusion ?proof .
```

### 3. RDFS Entailment (`--profile rdfs`)

**Module**: `reasoning/rdfs.rs`

Implements RDFS semantics including:

| Rule | Description |
|------|-------------|
| rdfs1 | Literal range typing |
| rdfs2 | Domain inference |
| rdfs3 | Range inference |
| rdfs4 | Resource typing |
| rdfs5 | Subproperty transitivity |
| rdfs6 | Property self-typing |
| rdfs7 | Subproperty inference |
| rdfs8 | Class hierarchy root |
| rdfs9 | Type propagation via subclass |
| rdfs10 | Class self-typing |
| rdfs11 | Subclass transitivity |
| rdfs12 | ContainerMembershipProperty |
| rdfs13 | Literal subclass |

```bash
cwm ontology.n3 data.n3 --profile rdfs --think
```

### 4. OWL 2 RL (`--profile owl`)

**Module**: `reasoning/owl2rl.rs`

Implements OWL 2 RL profile rules for tractable OWL reasoning:

**Property Axioms**:
- `owl:inverseOf`, `owl:SymmetricProperty`, `owl:TransitiveProperty`
- `owl:FunctionalProperty`, `owl:InverseFunctionalProperty`
- Property chains (`owl:propertyChainAxiom`)

**Class Axioms**:
- `owl:equivalentClass`, `rdfs:subClassOf`
- `owl:intersectionOf`, `owl:unionOf`
- `owl:hasValue`, `owl:someValuesFrom`, `owl:allValuesFrom`

**Equality**:
- `owl:sameAs` transitivity and substitution
- `owl:differentFrom` checking

```bash
cwm ontology.owl data.n3 --profile owl --think
```

### 5. Consistency Checking

**Module**: `reasoning/consistency.rs`

Detects logical inconsistencies and constraint violations:

- **Type Conflicts**: Entity with disjoint types
- **Cardinality Violations**: min/max cardinality constraints
- **Functional Property Violations**: Multiple values for functional properties
- **Disjointness Violations**: Instances of disjoint classes

```bash
cwm data.n3 --check-consistency
```

### 6. Temporal Reasoning

**Module**: `reasoning/temporal.rs`

Reasoning over time intervals and temporal relations:

**Allen's Interval Relations**:
- `before`, `after`, `meets`, `metBy`
- `overlaps`, `overlappedBy`, `starts`, `startedBy`
- `during`, `contains`, `finishes`, `finishedBy`
- `equals`

**Temporal Operators**:
- `always`, `eventually`, `until`, `since`

```n3
@prefix time: <http://www.w3.org/2006/time#> .
@prefix allen: <http://example.org/allen#> .

ex:meeting1 time:hasBeginning "2024-01-01T09:00:00Z" ;
            time:hasEnd "2024-01-01T10:00:00Z" .

{ ?x allen:before ?y } => { ?y allen:after ?x } .
```

### 7. Probabilistic Reasoning

**Module**: `reasoning/probabilistic.rs`

Bayesian network inference with belief propagation:

**Features**:
- Prior and conditional probability specification
- Evidence incorporation
- Multiple propagation methods: SumProduct, MaxProduct
- Aggregation: Noisy-OR, Noisy-AND, Average

```n3
@prefix prob: <http://example.org/prob#> .

ex:rain prob:hasProbability 0.3 .
{ ex:rain prob:hasProbability ?p .
  ?p math:greaterThan 0.5 }
=> { ex:takeUmbrella a ex:Recommended } .
```

### 8. Fuzzy Logic Reasoning

**Module**: `reasoning/fuzzy.rs`

Reasoning with degrees of truth and vague concepts:

**Membership Functions**:
- Triangular, Trapezoidal, Gaussian, Sigmoidal

**T-Norms** (AND):
- Minimum, Product, Łukasiewicz, Drastic

**T-Conorms** (OR):
- Maximum, Probabilistic Sum, Bounded Sum

**Defuzzification**:
- Centroid, Bisector, Mean of Maximum

```n3
@prefix fuzzy: <http://example.org/fuzzy#> .

ex:temperature fuzzy:value 28 .
ex:hot fuzzy:membershipFunction [
    a fuzzy:Triangular ;
    fuzzy:parameters (25 30 35)
] .
```

### 9. Defeasible Reasoning

**Module**: `reasoning/defeasible.rs`

Non-monotonic reasoning with exceptions and preferences:

**Features**:
- Strict rules (always apply)
- Defeasible rules (apply unless defeated)
- Defeaters (block other rules)
- Superiority relations

**Argumentation Semantics**:
- Grounded, Preferred, Stable, Complete

```n3
@prefix def: <http://example.org/defeasible#> .

# Birds typically fly
{ ?x a ex:Bird } def:defeasibly { ?x ex:canFly true } .

# Penguins are birds that don't fly
{ ?x a ex:Penguin } def:defeats { ?x ex:canFly true } .
{ ?x a ex:Penguin } => { ?x a ex:Bird } .
{ ?x a ex:Penguin } => { ?x ex:canFly false } .
```

### 10. Hypothetical Reasoning

**Module**: `reasoning/hypothetical.rs`

Reasoning with assumptions and counterfactuals:

**Features**:
- Assumption introduction and retraction
- Truth maintenance (ATMS-style)
- Abductive reasoning
- What-if analysis

```n3
@prefix hyp: <http://example.org/hypothetical#> .

# Assume rain, derive consequences
hyp:scenario1 hyp:assumes { ex:weather a ex:Rainy } .
hyp:scenario1 hyp:derives { ex:ground a ex:Wet } .
```

### 11. Constraint Logic Programming

**Module**: `reasoning/clp.rs`

Combining logic programming with constraint solving:

**Constraint Domains**:
- Finite domains (CLP(FD))
- Real numbers (CLP(R))
- Booleans (CLP(B))

**Search Strategies**:
- Depth-first, Breadth-first, Best-first
- Variable ordering heuristics
- Constraint propagation

```n3
@prefix clp: <http://example.org/clp#> .

# Constraint: X + Y = 10, X > 3, Y > 3
ex:problem clp:constraints [
    clp:add (ex:X ex:Y 10) ;
    clp:gt (ex:X 3) ;
    clp:gt (ex:Y 3)
] .
```

### 12. Machine Learning Integration

**Module**: `reasoning/ml.rs`

Hybrid neuro-symbolic reasoning:

**Features**:
- Entity embeddings (vector representations)
- Pattern learning from examples
- Rule confidence scoring
- Transfer learning for new domains

### 13. Natural Language Interface

**Module**: `reasoning/nlp.rs`

Natural language query and explanation:

**Features**:
- Query parsing from English
- Rule verbalization (explanation generation)
- Entity linking (text to URIs)
- Domain vocabulary management

---

## Theorem Proving Engines

Select an engine with `--engine <name>`:

### Resolution-Based Provers

#### 1. `resolution` - Classical Resolution

Standard resolution with factoring and subsumption.

```bash
cwm axioms.n3 --engine resolution --think
```

**Best for**: General first-order logic proving

#### 2. `otter` - Otter Strategy

Given-clause algorithm with set-of-support strategy.

```bash
cwm axioms.n3 --engine otter --think
```

**Features**:
- Set-of-support restriction
- Demodulation (equality simplification)
- Subsumption checking
- Weight-based clause selection

**Best for**: Efficient refutation proofs

#### 3. `prover9` - Prover9 Style

Advanced resolution with LPO term ordering.

**Features**:
- Lexicographic path ordering
- Hint handling
- Clause weighting
- Proof optimization

#### 4. `superposition` - Superposition Calculus

Modern equality reasoning (Vampire/E style).

```bash
cwm equational.n3 --engine superposition --think
```

**Features**:
- Paramodulation
- Ordering constraints
- Literal selection
- Redundancy elimination

**Best for**: Equational theories, algebra

### Connection-Based Provers

#### 5. `leancop` - Lean Connection

Minimal connection calculus prover.

**Features**:
- Clausal normal form
- Connection-driven search
- Regularity checking

**Best for**: Compact proofs, minimal search

#### 6. `nanocop` - Non-Clausal Connection

Connection prover without clausification.

**Features**:
- Direct formula handling
- Preserves structure
- Matrix characterization

### Tableau-Based Provers

#### 7. `tableau` - Analytic Tableaux

Classical semantic tableaux for first-order logic.

```bash
cwm logic.n3 --engine tableau --think --why
```

**Features**:
- Alpha/beta rule application
- Gamma (universal) and delta (existential) rules
- Loop checking
- Proof tree generation

**Best for**: Intuitive proofs, explanation generation

#### 8. `dl-tableau` - Description Logic Tableau

Specialized for OWL/DL reasoning.

```bash
cwm ontology.owl --engine dl-tableau --think
```

**Features**:
- Concept satisfiability
- Role assertions
- Blocking strategies
- Optimization for DL fragments

**Best for**: OWL ontologies, DL reasoning

### SAT/SMT Solvers

#### 9. `dpll` - DPLL Algorithm

Classic Davis-Putnam-Logemann-Loveland SAT solver.

**Features**:
- Unit propagation
- Pure literal elimination
- Backtracking search

#### 10. `cdcl` - Conflict-Driven Clause Learning

Modern SAT solver with learning.

```bash
cwm propositional.n3 --engine cdcl --think
```

**Features**:
- Conflict analysis
- Clause learning (1-UIP)
- Non-chronological backtracking
- VSIDS variable selection
- Restart strategies

**Best for**: Large SAT problems, industrial instances

#### 11. `smt` - SMT Solver

DPLL(T) with theory solvers.

**Features**:
- Equality with Uninterpreted Functions (EUF)
- Linear Integer Arithmetic (LIA)
- Theory propagation
- E-matching for quantifiers

### Term Rewriting

#### 12. `knuth-bendix` - Knuth-Bendix Completion

Completion procedure for equational theories.

```bash
cwm group-axioms.n3 --engine knuth-bendix --think
```

**Features**:
- Critical pair computation
- Reduction ordering
- Unfailing completion
- Canonical rewrite systems

**Best for**: Word problems, algebraic structures

---

## Reasoning Profiles

Pre-configured reasoning settings for common scenarios:

| Profile | Steps | Passes | Engine | Features |
|---------|-------|--------|--------|----------|
| `default` | 10,000 | 1 | - | Standard forward chaining |
| `rdfs` | 50,000 | 2 | - | RDFS entailment rules |
| `owl` | 100,000 | 3 | dl-tableau | OWL 2 RL rules |
| `shacl` | 10,000 | 1 | - | SHACL validation |
| `performance` | 1,000 | 1 | - | Speed optimized |
| `complete` | ∞ | 5 | - | Maximum completeness |
| `custom` | config | config | config | User-defined |

```bash
cwm data.n3 --profile rdfs --think
cwm data.n3 --profile owl --think
cwm data.n3 --profile performance --think
```

---

## Advanced Reasoning Features

### Tabling (Memoization)

Prevents infinite loops and recomputation:

```toml
[reasoning]
enable_tabling = true
```

### Stratification

Handles negation correctly via stratified evaluation:

```toml
[reasoning]
enable_stratification = true
```

### Distributed Reasoning

**Module**: `reasoning/distributed.rs`

Partitions reasoning across multiple nodes:

- Graph partitioning strategies
- Parallel rule application
- Result merging

### IPC & Clustering

**Modules**: `reasoning/ipc.rs`, `reasoning/cluster.rs`

Inter-process communication and cluster coordination:

- TCP/UDP/Unix socket transports
- Raft-based leader election
- Distributed locks and barriers

---

## Configuration

### Via Config File

```toml
[reasoning]
profile = "owl"
max_steps = 100000
think_passes = 3
engine = "dl-tableau"
enable_tabling = true
enable_stratification = true
enable_proof = false
```

### Via Command Line

```bash
cwm data.n3 \
  --profile owl \
  --max-steps 100000 \
  --think-passes 3 \
  --engine dl-tableau \
  --think
```

### Via Environment

```bash
export CWM_PROFILE=owl
export CWM_MAX_STEPS=100000
export CWM_ENGINE=dl-tableau
cwm data.n3 --think
```

---

## Examples

### RDFS Subclass Inference

```n3
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/> .

ex:Dog rdfs:subClassOf ex:Mammal .
ex:Mammal rdfs:subClassOf ex:Animal .
ex:fido a ex:Dog .
```

```bash
$ cwm animals.n3 --profile rdfs --think --filter
ex:Dog rdfs:subClassOf ex:Animal .
ex:fido a ex:Mammal .
ex:fido a ex:Animal .
```

### OWL Property Chain

```n3
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://example.org/> .

ex:hasUncle owl:propertyChainAxiom (ex:hasParent ex:hasBrother) .
ex:alice ex:hasParent ex:bob .
ex:bob ex:hasBrother ex:charlie .
```

```bash
$ cwm family.n3 --profile owl --think --filter
ex:alice ex:hasUncle ex:charlie .
```

### Theorem Proving

```n3
@prefix : <http://example.org/> .

# Group axioms
{ ?x :op (:e ?x) } => { ?x :eq ?x } .  # identity
{ ?x :op (?y ?z) . ?a :op (?x ?y) }
    => { ?a :op (:assoc ?x ?y ?z) } .  # associativity
```

```bash
$ cwm group.n3 theorem.n3 --engine otter --think --why
```

### Proof Generation

```bash
$ cwm axioms.n3 --think --why
# Outputs proof trace showing inference steps
```

---

## Performance Tuning

### For Speed

```bash
cwm data.n3 --profile performance --think
```

Or configure:

```toml
[reasoning]
profile = "performance"
max_steps = 1000
think_passes = 1
enable_tabling = false
```

### For Completeness

```bash
cwm data.n3 --profile complete --think
```

Or configure:

```toml
[reasoning]
profile = "complete"
max_steps = 0  # unlimited
think_passes = 5
enable_tabling = true
enable_stratification = true
```

### Memory Management

For large datasets:

```bash
cwm --pipe large-data.n3 --think  # streaming mode
```

---

## See Also

- [BUILTINS.md](BUILTINS.md) - Built-in predicates reference
- [SPARQL.md](SPARQL.md) - SPARQL query support
- [CONFIGURATION.md](CONFIGURATION.md) - Full configuration reference
