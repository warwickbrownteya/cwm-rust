# Changelog

All notable changes to cwm-rust will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-12-29

### Initial Release

First public release of cwm-rust, a high-performance Rust implementation of the
Closed World Machine (CWM) N3 reasoner with integrated theorem proving.

### Added

#### Core Features
- **266+ built-in predicates** across 8 namespaces:
  - `math:` - 40+ arithmetic, trigonometric, bitwise operations
  - `string:` - 40+ string manipulation, regex, encoding
  - `list:` - 25+ list operations, set operations
  - `log:` - 30+ logical operations, rule handling
  - `time:` - 20+ temporal operations
  - `crypto:` - 15+ cryptographic functions
  - `os:` - 10+ system operations
  - `graph:` - 20+ RDF graph operations

#### Reasoning Engines
- **25 reasoning strategies**:
  - Forward and backward chaining
  - RDFS entailment rules
  - OWL 2 RL profile support
  - Temporal reasoning (Allen's interval algebra)
  - Probabilistic inference (Bayesian networks)
  - Fuzzy logic (T-norms, membership functions)
  - Defeasible reasoning (argumentation)
  - Hypothetical reasoning (truth maintenance)
  - Constraint logic programming
  - Machine learning integration (embeddings)
  - Natural language interface
  - Knowledge graph completion

- **12 theorem proving engines**:
  - Resolution (classical)
  - Otter (set-of-support)
  - Prover9 (LPO ordering)
  - Superposition (paramodulation)
  - LeanCoP (connection calculus)
  - NanoCoP (non-clausal)
  - Tableau (analytic)
  - DL-Tableau (description logic)
  - DPLL (SAT solving)
  - CDCL (conflict-driven clause learning)
  - SMT (DPLL(T) with theories)
  - Knuth-Bendix (term rewriting)

#### SPARQL Support
- Full SPARQL 1.1 Query implementation
- All query forms: SELECT, CONSTRUCT, ASK, DESCRIBE
- Property paths (*, +, ?, ^, /, |)
- Aggregates (COUNT, SUM, AVG, MIN, MAX, GROUP_CONCAT, SAMPLE)
- Subqueries and federated queries (SERVICE)
- SPARQL Update (INSERT, DELETE, LOAD, CLEAR)
- XML and JSON result formats
- Query optimization and caching

#### Server & Integration
- HTTP SPARQL endpoint server (async with Tokio/Axum)
- CORS support
- Rate limiting
- Query result caching
- Apache Jena Fuseki backend integration
- SQLite persistent storage backend

#### Distributed Reasoning
- Inter-process communication (TCP, UDP, Unix sockets)
- Raft-based cluster coordination
- Distributed locks and barriers
- Graph partitioning for parallel inference

#### Output Formats
- N3 (Notation3)
- N-Triples
- RDF/XML
- JSON-LD
- Debug format

#### Tooling & Packaging
- XDG-compliant installation (Makefile)
- Universal install script (install.sh)
- Shell completions (bash, zsh, fish)
- Man page documentation
- Homebrew formula (macOS)
- Debian package configuration
- RPM spec (Fedora/RHEL)
- Windows installer (WiX)
- GitHub Actions CI/CD workflow

#### Documentation
- Comprehensive README
- Built-in predicates reference (BUILTINS.md)
- Reasoning strategies guide (REASONING.md)
- SPARQL reference (SPARQL.md)
- Configuration guide (CONFIGURATION.md)
- Distributed reasoning guide (DISTRIBUTED.md)
- Examples and use cases (EXAMPLES.md)
- Tool comparison analysis (COMPARISON.md)

### Performance
- 10-100x faster than original Python CWM
- Optimized release profile with LTO
- Efficient memory usage
- Connection pooling for HTTP
- Query result caching

### Compatibility
- 100% SWAP built-in predicate coverage
- Original CWM command-line flag compatibility
- EYE reasoner test compatibility

### Technical
- 66,000+ lines of Rust code
- 78 source files
- 439 passing tests
- Rust 1.75+ required
- MIT/Apache-2.0 dual license

## [Unreleased]

### Planned
- WebAssembly (WASM) compilation target
- GraphQL interface
- More theorem prover integrations
- Enhanced ML/embedding support
- Cloud-native deployment options
