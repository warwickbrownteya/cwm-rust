# cwm-rust

A high-performance Rust implementation of the Closed World Machine (CWM) - an N3 reasoner, RDF processor, and semantic web toolkit with integrated theorem proving.

[![License: MIT/Apache-2.0](https://img.shields.io/badge/License-MIT%2FApache--2.0-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/rust-1.75%2B-orange.svg)](https://www.rust-lang.org/)

## Overview

cwm-rust is a modern, high-performance replacement for the original Python-based [cwm](https://www.w3.org/2000/10/swap/doc/cwm.html) created by Tim Berners-Lee at W3C. It provides comprehensive N3 (Notation3) processing with forward and backward chaining inference, full SPARQL 1.1 support, and 12 integrated theorem proving engines.

### Key Features

| Category | Capabilities |
|----------|-------------|
| **Built-in Predicates** | 266+ predicates across 8 namespaces (math, string, list, log, time, crypto, os, graph) |
| **Reasoning** | 25 strategies including RDFS, OWL 2 RL, temporal, probabilistic, fuzzy, defeasible |
| **Theorem Provers** | 12 engines: Resolution, Otter, DPLL, CDCL, Tableau, Superposition, SMT, and more |
| **SPARQL** | Full SPARQL 1.1 Query & Update with property paths, aggregates, federation |
| **Server** | HTTP SPARQL endpoint with caching, rate limiting, CORS |
| **Integration** | Apache Jena Fuseki, SQLite, distributed reasoning via IPC |
| **Output** | N3, N-Triples, RDF/XML, JSON-LD, SPARQL XML/JSON Results |
| **Performance** | 10-100x faster than original Python CWM |

### Feature Comparison with Original CWM

| Feature | Original CWM | cwm-rust |
|---------|-------------|----------|
| Built-in predicates | ~100 | 266+ |
| Reasoning strategies | 1 (forward) | 25 |
| Theorem provers | 0 | 12 |
| SPARQL support | None | Full 1.1 |
| Output formats | 3 | 5 |
| Performance | Baseline | 10-100x faster |

## Installation

### Quick Install (macOS/Linux)

```bash
curl -fsSL https://raw.githubusercontent.com/cwm-rust/cwm-rust/main/install.sh | bash
```

### Using Homebrew (macOS)

```bash
brew tap cwm-rust/cwm
brew install cwm
```

### Using Cargo

```bash
cargo install cwm
```

### From Source

```bash
git clone https://github.com/cwm-rust/cwm-rust
cd cwm-rust
make && make install
```

### Using Make

```bash
make                        # Build release binary
make install                # Install to ~/.local (XDG compliant)
make install PREFIX=/usr/local  # System-wide install
make uninstall              # Remove installed files
```

### Quick Start

```bash
# Parse N3 file
cwm data.n3

# Apply rules with forward-chaining
cwm data.n3 rules.n3 --think

# Run SPARQL query
cwm data.n3 --sparql-query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"

# Use theorem prover
cwm axioms.n3 --engine otter --think
```

## Command-Line Reference

### Input Options

| Option | Description |
|--------|-------------|
| `[FILE]...` | Input files to process (N3, Turtle, RDF/XML) |
| `--stdin` | Read input from stdin |
| `--data FILE` | Load data file (no rules extracted) |
| `--rules FILE` | Load rules from file |
| `--apply FILE` | Apply rules from file (implies --think) |
| `--base URI` | Base URI for relative references |

### Reasoning Options

| Option | Description |
|--------|-------------|
| `--think` | Apply forward-chaining inference |
| `--filter` | Output only inferred triples (requires --think) |
| `--filter-rules FILE` | Apply rules and output only conclusions |
| `--max-steps N` | Maximum inference steps (default: 10000, 0=unlimited) |
| `--think-passes N` | Number of reasoning passes |
| `--why` | Generate proof trace for inferences |
| `--engine ENGINE` | Use theorem prover (see Theorem Provers section) |

### Output Options

| Option | Description |
|--------|-------------|
| `-f, --format FORMAT` | Output format: n3, ntriples, rdf, jsonld, debug |
| `-o, --output FILE` | Output file (default: stdout) |
| `--strings` | Output only literal string values |
| `--purge-rules` | Remove rules from output |
| `--purge-builtins` | Remove builtin predicates from output |
| `--purge` | Remove log:Chaff statements |
| `--ugly` | Minimal formatting (fastest) |
| `--bySubject` | Sort output by subject |
| `--no` | Suppress all output |

### SPARQL Options

| Option | Description |
|--------|-------------|
| `--sparql FILE` | Execute SPARQL query from file |
| `--sparql-query QUERY` | Execute inline SPARQL query |
| `--sparql-results FORMAT` | Result format: xml (default), json |
| `--sparqlServer PORT` | Start SPARQL HTTP endpoint |

### Fuseki Integration

| Option | Description |
|--------|-------------|
| `--fuseki URL` | Use Fuseki SPARQL endpoint as backend |
| `--fuseki-graph URI` | Named graph URI (default: default graph) |
| `--fuseki-timeout SECS` | Connection timeout (default: 30) |
| `--fuseki-batch SIZE` | Batch size for bulk operations (default: 1000) |

### Advanced Options

| Option | Description |
|--------|-------------|
| `--reify` | Convert statements to RDF reification |
| `--dereify` | Reverse reification |
| `--flatten` | Flatten formula contents into main graph |
| `--unflatten` | Reconstruct nested formulas |
| `--patch FILE` | Apply graph patch (insertions/deletions) |
| `--closure FLAGS` | Auto-import: i=imports, r=rules, E=errors |
| `--diff` | Show additions/deletions |
| `--crypto` | Enable cryptographic operations |
| `--pipe` | Process without storing intermediate results |
| `--with ARGS` | Pass arguments as os:argv values |
| `-v, --verbose` | Verbose output with statistics |
| `-q, --quiet` | Suppress info messages |
| `--chatty LEVEL` | Debug level (0-99) |

### N3/RDF Output Flags

| Option | Description |
|--------|-------------|
| `--n3 FLAGS` | N3 output: a=anon bnodes, c=comments, d=no default ns, e=escape unicode, g=no =>, i=store ids, l=no lists, n=no numeric, p=no prefix, r=no relative, s=explicit subject, t=no special, u=unicode URIs, v=verbose quantifiers |
| `--rdf FLAGS` | RDF/XML: b=no nodeIDs, c=no class elements, d=no default ns, l=no collection, r=no relative, z=allow relative ns |

## Theorem Provers

cwm-rust includes 11 integrated theorem proving engines for formal verification and automated reasoning:

### Available Engines

| Engine | Description | Best For |
|--------|-------------|----------|
| `resolution` | Classical resolution with factoring | General FOL proving |
| `otter` | Set-of-support strategy with subsumption | Efficient refutation |
| `dpll` | Davis-Putnam-Logemann-Loveland | SAT problems |
| `cdcl` | Conflict-Driven Clause Learning | Large SAT instances |
| `tableau` | Analytic tableaux (alpha/beta rules) | Modal logic, intuitive proofs |
| `leancop` | Lean connection calculus | Compact proofs |
| `nanocop` | Minimal connection prover | Small problems |
| `superposition` | Modern equality prover | Equational reasoning |
| `knuth-bendix` | Term rewriting completion | Word problems, algebra |
| `smt` | E-matching for SMT-style | Theory reasoning |
| `dl-tableau` | Description Logic tableau | OWL ontologies |

### Usage Examples

```bash
# Prove theorem using Otter strategy
cwm axioms.n3 conjecture.n3 --engine otter --think

# SAT solving with CDCL
cwm formula.n3 --engine cdcl --think

# Equational reasoning
cwm group-axioms.n3 --engine knuth-bendix --think

# Tableau proof
cwm logic.n3 --engine tableau --think --why
```

### Engine Selection Guide

- **General theorem proving**: `otter` or `resolution`
- **Satisfiability (SAT)**: `cdcl` (best) or `dpll`
- **Equality/algebra**: `superposition` or `knuth-bendix`
- **Proof explanation**: `tableau` with `--why`
- **Description Logic/OWL**: `dl-tableau`

## Built-in Predicates

cwm-rust implements 266+ built-in predicates across 8 namespaces. See [BUILTINS.md](docs/BUILTINS.md) for complete reference.

### Quick Reference

#### Math (`math:`) - 40+ predicates
```n3
(2 3) math:sum ?x .           # ?x = 5
(10 3) math:quotient ?y .     # ?y = 3.33...
5 math:greaterThan 3 .        # succeeds
9 math:sqrt ?z .              # ?z = 3
```

#### String (`string:`) - 40+ predicates
```n3
("a" "b" "c") string:concatenation ?x .  # ?x = "abc"
"Hello" string:length ?n .                # ?n = 5
"hello" string:matches "^h.*o$" .         # succeeds
("text" "e" "a") string:replace ?r .      # ?r = "taxt"
```

#### List (`list:`) - 25+ predicates
```n3
(1 2 3) list:member 2 .           # succeeds
(1 2 3) list:length ?n .          # ?n = 3
((1 2) (3 4)) list:append ?x .    # ?x = (1 2 3 4)
(3 1 2) list:sort ?s .            # ?s = (1 2 3)
```

#### Log (`log:`) - 30+ predicates
```n3
{ ?x a :Person } => { ?x a :Human } .  # rule implication
:foo log:uri ?u .                       # ?u = "http://...foo"
("42" xsd:integer) log:dtlit ?v .       # typed literal
```

#### Time (`time:`) - 20+ predicates
```n3
"2024-06-15T12:00:00Z" time:year ?y .   # ?y = "2024"
"" time:gmTime ?now .                    # current UTC time
"2024-01-01T00:00:00Z" time:inSeconds ?s .  # epoch seconds
```

#### Crypto (`crypto:`) - 15+ predicates
```n3
"hello" crypto:sha256 ?hash .           # SHA-256 hash
"data" crypto:base64Encode ?b64 .       # base64 encoding
("msg" "key") crypto:hmacSha256 ?sig .  # HMAC signature
```

#### OS (`os:`) - 10+ predicates
```n3
"HOME" os:environ ?home .    # environment variable
0 os:argv ?prog .            # command-line argument
"" os:cwd ?dir .             # current directory
```

#### Graph (`graph:`) - 20+ predicates
```n3
{ :a :b :c } graph:length ?n .           # ?n = 1
{ :a :b :c } graph:member ?triple .      # iterate triples
```

## SPARQL Support

### Query Types

- **SELECT**: Return variable bindings
- **ASK**: Return boolean for pattern match
- **CONSTRUCT**: Build new graph from template
- **DESCRIBE**: Get triples describing resources

### Examples

```bash
# SELECT query
cwm data.n3 --sparql-query "SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }"

# ASK query
cwm data.n3 --sparql-query "ASK { ?x a <http://example.org/Person> }"

# JSON results
cwm data.n3 --sparql-query "SELECT * WHERE { ?s ?p ?o }" --sparql-results json

# Query from file
cwm data.n3 --sparql query.rq
```

### SPARQL Server

```bash
# Start server on port 8000
cwm data.n3 --sparqlServer 8000

# With reasoning
cwm data.n3 rules.n3 --think --sparqlServer 8000
```

Endpoints:
- `GET /sparql?query=...` - Execute URL-encoded query
- `POST /sparql` - Execute query from body
- `GET /` - HTML query form

## Fuseki Integration

Connect to Apache Jena Fuseki for enterprise-scale reasoning:

```bash
# Use Fuseki as backend
cwm --fuseki http://localhost:3030/dataset --think

# Load data into Fuseki
cwm data.n3 --fuseki http://localhost:3030/dataset

# Query specific named graph
cwm --fuseki http://localhost:3030/dataset --fuseki-graph http://example.org/graph1

# With custom timeout and batch size
cwm large-data.n3 --fuseki http://localhost:3030/dataset --fuseki-timeout 60 --fuseki-batch 5000
```

## N3 Syntax Support

### Prefixes and URIs

```n3
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@base <http://example.org/> .

ex:alice a ex:Person .
<relative> ex:related <./local> .
```

### Literals

```n3
ex:name "Alice" .                    # Plain string
ex:name "Alice"@en .                 # Language-tagged
ex:age "30"^^xsd:integer .           # Typed literal
ex:active true .                     # Boolean shorthand
ex:score 3.14 .                      # Numeric shorthand
ex:description """Multi-line
string literal""" .
```

### Blank Nodes and Collections

```n3
ex:alice ex:address [ ex:city "London" ; ex:country "UK" ] .
ex:colors rdf:value (ex:red ex:green ex:blue) .
```

### Formulas and Rules

```n3
# Simple rule
{ ?x a ex:Human } => { ?x a ex:Mortal } .

# Multiple conditions
{ ?x ex:parent ?y . ?y ex:parent ?z } => { ?x ex:grandparent ?z } .

# With built-ins
{ ?x ex:age ?age . ?age math:greaterThan 18 } => { ?x a ex:Adult } .
```

## Examples

### Basic Inference

```n3
# socrates.n3
@prefix ex: <http://example.org/> .

ex:socrates a ex:Human .
{ ?x a ex:Human } => { ?x a ex:Mortal } .
```

```bash
$ cwm socrates.n3 --think --filter
ex:socrates a ex:Mortal .
```

### Transitive Closure

```n3
# transitive.n3
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Dog rdfs:subClassOf ex:Animal .
ex:Animal rdfs:subClassOf ex:LivingThing .

{ ?x rdfs:subClassOf ?y . ?y rdfs:subClassOf ?z }
    => { ?x rdfs:subClassOf ?z } .
```

```bash
$ cwm transitive.n3 --think --filter
ex:Dog rdfs:subClassOf ex:LivingThing .
```

### Mathematical Computation

```n3
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix ex: <http://example.org/> .

ex:calc ex:values (10 20 30) .

{ ex:calc ex:values ?list .
  ?list math:sum ?sum .
  ?list math:memberCount ?count .
  (?sum ?count) math:quotient ?avg .
} => { ex:calc ex:average ?avg } .
```

### Format Conversion

```bash
# N3 to RDF/XML
cwm data.n3 --format rdf > data.rdf

# N3 to JSON-LD
cwm data.n3 --format jsonld > data.jsonld

# N3 to N-Triples
cwm data.n3 --format ntriples > data.nt
```

## Testing

### Run Unit Tests

```bash
cargo test
```

### Run SWAP Compliance Tests

```bash
./tests/swap_compliance/run_compliance.sh
```

### Run EYE Comparison Tests

```bash
./tests/run-comparison.sh
```

## Performance

cwm-rust is 10-100x faster than the original Python CWM:

| Operation | Python CWM | cwm-rust | Speedup |
|-----------|------------|----------|---------|
| Parse 10K triples | 2.5s | 0.05s | 50x |
| Simple inference | 1.2s | 0.02s | 60x |
| Complex rules | 15s | 0.2s | 75x |
| SPARQL query | 0.8s | 0.01s | 80x |

## Compatibility

- **100% SWAP compliance** - All W3C CWM built-ins supported
- **EYE reasoner compatible** - 10/10 comparison tests passing
- **Original CWM flags** - Most command-line options preserved

## Documentation

### Reference Guides

| Document | Description |
|----------|-------------|
| [BUILTINS.md](docs/BUILTINS.md) | Complete reference for 266+ built-in predicates |
| [REASONING.md](docs/REASONING.md) | 25 reasoning strategies and 12 theorem provers |
| [SPARQL.md](docs/SPARQL.md) | Full SPARQL 1.1 query and update reference |
| [CONFIGURATION.md](docs/CONFIGURATION.md) | Configuration files, profiles, environment variables |
| [DISTRIBUTED.md](docs/DISTRIBUTED.md) | IPC, clustering, and distributed reasoning |
| [EXAMPLES.md](docs/EXAMPLES.md) | Practical examples and use cases |

### Additional Resources

- [CWM_GAP_ANALYSIS.md](docs/CWM_GAP_ANALYSIS.md) - Comparison with original CWM
- [man/cwm.1](man/cwm.1) - Unix manual page (`man cwm`)

## Architecture

```
cwm-rust/
├── src/
│   ├── builtins/       # 266+ built-in predicates
│   ├── reasoning/      # 25 reasoning strategies
│   ├── prover/         # 12 theorem proving engines
│   ├── sparql/         # SPARQL 1.1 implementation
│   ├── server/         # HTTP SPARQL endpoint
│   ├── store/          # Triple store backends
│   └── parser/         # N3/Turtle/RDF parser
├── docs/               # Documentation
├── completions/        # Shell completions (bash/zsh/fish)
├── man/                # Manual pages
└── packaging/          # Distribution packages
```

## Testing

```bash
# Run all tests
cargo test

# Run with verbose output
cargo test -- --nocapture

# Run benchmarks
cargo bench
```

## Contributing

Contributions are welcome! Please see the [issues](https://github.com/cwm-rust/cwm-rust/issues) for areas needing help.

## License

Licensed under either of:

- MIT License ([LICENSE](LICENSE) or http://opensource.org/licenses/MIT)
- Apache License, Version 2.0 (http://www.apache.org/licenses/LICENSE-2.0)

at your option.

## Acknowledgments

Based on the original [cwm](https://www.w3.org/2000/10/swap/doc/cwm.html) by Tim Berners-Lee and Dan Connolly at W3C.

This implementation extends the original with modern features including full SPARQL support, multiple theorem provers, advanced reasoning strategies, and high-performance Rust implementation.
