# cwm-rust

A Rust implementation of the Closed World Machine (cwm) N3 reasoner and RDF processor.

## Overview

cwm-rust is a modern replacement for the original Python 2 based [cwm](https://www.w3.org/2000/10/swap/doc/cwm.html) tool. It parses N3 (Notation3) syntax, stores RDF triples, and performs forward-chaining inference using N3 rules.

## Features

- **N3/Turtle Parsing**: Full support for N3 syntax including prefixes, blank nodes, collections, and typed literals
- **Formula Support**: Quoted graphs using `{ ... }` syntax
- **Rule-Based Inference**: Forward-chaining with `{ body } => { head }` rules
- **Built-in Predicates**: Math, string, logic, and list operations
- **Prefix-Compressed Output**: Clean, readable N3 output with prefix declarations
- **Filter Mode**: Output only inferred triples

## Installation

### From Source

```bash
git clone <repository-url>
cd cwm-rust
cargo build --release
```

The binary will be at `target/release/cwm`.

## Usage

### Basic Parsing

```bash
# Parse and output N3 file
cwm input.n3

# Read from stdin
echo '@prefix ex: <http://example.org/> . ex:a ex:b ex:c .' | cwm --stdin
```

### Forward-Chaining Inference

```bash
# Apply rules with --think
cwm data.n3 rules.n3 --think

# Show only inferred triples
cwm data.n3 rules.n3 --think --filter

# Verbose output with statistics
cwm input.n3 --think -v
```

### Output Formats

```bash
cwm input.n3 -f n3        # N3/Turtle (default)
cwm input.n3 -f ntriples  # N-Triples
cwm input.n3 -f debug     # Debug format
```

## CLI Options

| Option | Description |
|--------|-------------|
| `--stdin` | Read input from stdin |
| `--think` | Apply rules (forward-chaining inference) |
| `--filter` | Output only inferred triples (requires --think) |
| `--max-steps N` | Maximum inference steps (default: 10000) |
| `-f, --format` | Output format: n3, ntriples, debug |
| `-o, --output` | Output file (defaults to stdout) |
| `-v, --verbose` | Show reasoning statistics |
| `-q, --quiet` | Suppress info messages |

## N3 Syntax Support

### Prefixes and Base

```n3
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@base <http://example.org/> .
```

### Triples

```n3
ex:alice a ex:Person .
ex:alice ex:name "Alice" .
ex:alice ex:knows ex:bob, ex:charlie .
ex:alice ex:age 30 ;
         ex:email "alice@example.org" .
```

### Blank Nodes

```n3
ex:alice ex:address [ ex:city "London" ; ex:country "UK" ] .
ex:bob ex:address _:addr1 .
_:addr1 ex:city "Paris" .
```

### Collections (Lists)

```n3
ex:colors rdf:value (ex:red ex:green ex:blue) .
```

### Literals

```n3
ex:name "Alice" .                              # Plain literal
ex:name "Alice"@en .                           # Language-tagged
ex:age "30"^^xsd:integer .                     # Typed literal
ex:active "true"^^xsd:boolean .
```

### Formulas and Rules

```n3
# Rule: All humans are mortal
{ ?x a ex:Human } => { ?x a ex:Mortal } .

# Transitive closure
{ ?x rdfs:subClassOf ?y . ?y rdfs:subClassOf ?z }
    => { ?x rdfs:subClassOf ?z } .

# Type propagation
{ ?x a ?class . ?class rdfs:subClassOf ?super }
    => { ?x a ?super } .
```

## Built-in Predicates

### Math (`http://www.w3.org/2000/10/swap/math#`)

| Predicate | Description | Example |
|-----------|-------------|---------|
| `math:sum` | Addition | `(2 3) math:sum ?x` → `?x = 5` |
| `math:difference` | Subtraction | `(5 3) math:difference ?x` → `?x = 2` |
| `math:product` | Multiplication | `(2 3) math:product ?x` → `?x = 6` |
| `math:quotient` | Division | `(6 2) math:quotient ?x` → `?x = 3` |
| `math:lessThan` | Less than | `2 math:lessThan 5` → succeeds |
| `math:greaterThan` | Greater than | `5 math:greaterThan 2` → succeeds |
| `math:equalTo` | Numeric equality | `5 math:equalTo 5` → succeeds |

### String (`http://www.w3.org/2000/10/swap/string#`)

| Predicate | Description | Example |
|-----------|-------------|---------|
| `string:concatenation` | Concatenate strings | `("a" "b") string:concatenation ?x` → `?x = "ab"` |
| `string:contains` | Substring check | `"hello" string:contains "ell"` → succeeds |
| `string:length` | String length | `"hello" string:length ?x` → `?x = 5` |

### Logic (`http://www.w3.org/2000/10/swap/log#`)

| Predicate | Description |
|-----------|-------------|
| `log:implies` | Rule implication (body => head) |
| `log:equalTo` | Term equality |
| `log:notEqualTo` | Term inequality |

### List (`http://www.w3.org/2000/10/swap/list#`)

| Predicate | Description | Example |
|-----------|-------------|---------|
| `list:member` | List membership | `(1 2 3) list:member 2` → succeeds |
| `list:length` | List length | `(1 2 3) list:length ?x` → `?x = 3` |
| `list:first` | First element | `(1 2 3) list:first ?x` → `?x = 1` |
| `list:rest` | Tail of list | `(1 2 3) list:rest ?x` → `?x = (2 3)` |

## Examples

### Example 1: Socrates Syllogism

```n3
@prefix ex: <http://example.org/> .

ex:socrates a ex:Human .

{ ?x a ex:Human } => { ?x a ex:Mortal } .
```

```bash
$ cwm socrates.n3 --think --filter
@prefix ex: <http://example.org/> .

ex:socrates a ex:Mortal .
```

### Example 2: Transitive Relationships

```n3
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Dog rdfs:subClassOf ex:Animal .
ex:Animal rdfs:subClassOf ex:LivingThing .
ex:fido a ex:Dog .

{ ?x rdfs:subClassOf ?y . ?y rdfs:subClassOf ?z }
    => { ?x rdfs:subClassOf ?z } .

{ ?x a ?class . ?class rdfs:subClassOf ?super }
    => { ?x a ?super } .
```

```bash
$ cwm transitive.n3 --think --filter
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/> .

ex:Dog rdfs:subClassOf ex:LivingThing .
ex:fido
    a ex:Animal ;
    a ex:LivingThing .
```

### Example 3: Family Relationships

```n3
@prefix ex: <http://example.org/> .

ex:alice ex:parent ex:bob .
ex:bob ex:parent ex:charlie .

{ ?x ex:parent ?y . ?y ex:parent ?z } => { ?x ex:grandparent ?z } .
```

```bash
$ cwm family.n3 --think --filter
@prefix ex: <http://example.org/> .

ex:alice ex:grandparent ex:charlie .
```

## Output Format

cwm-rust produces clean, readable N3 output:

- **Prefix declarations** for all used namespaces
- **Prefixed URIs** instead of full IRIs (`ex:foo` not `<http://example.org/foo>`)
- **`a` shorthand** for `rdf:type`
- **Compact typed literals** (`"30"^^xsd:integer`)
- **Pretty-printed** with `;` for same subject, grouped by subject

## Development

### Running Tests

```bash
cargo test
```

### Building

```bash
cargo build          # Debug build
cargo build --release  # Release build (optimized)
```

## License

MIT

## Acknowledgments

Based on the original [cwm](https://www.w3.org/2000/10/swap/doc/cwm.html) by Tim Berners-Lee and Dan Connolly.
