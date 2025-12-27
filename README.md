# cwm-rust

A Rust implementation of the Closed World Machine (cwm) N3 reasoner and RDF processor.

## Overview

cwm-rust is a modern, high-performance replacement for the original Python 2 based [cwm](https://www.w3.org/2000/10/swap/doc/cwm.html) tool. It parses N3 (Notation3) syntax, stores RDF triples, and performs forward-chaining inference using N3 rules.

**Key highlights:**
- **147 built-in predicates** across 8 namespaces (math, string, list, log, time, crypto, os, graph)
- **100% feature parity** with original cwm
- **Compatible with EYE reasoner** (10/10 compatibility tests passing)
- **Multiple output formats**: N3, N-Triples, RDF/XML, JSON-LD
- **SPARQL query support** with XML and JSON result formats
- **SPARQL HTTP endpoint** server mode
- **N3QL query support** for pattern-based queries
- **Proof generation** with --why flag

## Features

- **N3/Turtle Parsing**: Full support for N3 syntax including prefixes, blank nodes, collections, and typed literals
- **Formula Support**: Quoted graphs using `{ ... }` syntax
- **Rule-Based Inference**: Forward-chaining with `{ body } => { head }` rules
- **147 Built-in Predicates**: Math, string, list, logic, time, crypto, OS, and graph operations
- **Multiple Output Formats**: N3/Turtle, N-Triples, RDF/XML, JSON-LD
- **SPARQL Queries**: SELECT, ASK, CONSTRUCT, DESCRIBE with XML/JSON results
- **Web Fetching**: Load remote N3 documents via HTTP/HTTPS
- **Filter Mode**: Output only inferred triples
- **Flexible Input**: Load data files, rule files, or both

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

# Multiple input files
cwm data1.n3 data2.n3 rules.n3
```

### Forward-Chaining Inference

```bash
# Apply rules with --think
cwm data.n3 rules.n3 --think

# Show only inferred triples
cwm data.n3 rules.n3 --think --filter

# Apply rules from separate file
cwm data.n3 --apply rules.n3

# Load data without extracting rules
cwm --data facts.n3 --rules inference.n3 --think

# Verbose output with statistics
cwm input.n3 --think -v
```

### Output Formats

```bash
cwm input.n3 -f n3        # N3/Turtle (default)
cwm input.n3 -f ntriples  # N-Triples
cwm input.n3 -f rdf       # RDF/XML
cwm input.n3 -f jsonld    # JSON-LD
cwm input.n3 -f debug     # Debug format
```

### Output Filtering

```bash
# Output only literal string values
cwm input.n3 --think --strings

# Remove rules from output
cwm input.n3 --think --purge-rules

# Remove builtin predicates from output
cwm input.n3 --think --purge-builtins
```

## CLI Options

| Option | Description |
|--------|-------------|
| `--stdin` | Read input from stdin |
| `--think` | Apply rules (forward-chaining inference) |
| `--filter` | Output only inferred triples (requires --think) |
| `--max-steps N` | Maximum inference steps (default: 10000, 0 for unlimited) |
| `-f, --format` | Output format: n3, ntriples, rdf, debug |
| `-o, --output` | Output file (defaults to stdout) |
| `-v, --verbose` | Show reasoning statistics |
| `-q, --quiet` | Suppress info messages |
| `--apply FILE` | Apply rules from file (implies --think) |
| `--rules FILE` | Load rules from file |
| `--data FILE` | Load data file (no rules extracted) |
| `--strings` | Output only literal string values |
| `--purge-rules` | Remove rules (log:implies) from output |
| `--purge-builtins` | Remove builtin predicates from output |
| `--flatten` | Flatten formula contents into main graph |
| `--think-passes N` | Number of reasoning passes |
| `--base URI` | Base URI for relative references |
| `--no` | Suppress all output |
| `--purge` | Remove statements with log:Chaff class |
| `--chatty LEVEL` | Debug level (0-99, higher = more verbose) |
| `--ugly` | Minimal formatting, fastest mode |
| `--bySubject` | Sort output by subject |
| `--pipe` | Process without storing intermediate results |
| `--reify` | Convert statements to RDF reification |
| `--dereify` | Reverse reification (reconstruct original triples) |
| `--with ARGS` | Pass remaining arguments as os:argv values |
| `--sparql FILE` | Execute SPARQL query from file |
| `--sparql-query QUERY` | Execute inline SPARQL query |
| `--sparql-results FORMAT` | SPARQL result format: xml (default), json |
| `--n3 FLAGS` | N3 output flags: a/d/e/i/l/n/p/r/s/t |
| `--rdf FLAGS` | RDF/XML flags for input/output control |
| `--why` | Generate proof trace for inferences |
| `--closure FLAGS` | Auto-import ontologies: i=imports, r=rules, E=errors |
| `--patch FILE` | Apply graph edits (insertions/deletions) |
| `--language LANG` | Specify input/output language |
| `--sparqlServer PORT` | Start SPARQL HTTP endpoint on port |
| `--query FILE` | Execute N3QL query from file |

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

## Built-in Predicates (147 total)

### Math (`http://www.w3.org/2000/10/swap/math#`) - 40 predicates

| Predicate | Description | Example |
|-----------|-------------|---------|
| `math:sum` | Addition | `(2 3) math:sum ?x` → `?x = 5` |
| `math:difference` | Subtraction | `(5 3) math:difference ?x` → `?x = 2` |
| `math:product` | Multiplication | `(2 3) math:product ?x` → `?x = 6` |
| `math:quotient` | Division | `(6 2) math:quotient ?x` → `?x = 3` |
| `math:integerQuotient` | Integer division | `(7 2) math:integerQuotient ?x` → `?x = 3` |
| `math:remainder` | Modulo | `(7 3) math:remainder ?x` → `?x = 1` |
| `math:exponentiation` | Power | `(2 3) math:exponentiation ?x` → `?x = 8` |
| `math:negation` | Negate | `5 math:negation ?x` → `?x = -5` |
| `math:absoluteValue` | Absolute value | `-5 math:absoluteValue ?x` → `?x = 5` |
| `math:rounded` | Round | `3.7 math:rounded ?x` → `?x = 4` |
| `math:floor` | Floor | `3.7 math:floor ?x` → `?x = 3` |
| `math:ceiling` | Ceiling | `3.2 math:ceiling ?x` → `?x = 4` |
| `math:sin` | Sine (radians) | `0 math:sin ?x` → `?x = 0` |
| `math:cos` | Cosine | `0 math:cos ?x` → `?x = 1` |
| `math:tan` | Tangent | `0 math:tan ?x` → `?x = 0` |
| `math:asin` | Arc sine | `0 math:asin ?x` → `?x = 0` |
| `math:acos` | Arc cosine | `1 math:acos ?x` → `?x = 0` |
| `math:atan` | Arc tangent | `0 math:atan ?x` → `?x = 0` |
| `math:atan2` | Arc tangent (y,x) | `(1 1) math:atan2 ?x` |
| `math:sinh`, `math:cosh`, `math:tanh` | Hyperbolic functions |
| `math:degrees` | Radians to degrees | `3.14159 math:degrees ?x` → `?x ≈ 180` |
| `math:radians` | Degrees to radians | `180 math:radians ?x` → `?x ≈ 3.14159` |
| `math:logarithm` | Natural log | `2.718 math:logarithm ?x` → `?x ≈ 1` |
| `math:sqrt` | Square root | `9 math:sqrt ?x` → `?x = 3` |
| `math:lessThan` | Less than | `2 math:lessThan 5` → succeeds |
| `math:greaterThan` | Greater than | `5 math:greaterThan 2` → succeeds |
| `math:equalTo` | Numeric equality | `5 math:equalTo 5` → succeeds |
| `math:notEqualTo` | Numeric inequality | `5 math:notEqualTo 3` → succeeds |
| `math:notLessThan` | Greater or equal | `5 math:notLessThan 5` → succeeds |
| `math:notGreaterThan` | Less or equal | `5 math:notGreaterThan 5` → succeeds |
| `math:memberCount` | Count items in list | `(1 2 3) math:memberCount ?x` → `?x = 3` |
| `math:sumOf` | Sum of list | `(1 2 3) math:sumOf ?x` → `?x = 6` |
| `math:productOf` | Product of list | `(2 3 4) math:productOf ?x` → `?x = 24` |
| `math:min` | Minimum of list | `(5 2 8) math:min ?x` → `?x = 2` |
| `math:max` | Maximum of list | `(5 2 8) math:max ?x` → `?x = 8` |
| `math:average` | Average of list | `(2 4 6) math:average ?x` → `?x = 4` |

### String (`http://www.w3.org/2000/10/swap/string#`) - 40 predicates

| Predicate | Description | Example |
|-----------|-------------|---------|
| `string:concatenation` | Concatenate list | `("a" "b" "c") string:concatenation ?x` → `?x = "abc"` |
| `string:concat` | Concatenate pair | `("a" "b") string:concat ?x` → `?x = "ab"` |
| `string:contains` | Substring check | `"hello" string:contains "ell"` → succeeds |
| `string:notContains` | No substring | `"hello" string:notContains "xyz"` → succeeds |
| `string:startsWith` | Prefix check | `"hello" string:startsWith "hel"` → succeeds |
| `string:endsWith` | Suffix check | `"hello" string:endsWith "llo"` → succeeds |
| `string:length` | String length | `"hello" string:length ?x` → `?x = 5` |
| `string:lowerCase` | To lowercase | `"HELLO" string:lowerCase ?x` → `?x = "hello"` |
| `string:upperCase` | To uppercase | `"hello" string:upperCase ?x` → `?x = "HELLO"` |
| `string:matches` | Regex match | `"hello" string:matches "h.*o"` → succeeds |
| `string:notMatches` | Regex no match | `"hello" string:notMatches "^x"` → succeeds |
| `string:replace` | Regex replace | `("hello" "l" "L") string:replace ?x` → `?x = "heLLo"` |
| `string:scrape` | Extract match | `("hello123" "([0-9]+)") string:scrape ?x` → `?x = "123"` |
| `string:format` | Format string | `("%s is %s" "sky" "blue") string:format ?x` → `?x = "sky is blue"` |
| `string:substring` | Extract substring | `("hello" 1 3) string:substring ?x` → `?x = "ell"` |
| `string:split` | Split string | `("a,b,c" ",") string:split ?x` → `?x = ("a" "b" "c")` |
| `string:equalIgnoringCase` | Case-insensitive equal | `"Hello" string:equalIgnoringCase "hello"` → succeeds |
| `string:containsIgnoringCase` | Case-insensitive contains | `"Hello" string:containsIgnoringCase "ELL"` → succeeds |
| `string:containsRoughly` | Whitespace-normalized contains |
| `string:lessThan`, `string:greaterThan` | Lexicographic comparison |
| `string:notLessThan`, `string:notGreaterThan` | Lexicographic comparison |
| `string:trim` | Trim whitespace | `"  hello  " string:trim ?x` → `?x = "hello"` |
| `string:trimLeft` | Trim leading whitespace | `"  hello" string:trimLeft ?x` → `?x = "hello"` |
| `string:trimRight` | Trim trailing whitespace | `"hello  " string:trimRight ?x` → `?x = "hello"` |
| `string:normalize` | Normalize whitespace | `"  a   b  " string:normalize ?x` → `?x = "a b"` |
| `string:reverse` | Reverse string | `"hello" string:reverse ?x` → `?x = "olleh"` |
| `string:indexOf` | Find substring index | `("hello" "l") string:indexOf ?x` → `?x = 2` |
| `string:lastIndexOf` | Find last index | `("hello" "l") string:lastIndexOf ?x` → `?x = 3` |
| `string:repeat` | Repeat string | `("ab" 3) string:repeat ?x` → `?x = "ababab"` |
| `string:padStart` | Pad start of string | `("42" 5 "0") string:padStart ?x` → `?x = "00042"` |
| `string:padEnd` | Pad end of string | `("42" 5) string:padEnd ?x` → `?x = "42   "` |
| `string:join` | Join list with separator | `(("a" "b" "c") "-") string:join ?x` → `?x = "a-b-c"` |
| `string:lines` | Split by newlines | `"a\nb" string:lines ?x` → `?x = ("a" "b")` |
| `string:words` | Split by whitespace | `"a b c" string:words ?x` → `?x = ("a" "b" "c")` |

### List (`http://www.w3.org/2000/10/swap/list#`) - 22 predicates

| Predicate | Description | Example |
|-----------|-------------|---------|
| `list:member` | List membership | `(1 2 3) list:member 2` → succeeds |
| `list:in` | Element in list | `2 list:in (1 2 3)` → succeeds |
| `list:length` | List length | `(1 2 3) list:length ?x` → `?x = 3` |
| `list:first` | First element | `(1 2 3) list:first ?x` → `?x = 1` |
| `list:last` | Last element | `(1 2 3) list:last ?x` → `?x = 3` |
| `list:rest` | Tail of list | `(1 2 3) list:rest ?x` → `?x = (2 3)` |
| `list:nth` | Element at index | `((1 2 3) 1) list:nth ?x` → `?x = 2` |
| `list:append` | Concatenate lists | `((1 2) (3 4)) list:append ?x` → `?x = (1 2 3 4)` |
| `list:remove` | Remove element | `((1 2 3) 2) list:remove ?x` → `?x = (1 3)` |
| `list:unique` | Remove duplicates | `(1 2 2 3) list:unique ?x` → `?x = (1 2 3)` |
| `list:sort` | Sort list | `(3 1 2) list:sort ?x` → `?x = (1 2 3)` |
| `list:reverse` | Reverse list | `(1 2 3) list:reverse ?x` → `?x = (3 2 1)` |
| `list:flat` | Flatten one level | `((1 2) (3 4)) list:flat ?x` → `?x = (1 2 3 4)` |
| `list:iterate` | Enumerate with index | `(a b) list:iterate ?x` → `?x = ((0 a) (1 b))` |
| `list:setDifference` | Set difference | `((1 2 3) (2)) list:setDifference ?x` → `?x = (1 3)` |
| `list:setIntersection` | Set intersection | `((1 2 3) (2 3 4)) list:setIntersection ?x` → `?x = (2 3)` |
| `list:setUnion` | Set union | `((1 2) (2 3)) list:setUnion ?x` → `?x = (1 2 3)` |
| `list:take` | Take first N elements | `((1 2 3 4) 2) list:take ?x` → `?x = (1 2)` |
| `list:drop` | Drop first N elements | `((1 2 3 4) 2) list:drop ?x` → `?x = (3 4)` |
| `list:slice` | Extract slice | `((1 2 3 4) 1 3) list:slice ?x` → `?x = (2 3)` |
| `list:zip` | Zip two lists | `((1 2) (a b)) list:zip ?x` → `?x = ((1 a) (2 b))` |

### Log (`http://www.w3.org/2000/10/swap/log#`) - 22 predicates

| Predicate | Description |
|-----------|-------------|
| `log:implies` | Rule implication (body => head) |
| `log:equalTo` | Term equality |
| `log:notEqualTo` | Term inequality |
| `log:includes` | Formula includes triples |
| `log:notIncludes` | Formula excludes triples |
| `log:uri` | URI to string |
| `log:rawUri` | Raw URI string |
| `log:rawType` | Get term type (URI, Literal, Formula, List, BlankNode) |
| `log:dtlit` | Create typed literal: `("42" xsd:integer) log:dtlit ?x` |
| `log:langlit` | Create language literal: `("hello" "en") log:langlit ?x` |
| `log:conjunction` | Combine formulas |
| `log:n3String` | Formula to N3 string |
| `log:content` | Get file/URL content |
| `log:semantics` | Load and parse document |
| `log:outputString` | Mark for string output |
| `log:bound` | Check if variable is bound |
| `log:notBound` | Check if variable is unbound |
| `log:racine` | Base URI without fragment |
| `log:parsedAsN3` | Parse string as N3 |
| `log:semanticsOrError` | Load document or return error |
| `log:conclusion` | Derive all conclusions |
| `log:collectAllIn` | Collect all matching bindings |

### Time (`http://www.w3.org/2000/10/swap/time#`) - 11 predicates

| Predicate | Description | Example |
|-----------|-------------|---------|
| `time:inSeconds` | DateTime to Unix timestamp | `"2024-01-01T00:00:00Z" time:inSeconds ?x` |
| `time:gmTime` | Timestamp to UTC string | `0 time:gmTime ?x` → `?x = "1970-01-01T00:00:00+00:00"` |
| `time:localTime` | Timestamp to local time | `0 time:localTime ?x` |
| `time:year` | Extract year | `"2024-01-15T12:00:00Z" time:year ?x` → `?x = 2024` |
| `time:month` | Extract month (1-12) | `"2024-01-15T12:00:00Z" time:month ?x` → `?x = 1` |
| `time:day` | Extract day (1-31) | `"2024-01-15T12:00:00Z" time:day ?x` → `?x = 15` |
| `time:hour` | Extract hour (0-23) | `"2024-01-15T12:00:00Z" time:hour ?x` → `?x = 12` |
| `time:minute` | Extract minute (0-59) | `"2024-01-15T12:30:00Z" time:minute ?x` → `?x = 30` |
| `time:second` | Extract second (0-59) | `"2024-01-15T12:00:45Z" time:second ?x` → `?x = 45` |
| `time:dayOfWeek` | Day of week (0=Sun) | `"2024-01-15T12:00:00Z" time:dayOfWeek ?x` → `?x = 1` |
| `time:timeZone` | Timezone offset (minutes) | `"2024-01-15T12:00:00+05:30" time:timeZone ?x` → `?x = 330` |

### Crypto (`http://www.w3.org/2000/10/swap/crypto#`) - 9 predicates

| Predicate | Description | Example |
|-----------|-------------|---------|
| `crypto:md5` | MD5 hash | `"hello" crypto:md5 ?x` → `?x = "5d41402abc4b2a76..."` |
| `crypto:sha` | SHA-256 hash | `"hello" crypto:sha ?x` → `?x = "2cf24dba5fb0a30e..."` |
| `crypto:sha1` | SHA-1 hash | `"hello" crypto:sha1 ?x` |
| `crypto:sha512` | SHA-512 hash | `"hello" crypto:sha512 ?x` |
| `crypto:sign` | HMAC-SHA256 signature | `("data" "key") crypto:sign ?x` |
| `crypto:verify` | Verify HMAC signature | `(("data" "key") "sig") crypto:verify ?x` → `?x = "1"` or `"0"` |
| `crypto:verifyBoolean` | Verify (success/fail) | `(("data" "key") "sig") crypto:verifyBoolean true` → succeeds/fails |
| `crypto:base64Encode` | Base64 encode | `"hello" crypto:base64Encode ?x` → `?x = "aGVsbG8="` |
| `crypto:base64Decode` | Base64 decode | `"aGVsbG8=" crypto:base64Decode ?x` → `?x = "hello"` |

### OS (`http://www.w3.org/2000/10/swap/os#`) - 4 predicates

| Predicate | Description | Example |
|-----------|-------------|---------|
| `os:environ` | Environment variable | `"HOME" os:environ ?x` → `?x = "/home/user"` |
| `os:argv` | Command line argument | `0 os:argv ?x` → `?x = "cwm"` |
| `os:baseAbsolute` | Resolve to absolute path | `"./file" os:baseAbsolute ?x` |
| `os:baseRelative` | Convert to relative path | `"/abs/path" os:baseRelative ?x` |

### Graph (`http://www.w3.org/2000/10/swap/graph#`) - 5 predicates

| Predicate | Description | Example |
|-----------|-------------|---------|
| `graph:difference` | Graph difference | `({:a :b :c} {:a :b :c}) graph:difference ?x` |
| `graph:union` | Graph union | `({:a :b :c} {:d :e :f}) graph:union ?x` |
| `graph:intersection` | Graph intersection | `({:a :b :c. :d :e :f} {:a :b :c}) graph:intersection ?x` |
| `graph:length` | Triple count | `{:a :b :c. :d :e :f} graph:length ?x` → `?x = 2` |
| `graph:member` | Get triples from graph | `{:a :b :c} graph:member ?x` |

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

### Example 3: Math Operations

```n3
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix ex: <http://example.org/> .

ex:calculation ex:input (10 3) .

{ ex:calculation ex:input (?a ?b) .
  (?a ?b) math:sum ?sum .
  (?a ?b) math:product ?prod .
} => {
  ex:calculation ex:sum ?sum ;
                 ex:product ?prod .
} .
```

### Example 4: String Processing

```n3
@prefix string: <http://www.w3.org/2000/10/swap/string#> .
@prefix ex: <http://example.org/> .

ex:text ex:value "Hello, World!" .

{ ex:text ex:value ?v .
  ?v string:lowerCase ?lower .
  (?v " " "-") string:replace ?slug .
} => {
  ex:text ex:lowercase ?lower ;
          ex:slug ?slug .
} .
```

### Example 5: RDF/XML Output

```bash
$ echo '@prefix ex: <http://example.org/> .
ex:alice a ex:Person ; ex:name "Alice" .' | cwm --stdin --format rdf
```

```xml
<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:ex="http://example.org/"
>
  <rdf:Description rdf:about="http://example.org/alice">
    <rdf:type rdf:resource="http://example.org/Person"/>
    <ex:name>Alice</ex:name>
  </rdf:Description>
</rdf:RDF>
```

### Example 6: JSON-LD Output

```bash
$ echo '@prefix ex: <http://example.org/> .
ex:alice a ex:Person ; ex:name "Alice" ; ex:age 30 .' | cwm --stdin --format jsonld
```

```json
{
  "@context": {
    "ex": "http://example.org/"
  },
  "@graph": [
    {
      "@id": "http://example.org/alice",
      "@type": "http://example.org/Person",
      "http://example.org/name": "Alice",
      "http://example.org/age": 30
    }
  ]
}
```

### Example 7: Reification

```bash
# Convert statements to RDF reification
$ cwm data.n3 --reify

# Reverse reification (reconstruct original statements)
$ cwm reified-data.n3 --dereify
```

## SPARQL Query Support

cwm-rust includes built-in SPARQL query support for querying RDF data after loading and reasoning.

### Query Types

- **SELECT**: Return variable bindings as a table
- **ASK**: Return boolean (true/false) for pattern match
- **CONSTRUCT**: Build a new graph from a template
- **DESCRIBE**: Get triples describing resources

### CLI Options

| Option | Description |
|--------|-------------|
| `--sparql FILE` | Execute SPARQL query from file |
| `--sparql-query QUERY` | Execute inline SPARQL query |
| `--sparql-results FORMAT` | Result format: `xml` (default), `json` |

### SPARQL Examples

#### SELECT Query

```bash
echo '@prefix ex: <http://example.org/> .
ex:alice ex:name "Alice" ; ex:age 30 .
ex:bob ex:name "Bob" ; ex:age 25 .' | \
cwm --stdin --sparql-query "SELECT ?name ?age WHERE { ?s <http://example.org/name> ?name . ?s <http://example.org/age> ?age }"
```

#### ASK Query

```bash
echo '@prefix ex: <http://example.org/> .
ex:alice a ex:Person .' | \
cwm --stdin --sparql-query "ASK { ?x a <http://example.org/Person> }"
```

#### JSON Output

```bash
cwm data.n3 --sparql-query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" --sparql-results json
```

### SPARQL Features Supported

- **PREFIX declarations**: `PREFIX ex: <http://example.org/>`
- **SELECT DISTINCT**: Remove duplicate results
- **ORDER BY**: Sort results (ASC/DESC)
- **LIMIT/OFFSET**: Pagination
- **OPTIONAL**: Left outer join patterns
- **FILTER**: Conditions with operators
  - Comparison: `<`, `>`, `<=`, `>=`, `=`, `!=`
  - Boolean: `&&`, `||`, `!`
  - Functions: `BOUND()`, `REGEX()`, `CONTAINS()`, `STRSTARTS()`, `STRENDS()`, `STR()`, `STRLEN()`, `LANG()`, `DATATYPE()`, `isIRI()`, `isBlank()`, `isLiteral()`

### Result Formats

**XML (application/sparql-results+xml)**:
```xml
<?xml version="1.0"?>
<sparql xmlns="http://www.w3.org/2005/sparql-results#">
  <head>
    <variable name="name"/>
  </head>
  <results>
    <result>
      <binding name="name">
        <literal>Alice</literal>
      </binding>
    </result>
  </results>
</sparql>
```

**JSON (application/sparql-results+json)**:
```json
{
  "head": { "vars": ["name"] },
  "results": {
    "bindings": [
      { "name": { "type": "literal", "value": "Alice" } }
    ]
  }
}
```

## Output Format

cwm-rust produces clean, readable N3 output:

- **Prefix declarations** for all used namespaces
- **Prefixed URIs** instead of full IRIs (`ex:foo` not `<http://example.org/foo>`)
- **`a` shorthand** for `rdf:type`
- **Compact typed literals** (`"30"^^xsd:integer`)
- **Pretty-printed** with `;` for same subject, grouped by subject

## SPARQL Server

cwm-rust can run as a SPARQL HTTP endpoint:

```bash
# Load data and start SPARQL server on port 8000
cwm data.n3 --sparqlServer 8000

# With reasoning applied first
cwm data.n3 rules.n3 --think --sparqlServer 8000
```

The server provides:
- **GET /sparql?query=...** - Execute URL-encoded query
- **POST /sparql** - Execute query from request body
- **GET /** - HTML form for testing queries
- **Accept header** - Returns XML or JSON based on preference
- **CORS support** - Cross-origin requests allowed

## N3QL Queries

N3QL is a pattern-based query language using N3 rules:

```bash
# Run N3QL query file against data
cwm data.n3 --query query.n3
```

Example query file:
```n3
@prefix : <http://example.org/> .

# Find all names
{ ?person :name ?name } => { ?person :hasName ?name } .
```

N3QL queries return triples derived by applying the query rules to the data.

## Proof Generation

The `--why` flag generates proof traces explaining how conclusions were derived:

```bash
cwm data.n3 rules.n3 --think --why
```

Output shows each derived triple with the rule that produced it.

## Patch Files

Apply graph edits with `--patch`:

```bash
cwm data.n3 --patch changes.patch
```

Patch file format:
```
@prefix ex: <http://example.org/> .

+ ex:alice ex:knows ex:bob    # Add triple
- ex:alice ex:knows ex:carol  # Remove triple
```

## Ontology Imports

The `--closure` flag loads imported ontologies:

```bash
cwm ontology.n3 --closure=i   # Load owl:imports
cwm ontology.n3 --closure=ir  # Load imports and rules
```

Flags:
- `i` - Follow owl:imports statements
- `r` - Also load rules from imported files
- `E` - Show errors on import failures

## Compatibility

cwm-rust is tested for compatibility with the [EYE reasoner](https://github.com/eyereasoner/eye):

- 10/10 compatibility tests passing
- Identical inference results for core reasoning patterns
- Minor syntactic differences (blank node IDs, numeric literal formatting)

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

### Running EYE Comparison Tests

```bash
bash tests/run-comparison.sh
```

## License

MIT

## Acknowledgments

Based on the original [cwm](https://www.w3.org/2000/10/swap/doc/cwm.html) by Tim Berners-Lee and Dan Connolly.
