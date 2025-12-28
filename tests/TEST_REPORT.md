# CWM-Rust Comprehensive Test Report

**Date**: 2025-12-28
**CWM-Rust**: v0.1.0
**Total Tests**: 79 unit tests + 22 integration tests

## Summary

### Unit Tests (Rust)
All 79 unit tests pass:
```
test result: ok. 79 passed; 0 failed; 0 ignored
```

### Integration Tests (N3 Reasoning)

| Test | Description | Status |
|------|-------------|--------|
| 01 | Basic triples | PASS |
| 02 | Simple rule (Human → Mortal) | PASS |
| 03 | Transitive closure (subClassOf) | PASS |
| 04 | Family relations (parent, Father, Mother) | PASS |
| 05 | Multi-pattern rule (knows → knowsIndirectly) | PASS |
| 06 | Type propagation (instance → superclass) | PASS |
| 07 | Blank nodes | PASS |
| 08 | Collections/lists | PASS |
| 09 | Symmetric relations | PASS |
| 10 | Chain rule (next → skip) | PASS |
| 11 | Math builtins (sum, product, etc.) | PASS |
| 12 | String builtins (concat, length, etc.) | PASS |
| 13 | List builtins (member, first, rest, etc.) | PASS |
| 14 | Log builtins (uri, rawUri, equalTo, etc.) | PASS |
| 15 | Negation as failure (NAF) | PASS |
| 16 | Crypto builtins (sha256, md5, etc.) | PASS |
| 17 | Comparison builtins (greaterThan, lessThan, etc.) | PASS |

**Result: 17/17 N3 tests passing**

### SPARQL Tests

| Test | Description | Status |
|------|-------------|--------|
| SELECT basic | Basic triple patterns | PASS |
| SELECT FILTER | Numeric filtering | PASS |
| ASK | Boolean existence check | PASS |
| CONSTRUCT | Graph construction | PASS |
| OPTIONAL | Left outer join | PASS |

**Result: 5/5 SPARQL tests passing**

## Feature Coverage

### Built-in Predicates (266 total)

#### Math Namespace (math:)
- sum, product, difference, quotient, remainder
- absoluteValue, negation, floor, ceiling
- greaterThan, lessThan, equalTo, notEqualTo
- sin, cos, tan, asin, acos, atan
- log, exp, power, sqrt

#### String Namespace (string:)
- concatenation, length, contains
- matches, replace
- upperCase, lowerCase
- startsWith, endsWith
- split, join, trim

#### List Namespace (list:)
- member, in, first, rest, last
- length, append, reverse
- sort, unique

#### Log Namespace (log:)
- uri, rawUri (bidirectional)
- equalTo, notEqualTo
- includes, notIncludes
- conjunction, conclusion
- content, semantics

#### Crypto Namespace (crypto:)
- sha256, sha512, sha1, md5
- hmac

#### Time Namespace (time:)
- gmTime, localTime
- year, month, day, hour, minute, second

### CLI Features

| Feature | Flag | Status |
|---------|------|--------|
| Forward chaining | --think | PASS |
| Filter inferred | --filter | PASS |
| Delta/diff output | --diff | PASS |
| Proof generation | --why | PASS |
| Purge rules | --purge-rules | PASS |
| Purge builtins | --purge-builtins | PASS |
| Reification | --reify | PASS |
| Dereification | --dereify | PASS |
| N-Triples output | --format ntriples | PASS |
| JSON-LD output | --format jsonld | PASS |
| RDF/XML output | --format rdf | PASS |
| SPARQL query | --sparql | PASS |
| SPARQL server | --sparqlServer | PASS |
| Closure/imports | --closure | PASS |
| Patch files | --patch | PASS |

## Example Outputs

### Delta/Diff Output
```
$ cwm test.n3 --think --diff
# Graph diff output
# 2 addition(s), 0 deletion(s)

@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix : <http://example.org/> .

# Additions
+ :socrates a :Mortal .
+ :plato a :Mortal .

# Summary: +2 -0
```

### SPARQL ASK Result
```xml
<?xml version="1.0"?>
<sparql xmlns="http://www.w3.org/2005/sparql-results#">
  <head/>
  <boolean>true</boolean>
</sparql>
```

## Compatibility

### EYE Reasoner Comparison
CWM-Rust produces identical inferences to EYE for all tested rule patterns:
- Simple rules with single pattern
- Multi-pattern rules
- Transitive closure rules
- Type propagation rules
- Symmetric relation rules
- Chain rules

### Original CWM Compatibility
CWM-Rust implements most features from the original Python CWM:
- N3 parsing and serialization
- Forward-chaining inference
- Backward-chaining inference
- 266+ built-in predicates (vs ~101 in original)
- SPARQL 1.1 query support
- Proof generation with W3C reason vocabulary
- Delta/diff output
- OWL imports with transitive loading
- Graph patch support

## Known Limitations

1. **Empty prefix in SPARQL**: The `:localName` syntax requires using `PREFIX :` declaration. Use full URIs or named prefixes like `ex:` as alternatives.

2. **SPARQL SERVICE**: Federated queries are implemented but require network access.

3. **Backward chaining**: Limited to 10,000 results by default to prevent infinite loops.

## Running Tests

```bash
# Run all unit tests
cargo test

# Run N3 integration tests
./tests/run-tests.sh

# Run individual N3 test
cargo run -- tests/n3/02-simple-rule.n3 --think

# Run SPARQL test
cargo run -- tests/sparql/data.n3 --sparql tests/sparql/select-basic.rq

# Test diff output
cargo run -- tests/n3/02-simple-rule.n3 --think --diff
```
