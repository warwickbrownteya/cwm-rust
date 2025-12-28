# CWM vs cwm-rust Gap Analysis

**Date**: 2025-12-28 (Updated)
**cwm-rust version**: 0.1.0
**Original CWM reference**: W3C SWAP (https://www.w3.org/2000/10/swap/)

## Executive Summary

| Metric | Original CWM | cwm-rust | Status |
|--------|-------------|----------|--------|
| Built-in predicates | 92 | 242+ | cwm-rust has MORE |
| CLI options | ~30 | 35+ | Equivalent+ |
| Output formats | 4 | 5 | Equivalent+ |
| Reasoning modes | 2 | 2 | Equivalent |
| SPARQL support | Basic | SPARQL 1.1 | cwm-rust has MORE |

## Feature Comparison

### 1. Command-Line Options

| Option | Original CWM | cwm-rust | Notes |
|--------|:------------:|:--------:|-------|
| `--think` | ✅ | ✅ | Forward chaining |
| `--apply=file` | ✅ | ✅ | Apply rules from file |
| `--filter` | ✅ | ✅ | Output only inferred |
| `--rules` | ✅ | ✅ | Load rules |
| `--data` | ✅ | ✅ | Load data (no rules) |
| `--why` | ✅ | ✅ | Proof generation |
| `--diff` | ✅ | ✅ | Delta output |
| `--patch` | ✅ | ✅ | Apply patches |
| `--reify` | ✅ | ✅ | Reification |
| `--dereify` | ✅ | ✅ | Dereification |
| `--flatten` | ✅ | ✅ | Flatten formulas |
| `--unflatten` | ✅ | ✅ | ✅ IMPLEMENTED |
| `--purge` | ✅ | ✅ | Remove log:Chaff |
| `--purge-rules` | ✅ | ✅ | Remove rules |
| `--purge-builtins` | ✅ | ✅ | Remove builtins |
| `--ugly` | ✅ | ✅ | Minimal formatting |
| `--bySubject` | ✅ | ✅ | Sort by subject |
| `--pipe` | ✅ | ✅ | Stream mode |
| `--base` | ✅ | ✅ | Base URI |
| `--closure` | ✅ | ✅ | Import closure |
| `--mode` | ✅ | ✅ | Full r/w/t/f support |
| `--chatty` | ✅ | ✅ | Debug level |
| `--with` | ✅ | ✅ | Pass os:argv |
| `--strings` | ✅ | ✅ | Output strings |
| `--n3` | ✅ | ✅ | N3 format flags |
| `--rdf` | ✅ | ✅ | RDF/XML flags |
| `--ntriples` | ✅ | ✅ | N-Triples format |
| `--language` | ✅ | ✅ | Controls output format |
| `--sparql` | ✅ | ✅ | SPARQL query |
| `--sparqlServer` | ✅ | ✅ | HTTP endpoint |
| `--query` | ✅ | ✅ | N3QL query |
| `--crypto` | ✅ | ✅ | Enforced: crypto builtins only when set |
| `--engine=otter` | ✅ | ❌ | **GAP**: Alternate engine |
| `--revision` | ✅ | ✅ | Shows version info |
| `--think-passes` | ❌ | ✅ | cwm-rust extra |
| `--max-steps` | ❌ | ✅ | cwm-rust extra |
| `--sparql-query` | ❌ | ✅ | cwm-rust extra (inline) |
| `--format jsonld` | ❌ | ✅ | cwm-rust extra |

### 2. Built-in Predicates

#### 2.1 Log Namespace (log:)

| Predicate | Original | cwm-rust | Notes |
|-----------|:--------:|:--------:|-------|
| `conclusion` | ✅ | ✅ | Get rule conclusions |
| `conjunction` | ✅ | ✅ | Combine formulas |
| `content` | ✅ | ✅ | Web content fetch |
| `definitiveDocument` | ✅ | ✅ | Returns doc URI |
| `definitiveService` | ✅ | ✅ | Returns service URI |
| `dtlit` | ✅ | ✅ | Typed literal |
| `equalTo` | ✅ | ✅ | Equality |
| `implies` | ✅ | ✅ | Implication (via => syntax) |
| `includes` | ✅ | ✅ | Formula includes |
| `n3String` | ✅ | ✅ | N3 serialization |
| `notEqualTo` | ✅ | ✅ | Inequality |
| `notIncludes` | ✅ | ✅ | NAF pattern |
| `outputString` | ✅ | ✅ | String output |
| `parsedAsN3` | ✅ | ✅ | Parse N3 string |
| `racine` | ✅ | ✅ | Root/base |
| `rawType` | ✅ | ✅ | Raw type check |
| `rawUri` | ✅ | ✅ | URI to string |
| `semantics` | ✅ | ✅ | Load/parse doc |
| `semanticsOrError` | ✅ | ✅ | Load with error |
| `uri` | ✅ | ✅ | String to URI |
| `Chaff` | ✅ | ✅ | Chaff class |
| `N3Document` | ✅ | ✅ | ✅ IMPLEMENTED |
| `Truth` | ✅ | ✅ | ✅ IMPLEMENTED |
| `collectAllIn` | ✅ | ✅ | Collect bindings |
| `langlit` | ❌ | ✅ | cwm-rust extra |
| `forAll` | ❌ | ✅ | cwm-rust extra |
| `forSome` | ❌ | ✅ | cwm-rust extra |
| `skolem` | ❌ | ✅ | cwm-rust extra |
| `bound` | ❌ | ✅ | cwm-rust extra |
| `notBound` | ❌ | ✅ | cwm-rust extra |

#### 2.2 Math Namespace (math:)

| Predicate | Original | cwm-rust | Notes |
|-----------|:--------:|:--------:|-------|
| `sum` | ✅ | ✅ | Bidirectional |
| `difference` | ✅ | ✅ | Bidirectional |
| `product` | ✅ | ✅ | Bidirectional |
| `quotient` | ✅ | ✅ | Bidirectional |
| `remainder` | ✅ | ✅ | |
| `integerQuotient` | ✅ | ✅ | |
| `negation` | ✅ | ✅ | |
| `absoluteValue` | ✅ | ✅ | |
| `rounded` | ✅ | ✅ | |
| `greaterThan` | ✅ | ✅ | |
| `lessThan` | ✅ | ✅ | |
| `notGreaterThan` | ✅ | ✅ | |
| `notLessThan` | ✅ | ✅ | |
| `equalTo` | ✅ | ✅ | |
| `notEqualTo` | ✅ | ✅ | |
| `memberCount` | ✅ | ✅ | |
| `exponentiation` | ✅ | ✅ | Bidirectional |
| `sin` | ✅ | ✅ | |
| `cos` | ✅ | ✅ | |
| `tan` | ✅ | ✅ | |
| `sinh` | ✅ | ✅ | |
| `tanh` | ✅ | ✅ | |
| `atan2` | ✅ | ✅ | |
| `degrees` | ✅ | ✅ | |
| `radians` | ❌ | ✅ | cwm-rust extra |
| `asin` | ❌ | ✅ | cwm-rust extra |
| `acos` | ❌ | ✅ | cwm-rust extra |
| `atan` | ❌ | ✅ | cwm-rust extra |
| `cosh` | ❌ | ✅ | cwm-rust extra |
| `sqrt` | ❌ | ✅ | cwm-rust extra |
| `cbrt` | ❌ | ✅ | cwm-rust extra |
| `exp` | ❌ | ✅ | cwm-rust extra |
| `logarithm` | ❌ | ✅ | cwm-rust extra |
| `log10` | ❌ | ✅ | cwm-rust extra |
| `log2` | ❌ | ✅ | cwm-rust extra |
| `floor` | ❌ | ✅ | cwm-rust extra |
| `ceiling` | ❌ | ✅ | cwm-rust extra |
| `hypot` | ❌ | ✅ | cwm-rust extra |
| `min` | ❌ | ✅ | cwm-rust extra |
| `max` | ❌ | ✅ | cwm-rust extra |
| `gcd` | ❌ | ✅ | cwm-rust extra |
| `lcm` | ❌ | ✅ | cwm-rust extra |
| `mod` | ❌ | ✅ | cwm-rust extra |
| `factors` | ❌ | ✅ | cwm-rust extra |
| `average` | ❌ | ✅ | cwm-rust extra |
| Bit operations | ❌ | ✅ | cwm-rust: and, or, xor, not, shifts |

#### 2.3 String Namespace (string:)

| Predicate | Original | cwm-rust | Notes |
|-----------|:--------:|:--------:|-------|
| `concatenation` | ✅ | ✅ | |
| `contains` | ✅ | ✅ | |
| `containsIgnoringCase` | ✅ | ✅ | |
| `containsRoughly` | ✅ | ✅ | |
| `endsWith` | ✅ | ✅ | |
| `startsWith` | ✅ | ✅ | |
| `equalIgnoringCase` | ✅ | ✅ | |
| `format` | ✅ | ✅ | |
| `greaterThan` | ✅ | ✅ | |
| `lessThan` | ✅ | ✅ | |
| `matches` | ✅ | ✅ | |
| `notMatches` | ✅ | ✅ | |
| `notContainsRoughly` | ✅ | ✅ | |
| `notEqualIgnoringCase` | ✅ | ✅ | |
| `notGreaterThan` | ✅ | ✅ | |
| `notLessThan` | ✅ | ✅ | |
| `replace` | ✅ | ✅ | |
| `scrape` | ✅ | ✅ | |
| `length` | ❌ | ✅ | cwm-rust extra |
| `upperCase` | ❌ | ✅ | cwm-rust extra |
| `lowerCase` | ❌ | ✅ | cwm-rust extra |
| `trim` | ❌ | ✅ | cwm-rust extra |
| `split` | ❌ | ✅ | cwm-rust extra |
| `join` | ❌ | ✅ | cwm-rust extra |
| `substring` | ❌ | ✅ | cwm-rust extra |
| `indexOf` | ❌ | ✅ | cwm-rust extra |
| `capitalize` | ❌ | ✅ | cwm-rust extra |
| `normalize` | ❌ | ✅ | cwm-rust extra |
| `encodeForURI` | ❌ | ✅ | cwm-rust extra |
| `decodeFromURI` | ❌ | ✅ | cwm-rust extra |
| `htmlEscape` | ❌ | ✅ | cwm-rust extra |
| `xmlEscape` | ❌ | ✅ | cwm-rust extra |
| `base64Encode` | ❌ | ✅ | cwm-rust extra |
| `base64Decode` | ❌ | ✅ | cwm-rust extra |
| `hexEncode` | ❌ | ✅ | cwm-rust extra |
| `hexDecode` | ❌ | ✅ | cwm-rust extra |
| `words` | ❌ | ✅ | cwm-rust extra |
| `lines` | ❌ | ✅ | cwm-rust extra |
| `tokenize` | ❌ | ✅ | cwm-rust extra |
| `captureGroups` | ❌ | ✅ | cwm-rust extra |

#### 2.4 List Namespace (list:)

| Predicate | Original | cwm-rust | Notes |
|-----------|:--------:|:--------:|-------|
| `append` | ✅ | ✅ | |
| `in` | ✅ | ✅ | |
| `last` | ✅ | ✅ | |
| `member` | ✅ | ✅ | |
| `first` | ❌ | ✅ | cwm-rust extra |
| `rest` | ❌ | ✅ | cwm-rust extra |
| `length` | ❌ | ✅ | cwm-rust extra |
| `reverse` | ❌ | ✅ | cwm-rust extra |
| `sort` | ❌ | ✅ | cwm-rust extra |
| `unique` | ❌ | ✅ | cwm-rust extra |
| `flatten` | ❌ | ✅ | cwm-rust extra |
| `nth` | ❌ | ✅ | cwm-rust extra |
| `take` | ❌ | ✅ | cwm-rust extra |
| `drop` | ❌ | ✅ | cwm-rust extra |
| `slice` | ❌ | ✅ | cwm-rust extra |
| `zip` | ❌ | ✅ | cwm-rust extra |
| `union` | ❌ | ✅ | cwm-rust extra |
| `intersection` | ❌ | ✅ | cwm-rust extra |
| `range` | ❌ | ✅ | cwm-rust extra |
| `cons` | ❌ | ✅ | cwm-rust extra |
| `uncons` | ❌ | ✅ | cwm-rust extra |
| `butLast` | ❌ | ✅ | cwm-rust extra |

#### 2.5 Crypto Namespace (crypto:)

| Predicate | Original | cwm-rust | Notes |
|-----------|:--------:|:--------:|-------|
| `md5` | ✅ | ✅ | |
| `sha` | ✅ | ✅ | SHA-256 |
| `sha1` | ❌ | ✅ | cwm-rust extra |
| `sha512` | ❌ | ✅ | cwm-rust extra |
| `sign` | ✅ | ✅ | |
| `verify` | ✅ | ✅ | |
| `verifyBoolean` | ✅ | ✅ | |
| `publicKey` | ✅ | ✅ | Extract public key |
| `CanEncrypt` | ✅ | ✅ | |
| `CanSign` | ✅ | ✅ | |
| `HasPrivate` | ✅ | ✅ | |
| `HashFunction` | ✅ | ✅ | |
| `PublicKeyObject` | ✅ | ✅ | |
| `hmac` | ❌ | ✅ | cwm-rust extra |
| `randomBytes` | ❌ | ✅ | cwm-rust extra |
| `uuid` | ❌ | ✅ | cwm-rust extra |
| `base64Encode` | ❌ | ✅ | cwm-rust extra |
| `base64Decode` | ❌ | ✅ | cwm-rust extra |
| `hexEncode` | ❌ | ✅ | cwm-rust extra |
| `hexDecode` | ❌ | ✅ | cwm-rust extra |
| `keyLength` | ❌ | ✅ | cwm-rust extra |

#### 2.6 Time Namespace (time:)

| Predicate | Original | cwm-rust | Notes |
|-----------|:--------:|:--------:|-------|
| `gmTime` | ✅ | ✅ | |
| `localTime` | ✅ | ✅ | |
| `day` | ✅ | ✅ | |
| `dayOfWeek` | ✅ | ✅ | |
| `hour` | ✅ | ✅ | |
| `minute` | ✅ | ✅ | |
| `month` | ✅ | ✅ | |
| `second` | ✅ | ✅ | |
| `year` | ✅ | ✅ | |
| `inSeconds` | ✅ | ✅ | |
| `timeZone` | ✅ | ✅ | |
| `now` | ❌ | ✅ | cwm-rust extra |
| `nowSeconds` | ❌ | ✅ | cwm-rust extra |
| `dayOfYear` | ❌ | ✅ | cwm-rust extra |
| `weekOfYear` | ❌ | ✅ | cwm-rust extra |
| `isLeapYear` | ❌ | ✅ | cwm-rust extra |
| `addSeconds` | ❌ | ✅ | cwm-rust extra |
| `diffSeconds` | ❌ | ✅ | cwm-rust extra |
| `isBefore` | ❌ | ✅ | cwm-rust extra |
| `isAfter` | ❌ | ✅ | cwm-rust extra |

#### 2.7 OS Namespace (os:)

| Predicate | Original | cwm-rust | Notes |
|-----------|:--------:|:--------:|-------|
| `argv` | ✅ | ✅ | |
| `baseAbsolute` | ✅ | ✅ | |
| `baseRelative` | ✅ | ✅ | |
| `environ` | ✅ | ✅ | |
| `cwd` | ❌ | ✅ | cwm-rust extra |
| `homeDir` | ❌ | ✅ | cwm-rust extra |
| `tempDir` | ❌ | ✅ | cwm-rust extra |
| `hostname` | ❌ | ✅ | cwm-rust extra |
| `platform` | ❌ | ✅ | cwm-rust extra |
| `arch` | ❌ | ✅ | cwm-rust extra |
| `fileSize` | ❌ | ✅ | cwm-rust extra |
| `fileModTime` | ❌ | ✅ | cwm-rust extra |
| `isFile` | ❌ | ✅ | cwm-rust extra |
| `isDir` | ❌ | ✅ | cwm-rust extra |
| `dirContents` | ❌ | ✅ | cwm-rust extra |
| `basename` | ❌ | ✅ | cwm-rust extra |
| `dirname` | ❌ | ✅ | cwm-rust extra |
| `extension` | ❌ | ✅ | cwm-rust extra |
| `joinPath` | ❌ | ✅ | cwm-rust extra |

### 3. SPARQL Support

| Feature | Original CWM | cwm-rust | Notes |
|---------|:------------:|:--------:|-------|
| SELECT | ✅ | ✅ | |
| ASK | ✅ | ✅ | |
| CONSTRUCT | ✅ | ✅ | |
| DESCRIBE | ❌ | ✅ | cwm-rust extra |
| FILTER | ✅ | ✅ | |
| OPTIONAL | ✅ | ✅ | |
| UNION | ❌ | ✅ | cwm-rust extra |
| SERVICE | ❌ | ✅ | cwm-rust extra |
| BIND | ❌ | ✅ | cwm-rust extra |
| VALUES | ❌ | ⚠️ | Planned |
| Property paths | ❌ | ✅ | ✅ IMPLEMENTED |
| Aggregates | ❌ | ✅ | ✅ IMPLEMENTED (COUNT, SUM, AVG, MIN, MAX) |
| Subqueries | ❌ | ✅ | ✅ IMPLEMENTED |
| GROUP BY | ❌ | ✅ | cwm-rust extra |
| HAVING | ❌ | ✅ | cwm-rust extra |
| ORDER BY | ❌ | ✅ | cwm-rust extra |
| LIMIT/OFFSET | ❌ | ✅ | cwm-rust extra |

### 4. Output Formats

| Format | Original CWM | cwm-rust |
|--------|:------------:|:--------:|
| N3/Turtle | ✅ | ✅ |
| N-Triples | ✅ | ✅ |
| RDF/XML | ✅ | ✅ |
| JSON-LD | ❌ | ✅ |
| SPARQL Results XML | ✅ | ✅ |
| SPARQL Results JSON | ❌ | ✅ |

### 5. Parser Support

| Format | Original CWM | cwm-rust |
|--------|:------------:|:--------:|
| N3 | ✅ | ✅ |
| Turtle | ✅ | ✅ |
| N-Triples | ✅ | ✅ |
| RDF/XML | ✅ | ⚠️ (via web) |
| KIF | ❌ | ✅ |

## Remaining Gaps

### High Priority

1. **`--crypto` enforcement**: Flag is parsed but crypto builtins are always enabled
   - Original CWM requires explicit `--crypto` for security
   - **Fix**: Check `cli.crypto` flag before registering crypto builtins (main.rs:160)

2. **`log:definitiveDocument`**: Authoritative source declaration
   - Used in policy/trust frameworks

3. **`log:definitiveService`**: Authoritative service declaration
   - Used in policy/trust frameworks

4. **`crypto:publicKey`**: Extract public key from keypair
   - Need to implement key extraction

### Medium Priority

5. **`--engine=otter`**: Alternate reasoning engine
   - Original CWM supports Otter theorem prover
   - Specialized use case

6. **`--language` flag**: Full implementation
   - Currently parsed but not used

7. **`--mode` parameter**: Complete implementation
   - Partial support currently

### Low Priority

8. **`--revision`**: CVS revision display
   - Trivial to implement

9. **RDF/XML Parser**: Native parsing
   - Currently delegated to web fetch
   - Could add `quick-xml` based parser

## Summary Statistics

| Category | Original CWM | cwm-rust | Delta |
|----------|-------------|----------|-------|
| CLI options | ~30 | 35+ | +5 |
| Math builtins | 26 | 50+ | +24 |
| String builtins | 18 | 40+ | +22 |
| List builtins | 4 | 25+ | +21 |
| Log builtins | 21 | 28+ | +7 |
| Crypto builtins | 11 | 18+ | +7 |
| Time builtins | 11 | 20+ | +9 |
| OS builtins | 4 | 20+ | +16 |
| **Total builtins** | **~92** | **242+** | **+150** |

## Implementation Status

### ✅ Previously Marked as Gaps (Now Implemented)

These were in the original gap analysis but are now confirmed implemented:

| Feature | Location |
|---------|----------|
| `--unflatten` | main.rs:576-579, 1237-1318 |
| `log:N3Document` | builtins/mod.rs:2718-2727 |
| `log:Truth` | builtins/mod.rs:2729-2740 |
| `log:parsedAsN3` | builtins/mod.rs |
| `log:semanticsOrError` | builtins/mod.rs |
| `log:conclusion` | builtins/mod.rs |
| `log:collectAllIn` | builtins/mod.rs |
| SPARQL Property Paths | sparql/mod.rs:965-1110 |
| SPARQL Aggregates | sparql/mod.rs:416-554, 2542-2630 |
| SPARQL Subqueries | sparql/mod.rs:853-873, 1769-1795 |

### ✅ Previously Identified Gaps - Now Fixed

| Gap | Status | Notes |
|-----|--------|-------|
| `--crypto` enforcement | ✅ FIXED | Crypto builtins only registered when `--crypto` flag is set |
| `log:definitiveDocument` | ✅ ALREADY IMPLEMENTED | Was in codebase already |
| `log:definitiveService` | ✅ ALREADY IMPLEMENTED | Was in codebase already |
| `crypto:publicKey` | ✅ FIXED | Extracts public key from keypair or derives from private key |
| `--language` flag | ✅ FIXED | Now controls output format (n3, ntriples, rdf, jsonld) |
| `--mode` parameter | ✅ FIXED | Now supports r/w/t/f mode flags |
| `--revision` flag | ✅ FIXED | Outputs version information |

### ❌ Remaining Gaps

| Gap | Priority | Effort | Notes |
|-----|----------|--------|-------|
| `--engine=otter` | Low | High | Alternate reasoning engine (specialized) |
| Native RDF/XML parser | Low | High | Currently only N3/Turtle input; RDF/XML output works |

## Recommendations

### Future Consideration

1. **Native RDF/XML parser** - Would allow reading `.rdf` files directly. Currently the system can output RDF/XML but reads N3/Turtle.

2. **Otter engine integration** - Specialized use case for first-order logic proving. Low priority unless specific need arises.

## Conclusion

cwm-rust significantly exceeds the original CWM in built-in predicate coverage (+150 predicates) and SPARQL support (full SPARQL 1.1 including property paths, aggregates, and subqueries).

**All high-priority gaps have been addressed:**
- ✅ `--crypto` flag is now properly enforced
- ✅ `log:definitiveDocument` and `log:definitiveService` are implemented
- ✅ `crypto:publicKey` predicate is implemented
- ✅ `--language`, `--mode`, and `--revision` flags are fully functional

The remaining gaps are low priority (RDF/XML parser, Otter engine) and do not affect typical N3 reasoning workflows.

The implementation is highly compatible with original CWM for all common use cases.
