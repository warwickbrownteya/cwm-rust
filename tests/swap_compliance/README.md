# SWAP Compliance Test Suite

This directory contains comprehensive compliance tests for cwm-rust against the W3C CWM/SWAP (Semantic Web Application Platform) specification.

## Test Files

| File | Description | Predicates Tested |
|------|-------------|-------------------|
| `math.n3` | Math namespace | sum, product, difference, quotient, remainder, exponentiation, negation, absoluteValue, rounded, sin, cos, tan, sinh, tanh, atan2, degrees, lessThan, greaterThan, equalTo, memberCount |
| `string.n3` | String namespace | concatenation, contains, containsIgnoringCase, containsRoughly, matches, notMatches, startsWith, endsWith, replace, scrape, format, length, upperCase, lowerCase, trim, split, substring |
| `list.n3` | List namespace | append, member, in, last, first, rest, nth, length, reverse, sort, unique, remove, flatten, iterate |
| `log.n3` | Log namespace | uri, racine, equalTo, notEqualTo, conjunction, n3String, parsedAsN3, includes, notIncludes, implies, rawType, dtlit |
| `time.n3` | Time namespace | gmTime, localTime, year, month, day, hour, minute, second, dayOfWeek, timeZone, inSeconds, durationInSeconds, add, difference, before, after, format |
| `crypto.n3` | Crypto namespace | sha, md5, sha256, sha384, sha512, hmacSha256, hmacSha512, base64Encode, base64Decode, hexEncode, hexDecode |
| `os.n3` | OS namespace | environ, argv, baseAbsolute, baseRelative, pid, hostname, cwd, username, platform, arch, time |
| `graph.n3` | Graph namespace | member, length, difference, equalTo, notEqualTo, subject, predicate, object |
| `n3_syntax.n3` | N3 Syntax | All N3 syntax features including triples, URIs, blank nodes, literals, collections, formulas, rules, paths |

## Running Tests

```bash
# Make the runner executable
chmod +x run_compliance.sh

# Run all compliance tests
./run_compliance.sh
```

## Test Structure

Each test file follows this pattern:

```n3
# Test with expected result
{ (2 3) math:sum ?result } => { :sum_test1 :result ?result }.

# Boolean test (fires if predicate succeeds)
{ 3 math:lessThan 5 } => { :lt_test1 :passed true }.

# Negative test (should NOT fire)
{ 5 math:lessThan 3 } => { :lt_test2 :passed true }.
```

## Reference

These tests are based on the official CWM built-in documentation:
- https://www.w3.org/2000/10/swap/doc/CwmBuiltins

## Coverage

### Original CWM Built-ins (100% coverage target)

- **math:** 27 predicates
- **string:** 18 predicates
- **list:** 4 predicates
- **log:** 21 predicates
- **time:** 11 predicates
- **crypto:** 11 predicates
- **os:** 4 predicates

### cwm-rust Extensions

cwm-rust extends the original CWM with additional predicates for:
- Extended hash algorithms (SHA-256, SHA-384, SHA-512)
- HMAC functions
- Base64 and hex encoding
- Additional string operations (length, case, trim, split)
- Additional list operations (reverse, sort, unique, flatten)
- Time arithmetic and comparison
- File system operations
- Platform information

## Compatibility Notes

1. **Regex syntax**: cwm-rust uses Rust regex syntax (similar to Perl/Python)
2. **Time formats**: Follows ISO 8601 and strftime patterns
3. **Unicode**: Full Unicode support in URIs and literals
4. **Formulas**: Full N3 formula/graph support including nested formulas
