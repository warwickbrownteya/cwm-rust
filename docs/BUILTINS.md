# cwm-rust Built-in Predicates Reference

This document provides a complete reference for all 266+ built-in predicates implemented in cwm-rust.

## Namespace Prefixes

```n3
@prefix math:   <http://www.w3.org/2000/10/swap/math#> .
@prefix string: <http://www.w3.org/2000/10/swap/string#> .
@prefix list:   <http://www.w3.org/2000/10/swap/list#> .
@prefix log:    <http://www.w3.org/2000/10/swap/log#> .
@prefix time:   <http://www.w3.org/2000/10/swap/time#> .
@prefix crypto: <http://www.w3.org/2000/10/swap/crypto#> .
@prefix os:     <http://www.w3.org/2000/10/swap/os#> .
@prefix graph:  <http://www.w3.org/2000/10/swap/graph#> .
```

---

## Math Namespace (`math:`)

Mathematical operations on numeric values.

### Arithmetic Operations

| Predicate | Signature | Description | Example |
|-----------|-----------|-------------|---------|
| `math:sum` | `(list) → number` | Sum of all numbers in list | `(2 3 5) math:sum ?x` → `10` |
| `math:difference` | `(a b) → number` | Subtraction: a - b | `(10 3) math:difference ?x` → `7` |
| `math:product` | `(list) → number` | Product of all numbers in list | `(2 3 4) math:product ?x` → `24` |
| `math:quotient` | `(a b) → number` | Division: a / b | `(10 4) math:quotient ?x` → `2.5` |
| `math:integerQuotient` | `(a b) → integer` | Integer division: floor(a / b) | `(10 3) math:integerQuotient ?x` → `3` |
| `math:remainder` | `(a b) → number` | Modulo: a mod b | `(10 3) math:remainder ?x` → `1` |
| `math:exponentiation` | `(base exp) → number` | Power: base^exp | `(2 10) math:exponentiation ?x` → `1024` |
| `math:negation` | `number ↔ number` | Negate: -n | `5 math:negation ?x` → `-5` |
| `math:absoluteValue` | `number → number` | Absolute value | `-5 math:absoluteValue ?x` → `5` |

### Rounding Operations

| Predicate | Signature | Description | Example |
|-----------|-----------|-------------|---------|
| `math:rounded` | `number → integer` | Round to nearest integer | `3.7 math:rounded ?x` → `4` |
| `math:floor` | `number → integer` | Round down | `3.7 math:floor ?x` → `3` |
| `math:ceiling` | `number → integer` | Round up | `3.2 math:ceiling ?x` → `4` |

### Trigonometric Functions

All angles in radians.

| Predicate | Description | Example |
|-----------|-------------|---------|
| `math:sin` | Sine | `0 math:sin ?x` → `0` |
| `math:cos` | Cosine | `0 math:cos ?x` → `1` |
| `math:tan` | Tangent | `0 math:tan ?x` → `0` |
| `math:asin` | Arc sine | `0 math:asin ?x` → `0` |
| `math:acos` | Arc cosine | `1 math:acos ?x` → `0` |
| `math:atan` | Arc tangent | `0 math:atan ?x` → `0` |
| `math:atan2` | Arc tangent of y/x | `(1 1) math:atan2 ?x` → `0.785...` |
| `math:sinh` | Hyperbolic sine | `0 math:sinh ?x` → `0` |
| `math:cosh` | Hyperbolic cosine | `0 math:cosh ?x` → `1` |
| `math:tanh` | Hyperbolic tangent | `0 math:tanh ?x` → `0` |

### Conversion Functions

| Predicate | Description | Example |
|-----------|-------------|---------|
| `math:degrees` | Radians to degrees | `3.14159 math:degrees ?x` → `180` |
| `math:radians` | Degrees to radians | `180 math:radians ?x` → `3.14159` |
| `math:logarithm` | Natural logarithm | `2.718 math:logarithm ?x` → `1` |
| `math:sqrt` | Square root | `9 math:sqrt ?x` → `3` |

### Comparison Predicates

| Predicate | Description | Example |
|-----------|-------------|---------|
| `math:lessThan` | a < b | `3 math:lessThan 5` → succeeds |
| `math:greaterThan` | a > b | `5 math:greaterThan 3` → succeeds |
| `math:equalTo` | a = b (numeric) | `5 math:equalTo 5` → succeeds |
| `math:notEqualTo` | a ≠ b | `5 math:notEqualTo 3` → succeeds |
| `math:notLessThan` | a ≥ b | `5 math:notLessThan 5` → succeeds |
| `math:notGreaterThan` | a ≤ b | `5 math:notGreaterThan 5` → succeeds |

### List Aggregation

| Predicate | Description | Example |
|-----------|-------------|---------|
| `math:memberCount` | Count list elements | `(1 2 3) math:memberCount ?x` → `3` |
| `math:sumOf` | Sum of list | `(1 2 3) math:sumOf ?x` → `6` |
| `math:productOf` | Product of list | `(2 3 4) math:productOf ?x` → `24` |
| `math:min` | Minimum value | `(5 2 8) math:min ?x` → `2` |
| `math:max` | Maximum value | `(5 2 8) math:max ?x` → `8` |
| `math:average` | Average value | `(2 4 6) math:average ?x` → `4` |

### Bitwise Operations

| Predicate | Description | Example |
|-----------|-------------|---------|
| `math:bitAnd` | Bitwise AND | `(12 10) math:bitAnd ?x` → `8` |
| `math:bitOr` | Bitwise OR | `(12 10) math:bitOr ?x` → `14` |
| `math:bitXor` | Bitwise XOR | `(12 10) math:bitXor ?x` → `6` |
| `math:bitNot` | Bitwise NOT | `12 math:bitNot ?x` |

---

## String Namespace (`string:`)

String manipulation operations.

### Concatenation

| Predicate | Signature | Description | Example |
|-----------|-----------|-------------|---------|
| `string:concatenation` | `(list) → string` | Join strings | `("a" "b" "c") string:concatenation ?x` → `"abc"` |
| `string:concat` | `(a b) → string` | Concatenate pair | `("a" "b") string:concat ?x` → `"ab"` |

### Substring Tests

| Predicate | Description | Example |
|-----------|-------------|---------|
| `string:contains` | Substring exists | `"hello" string:contains "ell"` → succeeds |
| `string:notContains` | No substring | `"hello" string:notContains "xyz"` → succeeds |
| `string:containsIgnoringCase` | Case-insensitive | `"Hello" string:containsIgnoringCase "ELL"` → succeeds |
| `string:containsRoughly` | Normalized whitespace | `"  foo  bar  " string:containsRoughly "foo bar"` → succeeds |
| `string:startsWith` | Prefix test | `"hello" string:startsWith "hel"` → succeeds |
| `string:endsWith` | Suffix test | `"hello" string:endsWith "llo"` → succeeds |

### String Comparison

| Predicate | Description | Example |
|-----------|-------------|---------|
| `string:lessThan` | Lexicographic < | `"apple" string:lessThan "banana"` → succeeds |
| `string:greaterThan` | Lexicographic > | `"banana" string:greaterThan "apple"` → succeeds |
| `string:notLessThan` | Lexicographic ≥ | `"apple" string:notLessThan "apple"` → succeeds |
| `string:notGreaterThan` | Lexicographic ≤ | `"apple" string:notGreaterThan "apple"` → succeeds |
| `string:equalIgnoringCase` | Case-insensitive = | `"Hello" string:equalIgnoringCase "hello"` → succeeds |
| `string:notEqualIgnoringCase` | Case-insensitive ≠ | `"Hello" string:notEqualIgnoringCase "World"` → succeeds |

### Regular Expressions

| Predicate | Description | Example |
|-----------|-------------|---------|
| `string:matches` | Regex match | `"hello123" string:matches "^hello[0-9]+$"` → succeeds |
| `string:notMatches` | Regex no match | `"hello" string:notMatches "^[0-9]+$"` → succeeds |
| `string:scrape` | Extract match | `("hello123" "([0-9]+)") string:scrape ?x` → `"123"` |
| `string:replace` | Regex replace | `("hello" "l" "L") string:replace ?x` → `"heLLo"` |

### String Transformation

| Predicate | Description | Example |
|-----------|-------------|---------|
| `string:length` | String length | `"hello" string:length ?x` → `5` |
| `string:lowerCase` | To lowercase | `"HELLO" string:lowerCase ?x` → `"hello"` |
| `string:upperCase` | To uppercase | `"hello" string:upperCase ?x` → `"HELLO"` |
| `string:trim` | Trim whitespace | `"  hello  " string:trim ?x` → `"hello"` |
| `string:trimLeft` | Trim leading | `"  hello" string:trimLeft ?x` → `"hello"` |
| `string:trimRight` | Trim trailing | `"hello  " string:trimRight ?x` → `"hello"` |
| `string:normalize` | Normalize whitespace | `"  a   b  " string:normalize ?x` → `"a b"` |
| `string:reverse` | Reverse string | `"hello" string:reverse ?x` → `"olleh"` |

### Substring Operations

| Predicate | Description | Example |
|-----------|-------------|---------|
| `string:substring` | Extract substring | `("hello" 1 3) string:substring ?x` → `"ell"` |
| `string:indexOf` | Find first index | `("hello" "l") string:indexOf ?x` → `2` |
| `string:lastIndexOf` | Find last index | `("hello" "l") string:lastIndexOf ?x` → `3` |

### Split and Join

| Predicate | Description | Example |
|-----------|-------------|---------|
| `string:split` | Split by delimiter | `("a,b,c" ",") string:split ?x` → `("a" "b" "c")` |
| `string:join` | Join with separator | `(("a" "b" "c") "-") string:join ?x` → `"a-b-c"` |
| `string:lines` | Split by newlines | `"a\nb" string:lines ?x` → `("a" "b")` |
| `string:words` | Split by whitespace | `"a b c" string:words ?x` → `("a" "b" "c")` |

### Padding and Repetition

| Predicate | Description | Example |
|-----------|-----------|---------|
| `string:repeat` | Repeat string | `("ab" 3) string:repeat ?x` → `"ababab"` |
| `string:padStart` | Pad start | `("42" 5 "0") string:padStart ?x` → `"00042"` |
| `string:padEnd` | Pad end | `("42" 5) string:padEnd ?x` → `"42   "` |

### Formatting

| Predicate | Description | Example |
|-----------|-------------|---------|
| `string:format` | Printf-style format | `("%s is %d" "answer" 42) string:format ?x` → `"answer is 42"` |

---

## List Namespace (`list:`)

List operations.

### Membership

| Predicate | Description | Example |
|-----------|-------------|---------|
| `list:member` | Element in list | `(1 2 3) list:member 2` → succeeds |
| `list:in` | Element in list (reversed) | `2 list:in (1 2 3)` → succeeds |

### Access

| Predicate | Description | Example |
|-----------|-------------|---------|
| `list:first` | First element | `(1 2 3) list:first ?x` → `1` |
| `list:last` | Last element | `(1 2 3) list:last ?x` → `3` |
| `list:rest` | Tail of list | `(1 2 3) list:rest ?x` → `(2 3)` |
| `list:nth` | Element at index | `((1 2 3) 1) list:nth ?x` → `2` |
| `list:length` | List length | `(1 2 3) list:length ?x` → `3` |

### Modification

| Predicate | Description | Example |
|-----------|-------------|---------|
| `list:append` | Concatenate lists | `((1 2) (3 4)) list:append ?x` → `(1 2 3 4)` |
| `list:remove` | Remove element | `((1 2 3) 2) list:remove ?x` → `(1 3)` |
| `list:unique` | Remove duplicates | `(1 2 2 3) list:unique ?x` → `(1 2 3)` |
| `list:sort` | Sort list | `(3 1 2) list:sort ?x` → `(1 2 3)` |
| `list:reverse` | Reverse list | `(1 2 3) list:reverse ?x` → `(3 2 1)` |
| `list:flat` | Flatten one level | `((1 2) (3 4)) list:flat ?x` → `(1 2 3 4)` |

### Set Operations

| Predicate | Description | Example |
|-----------|-------------|---------|
| `list:setUnion` | Set union | `((1 2) (2 3)) list:setUnion ?x` → `(1 2 3)` |
| `list:setIntersection` | Set intersection | `((1 2 3) (2 3 4)) list:setIntersection ?x` → `(2 3)` |
| `list:setDifference` | Set difference | `((1 2 3) (2)) list:setDifference ?x` → `(1 3)` |

### Slicing

| Predicate | Description | Example |
|-----------|-------------|---------|
| `list:take` | Take first N | `((1 2 3 4) 2) list:take ?x` → `(1 2)` |
| `list:drop` | Drop first N | `((1 2 3 4) 2) list:drop ?x` → `(3 4)` |
| `list:slice` | Extract slice | `((1 2 3 4) 1 3) list:slice ?x` → `(2 3)` |

### Iteration

| Predicate | Description | Example |
|-----------|-------------|---------|
| `list:iterate` | Enumerate with index | `(a b) list:iterate ?x` → `((0 a) (1 b))` |
| `list:zip` | Zip two lists | `((1 2) (a b)) list:zip ?x` → `((1 a) (2 b))` |

---

## Log Namespace (`log:`)

Logical and semantic operations.

### Rules and Implication

| Predicate | Description |
|-----------|-------------|
| `log:implies` | Rule implication (body => head) |

### Term Comparison

| Predicate | Description | Example |
|-----------|-------------|---------|
| `log:equalTo` | Term equality | `:foo log:equalTo :foo` → succeeds |
| `log:notEqualTo` | Term inequality | `:foo log:notEqualTo :bar` → succeeds |

### Formula Operations

| Predicate | Description | Example |
|-----------|-------------|---------|
| `log:includes` | Formula contains pattern | `{ :a :b :c } log:includes { :a :b :c }` → succeeds |
| `log:notIncludes` | Formula excludes pattern | `{ :a :b :c } log:notIncludes { :x :y :z }` → succeeds |
| `log:conjunction` | Merge formulas | `({ :a :b :c } { :d :e :f }) log:conjunction ?x` |
| `log:conclusion` | Derive all conclusions | `{ rules } log:conclusion ?x` |

### URI Operations

| Predicate | Description | Example |
|-----------|-------------|---------|
| `log:uri` | Get URI as string | `:foo log:uri ?x` → `"http://...foo"` |
| `log:rawUri` | Raw URI string | `<http://example.org/> log:rawUri ?x` |
| `log:racine` | Base URI without fragment | `<http://ex.org/doc#frag> log:racine ?x` → `<http://ex.org/doc>` |

### Type Checking

| Predicate | Description | Example |
|-----------|-------------|---------|
| `log:rawType` | Get term type | `:resource log:rawType ?t` → `"Other"` |
| `log:bound` | Variable is bound | `?x log:bound ?b` |
| `log:notBound` | Variable is unbound | `?x log:notBound ?b` |

### Literal Construction

| Predicate | Description | Example |
|-----------|-------------|---------|
| `log:dtlit` | Create typed literal | `("42" xsd:integer) log:dtlit ?x` |
| `log:langlit` | Create language literal | `("hello" "en") log:langlit ?x` |

### Document Operations

| Predicate | Description |
|-----------|-------------|
| `log:content` | Get file/URL content as string |
| `log:semantics` | Load and parse document |
| `log:semanticsOrError` | Load or return error message |
| `log:parsedAsN3` | Parse string as N3 |
| `log:n3String` | Serialize formula to N3 |

### Output

| Predicate | Description |
|-----------|-------------|
| `log:outputString` | Mark for string output |
| `log:collectAllIn` | Collect all matching bindings |

---

## Time Namespace (`time:`)

Date and time operations.

### Current Time

| Predicate | Description | Example |
|-----------|-------------|---------|
| `time:gmTime` | Current UTC time | `"" time:gmTime ?now` |
| `time:localTime` | Current local time | `"" time:localTime ?now` |

### DateTime Components

| Predicate | Description | Example |
|-----------|-------------|---------|
| `time:year` | Extract year | `"2024-06-15T12:00:00Z" time:year ?x` → `"2024"` |
| `time:month` | Extract month (01-12) | `"2024-06-15T12:00:00Z" time:month ?x` → `"06"` |
| `time:day` | Extract day (01-31) | `"2024-06-15T12:00:00Z" time:day ?x` → `"15"` |
| `time:hour` | Extract hour (00-23) | `"2024-06-15T12:30:00Z" time:hour ?x` → `"12"` |
| `time:minute` | Extract minute (00-59) | `"2024-06-15T12:30:00Z" time:minute ?x` → `"30"` |
| `time:second` | Extract second (00-59) | `"2024-06-15T12:00:45Z" time:second ?x` → `"45"` |
| `time:dayOfWeek` | Day of week (0=Sun) | `"2024-06-15T12:00:00Z" time:dayOfWeek ?x` → `6` |
| `time:timeZone` | Timezone offset | `"2024-06-15T12:00:00-05:00" time:timeZone ?x` → `"-05:00"` |

### Epoch Conversion

| Predicate | Description | Example |
|-----------|-------------|---------|
| `time:inSeconds` | DateTime ↔ Unix timestamp | `"2024-01-01T00:00:00Z" time:inSeconds ?x` |

### Duration

| Predicate | Description | Example |
|-----------|-------------|---------|
| `time:durationInSeconds` | Parse ISO 8601 duration | `"P1D" time:durationInSeconds ?x` → `86400` |

### Arithmetic

| Predicate | Description | Example |
|-----------|-------------|---------|
| `time:add` | Add duration to datetime | `("2024-06-15T00:00:00Z" "P1D") time:add ?x` |
| `time:difference` | Difference between datetimes | `("2024-06-16" "2024-06-15") time:difference ?x` |

### Comparison

| Predicate | Description |
|-----------|-------------|
| `time:before` | DateTime is before |
| `time:after` | DateTime is after |
| `time:equalTo` | DateTime equality |

---

## Crypto Namespace (`crypto:`)

Cryptographic operations.

### Hash Functions

| Predicate | Description | Example |
|-----------|-------------|---------|
| `crypto:md5` | MD5 hash | `"hello" crypto:md5 ?x` |
| `crypto:sha` | SHA-1 hash | `"hello" crypto:sha ?x` |
| `crypto:sha1` | SHA-1 hash (alias) | `"hello" crypto:sha1 ?x` |
| `crypto:sha256` | SHA-256 hash | `"hello" crypto:sha256 ?x` |
| `crypto:sha384` | SHA-384 hash | `"hello" crypto:sha384 ?x` |
| `crypto:sha512` | SHA-512 hash | `"hello" crypto:sha512 ?x` |

### HMAC

| Predicate | Description | Example |
|-----------|-------------|---------|
| `crypto:hmacSha256` | HMAC-SHA256 | `("message" "key") crypto:hmacSha256 ?x` |
| `crypto:hmacSha512` | HMAC-SHA512 | `("message" "key") crypto:hmacSha512 ?x` |
| `crypto:sign` | HMAC signature | `("data" "key") crypto:sign ?x` |
| `crypto:verify` | Verify signature | `(("data" "key") "sig") crypto:verify ?x` |
| `crypto:verifyBoolean` | Verify (boolean) | `(("data" "key") "sig") crypto:verifyBoolean true` |

### Encoding

| Predicate | Description | Example |
|-----------|-------------|---------|
| `crypto:base64Encode` | Base64 encode | `"hello" crypto:base64Encode ?x` → `"aGVsbG8="` |
| `crypto:base64Decode` | Base64 decode | `"aGVsbG8=" crypto:base64Decode ?x` → `"hello"` |
| `crypto:hexEncode` | Hex encode | `"hello" crypto:hexEncode ?x` → `"68656c6c6f"` |
| `crypto:hexDecode` | Hex decode | `"68656c6c6f" crypto:hexDecode ?x` → `"hello"` |

---

## OS Namespace (`os:`)

Operating system operations.

### Environment

| Predicate | Description | Example |
|-----------|-------------|---------|
| `os:environ` | Environment variable | `"HOME" os:environ ?x` |
| `os:argv` | Command-line argument | `0 os:argv ?x` |

### Paths

| Predicate | Description | Example |
|-----------|-------------|---------|
| `os:baseAbsolute` | Resolve to absolute | `"./file" os:baseAbsolute ?x` |
| `os:baseRelative` | Make relative | `"/abs/path" os:baseRelative ?x` |
| `os:cwd` | Current directory | `"" os:cwd ?x` |

### System Info

| Predicate | Description | Example |
|-----------|-------------|---------|
| `os:pid` | Process ID | `"" os:pid ?x` |
| `os:hostname` | Machine hostname | `"" os:hostname ?x` |
| `os:username` | Current user | `"" os:username ?x` |
| `os:platform` | Platform name | `"" os:platform ?x` → `"darwin"` or `"linux"` |
| `os:arch` | CPU architecture | `"" os:arch ?x` |
| `os:time` | Unix timestamp | `"" os:time ?x` |

---

## Graph Namespace (`graph:`)

Graph/formula operations.

### Access

| Predicate | Description | Example |
|-----------|-------------|---------|
| `graph:member` | Get triples from graph | `{ :a :b :c } graph:member ?x` |
| `graph:length` | Count triples | `{ :a :b :c } graph:length ?x` → `1` |

### Comparison

| Predicate | Description |
|-----------|-------------|
| `graph:equalTo` | Graph isomorphism |
| `graph:notEqualTo` | Graphs not isomorphic |

### Set Operations

| Predicate | Description |
|-----------|-------------|
| `graph:union` | Graph union |
| `graph:intersection` | Graph intersection |
| `graph:difference` | Graph difference |

### Statement Access

| Predicate | Description |
|-----------|-------------|
| `graph:subject` | Get subject of statement |
| `graph:predicate` | Get predicate of statement |
| `graph:object` | Get object of statement |

---

## See Also

- [README.md](../README.md) - Main documentation
- [COMPARISON.md](../COMPARISON.md) - Comparison with original CWM
- [tests/swap_compliance/](../tests/swap_compliance/) - Compliance tests
