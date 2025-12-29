# CWM-Rust SPARQL Reference

Complete reference for SPARQL 1.1 support in cwm-rust.

## Table of Contents

- [Overview](#overview)
- [Query Types](#query-types)
- [Query Patterns](#query-patterns)
- [Functions & Operators](#functions--operators)
- [Property Paths](#property-paths)
- [Aggregates](#aggregates)
- [Subqueries](#subqueries)
- [Federated Queries](#federated-queries)
- [Update Operations](#update-operations)
- [SPARQL Server](#sparql-server)
- [Result Formats](#result-formats)
- [Examples](#examples)

---

## Overview

cwm-rust implements the complete SPARQL 1.1 Query Language specification with:

- All four query forms (SELECT, CONSTRUCT, ASK, DESCRIBE)
- Full graph pattern support (OPTIONAL, UNION, MINUS, FILTER, BIND)
- Property paths (*, +, ?, ^, /, |)
- Aggregates (COUNT, SUM, AVG, MIN, MAX, GROUP_CONCAT, SAMPLE)
- Subqueries and federated queries (SERVICE)
- Query optimization and result caching
- Multiple result formats (XML, JSON)

### Command-Line Usage

```bash
# Inline query
cwm data.n3 --sparql-query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"

# Query from file
cwm data.n3 --sparql query.rq

# With JSON results
cwm data.n3 --sparql-query "SELECT * WHERE { ?s ?p ?o }" --sparql-results json

# Start SPARQL endpoint
cwm data.n3 --sparqlServer 8080
```

---

## Query Types

### SELECT

Returns variable bindings as a result set.

```sparql
SELECT ?name ?age
WHERE {
  ?person a foaf:Person ;
          foaf:name ?name ;
          foaf:age ?age .
}
ORDER BY ?name
LIMIT 10
```

**Modifiers**:
- `DISTINCT` - Remove duplicate rows
- `REDUCED` - Allow but not require duplicate removal
- `ORDER BY` - Sort results (ASC/DESC)
- `LIMIT` - Maximum rows to return
- `OFFSET` - Skip first N rows

### CONSTRUCT

Builds an RDF graph from a template.

```sparql
CONSTRUCT {
  ?person foaf:knows ?friend .
  ?friend foaf:knows ?person .
}
WHERE {
  ?person ex:friendOf ?friend .
}
```

### ASK

Returns boolean indicating pattern match.

```sparql
ASK {
  ex:alice a foaf:Person .
}
```

### DESCRIBE

Returns triples describing a resource.

```sparql
DESCRIBE ex:alice
```

```sparql
DESCRIBE ?person
WHERE {
  ?person foaf:name "Alice" .
}
```

---

## Query Patterns

### Basic Graph Pattern

```sparql
WHERE {
  ?s ?p ?o .
}
```

### OPTIONAL

Include if available, otherwise bind to UNDEF.

```sparql
SELECT ?name ?email
WHERE {
  ?person foaf:name ?name .
  OPTIONAL { ?person foaf:mbox ?email }
}
```

### UNION

Alternative patterns (logical OR).

```sparql
SELECT ?name
WHERE {
  { ?person foaf:name ?name }
  UNION
  { ?person schema:name ?name }
}
```

### MINUS

Set difference (exclude matching patterns).

```sparql
SELECT ?person
WHERE {
  ?person a foaf:Person .
  MINUS { ?person ex:inactive true }
}
```

### FILTER

Constrain results with expressions.

```sparql
SELECT ?name ?age
WHERE {
  ?person foaf:name ?name ;
          foaf:age ?age .
  FILTER (?age >= 18 && ?age < 65)
}
```

### BIND

Assign expression result to variable.

```sparql
SELECT ?name ?ageInMonths
WHERE {
  ?person foaf:name ?name ;
          foaf:age ?age .
  BIND (?age * 12 AS ?ageInMonths)
}
```

### VALUES

Inline data for binding.

```sparql
SELECT ?name ?city
WHERE {
  VALUES ?city { ex:London ex:Paris ex:Berlin }
  ?person foaf:name ?name ;
          ex:livesIn ?city .
}
```

### GRAPH

Query named graphs.

```sparql
SELECT ?s ?p ?o
WHERE {
  GRAPH <http://example.org/graph1> {
    ?s ?p ?o .
  }
}
```

---

## Functions & Operators

### Comparison

| Operator | Description |
|----------|-------------|
| `=` | Equal |
| `!=` | Not equal |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less than or equal |
| `>=` | Greater than or equal |

### Logical

| Operator | Description |
|----------|-------------|
| `&&` | Logical AND |
| `\|\|` | Logical OR |
| `!` | Logical NOT |

### Arithmetic

| Operator | Description |
|----------|-------------|
| `+` | Addition |
| `-` | Subtraction |
| `*` | Multiplication |
| `/` | Division |

### String Functions

| Function | Description | Example |
|----------|-------------|---------|
| `STR(?x)` | Convert to string | `STR(42)` → "42" |
| `STRLEN(?s)` | String length | `STRLEN("hello")` → 5 |
| `SUBSTR(?s, start, len)` | Substring | `SUBSTR("hello", 2, 3)` → "ell" |
| `UCASE(?s)` | Uppercase | `UCASE("hello")` → "HELLO" |
| `LCASE(?s)` | Lowercase | `LCASE("HELLO")` → "hello" |
| `CONTAINS(?s, ?t)` | Contains substring | `CONTAINS("hello", "ell")` → true |
| `STRSTARTS(?s, ?t)` | Starts with | `STRSTARTS("hello", "he")` → true |
| `STRENDS(?s, ?t)` | Ends with | `STRENDS("hello", "lo")` → true |
| `CONCAT(?s1, ?s2, ...)` | Concatenate | `CONCAT("a", "b", "c")` → "abc" |
| `REPLACE(?s, pat, rep)` | Regex replace | `REPLACE("hello", "l", "L")` → "heLLo" |
| `REGEX(?s, pattern)` | Regex match | `REGEX("hello", "^h.*o$")` → true |
| `ENCODE_FOR_URI(?s)` | URI encode | `ENCODE_FOR_URI("a b")` → "a%20b" |

### Numeric Functions

| Function | Description |
|----------|-------------|
| `ABS(?n)` | Absolute value |
| `ROUND(?n)` | Round to nearest |
| `CEIL(?n)` | Round up |
| `FLOOR(?n)` | Round down |
| `RAND()` | Random 0-1 |

### Date/Time Functions

| Function | Description |
|----------|-------------|
| `NOW()` | Current datetime |
| `YEAR(?dt)` | Extract year |
| `MONTH(?dt)` | Extract month |
| `DAY(?dt)` | Extract day |
| `HOURS(?dt)` | Extract hours |
| `MINUTES(?dt)` | Extract minutes |
| `SECONDS(?dt)` | Extract seconds |
| `TIMEZONE(?dt)` | Get timezone |
| `TZ(?dt)` | Timezone string |

### Type Functions

| Function | Description |
|----------|-------------|
| `BOUND(?v)` | Variable is bound |
| `ISIRI(?x)` | Is IRI |
| `ISBLANK(?x)` | Is blank node |
| `ISLITERAL(?x)` | Is literal |
| `ISNUMERIC(?x)` | Is numeric |
| `DATATYPE(?x)` | Get datatype |
| `LANG(?x)` | Get language tag |
| `LANGMATCHES(?tag, ?range)` | Language match |
| `SAMETERM(?x, ?y)` | Identical terms |

### Constructor Functions

| Function | Description |
|----------|-------------|
| `IRI(?s)` | Create IRI |
| `BNODE()` | Create blank node |
| `BNODE(?id)` | Named blank node |
| `STRDT(?s, ?dt)` | Typed literal |
| `STRLANG(?s, ?lang)` | Language literal |
| `UUID()` | Generate UUID IRI |
| `STRUUID()` | Generate UUID string |

### Hash Functions

| Function | Description |
|----------|-------------|
| `MD5(?s)` | MD5 hash |
| `SHA1(?s)` | SHA-1 hash |
| `SHA256(?s)` | SHA-256 hash |
| `SHA384(?s)` | SHA-384 hash |
| `SHA512(?s)` | SHA-512 hash |

### Conditional

```sparql
IF(?age >= 18, "adult", "minor")

COALESCE(?preferredName, ?name, "Unknown")

# CASE-style via nested IF
IF(?status = 1, "Active",
   IF(?status = 2, "Pending", "Inactive"))
```

### EXISTS / NOT EXISTS

```sparql
SELECT ?person
WHERE {
  ?person a foaf:Person .
  FILTER NOT EXISTS {
    ?person ex:banned true .
  }
}
```

---

## Property Paths

Navigate graph structure with path expressions.

| Syntax | Description | Example |
|--------|-------------|---------|
| `iri` | Single step | `foaf:knows` |
| `^path` | Inverse | `^foaf:knows` |
| `path1/path2` | Sequence | `foaf:knows/foaf:name` |
| `path1\|path2` | Alternative | `foaf:name\|schema:name` |
| `path*` | Zero or more | `rdfs:subClassOf*` |
| `path+` | One or more | `foaf:knows+` |
| `path?` | Zero or one | `foaf:nick?` |
| `!iri` | Negation | `!rdf:type` |
| `!(iri1\|iri2)` | Negated set | `!(rdf:type\|rdfs:label)` |
| `(path)` | Grouping | `(foaf:knows/foaf:name)*` |

### Examples

```sparql
# Transitive closure (all ancestors)
SELECT ?ancestor
WHERE {
  ex:alice ex:parent+ ?ancestor .
}

# Reachable via any path
SELECT ?friend
WHERE {
  ex:alice foaf:knows* ?friend .
}

# Inverse relationship
SELECT ?follower
WHERE {
  ?follower ^ex:follows ex:bob .
}

# Alternative predicates
SELECT ?name
WHERE {
  ?person (foaf:name|schema:name|rdfs:label) ?name .
}
```

---

## Aggregates

### Aggregate Functions

| Function | Description |
|----------|-------------|
| `COUNT(*)` | Count rows |
| `COUNT(?x)` | Count bound values |
| `COUNT(DISTINCT ?x)` | Count unique values |
| `SUM(?x)` | Sum of values |
| `AVG(?x)` | Average |
| `MIN(?x)` | Minimum |
| `MAX(?x)` | Maximum |
| `SAMPLE(?x)` | Arbitrary value |
| `GROUP_CONCAT(?x; separator=",")` | Concatenate values |

### GROUP BY

```sparql
SELECT ?country (COUNT(?person) AS ?population)
WHERE {
  ?person ex:country ?country .
}
GROUP BY ?country
ORDER BY DESC(?population)
```

### HAVING

Filter groups after aggregation.

```sparql
SELECT ?author (COUNT(?book) AS ?bookCount)
WHERE {
  ?book ex:author ?author .
}
GROUP BY ?author
HAVING (COUNT(?book) > 5)
```

---

## Subqueries

Nest queries for complex logic.

```sparql
SELECT ?person ?avgFriendAge
WHERE {
  ?person a foaf:Person .
  {
    SELECT ?person (AVG(?age) AS ?avgFriendAge)
    WHERE {
      ?person foaf:knows ?friend .
      ?friend foaf:age ?age .
    }
    GROUP BY ?person
  }
}
```

---

## Federated Queries

Query remote SPARQL endpoints with SERVICE.

```sparql
SELECT ?name ?dbpediaInfo
WHERE {
  ?person foaf:name ?name .
  SERVICE <http://dbpedia.org/sparql> {
    ?dbpediaInfo rdfs:label ?name .
  }
}
```

### SERVICE Options

```sparql
# Silent (ignore errors)
SERVICE SILENT <http://example.org/sparql> {
  ?s ?p ?o .
}
```

---

## Update Operations

SPARQL Update for modifying data.

### INSERT DATA

```sparql
INSERT DATA {
  ex:alice foaf:name "Alice" .
  ex:alice foaf:age 30 .
}
```

### DELETE DATA

```sparql
DELETE DATA {
  ex:alice foaf:age 30 .
}
```

### DELETE/INSERT

```sparql
DELETE { ?person foaf:age ?oldAge }
INSERT { ?person foaf:age ?newAge }
WHERE {
  ?person foaf:age ?oldAge .
  BIND (?oldAge + 1 AS ?newAge)
}
```

### LOAD

```sparql
LOAD <http://example.org/data.ttl> INTO GRAPH <http://example.org/graph1>
```

### CLEAR

```sparql
CLEAR GRAPH <http://example.org/graph1>
CLEAR DEFAULT
CLEAR ALL
```

### DROP

```sparql
DROP GRAPH <http://example.org/graph1>
```

### CREATE

```sparql
CREATE GRAPH <http://example.org/newgraph>
```

> **Note**: Write operations are disabled on the SPARQL server endpoint for security. Use command-line operations for updates.

---

## SPARQL Server

### Starting the Server

```bash
# Basic server on port 8080
cwm data.n3 --sparqlServer 8080

# With reasoning
cwm data.n3 rules.n3 --think --sparqlServer 8080

# With Fuseki backend
cwm --fuseki http://localhost:3030/dataset --sparqlServer 8080
```

### Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/` | GET | HTML query form |
| `/sparql` | GET | Query via URL parameter |
| `/sparql` | POST | Query via request body |
| `/health` | GET | Health check |
| `/stats` | GET | Store statistics |
| `/cache/stats` | GET | Cache statistics |
| `/cache/clear` | POST | Clear query cache |

### Query via GET

```bash
curl "http://localhost:8080/sparql?query=SELECT%20*%20WHERE%20%7B%20%3Fs%20%3Fp%20%3Fo%20%7D"
```

### Query via POST

```bash
# URL-encoded
curl -X POST http://localhost:8080/sparql \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "query=SELECT * WHERE { ?s ?p ?o }"

# Direct SPARQL
curl -X POST http://localhost:8080/sparql \
  -H "Content-Type: application/sparql-query" \
  -d "SELECT * WHERE { ?s ?p ?o }"
```

### Accept Headers

| Accept Header | Result Format |
|---------------|---------------|
| `application/sparql-results+xml` | SPARQL XML Results |
| `application/sparql-results+json` | SPARQL JSON Results |
| `application/json` | SPARQL JSON Results |
| `text/xml` | SPARQL XML Results |

### Server Configuration

```toml
[server]
port = 8080
host = "0.0.0.0"
cors_enabled = true
max_body_size = 10485760  # 10 MB
timeout_secs = 30

[server.cache]
enabled = true
max_entries = 1000
ttl_secs = 300

[server.rate_limit]
enabled = true
requests_per_second = 100
burst = 50
```

---

## Result Formats

### SPARQL XML Results

```bash
cwm data.n3 --sparql-query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" --sparql-results xml
```

```xml
<?xml version="1.0"?>
<sparql xmlns="http://www.w3.org/2005/sparql-results#">
  <head>
    <variable name="s"/>
    <variable name="p"/>
    <variable name="o"/>
  </head>
  <results>
    <result>
      <binding name="s"><uri>http://example.org/alice</uri></binding>
      <binding name="p"><uri>http://xmlns.com/foaf/0.1/name</uri></binding>
      <binding name="o"><literal>Alice</literal></binding>
    </result>
  </results>
</sparql>
```

### SPARQL JSON Results

```bash
cwm data.n3 --sparql-query "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" --sparql-results json
```

```json
{
  "head": {
    "vars": ["s", "p", "o"]
  },
  "results": {
    "bindings": [
      {
        "s": {"type": "uri", "value": "http://example.org/alice"},
        "p": {"type": "uri", "value": "http://xmlns.com/foaf/0.1/name"},
        "o": {"type": "literal", "value": "Alice"}
      }
    ]
  }
}
```

---

## Examples

### Find All People and Their Friends

```sparql
SELECT ?person ?name ?friend ?friendName
WHERE {
  ?person a foaf:Person ;
          foaf:name ?name ;
          foaf:knows ?friend .
  ?friend foaf:name ?friendName .
}
ORDER BY ?name
```

### Count Resources by Type

```sparql
SELECT ?type (COUNT(?resource) AS ?count)
WHERE {
  ?resource a ?type .
}
GROUP BY ?type
ORDER BY DESC(?count)
LIMIT 20
```

### Find Shortest Path (via Property Path)

```sparql
SELECT ?start ?end (COUNT(?mid) AS ?distance)
WHERE {
  VALUES ?start { ex:alice }
  VALUES ?end { ex:david }
  ?start foaf:knows+ ?mid .
  ?mid foaf:knows* ?end .
}
GROUP BY ?start ?end
ORDER BY ?distance
LIMIT 1
```

### OPTIONAL with Defaults

```sparql
SELECT ?name (COALESCE(?email, "No email") AS ?contact)
WHERE {
  ?person foaf:name ?name .
  OPTIONAL { ?person foaf:mbox ?email }
}
```

### Date Filtering

```sparql
SELECT ?event ?date
WHERE {
  ?event ex:date ?date .
  FILTER (
    ?date >= "2024-01-01"^^xsd:date &&
    ?date < "2025-01-01"^^xsd:date
  )
}
ORDER BY ?date
```

### Text Search with Regex

```sparql
SELECT ?product ?description
WHERE {
  ?product ex:description ?description .
  FILTER REGEX(?description, "\\bsale\\b", "i")
}
```

---

## Query Optimization

cwm-rust includes automatic query optimization:

- **Triple pattern reordering** - Most selective patterns first
- **Filter pushing** - Apply filters early
- **Join ordering** - Optimize join sequence
- **Subquery optimization** - Flatten where possible

### Optimization Hints

For best performance:

1. Put most selective patterns first
2. Use specific types over generic patterns
3. Avoid `SELECT *` when possible
4. Use LIMIT early for exploratory queries
5. Enable caching for repeated queries

---

## See Also

- [BUILTINS.md](BUILTINS.md) - Built-in predicates
- [REASONING.md](REASONING.md) - Reasoning strategies
- [SERVER.md](SERVER.md) - HTTP server configuration
