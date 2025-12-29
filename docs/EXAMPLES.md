# CWM-Rust Examples & Use Cases

Practical examples demonstrating cwm-rust capabilities for various use cases.

## Table of Contents

- [Getting Started](#getting-started)
- [Basic Reasoning](#basic-reasoning)
- [SPARQL Queries](#sparql-queries)
- [Data Transformation](#data-transformation)
- [Ontology Processing](#ontology-processing)
- [Theorem Proving](#theorem-proving)
- [Web Integration](#web-integration)
- [Advanced Scenarios](#advanced-scenarios)

---

## Getting Started

### Hello World

```n3
# hello.n3
@prefix ex: <http://example.org/> .

ex:hello ex:message "Hello, World!" .
```

```bash
$ cwm hello.n3
@prefix ex: <http://example.org/> .

ex:hello ex:message "Hello, World!" .
```

### Simple Rule

```n3
# socrates.n3
@prefix ex: <http://example.org/> .

ex:socrates a ex:Human .

{ ?x a ex:Human } => { ?x a ex:Mortal } .
```

```bash
$ cwm socrates.n3 --think
@prefix ex: <http://example.org/> .

ex:socrates a ex:Human, ex:Mortal .
```

---

## Basic Reasoning

### Transitive Closure

```n3
# ancestors.n3
@prefix ex: <http://example.org/> .

ex:alice ex:parent ex:bob .
ex:bob ex:parent ex:carol .
ex:carol ex:parent ex:david .

# Transitive rule
{ ?x ex:parent ?y . ?y ex:parent ?z } => { ?x ex:ancestor ?z } .

# Direct parent is ancestor
{ ?x ex:parent ?y } => { ?x ex:ancestor ?y } .
```

```bash
$ cwm ancestors.n3 --think --filter
ex:alice ex:ancestor ex:bob, ex:carol, ex:david .
ex:bob ex:ancestor ex:carol, ex:david .
ex:carol ex:ancestor ex:david .
```

### Symmetric and Inverse Properties

```n3
# relationships.n3
@prefix ex: <http://example.org/> .

ex:alice ex:knows ex:bob .
ex:carol ex:parentOf ex:david .

# Symmetric: knows is mutual
{ ?x ex:knows ?y } => { ?y ex:knows ?x } .

# Inverse: childOf is inverse of parentOf
{ ?x ex:parentOf ?y } => { ?y ex:childOf ?x } .
```

```bash
$ cwm relationships.n3 --think --filter
ex:bob ex:knows ex:alice .
ex:david ex:childOf ex:carol .
```

### Classification with Conditions

```n3
# classification.n3
@prefix ex: <http://example.org/> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .

ex:product1 ex:price 50 ; ex:category ex:Electronics .
ex:product2 ex:price 150 ; ex:category ex:Electronics .
ex:product3 ex:price 25 ; ex:category ex:Books .

# Classify as expensive if price > 100
{ ?p ex:price ?price . ?price math:greaterThan 100 }
    => { ?p a ex:ExpensiveItem } .

# Classify as budget if price < 30
{ ?p ex:price ?price . ?price math:lessThan 30 }
    => { ?p a ex:BudgetItem } .
```

```bash
$ cwm classification.n3 --think --filter
ex:product2 a ex:ExpensiveItem .
ex:product3 a ex:BudgetItem .
```

---

## SPARQL Queries

### Basic SELECT

```bash
# Find all people and their names
cwm data.n3 --sparql-query "
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?person ?name
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
  }
  ORDER BY ?name
"
```

### Aggregation

```bash
# Count people per city
cwm data.n3 --sparql-query "
  PREFIX ex: <http://example.org/>
  SELECT ?city (COUNT(?person) AS ?count)
  WHERE {
    ?person ex:livesIn ?city .
  }
  GROUP BY ?city
  HAVING (COUNT(?person) > 5)
  ORDER BY DESC(?count)
"
```

### Property Paths

```bash
# Find all ancestors (transitive)
cwm family.n3 --sparql-query "
  PREFIX ex: <http://example.org/>
  SELECT ?person ?ancestor
  WHERE {
    ?person ex:parent+ ?ancestor .
  }
"
```

### OPTIONAL and FILTER

```bash
# Find people, optionally with email, filter by age
cwm data.n3 --sparql-query "
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX ex: <http://example.org/>
  SELECT ?name ?email ?age
  WHERE {
    ?person foaf:name ?name ;
            ex:age ?age .
    OPTIONAL { ?person foaf:mbox ?email }
    FILTER (?age >= 18)
  }
"
```

### CONSTRUCT

```bash
# Transform data to different schema
cwm data.n3 --sparql-query "
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX schema: <http://schema.org/>
  CONSTRUCT {
    ?person a schema:Person ;
            schema:name ?name ;
            schema:email ?email .
  }
  WHERE {
    ?person a foaf:Person ;
            foaf:name ?name .
    OPTIONAL { ?person foaf:mbox ?email }
  }
"
```

---

## Data Transformation

### Format Conversion

```bash
# N3 to RDF/XML
cwm data.n3 --format rdf > data.rdf

# N3 to JSON-LD
cwm data.n3 --format jsonld > data.jsonld

# N3 to N-Triples
cwm data.n3 --format ntriples > data.nt
```

### Data Enrichment

```n3
# enrich.n3
@prefix ex: <http://example.org/> .
@prefix string: <http://www.w3.org/2000/10/swap/string#> .

# Add full name from first and last
{ ?p ex:firstName ?first .
  ?p ex:lastName ?last .
  (?first " " ?last) string:concatenation ?full }
    => { ?p ex:fullName ?full } .

# Normalize email to lowercase
{ ?p ex:email ?email .
  ?email string:lowerCase ?normalized }
    => { ?p ex:normalizedEmail ?normalized } .
```

```bash
cwm data.n3 enrich.n3 --think
```

### Schema Migration

```n3
# migrate.n3
@prefix old: <http://old-schema.org/> .
@prefix new: <http://new-schema.org/> .

# Migrate old property to new
{ ?s old:userName ?v } => { ?s new:username ?v } .
{ ?s old:emailAddress ?v } => { ?s new:email ?v } .
{ ?s a old:User } => { ?s a new:Account } .
```

```bash
cwm old-data.n3 migrate.n3 --think --purge-rules > new-data.n3
```

---

## Ontology Processing

### RDFS Inference

```n3
# ontology.n3
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/> .

ex:Dog rdfs:subClassOf ex:Mammal .
ex:Mammal rdfs:subClassOf ex:Animal .
ex:Animal rdfs:subClassOf ex:LivingThing .

ex:hasPet rdfs:domain ex:Person .
ex:hasPet rdfs:range ex:Animal .

# Instance
ex:alice ex:hasPet ex:fido .
ex:fido a ex:Dog .
```

```bash
$ cwm ontology.n3 --profile rdfs --think --filter
ex:Dog rdfs:subClassOf ex:Animal, ex:LivingThing .
ex:Mammal rdfs:subClassOf ex:LivingThing .
ex:alice a ex:Person .
ex:fido a ex:Mammal, ex:Animal, ex:LivingThing .
```

### OWL 2 RL Reasoning

```n3
# owl-example.n3
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://example.org/> .

ex:hasSpouse a owl:SymmetricProperty .
ex:hasParent owl:inverseOf ex:hasChild .
ex:hasAncestor a owl:TransitiveProperty .

ex:alice ex:hasSpouse ex:bob .
ex:carol ex:hasParent ex:alice .
ex:alice ex:hasAncestor ex:david .
ex:david ex:hasAncestor ex:eve .
```

```bash
$ cwm owl-example.n3 --profile owl --think --filter
ex:bob ex:hasSpouse ex:alice .           # Symmetric
ex:alice ex:hasChild ex:carol .           # Inverse
ex:alice ex:hasAncestor ex:eve .          # Transitive
```

### Consistency Checking

```n3
# Check for constraint violations
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://example.org/> .

ex:Male owl:disjointWith ex:Female .

ex:alice a ex:Male .
ex:alice a ex:Female .  # Inconsistency!
```

```bash
$ cwm inconsistent.n3 --profile owl --check-consistency
ERROR: Consistency violation: ex:alice is member of disjoint classes ex:Male and ex:Female
```

---

## Theorem Proving

### Propositional Logic

```n3
# propositional.n3
@prefix logic: <http://example.org/logic#> .

# Modus Ponens: If P and P→Q, then Q
logic:p logic:holds true .
{ logic:p logic:holds true } => { logic:q logic:holds true } .
```

```bash
$ cwm propositional.n3 --engine dpll --think
logic:q logic:holds true .
```

### First-Order Logic

```n3
# fol.n3
@prefix : <http://example.org/> .

# All humans are mortal
# Socrates is human
# Therefore, Socrates is mortal

:socrates a :Human .
{ ?x a :Human } => { ?x a :Mortal } .
```

```bash
$ cwm fol.n3 --engine otter --think --why
# Shows proof trace
```

### Equational Reasoning

```n3
# group.n3
@prefix : <http://example.org/> .

# Group axioms
:e :leftIdentity true .  # e * x = x
:e :rightIdentity true . # x * e = x

# Prove: e * e = e
```

```bash
$ cwm group.n3 --engine knuth-bendix --think
```

---

## Web Integration

### SPARQL Endpoint Server

```bash
# Start server with data
cwm data.n3 --sparqlServer 8080

# With reasoning
cwm data.n3 rules.n3 --think --sparqlServer 8080
```

Query via curl:

```bash
# URL-encoded query
curl "http://localhost:8080/sparql?query=SELECT%20*%20WHERE%20%7B%20%3Fs%20%3Fp%20%3Fo%20%7D"

# POST query
curl -X POST http://localhost:8080/sparql \
  -H "Content-Type: application/sparql-query" \
  -d "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# JSON results
curl -X POST http://localhost:8080/sparql \
  -H "Content-Type: application/sparql-query" \
  -H "Accept: application/sparql-results+json" \
  -d "SELECT * WHERE { ?s ?p ?o } LIMIT 10"
```

### Fuseki Backend

```bash
# Use Fuseki as triple store
cwm --fuseki http://localhost:3030/dataset --think

# Load data into Fuseki
cwm data.n3 --fuseki http://localhost:3030/dataset

# Query with Fuseki backend and serve
cwm --fuseki http://localhost:3030/dataset --sparqlServer 8080
```

### Remote Data Fetching

```n3
# fetch.n3
@prefix log: <http://www.w3.org/2000/10/swap/log#> .

# Fetch and merge remote data
<http://example.org/data.n3> log:semantics ?graph .
```

```bash
cwm fetch.n3 --closure=i --think
```

---

## Advanced Scenarios

### Data Validation

```n3
# validate.n3
@prefix ex: <http://example.org/> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .
@prefix string: <http://www.w3.org/2000/10/swap/string#> .

# Email must contain @
{ ?p ex:email ?email .
  ?email string:notContains "@" }
    => { ?p ex:validationError "Invalid email format" } .

# Age must be positive
{ ?p ex:age ?age .
  ?age math:lessThan 0 }
    => { ?p ex:validationError "Age cannot be negative" } .

# Name is required
{ ?p a ex:Person .
  ?p log:notBound ex:name }
    => { ?p ex:validationError "Name is required" } .
```

```bash
cwm data.n3 validate.n3 --think --filter
```

### Event Processing

```n3
# events.n3
@prefix ev: <http://example.org/event#> .
@prefix time: <http://www.w3.org/2000/10/swap/time#> .
@prefix math: <http://www.w3.org/2000/10/swap/math#> .

# Detect high-value transactions
{ ?tx a ev:Transaction .
  ?tx ev:amount ?amt .
  ?amt math:greaterThan 10000 }
    => { ?tx a ev:HighValueTransaction .
         ?tx ev:requiresReview true } .

# Detect suspicious patterns: multiple transactions in short time
{ ?tx1 a ev:Transaction .
  ?tx2 a ev:Transaction .
  ?tx1 ev:account ?acct .
  ?tx2 ev:account ?acct .
  ?tx1 ev:timestamp ?t1 .
  ?tx2 ev:timestamp ?t2 .
  (?t2 ?t1) math:difference ?diff .
  ?diff math:lessThan 60 }  # Within 60 seconds
    => { ?acct ev:suspiciousActivity true } .
```

### Knowledge Graph Completion

```n3
# kgc.n3
@prefix ex: <http://example.org/> .

# Infer missing relationships
# If person works at company in city, person likely lives near city
{ ?p ex:worksAt ?company .
  ?company ex:headquarteredIn ?city }
    => { ?p ex:likelyLivesNear ?city } .

# If person authored paper with another, they likely know each other
{ ?p1 ex:authored ?paper .
  ?p2 ex:authored ?paper .
  ?p1 log:notEqualTo ?p2 }
    => { ?p1 ex:likelyKnows ?p2 } .
```

### Multi-File Pipeline

```bash
# Load data, apply transforms, run rules, query
cwm base-data.n3 \
    enrichment-rules.n3 \
    validation-rules.n3 \
    inference-rules.n3 \
    --think \
    --sparql-query "SELECT ?error WHERE { ?x ex:error ?error }" \
    --sparql-results json
```

### Batch Processing

```bash
# Process multiple files
for f in data/*.n3; do
  cwm "$f" rules.n3 --think --format ntriples >> all-inferences.nt
done

# Deduplicate
sort -u all-inferences.nt > unique-inferences.nt
```

### Differential Updates

```bash
# Compute what's new
cwm old-data.n3 --format ntriples | sort > old.nt
cwm new-data.n3 --format ntriples | sort > new.nt

# Additions
comm -13 old.nt new.nt > additions.nt

# Deletions
comm -23 old.nt new.nt > deletions.nt
```

---

## Performance Tips

### Large Datasets

```bash
# Use streaming mode
cwm --pipe large-data.n3 --think

# Limit inference steps
cwm data.n3 --max-steps 1000 --think

# Use performance profile
cwm data.n3 --profile performance --think
```

### Memory Optimization

```bash
# Use SQLite backend for large datasets
cwm data.n3 --db ./cache.db --think

# Use Fuseki for very large datasets
cwm --fuseki http://localhost:3030/dataset --think
```

### Query Optimization

```bash
# Enable caching for repeated queries
cwm data.n3 --sparqlServer 8080  # Caching enabled by default

# Use LIMIT for exploratory queries
cwm data.n3 --sparql-query "SELECT * WHERE { ?s ?p ?o } LIMIT 100"
```

---

## See Also

- [README.md](../README.md) - Quick start guide
- [BUILTINS.md](BUILTINS.md) - Built-in predicates
- [REASONING.md](REASONING.md) - Reasoning strategies
- [SPARQL.md](SPARQL.md) - SPARQL reference
- [CONFIGURATION.md](CONFIGURATION.md) - Configuration options
