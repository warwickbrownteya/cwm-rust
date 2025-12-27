# CWM-Rust vs EYE Reasoner Test Report

**Date**: 2025-12-27
**CWM-Rust**: v0.1.0
**EYE**: v11.23.2

## Summary

| Test | Description | CWM-Rust | EYE | Match |
|------|-------------|----------|-----|-------|
| 01 | Basic triples | PASS | PASS | Yes |
| 02 | Simple rule (Human → Mortal) | PASS | PASS | Yes |
| 03 | Transitive closure (subClassOf) | PASS | PASS | Yes |
| 04 | Family relations (parent, Father, Mother) | PASS | PASS | Yes |
| 05 | Multi-pattern rule (knows → knowsIndirectly) | PASS | PASS | Yes |
| 06 | Type propagation (instance → superclass) | PASS | PASS | Yes |
| 07 | Blank nodes | PASS | PASS | Yes* |
| 08 | Collections/lists | PASS | PASS | Yes* |
| 09 | Symmetric relations | PASS | PASS | Yes |
| 10 | Chain rule (next → skip) | PASS | PASS | Yes |

**Result: 10/10 tests passing**

\* Minor formatting differences (blank node IDs, numeric literal syntax)

## Test Details

### Test 02: Simple Rule
```n3
{ ?x a ex:Human } => { ?x a ex:Mortal } .
```
Both reasoners correctly infer:
- `ex:socrates a ex:Mortal`
- `ex:plato a ex:Mortal`
- `ex:aristotle a ex:Mortal`

### Test 03: Transitive Closure
```n3
{ ?x rdfs:subClassOf ?y . ?y rdfs:subClassOf ?z } => { ?x rdfs:subClassOf ?z } .
```
Both reasoners correctly infer:
- `ex:Dog rdfs:subClassOf ex:Animal`
- `ex:Dog rdfs:subClassOf ex:LivingThing`
- `ex:Mammal rdfs:subClassOf ex:LivingThing`

### Test 04: Family Relations
```n3
{ ?x :hasChild ?y } => { ?y :hasParent ?x } .
{ ?x :hasChild ?y . ?x a :Male } => { ?x a :Father } .
{ ?x :hasChild ?y . ?x a :Female } => { ?x a :Mother } .
```
Both reasoners correctly infer:
- `:alice :hasParent :john`
- `:alice :hasParent :mary`
- `:bob :hasParent :john`
- `:bob :hasParent :mary`
- `:john a :Father`
- `:mary a :Mother`

### Test 05: Multi-Pattern Rule
```n3
{ ?x ex:knows ?y . ?y ex:knows ?z } => { ?x ex:knowsIndirectly ?z } .
```
Both reasoners correctly infer:
- `ex:alice ex:knowsIndirectly ex:charlie`
- `ex:bob ex:knowsIndirectly ex:david`

### Test 06: Type Propagation
```n3
{ ?x a ?class . ?class rdfs:subClassOf ?super } => { ?x a ?super } .
```
Both reasoners correctly infer:
- `ex:fido a ex:Animal`
- `ex:fido a ex:LivingThing`
- `ex:tweety a ex:Animal`
- `ex:tweety a ex:LivingThing`
- `ex:nemo a ex:Animal`
- `ex:nemo a ex:LivingThing`

### Test 09: Symmetric Relations
```n3
{ ?x ex:marriedTo ?y } => { ?y ex:marriedTo ?x } .
{ ?x ex:friendOf ?y } => { ?y ex:friendOf ?x } .
```
Both reasoners correctly infer:
- `ex:bob ex:marriedTo ex:alice`
- `ex:david ex:friendOf ex:charlie`

### Test 10: Chain Rule
```n3
{ ?x ex:next ?y . ?y ex:next ?z } => { ?x ex:skip ?z } .
```
Both reasoners correctly infer:
- `ex:a ex:skip ex:c`
- `ex:b ex:skip ex:d`
- `ex:c ex:skip ex:e`

## Minor Differences

### Blank Node IDs
CWM-Rust and EYE use different skolemization schemes for blank nodes:
- CWM-Rust: `_:b0`, `_:addr1`
- EYE: `_:e_2`, `_:e_addr1_1`

This is expected and correct behavior.

### Numeric Literals
- CWM-Rust: `"1"^^xsd:integer`
- EYE: `1`

Both are valid N3 representations.

## Conclusion

CWM-Rust produces **identical inferences** to EYE for all tested rule patterns:
- Simple rules with single pattern
- Multi-pattern rules
- Transitive closure rules
- Type propagation rules
- Symmetric relation rules
- Chain rules

The implementation is compatible with the EYE reasoner for core N3 reasoning tasks.
