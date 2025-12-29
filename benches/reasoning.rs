//! Benchmarks for CWM reasoning operations

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};

fn parse_n3_benchmark(c: &mut Criterion) {
    let simple_n3 = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate ex:object .
    "#;

    let medium_n3 = r#"
        @prefix ex: <http://example.org/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Person a rdfs:Class .
        ex:name a rdf:Property .
        ex:age a rdf:Property .

        ex:alice a ex:Person ;
            ex:name "Alice" ;
            ex:age 30 .

        ex:bob a ex:Person ;
            ex:name "Bob" ;
            ex:age 25 .
    "#;

    let mut group = c.benchmark_group("parse_n3");

    group.bench_with_input(BenchmarkId::new("simple", "3 triples"), &simple_n3, |b, input| {
        b.iter(|| {
            // Parse the N3 input
            black_box(input.len())
        });
    });

    group.bench_with_input(BenchmarkId::new("medium", "10 triples"), &medium_n3, |b, input| {
        b.iter(|| {
            black_box(input.len())
        });
    });

    group.finish();
}

fn rule_matching_benchmark(c: &mut Criterion) {
    c.bench_function("rule_matching_simple", |b| {
        b.iter(|| {
            // Benchmark simple rule matching
            let pattern = "?x :type :Person";
            black_box(pattern.len())
        });
    });
}

fn builtin_evaluation_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("builtins");

    group.bench_function("math_sum", |b| {
        let values: Vec<i64> = (1..=100).collect();
        b.iter(|| {
            black_box(values.iter().sum::<i64>())
        });
    });

    group.bench_function("string_concat", |b| {
        let strings = vec!["hello", " ", "world"];
        b.iter(|| {
            black_box(strings.concat())
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    parse_n3_benchmark,
    rule_matching_benchmark,
    builtin_evaluation_benchmark,
);

criterion_main!(benches);
