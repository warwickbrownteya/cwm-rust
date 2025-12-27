//! CWM - Closed World Machine
//!
//! Command-line interface for N3 reasoning.

use std::fs;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::collections::HashMap;

use anyhow::{Context, Result};
use clap::{Parser, ValueEnum};
use indexmap::IndexMap;

use cwm::{Store, Reasoner, ReasonerConfig, parse, Term, Triple, FormulaRef, Literal, Datatype};

#[derive(Parser)]
#[command(name = "cwm")]
#[command(author = "CWM Rust Authors")]
#[command(version = "0.1.0")]
#[command(about = "Closed World Machine - N3 reasoner and RDF processor", long_about = None)]
struct Cli {
    /// Input files to process
    #[arg(value_name = "FILE")]
    inputs: Vec<PathBuf>,

    /// Read input from stdin
    #[arg(long)]
    stdin: bool,

    /// Apply rules (forward-chaining inference)
    #[arg(long)]
    think: bool,

    /// Output only inferred triples (requires --think)
    #[arg(long)]
    filter: bool,

    /// Maximum number of inference steps
    #[arg(long, default_value = "10000")]
    max_steps: usize,

    /// Output format
    #[arg(short, long, value_enum, default_value = "n3")]
    format: OutputFormat,

    /// Base URI for relative references
    #[arg(long)]
    base: Option<String>,

    /// Output file (defaults to stdout)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,

    /// Quiet mode (suppress info messages)
    #[arg(short, long)]
    quiet: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, ValueEnum)]
enum OutputFormat {
    /// N3/Turtle format
    N3,
    /// N-Triples format
    Ntriples,
    /// Debug format
    Debug,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Collect input content
    let mut content = String::new();

    if cli.stdin || cli.inputs.is_empty() {
        io::stdin().read_to_string(&mut content)
            .context("Failed to read from stdin")?;
    }

    for path in &cli.inputs {
        let file_content = fs::read_to_string(path)
            .with_context(|| format!("Failed to read file: {}", path.display()))?;
        content.push_str(&file_content);
        content.push('\n');
    }

    // Parse input
    let parse_result = parse(&content)
        .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;

    let mut store = Store::new();
    store.add_all(parse_result.triples.clone());

    // Track original triples for --filter mode
    let original_triples: std::collections::HashSet<Triple> = if cli.filter {
        parse_result.triples.iter().cloned().collect()
    } else {
        std::collections::HashSet::new()
    };

    if !cli.quiet && cli.verbose {
        eprintln!("Loaded {} triples", store.len());
        if !parse_result.rules.is_empty() {
            eprintln!("Found {} rules", parse_result.rules.len());
        }
    }

    // Run reasoning if requested
    if cli.think {
        let config = ReasonerConfig {
            max_steps: cli.max_steps,
            recursive: true,
            filter: cli.filter,
        };

        let mut reasoner = Reasoner::with_config(config);

        // Add rules from the parsed input
        for rule in parse_result.rules {
            reasoner.add_rule(rule);
        }

        let stats = reasoner.run(&mut store);

        if !cli.quiet && cli.verbose {
            eprintln!(
                "Reasoning: {} steps, {} rules fired, {} triples derived",
                stats.steps, stats.rules_fired, stats.triples_derived
            );
        }
    } else if cli.filter {
        // --filter without --think: warn user
        eprintln!("Warning: --filter has no effect without --think");
    }

    // Apply filter if requested: output only inferred triples
    let output_store = if cli.filter && cli.think {
        let mut filtered = Store::new();
        for triple in store.iter() {
            if !original_triples.contains(triple) {
                filtered.add(triple.clone());
            }
        }
        filtered
    } else {
        store
    };

    // Generate output
    let output_content = match cli.format {
        OutputFormat::N3 => format_n3(&output_store, &parse_result.prefixes),
        OutputFormat::Ntriples => format_ntriples(&output_store),
        OutputFormat::Debug => format!("{:?}", output_store),
    };

    // Write output
    if let Some(output_path) = cli.output {
        fs::write(&output_path, output_content)
            .with_context(|| format!("Failed to write to: {}", output_path.display()))?;
    } else {
        io::stdout().write_all(output_content.as_bytes())
            .context("Failed to write to stdout")?;
    }

    Ok(())
}

/// N3 Formatter with prefix support
struct N3Formatter<'a> {
    /// Reverse lookup: URI prefix -> short prefix
    reverse: HashMap<&'a str, &'a str>,
}

impl<'a> N3Formatter<'a> {
    fn new(prefixes: &'a IndexMap<String, String>) -> Self {
        let mut reverse = HashMap::new();
        for (short, long) in prefixes {
            reverse.insert(long.as_str(), short.as_str());
        }
        N3Formatter { reverse }
    }

    /// Try to compact a URI using known prefixes
    fn compact_uri(&self, uri: &str) -> String {
        // Check common prefixes first
        const RDF: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
        const RDFS: &str = "http://www.w3.org/2000/01/rdf-schema#";
        const XSD: &str = "http://www.w3.org/2001/XMLSchema#";
        const OWL: &str = "http://www.w3.org/2002/07/owl#";
        const LOG: &str = "http://www.w3.org/2000/10/swap/log#";

        // Check for rdf:type shorthand
        if uri == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" {
            return "a".to_string();
        }

        // Try user-defined prefixes first
        for (long, short) in &self.reverse {
            if uri.starts_with(*long) {
                let local = &uri[long.len()..];
                if is_valid_local_name(local) {
                    return format!("{}:{}", short, local);
                }
            }
        }

        // Try well-known prefixes
        let well_known = [
            (RDF, "rdf"),
            (RDFS, "rdfs"),
            (XSD, "xsd"),
            (OWL, "owl"),
            (LOG, "log"),
        ];

        for (prefix_uri, prefix_name) in well_known {
            if uri.starts_with(prefix_uri) {
                let local = &uri[prefix_uri.len()..];
                if is_valid_local_name(local) {
                    return format!("{}:{}", prefix_name, local);
                }
            }
        }

        // Fall back to full URI
        format!("<{}>", uri)
    }

    /// Format a term
    fn format_term(&self, term: &Term) -> String {
        match term {
            Term::Uri(u) => self.compact_uri(u.as_str()),
            Term::Literal(l) => self.format_literal(l),
            Term::BlankNode(b) => format!("{}", b),
            Term::Variable(v) => format!("{}", v),
            Term::List(l) => self.format_list(l),
            Term::Formula(f) => self.format_formula(f),
        }
    }

    /// Format a literal with compact datatype
    fn format_literal(&self, lit: &Literal) -> String {
        match lit.datatype() {
            Datatype::Plain => format!("\"{}\"", lit.value()),
            Datatype::Language(lang) => format!("\"{}\"@{}", lit.value(), lang),
            Datatype::Typed(dt) => {
                let compact_dt = self.compact_uri(dt);
                format!("\"{}\"^^{}", lit.value(), compact_dt)
            }
        }
    }

    /// Format a list
    fn format_list(&self, list: &cwm::List) -> String {
        let items: Vec<String> = list.iter().map(|t| self.format_term(t)).collect();
        format!("({})", items.join(" "))
    }

    /// Format a formula
    fn format_formula(&self, formula: &FormulaRef) -> String {
        let mut parts = Vec::new();
        for triple in formula.triples() {
            parts.push(format!(
                "{} {} {}",
                self.format_term(&triple.subject),
                self.format_term(&triple.predicate),
                self.format_term(&triple.object)
            ));
        }
        format!("{{ {} }}", parts.join(" . "))
    }
}

/// Check if a string is a valid local name for prefixed URIs
fn is_valid_local_name(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let mut chars = s.chars();
    let first = chars.next().unwrap();
    if !first.is_alphabetic() && first != '_' {
        return false;
    }
    chars.all(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == '.')
}

/// Format store as N3/Turtle with prefix declarations
fn format_n3(store: &Store, prefixes: &IndexMap<String, String>) -> String {
    let mut output = String::new();
    let formatter = N3Formatter::new(prefixes);

    // Find which prefixes are actually used
    let mut used_namespaces: std::collections::HashSet<String> = std::collections::HashSet::new();
    for triple in store.iter() {
        collect_used_namespaces(&triple.subject, prefixes, &mut used_namespaces);
        collect_used_namespaces(&triple.predicate, prefixes, &mut used_namespaces);
        collect_used_namespaces(&triple.object, prefixes, &mut used_namespaces);
    }

    // Output only used prefix declarations (in order)
    let mut output_prefixes = Vec::new();
    for (short, long) in prefixes {
        if used_namespaces.contains(long) {
            output_prefixes.push((short.clone(), long.clone()));
        }
    }

    for (short, long) in &output_prefixes {
        output.push_str(&format!("@prefix {}: <{}> .\n", short, long));
    }

    if !output_prefixes.is_empty() {
        output.push('\n');
    }

    // Group by subject for pretty output
    let mut by_subject: IndexMap<String, Vec<&Triple>> = IndexMap::new();

    for triple in store.iter() {
        let key = formatter.format_term(&triple.subject);
        by_subject.entry(key).or_default().push(triple);
    }

    for (subject, triples) in by_subject {
        if triples.len() == 1 {
            let t = triples[0];
            output.push_str(&format!(
                "{} {} {} .\n",
                subject,
                formatter.format_term(&t.predicate),
                formatter.format_term(&t.object)
            ));
        } else {
            // Pretty print with ; separator
            output.push_str(&format!("{}\n", subject));
            for (i, t) in triples.iter().enumerate() {
                let sep = if i < triples.len() - 1 { " ;" } else { " ." };
                output.push_str(&format!(
                    "    {} {}{}\n",
                    formatter.format_term(&t.predicate),
                    formatter.format_term(&t.object),
                    sep
                ));
            }
        }
    }

    output
}

/// Collect namespaces used by a term
fn collect_used_namespaces(term: &Term, prefixes: &IndexMap<String, String>, used: &mut std::collections::HashSet<String>) {
    match term {
        Term::Uri(u) => {
            let uri = u.as_str();
            for ns in prefixes.values() {
                if uri.starts_with(ns) {
                    used.insert(ns.clone());
                    break;
                }
            }
        }
        Term::Literal(lit) => {
            // Check datatype URI
            if let Some(dt_uri) = lit.datatype_uri() {
                for ns in prefixes.values() {
                    if dt_uri.starts_with(ns) {
                        used.insert(ns.clone());
                        break;
                    }
                }
            }
        }
        Term::Formula(f) => {
            for triple in f.triples() {
                collect_used_namespaces(&triple.subject, prefixes, used);
                collect_used_namespaces(&triple.predicate, prefixes, used);
                collect_used_namespaces(&triple.object, prefixes, used);
            }
        }
        Term::List(l) => {
            for item in l.iter() {
                collect_used_namespaces(item, prefixes, used);
            }
        }
        _ => {}
    }
}

/// Format store as N-Triples (always uses full URIs)
fn format_ntriples(store: &Store) -> String {
    let mut output = String::new();
    for triple in store.iter() {
        output.push_str(&format!("{} {} {} .\n", triple.subject, triple.predicate, triple.object));
    }
    output
}
