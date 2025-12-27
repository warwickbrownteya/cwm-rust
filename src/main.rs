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

    /// Maximum number of inference steps (0 for unlimited)
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

    /// Apply rules from specified file(s)
    #[arg(long = "apply", value_name = "RULES")]
    apply: Vec<PathBuf>,

    /// Load rules from specified file(s)
    #[arg(long = "rules", value_name = "RULES")]
    rules: Vec<PathBuf>,

    /// Output strings only (values of matching patterns)
    #[arg(long)]
    strings: bool,

    /// Purge rules from output
    #[arg(long = "purge-rules")]
    purge_rules: bool,

    /// Purge triples with builtin predicates from output
    #[arg(long = "purge-builtins")]
    purge_builtins: bool,

    /// Operating mode: r=read, w=write, t=think, f=filter
    #[arg(long = "mode", value_name = "MODE")]
    mode: Option<String>,

    /// Data files to load (no rules extracted)
    #[arg(long = "data", value_name = "FILE")]
    data: Vec<PathBuf>,

    /// Flatten formula contents into main graph
    #[arg(long)]
    flatten: bool,

    /// Number of think passes (0 for unlimited)
    #[arg(long = "think-passes", value_name = "N")]
    think_passes: Option<usize>,

    /// Suppress all output
    #[arg(long)]
    no: bool,

    /// Purge statements with log:Chaff class
    #[arg(long)]
    purge: bool,

    /// Debug/chatty level (0-99, higher = more verbose)
    #[arg(long, value_name = "LEVEL", default_value = "0")]
    chatty: u8,

    /// Minimal formatting, fastest mode
    #[arg(long)]
    ugly: bool,

    /// Sort output by subject
    #[arg(long = "bySubject")]
    by_subject: bool,

    /// Pass remaining arguments as os:argv values
    #[arg(long, value_name = "ARGS", num_args = 0..)]
    with: Vec<String>,

    /// Pipe mode: process without storing intermediate results
    #[arg(long)]
    pipe: bool,

    /// Reify statements (convert to RDF reification)
    #[arg(long)]
    reify: bool,

    /// Dereify statements (reverse reification)
    #[arg(long)]
    dereify: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, ValueEnum)]
enum OutputFormat {
    /// N3/Turtle format
    N3,
    /// N-Triples format
    Ntriples,
    /// RDF/XML format
    Rdf,
    /// JSON-LD format
    Jsonld,
    /// Debug format
    Debug,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Collect all prefixes
    let mut all_prefixes: IndexMap<String, String> = IndexMap::new();
    let mut all_rules = Vec::new();

    // Collect input content
    let mut content = String::new();

    if cli.stdin || (cli.inputs.is_empty() && cli.data.is_empty() && cli.apply.is_empty() && cli.rules.is_empty()) {
        io::stdin().read_to_string(&mut content)
            .context("Failed to read from stdin")?;
    }

    for path in &cli.inputs {
        let file_content = fs::read_to_string(path)
            .with_context(|| format!("Failed to read file: {}", path.display()))?;
        content.push_str(&file_content);
        content.push('\n');
    }

    // Parse main input
    let parse_result = parse(&content)
        .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;

    let mut store = Store::new();
    store.add_all(parse_result.triples.clone());
    all_prefixes.extend(parse_result.prefixes.clone());
    all_rules.extend(parse_result.rules.clone());

    // Load data files (no rules extracted)
    for path in &cli.data {
        let file_content = fs::read_to_string(path)
            .with_context(|| format!("Failed to read data file: {}", path.display()))?;
        let data_result = parse(&file_content)
            .map_err(|e| anyhow::anyhow!("Parse error in {}: {}", path.display(), e))?;
        store.add_all(data_result.triples);
        all_prefixes.extend(data_result.prefixes);
        // Don't add rules from --data files
    }

    // Load rules from --rules files
    for path in &cli.rules {
        let file_content = fs::read_to_string(path)
            .with_context(|| format!("Failed to read rules file: {}", path.display()))?;
        let rules_result = parse(&file_content)
            .map_err(|e| anyhow::anyhow!("Parse error in {}: {}", path.display(), e))?;
        all_prefixes.extend(rules_result.prefixes);
        all_rules.extend(rules_result.rules);
        // Also add any triples from rules files
        store.add_all(rules_result.triples);
    }

    // Load rules from --apply files (same as --rules but implies --think)
    let should_think = cli.think || !cli.apply.is_empty();
    for path in &cli.apply {
        let file_content = fs::read_to_string(path)
            .with_context(|| format!("Failed to read apply file: {}", path.display()))?;
        let apply_result = parse(&file_content)
            .map_err(|e| anyhow::anyhow!("Parse error in {}: {}", path.display(), e))?;
        all_prefixes.extend(apply_result.prefixes);
        all_rules.extend(apply_result.rules);
        store.add_all(apply_result.triples);
    }

    // Track original triples for --filter mode
    let original_triples: std::collections::HashSet<Triple> = if cli.filter {
        store.iter().cloned().collect()
    } else {
        std::collections::HashSet::new()
    };

    if !cli.quiet && cli.verbose {
        eprintln!("Loaded {} triples", store.len());
        if !all_rules.is_empty() {
            eprintln!("Found {} rules", all_rules.len());
        }
    }

    // Run reasoning if requested
    if should_think {
        let max_steps = if cli.max_steps == 0 { usize::MAX } else { cli.max_steps };
        let config = ReasonerConfig {
            max_steps,
            recursive: true,
            filter: cli.filter,
        };

        let mut reasoner = Reasoner::with_config(config);

        // Add all rules
        for rule in all_rules {
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
    let mut output_store = if cli.filter && should_think {
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

    // Purge rules from output if requested
    if cli.purge_rules {
        let log_implies = "http://www.w3.org/2000/10/swap/log#implies";
        let mut filtered = Store::new();
        for triple in output_store.iter() {
            if let Term::Uri(uri) = &triple.predicate {
                if uri.as_str() != log_implies {
                    filtered.add(triple.clone());
                }
            } else {
                filtered.add(triple.clone());
            }
        }
        output_store = filtered;
    }

    // Purge log:Chaff triples if requested
    if cli.purge {
        let log_chaff = "http://www.w3.org/2000/10/swap/log#Chaff";
        let rdf_type = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

        // Find all subjects that are typed as log:Chaff
        let chaff_subjects: std::collections::HashSet<String> = output_store.iter()
            .filter(|t| {
                if let (Term::Uri(pred), Term::Uri(obj)) = (&t.predicate, &t.object) {
                    pred.as_str() == rdf_type && obj.as_str() == log_chaff
                } else {
                    false
                }
            })
            .map(|t| format!("{:?}", t.subject))
            .collect();

        // Remove triples with chaff subjects
        let mut filtered = Store::new();
        for triple in output_store.iter() {
            let subject_key = format!("{:?}", triple.subject);
            if !chaff_subjects.contains(&subject_key) {
                filtered.add(triple.clone());
            }
        }
        output_store = filtered;
    }

    // Purge builtins from output if requested
    if cli.purge_builtins {
        let builtin_namespaces = [
            "http://www.w3.org/2000/10/swap/math#",
            "http://www.w3.org/2000/10/swap/string#",
            "http://www.w3.org/2000/10/swap/log#",
            "http://www.w3.org/2000/10/swap/list#",
            "http://www.w3.org/2000/10/swap/crypto#",
            "http://www.w3.org/2000/10/swap/time#",
            "http://www.w3.org/2000/10/swap/os#",
            "http://www.w3.org/2000/10/swap/graph#",
        ];
        let mut filtered = Store::new();
        for triple in output_store.iter() {
            if let Term::Uri(uri) = &triple.predicate {
                let uri_str = uri.as_str();
                let is_builtin = builtin_namespaces.iter().any(|ns| uri_str.starts_with(ns));
                if !is_builtin {
                    filtered.add(triple.clone());
                }
            } else {
                filtered.add(triple.clone());
            }
        }
        output_store = filtered;
    }

    // Apply reification if requested
    if cli.reify {
        output_store = reify_store(&output_store);
    }

    // Apply dereification if requested
    if cli.dereify {
        output_store = dereify_store(&output_store);
    }

    // Suppress output if --no flag is set
    if cli.no {
        return Ok(());
    }

    // Generate output
    let output_content = if cli.strings {
        // --strings mode: output literal values only
        let mut strings = Vec::new();
        for triple in output_store.iter() {
            if let Term::Literal(lit) = &triple.object {
                strings.push(lit.value().to_string());
            }
        }
        strings.join("\n") + if strings.is_empty() { "" } else { "\n" }
    } else if cli.ugly {
        // Ugly mode: minimal formatting, fastest
        format_ntriples(&output_store)
    } else {
        match cli.format {
            OutputFormat::N3 => format_n3(&output_store, &all_prefixes),
            OutputFormat::Ntriples => format_ntriples(&output_store),
            OutputFormat::Rdf => format_rdfxml(&output_store, &all_prefixes),
            OutputFormat::Jsonld => format_jsonld(&output_store, &all_prefixes),
            OutputFormat::Debug => format!("{:?}", output_store),
        }
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

/// Format store as RDF/XML
fn format_rdfxml(store: &Store, prefixes: &IndexMap<String, String>) -> String {
    let mut output = String::new();

    output.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    output.push_str("<rdf:RDF\n");
    output.push_str("    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n");

    // Add namespace declarations
    for (short, long) in prefixes {
        if short != "rdf" {
            output.push_str(&format!("    xmlns:{}=\"{}\"\n", short, long));
        }
    }
    output.push_str(">\n\n");

    // Group triples by subject
    let mut by_subject: IndexMap<String, Vec<&Triple>> = IndexMap::new();
    for triple in store.iter() {
        let key = format!("{:?}", triple.subject);
        by_subject.entry(key).or_default().push(triple);
    }

    for (_, triples) in by_subject {
        if triples.is_empty() {
            continue;
        }

        let subject = &triples[0].subject;

        // Start rdf:Description
        match subject {
            Term::Uri(u) => {
                output.push_str(&format!("  <rdf:Description rdf:about=\"{}\">\n", escape_xml(u.as_str())));
            }
            Term::BlankNode(b) => {
                output.push_str(&format!("  <rdf:Description rdf:nodeID=\"{}\">\n", b.id()));
            }
            _ => continue, // Skip non-resource subjects
        }

        // Output predicates and objects
        for triple in &triples {
            if let Term::Uri(pred_uri) = &triple.predicate {
                let pred_str = pred_uri.as_str();

                // Try to split into prefix:local
                let (ns, local) = split_uri(pred_str);

                // Find prefix for namespace
                let prefix = prefixes.iter()
                    .find(|(_, v)| v.as_str() == ns)
                    .map(|(k, _)| k.as_str());

                let pred_qname = if let Some(p) = prefix {
                    format!("{}:{}", p, local)
                } else if ns == "http://www.w3.org/1999/02/22-rdf-syntax-ns#" {
                    format!("rdf:{}", local)
                } else {
                    // Use full URI as element name (not ideal but valid)
                    continue;
                };

                match &triple.object {
                    Term::Uri(u) => {
                        output.push_str(&format!(
                            "    <{} rdf:resource=\"{}\"/>\n",
                            pred_qname,
                            escape_xml(u.as_str())
                        ));
                    }
                    Term::Literal(lit) => {
                        match lit.datatype() {
                            Datatype::Plain => {
                                output.push_str(&format!(
                                    "    <{}>{}</{}>\n",
                                    pred_qname,
                                    escape_xml(lit.value()),
                                    pred_qname
                                ));
                            }
                            Datatype::Language(lang) => {
                                output.push_str(&format!(
                                    "    <{} xml:lang=\"{}\">{}</{}>\n",
                                    pred_qname,
                                    lang,
                                    escape_xml(lit.value()),
                                    pred_qname
                                ));
                            }
                            Datatype::Typed(dt) => {
                                output.push_str(&format!(
                                    "    <{} rdf:datatype=\"{}\">{}</{}>\n",
                                    pred_qname,
                                    escape_xml(dt),
                                    escape_xml(lit.value()),
                                    pred_qname
                                ));
                            }
                        }
                    }
                    Term::BlankNode(b) => {
                        output.push_str(&format!(
                            "    <{} rdf:nodeID=\"{}\"/>\n",
                            pred_qname,
                            b.id()
                        ));
                    }
                    _ => {} // Skip complex objects
                }
            }
        }

        output.push_str("  </rdf:Description>\n\n");
    }

    output.push_str("</rdf:RDF>\n");
    output
}

/// Escape special XML characters
fn escape_xml(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

/// Split a URI into namespace and local name
fn split_uri(uri: &str) -> (&str, &str) {
    if let Some(pos) = uri.rfind('#') {
        (&uri[..=pos], &uri[pos + 1..])
    } else if let Some(pos) = uri.rfind('/') {
        (&uri[..=pos], &uri[pos + 1..])
    } else {
        (uri, "")
    }
}

/// Reify a store (convert statements to RDF reification)
fn reify_store(store: &Store) -> Store {
    let mut result = Store::new();
    let rdf_type = Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
    let rdf_statement = Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement");
    let rdf_subject = Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#subject");
    let rdf_predicate = Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate");
    let rdf_object = Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#object");

    for (i, triple) in store.iter().enumerate() {
        // Create a blank node for this statement
        let stmt_node = Term::BlankNode(cwm::BlankNode::labeled(format!("stmt{}", i)));

        // Add the reification triples
        result.add(Triple::new(stmt_node.clone(), rdf_type.clone(), rdf_statement.clone()));
        result.add(Triple::new(stmt_node.clone(), rdf_subject.clone(), triple.subject.clone()));
        result.add(Triple::new(stmt_node.clone(), rdf_predicate.clone(), triple.predicate.clone()));
        result.add(Triple::new(stmt_node.clone(), rdf_object.clone(), triple.object.clone()));
    }

    result
}

/// Dereify a store (reverse reification)
fn dereify_store(store: &Store) -> Store {
    let mut result = Store::new();
    let rdf_type_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
    let rdf_statement_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement";
    let rdf_subject_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject";
    let rdf_predicate_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate";
    let rdf_object_uri = "http://www.w3.org/1999/02/22-rdf-syntax-ns#object";

    // Find all statement nodes
    let mut statement_nodes: std::collections::HashSet<String> = std::collections::HashSet::new();
    for triple in store.iter() {
        if let (Term::Uri(pred), Term::Uri(obj)) = (&triple.predicate, &triple.object) {
            if pred.as_str() == rdf_type_uri && obj.as_str() == rdf_statement_uri {
                statement_nodes.insert(format!("{:?}", triple.subject));
            }
        }
    }

    // Collect subject/predicate/object for each statement node
    let mut subjects: HashMap<String, Term> = HashMap::new();
    let mut predicates: HashMap<String, Term> = HashMap::new();
    let mut objects: HashMap<String, Term> = HashMap::new();

    for triple in store.iter() {
        let subj_key = format!("{:?}", triple.subject);
        if statement_nodes.contains(&subj_key) {
            if let Term::Uri(pred) = &triple.predicate {
                let pred_str = pred.as_str();
                if pred_str == rdf_subject_uri {
                    subjects.insert(subj_key.clone(), triple.object.clone());
                } else if pred_str == rdf_predicate_uri {
                    predicates.insert(subj_key.clone(), triple.object.clone());
                } else if pred_str == rdf_object_uri {
                    objects.insert(subj_key.clone(), triple.object.clone());
                }
            }
        }
    }

    // Reconstruct original triples
    for stmt_key in &statement_nodes {
        if let (Some(s), Some(p), Some(o)) = (subjects.get(stmt_key), predicates.get(stmt_key), objects.get(stmt_key)) {
            result.add(Triple::new(s.clone(), p.clone(), o.clone()));
        }
    }

    // Also add non-reification triples
    for triple in store.iter() {
        let subj_key = format!("{:?}", triple.subject);
        if !statement_nodes.contains(&subj_key) {
            // Check if this triple is part of reification pattern
            if let Term::Uri(pred) = &triple.predicate {
                let pred_str = pred.as_str();
                if pred_str != rdf_subject_uri && pred_str != rdf_predicate_uri &&
                   pred_str != rdf_object_uri && !(pred_str == rdf_type_uri &&
                   matches!(&triple.object, Term::Uri(u) if u.as_str() == rdf_statement_uri)) {
                    result.add(triple.clone());
                }
            } else {
                result.add(triple.clone());
            }
        }
    }

    result
}

/// Format store as JSON-LD
fn format_jsonld(store: &Store, prefixes: &IndexMap<String, String>) -> String {
    let mut output = String::new();
    output.push_str("{\n");

    // Add context
    output.push_str("  \"@context\": {\n");
    let mut context_entries = Vec::new();
    for (short, long) in prefixes {
        context_entries.push(format!("    \"{}\": \"{}\"", short, long));
    }
    output.push_str(&context_entries.join(",\n"));
    if !context_entries.is_empty() {
        output.push('\n');
    }
    output.push_str("  },\n");

    // Group triples by subject
    let mut by_subject: IndexMap<String, Vec<&Triple>> = IndexMap::new();
    for triple in store.iter() {
        let key = format!("{:?}", triple.subject);
        by_subject.entry(key).or_default().push(triple);
    }

    output.push_str("  \"@graph\": [\n");

    let mut first_subject = true;
    for (_, triples) in by_subject {
        if triples.is_empty() {
            continue;
        }

        if !first_subject {
            output.push_str(",\n");
        }
        first_subject = false;

        output.push_str("    {\n");

        // Output @id
        let subject = &triples[0].subject;
        match subject {
            Term::Uri(u) => {
                output.push_str(&format!("      \"@id\": \"{}\",\n", u.as_str()));
            }
            Term::BlankNode(b) => {
                output.push_str(&format!("      \"@id\": \"_:{}\",\n", b.id()));
            }
            _ => {}
        }

        // Group by predicate
        let mut by_pred: IndexMap<String, Vec<&Term>> = IndexMap::new();
        for triple in &triples {
            if let Term::Uri(pred) = &triple.predicate {
                by_pred.entry(pred.as_str().to_string()).or_default().push(&triple.object);
            }
        }

        let pred_count = by_pred.len();
        for (i, (pred_uri, objs)) in by_pred.iter().enumerate() {
            let pred_key = if pred_uri == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" {
                "@type".to_string()
            } else {
                format!("\"{}\"", pred_uri)
            };

            output.push_str(&format!("      {}: ", pred_key));

            if objs.len() == 1 {
                output.push_str(&format_jsonld_value(objs[0]));
            } else {
                output.push_str("[\n");
                for (j, obj) in objs.iter().enumerate() {
                    output.push_str(&format!("        {}", format_jsonld_value(obj)));
                    if j < objs.len() - 1 {
                        output.push(',');
                    }
                    output.push('\n');
                }
                output.push_str("      ]");
            }

            if i < pred_count - 1 {
                output.push(',');
            }
            output.push('\n');
        }

        output.push_str("    }");
    }

    output.push_str("\n  ]\n");
    output.push_str("}\n");

    output
}

/// Format a term as JSON-LD value
fn format_jsonld_value(term: &Term) -> String {
    match term {
        Term::Uri(u) => format!("\"{}\"", u.as_str()),
        Term::Literal(lit) => {
            match lit.datatype() {
                Datatype::Plain => format!("\"{}\"", escape_json(lit.value())),
                Datatype::Language(lang) => {
                    format!("{{ \"@value\": \"{}\", \"@language\": \"{}\" }}", escape_json(lit.value()), lang)
                }
                Datatype::Typed(dt) => {
                    // Check for native JSON types
                    if dt == "http://www.w3.org/2001/XMLSchema#integer" ||
                       dt == "http://www.w3.org/2001/XMLSchema#decimal" ||
                       dt == "http://www.w3.org/2001/XMLSchema#double" {
                        lit.value().to_string()
                    } else if dt == "http://www.w3.org/2001/XMLSchema#boolean" {
                        lit.value().to_string()
                    } else {
                        format!("{{ \"@value\": \"{}\", \"@type\": \"{}\" }}", escape_json(lit.value()), dt)
                    }
                }
            }
        }
        Term::BlankNode(b) => format!("{{ \"@id\": \"_:{}\" }}", b.id()),
        _ => "null".to_string(),
    }
}

/// Escape special JSON characters
fn escape_json(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}
