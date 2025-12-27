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

use cwm::{Store, Reasoner, ReasonerConfig, parse, Term, Triple, FormulaRef, Literal, Datatype, execute_sparql, QueryResult, format_results_xml, format_results_json, Rule};

/// N3 output formatting options
#[derive(Default, Clone)]
struct N3Options {
    /// Use anonymous blank node syntax (_: convention)
    anon_bnodes: bool,
    /// Don't use default namespace
    no_default_ns: bool,
    /// Escape unicode using \u notation
    escape_unicode: bool,
    /// Use identifiers from store
    use_store_ids: bool,
    /// Don't use list syntax (..)
    no_list_syntax: bool,
    /// No numeric syntax - use strings with ^^
    no_numeric: bool,
    /// No prefix - always use URIs in <>
    no_prefix: bool,
    /// No relative URIs
    no_relative: bool,
    /// Explicit subject for every statement (no ; shorthand)
    explicit_subject: bool,
    /// No special syntax (= and ())
    no_special: bool,
}

impl N3Options {
    fn from_flags(flags: &str) -> Self {
        let mut opts = N3Options::default();
        for c in flags.chars() {
            match c {
                'a' => opts.anon_bnodes = true,
                'd' => opts.no_default_ns = true,
                'e' => opts.escape_unicode = true,
                'i' => opts.use_store_ids = true,
                'l' => opts.no_list_syntax = true,
                'n' => opts.no_numeric = true,
                'p' => opts.no_prefix = true,
                'r' => opts.no_relative = true,
                's' => opts.explicit_subject = true,
                't' => opts.no_special = true,
                _ => {}
            }
        }
        opts
    }
}

/// RDF/XML output formatting options (for future --rdf= flag support)
#[allow(dead_code)]
#[derive(Default, Clone)]
struct RdfXmlOptions {
    /// Don't use nodeIDs for blank nodes
    no_node_ids: bool,
    /// Don't use elements as class names
    no_class_elements: bool,
    /// No default namespace
    no_default_ns: bool,
    /// Don't use RDF collection syntax
    no_collection: bool,
    /// No relative URIs
    no_relative: bool,
    /// Allow relative URIs for namespaces
    allow_relative_ns: bool,
    // Input flags
    /// Strict spec - unknown parse type as Literal
    strict: bool,
    /// Transparent foreign XML
    transparent_xml: bool,
    /// Local namespace for non-prefixed attrs
    local_attrs: bool,
    /// Default namespace as local
    default_local: bool,
    /// Don't require outer rdf:RDF
    no_rdf_root: bool,
}

impl RdfXmlOptions {
    #[allow(dead_code)]
    fn from_flags(flags: &str) -> Self {
        let mut opts = RdfXmlOptions::default();
        for c in flags.chars() {
            match c {
                'b' => opts.no_node_ids = true,
                'c' => opts.no_class_elements = true,
                'd' => opts.no_default_ns = true,
                'l' => opts.no_collection = true,
                'r' => opts.no_relative = true,
                'z' => opts.allow_relative_ns = true,
                'S' => opts.strict = true,
                'T' => opts.transparent_xml = true,
                'L' => opts.local_attrs = true,
                'D' => opts.default_local = true,
                'R' => opts.no_rdf_root = true,
                _ => {}
            }
        }
        opts
    }
}

/// A proof step explaining how a triple was derived
#[derive(Clone, Debug)]
struct ProofStep {
    /// The derived triple
    triple: Triple,
    /// The rule that produced it (if any)
    rule: Option<usize>,
    /// The bindings used (for future detailed proof output)
    #[allow(dead_code)]
    bindings: Vec<(String, Term)>,
    /// The antecedent triples that matched
    antecedents: Vec<Triple>,
}

/// Proof trace for --why option
#[derive(Default)]
struct ProofTrace {
    steps: Vec<ProofStep>,
}

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

    /// Execute SPARQL query from file
    #[arg(long = "sparql", value_name = "QUERY_FILE")]
    sparql: Option<PathBuf>,

    /// Execute inline SPARQL query
    #[arg(long = "sparql-query", value_name = "QUERY")]
    sparql_query: Option<String>,

    /// Output format for SPARQL results: xml, json
    #[arg(long = "sparql-results", value_name = "FORMAT", default_value = "xml")]
    sparql_results: String,

    /// N3 output flags: a=anon bnodes, d=no default ns, e=escape unicode,
    /// i=use store ids, l=no list syntax, n=no numeric, p=no prefix,
    /// r=no relative URIs, s=explicit subject, t=no special syntax
    #[arg(long = "n3", value_name = "FLAGS")]
    n3_flags: Option<String>,

    /// RDF/XML output flags: b=no nodeIDs, c=no class elements, d=no default ns,
    /// l=no collection, r=no relative URIs, z=allow relative ns
    /// RDF/XML input flags: S=strict, T=transparent XML, L=local attrs, D=default local, R=no rdf:RDF required
    #[arg(long = "rdf", value_name = "FLAGS")]
    rdf_flags: Option<String>,

    /// Generate proof/explanation for inferences
    #[arg(long)]
    why: bool,

    /// Closure flags for automatic imports: s=subject, p=predicate, o=object,
    /// t=type, i=imports, r=rules, E=errors, n=no
    #[arg(long = "closure", value_name = "FLAGS")]
    closure: Option<String>,

    /// Apply graph patch file (insertions and deletions)
    #[arg(long = "patch", value_name = "FILE")]
    patch: Option<PathBuf>,

    /// Language for input/output (n3, rdf, ntriples)
    #[arg(long = "language", value_name = "LANG")]
    language: Option<String>,

    /// Start SPARQL HTTP endpoint on specified port (default: 8000)
    #[arg(long = "sparqlServer", value_name = "PORT")]
    sparql_server: Option<u16>,

    /// Execute N3QL query from file (N3 pattern matching)
    #[arg(long = "query", value_name = "FILE")]
    n3ql_query: Option<PathBuf>,
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

    // Apply patch file if specified (insertions and deletions)
    if let Some(patch_path) = &cli.patch {
        let patch_content = fs::read_to_string(patch_path)
            .with_context(|| format!("Failed to read patch file: {}", patch_path.display()))?;
        apply_patch(&mut store, &patch_content, &mut all_prefixes)?;
    }

    // Handle closure flags for automatic imports
    if let Some(closure_flags) = &cli.closure {
        load_closure(&mut store, &mut all_prefixes, &mut all_rules, closure_flags, cli.verbose && !cli.quiet)?;
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
    let mut proof_trace = ProofTrace::default();
    if should_think {
        let max_steps = if cli.max_steps == 0 { usize::MAX } else { cli.max_steps };
        let config = ReasonerConfig {
            max_steps,
            recursive: true,
            filter: cli.filter,
        };

        let mut reasoner = Reasoner::with_config(config);

        // Add all rules
        for rule in &all_rules {
            reasoner.add_rule(rule.clone());
        }

        // If --why is set, we track derivations
        if cli.why {
            let stats = run_with_proof_tracking(&mut reasoner, &mut store, &all_rules, &mut proof_trace);
            if !cli.quiet && cli.verbose {
                eprintln!(
                    "Reasoning: {} steps, {} rules fired, {} triples derived",
                    stats.steps, stats.rules_fired, stats.triples_derived
                );
            }
        } else {
            let stats = reasoner.run(&mut store);
            if !cli.quiet && cli.verbose {
                eprintln!(
                    "Reasoning: {} steps, {} rules fired, {} triples derived",
                    stats.steps, stats.rules_fired, stats.triples_derived
                );
            }
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

    // Execute SPARQL query if specified
    if cli.sparql.is_some() || cli.sparql_query.is_some() {
        let query_str = if let Some(query_file) = &cli.sparql {
            fs::read_to_string(query_file)
                .with_context(|| format!("Failed to read SPARQL query file: {}", query_file.display()))?
        } else {
            cli.sparql_query.clone().unwrap()
        };

        let result = execute_sparql(&output_store, &query_str)
            .map_err(|e| anyhow::anyhow!("SPARQL error: {}", e))?;

        // Format and output SPARQL results
        let sparql_output = match cli.sparql_results.to_lowercase().as_str() {
            "json" => format_results_json(&result),
            _ => format_results_xml(&result),
        };

        // Handle CONSTRUCT queries - output as RDF
        if let QueryResult::Graph(triples) = &result {
            let mut result_store = Store::new();
            for triple in triples {
                result_store.add(triple.clone());
            }
            let rdf_output = format_n3(&result_store, &all_prefixes);
            if let Some(output_path) = cli.output {
                fs::write(&output_path, rdf_output)
                    .with_context(|| format!("Failed to write to: {}", output_path.display()))?;
            } else {
                io::stdout().write_all(rdf_output.as_bytes())
                    .context("Failed to write to stdout")?;
            }
        } else {
            if let Some(output_path) = cli.output {
                fs::write(&output_path, sparql_output)
                    .with_context(|| format!("Failed to write to: {}", output_path.display()))?;
            } else {
                io::stdout().write_all(sparql_output.as_bytes())
                    .context("Failed to write to stdout")?;
            }
        }

        return Ok(());
    }

    // Execute N3QL query if specified
    if let Some(query_path) = &cli.n3ql_query {
        let query_content = fs::read_to_string(query_path)
            .with_context(|| format!("Failed to read N3QL query file: {}", query_path.display()))?;

        let result = execute_n3ql(&output_store, &query_content, &all_prefixes)?;
        let result_output = format_n3(&result, &all_prefixes);

        if let Some(output_path) = &cli.output {
            fs::write(output_path, &result_output)
                .with_context(|| format!("Failed to write N3QL result to: {}", output_path.display()))?;
        } else {
            io::stdout().write_all(result_output.as_bytes())
                .context("Failed to write N3QL result to stdout")?;
        }

        return Ok(());
    }

    // Start SPARQL server if requested
    if let Some(port) = cli.sparql_server {
        let port = if port == 0 { 8000 } else { port };
        return run_sparql_server(&output_store, &all_prefixes, port);
    }

    // Suppress output if --no flag is set
    if cli.no {
        return Ok(());
    }

    // Output proof trace if --why was specified
    if cli.why && !proof_trace.steps.is_empty() {
        let proof_output = format_proof(&proof_trace, &all_prefixes);
        if let Some(output_path) = &cli.output {
            fs::write(output_path, &proof_output)
                .with_context(|| format!("Failed to write proof to: {}", output_path.display()))?;
        } else {
            io::stdout().write_all(proof_output.as_bytes())
                .context("Failed to write proof to stdout")?;
        }
        return Ok(());
    }

    // Parse N3 options if specified
    let n3_opts = cli.n3_flags.as_ref().map(|f| N3Options::from_flags(f));

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
            OutputFormat::N3 => {
                if let Some(opts) = &n3_opts {
                    format_n3_with_options(&output_store, &all_prefixes, opts)
                } else {
                    format_n3(&output_store, &all_prefixes)
                }
            }
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

/// Apply a patch file to the store
/// Patch format: lines starting with + are insertions, - are deletions
fn apply_patch(store: &mut Store, patch_content: &str, prefixes: &mut IndexMap<String, String>) -> Result<()> {
    let mut insertions = Vec::new();
    let mut deletions = Vec::new();

    // First, parse the patch file as N3 to get any prefix declarations
    let patch_result = parse(patch_content)
        .map_err(|e| anyhow::anyhow!("Patch parse error: {}", e))?;
    prefixes.extend(patch_result.prefixes);

    // Process each line
    for line in patch_content.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') || trimmed.starts_with("@prefix") || trimmed.starts_with("@base") {
            continue;
        }

        if let Some(rest) = trimmed.strip_prefix('+') {
            // Parse and add as insertion
            let triple_str = rest.trim();
            if !triple_str.is_empty() {
                if let Ok(result) = parse(&format!("{} .", triple_str)) {
                    insertions.extend(result.triples);
                }
            }
        } else if let Some(rest) = trimmed.strip_prefix('-') {
            // Parse and add as deletion
            let triple_str = rest.trim();
            if !triple_str.is_empty() {
                if let Ok(result) = parse(&format!("{} .", triple_str)) {
                    deletions.extend(result.triples);
                }
            }
        }
    }

    // Apply deletions
    for triple in &deletions {
        store.remove(triple);
    }

    // Apply insertions
    for triple in insertions {
        store.add(triple);
    }

    Ok(())
}

/// Load closure (imports) based on flags
fn load_closure(
    store: &mut Store,
    prefixes: &mut IndexMap<String, String>,
    rules: &mut Vec<Rule>,
    flags: &str,
    verbose: bool
) -> Result<()> {
    let load_imports = flags.contains('i');
    let load_rules = flags.contains('r');

    if !load_imports && !load_rules {
        return Ok(());
    }

    // Find owl:imports statements
    let owl_imports = "http://www.w3.org/2002/07/owl#imports";
    let mut import_uris: Vec<String> = Vec::new();

    for triple in store.iter() {
        if let Term::Uri(pred) = &triple.predicate {
            if pred.as_str() == owl_imports {
                if let Term::Uri(obj) = &triple.object {
                    import_uris.push(obj.as_str().to_string());
                }
            }
        }
    }

    // Load each imported document
    for uri in import_uris {
        if verbose {
            eprintln!("Loading import: {}", uri);
        }

        // Try to fetch the document
        match fetch_document(&uri) {
            Ok(content) => {
                if let Ok(result) = parse(&content) {
                    store.add_all(result.triples);
                    prefixes.extend(result.prefixes);
                    if load_rules {
                        rules.extend(result.rules);
                    }
                }
            }
            Err(e) => {
                if !flags.contains('E') {
                    // Ignore errors unless E flag is set
                    eprintln!("Warning: Failed to load {}: {}", uri, e);
                }
            }
        }
    }

    Ok(())
}

/// Fetch a document from a URI (file or HTTP)
fn fetch_document(uri: &str) -> Result<String> {
    if uri.starts_with("file://") {
        let path = &uri[7..];
        fs::read_to_string(path).context("Failed to read local file")
    } else if uri.starts_with("http://") || uri.starts_with("https://") {
        let response = ureq::get(uri)
            .call()
            .map_err(|e| anyhow::anyhow!("HTTP error: {}", e))?;
        response.into_string().context("Failed to read HTTP response")
    } else {
        // Try as local file path
        fs::read_to_string(uri).context("Failed to read file")
    }
}

/// Run reasoning with proof tracking
fn run_with_proof_tracking(
    reasoner: &mut Reasoner,
    store: &mut Store,
    _rules: &[Rule],
    proof: &mut ProofTrace,
) -> cwm::ReasonerStats {
    // Track which triples existed before reasoning
    let original: std::collections::HashSet<Triple> = store.iter().cloned().collect();

    // Run the reasoner
    let stats = reasoner.run(store);

    // Find new triples and create proof steps
    for triple in store.iter() {
        if !original.contains(triple) {
            // This triple was derived - try to find which rule produced it
            let step = ProofStep {
                triple: triple.clone(),
                rule: None, // We'd need to track this during reasoning for accurate info
                bindings: Vec::new(),
                antecedents: Vec::new(),
            };
            proof.steps.push(step);
        }
    }

    stats.clone()
}

/// Format proof trace as N3 with annotations
fn format_proof(proof: &ProofTrace, prefixes: &IndexMap<String, String>) -> String {
    let mut output = String::new();
    let formatter = N3Formatter::new(prefixes);

    // Add prefix declarations
    output.push_str("# Proof trace\n");
    output.push_str("@prefix log: <http://www.w3.org/2000/10/swap/log#> .\n");
    output.push_str("@prefix r: <http://www.w3.org/2000/10/swap/reason#> .\n\n");

    for (i, step) in proof.steps.iter().enumerate() {
        output.push_str(&format!("# Step {}\n", i + 1));
        output.push_str(&format!(
            "{} {} {} .\n",
            formatter.format_term(&step.triple.subject),
            formatter.format_term(&step.triple.predicate),
            formatter.format_term(&step.triple.object)
        ));

        if let Some(rule_idx) = step.rule {
            output.push_str(&format!("    # Derived by rule {}\n", rule_idx));
        }

        if !step.antecedents.is_empty() {
            output.push_str("    # From:\n");
            for ant in &step.antecedents {
                output.push_str(&format!(
                    "    #   {} {} {}\n",
                    formatter.format_term(&ant.subject),
                    formatter.format_term(&ant.predicate),
                    formatter.format_term(&ant.object)
                ));
            }
        }
        output.push('\n');
    }

    output
}

/// Compute the difference between two graphs
pub fn graph_diff(before: &Store, after: &Store) -> (Vec<Triple>, Vec<Triple>) {
    let before_set: std::collections::HashSet<&Triple> = before.iter().collect();
    let after_set: std::collections::HashSet<&Triple> = after.iter().collect();

    // Additions: in after but not in before
    let additions: Vec<Triple> = after_set
        .difference(&before_set)
        .map(|t| (*t).clone())
        .collect();

    // Deletions: in before but not in after
    let deletions: Vec<Triple> = before_set
        .difference(&after_set)
        .map(|t| (*t).clone())
        .collect();

    (additions, deletions)
}

/// Format N3 with custom options
fn format_n3_with_options(store: &Store, prefixes: &IndexMap<String, String>, opts: &N3Options) -> String {
    let mut output = String::new();

    // Only output prefixes if not suppressed
    if !opts.no_prefix {
        for (short, long) in prefixes {
            if opts.no_default_ns && short.is_empty() {
                continue;
            }
            output.push_str(&format!("@prefix {}: <{}> .\n", short, long));
        }
        if !prefixes.is_empty() {
            output.push('\n');
        }
    }

    // Group by subject unless explicit subject mode
    if opts.explicit_subject {
        // Output each triple on its own line
        for triple in store.iter() {
            let subj = format_term_with_options(&triple.subject, prefixes, opts);
            let pred = format_term_with_options(&triple.predicate, prefixes, opts);
            let obj = format_term_with_options(&triple.object, prefixes, opts);
            output.push_str(&format!("{} {} {} .\n", subj, pred, obj));
        }
    } else {
        // Group by subject (standard pretty printing)
        let formatter = N3Formatter::new(prefixes);
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
                    format_term_with_options(&t.predicate, prefixes, opts),
                    format_term_with_options(&t.object, prefixes, opts)
                ));
            } else {
                output.push_str(&format!("{}\n", subject));
                for (i, t) in triples.iter().enumerate() {
                    let sep = if i < triples.len() - 1 { " ;" } else { " ." };
                    output.push_str(&format!(
                        "    {} {}{}\n",
                        format_term_with_options(&t.predicate, prefixes, opts),
                        format_term_with_options(&t.object, prefixes, opts),
                        sep
                    ));
                }
            }
        }
    }

    output
}

/// Format a term with N3 options
fn format_term_with_options(term: &Term, prefixes: &IndexMap<String, String>, opts: &N3Options) -> String {
    match term {
        Term::Uri(u) => {
            if opts.no_prefix {
                format!("<{}>", u.as_str())
            } else {
                // Try to use prefix
                let formatter = N3Formatter::new(prefixes);
                formatter.compact_uri(u.as_str())
            }
        }
        Term::Literal(lit) => {
            if opts.no_numeric {
                // Force string syntax for numbers
                format!("\"{}\"", lit.value())
            } else {
                let formatter = N3Formatter::new(prefixes);
                formatter.format_literal(lit)
            }
        }
        Term::BlankNode(b) => {
            if opts.anon_bnodes {
                format!("_:{}", b.id())
            } else {
                format!("{}", b)
            }
        }
        Term::List(l) => {
            if opts.no_list_syntax {
                // Would need to convert to rdf:first/rdf:rest - for now just use standard
                let formatter = N3Formatter::new(prefixes);
                formatter.format_list(l)
            } else {
                let formatter = N3Formatter::new(prefixes);
                formatter.format_list(l)
            }
        }
        _ => {
            let formatter = N3Formatter::new(prefixes);
            formatter.format_term(term)
        }
    }
}

/// Run a SPARQL HTTP endpoint server
fn run_sparql_server(store: &Store, _prefixes: &IndexMap<String, String>, port: u16) -> Result<()> {
    use tiny_http::{Server, Response, Header};

    let addr = format!("0.0.0.0:{}", port);
    let server = Server::http(&addr)
        .map_err(|e| anyhow::anyhow!("Failed to start server: {}", e))?;

    eprintln!("SPARQL server listening on http://localhost:{}/sparql", port);
    eprintln!("Press Ctrl+C to stop");

    // Clone data for the server loop
    let store_copy = store.clone();

    for mut request in server.incoming_requests() {
        let url = request.url().to_string();
        let method = request.method().to_string();

        // Handle SPARQL endpoint
        if url.starts_with("/sparql") {
            let query = if method == "GET" {
                // Parse query from URL parameters
                if let Some(pos) = url.find("?query=") {
                    let query_part = &url[pos + 7..];
                    // URL decode the query
                    percent_decode_str(query_part)
                        .decode_utf8()
                        .map(|s| s.into_owned())
                        .ok()
                } else {
                    None
                }
            } else if method == "POST" {
                // Read query from body
                let mut content = String::new();
                {
                    use std::io::Read;
                    let reader = request.as_reader();
                    let _ = reader.take(1024 * 1024).read_to_string(&mut content);
                }

                // Check content type
                let content_type: String = request.headers()
                    .iter()
                    .find(|h| h.field.to_string().to_lowercase() == "content-type")
                    .map(|h| h.value.to_string())
                    .unwrap_or_default();

                if content_type.contains("application/sparql-query") {
                    Some(content)
                } else if content_type.contains("application/x-www-form-urlencoded") {
                    // Parse form data
                    if let Some(pos) = content.find("query=") {
                        let query_part = &content[pos + 6..];
                        let end = query_part.find('&').unwrap_or(query_part.len());
                        percent_decode_str(&query_part[..end])
                            .decode_utf8()
                            .map(|s| s.into_owned())
                            .ok()
                    } else {
                        None
                    }
                } else {
                    Some(content)
                }
            } else {
                None
            };

            if let Some(query_str) = query {
                match execute_sparql(&store_copy, &query_str) {
                    Ok(result) => {
                        // Check Accept header for format
                        let accept: String = request.headers()
                            .iter()
                            .find(|h| h.field.to_string().to_lowercase() == "accept")
                            .map(|h| h.value.to_string())
                            .unwrap_or_else(|| "application/sparql-results+xml".to_string());

                        let (content_type, body) = if accept.contains("json") {
                            ("application/sparql-results+json", format_results_json(&result))
                        } else {
                            ("application/sparql-results+xml", format_results_xml(&result))
                        };

                        let response = Response::from_string(body)
                            .with_header(Header::from_bytes(
                                &b"Content-Type"[..],
                                content_type.as_bytes()
                            ).unwrap())
                            .with_header(Header::from_bytes(
                                &b"Access-Control-Allow-Origin"[..],
                                &b"*"[..]
                            ).unwrap());

                        let _ = request.respond(response);
                    }
                    Err(e) => {
                        let response = Response::from_string(format!("SPARQL Error: {}", e))
                            .with_status_code(400);
                        let _ = request.respond(response);
                    }
                }
            } else {
                let response = Response::from_string("Missing query parameter")
                    .with_status_code(400);
                let _ = request.respond(response);
            }
        } else if url == "/" || url == "/index.html" {
            // Serve a simple HTML form for testing
            let html = r#"<!DOCTYPE html>
<html>
<head><title>CWM SPARQL Endpoint</title></head>
<body>
<h1>CWM SPARQL Endpoint</h1>
<form action="/sparql" method="POST">
<textarea name="query" rows="10" cols="60">
SELECT ?s ?p ?o
WHERE { ?s ?p ?o }
LIMIT 10
</textarea>
<br>
<button type="submit">Execute Query</button>
</form>
</body>
</html>"#;
            let response = Response::from_string(html)
                .with_header(Header::from_bytes(
                    &b"Content-Type"[..],
                    &b"text/html"[..]
                ).unwrap());
            let _ = request.respond(response);
        } else {
            let response = Response::from_string("Not Found")
                .with_status_code(404);
            let _ = request.respond(response);
        }
    }

    Ok(())
}

use percent_encoding::percent_decode_str;

/// Execute N3QL query (N3 pattern matching)
///
/// N3QL queries are N3 files with rules that produce output.
/// The query is applied to the data, and matching patterns generate output triples.
///
/// Example query:
/// ```n3
/// @prefix : <http://example.org/> .
/// { ?s :name ?name } => { ?s :hasName ?name } .
/// ```
fn execute_n3ql(store: &Store, query_content: &str, _prefixes: &IndexMap<String, String>) -> Result<Store> {
    // Parse the query as N3
    let query_result = parse(query_content)
        .map_err(|e| anyhow::anyhow!("N3QL parse error: {}", e))?;

    // Create a copy of the store for reasoning
    let mut working_store = store.clone();

    // Add any triples from the query file
    for triple in query_result.triples {
        working_store.add(triple);
    }

    // If there are rules in the query, apply them
    let has_rules = !query_result.rules.is_empty();
    if has_rules {
        let config = ReasonerConfig {
            max_steps: 10000,
            recursive: true,
            filter: false,
        };

        let mut reasoner = Reasoner::with_config(config);
        for rule in query_result.rules {
            reasoner.add_rule(rule);
        }
        reasoner.run(&mut working_store);
    }

    // Find result triples - look for triples with special predicates
    // or just return all new triples derived
    let original: std::collections::HashSet<&Triple> = store.iter().collect();

    let mut result = Store::new();
    for triple in working_store.iter() {
        if !original.contains(triple) {
            result.add(triple.clone());
        }
    }

    // If no new triples, return matching triples based on query patterns
    if result.is_empty() && has_rules {
        // Just return all triples from the store as the result
        for triple in store.iter() {
            result.add(triple.clone());
        }
    }

    Ok(result)
}
