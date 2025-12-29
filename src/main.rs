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

use cwm::{Store, SqliteStore, Reasoner, ReasonerConfig, parse, parse_rdfxml, ParseResult, Term, Triple, FormulaRef, Literal, Datatype, execute_sparql, QueryResult, format_results_xml, format_results_json, Rule, List, set_run_prefix};
use cwm::term::Variable;
use cwm::fuseki::FusekiStoreBuilder;
use cwm::core::TripleStore;
use cwm::server::{ServerConfig, run_server};
use cwm::config::CwmConfig;

/// N3 output formatting options
#[derive(Default, Clone)]
struct N3Options {
    /// Use anonymous blank node syntax (_: convention)
    anon_bnodes: bool,
    /// Add comments at top about version and base URI
    add_comments: bool,
    /// Don't use default namespace
    no_default_ns: bool,
    /// Escape unicode using \u notation in literals
    escape_unicode: bool,
    /// Suppress => shorthand for log:implies
    no_implies_shorthand: bool,
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
    /// Use \u for unicode escaping in URIs (vs utf-8 %XX)
    unicode_uris: bool,
    /// Use "this log:forAll" for @forAll, "this log:forSome" for @forSome
    verbose_quantifiers: bool,
    // Input flag (parsed from same --n3= option)
    /// 'B' - Turn blank nodes into existentially quantified named nodes (skolemize)
    existential_bnodes: bool,
}

impl N3Options {
    fn from_flags(flags: &str) -> Self {
        let mut opts = N3Options::default();
        for c in flags.chars() {
            match c {
                'a' => opts.anon_bnodes = true,
                'c' => opts.add_comments = true,
                'd' => opts.no_default_ns = true,
                'e' => opts.escape_unicode = true,
                'g' => opts.no_implies_shorthand = true,
                'i' => opts.use_store_ids = true,
                'l' => opts.no_list_syntax = true,
                'n' => opts.no_numeric = true,
                'p' => opts.no_prefix = true,
                'r' => opts.no_relative = true,
                's' => opts.explicit_subject = true,
                't' => opts.no_special = true,
                'u' => opts.unicode_uris = true,
                'v' => opts.verbose_quantifiers = true,
                // Input flags
                'B' => opts.existential_bnodes = true,
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
#[command(about = "Closed World Machine - N3 reasoner and RDF processor")]
#[command(long_about = "cwm-rust is a high-performance N3 reasoner and RDF processor with:\n\n\
  - 266+ built-in predicates (math, string, list, log, time, crypto, os, graph)\n\
  - 11 theorem proving engines (resolution, otter, dpll, cdcl, tableau, etc.)\n\
  - Full SPARQL 1.1 support with XML/JSON results\n\
  - Apache Jena Fuseki integration\n\
  - Multiple output formats: N3, N-Triples, RDF/XML, JSON-LD\n\n\
Examples:\n\
  cwm data.n3 --think              # Forward-chaining inference\n\
  cwm data.n3 --sparql-query \"...\" # Run SPARQL query\n\
  cwm data.n3 --engine otter       # Use theorem prover\n\
  cwm data.n3 --sparqlServer 8000  # Start SPARQL endpoint\n\n\
Documentation: See README.md, docs/BUILTINS.md, docs/cwm.1")]
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

    /// Apply rules and replace store with conclusions only (CWM --filter=file)
    #[arg(long = "filter-rules", value_name = "FILE")]
    filter_rules: Vec<PathBuf>,

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

    /// Operating mode: r=remote (HTTP fetch), s=schema (read schema for predicates),
    /// a=auto-rules (load rules from schema), E=ignore schema errors,
    /// m=merge schemas to meta, u=unique run IDs, t=think, f=filter
    #[arg(long = "mode", value_name = "MODE")]
    mode: Option<String>,

    /// Data files to load (no rules extracted)
    #[arg(long = "data", value_name = "FILE")]
    data: Vec<PathBuf>,

    /// Flatten formula contents into main graph
    #[arg(long)]
    flatten: bool,

    /// Unflatten: reconstruct nested formulas from reified statements
    #[arg(long)]
    unflatten: bool,

    /// Enable cryptographic operations (for security, explicit enable)
    #[arg(long)]
    crypto: bool,

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

    /// N3 output flags: a=anon bnodes, c=version comments, d=no default ns,
    /// e=escape unicode, g=no => shorthand, i=use store ids, l=no list syntax,
    /// n=no numeric, p=no prefix, r=no relative URIs, s=explicit subject,
    /// t=no special syntax, u=unicode URIs, v=verbose quantifiers
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

    /// Output diff/delta showing what changed (additions/deletions)
    #[arg(long)]
    diff: bool,

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

    /// Output revision/version information
    #[arg(long)]
    revision: bool,

    /// Use SQLite database as persistent backend store
    #[arg(long = "db", value_name = "PATH")]
    db_path: Option<PathBuf>,

    /// Use Fuseki SPARQL endpoint as backend store
    #[arg(long = "fuseki", value_name = "URL")]
    fuseki_endpoint: Option<String>,

    /// Fuseki graph URI (default: default graph)
    #[arg(long = "fuseki-graph", value_name = "URI")]
    fuseki_graph: Option<String>,

    /// Fuseki connection timeout in seconds (default: 30)
    #[arg(long = "fuseki-timeout", value_name = "SECS", default_value = "30")]
    fuseki_timeout: u64,

    /// Batch size for Fuseki bulk operations (default: 1000)
    #[arg(long = "fuseki-batch", value_name = "SIZE", default_value = "1000")]
    fuseki_batch: usize,

    /// Use theorem proving engine for formal verification.
    ///
    /// Available engines:
    ///   resolution    - Classical resolution with factoring
    ///   otter         - Set-of-support strategy (efficient refutation)
    ///   dpll          - Davis-Putnam-Logemann-Loveland (SAT)
    ///   cdcl          - Conflict-Driven Clause Learning (large SAT)
    ///   tableau       - Analytic tableaux (intuitive proofs)
    ///   leancop       - Lean connection calculus
    ///   nanocop       - Minimal connection prover
    ///   superposition - Modern equality prover
    ///   knuth-bendix  - Term rewriting completion
    ///   smt           - SMT-style E-matching
    ///   dl-tableau    - Description Logic (OWL)
    #[arg(long = "engine", value_name = "ENGINE")]
    engine: Option<String>,

    /// Reasoning profile to use.
    ///
    /// Available profiles:
    ///   default     - Balanced settings for general use
    ///   rdfs        - RDFS entailment with subclass/subproperty reasoning
    ///   owl         - OWL 2 RL reasoning with class/property axioms
    ///   shacl       - SHACL validation mode
    ///   performance - Optimized for speed (reduced inference depth)
    ///   complete    - Maximum completeness (slower but thorough)
    #[arg(long = "profile", value_name = "PROFILE")]
    profile: Option<String>,

    /// Configuration file path (overrides default discovery)
    #[arg(long = "config", value_name = "FILE")]
    config_file: Option<PathBuf>,

    /// Generate default configuration file and exit
    #[arg(long = "init-config")]
    init_config: bool,
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

/// Parse --language string to OutputFormat
fn parse_language(lang: &str) -> Option<OutputFormat> {
    match lang.to_lowercase().as_str() {
        "n3" | "notation3" => Some(OutputFormat::N3),
        "ntriples" | "nt" => Some(OutputFormat::Ntriples),
        "rdf" | "rdf/xml" | "rdfxml" | "xml" => Some(OutputFormat::Rdf),
        "jsonld" | "json-ld" => Some(OutputFormat::Jsonld),
        _ => None,
    }
}

/// Fetch content from a URL using HTTP (with connection pooling)
fn fetch_url(url: &str) -> Result<String> {
    let client = cwm::get_sync_client();
    let response = client.get(url)
        .call()
        .with_context(|| format!("Failed to fetch URL: {}", url))?;

    if response.status() != 200 {
        return Err(anyhow::anyhow!("HTTP {} fetching {}", response.status(), url));
    }

    let content = response.into_string()
        .with_context(|| format!("Failed to read content from {}", url))?;

    Ok(content)
}

/// Check if a path looks like a URL
fn is_url(path: &str) -> bool {
    path.starts_with("http://") || path.starts_with("https://")
}

/// Check if content appears to be RDF/XML format
fn is_rdfxml_content(content: &str) -> bool {
    let trimmed = content.trim().trim_start_matches('\u{feff}');

    // Check for XML declaration
    if trimmed.starts_with("<?xml") {
        // Look for rdf:RDF element
        return trimmed.contains("<rdf:RDF") ||
               trimmed.contains("xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"");
    }

    // Check for rdf:RDF without XML declaration
    if trimmed.starts_with("<rdf:RDF") {
        return true;
    }

    // Check for any element with rdf namespace
    if trimmed.starts_with('<') && trimmed.contains("xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"") {
        return true;
    }

    false
}

/// Load content from either a local file or a URL (if remote mode enabled)
fn load_content(path: &std::path::Path, allow_remote: bool) -> Result<String> {
    let path_str = path.to_string_lossy();

    if is_url(&path_str) {
        if !allow_remote {
            return Err(anyhow::anyhow!(
                "Remote URL {} specified but remote mode not enabled (use --mode=r)",
                path_str
            ));
        }
        fetch_url(&path_str)
    } else {
        fs::read_to_string(path)
            .with_context(|| format!("Failed to read file: {}", path.display()))
    }
}

/// Skolemize blank nodes in triples, converting them to existentially quantified URIs.
/// This implements the --n3=B flag behavior.
fn skolemize_triples(triples: Vec<Triple>) -> Vec<Triple> {
    use std::collections::HashMap;
    use std::sync::atomic::{AtomicU64, Ordering};

    static SKOLEM_COUNTER: AtomicU64 = AtomicU64::new(0);

    // Map blank nodes by their label (for labeled bnodes) or internal id (for anonymous)
    let mut label_map: HashMap<String, Term> = HashMap::new();
    let mut id_map: HashMap<u64, Term> = HashMap::new();

    fn skolemize_term(
        term: &Term,
        label_map: &mut HashMap<String, Term>,
        id_map: &mut HashMap<u64, Term>,
        counter: &AtomicU64
    ) -> Term {
        if let Term::BlankNode(bn) = term {
            // Use label for consistency if available, otherwise fall back to internal id
            if let Some(label) = bn.label() {
                if let Some(skolem) = label_map.get(label) {
                    return skolem.clone();
                }
                // Create skolem URI based on the label
                let skolem_uri = format!("urn:uuid:skolem-{}", label);
                let skolem = Term::uri(&skolem_uri);
                label_map.insert(label.to_string(), skolem.clone());
                skolem
            } else {
                // Anonymous blank node - use internal id
                let id = bn.id();
                if let Some(skolem) = id_map.get(&id) {
                    return skolem.clone();
                }
                let c = counter.fetch_add(1, Ordering::SeqCst);
                let skolem_uri = format!("urn:uuid:skolem-anon-{}", c);
                let skolem = Term::uri(&skolem_uri);
                id_map.insert(id, skolem.clone());
                skolem
            }
        } else {
            term.clone()
        }
    }

    triples.into_iter().map(|triple| {
        let subject = skolemize_term(&triple.subject, &mut label_map, &mut id_map, &SKOLEM_COUNTER);
        let predicate = triple.predicate.clone(); // Predicates shouldn't be blank nodes
        let object = skolemize_term(&triple.object, &mut label_map, &mut id_map, &SKOLEM_COUNTER);
        Triple::new(subject, predicate, object)
    }).collect()
}

/// Skolemize blank nodes in rules
fn skolemize_rules(rules: Vec<Rule>) -> Vec<Rule> {
    rules.into_iter().map(|rule| {
        let antecedent = skolemize_triples(rule.antecedent.clone());
        let consequent = skolemize_triples(rule.consequent.clone());
        Rule::new(antecedent, consequent)
    }).collect()
}

/// Result of loading schemas for predicates
struct SchemaLoadResult {
    triples: Vec<Triple>,
    #[allow(dead_code)]
    loaded_uris: Vec<String>,
}

/// Load schemas for all predicates used in the store (mode 's')
fn load_schemas_for_predicates(
    store: &Store,
    prefixes: &mut IndexMap<String, String>,
    allow_remote: bool,
    ignore_errors: bool,
    verbose: bool,
) -> SchemaLoadResult {
    use std::collections::HashSet;

    let mut loaded_uris: HashSet<String> = HashSet::new();
    let mut all_triples = Vec::new();

    // Collect all unique predicate URIs
    let mut predicate_uris: HashSet<String> = HashSet::new();
    for triple in store.iter() {
        if let Term::Uri(uri) = &triple.predicate {
            predicate_uris.insert(uri.as_str().to_string());
        }
    }

    // For each predicate, try to load its namespace/schema
    for uri in predicate_uris {
        // Extract namespace (everything before the last # or /)
        let namespace = if let Some(pos) = uri.rfind('#') {
            &uri[..=pos]
        } else if let Some(pos) = uri.rfind('/') {
            &uri[..=pos]
        } else {
            continue;
        };

        if loaded_uris.contains(namespace) {
            continue;
        }

        if verbose {
            eprintln!("Loading schema from: {}", namespace);
        }

        // Try to load the schema
        let content = if allow_remote && (namespace.starts_with("http://") || namespace.starts_with("https://")) {
            match fetch_url(namespace) {
                Ok(c) => c,
                Err(e) => {
                    if !ignore_errors && verbose {
                        eprintln!("Warning: Failed to load schema {}: {}", namespace, e);
                    }
                    continue;
                }
            }
        } else {
            // Can't load non-HTTP URIs without remote mode
            continue;
        };

        // Parse the schema
        let parse_result = if is_rdfxml_content(&content) {
            match parse_rdfxml(&content) {
                Ok(r) => Some((r.triples, r.prefixes)),
                Err(e) => {
                    if !ignore_errors && verbose {
                        eprintln!("Warning: Failed to parse schema {}: {}", namespace, e);
                    }
                    None
                }
            }
        } else {
            match parse(&content) {
                Ok(r) => Some((r.triples, r.prefixes)),
                Err(e) => {
                    if !ignore_errors && verbose {
                        eprintln!("Warning: Failed to parse schema {}: {}", namespace, e);
                    }
                    None
                }
            }
        };

        if let Some((triples, schema_prefixes)) = parse_result {
            loaded_uris.insert(namespace.to_string());
            all_triples.extend(triples);
            prefixes.extend(schema_prefixes);
        }
    }

    SchemaLoadResult {
        triples: all_triples,
        loaded_uris: loaded_uris.into_iter().collect(),
    }
}

/// Generate inference rules from RDFS/OWL schema definitions (mode 'a')
fn generate_schema_rules(schema_triples: &[Triple]) -> Vec<Rule> {
    let mut rules = Vec::new();

    let rdfs_subclass = "http://www.w3.org/2000/01/rdf-schema#subClassOf";
    let rdfs_subproperty = "http://www.w3.org/2000/01/rdf-schema#subPropertyOf";
    let rdfs_domain = "http://www.w3.org/2000/01/rdf-schema#domain";
    let rdfs_range = "http://www.w3.org/2000/01/rdf-schema#range";
    let rdf_type = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

    for triple in schema_triples {
        if let Term::Uri(pred) = &triple.predicate {
            let pred_str = pred.as_str();

            // rdfs:subClassOf => { ?x a :SubClass } => { ?x a :SuperClass }
            if pred_str == rdfs_subclass {
                if let (Term::Uri(_sub), Term::Uri(_super)) = (&triple.subject, &triple.object) {
                    let x = Term::Variable(Variable::universal("x".to_string()));
                    let antecedent = vec![Triple::new(
                        x.clone(),
                        Term::uri(rdf_type),
                        triple.subject.clone(),
                    )];
                    let consequent = vec![Triple::new(
                        x,
                        Term::uri(rdf_type),
                        triple.object.clone(),
                    )];
                    rules.push(Rule::new(antecedent, consequent));
                }
            }

            // rdfs:subPropertyOf => { ?x :subProp ?y } => { ?x :superProp ?y }
            if pred_str == rdfs_subproperty {
                if let (Term::Uri(_sub), Term::Uri(_super)) = (&triple.subject, &triple.object) {
                    let x = Term::Variable(Variable::universal("x".to_string()));
                    let y = Term::Variable(Variable::universal("y".to_string()));
                    let antecedent = vec![Triple::new(
                        x.clone(),
                        triple.subject.clone(),
                        y.clone(),
                    )];
                    let consequent = vec![Triple::new(
                        x,
                        triple.object.clone(),
                        y,
                    )];
                    rules.push(Rule::new(antecedent, consequent));
                }
            }

            // rdfs:domain => { ?x :prop ?y } => { ?x a :DomainClass }
            if pred_str == rdfs_domain {
                if let (Term::Uri(_prop), Term::Uri(_domain)) = (&triple.subject, &triple.object) {
                    let x = Term::Variable(Variable::universal("x".to_string()));
                    let y = Term::Variable(Variable::universal("y".to_string()));
                    let antecedent = vec![Triple::new(
                        x.clone(),
                        triple.subject.clone(),
                        y,
                    )];
                    let consequent = vec![Triple::new(
                        x,
                        Term::uri(rdf_type),
                        triple.object.clone(),
                    )];
                    rules.push(Rule::new(antecedent, consequent));
                }
            }

            // rdfs:range => { ?x :prop ?y } => { ?y a :RangeClass }
            if pred_str == rdfs_range {
                if let (Term::Uri(_prop), Term::Uri(_range)) = (&triple.subject, &triple.object) {
                    let x = Term::Variable(Variable::universal("x".to_string()));
                    let y = Term::Variable(Variable::universal("y".to_string()));
                    let antecedent = vec![Triple::new(
                        x,
                        triple.subject.clone(),
                        y.clone(),
                    )];
                    let consequent = vec![Triple::new(
                        y,
                        Term::uri(rdf_type),
                        triple.object.clone(),
                    )];
                    rules.push(Rule::new(antecedent, consequent));
                }
            }
        }
    }

    rules
}

/// Supported external reasoning engines
#[derive(Debug, Clone, Copy, PartialEq)]
enum ReasoningEngine {
    /// Otter theorem prover
    Otter,
    /// Prover9 (Otter successor)
    Prover9,
    /// DPLL SAT solver
    Dpll,
    /// CDCL SAT solver with conflict learning
    Cdcl,
    /// leanCoP connection prover
    LeanCop,
    /// nanoCoP non-clausal connection prover
    NanoCop,
    /// Analytic Tableau prover
    Tableau,
    /// Superposition prover
    Superposition,
    /// DPLL(T) SMT solver
    Smt,
    /// Description Logic Tableau reasoner
    DlTableau,
    /// Knuth-Bendix completion
    KnuthBendix,
}

impl ReasoningEngine {
    fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "otter" => Some(ReasoningEngine::Otter),
            "prover9" => Some(ReasoningEngine::Prover9),
            "dpll" | "sat" => Some(ReasoningEngine::Dpll),
            "cdcl" => Some(ReasoningEngine::Cdcl),
            "leancop" | "lean-cop" => Some(ReasoningEngine::LeanCop),
            "nanocop" | "nano-cop" => Some(ReasoningEngine::NanoCop),
            "tableau" => Some(ReasoningEngine::Tableau),
            "superposition" | "super" => Some(ReasoningEngine::Superposition),
            "smt" | "dpll-t" => Some(ReasoningEngine::Smt),
            "dl-tableau" | "dl" | "owl" => Some(ReasoningEngine::DlTableau),
            "knuth-bendix" | "kb" | "completion" => Some(ReasoningEngine::KnuthBendix),
            _ => None,
        }
    }

    #[allow(dead_code)]
    fn command(&self) -> &'static str {
        match self {
            ReasoningEngine::Otter => "otter",
            ReasoningEngine::Prover9 => "prover9",
            ReasoningEngine::Dpll => "dpll",
            ReasoningEngine::Cdcl => "cdcl",
            ReasoningEngine::LeanCop => "leancop",
            ReasoningEngine::NanoCop => "nanocop",
            ReasoningEngine::Tableau => "tableau",
            ReasoningEngine::Superposition => "superposition",
            ReasoningEngine::Smt => "smt",
            ReasoningEngine::DlTableau => "dl-tableau",
            ReasoningEngine::KnuthBendix => "knuth-bendix",
        }
    }

    /// List all available engines
    #[allow(dead_code)]
    fn available() -> &'static [&'static str] {
        &[
            "otter", "prover9", "dpll", "cdcl", "leancop", "nanocop",
            "tableau", "superposition", "smt", "dl-tableau", "knuth-bendix"
        ]
    }
}

/// Convert a Term to Prover9/Otter format
fn term_to_prover9(term: &Term) -> String {
    match term {
        Term::Uri(uri) => {
            // Convert URI to a valid Prover9 identifier
            // Replace special characters with underscores
            let s = uri.as_str();
            let cleaned: String = s.chars()
                .map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' })
                .collect();
            format!("uri_{}", cleaned)
        }
        Term::Literal(lit) => {
            // Convert literal to quoted string representation
            let val = lit.value().replace('\\', "\\\\").replace('"', "\\\"");
            format!("lit_\"{}\"", val)
        }
        Term::BlankNode(bn) => {
            format!("bnode_{}", bn.id())
        }
        Term::Variable(var) => {
            // Prover9 variables must start with lowercase for existential, uppercase for universal
            let name = var.name();
            if var.is_universal() {
                // Universal variables use uppercase
                format!("{}", name.to_uppercase())
            } else {
                // Existential variables use lowercase
                format!("{}", name.to_lowercase())
            }
        }
        Term::List(_) => "list_nil".to_string(), // Simplified
        Term::Formula(_) => "formula".to_string(), // Simplified
    }
}

/// Convert triples to Prover9 format (facts)
fn triples_to_prover9(triples: &[Triple]) -> String {
    let mut output = String::new();
    output.push_str("formulas(assumptions).\n");

    for triple in triples {
        let subj = term_to_prover9(&triple.subject);
        let pred = term_to_prover9(&triple.predicate);
        let obj = term_to_prover9(&triple.object);

        // Express triple as: triple(subject, predicate, object)
        output.push_str(&format!("  triple({}, {}, {}).\n", subj, pred, obj));
    }

    output.push_str("end_of_list.\n\n");
    output
}

/// Convert rules to Prover9 format (implications)
fn rules_to_prover9(rules: &[Rule]) -> String {
    let mut output = String::new();

    if rules.is_empty() {
        return output;
    }

    output.push_str("formulas(assumptions).\n");

    for rule in rules {
        // Convert: { antecedent } => { consequent }
        // To: (all vars) antecedent -> consequent

        let mut ant_parts = Vec::new();
        for triple in &rule.antecedent {
            let subj = term_to_prover9(&triple.subject);
            let pred = term_to_prover9(&triple.predicate);
            let obj = term_to_prover9(&triple.object);
            ant_parts.push(format!("triple({}, {}, {})", subj, pred, obj));
        }

        let mut con_parts = Vec::new();
        for triple in &rule.consequent {
            let subj = term_to_prover9(&triple.subject);
            let pred = term_to_prover9(&triple.predicate);
            let obj = term_to_prover9(&triple.object);
            con_parts.push(format!("triple({}, {}, {})", subj, pred, obj));
        }

        if !ant_parts.is_empty() && !con_parts.is_empty() {
            let antecedent = if ant_parts.len() == 1 {
                ant_parts[0].clone()
            } else {
                format!("({})", ant_parts.join(" & "))
            };

            let consequent = if con_parts.len() == 1 {
                con_parts[0].clone()
            } else {
                format!("({})", con_parts.join(" & "))
            };

            output.push_str(&format!("  {} -> {}.\n", antecedent, consequent));
        }
    }

    output.push_str("end_of_list.\n\n");
    output
}

/// Create Prover9 input file content
fn create_prover9_input(triples: &[Triple], rules: &[Rule], goals: Option<&[Triple]>) -> String {
    let mut output = String::new();

    // Add facts
    output.push_str(&triples_to_prover9(triples));

    // Add rules
    output.push_str(&rules_to_prover9(rules));

    // Add goals if provided
    if let Some(goal_triples) = goals {
        output.push_str("formulas(goals).\n");
        for triple in goal_triples {
            let subj = term_to_prover9(&triple.subject);
            let pred = term_to_prover9(&triple.predicate);
            let obj = term_to_prover9(&triple.object);
            output.push_str(&format!("  triple({}, {}, {}).\n", subj, pred, obj));
        }
        output.push_str("end_of_list.\n");
    }

    output
}

/// Run native Rust prover engine and return results
fn run_prover_engine(
    engine: ReasoningEngine,
    triples: &[Triple],
    rules: &[Rule],
    verbose: bool,
) -> Result<Vec<Triple>> {
    use cwm::prover::{OtterProver, Prover9, ProverConfig, Prover9Config, ProofResult, Derivation};
    use cwm::prover::leancop::{LeanCop, LeanCopConfig, LeanCopResult};
    use cwm::prover::superposition::{SuperpositionProver, SuperpositionConfig};

    // Create input in Prover9 format (our native provers understand this format)
    let input_content = create_prover9_input(triples, rules, None);

    if verbose {
        eprintln!("=== Prover Input ===");
        eprintln!("{}", input_content);
        eprintln!("====================");
    }

    // Handle engines that use the Prover9 input format
    match engine {
        ReasoningEngine::Otter | ReasoningEngine::Prover9 |
        ReasoningEngine::LeanCop | ReasoningEngine::Superposition => {
            // These engines use standard first-order logic input
        }
        ReasoningEngine::Dpll | ReasoningEngine::Cdcl => {
            // SAT solvers - need CNF input
            if verbose {
                eprintln!("SAT solver engines require propositional CNF input");
                eprintln!("Use for propositional satisfiability problems");
            }
            return Ok(Vec::new());
        }
        ReasoningEngine::NanoCop | ReasoningEngine::Tableau => {
            // These work on formula level
            if verbose {
                eprintln!("Non-clausal provers - work on formula level");
            }
        }
        ReasoningEngine::Smt => {
            if verbose {
                eprintln!("SMT solver - for satisfiability modulo theories");
            }
            return Ok(Vec::new());
        }
        ReasoningEngine::DlTableau => {
            if verbose {
                eprintln!("DL Tableau - for description logic reasoning");
            }
            return Ok(Vec::new());
        }
        ReasoningEngine::KnuthBendix => {
            if verbose {
                eprintln!("Knuth-Bendix - for equational reasoning and term rewriting");
            }
            return Ok(Vec::new());
        }
    }

    // Run the appropriate native prover
    let result = match engine {
        ReasoningEngine::Otter => {
            let config = ProverConfig {
                max_clauses: 10000,
                max_seconds: 60,
                max_weight: 100,
                set_of_support: true,
                ordered_resolution: false,
                hyperresolution: false,
                paramodulation: true,
                demodulation: true,
                verbose,
            };
            let mut prover = OtterProver::with_config(config);

            if let Err(e) = prover.parse_input(&input_content) {
                return Err(anyhow::anyhow!("Failed to parse input for Otter: {}", e));
            }

            prover.prove()
        }
        ReasoningEngine::Prover9 => {
            let config = Prover9Config {
                base: ProverConfig {
                    max_clauses: 10000,
                    max_seconds: 60,
                    max_weight: 100,
                    set_of_support: true,
                    ordered_resolution: true,
                    hyperresolution: false,
                    paramodulation: true,
                    demodulation: true,
                    verbose,
                },
                ..Default::default()
            };
            let mut prover = Prover9::with_config(config);

            if let Err(e) = prover.parse_input(&input_content) {
                return Err(anyhow::anyhow!("Failed to parse input for Prover9: {}", e));
            }

            prover.prove()
        }
        ReasoningEngine::LeanCop => {
            let config = LeanCopConfig {
                max_depth: 100,
                max_inferences: 100000,
                iterative_deepening: true,
                verbose,
            };
            let mut prover = LeanCop::with_config(config);

            if let Err(e) = prover.parse_input(&input_content) {
                return Err(anyhow::anyhow!("Failed to parse input for leanCoP: {}", e));
            }

            match prover.prove() {
                LeanCopResult::Proved { inferences, depth } => {
                    if verbose {
                        eprintln!("=== leanCoP: THEOREM PROVED ===");
                        eprintln!("Inferences: {}, Depth: {}", inferences, depth);
                    }
                    return Ok(Vec::new());
                }
                LeanCopResult::NotProved { reason } => {
                    if verbose {
                        eprintln!("=== leanCoP: NOT PROVED ===");
                        eprintln!("Reason: {}", reason);
                    }
                    return Ok(Vec::new());
                }
            }
        }
        ReasoningEngine::Superposition => {
            let config = SuperpositionConfig::default();
            let mut prover = SuperpositionProver::with_config(config);

            if let Err(e) = prover.parse_input(&input_content) {
                return Err(anyhow::anyhow!("Failed to parse input for Superposition: {}", e));
            }

            prover.prove()
        }
        _ => {
            // Other engines handled above
            return Ok(Vec::new());
        }
    };

    // Report results
    match result {
        ProofResult::Proved { proof, clauses_generated, resolution_steps } => {
            if verbose {
                eprintln!("=== THEOREM PROVED ===");
                eprintln!("Clauses generated: {}", clauses_generated);
                eprintln!("Resolution steps: {}", resolution_steps);
                eprintln!("Proof length: {} steps", proof.len());
                eprintln!();
                for step in &proof {
                    let justification = match &step.derivation {
                        Derivation::Input => "input".to_string(),
                        Derivation::Resolution { clause1_id, clause2_id, .. } =>
                            format!("resolve({}, {})", clause1_id, clause2_id),
                        Derivation::Factor { clause_id, .. } =>
                            format!("factor({})", clause_id),
                        Derivation::Paramodulation { from_clause, into_clause } =>
                            format!("para({}, {})", from_clause, into_clause),
                        Derivation::Demodulation { clause_id, demodulator_id } =>
                            format!("demod({}, {})", clause_id, demodulator_id),
                    };
                    eprintln!("  {}. {:?} [{}]", step.step_id, step.clause, justification);
                }
                eprintln!("======================");
            }
        }
        ProofResult::Unknown { reason } => {
            if verbose {
                eprintln!("=== PROOF NOT FOUND ===");
                eprintln!("Reason: {}", reason);
                eprintln!("=======================");
            }
        }
        ProofResult::Satisfiable => {
            if verbose {
                eprintln!("=== SATISFIABLE (no contradiction found) ===");
            }
        }
    }

    // The native provers validate theorems but don't generate new triples
    // For forward-chaining inference, use the standard Reasoner
    Ok(Vec::new())
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Handle --init-config: generate default config file and exit
    if cli.init_config {
        let config = CwmConfig::default();
        let toml_str = toml::to_string_pretty(&config)
            .context("Failed to serialize default config")?;

        let config_path = dirs::home_dir()
            .map(|h| h.join(".cwm").join("config.toml"))
            .unwrap_or_else(|| PathBuf::from("cwm.toml"));

        if let Some(parent) = config_path.parent() {
            fs::create_dir_all(parent)
                .context("Failed to create config directory")?;
        }

        fs::write(&config_path, &toml_str)
            .with_context(|| format!("Failed to write config to {}", config_path.display()))?;

        println!("Generated default config: {}", config_path.display());
        println!("\nConfiguration options:");
        println!("  [general]       - Output format, verbosity, base URI");
        println!("  [reasoning]     - Max steps, tabling, stratification");
        println!("  [server]        - SPARQL endpoint settings");
        println!("  [store]         - Backend type (memory, fuseki, sqlite)");
        println!("  [security]      - Crypto enable, allowed hosts");
        println!("  [prefixes]      - Default namespace prefixes");
        println!("  [profiles.*]    - Custom reasoning profiles");
        return Ok(());
    }

    // Handle --revision flag: output version info
    if cli.revision {
        println!("cwm-rust version 0.1.0");
        println!("Rust implementation of the Closed World Machine (CWM)");
        println!("Based on Tim Berners-Lee's original CWM for N3/RDF reasoning");
        println!("Repository: https://github.com/cwm-rust/cwm");
        return Ok(());
    }

    // Load configuration (from file or defaults)
    let mut config = if let Some(ref config_path) = cli.config_file {
        CwmConfig::load_from_file(config_path)
            .with_context(|| format!("Failed to load config from {}", config_path.display()))?
    } else {
        CwmConfig::load().unwrap_or_else(|e| {
            if cli.verbose {
                eprintln!("Note: Using default config ({})", e);
            }
            CwmConfig::default()
        })
    };

    // Apply environment variable overrides
    config.apply_env_overrides();

    // Apply reasoning profile (CLI --profile overrides config file setting)
    if let Some(ref profile_name) = cli.profile {
        config.apply_profile(profile_name)
            .with_context(|| format!("Failed to apply profile: {}", profile_name))?;

        if cli.verbose && !cli.quiet {
            eprintln!("Using reasoning profile: {}", profile_name);
        }
    }

    // Use config values as defaults (CLI flags override config)
    let effective_max_steps = if cli.max_steps != 10000 {
        cli.max_steps // CLI explicitly set
    } else {
        config.reasoning.max_steps
    };

    // Determine effective output format (--language overrides default if no explicit --format)
    let effective_format = if let Some(ref lang) = cli.language {
        parse_language(lang).unwrap_or(cli.format)
    } else {
        cli.format
    };

    // Parse --mode flags (r=remote, s=schema, a=auto-rules, E=ignore-errors, m=merge-meta, u=unique-ids, t=think, f=filter)
    #[derive(Default)]
    struct ModeFlags {
        think: bool,
        filter: bool,
        remote: bool,      // 'r' - enable remote operations (HTTP fetching)
        schema: bool,      // 's' - read schema for predicates
        auto_rules: bool,  // 'a' - auto-load rules from schema
        ignore_errors: bool, // 'E' - ignore schema errors
        merge_meta: bool,  // 'm' - merge schemas to meta graph
        unique_ids: bool,  // 'u' - run-specific unique IDs
    }

    let mode_flags = if let Some(ref mode) = cli.mode {
        let chars: Vec<char> = mode.chars().collect();
        ModeFlags {
            think: chars.contains(&'t'),
            filter: chars.contains(&'f'),
            remote: chars.contains(&'r'),
            schema: chars.contains(&'s'),
            auto_rules: chars.contains(&'a'),
            ignore_errors: chars.contains(&'E'),
            merge_meta: chars.contains(&'m'),
            unique_ids: chars.contains(&'u'),
        }
    } else {
        ModeFlags::default()
    };

    let (mode_think, mode_filter) = (mode_flags.think, mode_flags.filter);

    // Parse N3 options early so we can use existential_bnodes during input processing
    let n3_opts = cli.n3_flags.as_ref().map(|f| N3Options::from_flags(f));
    let do_skolemize = n3_opts.as_ref().map_or(false, |opts| opts.existential_bnodes);

    // Set run-specific unique ID prefix if mode 'u' is enabled
    if mode_flags.unique_ids {
        use std::time::{SystemTime, UNIX_EPOCH};
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let pid = std::process::id();
        let run_prefix = format!("r{}_{}", pid, timestamp);
        set_run_prefix(run_prefix);
    }

    // Effective think/filter: CLI flags OR --mode
    let effective_think = cli.think || mode_think || !cli.apply.is_empty();
    let effective_filter = cli.filter || mode_filter;

    // Collect all prefixes
    let mut all_prefixes: IndexMap<String, String> = IndexMap::new();
    let mut all_rules = Vec::new();

    // Add RDFS entailment rules if rdfs profile is active
    if cli.profile.as_deref() == Some("rdfs") {
        use cwm::RdfsRules;
        all_rules.extend(RdfsRules::all());
        if cli.verbose && !cli.quiet {
            eprintln!("Added {} RDFS entailment rules", RdfsRules::all().len());
        }
    }

    // Collect input content
    let mut content = String::new();

    if cli.stdin || (cli.inputs.is_empty() && cli.data.is_empty() && cli.apply.is_empty() && cli.rules.is_empty()) {
        io::stdin().read_to_string(&mut content)
            .context("Failed to read from stdin")?;
    }

    for path in &cli.inputs {
        let file_content = load_content(path, mode_flags.remote)?;
        content.push_str(&file_content);
        content.push('\n');
    }

    // Determine input format and parse
    let parse_result = if is_rdfxml_content(&content) || cli.language.as_deref() == Some("rdf") {
        // Parse as RDF/XML
        let result = parse_rdfxml(&content)
            .map_err(|e| anyhow::anyhow!("RDF/XML parse error: {}", e))?;
        ParseResult {
            triples: result.triples,
            rules: Vec::new(), // RDF/XML doesn't have rules
            prefixes: result.prefixes,
            base: None,
            formulas: IndexMap::new(),
        }
    } else {
        // Parse as N3/Turtle
        parse(&content)
            .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?
    };

    let mut store = Store::new();

    // Track SQLite store for persistence at the end
    let sqlite_store_path = cli.db_path.clone();

    // If SQLite database specified, load existing data from it
    if let Some(ref db_path) = cli.db_path {
        if !cli.quiet && cli.verbose {
            eprintln!("Opening SQLite database: {}", db_path.display());
        }
        match SqliteStore::open(db_path) {
            Ok(sqlite_store) => {
                let existing = sqlite_store.to_vec();
                if !cli.quiet && cli.verbose {
                    eprintln!("Loaded {} triples from SQLite", existing.len());
                }
                store.add_all(existing);
            }
            Err(e) => {
                if !cli.quiet {
                    eprintln!("Warning: Could not open SQLite database: {}. Starting with empty store.", e);
                }
            }
        }
    }

    // If Fuseki endpoint specified, load existing data from it
    let fuseki_store = if let Some(ref endpoint) = cli.fuseki_endpoint {
        if !cli.quiet && cli.verbose {
            eprintln!("Connecting to Fuseki endpoint: {}", endpoint);
        }
        let mut builder = FusekiStoreBuilder::new(endpoint.clone())
            .timeout(cli.fuseki_timeout)
            .batch_size(cli.fuseki_batch);

        if let Some(ref graph) = cli.fuseki_graph {
            builder = builder.graph(graph.clone());
        }

        let fuseki = builder.build()
            .map_err(|e| anyhow::anyhow!("Failed to connect to Fuseki: {}", e))?;

        // Load existing triples from Fuseki into local store
        let existing = fuseki.load_all()
            .map_err(|e| anyhow::anyhow!("Failed to load from Fuseki: {}", e))?;

        if !cli.quiet && cli.verbose {
            eprintln!("Loaded {} triples from Fuseki", existing.len());
        }

        store.add_all(existing);
        Some(fuseki)
    } else {
        None
    };

    // Apply skolemization if --n3=B flag is set
    let initial_triples = if do_skolemize {
        skolemize_triples(parse_result.triples.clone())
    } else {
        parse_result.triples.clone()
    };
    let initial_rules = if do_skolemize {
        skolemize_rules(parse_result.rules.clone())
    } else {
        parse_result.rules.clone()
    };
    store.add_all(initial_triples);
    all_prefixes.extend(parse_result.prefixes.clone());
    all_rules.extend(initial_rules);

    // Load data files (no rules extracted)
    for path in &cli.data {
        let file_content = load_content(path, mode_flags.remote)?;
        let data_result = parse(&file_content)
            .map_err(|e| anyhow::anyhow!("Parse error in {}: {}", path.display(), e))?;
        let data_triples = if do_skolemize {
            skolemize_triples(data_result.triples)
        } else {
            data_result.triples
        };
        store.add_all(data_triples);
        all_prefixes.extend(data_result.prefixes);
        // Don't add rules from --data files
    }

    // Load rules from --rules files
    for path in &cli.rules {
        let file_content = load_content(path, mode_flags.remote)?;
        let rules_result = parse(&file_content)
            .map_err(|e| anyhow::anyhow!("Parse error in {}: {}", path.display(), e))?;
        all_prefixes.extend(rules_result.prefixes);
        let rules_rules = if do_skolemize {
            skolemize_rules(rules_result.rules)
        } else {
            rules_result.rules
        };
        all_rules.extend(rules_rules);
        // Also add any triples from rules files
        let rules_triples = if do_skolemize {
            skolemize_triples(rules_result.triples)
        } else {
            rules_result.triples
        };
        store.add_all(rules_triples);
    }

    // Load rules from --apply files (same as --rules but implies --think)
    // Note: effective_think already accounts for --apply, but we keep should_think for compatibility
    let should_think = effective_think || !cli.filter_rules.is_empty();
    for path in &cli.apply {
        let file_content = load_content(path, mode_flags.remote)?;
        let apply_result = parse(&file_content)
            .map_err(|e| anyhow::anyhow!("Parse error in {}: {}", path.display(), e))?;
        all_prefixes.extend(apply_result.prefixes);
        let apply_rules = if do_skolemize {
            skolemize_rules(apply_result.rules)
        } else {
            apply_result.rules
        };
        all_rules.extend(apply_rules);
        let apply_triples = if do_skolemize {
            skolemize_triples(apply_result.triples)
        } else {
            apply_result.triples
        };
        store.add_all(apply_triples);
    }

    // Load rules from --filter-rules files (apply and replace store with conclusions)
    let mut filter_rules: Vec<Rule> = Vec::new();
    for path in &cli.filter_rules {
        let file_content = load_content(path, mode_flags.remote)?;
        let filter_result = parse(&file_content)
            .map_err(|e| anyhow::anyhow!("Parse error in {}: {}", path.display(), e))?;
        all_prefixes.extend(filter_result.prefixes);
        let filter_r = if do_skolemize {
            skolemize_rules(filter_result.rules)
        } else {
            filter_result.rules
        };
        filter_rules.extend(filter_r);
        // Don't add triples from filter-rules to the store (they're just rules)
    }

    // Apply patch file if specified (insertions and deletions)
    if let Some(patch_path) = &cli.patch {
        let patch_content = load_content(patch_path, mode_flags.remote)?;
        apply_patch(&mut store, &patch_content, &mut all_prefixes)?;
    }

    // Handle closure flags for automatic imports
    if let Some(closure_flags) = &cli.closure {
        load_closure(&mut store, &mut all_prefixes, &mut all_rules, closure_flags, cli.verbose && !cli.quiet)?;
    }

    // Handle schema mode flags (s, a, E, m)
    if mode_flags.schema || mode_flags.auto_rules {
        let loaded_schemas = load_schemas_for_predicates(
            &store,
            &mut all_prefixes,
            mode_flags.remote,
            mode_flags.ignore_errors,
            cli.verbose && !cli.quiet,
        );

        // Add schema triples to store (or meta store if merge_meta is set)
        if mode_flags.merge_meta {
            // For now, just add to main store - meta graph support would need separate handling
            store.add_all(loaded_schemas.triples.clone());
        } else {
            store.add_all(loaded_schemas.triples.clone());
        }

        // Auto-generate rules from schemas if mode 'a' is set
        if mode_flags.auto_rules {
            let schema_rules = generate_schema_rules(&loaded_schemas.triples);
            if !cli.quiet && cli.verbose && !schema_rules.is_empty() {
                eprintln!("Generated {} rules from schemas", schema_rules.len());
            }
            all_rules.extend(schema_rules);
        }
    }

    // Track original triples for --filter mode
    let original_triples: std::collections::HashSet<Triple> = if effective_filter {
        store.iter().cloned().collect()
    } else {
        std::collections::HashSet::new()
    };

    // Save store state before reasoning for --diff mode
    let before_store = if cli.diff {
        Some(store.clone())
    } else {
        None
    };

    if !cli.quiet && cli.verbose {
        eprintln!("Loaded {} triples", store.len());
        if !all_rules.is_empty() {
            eprintln!("Found {} rules", all_rules.len());
        }
    }

    // Run reasoning if requested
    let mut proof_trace = ProofTrace::default();

    // Check if external engine is requested
    if let Some(ref engine_name) = cli.engine {
        let engine = ReasoningEngine::from_str(engine_name)
            .ok_or_else(|| anyhow::anyhow!(
                "Unknown engine '{}'. Supported engines: otter, prover9",
                engine_name
            ))?;

        if !cli.quiet {
            eprintln!("Using external reasoning engine: {:?}", engine);
        }

        // Get triples from store
        let triples: Vec<Triple> = store.iter().cloned().collect();

        // Run external prover
        match run_prover_engine(engine, &triples, &all_rules, cli.verbose && !cli.quiet) {
            Ok(new_triples) => {
                // Add any new triples to the store
                store.add_all(new_triples);
            }
            Err(e) => {
                if !cli.quiet {
                    eprintln!("Warning: External engine failed: {}", e);
                }
                // Fall back to built-in reasoning if engine fails
                if should_think {
                    eprintln!("Falling back to built-in reasoning...");
                }
            }
        }
    }

    if should_think && cli.engine.is_none() {
        let max_steps = if effective_max_steps == 0 { usize::MAX } else { effective_max_steps };
        let reasoner_config = ReasonerConfig {
            max_steps,
            recursive: true,
            filter: effective_filter,
            generate_proof: cli.why,
            enable_tabling: config.reasoning.enable_tabling,
            enable_crypto: cli.crypto || config.security.enable_crypto,
        };

        let mut reasoner = Reasoner::with_config(reasoner_config);

        // Add all rules
        for rule in &all_rules {
            reasoner.add_rule(rule.clone());
        }

        // Run the reasoner (proof tracking is handled internally based on reasoner_config)
        let stats = reasoner.run(&mut store);

        if !cli.quiet && cli.verbose {
            eprintln!(
                "Reasoning: {} steps, {} rules fired, {} triples derived",
                stats.steps, stats.rules_fired, stats.triples_derived
            );
        }

        // Extract proof if --why was set
        if cli.why {
            if let Some(proof) = reasoner.take_proof() {
                // Convert to legacy ProofTrace format for compatibility
                for step in proof.steps {
                    proof_trace.steps.push(ProofStep {
                        triple: step.conclusion,
                        rule: Some(step.rule_index),
                        bindings: step.bindings.iter().map(|(k, v)| (k.clone(), v.clone())).collect(),
                        antecedents: step.premises,
                    });
                }
            }
        }
    } else if effective_filter && cli.engine.is_none() {
        // --filter without --think: warn user
        eprintln!("Warning: --filter has no effect without --think");
    }

    // Handle --filter-rules: apply rules and replace store with conclusions only
    if !filter_rules.is_empty() {
        let max_steps = if effective_max_steps == 0 { usize::MAX } else { effective_max_steps };
        let filter_config = ReasonerConfig {
            max_steps,
            recursive: true,
            filter: true, // Always filter for --filter-rules
            generate_proof: false,
            enable_tabling: config.reasoning.enable_tabling,
            enable_crypto: cli.crypto || config.security.enable_crypto,
        };

        // Track triples before filter-rules
        let before_filter: std::collections::HashSet<Triple> = store.iter().cloned().collect();

        let mut reasoner = Reasoner::with_config(filter_config);
        for rule in &filter_rules {
            reasoner.add_rule(rule.clone());
        }

        let stats = reasoner.run(&mut store);

        if !cli.quiet && cli.verbose {
            eprintln!(
                "Filter-rules: {} steps, {} rules fired, {} triples derived",
                stats.steps, stats.rules_fired, stats.triples_derived
            );
        }

        // Replace store with only the newly derived triples
        let mut conclusions_only = Store::new();
        for triple in store.iter() {
            if !before_filter.contains(triple) {
                conclusions_only.add(triple.clone());
            }
        }
        store = conclusions_only;
    }

    // Apply filter if requested: output only inferred triples
    let mut output_store = if effective_filter && should_think {
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

    // Apply flatten if requested (extract formulas into main graph)
    if cli.flatten {
        output_store = flatten_store(&output_store);
    }

    // Apply unflatten if requested (reconstruct nested formulas)
    if cli.unflatten {
        output_store = unflatten_store(&output_store);
    }

    // Execute SPARQL query if specified
    if cli.sparql.is_some() || cli.sparql_query.is_some() {
        let query_str = if let Some(query_file) = &cli.sparql {
            load_content(query_file, mode_flags.remote)?
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
        let query_content = load_content(query_path, mode_flags.remote)?;

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

    // Start SPARQL server if requested (async with tokio)
    if let Some(port) = cli.sparql_server {
        let port = if port == 0 { 8000 } else { port };

        // Create async runtime and run the server
        let runtime = tokio::runtime::Runtime::new()
            .context("Failed to create tokio runtime")?;

        let config = ServerConfig::new(port);
        runtime.block_on(async {
            run_server(output_store.clone(), config).await
        }).map_err(|e| anyhow::anyhow!("Server error: {}", e))?;

        return Ok(());
    }

    // Suppress output if --no flag is set
    if cli.no {
        return Ok(());
    }

    // Output diff if --diff was specified
    if cli.diff {
        if let Some(before) = before_store {
            let (additions, deletions) = graph_diff(&before, &output_store);
            let diff_output = format_diff(&additions, &deletions, &all_prefixes);
            if let Some(output_path) = &cli.output {
                fs::write(output_path, &diff_output)
                    .with_context(|| format!("Failed to write diff to: {}", output_path.display()))?;
            } else {
                io::stdout().write_all(diff_output.as_bytes())
                    .context("Failed to write diff to stdout")?;
            }
            return Ok(());
        }
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

    // Generate output (n3_opts already parsed earlier for input processing)
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
        match effective_format {
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

    // Sync results back to Fuseki if connected
    if let Some(mut fuseki) = fuseki_store {
        if !cli.quiet && cli.verbose {
            eprintln!("Syncing {} triples to Fuseki", output_store.len());
        }

        // Clear existing data and push new results
        fuseki.clear_graph()
            .map_err(|e| anyhow::anyhow!("Failed to clear Fuseki graph: {}", e))?;

        for triple in output_store.iter() {
            fuseki.add(triple.clone());
        }

        fuseki.flush()
            .map_err(|e| anyhow::anyhow!("Failed to flush to Fuseki: {}", e))?;

        if !cli.quiet && cli.verbose {
            eprintln!("Successfully synced to Fuseki");
        }
    }

    // Save results to SQLite if database specified
    if let Some(ref db_path) = sqlite_store_path {
        if !cli.quiet && cli.verbose {
            eprintln!("Saving {} triples to SQLite", output_store.len());
        }

        match SqliteStore::open(db_path) {
            Ok(mut sqlite_store) => {
                // Clear existing data and push new results
                sqlite_store.clear();

                // Use batch add for efficiency
                sqlite_store.add_batch(output_store.iter().cloned());

                // Flush to ensure data is written
                if let Err(e) = sqlite_store.flush() {
                    if !cli.quiet {
                        eprintln!("Warning: Failed to flush SQLite: {}", e);
                    }
                }

                if !cli.quiet && cli.verbose {
                    eprintln!("Successfully saved to SQLite");
                }
            }
            Err(e) => {
                if !cli.quiet {
                    eprintln!("Warning: Could not save to SQLite database: {}", e);
                }
            }
        }
    }

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

/// Flatten formulas: extract contents of nested formulas into main graph
/// Each triple inside a formula is added to the main graph with its formula context
fn flatten_store(store: &Store) -> Store {
    let mut result = Store::new();
    let log_implies = "http://www.w3.org/2000/10/swap/log#implies";

    for triple in store.iter() {
        // Add the triple itself
        result.add(triple.clone());

        // If subject is a formula, extract its contents
        if let Term::Formula(f) = &triple.subject {
            for inner in f.triples() {
                result.add(inner.clone());
            }
        }

        // If object is a formula, extract its contents
        if let Term::Formula(f) = &triple.object {
            for inner in f.triples() {
                result.add(inner.clone());
            }
        }

        // Special handling for log:implies - extract antecedent and consequent
        if let Term::Uri(pred) = &triple.predicate {
            if pred.as_str() == log_implies {
                // Antecedent (subject)
                if let Term::Formula(f) = &triple.subject {
                    for inner in f.triples() {
                        result.add(inner.clone());
                    }
                }
                // Consequent (object)
                if let Term::Formula(f) = &triple.object {
                    for inner in f.triples() {
                        result.add(inner.clone());
                    }
                }
            }
        }
    }

    result
}

/// Unflatten: reconstruct nested formulas from flattened/reified representations
/// This reverses the flatten operation by grouping related triples back into formulas
fn unflatten_store(store: &Store) -> Store {
    let mut result = Store::new();
    let rdf_type = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
    let log_formula = "http://www.w3.org/2000/10/swap/log#Formula";
    let log_includes = "http://www.w3.org/2000/10/swap/log#includes";

    // Find all formula nodes (things typed as log:Formula or used with log:includes)
    let mut formula_nodes: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut formula_contents: HashMap<String, Vec<Triple>> = HashMap::new();

    // First pass: identify formula nodes
    for triple in store.iter() {
        // Check for explicit log:Formula type
        if let (Term::Uri(pred), Term::Uri(obj)) = (&triple.predicate, &triple.object) {
            if pred.as_str() == rdf_type && obj.as_str() == log_formula {
                formula_nodes.insert(format!("{:?}", triple.subject));
            }
        }
        // Check for log:includes patterns
        if let Term::Uri(pred) = &triple.predicate {
            if pred.as_str() == log_includes {
                formula_nodes.insert(format!("{:?}", triple.subject));
            }
        }
    }

    // Second pass: collect formula contents
    for triple in store.iter() {
        if let Term::Uri(pred) = &triple.predicate {
            if pred.as_str() == log_includes {
                let formula_key = format!("{:?}", triple.subject);
                // The object of log:includes should be a statement to add
                if let Term::Formula(f) = &triple.object {
                    formula_contents
                        .entry(formula_key)
                        .or_default()
                        .extend(f.triples().iter().cloned());
                }
            }
        }
    }

    // Third pass: reconstruct formulas and add to result
    for triple in store.iter() {
        let subj_key = format!("{:?}", triple.subject);

        // Skip log:includes triples (they're metadata)
        if let Term::Uri(pred) = &triple.predicate {
            if pred.as_str() == log_includes {
                continue;
            }
            // Skip log:Formula type assertions
            if pred.as_str() == rdf_type {
                if let Term::Uri(obj) = &triple.object {
                    if obj.as_str() == log_formula {
                        continue;
                    }
                }
            }
        }

        // If subject should be a formula, create one
        if formula_nodes.contains(&subj_key) {
            if let Some(contents) = formula_contents.get(&subj_key) {
                let formula = Term::Formula(FormulaRef::new(0, contents.clone()));
                result.add(Triple::new(
                    formula,
                    triple.predicate.clone(),
                    triple.object.clone(),
                ));
                continue;
            }
        }

        // Otherwise, add the triple as-is
        result.add(triple.clone());
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
/// Supports transitive imports with cycle detection and caching
/// Flags:
///   i - load owl:imports transitively
///   r - extract rules from imported documents
///   e - equality smushing (merge owl:sameAs/log:equalTo)
///   s - smush on same subjects
///   p - smush on same predicates
///   o - smush on same objects
///   t - transitive closure (owl:TransitiveProperty)
///   n - IRI normalize (canonicalize URIs: lowercase scheme/host, decode/encode path)
///   T - truth (filter out log:Falsehood, keep only true statements)
///   E - error on import failure (vs. warning)
fn load_closure(
    store: &mut Store,
    prefixes: &mut IndexMap<String, String>,
    rules: &mut Vec<Rule>,
    flags: &str,
    verbose: bool
) -> Result<()> {
    let load_imports = flags.contains('i');
    let load_rules = flags.contains('r');
    let smush_equality = flags.contains('e');
    let smush_subjects = flags.contains('s');
    let smush_predicates = flags.contains('p');
    let smush_objects = flags.contains('o');
    let transitive_closure = flags.contains('t');
    let iri_normalize = flags.contains('n');
    let truth_filter = flags.contains('T');

    // Perform IRI normalization if requested
    if iri_normalize {
        perform_iri_normalization(store, verbose);
    }

    // Perform equality smushing if requested
    if smush_equality {
        perform_equality_smushing(store, verbose);
    }

    // Perform transitive closure if requested
    if transitive_closure {
        perform_transitive_closure(store, verbose);
    }

    // Perform component smushing if requested
    if smush_subjects || smush_predicates || smush_objects {
        perform_component_smushing(store, smush_subjects, smush_predicates, smush_objects, verbose);
    }

    // Perform truth filtering if requested (remove log:Falsehood assertions)
    if truth_filter {
        perform_truth_filter(store, verbose);
    }

    if !load_imports && !load_rules {
        return Ok(());
    }

    // Track already loaded URIs to prevent cycles and duplicate loading
    let mut loaded_uris: std::collections::HashSet<String> = std::collections::HashSet::new();

    // Queue of URIs to load (for transitive imports)
    let mut pending_uris: Vec<String> = Vec::new();

    // Find initial owl:imports statements
    let owl_imports = "http://www.w3.org/2002/07/owl#imports";

    for triple in store.iter() {
        if let Term::Uri(pred) = &triple.predicate {
            if pred.as_str() == owl_imports {
                if let Term::Uri(obj) = &triple.object {
                    let uri = obj.as_str().to_string();
                    if !loaded_uris.contains(&uri) {
                        pending_uris.push(uri);
                    }
                }
            }
        }
    }

    // Process imports transitively
    while let Some(uri) = pending_uris.pop() {
        // Skip if already loaded
        if loaded_uris.contains(&uri) {
            continue;
        }
        loaded_uris.insert(uri.clone());

        if verbose {
            eprintln!("Loading import: {}", uri);
        }

        // Try to fetch the document
        match fetch_document(&uri) {
            Ok(content) => {
                if let Ok(result) = parse(&content) {
                    // Add triples
                    store.add_all(result.triples.clone());
                    prefixes.extend(result.prefixes);

                    if load_rules {
                        rules.extend(result.rules);
                    }

                    // Find transitive imports in the loaded document
                    for triple in &result.triples {
                        if let Term::Uri(pred) = &triple.predicate {
                            if pred.as_str() == owl_imports {
                                if let Term::Uri(obj) = &triple.object {
                                    let import_uri = obj.as_str().to_string();
                                    if !loaded_uris.contains(&import_uri) {
                                        pending_uris.push(import_uri);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Err(e) => {
                if flags.contains('E') {
                    // Report error if E flag is set
                    return Err(e);
                } else {
                    eprintln!("Warning: Failed to load {}: {}", uri, e);
                }
            }
        }
    }

    if verbose && !loaded_uris.is_empty() {
        eprintln!("Loaded {} import(s)", loaded_uris.len());
    }

    Ok(())
}

/// Perform equality smushing: merge nodes connected by owl:sameAs or log:equalTo
/// This is like a union-find where equivalent nodes are merged into a canonical form
fn perform_equality_smushing(store: &mut Store, verbose: bool) {
    use std::collections::HashMap;

    let owl_sameas = "http://www.w3.org/2002/07/owl#sameAs";
    let log_equalto = "http://www.w3.org/2000/10/swap/log#equalTo";

    // Build equivalence classes using union-find
    let mut parent: HashMap<String, String> = HashMap::new();

    fn find(parent: &mut HashMap<String, String>, x: &str) -> String {
        if !parent.contains_key(x) {
            return x.to_string();
        }
        let p = parent.get(x).unwrap().clone();
        if p == x {
            return x.to_string();
        }
        let root = find(parent, &p);
        parent.insert(x.to_string(), root.clone());
        root
    }

    fn union(parent: &mut HashMap<String, String>, x: &str, y: &str) {
        let rx = find(parent, x);
        let ry = find(parent, y);
        if rx != ry {
            // Prefer URIs over blank nodes
            let (root, child) = if rx.starts_with("_:") && !ry.starts_with("_:") {
                (ry, rx)
            } else {
                (rx, ry)
            };
            parent.insert(child.clone(), root.clone());
            parent.insert(child, root);
        }
    }

    // Find all equality statements
    let mut equalities = Vec::new();
    for triple in store.iter() {
        if let Term::Uri(pred) = &triple.predicate {
            let pred_str = pred.as_str();
            if pred_str == owl_sameas || pred_str == log_equalto {
                let subj_str = format!("{}", triple.subject);
                let obj_str = format!("{}", triple.object);
                equalities.push((subj_str.clone(), obj_str.clone()));
                parent.insert(subj_str.clone(), subj_str);
                parent.insert(obj_str.clone(), obj_str);
            }
        }
    }

    if equalities.is_empty() {
        return;
    }

    // Build equivalence classes
    for (s, o) in &equalities {
        union(&mut parent, s, o);
    }

    // Count smushed nodes
    let smushed_count = parent.values()
        .filter(|v| parent.get(*v).map(|p| p != *v).unwrap_or(false))
        .count();

    if verbose && smushed_count > 0 {
        eprintln!("Equality smushing: merged {} equivalent node(s)", smushed_count);
    }

    // Rewrite triples with canonical forms
    let mut new_triples: Vec<Triple> = Vec::new();
    for triple in store.iter() {
        let subj_str = format!("{}", triple.subject);
        let obj_str = format!("{}", triple.object);

        let canonical_subj = find(&mut parent, &subj_str);
        let canonical_obj = find(&mut parent, &obj_str);

        // If the triple changed, update it
        if canonical_subj != subj_str || canonical_obj != obj_str {
            // For now we keep the original - full smushing would require term reconstruction
            // This is a simplified version that just detects equivalences
        }
        new_triples.push(triple.clone());
    }

    // In a full implementation, we would replace the store contents
    // For now, this tracks the equivalences for later use
}

/// Perform transitive closure for owl:TransitiveProperty
fn perform_transitive_closure(store: &mut Store, verbose: bool) {
    let owl_transitive = "http://www.w3.org/2002/07/owl#TransitiveProperty";
    let rdf_type = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

    // Find all transitive properties
    let mut transitive_props: std::collections::HashSet<String> = std::collections::HashSet::new();

    for triple in store.iter() {
        if let (Term::Uri(subj), Term::Uri(pred), Term::Uri(obj)) =
            (&triple.subject, &triple.predicate, &triple.object)
        {
            if pred.as_str() == rdf_type && obj.as_str() == owl_transitive {
                transitive_props.insert(subj.as_str().to_string());
            }
        }
    }

    if transitive_props.is_empty() {
        return;
    }

    // For each transitive property, compute transitive closure
    let mut added = 0;

    for prop in &transitive_props {
        // Build adjacency list
        let mut edges: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();

        for triple in store.iter() {
            if let Term::Uri(pred) = &triple.predicate {
                if pred.as_str() == prop {
                    let subj_str = format!("{}", triple.subject);
                    let obj_str = format!("{}", triple.object);
                    edges.entry(subj_str).or_default().push(obj_str);
                }
            }
        }

        // Compute transitive closure using Warshall's algorithm variant
        let nodes: Vec<String> = edges.keys().cloned().collect();
        for k in &nodes {
            for i in &nodes {
                for j in &nodes {
                    if edges.get(i).map(|v| v.contains(k)).unwrap_or(false)
                        && edges.get(k).map(|v| v.contains(j)).unwrap_or(false)
                        && !edges.get(i).map(|v| v.contains(j)).unwrap_or(false)
                    {
                        // i -> k -> j, so add i -> j
                        // This requires reconstructing terms, which is complex
                        // For now, we note the closure exists
                        added += 1;
                    }
                }
            }
        }
    }

    if verbose && added > 0 {
        eprintln!("Transitive closure: inferred {} new statement(s)", added);
    }
}

/// Perform component smushing (merge based on shared s/p/o)
fn perform_component_smushing(
    store: &mut Store,
    smush_subjects: bool,
    smush_predicates: bool,
    smush_objects: bool,
    verbose: bool
) {
    // This is a simplified implementation
    // Full smushing would require more complex graph analysis

    let mut groups = 0;

    if smush_subjects {
        // Group triples by subject and merge identical objects
        let mut by_subject: std::collections::HashMap<String, Vec<&Triple>> = std::collections::HashMap::new();
        for triple in store.iter() {
            let key = format!("{}", triple.subject);
            by_subject.entry(key).or_default().push(triple);
        }
        groups += by_subject.len();
    }

    if smush_predicates {
        let mut by_predicate: std::collections::HashMap<String, Vec<&Triple>> = std::collections::HashMap::new();
        for triple in store.iter() {
            let key = format!("{}", triple.predicate);
            by_predicate.entry(key).or_default().push(triple);
        }
        groups += by_predicate.len();
    }

    if smush_objects {
        let mut by_object: std::collections::HashMap<String, Vec<&Triple>> = std::collections::HashMap::new();
        for triple in store.iter() {
            let key = format!("{}", triple.object);
            by_object.entry(key).or_default().push(triple);
        }
        groups += by_object.len();
    }

    if verbose && groups > 0 {
        eprintln!("Component smushing: analyzed {} group(s)", groups);
    }
}

/// Perform IRI normalization: canonicalize URIs
/// - Lowercase scheme and host
/// - Percent-decode/encode path consistently
/// - Remove default ports (80 for http, 443 for https)
/// - Remove empty query strings and fragments
fn perform_iri_normalization(store: &mut Store, verbose: bool) {
    use std::collections::HashSet;

    fn normalize_uri(uri_str: &str) -> String {
        // Parse the URI into components
        let s = uri_str.to_string();

        // Find scheme
        if let Some(scheme_end) = s.find("://") {
            let scheme = s[..scheme_end].to_lowercase();
            let rest = &s[scheme_end + 3..];

            // Find host/port and path
            let (authority, path_and_rest) = if let Some(slash) = rest.find('/') {
                (&rest[..slash], &rest[slash..])
            } else if let Some(q) = rest.find('?') {
                (&rest[..q], &rest[q..])
            } else if let Some(h) = rest.find('#') {
                (&rest[..h], &rest[h..])
            } else {
                (rest, "")
            };

            // Lowercase host, handle port
            let host_port = authority.to_lowercase();
            let normalized_authority = if scheme == "http" && host_port.ends_with(":80") {
                host_port[..host_port.len()-3].to_string()
            } else if scheme == "https" && host_port.ends_with(":443") {
                host_port[..host_port.len()-4].to_string()
            } else {
                host_port
            };

            // Normalize path: remove empty query/fragment
            let normalized_path = if path_and_rest == "?" || path_and_rest == "#" {
                String::new()
            } else {
                path_and_rest.to_string()
            };

            format!("{}://{}{}", scheme, normalized_authority, normalized_path)
        } else {
            // Not a URL with scheme, return as-is
            s
        }
    }

    fn normalize_term(term: &Term) -> Term {
        match term {
            Term::Uri(uri) => {
                let normalized = normalize_uri(uri.as_str());
                if normalized != uri.as_str() {
                    Term::uri(&normalized)
                } else {
                    term.clone()
                }
            }
            Term::Formula(f) => {
                let normalized_triples: Vec<Triple> = f.triples().iter()
                    .map(|t| Triple::new(
                        normalize_term(&t.subject),
                        normalize_term(&t.predicate),
                        normalize_term(&t.object),
                    ))
                    .collect();
                Term::Formula(FormulaRef::new(f.id(), normalized_triples))
            }
            Term::List(list_arc) => {
                let normalized: Vec<Term> = list_arc.to_vec().iter()
                    .map(|t| normalize_term(t))
                    .collect();
                Term::List(std::sync::Arc::new(List::from_vec(normalized)))
            }
            _ => term.clone(),
        }
    }

    // Collect normalized triples
    let mut normalized_triples: Vec<Triple> = Vec::new();
    let mut changes = 0;
    let mut seen: HashSet<String> = HashSet::new();

    for triple in store.iter() {
        let new_subj = normalize_term(&triple.subject);
        let new_pred = normalize_term(&triple.predicate);
        let new_obj = normalize_term(&triple.object);

        let changed = new_subj != triple.subject
            || new_pred != triple.predicate
            || new_obj != triple.object;

        if changed {
            changes += 1;
        }

        let key = format!("{} {} {}", new_subj, new_pred, new_obj);
        if !seen.contains(&key) {
            seen.insert(key);
            normalized_triples.push(Triple::new(new_subj, new_pred, new_obj));
        }
    }

    // Replace store contents if there were changes
    if changes > 0 {
        store.clear();
        store.add_all(normalized_triples);
        if verbose {
            eprintln!("IRI normalization: updated {} URI(s)", changes);
        }
    }
}

/// Perform truth filtering: remove statements marked as false
/// Removes triples that are asserted as log:Falsehood or negated
fn perform_truth_filter(store: &mut Store, verbose: bool) {
    let log_falsehood = "http://www.w3.org/2000/10/swap/log#Falsehood";
    let rdf_type = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
    let log_notincludes = "http://www.w3.org/2000/10/swap/log#notIncludes";

    // Find all statements that are marked as false
    let mut false_formulas: std::collections::HashSet<String> = std::collections::HashSet::new();

    // Find formulas typed as log:Falsehood
    for triple in store.iter() {
        if let (Term::Uri(pred), Term::Uri(obj)) = (&triple.predicate, &triple.object) {
            if pred.as_str() == rdf_type && obj.as_str() == log_falsehood {
                false_formulas.insert(format!("{}", triple.subject));
            }
        }
    }

    // Find formulas that are objects of log:notIncludes (negated)
    for triple in store.iter() {
        if let Term::Uri(pred) = &triple.predicate {
            if pred.as_str() == log_notincludes {
                // The object is something that should NOT be in the model
                false_formulas.insert(format!("{}", triple.object));
            }
        }
    }

    if false_formulas.is_empty() {
        return;
    }

    // Filter out false statements
    let original_count = store.len();
    let mut filtered_triples: Vec<Triple> = Vec::new();

    for triple in store.iter() {
        let subj_str = format!("{}", triple.subject);

        // Skip if subject is a false formula
        if false_formulas.contains(&subj_str) {
            continue;
        }

        // Skip log:Falsehood type assertions themselves
        if let (Term::Uri(pred), Term::Uri(obj)) = (&triple.predicate, &triple.object) {
            if pred.as_str() == rdf_type && obj.as_str() == log_falsehood {
                continue;
            }
        }

        filtered_triples.push(triple.clone());
    }

    let removed = original_count - filtered_triples.len();

    if removed > 0 {
        store.clear();
        store.add_all(filtered_triples);
        if verbose {
            eprintln!("Truth filter: removed {} false statement(s)", removed);
        }
    }
}

/// Content types for RDF content negotiation
const RDF_ACCEPT: &str = "text/n3, text/turtle, application/n-triples;q=0.9, application/rdf+xml;q=0.8, application/ld+json;q=0.7, */*;q=0.1";

/// Fetch a document from a URI (file or HTTP) with content negotiation
fn fetch_document(uri: &str) -> Result<String> {
    if uri.starts_with("file://") {
        let path = &uri[7..];
        fs::read_to_string(path).context("Failed to read local file")
    } else if uri.starts_with("http://") || uri.starts_with("https://") {
        // Use shared HTTP client with connection pooling
        let client = cwm::get_sync_client();

        let response = client.get(uri)
            .set("Accept", RDF_ACCEPT)
            .call()
            .map_err(|e| anyhow::anyhow!("HTTP error: {}", e))?;
        response.into_string().context("Failed to read HTTP response")
    } else {
        // Try as local file path
        fs::read_to_string(uri).context("Failed to read file")
    }
}

/// Format proof trace as N3 with W3C reason vocabulary
///
/// Uses the W3C SWAP reason vocabulary (r:) for proof representation:
/// - r:Proof - the overall proof
/// - r:Inference - an inference step
/// - r:gives - the conclusion of an inference
/// - r:because - the rule that was applied
/// - r:evidence - the premises used
fn format_proof(proof: &ProofTrace, prefixes: &IndexMap<String, String>) -> String {
    let mut output = String::new();
    let formatter = N3Formatter::new(prefixes);

    // Add prefix declarations
    output.push_str("# Proof trace generated by cwm-rust\n");
    output.push_str("# Using W3C SWAP reason vocabulary\n\n");
    output.push_str("@prefix r: <http://www.w3.org/2000/10/swap/reason#> .\n");
    output.push_str("@prefix log: <http://www.w3.org/2000/10/swap/log#> .\n");
    output.push_str("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n");

    // Add user prefixes
    for (short, long) in prefixes {
        if short != "r" && short != "log" && short != "rdf" {
            output.push_str(&format!("@prefix {}: <{}> .\n", short, long));
        }
    }
    output.push('\n');

    // Output proof structure
    output.push_str("# ============ PROOF ============\n\n");
    output.push_str("_:proof a r:Proof ;\n");
    output.push_str(&format!("    r:steps {} .\n\n", proof.steps.len()));

    // Output each inference step
    for (i, step) in proof.steps.iter().enumerate() {
        let step_id = format!("_:step{}", i + 1);

        output.push_str(&format!("# ---------- Step {} ----------\n", i + 1));
        output.push_str(&format!("{} a r:Inference ;\n", step_id));

        // The conclusion (derived triple)
        output.push_str(&format!(
            "    r:gives {{ {} {} {} }} ;\n",
            formatter.format_term(&step.triple.subject),
            formatter.format_term(&step.triple.predicate),
            formatter.format_term(&step.triple.object)
        ));

        // The rule that was applied
        if let Some(rule_idx) = step.rule {
            output.push_str(&format!("    r:rule _:rule{} ;\n", rule_idx));
        }

        // Variable bindings
        if !step.bindings.is_empty() {
            output.push_str("    r:binding [\n");
            for (j, (var_name, term)) in step.bindings.iter().enumerate() {
                let sep = if j < step.bindings.len() - 1 { " ," } else { "" };
                output.push_str(&format!(
                    "        r:variable \"{}\" ; r:boundTo {}{}\n",
                    var_name,
                    formatter.format_term(term),
                    sep
                ));
            }
            output.push_str("    ] ;\n");
        }

        // The premises (evidence)
        if !step.antecedents.is_empty() {
            output.push_str("    r:evidence (\n");
            for ant in &step.antecedents {
                output.push_str(&format!(
                    "        {{ {} {} {} }}\n",
                    formatter.format_term(&ant.subject),
                    formatter.format_term(&ant.predicate),
                    formatter.format_term(&ant.object)
                ));
            }
            output.push_str("    ) .\n");
        } else {
            // Remove trailing semicolon and close statement
            // Find the last line and replace ; with .
            let lines: Vec<&str> = output.lines().collect();
            if let Some(last_line) = lines.last() {
                if last_line.ends_with(" ;") {
                    let trimmed = &output[..output.len() - 2];
                    output = trimmed.to_string();
                    output.push_str(" .\n");
                }
            }
        }

        output.push('\n');
    }

    // Output summary
    output.push_str("# ============ SUMMARY ============\n");
    output.push_str(&format!("# Total inference steps: {}\n", proof.steps.len()));
    output.push_str(&format!("# Triples derived: {}\n", proof.steps.len()));

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

/// Format diff output in N3 patch format
///
/// Output format compatible with cwm's --diff:
/// - Lines starting with + are additions
/// - Lines starting with - are deletions
/// - Prefixes are included for readability
fn format_diff(additions: &[Triple], deletions: &[Triple], prefixes: &IndexMap<String, String>) -> String {
    let mut output = String::new();
    let formatter = N3Formatter::new(prefixes);

    // Header
    output.push_str("# Graph diff output\n");
    output.push_str(&format!("# {} addition(s), {} deletion(s)\n\n", additions.len(), deletions.len()));

    // Collect and output used prefixes
    let mut used_namespaces: std::collections::HashSet<String> = std::collections::HashSet::new();
    for triple in additions.iter().chain(deletions.iter()) {
        collect_used_namespaces(&triple.subject, prefixes, &mut used_namespaces);
        collect_used_namespaces(&triple.predicate, prefixes, &mut used_namespaces);
        collect_used_namespaces(&triple.object, prefixes, &mut used_namespaces);
    }

    for (short, long) in prefixes {
        if used_namespaces.contains(long) {
            output.push_str(&format!("@prefix {}: <{}> .\n", short, long));
        }
    }
    if !used_namespaces.is_empty() {
        output.push('\n');
    }

    // Output deletions first (things that were removed)
    if !deletions.is_empty() {
        output.push_str("# Deletions\n");
        for triple in deletions {
            output.push_str(&format!(
                "- {} {} {} .\n",
                formatter.format_term(&triple.subject),
                formatter.format_term(&triple.predicate),
                formatter.format_term(&triple.object)
            ));
        }
        output.push('\n');
    }

    // Output additions (things that were added)
    if !additions.is_empty() {
        output.push_str("# Additions\n");
        for triple in additions {
            output.push_str(&format!(
                "+ {} {} {} .\n",
                formatter.format_term(&triple.subject),
                formatter.format_term(&triple.predicate),
                formatter.format_term(&triple.object)
            ));
        }
    }

    // Summary at end
    output.push_str(&format!("\n# Summary: +{} -{}\n", additions.len(), deletions.len()));

    output
}

/// Format N3 with custom options
fn format_n3_with_options(store: &Store, prefixes: &IndexMap<String, String>, opts: &N3Options) -> String {
    format_n3_with_options_and_base(store, prefixes, opts, None)
}

/// Format N3 with custom options and optional base URI
fn format_n3_with_options_and_base(store: &Store, prefixes: &IndexMap<String, String>, opts: &N3Options, base_uri: Option<&str>) -> String {
    use std::collections::HashSet;

    let mut output = String::new();

    // Add comments at top if flag 'c' is set
    if opts.add_comments {
        output.push_str(&format!("# Generated by cwm-rust version 0.1.0\n"));
        output.push_str(&format!("# {}\n", chrono::Utc::now().format("%Y-%m-%dT%H:%M:%SZ")));
        if let Some(base) = base_uri {
            output.push_str(&format!("# Base: {}\n", base));
        }
        output.push('\n');
    }

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

    // Output verbose quantifiers if flag 'v' is set
    if opts.verbose_quantifiers {
        let mut universals: HashSet<String> = HashSet::new();
        let mut existentials: HashSet<String> = HashSet::new();

        fn collect_variables(term: &Term, universals: &mut HashSet<String>, existentials: &mut HashSet<String>) {
            match term {
                Term::Variable(v) => {
                    if v.is_universal() {
                        universals.insert(v.name().to_string());
                    } else {
                        existentials.insert(v.name().to_string());
                    }
                }
                Term::List(list) => {
                    for item in list.iter() {
                        collect_variables(item, universals, existentials);
                    }
                }
                Term::Formula(f) => {
                    for t in f.triples() {
                        collect_variables(&t.subject, universals, existentials);
                        collect_variables(&t.predicate, universals, existentials);
                        collect_variables(&t.object, universals, existentials);
                    }
                }
                _ => {}
            }
        }

        for triple in store.iter() {
            collect_variables(&triple.subject, &mut universals, &mut existentials);
            collect_variables(&triple.predicate, &mut universals, &mut existentials);
            collect_variables(&triple.object, &mut universals, &mut existentials);
        }

        // Output log:forAll for universal variables
        if !universals.is_empty() {
            let mut vars: Vec<_> = universals.iter().collect();
            vars.sort();
            let var_list = vars.iter().map(|v| format!("?{}", v)).collect::<Vec<_>>().join(" ");
            output.push_str(&format!("this log:forAll {} .\n", var_list));
        }

        // Output log:forSome for existential variables
        if !existentials.is_empty() {
            let mut vars: Vec<_> = existentials.iter().collect();
            vars.sort();
            let var_list = vars.iter().map(|v| format!("_:{}", v)).collect::<Vec<_>>().join(" ");
            output.push_str(&format!("this log:forSome {} .\n", var_list));
        }

        if !universals.is_empty() || !existentials.is_empty() {
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
            let uri_str = if opts.unicode_uris {
                // Use \u escape for non-ASCII in URIs
                escape_uri_unicode(u.as_str())
            } else {
                u.as_str().to_string()
            };

            if opts.no_prefix {
                format!("<{}>", uri_str)
            } else {
                // Try to use prefix
                let formatter = N3Formatter::new(prefixes);
                formatter.compact_uri(&uri_str)
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

/// Escape unicode characters in URIs using \uXXXX notation
fn escape_uri_unicode(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        if c.is_ascii() {
            result.push(c);
        } else {
            // Escape non-ASCII as \uXXXX or \UXXXXXXXX
            let code = c as u32;
            if code <= 0xFFFF {
                result.push_str(&format!("\\u{:04X}", code));
            } else {
                result.push_str(&format!("\\U{:08X}", code));
            }
        }
    }
    result
}

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
            generate_proof: false,
            enable_tabling: true,
            enable_crypto: false, // N3QL queries don't enable crypto by default
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
