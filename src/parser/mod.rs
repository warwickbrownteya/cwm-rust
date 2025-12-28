//! N3/Turtle parser
//!
//! This module implements a parser for Notation3 (N3) and Turtle syntax,
//! including N3 rules with formula syntax: `{ body } => { head } .`
//!
//! Also includes a KIF (Knowledge Interchange Format) parser for logic-based
//! knowledge representation.

pub mod kif;

pub use kif::parse_kif;

use std::sync::Arc;

use nom::{
    IResult,
    bytes::complete::{tag, take_while, take_while1, take_until},
    character::complete::{char, multispace1, digit1},
    combinator::{opt, map, value, recognize, peek},
    sequence::{delimited, preceded, pair, tuple},
    branch::alt,
    multi::many0,
};

use indexmap::IndexMap;

use crate::term::{Term, Triple, Uri, Literal, BlankNode, Variable, List, FormulaRef};
use crate::term::uri::ns;
use crate::reasoner::Rule;

/// Parser error type
#[derive(Debug, Clone, thiserror::Error)]
pub enum ParseError {
    #[error("Syntax error at position {position}: {message}")]
    Syntax { position: usize, message: String },

    #[error("Undefined prefix: {prefix}")]
    UndefinedPrefix { prefix: String },

    #[error("Invalid URI: {uri}")]
    InvalidUri { uri: String },

    #[error("Unexpected end of input")]
    UnexpectedEof,
}

/// Parser state holding prefix mappings
#[derive(Debug, Clone, Default)]
pub struct ParserState {
    /// Prefix to namespace mappings
    prefixes: IndexMap<String, String>,
    /// Base URI for relative resolution
    base: Option<Uri>,
    /// Active keywords from @keywords directive (None = all N3 keywords active)
    keywords: Option<Vec<String>>,
    /// Default namespace for bare words (set by @default directive)
    default_namespace: Option<String>,
}

impl ParserState {
    pub fn new() -> Self {
        let mut state = Self::default();
        // Add standard prefixes
        state.add_prefix("rdf", ns::RDF);
        state.add_prefix("rdfs", ns::RDFS);
        state.add_prefix("xsd", ns::XSD);
        state.add_prefix("owl", ns::OWL);
        state.add_prefix("log", ns::LOG);
        state.add_prefix("math", ns::MATH);
        state.add_prefix("string", ns::STRING);
        state.add_prefix("list", ns::LIST);
        state
    }

    pub fn with_base(base: Uri) -> Self {
        let mut state = Self::new();
        state.base = Some(base);
        state
    }

    pub fn add_prefix(&mut self, prefix: &str, namespace: &str) {
        self.prefixes.insert(prefix.to_string(), namespace.to_string());
    }

    pub fn resolve_prefix(&self, prefix: &str, local: &str) -> Result<Uri, ParseError> {
        if let Some(ns) = self.prefixes.get(prefix) {
            Ok(Uri::new(format!("{}{}", ns, local)))
        } else {
            Err(ParseError::UndefinedPrefix { prefix: prefix.to_string() })
        }
    }

    pub fn resolve_relative(&self, relative: &str) -> Uri {
        if let Some(base) = &self.base {
            base.resolve(relative)
        } else {
            Uri::new(relative.to_string())
        }
    }

    pub fn prefixes(&self) -> &IndexMap<String, String> {
        &self.prefixes
    }

    /// Set active keywords from @keywords directive
    pub fn set_keywords(&mut self, keywords: Vec<String>) {
        self.keywords = Some(keywords);
    }

    /// Check if a bare word is an active keyword
    pub fn is_keyword(&self, word: &str) -> bool {
        match &self.keywords {
            None => {
                // Default: all N3 keywords are active
                matches!(word, "a" | "is" | "of" | "true" | "false" | "has" | "this")
            }
            Some(keywords) => keywords.iter().any(|k| k == word),
        }
    }

    /// Check if keywords mode is active (i.e., bare words allowed)
    pub fn keywords_mode(&self) -> bool {
        self.keywords.is_some()
    }

    /// Set the default namespace from @default directive
    pub fn set_default_namespace(&mut self, namespace: &str) {
        self.default_namespace = Some(namespace.to_string());
        // Also register as the empty prefix
        self.prefixes.insert(String::new(), namespace.to_string());
    }

    /// Get the default namespace
    pub fn default_namespace(&self) -> Option<&str> {
        self.default_namespace.as_deref()
    }

    /// Resolve a bare word using default namespace
    pub fn resolve_bare_word(&self, word: &str) -> Option<Uri> {
        if let Some(ns) = &self.default_namespace {
            Some(Uri::new(format!("{}{}", ns, word)))
        } else if let Some(base) = &self.base {
            Some(base.resolve(&format!("#{}", word)))
        } else {
            Some(Uri::new(format!("#{}", word)))
        }
    }
}

/// Parse whitespace and comments
fn ws(input: &str) -> IResult<&str, ()> {
    value(
        (),
        many0(alt((
            value((), multispace1),
            value((), preceded(char('#'), take_while(|c| c != '\n'))),
        )))
    )(input)
}

/// Parse an IRI reference <...>
fn iri_ref(input: &str) -> IResult<&str, &str> {
    delimited(
        char('<'),
        take_while(|c| c != '>' && c != ' ' && c != '\n' && c != '\r'),
        char('>'),
    )(input)
}

/// Parse a prefixed name (prefix:local)
fn prefixed_name(input: &str) -> IResult<&str, (&str, &str)> {
    let pn_chars = |c: char| c.is_alphanumeric() || c == '_' || c == '-' || c == '.';

    let (input, prefix) = take_while(|c: char| c.is_alphanumeric() || c == '_')(input)?;
    let (input, _) = char(':')(input)?;
    let (input, local) = take_while(pn_chars)(input)?;

    Ok((input, (prefix, local)))
}

/// Parse a string literal with possible escape sequences
fn string_literal(input: &str) -> IResult<&str, String> {
    alt((
        // Long string """..."""
        map(
            delimited(
                tag("\"\"\""),
                take_until("\"\"\""),
                tag("\"\"\""),
            ),
            |s: &str| unescape_string(s)
        ),
        // Long string '''...'''
        map(
            delimited(
                tag("'''"),
                take_until("'''"),
                tag("'''"),
            ),
            |s: &str| unescape_string(s)
        ),
        // Short string "..."
        map(
            delimited(
                char('"'),
                recognize(many0(alt((
                    take_while1(|c| c != '"' && c != '\\' && c != '\n'),
                    map(pair(char('\\'), take_while1(|c: char| c.is_ascii())), |(_, _)| ""),
                )))),
                char('"'),
            ),
            |s: &str| unescape_string(s)
        ),
        // Single-quoted string '...'
        map(
            delimited(
                char('\''),
                take_while(|c| c != '\'' && c != '\n'),
                char('\''),
            ),
            |s: &str| unescape_string(s)
        ),
    ))(input)
}

/// Unescape common escape sequences
fn unescape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('\'') => result.push('\''),
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }
    result
}

/// Parse a variable ?name
fn variable(input: &str) -> IResult<&str, Variable> {
    let (input, _) = char('?')(input)?;
    let (input, name) = take_while1(|c: char| c.is_alphanumeric() || c == '_')(input)?;
    Ok((input, Variable::universal(name.to_string())))
}

/// Parse a blank node _:label
fn blank_node_label(input: &str) -> IResult<&str, BlankNode> {
    let (input, _) = tag("_:")(input)?;
    let (input, label) = take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-')(input)?;
    Ok((input, BlankNode::labeled(label.to_string())))
}

/// Parse a numeric literal
fn numeric_literal(input: &str) -> IResult<&str, Literal> {
    let (input, neg) = opt(char('-'))(input)?;
    let (input, digits) = digit1(input)?;
    let (input, decimal) = opt(pair(char('.'), digit1))(input)?;
    let (input, exp) = opt(tuple((alt((char('e'), char('E'))), opt(alt((char('+'), char('-')))), digit1)))(input)?;

    let mut value = String::new();
    if neg.is_some() {
        value.push('-');
    }
    value.push_str(digits);

    let datatype = if exp.is_some() {
        if let Some((_, frac)) = decimal {
            value.push('.');
            value.push_str(frac);
        }
        if let Some((e, sign, exp_digits)) = exp {
            value.push(e);
            if let Some(s) = sign {
                value.push(s);
            }
            value.push_str(exp_digits);
        }
        "http://www.w3.org/2001/XMLSchema#double"
    } else if let Some((_, frac)) = decimal {
        value.push('.');
        value.push_str(frac);
        "http://www.w3.org/2001/XMLSchema#decimal"
    } else {
        "http://www.w3.org/2001/XMLSchema#integer"
    };

    Ok((input, Literal::typed(value, datatype.to_string())))
}

/// Parse a boolean literal
fn boolean_literal(input: &str) -> IResult<&str, Literal> {
    alt((
        map(tag("true"), |_| Literal::typed("true".to_string(), "http://www.w3.org/2001/XMLSchema#boolean".to_string())),
        map(tag("false"), |_| Literal::typed("false".to_string(), "http://www.w3.org/2001/XMLSchema#boolean".to_string())),
    ))(input)
}

/// Parse 'a' as rdf:type
fn rdf_type_shorthand(input: &str) -> IResult<&str, ()> {
    let (input, _) = char('a')(input)?;
    // Make sure 'a' is not part of a longer word
    let (input, _) = peek(alt((
        value((), multispace1),
        value((), char('<')),
        value((), char('?')),
        value((), char('{')),
        value((), tag("_:")),
    )))(input)?;
    Ok((input, ()))
}

/// A parsed formula containing triples
#[derive(Debug, Clone, Default)]
pub struct Formula {
    pub triples: Vec<Triple>,
}

impl Formula {
    pub fn new() -> Self {
        Self::default()
    }
}

/// Result of parsing N3 content
#[derive(Debug, Clone)]
pub struct ParseResult {
    pub triples: Vec<Triple>,
    pub prefixes: IndexMap<String, String>,
    pub base: Option<String>,
    pub formulas: IndexMap<u64, Formula>,
    pub rules: Vec<Rule>,
}

/// N3 Document parser
pub struct N3Parser {
    state: ParserState,
    triples: Vec<Triple>,
    formulas: IndexMap<u64, Formula>,
    next_formula_id: u64,
}

impl N3Parser {
    pub fn new() -> Self {
        N3Parser {
            state: ParserState::new(),
            triples: Vec::new(),
            formulas: IndexMap::new(),
            next_formula_id: 0,
        }
    }

    pub fn with_base(base: &str) -> Self {
        N3Parser {
            state: ParserState::with_base(Uri::new(base.to_string())),
            triples: Vec::new(),
            formulas: IndexMap::new(),
            next_formula_id: 0,
        }
    }

    /// Allocate a new formula ID
    fn alloc_formula(&mut self) -> u64 {
        let id = self.next_formula_id;
        self.next_formula_id += 1;
        self.formulas.insert(id, Formula::new());
        id
    }

    /// Parse a complete N3 document
    pub fn parse_document(&mut self, input: &str) -> Result<(), ParseError> {
        let mut remaining = input;

        loop {
            // Skip whitespace and comments
            match ws(remaining) {
                Ok((rest, _)) => remaining = rest,
                Err(_) => {}
            }

            if remaining.is_empty() {
                break;
            }

            // Try to parse a directive or statement
            if remaining.starts_with('@') {
                remaining = self.parse_directive(remaining)?;
            } else if remaining.starts_with("PREFIX") || remaining.starts_with("BASE") {
                remaining = self.parse_sparql_directive(remaining)?;
            } else {
                remaining = self.parse_statement(remaining)?;
            }
        }

        Ok(())
    }

    /// Parse a @prefix or @base directive
    fn parse_directive<'a>(&mut self, input: &'a str) -> Result<&'a str, ParseError> {
        if input.starts_with("@prefix") {
            self.parse_prefix_directive(input)
        } else if input.starts_with("@base") {
            self.parse_base_directive(input)
        } else if input.starts_with("@forAll") {
            self.parse_forall_directive(input)
        } else if input.starts_with("@forSome") {
            self.parse_forsome_directive(input)
        } else if input.starts_with("@keywords") {
            self.parse_keywords_directive(input)
        } else if input.starts_with("@default") {
            self.parse_default_directive(input)
        } else {
            Err(ParseError::Syntax {
                position: 0,
                message: "Unknown directive".to_string(),
            })
        }
    }

    /// Parse @prefix directive
    fn parse_prefix_directive<'a>(&mut self, input: &'a str) -> Result<&'a str, ParseError> {
        let input = &input[7..]; // Skip "@prefix"
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        // Parse prefix name
        let (input, prefix) = take_while(|c: char| c.is_alphanumeric() || c == '_')(input)
            .map_err(|_: nom::Err<nom::error::Error<&str>>| ParseError::UnexpectedEof)?;
        let (input, _) = char::<&str, nom::error::Error<&str>>(':')(input)
            .map_err(|_| ParseError::Syntax { position: 0, message: "Expected ':' after prefix".to_string() })?;
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        // Parse namespace IRI
        let (input, namespace) = iri_ref(input)
            .map_err(|_| ParseError::Syntax { position: 0, message: "Expected IRI for namespace".to_string() })?;
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        // Expect '.'
        let (input, _) = char::<&str, nom::error::Error<&str>>('.')(input)
            .map_err(|_| ParseError::Syntax { position: 0, message: "Expected '.' after prefix directive".to_string() })?;

        self.state.add_prefix(prefix, namespace);
        Ok(input)
    }

    /// Parse @base directive
    fn parse_base_directive<'a>(&mut self, input: &'a str) -> Result<&'a str, ParseError> {
        let input = &input[5..]; // Skip "@base"
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        let (input, base_uri) = iri_ref(input)
            .map_err(|_| ParseError::Syntax { position: 0, message: "Expected IRI for base".to_string() })?;
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        let (input, _) = char::<&str, nom::error::Error<&str>>('.')(input)
            .map_err(|_| ParseError::Syntax { position: 0, message: "Expected '.' after base directive".to_string() })?;

        self.state.base = Some(Uri::new(base_uri.to_string()));
        Ok(input)
    }

    /// Parse @forAll directive (declares universal variables)
    fn parse_forall_directive<'a>(&mut self, input: &'a str) -> Result<&'a str, ParseError> {
        let input = &input[7..]; // Skip "@forAll"
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        // Parse variable list until '.'
        let mut remaining = input;
        loop {
            let (input, _) = ws(remaining).map_err(|_| ParseError::UnexpectedEof)?;

            if let Ok((rest, _)) = char::<&str, nom::error::Error<&str>>('.')(input) {
                return Ok(rest);
            }

            // Parse variable or prefixed name
            if let Ok((rest, _)) = variable(input) {
                remaining = rest;
            } else if let Ok((rest, _)) = prefixed_name(input) {
                remaining = rest;
            } else {
                return Err(ParseError::Syntax { position: 0, message: "Expected variable in @forAll".to_string() });
            }

            let (input, _) = ws(remaining).map_err(|_| ParseError::UnexpectedEof)?;

            // Check for comma or period
            if let Ok((rest, _)) = char::<&str, nom::error::Error<&str>>(',')(input) {
                remaining = rest;
            } else {
                remaining = input;
            }
        }
    }

    /// Parse @forSome directive (declares existential variables)
    fn parse_forsome_directive<'a>(&mut self, input: &'a str) -> Result<&'a str, ParseError> {
        let input = &input[8..]; // Skip "@forSome"
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        // Parse variable list until '.'
        let mut remaining = input;
        loop {
            let (input, _) = ws(remaining).map_err(|_| ParseError::UnexpectedEof)?;

            if let Ok((rest, _)) = char::<&str, nom::error::Error<&str>>('.')(input) {
                return Ok(rest);
            }

            // Parse variable or prefixed name
            if let Ok((rest, _)) = variable(input) {
                remaining = rest;
            } else if let Ok((rest, _)) = prefixed_name(input) {
                remaining = rest;
            } else {
                return Err(ParseError::Syntax { position: 0, message: "Expected variable in @forSome".to_string() });
            }

            let (input, _) = ws(remaining).map_err(|_| ParseError::UnexpectedEof)?;

            // Check for comma or period
            if let Ok((rest, _)) = char::<&str, nom::error::Error<&str>>(',')(input) {
                remaining = rest;
            } else {
                remaining = input;
            }
        }
    }

    /// Parse @keywords directive
    /// Syntax: @keywords is, of, a.
    /// This enables "keywords mode" where bare words are allowed and only
    /// the listed keywords are recognized as N3 keywords
    fn parse_keywords_directive<'a>(&mut self, input: &'a str) -> Result<&'a str, ParseError> {
        let input = &input[9..]; // Skip "@keywords"
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        let mut keywords = Vec::new();
        let mut remaining = input;

        loop {
            let (input, _) = ws(remaining).map_err(|_| ParseError::UnexpectedEof)?;

            // Check for period (end of directive)
            if let Ok((rest, _)) = char::<&str, nom::error::Error<&str>>('.')(input) {
                self.state.set_keywords(keywords);
                return Ok(rest);
            }

            // Parse a bare word (keyword name)
            let (rest, word) = take_while1(|c: char| c.is_alphanumeric() || c == '_')(input)
                .map_err(|_: nom::Err<nom::error::Error<&str>>| ParseError::Syntax {
                    position: 0,
                    message: "Expected keyword name in @keywords".to_string(),
                })?;

            keywords.push(word.to_string());

            let (input, _) = ws(rest).map_err(|_| ParseError::UnexpectedEof)?;

            // Check for comma or period
            if let Ok((rest, _)) = char::<&str, nom::error::Error<&str>>(',')(input) {
                remaining = rest;
            } else {
                remaining = input;
            }
        }
    }

    /// Parse @default directive
    /// Syntax: @default <namespace>.
    /// Sets the default namespace for bare words without prefix
    fn parse_default_directive<'a>(&mut self, input: &'a str) -> Result<&'a str, ParseError> {
        let input = &input[8..]; // Skip "@default"
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        // Parse namespace IRI
        let (input, namespace) = iri_ref(input)
            .map_err(|_| ParseError::Syntax { position: 0, message: "Expected IRI for default namespace".to_string() })?;
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        // Expect '.'
        let (input, _) = char::<&str, nom::error::Error<&str>>('.')(input)
            .map_err(|_| ParseError::Syntax { position: 0, message: "Expected '.' after @default directive".to_string() })?;

        self.state.set_default_namespace(namespace);
        Ok(input)
    }

    /// Parse SPARQL-style PREFIX/BASE
    fn parse_sparql_directive<'a>(&mut self, input: &'a str) -> Result<&'a str, ParseError> {
        if input.starts_with("PREFIX") {
            let input = &input[6..];
            let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

            let (input, prefix) = take_while(|c: char| c.is_alphanumeric() || c == '_')(input)
                .map_err(|_: nom::Err<nom::error::Error<&str>>| ParseError::UnexpectedEof)?;
            let (input, _) = char::<&str, nom::error::Error<&str>>(':')(input)
                .map_err(|_| ParseError::Syntax { position: 0, message: "Expected ':'".to_string() })?;
            let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

            let (input, namespace) = iri_ref(input)
                .map_err(|_| ParseError::Syntax { position: 0, message: "Expected IRI".to_string() })?;

            self.state.add_prefix(prefix, namespace);
            Ok(input)
        } else if input.starts_with("BASE") {
            let input = &input[4..];
            let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

            let (input, base_uri) = iri_ref(input)
                .map_err(|_| ParseError::Syntax { position: 0, message: "Expected IRI".to_string() })?;

            self.state.base = Some(Uri::new(base_uri.to_string()));
            Ok(input)
        } else {
            Err(ParseError::Syntax { position: 0, message: "Unknown directive".to_string() })
        }
    }

    /// Parse a statement (triple or rule)
    fn parse_statement<'a>(&mut self, input: &'a str) -> Result<&'a str, ParseError> {
        // Parse subject
        let (input, subject) = self.parse_term(input)?;
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        // Parse predicate-object list
        let input = self.parse_predicate_object_list(input, &subject)?;

        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        // Expect '.'
        let (input, _) = char::<&str, nom::error::Error<&str>>('.')(input)
            .map_err(|_| ParseError::Syntax { position: 0, message: "Expected '.' at end of statement".to_string() })?;

        Ok(input)
    }

    /// Parse predicate-object list (handles ; separator)
    fn parse_predicate_object_list<'a>(&mut self, input: &'a str, subject: &Term) -> Result<&'a str, ParseError> {
        let mut remaining = input;

        loop {
            // Parse predicate
            let (input, predicate) = self.parse_predicate(remaining)?;
            let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

            // Parse object list
            let input = self.parse_object_list(input, subject, &predicate)?;
            remaining = input;

            let (input, _) = ws(remaining).map_err(|_| ParseError::UnexpectedEof)?;

            // Check for ';' continuation
            if let Ok((rest, _)) = char::<&str, nom::error::Error<&str>>(';')(input) {
                let (rest, _) = ws(rest).map_err(|_| ParseError::UnexpectedEof)?;
                // Check if there's another predicate or if we're at the end
                if rest.starts_with('.') || rest.is_empty() {
                    remaining = rest;
                    break;
                }
                remaining = rest;
            } else {
                remaining = input;
                break;
            }
        }

        Ok(remaining)
    }

    /// Parse object list (handles , separator)
    fn parse_object_list<'a>(&mut self, input: &'a str, subject: &Term, predicate: &Term) -> Result<&'a str, ParseError> {
        let mut remaining = input;

        loop {
            // Parse object
            let (input, object) = self.parse_term(remaining)?;

            // Add triple
            self.triples.push(Triple::new(subject.clone(), predicate.clone(), object));

            let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

            // Check for ',' continuation
            if let Ok((rest, _)) = char::<&str, nom::error::Error<&str>>(',')(input) {
                let (rest, _) = ws(rest).map_err(|_| ParseError::UnexpectedEof)?;
                remaining = rest;
            } else {
                remaining = input;
                break;
            }
        }

        Ok(remaining)
    }

    /// Parse a predicate (including 'a' shorthand, '=>', 'has', 'is...of')
    fn parse_predicate<'a>(&mut self, input: &'a str) -> Result<(&'a str, Term), ParseError> {
        // Check for 'a' (rdf:type)
        if let Ok((rest, _)) = rdf_type_shorthand(input) {
            return Ok((rest, Term::uri(ns::rdf_type().as_str())));
        }

        // Check for => (log:implies)
        if input.starts_with("=>") {
            return Ok((&input[2..], Term::uri(ns::log_implies().as_str())));
        }

        // Check for <= (log:impliedBy - reverse implication)
        if input.starts_with("<=") {
            return Ok((&input[2..], Term::uri(format!("{}impliedBy", ns::LOG))));
        }

        // Check for = (owl:sameAs)
        if input.starts_with('=') && !input.starts_with("=>") {
            return Ok((&input[1..], Term::uri("http://www.w3.org/2002/07/owl#sameAs")));
        }

        // Check for 'has' keyword (syntactic sugar, just skip it)
        // "alice has parent bob" = "alice parent bob"
        if self.state.is_keyword("has") && input.starts_with("has") {
            let after = &input[3..];
            if after.starts_with(|c: char| c.is_whitespace()) {
                let (rest, _) = ws(after).map_err(|_| ParseError::UnexpectedEof)?;
                // Parse the actual predicate after 'has'
                return self.parse_term(rest);
            }
        }

        self.parse_term(input)
    }

    /// Parse a term (subject, predicate, or object)
    fn parse_term<'a>(&mut self, input: &'a str) -> Result<(&'a str, Term), ParseError> {
        // Parse the base term first
        let (remaining, base_term) = self.parse_base_term(input)?;

        // Check for path operators (! or ^)
        self.parse_path_expression(remaining, base_term)
    }

    /// Parse a base term without path operators
    fn parse_base_term<'a>(&mut self, input: &'a str) -> Result<(&'a str, Term), ParseError> {
        // Try formula { ... }
        if input.starts_with('{') {
            return self.parse_formula(input);
        }

        // Try IRI
        if let Ok((rest, uri)) = iri_ref(input) {
            let resolved = self.state.resolve_relative(uri);
            return Ok((rest, Term::Uri(Arc::new(resolved))));
        }

        // Try variable
        if let Ok((rest, var)) = variable(input) {
            return Ok((rest, Term::Variable(var)));
        }

        // Try blank node
        if let Ok((rest, blank)) = blank_node_label(input) {
            return Ok((rest, Term::BlankNode(blank)));
        }

        // Try anonymous blank node []
        if input.starts_with('[') {
            return self.parse_blank_node_property_list(input);
        }

        // Try collection ()
        if input.starts_with('(') {
            return self.parse_collection(input);
        }

        // Try prefixed name
        if let Ok((rest, (prefix, local))) = prefixed_name(input) {
            let uri = self.state.resolve_prefix(prefix, local)?;
            return Ok((rest, Term::Uri(Arc::new(uri))));
        }

        // Try literal
        if input.starts_with('"') || input.starts_with('\'') {
            return self.parse_literal(input);
        }

        // Try numeric literal
        if input.starts_with(|c: char| c.is_ascii_digit() || c == '-' || c == '+') {
            if let Ok((rest, lit)) = numeric_literal(input) {
                return Ok((rest, Term::Literal(Arc::new(lit))));
            }
        }

        // Try boolean
        if input.starts_with("true") || input.starts_with("false") {
            if let Ok((rest, lit)) = boolean_literal(input) {
                return Ok((rest, Term::Literal(Arc::new(lit))));
            }
        }

        // In keywords mode, try to parse bare words as URIs
        if self.state.keywords_mode() {
            // A bare word starts with a letter or underscore
            if input.starts_with(|c: char| c.is_alphabetic() || c == '_') {
                let (rest, word) = take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-')(input)
                    .map_err(|_: nom::Err<nom::error::Error<&str>>| ParseError::Syntax {
                        position: 0,
                        message: "Expected bare word".to_string(),
                    })?;

                // Check if it's an active keyword
                if self.state.is_keyword(word) {
                    match word {
                        "a" => return Ok((rest, Term::uri(ns::rdf_type().as_str()))),
                        "true" => return Ok((rest, Term::Literal(Arc::new(Literal::typed(
                            "true".to_string(),
                            "http://www.w3.org/2001/XMLSchema#boolean".to_string(),
                        ))))),
                        "false" => return Ok((rest, Term::Literal(Arc::new(Literal::typed(
                            "false".to_string(),
                            "http://www.w3.org/2001/XMLSchema#boolean".to_string(),
                        ))))),
                        "this" => {
                            // "this" refers to the current formula/document
                            return Ok((rest, Term::uri("#_this")));
                        }
                        // "is" and "of" are handled in parse_predicate
                        _ => {}
                    }
                }

                // Not a keyword, treat as a local name in the default namespace
                // Use @default namespace, or fall back to base URI or fragment
                let uri = self.state.resolve_bare_word(word)
                    .unwrap_or_else(|| Uri::new(format!("#{}", word)));
                return Ok((rest, Term::Uri(Arc::new(uri))));
            }
        }

        Err(ParseError::Syntax {
            position: 0,
            message: format!("Cannot parse term starting with: {}", &input[..input.len().min(20)]),
        })
    }

    /// Parse path expression operators (! for forward, ^ for backward)
    /// N3 path syntax:
    /// - subject!predicate = the object of (subject predicate ?x), returns ?x
    /// - subject^predicate = the subject of (?x predicate subject), returns ?x
    /// Paths can be chained: alice!knows!age
    fn parse_path_expression<'a>(&mut self, input: &'a str, current_term: Term) -> Result<(&'a str, Term), ParseError> {
        let mut remaining = input;
        let mut result_term = current_term;

        loop {
            // Check for path operator
            if remaining.starts_with('!') {
                // Forward path: subject!predicate means "the object of (subject predicate ?x)"
                remaining = &remaining[1..];
                let (rest, predicate) = self.parse_base_term(remaining)?;

                // Create a blank node to represent the path result
                let path_result = Term::BlankNode(BlankNode::fresh());

                // Add triple: result_term predicate path_result
                self.triples.push(Triple::new(result_term.clone(), predicate, path_result.clone()));

                result_term = path_result;
                remaining = rest;
            } else if remaining.starts_with('^') {
                // Backward path: subject^predicate means "the subject of (?x predicate subject)"
                remaining = &remaining[1..];
                let (rest, predicate) = self.parse_base_term(remaining)?;

                // Create a blank node to represent the path result
                let path_result = Term::BlankNode(BlankNode::fresh());

                // Add triple: path_result predicate result_term (reversed!)
                self.triples.push(Triple::new(path_result.clone(), predicate, result_term.clone()));

                result_term = path_result;
                remaining = rest;
            } else {
                // No more path operators
                break;
            }
        }

        Ok((remaining, result_term))
    }

    /// Parse a formula { ... }
    fn parse_formula<'a>(&mut self, input: &'a str) -> Result<(&'a str, Term), ParseError> {
        let (input, _) = char::<&str, nom::error::Error<&str>>('{')(input)
            .map_err(|_| ParseError::Syntax { position: 0, message: "Expected '{'".to_string() })?;
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        // Allocate a new formula
        let formula_id = self.alloc_formula();

        // Save current triples and parse into formula
        let outer_triples = std::mem::take(&mut self.triples);

        // Parse statements inside the formula
        let mut remaining = input;
        loop {
            let (input, _) = ws(remaining).map_err(|_| ParseError::UnexpectedEof)?;

            // Check for closing brace
            if let Ok((rest, _)) = char::<&str, nom::error::Error<&str>>('}')(input) {
                remaining = rest;
                break;
            }

            // Parse a statement inside the formula
            remaining = self.parse_formula_statement(input)?;
        }

        // Move parsed triples to the formula
        let formula_triples = std::mem::replace(&mut self.triples, outer_triples);
        if let Some(formula) = self.formulas.get_mut(&formula_id) {
            formula.triples = formula_triples.clone();
        }

        Ok((remaining, Term::Formula(FormulaRef::new(formula_id, formula_triples))))
    }

    /// Parse a statement inside a formula (no trailing dot required for last statement)
    fn parse_formula_statement<'a>(&mut self, input: &'a str) -> Result<&'a str, ParseError> {
        // Parse subject
        let (input, subject) = self.parse_term(input)?;
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        // Parse predicate-object list
        let input = self.parse_predicate_object_list(input, &subject)?;

        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        // '.' is optional before '}'
        if let Ok((rest, _)) = char::<&str, nom::error::Error<&str>>('.')(input) {
            Ok(rest)
        } else {
            Ok(input)
        }
    }

    /// Parse a literal with optional datatype or language tag
    fn parse_literal<'a>(&mut self, input: &'a str) -> Result<(&'a str, Term), ParseError> {
        let (input, value) = string_literal(input)
            .map_err(|_| ParseError::Syntax { position: 0, message: "Invalid string literal".to_string() })?;

        // Check for language tag
        if let Ok((rest, _)) = char::<&str, nom::error::Error<&str>>('@')(input) {
            let (rest, lang) = take_while1(|c: char| c.is_alphanumeric() || c == '-')(rest)
                .map_err(|_: nom::Err<nom::error::Error<&str>>| ParseError::Syntax { position: 0, message: "Invalid language tag".to_string() })?;
            return Ok((rest, Term::Literal(Arc::new(Literal::with_language(value, lang.to_string())))));
        }

        // Check for datatype
        if input.starts_with("^^") {
            let input = &input[2..];

            // IRI datatype
            if let Ok((rest, dt_uri)) = iri_ref(input) {
                return Ok((rest, Term::Literal(Arc::new(Literal::typed(value, dt_uri.to_string())))));
            }

            // Prefixed datatype
            if let Ok((rest, (prefix, local))) = prefixed_name(input) {
                let uri = self.state.resolve_prefix(prefix, local)?;
                return Ok((rest, Term::Literal(Arc::new(Literal::typed(value, uri.as_str().to_string())))));
            }
        }

        // Plain literal
        Ok((input, Term::Literal(Arc::new(Literal::plain(value)))))
    }

    /// Parse blank node property list [...]
    fn parse_blank_node_property_list<'a>(&mut self, input: &'a str) -> Result<(&'a str, Term), ParseError> {
        let (input, _) = char::<&str, nom::error::Error<&str>>('[')(input)
            .map_err(|_| ParseError::Syntax { position: 0, message: "Expected '['".to_string() })?;
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        let blank = Term::BlankNode(BlankNode::fresh());

        // Check for empty []
        if let Ok((rest, _)) = char::<&str, nom::error::Error<&str>>(']')(input) {
            return Ok((rest, blank));
        }

        // Parse property list
        let input = self.parse_predicate_object_list(input, &blank)?;
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        let (input, _) = char::<&str, nom::error::Error<&str>>(']')(input)
            .map_err(|_| ParseError::Syntax { position: 0, message: "Expected ']'".to_string() })?;

        Ok((input, blank))
    }

    /// Parse collection (...)
    fn parse_collection<'a>(&mut self, input: &'a str) -> Result<(&'a str, Term), ParseError> {
        let (input, _) = char::<&str, nom::error::Error<&str>>('(')(input)
            .map_err(|_| ParseError::Syntax { position: 0, message: "Expected '('".to_string() })?;
        let (input, _) = ws(input).map_err(|_| ParseError::UnexpectedEof)?;

        let mut items = Vec::new();
        let mut remaining = input;

        // Parse items until ')'
        loop {
            if let Ok((rest, _)) = char::<&str, nom::error::Error<&str>>(')')(remaining) {
                remaining = rest;
                break;
            }

            let (rest, term) = self.parse_term(remaining)?;
            items.push(term);

            let (rest, _) = ws(rest).map_err(|_| ParseError::UnexpectedEof)?;
            remaining = rest;
        }

        Ok((remaining, Term::List(Arc::new(List::from_vec(items)))))
    }

    /// Get the parsed triples
    pub fn triples(&self) -> &[Triple] {
        &self.triples
    }

    /// Get the parser state
    pub fn state(&self) -> &ParserState {
        &self.state
    }

    /// Get the formulas
    pub fn formulas(&self) -> &IndexMap<u64, Formula> {
        &self.formulas
    }

    /// Extract rules from parsed triples
    /// Rules are triples of the form: { body } log:implies { head }
    pub fn extract_rules(&self) -> Vec<Rule> {
        let implies_uri = ns::log_implies().as_str().to_string();
        let mut rules = Vec::new();

        for triple in &self.triples {
            // Check if predicate is log:implies
            if let Term::Uri(pred_uri) = &triple.predicate {
                if pred_uri.as_str() == implies_uri {
                    // Subject should be a formula (body)
                    // Object should be a formula (head)
                    if let (Term::Formula(body_ref), Term::Formula(head_ref)) = (&triple.subject, &triple.object) {
                        let rule = Rule::new(
                            body_ref.triples().to_vec(),
                            head_ref.triples().to_vec(),
                        );
                        rules.push(rule);
                    }
                }
            }
        }

        rules
    }

    /// Consume the parser and return results
    pub fn finish(self) -> ParseResult {
        let rules = self.extract_rules();

        ParseResult {
            triples: self.triples,
            prefixes: self.state.prefixes,
            base: self.state.base.map(|u| u.as_str().to_string()),
            formulas: self.formulas,
            rules,
        }
    }
}

impl Default for N3Parser {
    fn default() -> Self {
        Self::new()
    }
}

/// Parse N3/Turtle content
pub fn parse(input: &str) -> Result<ParseResult, ParseError> {
    let mut parser = N3Parser::new();
    parser.parse_document(input)?;
    Ok(parser.finish())
}

/// Parse a single term from a string
pub fn parse_term(input: &str, state: &ParserState) -> Result<Term, ParseError> {
    let input = input.trim();

    // Try IRI
    if input.starts_with('<') && input.ends_with('>') {
        let uri = &input[1..input.len()-1];
        return Ok(Term::uri(state.resolve_relative(uri).as_str()));
    }

    // Try variable
    if input.starts_with('?') {
        let name = &input[1..];
        return Ok(Term::universal(name));
    }

    // Try blank node
    if input.starts_with("_:") {
        let label = &input[2..];
        return Ok(Term::blank(label));
    }

    // Try prefixed name
    if let Some(colon_pos) = input.find(':') {
        if !input[..colon_pos].is_empty() {
            let prefix = &input[..colon_pos];
            let local = &input[colon_pos+1..];
            let uri = state.resolve_prefix(prefix, local)?;
            return Ok(Term::uri(uri.as_str()));
        }
    }

    // Try string literal
    if input.starts_with('"') || input.starts_with('\'') {
        let quote = input.chars().next().unwrap();
        if let Some(end) = input[1..].find(quote) {
            let value = &input[1..end+1];
            return Ok(Term::literal(value));
        }
    }

    Err(ParseError::Syntax {
        position: 0,
        message: format!("Cannot parse term: {}", input),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_iri_ref() {
        let result = iri_ref("<http://example.org/>").unwrap();
        assert_eq!(result.1, "http://example.org/");
    }

    #[test]
    fn test_prefixed_name() {
        let result = prefixed_name("rdf:type").unwrap();
        assert_eq!(result.1, ("rdf", "type"));
    }

    #[test]
    fn test_string_literal() {
        let result = string_literal("\"hello world\"").unwrap();
        assert_eq!(result.1, "hello world");
    }

    #[test]
    fn test_variable() {
        let result = variable("?x").unwrap();
        assert_eq!(result.1.name(), "x");
        assert!(result.1.is_universal());
    }

    #[test]
    fn test_parser_state() {
        let mut state = ParserState::new();
        state.add_prefix("ex", "http://example.org/");

        let uri = state.resolve_prefix("ex", "foo").unwrap();
        assert_eq!(uri.as_str(), "http://example.org/foo");
    }

    #[test]
    fn test_parse_simple_triple() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:knows ex:bob .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 1);

        let triple = &result.triples[0];
        assert_eq!(format!("{}", triple.subject), "<http://example.org/alice>");
        assert_eq!(format!("{}", triple.predicate), "<http://example.org/knows>");
        assert_eq!(format!("{}", triple.object), "<http://example.org/bob>");
    }

    #[test]
    fn test_parse_multiple_triples() {
        let input = r#"
            <http://example.org/alice> <http://example.org/knows> <http://example.org/bob> .
            <http://example.org/bob> <http://example.org/knows> <http://example.org/charlie> .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 2);
    }

    #[test]
    fn test_parse_with_semicolon() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:name "Alice" ;
                     ex:age "30" .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 2);
    }

    #[test]
    fn test_parse_with_comma() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:knows ex:bob, ex:charlie .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 2);
    }

    #[test]
    fn test_parse_rdf_type_shorthand() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 1);
        assert_eq!(format!("{}", result.triples[0].predicate),
                   "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>");
    }

    #[test]
    fn test_parse_typed_literal() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
            ex:alice ex:age "30"^^xsd:integer .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 1);
    }

    #[test]
    fn test_parse_lang_literal() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:name "Alice"@en .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 1);
    }

    #[test]
    fn test_parse_collection() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:friends (ex:bob ex:charlie) .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 1);
    }

    #[test]
    fn test_parse_blank_node() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:address [ ex:city "London" ] .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 2);
    }

    #[test]
    fn test_parse_numeric_literal() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            ex:item ex:count 42 .
            ex:item ex:price 19.99 .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 2);
    }

    #[test]
    fn test_parse_boolean_literal() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            ex:item ex:active true .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 1);
    }

    #[test]
    fn test_parse_comments() {
        let input = r#"
            # This is a comment
            @prefix ex: <http://example.org/> .
            ex:alice ex:knows ex:bob . # inline comment
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 1);
    }

    #[test]
    fn test_parse_formula() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            { ex:alice ex:knows ex:bob } ex:source ex:document1 .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 1);
        assert_eq!(result.formulas.len(), 1);

        // The formula should contain one triple
        let formula = result.formulas.values().next().unwrap();
        assert_eq!(formula.triples.len(), 1);
    }

    #[test]
    fn test_parse_rule() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            { ?x ex:parent ?y } => { ?y ex:child ?x } .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.rules.len(), 1);

        let rule = &result.rules[0];
        assert_eq!(rule.antecedent.len(), 1);
        assert_eq!(rule.consequent.len(), 1);
    }

    #[test]
    fn test_parse_multiple_rules() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

            # Rule 1: parent implies child
            { ?x ex:parent ?y } => { ?y ex:child ?x } .

            # Rule 2: human implies mortal
            { ?x a ex:Human } => { ?x a ex:Mortal } .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.rules.len(), 2);
    }

    #[test]
    fn test_parse_rule_with_multiple_patterns() {
        let input = r#"
            @prefix ex: <http://example.org/> .

            # Transitive closure of ancestor
            { ?x ex:parent ?y . ?y ex:ancestor ?z } => { ?x ex:ancestor ?z } .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.rules.len(), 1);

        let rule = &result.rules[0];
        assert_eq!(rule.antecedent.len(), 2);
        assert_eq!(rule.consequent.len(), 1);
    }

    #[test]
    fn test_parse_forall_directive() {
        let input = r#"
            @prefix ex: <http://example.org/> .
            @forAll ?x, ?y .
            { ?x ex:knows ?y } => { ?y ex:knownBy ?x } .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.rules.len(), 1);
    }

    #[test]
    fn test_parse_keywords_directive() {
        // With @keywords, bare words become URIs in the default namespace
        let input = r#"
            @base <http://example.org/> .
            @keywords a.
            alice a Person .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 1);

        let triple = &result.triples[0];
        // 'alice' and 'Person' should be resolved as URIs
        assert!(format!("{}", triple.subject).contains("alice"));
        assert!(format!("{}", triple.object).contains("Person"));
        // 'a' should be rdf:type
        assert_eq!(format!("{}", triple.predicate),
                   "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>");
    }

    #[test]
    fn test_parse_keywords_empty() {
        // @keywords with no keywords - all bare words become URIs
        let input = r#"
            @base <http://example.org/> .
            @keywords .
            alice knows bob .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 1);

        let triple = &result.triples[0];
        // All bare words should be resolved as URIs
        assert!(format!("{}", triple.subject).contains("alice"));
        assert!(format!("{}", triple.predicate).contains("knows"));
        assert!(format!("{}", triple.object).contains("bob"));
    }

    #[test]
    fn test_parse_keywords_multiple() {
        let input = r#"
            @base <http://example.org/> .
            @keywords a, has.
            alice a Person .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 1);
    }

    #[test]
    fn test_parse_has_keyword() {
        let input = r#"
            @base <http://example.org/> .
            @keywords has.
            alice has parent bob .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 1);

        let triple = &result.triples[0];
        // 'has' is syntactic sugar - the actual predicate is 'parent'
        assert!(format!("{}", triple.predicate).contains("parent"));
    }

    #[test]
    fn test_parse_forward_path() {
        // Forward path: alice!knows means "the object where alice knows ?x"
        let input = r#"
            @prefix ex: <http://example.org/> .
            ex:alice!ex:knows ex:name "Alice's friend" .
        "#;

        let result = parse(input).unwrap();
        // Should create 2 triples:
        // 1. ex:alice ex:knows _:b1
        // 2. _:b1 ex:name "Alice's friend"
        assert_eq!(result.triples.len(), 2);

        // Check that the first triple is the path expansion
        let first = &result.triples[0];
        assert!(format!("{}", first.subject).contains("alice"));
        assert!(format!("{}", first.predicate).contains("knows"));
    }

    #[test]
    fn test_parse_backward_path() {
        // Backward path: bob^knows means "the subject where ?x knows bob"
        let input = r#"
            @prefix ex: <http://example.org/> .
            ex:bob^ex:knows ex:name "Someone who knows Bob" .
        "#;

        let result = parse(input).unwrap();
        // Should create 2 triples:
        // 1. _:b1 ex:knows ex:bob
        // 2. _:b1 ex:name "Someone who knows Bob"
        assert_eq!(result.triples.len(), 2);

        // Check that the first triple is the path expansion (reversed)
        let first = &result.triples[0];
        assert!(format!("{}", first.object).contains("bob"));
        assert!(format!("{}", first.predicate).contains("knows"));
    }

    #[test]
    fn test_parse_chained_path() {
        // Chained path: alice!knows!age means traverse two relationships
        let input = r#"
            @prefix ex: <http://example.org/> .
            ex:alice!ex:knows!ex:age ex:unit "years" .
        "#;

        let result = parse(input).unwrap();
        // Should create 3 triples:
        // 1. ex:alice ex:knows _:b1
        // 2. _:b1 ex:age _:b2
        // 3. _:b2 ex:unit "years"
        assert_eq!(result.triples.len(), 3);
    }

    #[test]
    fn test_parse_default_directive() {
        // @default sets the default namespace for bare words
        let input = r#"
            @default <http://example.org/> .
            @keywords a.
            alice a Person .
        "#;

        let result = parse(input).unwrap();
        assert_eq!(result.triples.len(), 1);

        let triple = &result.triples[0];
        // 'alice' and 'Person' should use the default namespace
        assert!(format!("{}", triple.subject).contains("http://example.org/alice"));
        assert!(format!("{}", triple.object).contains("http://example.org/Person"));
    }
}
