//! KIF (Knowledge Interchange Format) parser
//!
//! Parses KIF S-expression syntax and converts to RDF triples.
//! KIF is a logic-based format used for knowledge interchange.
//!
//! # Syntax Examples
//!
//! ```kif
//! ; Comments start with semicolon
//! (defrelation parent (?x ?y))
//! (parent John Mary)
//! (=> (parent ?x ?y) (ancestor ?x ?y))
//! (forall (?x) (=> (human ?x) (mortal ?x)))
//! ```

use crate::term::{Term, Triple};
use super::{ParseError, ParseResult};
use indexmap::IndexMap;

/// KIF namespace for generated URIs
const KIF_NS: &str = "http://logic.stanford.edu/kif#";

/// Parse a KIF document into triples
pub fn parse_kif(input: &str) -> Result<ParseResult, ParseError> {
    let mut parser = KifParser::new();
    parser.parse(input)?;
    Ok(parser.finish())
}

/// KIF Parser state
struct KifParser {
    triples: Vec<Triple>,
    rules: Vec<crate::reasoner::Rule>,
    /// Counter for generating unique blank nodes
    blank_counter: u64,
}

impl KifParser {
    fn new() -> Self {
        KifParser {
            triples: Vec::new(),
            rules: Vec::new(),
            blank_counter: 0,
        }
    }

    fn finish(self) -> ParseResult {
        ParseResult {
            triples: self.triples,
            prefixes: IndexMap::new(),
            base: None,
            formulas: IndexMap::new(),
            rules: self.rules,
        }
    }

    fn fresh_blank(&mut self) -> Term {
        self.blank_counter += 1;
        Term::blank(format!("b{}", self.blank_counter))
    }

    fn parse(&mut self, input: &str) -> Result<(), ParseError> {
        let mut remaining = input.trim();

        while !remaining.is_empty() {
            // Skip comments
            if remaining.starts_with(';') {
                if let Some(pos) = remaining.find('\n') {
                    remaining = remaining[pos + 1..].trim();
                } else {
                    break;
                }
                continue;
            }

            // Skip whitespace
            remaining = remaining.trim_start();
            if remaining.is_empty() {
                break;
            }

            // Parse S-expression
            if remaining.starts_with('(') {
                let (expr, rest) = self.parse_sexpr(remaining)?;
                self.process_expr(&expr)?;
                remaining = rest.trim();
            } else {
                return Err(ParseError::Syntax {
                    position: input.len() - remaining.len(),
                    message: format!("Expected '(' but found '{}'", remaining.chars().next().unwrap_or(' ')),
                });
            }
        }

        Ok(())
    }

    /// Parse an S-expression, returning (parsed_expr, remaining_input)
    fn parse_sexpr<'a>(&self, input: &'a str) -> Result<(SExpr, &'a str), ParseError> {
        let input = input.trim();

        if input.starts_with('(') {
            // List expression
            let mut remaining = &input[1..];
            let mut items = Vec::new();

            loop {
                remaining = remaining.trim_start();

                // Skip comments inside expressions
                while remaining.starts_with(';') {
                    if let Some(pos) = remaining.find('\n') {
                        remaining = &remaining[pos + 1..];
                        remaining = remaining.trim_start();
                    } else {
                        remaining = "";
                        break;
                    }
                }

                if remaining.is_empty() {
                    return Err(ParseError::UnexpectedEof);
                }

                if remaining.starts_with(')') {
                    remaining = &remaining[1..];
                    break;
                }

                let (item, rest) = self.parse_sexpr(remaining)?;
                items.push(item);
                remaining = rest;
            }

            Ok((SExpr::List(items), remaining))
        } else if input.starts_with('?') {
            // Variable
            let end = input[1..]
                .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                .map(|i| i + 1)
                .unwrap_or(input.len());
            let name = &input[..end];
            Ok((SExpr::Variable(name.to_string()), &input[end..]))
        } else if input.starts_with('"') {
            // String literal
            let chars = input[1..].char_indices();
            let mut end = 0;
            let mut escaped = false;
            for (i, c) in chars {
                if escaped {
                    escaped = false;
                } else if c == '\\' {
                    escaped = true;
                } else if c == '"' {
                    end = i + 1;
                    break;
                }
            }
            let value = &input[1..end];
            Ok((SExpr::String(value.to_string()), &input[end + 1..]))
        } else if input.starts_with(|c: char| c.is_numeric() || c == '-') {
            // Number
            let end = input
                .find(|c: char| !c.is_numeric() && c != '.' && c != '-' && c != 'e' && c != 'E' && c != '+')
                .unwrap_or(input.len());
            let num = &input[..end];
            Ok((SExpr::Number(num.to_string()), &input[end..]))
        } else {
            // Symbol/atom
            let end = input
                .find(|c: char| c.is_whitespace() || c == '(' || c == ')' || c == ';')
                .unwrap_or(input.len());
            if end == 0 {
                return Err(ParseError::Syntax {
                    position: 0,
                    message: "Empty symbol".to_string(),
                });
            }
            let symbol = &input[..end];
            Ok((SExpr::Symbol(symbol.to_string()), &input[end..]))
        }
    }

    /// Process a parsed S-expression
    fn process_expr(&mut self, expr: &SExpr) -> Result<(), ParseError> {
        match expr {
            SExpr::List(items) if !items.is_empty() => {
                match &items[0] {
                    SExpr::Symbol(s) if s == "=>" || s == "implies" => {
                        // Implication/rule: (=> antecedent consequent)
                        if items.len() >= 3 {
                            let antecedent = self.expr_to_triples(&items[1])?;
                            let consequent = self.expr_to_triples(&items[2])?;
                            self.rules.push(crate::reasoner::Rule::new(antecedent, consequent));
                        }
                    }
                    SExpr::Symbol(s) if s == "forall" || s == "exists" => {
                        // Quantified expression: (forall (?x) body)
                        if items.len() >= 3 {
                            // Process body (the quantification is implicit in N3 variables)
                            self.process_expr(&items[2])?;
                        }
                    }
                    SExpr::Symbol(s) if s == "and" => {
                        // Conjunction: (and expr1 expr2 ...)
                        for item in &items[1..] {
                            self.process_expr(item)?;
                        }
                    }
                    SExpr::Symbol(s) if s == "or" => {
                        // Disjunction - convert each alternative to triples
                        // Note: N3 doesn't directly support disjunction, so we just add all alternatives
                        for item in &items[1..] {
                            self.process_expr(item)?;
                        }
                    }
                    SExpr::Symbol(s) if s == "not" => {
                        // Negation - skip for now (N3 has log:notIncludes but different semantics)
                        // TODO: Implement proper negation as failure
                    }
                    SExpr::Symbol(s) if s == "defrelation" || s == "deffunction" => {
                        // Definition - skip (just documentation)
                    }
                    SExpr::Symbol(predicate) => {
                        // Regular predicate: (predicate arg1 arg2 ...)
                        let triples = self.predicate_to_triples(predicate, &items[1..])?;
                        self.triples.extend(triples);
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Convert a predicate application to triples
    fn predicate_to_triples(&mut self, predicate: &str, args: &[SExpr]) -> Result<Vec<Triple>, ParseError> {
        let pred_uri = Term::uri(format!("{}{}", KIF_NS, predicate));

        match args.len() {
            0 => {
                // Nullary predicate: treat as rdf:type
                Ok(vec![Triple::new(
                    pred_uri,
                    Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                    Term::uri(format!("{}Proposition", KIF_NS)),
                )])
            }
            1 => {
                // Unary predicate: subject rdf:type predicate
                let subject = self.sexpr_to_term(&args[0])?;
                Ok(vec![Triple::new(
                    subject,
                    Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                    pred_uri,
                )])
            }
            2 => {
                // Binary predicate: subject predicate object
                let subject = self.sexpr_to_term(&args[0])?;
                let object = self.sexpr_to_term(&args[1])?;
                Ok(vec![Triple::new(subject, pred_uri, object)])
            }
            _ => {
                // N-ary predicate: use RDF list for arguments
                let subject = self.fresh_blank();
                let mut triples = vec![Triple::new(
                    subject.clone(),
                    Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                    pred_uri,
                )];

                // Add numbered argument properties
                for (i, arg) in args.iter().enumerate() {
                    let arg_term = self.sexpr_to_term(arg)?;
                    triples.push(Triple::new(
                        subject.clone(),
                        Term::uri(format!("{}arg{}", KIF_NS, i + 1)),
                        arg_term,
                    ));
                }

                Ok(triples)
            }
        }
    }

    /// Convert an S-expression to triples (for rule bodies)
    fn expr_to_triples(&mut self, expr: &SExpr) -> Result<Vec<Triple>, ParseError> {
        match expr {
            SExpr::List(items) if !items.is_empty() => {
                match &items[0] {
                    SExpr::Symbol(s) if s == "and" => {
                        let mut triples = Vec::new();
                        for item in &items[1..] {
                            triples.extend(self.expr_to_triples(item)?);
                        }
                        Ok(triples)
                    }
                    SExpr::Symbol(predicate) => {
                        self.predicate_to_triples(predicate, &items[1..])
                    }
                    _ => Ok(Vec::new()),
                }
            }
            _ => Ok(Vec::new()),
        }
    }

    /// Convert an S-expression atom to an RDF term
    fn sexpr_to_term(&self, expr: &SExpr) -> Result<Term, ParseError> {
        match expr {
            SExpr::Symbol(s) => {
                // Check for special constants
                match s.as_str() {
                    "true" | "True" | "TRUE" => Ok(Term::typed_literal("true", "http://www.w3.org/2001/XMLSchema#boolean")),
                    "false" | "False" | "FALSE" => Ok(Term::typed_literal("false", "http://www.w3.org/2001/XMLSchema#boolean")),
                    "nil" | "Nil" | "NIL" => Ok(Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")),
                    _ => Ok(Term::uri(format!("{}{}", KIF_NS, s))),
                }
            }
            SExpr::Variable(v) => {
                // Remove leading ? if present
                let name = v.strip_prefix('?').unwrap_or(v);
                Ok(Term::universal(name))
            }
            SExpr::String(s) => Ok(Term::literal(s.clone())),
            SExpr::Number(n) => {
                if n.contains('.') || n.contains('e') || n.contains('E') {
                    Ok(Term::typed_literal(n.clone(), "http://www.w3.org/2001/XMLSchema#decimal"))
                } else {
                    Ok(Term::typed_literal(n.clone(), "http://www.w3.org/2001/XMLSchema#integer"))
                }
            }
            SExpr::List(_) => {
                // Nested list - not directly convertible to a single term
                Err(ParseError::Syntax {
                    position: 0,
                    message: "Nested list cannot be converted to term".to_string(),
                })
            }
        }
    }
}

/// S-expression representation
#[derive(Debug, Clone)]
enum SExpr {
    /// Symbol/atom
    Symbol(String),
    /// Variable (?name)
    Variable(String),
    /// String literal
    String(String),
    /// Number
    Number(String),
    /// List (parenthesized expression)
    List(Vec<SExpr>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_fact() {
        let kif = "(parent John Mary)";
        let result = parse_kif(kif).expect("Failed to parse KIF");

        assert_eq!(result.triples.len(), 1);
        let triple = &result.triples[0];
        assert!(matches!(&triple.subject, Term::Uri(u) if u.as_str().ends_with("John")));
        assert!(matches!(&triple.predicate, Term::Uri(u) if u.as_str().ends_with("parent")));
        assert!(matches!(&triple.object, Term::Uri(u) if u.as_str().ends_with("Mary")));
    }

    #[test]
    fn test_parse_unary_predicate() {
        let kif = "(human Socrates)";
        let result = parse_kif(kif).expect("Failed to parse KIF");

        assert_eq!(result.triples.len(), 1);
        let triple = &result.triples[0];
        // Unary predicate becomes: Socrates rdf:type human
        assert!(matches!(&triple.subject, Term::Uri(u) if u.as_str().ends_with("Socrates")));
        assert!(matches!(&triple.predicate, Term::Uri(u) if u.as_str().contains("type")));
    }

    #[test]
    fn test_parse_rule() {
        let kif = "(=> (human ?x) (mortal ?x))";
        let result = parse_kif(kif).expect("Failed to parse KIF");

        assert_eq!(result.rules.len(), 1);
        let rule = &result.rules[0];
        assert_eq!(rule.antecedent.len(), 1);
        assert_eq!(rule.consequent.len(), 1);
    }

    #[test]
    fn test_parse_conjunction() {
        let kif = "(and (human Socrates) (greek Socrates))";
        let result = parse_kif(kif).expect("Failed to parse KIF");

        assert_eq!(result.triples.len(), 2);
    }

    #[test]
    fn test_parse_with_comments() {
        let kif = r#"
; This is a comment
(parent John Mary)
; Another comment
(parent Mary Tom)
"#;
        let result = parse_kif(kif).expect("Failed to parse KIF");
        assert_eq!(result.triples.len(), 2);
    }

    #[test]
    fn test_parse_forall() {
        let kif = "(forall (?x) (=> (human ?x) (mortal ?x)))";
        let result = parse_kif(kif).expect("Failed to parse KIF");

        // forall just passes through to the inner expression
        assert_eq!(result.rules.len(), 1);
    }

    #[test]
    fn test_parse_variables() {
        let kif = "(likes ?x ?y)";
        let result = parse_kif(kif).expect("Failed to parse KIF");

        assert_eq!(result.triples.len(), 1);
        let triple = &result.triples[0];
        assert!(matches!(&triple.subject, Term::Variable(_)));
        assert!(matches!(&triple.object, Term::Variable(_)));
    }
}
