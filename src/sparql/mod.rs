//! SPARQL query support
//!
//! Implements a subset of SPARQL 1.1 for querying RDF graphs.
//! Includes federated query support via SERVICE keyword.

use std::collections::HashMap;
use percent_encoding::{utf8_percent_encode, NON_ALPHANUMERIC};
use crate::term::{Term, Triple};
use crate::Store;

/// SPARQL query types
#[derive(Debug, Clone)]
pub enum Query {
    Select {
        variables: Vec<String>,
        distinct: bool,
        where_clause: WhereClause,
        order_by: Option<Vec<OrderCondition>>,
        limit: Option<usize>,
        offset: Option<usize>,
    },
    Ask {
        where_clause: WhereClause,
    },
    Construct {
        template: Vec<TriplePattern>,
        where_clause: WhereClause,
    },
    Describe {
        resources: Vec<Term>,
    },
}

/// WHERE clause containing graph patterns
#[derive(Debug, Clone)]
pub struct WhereClause {
    pub patterns: Vec<GraphPattern>,
}

/// Graph pattern types
#[derive(Debug, Clone)]
pub enum GraphPattern {
    /// Basic triple pattern
    Triple(TriplePattern),
    /// OPTIONAL pattern
    Optional(Box<WhereClause>),
    /// UNION of patterns
    Union(Box<WhereClause>, Box<WhereClause>),
    /// FILTER expression
    Filter(FilterExpr),
    /// BIND expression
    Bind(FilterExpr, String),
    /// Subquery group
    Group(Box<WhereClause>),
    /// SERVICE for federated queries (endpoint URI, patterns, silent flag)
    Service(String, Box<WhereClause>, bool),
}

/// Triple pattern with possible variables
#[derive(Debug, Clone)]
pub struct TriplePattern {
    pub subject: TermPattern,
    pub predicate: TermPattern,
    pub object: TermPattern,
}

/// Term or variable in a pattern
#[derive(Debug, Clone)]
pub enum TermPattern {
    Variable(String),
    Term(Term),
}

/// Filter expressions
#[derive(Debug, Clone)]
pub enum FilterExpr {
    /// Variable bound check
    Bound(String),
    /// Not bound check
    NotBound(String),
    /// Equality
    Equals(Box<FilterExpr>, Box<FilterExpr>),
    /// Inequality
    NotEquals(Box<FilterExpr>, Box<FilterExpr>),
    /// Less than
    LessThan(Box<FilterExpr>, Box<FilterExpr>),
    /// Greater than
    GreaterThan(Box<FilterExpr>, Box<FilterExpr>),
    /// Less than or equal
    LessOrEqual(Box<FilterExpr>, Box<FilterExpr>),
    /// Greater than or equal
    GreaterOrEqual(Box<FilterExpr>, Box<FilterExpr>),
    /// Logical AND
    And(Box<FilterExpr>, Box<FilterExpr>),
    /// Logical OR
    Or(Box<FilterExpr>, Box<FilterExpr>),
    /// Logical NOT
    Not(Box<FilterExpr>),
    /// Variable reference
    Var(String),
    /// Literal value
    Literal(Term),
    /// Regex match
    Regex(Box<FilterExpr>, String, Option<String>),
    /// String contains
    Contains(Box<FilterExpr>, Box<FilterExpr>),
    /// String starts with
    StrStarts(Box<FilterExpr>, Box<FilterExpr>),
    /// String ends with
    StrEnds(Box<FilterExpr>, Box<FilterExpr>),
    /// String length
    StrLen(Box<FilterExpr>),
    /// Is IRI check
    IsIri(Box<FilterExpr>),
    /// Is literal check
    IsLiteral(Box<FilterExpr>),
    /// Is blank node check
    IsBlank(Box<FilterExpr>),
    /// STR function
    Str(Box<FilterExpr>),
    /// LANG function
    Lang(Box<FilterExpr>),
    /// DATATYPE function
    Datatype(Box<FilterExpr>),
}

/// Order by condition
#[derive(Debug, Clone)]
pub struct OrderCondition {
    pub variable: String,
    pub ascending: bool,
}

/// Query results
#[derive(Debug, Clone)]
pub enum QueryResult {
    /// SELECT query results
    Bindings {
        variables: Vec<String>,
        solutions: Vec<HashMap<String, Term>>,
    },
    /// ASK query result
    Boolean(bool),
    /// CONSTRUCT query result
    Graph(Vec<Triple>),
}

/// SPARQL parser
pub struct SparqlParser<'a> {
    input: &'a str,
    pos: usize,
    prefixes: HashMap<String, String>,
}

impl<'a> SparqlParser<'a> {
    pub fn new(input: &'a str) -> Self {
        SparqlParser {
            input,
            pos: 0,
            prefixes: HashMap::new(),
        }
    }

    /// Parse a SPARQL query
    pub fn parse(&mut self) -> Result<Query, String> {
        self.skip_whitespace();

        // Parse PREFIX declarations
        while self.try_keyword("PREFIX") {
            self.parse_prefix()?;
            self.skip_whitespace();
        }

        // Parse BASE if present
        if self.try_keyword("BASE") {
            self.skip_whitespace();
            let _base = self.parse_iri()?;
            self.skip_whitespace();
        }

        // Parse query form
        if self.try_keyword("SELECT") {
            self.parse_select()
        } else if self.try_keyword("ASK") {
            self.parse_ask()
        } else if self.try_keyword("CONSTRUCT") {
            self.parse_construct()
        } else if self.try_keyword("DESCRIBE") {
            self.parse_describe()
        } else {
            Err("Expected SELECT, ASK, CONSTRUCT, or DESCRIBE".to_string())
        }
    }

    fn parse_prefix(&mut self) -> Result<(), String> {
        self.skip_whitespace();
        let prefix = self.parse_pname_ns()?;
        self.skip_whitespace();
        let iri = self.parse_iri()?;
        self.prefixes.insert(prefix, iri);
        Ok(())
    }

    fn parse_pname_ns(&mut self) -> Result<String, String> {
        let start = self.pos;
        while self.pos < self.input.len() {
            let c = self.current_char();
            if c == ':' {
                let prefix = self.input[start..self.pos].to_string();
                self.pos += 1; // consume ':'
                return Ok(prefix);
            } else if c.is_alphanumeric() || c == '_' {
                self.pos += 1;
            } else {
                break;
            }
        }
        Err("Expected prefix name".to_string())
    }

    fn parse_iri(&mut self) -> Result<String, String> {
        self.skip_whitespace();
        if self.current_char() == '<' {
            self.pos += 1;
            let start = self.pos;
            while self.pos < self.input.len() && self.current_char() != '>' {
                self.pos += 1;
            }
            let iri = self.input[start..self.pos].to_string();
            if self.current_char() == '>' {
                self.pos += 1;
            }
            Ok(iri)
        } else {
            // Try prefixed name
            self.parse_prefixed_name()
        }
    }

    fn parse_prefixed_name(&mut self) -> Result<String, String> {
        let start = self.pos;

        // Find the colon
        while self.pos < self.input.len() {
            let c = self.current_char();
            if c == ':' {
                let prefix = self.input[start..self.pos].to_string();
                self.pos += 1;

                // Parse local part
                let local_start = self.pos;
                while self.pos < self.input.len() {
                    let c = self.current_char();
                    if c.is_alphanumeric() || c == '_' || c == '-' || c == '.' {
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                let local = self.input[local_start..self.pos].to_string();

                // Resolve prefix
                if let Some(ns) = self.prefixes.get(&prefix) {
                    return Ok(format!("{}{}", ns, local));
                } else {
                    return Err(format!("Unknown prefix: {}", prefix));
                }
            } else if c.is_alphanumeric() || c == '_' {
                self.pos += 1;
            } else {
                break;
            }
        }

        Err("Expected IRI or prefixed name".to_string())
    }

    fn parse_select(&mut self) -> Result<Query, String> {
        self.skip_whitespace();

        let distinct = self.try_keyword("DISTINCT");
        self.skip_whitespace();

        // Parse variable list or *
        let variables = if self.current_char() == '*' {
            self.pos += 1;
            Vec::new() // Empty means all variables
        } else {
            self.parse_variable_list()?
        };

        self.skip_whitespace();

        // Parse WHERE clause
        if !self.try_keyword("WHERE") {
            return Err("Expected WHERE".to_string());
        }

        let where_clause = self.parse_where_clause()?;

        // Parse modifiers
        self.skip_whitespace();
        let order_by = if self.try_keyword("ORDER") {
            self.skip_whitespace();
            if !self.try_keyword("BY") {
                return Err("Expected BY after ORDER".to_string());
            }
            Some(self.parse_order_conditions()?)
        } else {
            None
        };

        self.skip_whitespace();
        let limit = if self.try_keyword("LIMIT") {
            self.skip_whitespace();
            Some(self.parse_integer()? as usize)
        } else {
            None
        };

        self.skip_whitespace();
        let offset = if self.try_keyword("OFFSET") {
            self.skip_whitespace();
            Some(self.parse_integer()? as usize)
        } else {
            None
        };

        Ok(Query::Select {
            variables,
            distinct,
            where_clause,
            order_by,
            limit,
            offset,
        })
    }

    fn parse_ask(&mut self) -> Result<Query, String> {
        self.skip_whitespace();

        // WHERE is optional for ASK - can be "ASK WHERE { ... }" or "ASK { ... }"
        self.try_keyword("WHERE");

        let where_clause = self.parse_where_clause()?;

        Ok(Query::Ask { where_clause })
    }

    fn parse_construct(&mut self) -> Result<Query, String> {
        self.skip_whitespace();

        // Parse template
        if self.current_char() != '{' {
            return Err("Expected { for CONSTRUCT template".to_string());
        }
        self.pos += 1;

        let template = self.parse_triple_patterns()?;

        self.skip_whitespace();
        if self.current_char() != '}' {
            return Err("Expected } for CONSTRUCT template".to_string());
        }
        self.pos += 1;

        self.skip_whitespace();

        if !self.try_keyword("WHERE") {
            return Err("Expected WHERE".to_string());
        }

        let where_clause = self.parse_where_clause()?;

        Ok(Query::Construct { template, where_clause })
    }

    fn parse_describe(&mut self) -> Result<Query, String> {
        self.skip_whitespace();

        let mut resources = Vec::new();

        while self.pos < self.input.len() {
            self.skip_whitespace();
            if self.current_char() == '<' || self.current_char().is_alphabetic() {
                let iri = self.parse_iri()?;
                resources.push(Term::uri(&iri));
            } else if self.current_char() == '?' {
                // Variable - skip for now
                self.pos += 1;
                while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }

        Ok(Query::Describe { resources })
    }

    fn parse_variable_list(&mut self) -> Result<Vec<String>, String> {
        let mut vars = Vec::new();

        while self.pos < self.input.len() {
            self.skip_whitespace();
            if self.current_char() == '?' || self.current_char() == '$' {
                self.pos += 1;
                let start = self.pos;
                while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
                    self.pos += 1;
                }
                vars.push(self.input[start..self.pos].to_string());
            } else {
                break;
            }
        }

        if vars.is_empty() {
            return Err("Expected at least one variable".to_string());
        }

        Ok(vars)
    }

    fn parse_where_clause(&mut self) -> Result<WhereClause, String> {
        self.skip_whitespace();

        if self.current_char() != '{' {
            return Err("Expected { for WHERE clause".to_string());
        }
        self.pos += 1;

        let patterns = self.parse_graph_patterns()?;

        self.skip_whitespace();
        if self.current_char() != '}' {
            return Err("Expected } for WHERE clause".to_string());
        }
        self.pos += 1;

        Ok(WhereClause { patterns })
    }

    fn parse_graph_patterns(&mut self) -> Result<Vec<GraphPattern>, String> {
        let mut patterns = Vec::new();

        loop {
            self.skip_whitespace();

            if self.current_char() == '}' || self.pos >= self.input.len() {
                break;
            }

            // Check for OPTIONAL
            if self.try_keyword("OPTIONAL") {
                let optional_clause = self.parse_where_clause()?;
                patterns.push(GraphPattern::Optional(Box::new(optional_clause)));
                continue;
            }

            // Check for FILTER
            if self.try_keyword("FILTER") {
                let filter = self.parse_filter()?;
                patterns.push(GraphPattern::Filter(filter));
                continue;
            }

            // Check for UNION
            if self.try_keyword("UNION") {
                // Need to handle this differently
                continue;
            }

            // Check for BIND
            if self.try_keyword("BIND") {
                self.skip_whitespace();
                if self.current_char() != '(' {
                    return Err("Expected ( after BIND".to_string());
                }
                self.pos += 1;

                let expr = self.parse_filter_expr()?;

                self.skip_whitespace();
                if !self.try_keyword("AS") {
                    return Err("Expected AS in BIND".to_string());
                }

                self.skip_whitespace();
                if self.current_char() != '?' && self.current_char() != '$' {
                    return Err("Expected variable after AS".to_string());
                }
                self.pos += 1;
                let start = self.pos;
                while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
                    self.pos += 1;
                }
                let var_name = self.input[start..self.pos].to_string();

                self.skip_whitespace();
                if self.current_char() != ')' {
                    return Err("Expected ) after BIND".to_string());
                }
                self.pos += 1;

                patterns.push(GraphPattern::Bind(expr, var_name));
                continue;
            }

            // Check for SERVICE (federated query)
            let silent = if self.try_keyword("SERVICE") {
                self.skip_whitespace();
                let is_silent = self.try_keyword("SILENT");
                if is_silent {
                    self.skip_whitespace();
                }

                // Parse endpoint URI
                let endpoint = if self.current_char() == '<' {
                    self.pos += 1;
                    let start = self.pos;
                    while self.pos < self.input.len() && self.current_char() != '>' {
                        self.pos += 1;
                    }
                    let uri = self.input[start..self.pos].to_string();
                    if self.current_char() == '>' {
                        self.pos += 1;
                    }
                    uri
                } else if self.current_char() == '?' || self.current_char() == '$' {
                    // Variable endpoint - not supported yet, skip
                    return Err("Variable SERVICE endpoints not yet supported".to_string());
                } else {
                    return Err("Expected URI after SERVICE".to_string());
                };

                // Parse the service pattern
                let service_clause = self.parse_where_clause()?;
                patterns.push(GraphPattern::Service(endpoint, Box::new(service_clause), is_silent));
                continue;
            } else {
                false
            };
            let _ = silent; // Suppress unused warning

            // Check for nested group
            if self.current_char() == '{' {
                let nested = self.parse_where_clause()?;
                patterns.push(GraphPattern::Group(Box::new(nested)));
                continue;
            }

            // Try to parse a triple pattern
            if let Ok(triple) = self.parse_triple_pattern() {
                patterns.push(GraphPattern::Triple(triple));

                // Skip optional '.'
                self.skip_whitespace();
                if self.current_char() == '.' {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }

        Ok(patterns)
    }

    fn parse_triple_patterns(&mut self) -> Result<Vec<TriplePattern>, String> {
        let mut patterns = Vec::new();

        loop {
            self.skip_whitespace();

            if self.current_char() == '}' || self.pos >= self.input.len() {
                break;
            }

            if let Ok(triple) = self.parse_triple_pattern() {
                patterns.push(triple);

                self.skip_whitespace();
                if self.current_char() == '.' {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }

        Ok(patterns)
    }

    fn parse_triple_pattern(&mut self) -> Result<TriplePattern, String> {
        self.skip_whitespace();
        let subject = self.parse_term_pattern()?;

        self.skip_whitespace();
        let predicate = if self.current_char() == 'a' && !self.peek_char(1).is_alphanumeric() {
            self.pos += 1;
            TermPattern::Term(Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
        } else {
            self.parse_term_pattern()?
        };

        self.skip_whitespace();
        let object = self.parse_term_pattern()?;

        Ok(TriplePattern { subject, predicate, object })
    }

    fn parse_term_pattern(&mut self) -> Result<TermPattern, String> {
        self.skip_whitespace();

        let c = self.current_char();

        if c == '?' || c == '$' {
            // Variable
            self.pos += 1;
            let start = self.pos;
            while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
                self.pos += 1;
            }
            Ok(TermPattern::Variable(self.input[start..self.pos].to_string()))
        } else if c == '<' {
            // IRI
            let iri = self.parse_iri()?;
            Ok(TermPattern::Term(Term::uri(&iri)))
        } else if c == '"' || c == '\'' {
            // Literal
            let lit = self.parse_literal()?;
            Ok(TermPattern::Term(lit))
        } else if c.is_numeric() || c == '-' || c == '+' {
            // Numeric literal
            let num = self.parse_number()?;
            Ok(TermPattern::Term(num))
        } else if c.is_alphabetic() {
            // Prefixed name or keyword
            let iri = self.parse_prefixed_name()?;
            Ok(TermPattern::Term(Term::uri(&iri)))
        } else {
            Err(format!("Unexpected character in term pattern: {}", c))
        }
    }

    fn parse_literal(&mut self) -> Result<Term, String> {
        let quote = self.current_char();
        self.pos += 1;

        let start = self.pos;
        while self.pos < self.input.len() && self.current_char() != quote {
            if self.current_char() == '\\' {
                self.pos += 2;
            } else {
                self.pos += 1;
            }
        }
        let value = self.input[start..self.pos].to_string();

        if self.current_char() == quote {
            self.pos += 1;
        }

        // Check for language tag or datatype
        if self.current_char() == '@' {
            self.pos += 1;
            let lang_start = self.pos;
            while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '-') {
                self.pos += 1;
            }
            let lang = self.input[lang_start..self.pos].to_string();
            Ok(Term::lang_literal(value, lang))
        } else if self.pos + 1 < self.input.len() && &self.input[self.pos..self.pos+2] == "^^" {
            self.pos += 2;
            let datatype = self.parse_iri()?;
            Ok(Term::typed_literal(value, &datatype))
        } else {
            Ok(Term::literal(value))
        }
    }

    fn parse_number(&mut self) -> Result<Term, String> {
        let start = self.pos;

        if self.current_char() == '-' || self.current_char() == '+' {
            self.pos += 1;
        }

        while self.pos < self.input.len() && self.current_char().is_numeric() {
            self.pos += 1;
        }

        if self.current_char() == '.' {
            self.pos += 1;
            while self.pos < self.input.len() && self.current_char().is_numeric() {
                self.pos += 1;
            }
        }

        let num_str = self.input[start..self.pos].to_string();
        if num_str.contains('.') {
            Ok(Term::typed_literal(num_str, "http://www.w3.org/2001/XMLSchema#decimal"))
        } else {
            Ok(Term::typed_literal(num_str, "http://www.w3.org/2001/XMLSchema#integer"))
        }
    }

    fn parse_filter(&mut self) -> Result<FilterExpr, String> {
        self.skip_whitespace();

        if self.current_char() == '(' {
            self.pos += 1;
            let expr = self.parse_filter_expr()?;
            self.skip_whitespace();
            if self.current_char() == ')' {
                self.pos += 1;
            }
            Ok(expr)
        } else {
            self.parse_filter_expr()
        }
    }

    fn parse_filter_expr(&mut self) -> Result<FilterExpr, String> {
        self.skip_whitespace();

        // Check for function calls
        if self.try_keyword("BOUND") {
            return self.parse_bound_call();
        }
        if self.try_keyword("ISIRI") || self.try_keyword("ISURI") {
            return self.parse_unary_call(|e| FilterExpr::IsIri(Box::new(e)));
        }
        if self.try_keyword("ISLITERAL") {
            return self.parse_unary_call(|e| FilterExpr::IsLiteral(Box::new(e)));
        }
        if self.try_keyword("ISBLANK") {
            return self.parse_unary_call(|e| FilterExpr::IsBlank(Box::new(e)));
        }
        if self.try_keyword("STR") {
            return self.parse_unary_call(|e| FilterExpr::Str(Box::new(e)));
        }
        if self.try_keyword("LANG") {
            return self.parse_unary_call(|e| FilterExpr::Lang(Box::new(e)));
        }
        if self.try_keyword("DATATYPE") {
            return self.parse_unary_call(|e| FilterExpr::Datatype(Box::new(e)));
        }
        if self.try_keyword("STRLEN") {
            return self.parse_unary_call(|e| FilterExpr::StrLen(Box::new(e)));
        }
        if self.try_keyword("CONTAINS") {
            return self.parse_binary_call(|a, b| FilterExpr::Contains(Box::new(a), Box::new(b)));
        }
        if self.try_keyword("STRSTARTS") {
            return self.parse_binary_call(|a, b| FilterExpr::StrStarts(Box::new(a), Box::new(b)));
        }
        if self.try_keyword("STRENDS") {
            return self.parse_binary_call(|a, b| FilterExpr::StrEnds(Box::new(a), Box::new(b)));
        }
        if self.try_keyword("REGEX") {
            return self.parse_regex_call();
        }

        // Check for NOT
        if self.current_char() == '!' {
            self.pos += 1;
            let inner = self.parse_filter_expr()?;
            return Ok(FilterExpr::Not(Box::new(inner)));
        }

        // Parse primary expression
        let left = self.parse_primary_expr()?;

        self.skip_whitespace();

        // Check for comparison operators
        if self.pos + 1 < self.input.len() {
            let op = &self.input[self.pos..self.pos.min(self.pos + 2)];
            match op {
                "==" | "= " => {
                    self.pos += if op == "==" { 2 } else { 1 };
                    let right = self.parse_filter_expr()?;
                    return Ok(FilterExpr::Equals(Box::new(left), Box::new(right)));
                }
                "!=" => {
                    self.pos += 2;
                    let right = self.parse_filter_expr()?;
                    return Ok(FilterExpr::NotEquals(Box::new(left), Box::new(right)));
                }
                "<=" => {
                    self.pos += 2;
                    let right = self.parse_filter_expr()?;
                    return Ok(FilterExpr::LessOrEqual(Box::new(left), Box::new(right)));
                }
                ">=" => {
                    self.pos += 2;
                    let right = self.parse_filter_expr()?;
                    return Ok(FilterExpr::GreaterOrEqual(Box::new(left), Box::new(right)));
                }
                _ => {}
            }

            if self.current_char() == '<' {
                self.pos += 1;
                let right = self.parse_filter_expr()?;
                return Ok(FilterExpr::LessThan(Box::new(left), Box::new(right)));
            }
            if self.current_char() == '>' {
                self.pos += 1;
                let right = self.parse_filter_expr()?;
                return Ok(FilterExpr::GreaterThan(Box::new(left), Box::new(right)));
            }
        }

        // Check for logical operators
        self.skip_whitespace();
        if self.try_keyword("&&") || self.try_keyword("AND") {
            let right = self.parse_filter_expr()?;
            return Ok(FilterExpr::And(Box::new(left), Box::new(right)));
        }
        if self.try_keyword("||") || self.try_keyword("OR") {
            let right = self.parse_filter_expr()?;
            return Ok(FilterExpr::Or(Box::new(left), Box::new(right)));
        }

        Ok(left)
    }

    fn parse_primary_expr(&mut self) -> Result<FilterExpr, String> {
        self.skip_whitespace();

        if self.current_char() == '(' {
            self.pos += 1;
            let expr = self.parse_filter_expr()?;
            self.skip_whitespace();
            if self.current_char() == ')' {
                self.pos += 1;
            }
            return Ok(expr);
        }

        if self.current_char() == '?' || self.current_char() == '$' {
            self.pos += 1;
            let start = self.pos;
            while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
                self.pos += 1;
            }
            return Ok(FilterExpr::Var(self.input[start..self.pos].to_string()));
        }

        if self.current_char() == '"' || self.current_char() == '\'' {
            let lit = self.parse_literal()?;
            return Ok(FilterExpr::Literal(lit));
        }

        if self.current_char().is_numeric() || self.current_char() == '-' || self.current_char() == '+' {
            let num = self.parse_number()?;
            return Ok(FilterExpr::Literal(num));
        }

        Err("Expected expression".to_string())
    }

    fn parse_bound_call(&mut self) -> Result<FilterExpr, String> {
        self.skip_whitespace();
        if self.current_char() != '(' {
            return Err("Expected ( after BOUND".to_string());
        }
        self.pos += 1;

        self.skip_whitespace();
        if self.current_char() != '?' && self.current_char() != '$' {
            return Err("Expected variable in BOUND".to_string());
        }
        self.pos += 1;

        let start = self.pos;
        while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
            self.pos += 1;
        }
        let var_name = self.input[start..self.pos].to_string();

        self.skip_whitespace();
        if self.current_char() != ')' {
            return Err("Expected ) after BOUND variable".to_string());
        }
        self.pos += 1;

        Ok(FilterExpr::Bound(var_name))
    }

    fn parse_unary_call<F>(&mut self, f: F) -> Result<FilterExpr, String>
    where
        F: FnOnce(FilterExpr) -> FilterExpr,
    {
        self.skip_whitespace();
        if self.current_char() != '(' {
            return Err("Expected (".to_string());
        }
        self.pos += 1;

        let arg = self.parse_filter_expr()?;

        self.skip_whitespace();
        if self.current_char() != ')' {
            return Err("Expected )".to_string());
        }
        self.pos += 1;

        Ok(f(arg))
    }

    fn parse_binary_call<F>(&mut self, f: F) -> Result<FilterExpr, String>
    where
        F: FnOnce(FilterExpr, FilterExpr) -> FilterExpr,
    {
        self.skip_whitespace();
        if self.current_char() != '(' {
            return Err("Expected (".to_string());
        }
        self.pos += 1;

        let arg1 = self.parse_filter_expr()?;

        self.skip_whitespace();
        if self.current_char() != ',' {
            return Err("Expected ,".to_string());
        }
        self.pos += 1;

        let arg2 = self.parse_filter_expr()?;

        self.skip_whitespace();
        if self.current_char() != ')' {
            return Err("Expected )".to_string());
        }
        self.pos += 1;

        Ok(f(arg1, arg2))
    }

    fn parse_regex_call(&mut self) -> Result<FilterExpr, String> {
        self.skip_whitespace();
        if self.current_char() != '(' {
            return Err("Expected ( after REGEX".to_string());
        }
        self.pos += 1;

        let text = self.parse_filter_expr()?;

        self.skip_whitespace();
        if self.current_char() != ',' {
            return Err("Expected , in REGEX".to_string());
        }
        self.pos += 1;

        self.skip_whitespace();
        let pattern = self.parse_literal()?;
        let pattern_str = match &pattern {
            Term::Literal(lit) => lit.value().to_string(),
            _ => return Err("Expected string pattern".to_string()),
        };

        self.skip_whitespace();
        let flags = if self.current_char() == ',' {
            self.pos += 1;
            self.skip_whitespace();
            let f = self.parse_literal()?;
            match f {
                Term::Literal(lit) => Some(lit.value().to_string()),
                _ => None,
            }
        } else {
            None
        };

        self.skip_whitespace();
        if self.current_char() != ')' {
            return Err("Expected ) after REGEX".to_string());
        }
        self.pos += 1;

        Ok(FilterExpr::Regex(Box::new(text), pattern_str, flags))
    }

    fn parse_order_conditions(&mut self) -> Result<Vec<OrderCondition>, String> {
        let mut conditions = Vec::new();

        loop {
            self.skip_whitespace();

            let ascending = if self.try_keyword("ASC") {
                true
            } else if self.try_keyword("DESC") {
                false
            } else {
                true // default ascending
            };

            self.skip_whitespace();

            // Handle optional parentheses
            let has_paren = self.current_char() == '(';
            if has_paren {
                self.pos += 1;
                self.skip_whitespace();
            }

            if self.current_char() != '?' && self.current_char() != '$' {
                break;
            }
            self.pos += 1;

            let start = self.pos;
            while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
                self.pos += 1;
            }
            let var_name = self.input[start..self.pos].to_string();

            if has_paren {
                self.skip_whitespace();
                if self.current_char() == ')' {
                    self.pos += 1;
                }
            }

            conditions.push(OrderCondition {
                variable: var_name,
                ascending,
            });
        }

        Ok(conditions)
    }

    fn parse_integer(&mut self) -> Result<i64, String> {
        let start = self.pos;
        if self.current_char() == '-' || self.current_char() == '+' {
            self.pos += 1;
        }
        while self.pos < self.input.len() && self.current_char().is_numeric() {
            self.pos += 1;
        }
        self.input[start..self.pos]
            .parse()
            .map_err(|_| "Invalid integer".to_string())
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len() {
            let c = self.current_char();
            if c.is_whitespace() {
                self.pos += 1;
            } else if c == '#' {
                // Skip comment
                while self.pos < self.input.len() && self.current_char() != '\n' {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }
    }

    fn current_char(&self) -> char {
        self.input[self.pos..].chars().next().unwrap_or('\0')
    }

    fn peek_char(&self, offset: usize) -> char {
        self.input[self.pos + offset..].chars().next().unwrap_or('\0')
    }

    fn try_keyword(&mut self, keyword: &str) -> bool {
        let remaining = &self.input[self.pos..];
        if remaining.to_uppercase().starts_with(&keyword.to_uppercase()) {
            let next_pos = self.pos + keyword.len();
            if next_pos >= self.input.len() || !self.input[next_pos..].chars().next().unwrap_or(' ').is_alphanumeric() {
                self.pos = next_pos;
                return true;
            }
        }
        false
    }
}

/// SPARQL query executor
pub struct SparqlEngine<'a> {
    store: &'a Store,
}

impl<'a> SparqlEngine<'a> {
    pub fn new(store: &'a Store) -> Self {
        SparqlEngine { store }
    }

    /// Execute a SPARQL query
    pub fn execute(&self, query: &Query) -> QueryResult {
        match query {
            Query::Select { variables, distinct, where_clause, order_by, limit, offset } => {
                let mut solutions = self.evaluate_where(where_clause);

                // Apply ORDER BY
                if let Some(order) = order_by {
                    self.apply_order(&mut solutions, order);
                }

                // Apply OFFSET
                if let Some(off) = offset {
                    if *off < solutions.len() {
                        solutions = solutions[*off..].to_vec();
                    } else {
                        solutions.clear();
                    }
                }

                // Apply LIMIT
                if let Some(lim) = limit {
                    solutions.truncate(*lim);
                }

                // Apply DISTINCT
                if *distinct {
                    let mut seen = std::collections::HashSet::new();
                    solutions.retain(|sol| {
                        let key = format!("{:?}", sol);
                        seen.insert(key)
                    });
                }

                // Project variables
                let result_vars = if variables.is_empty() {
                    // SELECT * - collect all variables
                    let mut all_vars: Vec<String> = solutions.iter()
                        .flat_map(|s| s.keys().cloned())
                        .collect();
                    all_vars.sort();
                    all_vars.dedup();
                    all_vars
                } else {
                    variables.clone()
                };

                QueryResult::Bindings {
                    variables: result_vars,
                    solutions,
                }
            }
            Query::Ask { where_clause } => {
                let solutions = self.evaluate_where(where_clause);
                QueryResult::Boolean(!solutions.is_empty())
            }
            Query::Construct { template, where_clause } => {
                let solutions = self.evaluate_where(where_clause);
                let mut triples = Vec::new();

                for solution in &solutions {
                    for pattern in template {
                        if let Some(triple) = self.instantiate_pattern(pattern, solution) {
                            triples.push(triple);
                        }
                    }
                }

                QueryResult::Graph(triples)
            }
            Query::Describe { resources } => {
                let mut triples = Vec::new();

                for resource in resources {
                    // Get all triples where resource is subject
                    for triple in self.store.iter() {
                        if &triple.subject == resource {
                            triples.push(triple.clone());
                        }
                    }
                }

                QueryResult::Graph(triples)
            }
        }
    }

    fn evaluate_where(&self, where_clause: &WhereClause) -> Vec<HashMap<String, Term>> {
        let mut solutions = vec![HashMap::new()];

        for pattern in &where_clause.patterns {
            solutions = self.evaluate_pattern(pattern, solutions);
        }

        solutions
    }

    fn evaluate_pattern(&self, pattern: &GraphPattern, mut solutions: Vec<HashMap<String, Term>>) -> Vec<HashMap<String, Term>> {
        match pattern {
            GraphPattern::Triple(tp) => {
                let mut new_solutions = Vec::new();

                for solution in solutions {
                    for triple in self.store.iter() {
                        if let Some(new_sol) = self.match_triple(tp, triple, &solution) {
                            new_solutions.push(new_sol);
                        }
                    }
                }

                new_solutions
            }
            GraphPattern::Optional(opt_clause) => {
                let mut new_solutions = Vec::new();

                for solution in solutions {
                    let opt_sols = self.evaluate_where_with_bindings(opt_clause, solution.clone());
                    if opt_sols.is_empty() {
                        new_solutions.push(solution);
                    } else {
                        new_solutions.extend(opt_sols);
                    }
                }

                new_solutions
            }
            GraphPattern::Union(left, right) => {
                let mut left_sols = self.evaluate_where_with_solutions(left, solutions.clone());
                let right_sols = self.evaluate_where_with_solutions(right, solutions);
                left_sols.extend(right_sols);
                left_sols
            }
            GraphPattern::Filter(expr) => {
                solutions.retain(|sol| self.evaluate_filter(expr, sol));
                solutions
            }
            GraphPattern::Bind(expr, var) => {
                for solution in &mut solutions {
                    if let Some(value) = self.evaluate_expr_to_term(expr, solution) {
                        solution.insert(var.clone(), value);
                    }
                }
                solutions
            }
            GraphPattern::Group(inner) => {
                self.evaluate_where_with_solutions(inner, solutions)
            }
            GraphPattern::Service(endpoint, service_clause, silent) => {
                // Execute federated query against remote SPARQL endpoint
                self.evaluate_service(endpoint, service_clause, solutions, *silent)
            }
        }
    }

    /// Execute a SERVICE query against a remote SPARQL endpoint
    fn evaluate_service(
        &self,
        endpoint: &str,
        service_clause: &WhereClause,
        solutions: Vec<HashMap<String, Term>>,
        silent: bool,
    ) -> Vec<HashMap<String, Term>> {
        let mut result = Vec::new();

        for solution in solutions {
            // Build a SPARQL query from the service clause
            let query = self.build_service_query(service_clause, &solution);

            // Execute against remote endpoint
            match self.execute_remote_query(endpoint, &query) {
                Ok(remote_solutions) => {
                    // Merge remote solutions with current solution
                    for remote_sol in remote_solutions {
                        let mut merged = solution.clone();
                        merged.extend(remote_sol);
                        result.push(merged);
                    }
                }
                Err(e) => {
                    if !silent {
                        eprintln!("SERVICE error for {}: {}", endpoint, e);
                    }
                    // If silent, continue with current solution unchanged
                    if silent {
                        result.push(solution.clone());
                    }
                }
            }
        }

        result
    }

    /// Build a SELECT query from a WHERE clause for remote execution
    fn build_service_query(&self, clause: &WhereClause, bindings: &HashMap<String, Term>) -> String {
        // Collect variables used in patterns
        let mut vars: std::collections::HashSet<String> = std::collections::HashSet::new();

        fn collect_vars_from_pattern(pattern: &GraphPattern, vars: &mut std::collections::HashSet<String>) {
            match pattern {
                GraphPattern::Triple(tp) => {
                    if let TermPattern::Variable(v) = &tp.subject { vars.insert(v.clone()); }
                    if let TermPattern::Variable(v) = &tp.predicate { vars.insert(v.clone()); }
                    if let TermPattern::Variable(v) = &tp.object { vars.insert(v.clone()); }
                }
                GraphPattern::Optional(inner) |
                GraphPattern::Group(inner) => {
                    for p in &inner.patterns {
                        collect_vars_from_pattern(p, vars);
                    }
                }
                GraphPattern::Union(left, right) => {
                    for p in &left.patterns {
                        collect_vars_from_pattern(p, vars);
                    }
                    for p in &right.patterns {
                        collect_vars_from_pattern(p, vars);
                    }
                }
                GraphPattern::Bind(_, var) => {
                    vars.insert(var.clone());
                }
                GraphPattern::Filter(_) | GraphPattern::Service(_, _, _) => {}
            }
        }

        for pattern in &clause.patterns {
            collect_vars_from_pattern(pattern, &mut vars);
        }

        // Build SELECT clause - only include unbound variables
        let select_vars: Vec<String> = vars.iter()
            .filter(|v| !bindings.contains_key(*v))
            .map(|v| format!("?{}", v))
            .collect();

        let select_clause = if select_vars.is_empty() {
            "*".to_string()
        } else {
            select_vars.join(" ")
        };

        // Build WHERE clause
        let where_clause = self.patterns_to_sparql(&clause.patterns, bindings);

        format!("SELECT {} WHERE {{ {} }}", select_clause, where_clause)
    }

    /// Convert patterns to SPARQL string, substituting bound variables
    fn patterns_to_sparql(&self, patterns: &[GraphPattern], bindings: &HashMap<String, Term>) -> String {
        let mut parts = Vec::new();

        for pattern in patterns {
            match pattern {
                GraphPattern::Triple(tp) => {
                    let s = self.term_pattern_to_sparql(&tp.subject, bindings);
                    let p = self.term_pattern_to_sparql(&tp.predicate, bindings);
                    let o = self.term_pattern_to_sparql(&tp.object, bindings);
                    parts.push(format!("{} {} {} .", s, p, o));
                }
                GraphPattern::Optional(inner) => {
                    let inner_str = self.patterns_to_sparql(&inner.patterns, bindings);
                    parts.push(format!("OPTIONAL {{ {} }}", inner_str));
                }
                GraphPattern::Filter(expr) => {
                    parts.push(format!("FILTER({})", self.filter_to_sparql(expr, bindings)));
                }
                _ => {}
            }
        }

        parts.join(" ")
    }

    /// Convert a term pattern to SPARQL string
    fn term_pattern_to_sparql(&self, tp: &TermPattern, bindings: &HashMap<String, Term>) -> String {
        match tp {
            TermPattern::Variable(v) => {
                if let Some(term) = bindings.get(v) {
                    self.term_to_sparql(term)
                } else {
                    format!("?{}", v)
                }
            }
            TermPattern::Term(t) => self.term_to_sparql(t),
        }
    }

    /// Convert a term to SPARQL string
    fn term_to_sparql(&self, term: &Term) -> String {
        match term {
            Term::Uri(uri) => format!("<{}>", uri.as_str()),
            Term::Literal(lit) => {
                if let Some(lang) = lit.language() {
                    format!("\"{}\"@{}", lit.value(), lang)
                } else if let Some(dt) = lit.datatype_uri() {
                    format!("\"{}\"^^<{}>", lit.value(), dt)
                } else {
                    format!("\"{}\"", lit.value())
                }
            }
            Term::BlankNode(bn) => format!("_:{}", bn.id()),
            _ => "[]".to_string(),
        }
    }

    /// Convert filter expression to SPARQL string
    fn filter_to_sparql(&self, expr: &FilterExpr, bindings: &HashMap<String, Term>) -> String {
        match expr {
            FilterExpr::Var(v) => {
                if let Some(term) = bindings.get(v) {
                    self.term_to_sparql(term)
                } else {
                    format!("?{}", v)
                }
            }
            FilterExpr::Literal(t) => self.term_to_sparql(t),
            FilterExpr::Bound(v) => format!("BOUND(?{})", v),
            FilterExpr::NotBound(v) => format!("!BOUND(?{})", v),
            FilterExpr::Equals(a, b) => format!("({} = {})", self.filter_to_sparql(a, bindings), self.filter_to_sparql(b, bindings)),
            FilterExpr::NotEquals(a, b) => format!("({} != {})", self.filter_to_sparql(a, bindings), self.filter_to_sparql(b, bindings)),
            FilterExpr::LessThan(a, b) => format!("({} < {})", self.filter_to_sparql(a, bindings), self.filter_to_sparql(b, bindings)),
            FilterExpr::GreaterThan(a, b) => format!("({} > {})", self.filter_to_sparql(a, bindings), self.filter_to_sparql(b, bindings)),
            FilterExpr::And(a, b) => format!("({} && {})", self.filter_to_sparql(a, bindings), self.filter_to_sparql(b, bindings)),
            FilterExpr::Or(a, b) => format!("({} || {})", self.filter_to_sparql(a, bindings), self.filter_to_sparql(b, bindings)),
            FilterExpr::Not(a) => format!("!({})", self.filter_to_sparql(a, bindings)),
            _ => "true".to_string(),
        }
    }

    /// Execute a query against a remote SPARQL endpoint
    fn execute_remote_query(&self, endpoint: &str, query: &str) -> Result<Vec<HashMap<String, Term>>, String> {
        // URL-encode the query using percent-encoding
        let encoded_query: String = utf8_percent_encode(query, NON_ALPHANUMERIC).to_string();
        let url = format!("{}?query={}", endpoint, encoded_query);

        // Execute HTTP request
        let agent = ureq::AgentBuilder::new()
            .timeout(std::time::Duration::from_secs(30))
            .build();

        let response = agent.get(&url)
            .set("Accept", "application/sparql-results+json")
            .call()
            .map_err(|e| format!("HTTP error: {}", e))?;

        let json_str = response.into_string()
            .map_err(|e| format!("Read error: {}", e))?;

        // Parse JSON response
        self.parse_sparql_json_results(&json_str)
    }

    /// Parse SPARQL JSON results format
    fn parse_sparql_json_results(&self, json: &str) -> Result<Vec<HashMap<String, Term>>, String> {
        // Simple JSON parsing for SPARQL results
        // Format: { "results": { "bindings": [ { "var": { "type": "uri", "value": "..." } } ] } }

        let mut solutions = Vec::new();

        // Find bindings array
        if let Some(bindings_start) = json.find("\"bindings\"") {
            let rest = &json[bindings_start..];
            if let Some(arr_start) = rest.find('[') {
                let arr_rest = &rest[arr_start + 1..];

                // Parse each binding object
                let mut depth = 1;
                let mut obj_start = 0;
                let mut in_string = false;
                let mut escape_next = false;

                for (i, c) in arr_rest.char_indices() {
                    if escape_next {
                        escape_next = false;
                        continue;
                    }
                    if c == '\\' {
                        escape_next = true;
                        continue;
                    }
                    if c == '"' {
                        in_string = !in_string;
                        continue;
                    }
                    if in_string {
                        continue;
                    }

                    if c == '{' {
                        if depth == 1 {
                            obj_start = i;
                        }
                        depth += 1;
                    } else if c == '}' {
                        depth -= 1;
                        if depth == 1 {
                            // Found a complete binding object
                            let obj = &arr_rest[obj_start..=i];
                            if let Ok(binding) = self.parse_binding_object(obj) {
                                solutions.push(binding);
                            }
                        }
                    } else if c == ']' && depth == 1 {
                        break;
                    }
                }
            }
        }

        Ok(solutions)
    }

    /// Parse a single binding object from SPARQL JSON results
    fn parse_binding_object(&self, obj: &str) -> Result<HashMap<String, Term>, String> {
        let mut binding = HashMap::new();

        // Very simple parser for { "var": { "type": "...", "value": "..." } }
        let mut pos = 0;
        while let Some(var_start) = obj[pos..].find('"') {
            let rest = &obj[pos + var_start + 1..];
            if let Some(var_end) = rest.find('"') {
                let var_name = &rest[..var_end];

                // Skip to the value object
                let after_var = &rest[var_end + 1..];
                if let Some(obj_start) = after_var.find('{') {
                    let value_rest = &after_var[obj_start..];
                    if let Some(obj_end) = value_rest.find('}') {
                        let value_obj = &value_rest[..obj_end + 1];

                        // Extract type and value
                        let term_type = self.extract_json_value(value_obj, "type");
                        let value = self.extract_json_value(value_obj, "value");

                        if let (Some(t), Some(v)) = (term_type, value) {
                            let term = match t.as_str() {
                                "uri" => Term::uri(&v),
                                "literal" => {
                                    let lang = self.extract_json_value(value_obj, "xml:lang");
                                    let datatype = self.extract_json_value(value_obj, "datatype");
                                    if let Some(l) = lang {
                                        Term::lang_literal(&v, &l)
                                    } else if let Some(dt) = datatype {
                                        Term::typed_literal(&v, &dt)
                                    } else {
                                        Term::literal(&v)
                                    }
                                }
                                "bnode" => Term::blank(&v),
                                _ => Term::literal(&v),
                            };
                            binding.insert(var_name.to_string(), term);
                        }

                        pos = pos + var_start + var_end + obj_start + obj_end + 3;
                        continue;
                    }
                }
            }
            break;
        }

        Ok(binding)
    }

    /// Extract a value from a simple JSON object
    fn extract_json_value(&self, obj: &str, key: &str) -> Option<String> {
        let pattern = format!("\"{}\"", key);
        if let Some(key_pos) = obj.find(&pattern) {
            let rest = &obj[key_pos + pattern.len()..];
            // Skip to the value
            if let Some(colon) = rest.find(':') {
                let after_colon = rest[colon + 1..].trim_start();
                if after_colon.starts_with('"') {
                    let value_start = 1;
                    let value_rest = &after_colon[value_start..];
                    // Find end of string, handling escapes
                    let mut end = 0;
                    let mut escape = false;
                    for (i, c) in value_rest.char_indices() {
                        if escape {
                            escape = false;
                            continue;
                        }
                        if c == '\\' {
                            escape = true;
                            continue;
                        }
                        if c == '"' {
                            end = i;
                            break;
                        }
                    }
                    return Some(value_rest[..end].to_string());
                }
            }
        }
        None
    }

    fn evaluate_where_with_bindings(&self, where_clause: &WhereClause, initial: HashMap<String, Term>) -> Vec<HashMap<String, Term>> {
        let mut solutions = vec![initial];

        for pattern in &where_clause.patterns {
            solutions = self.evaluate_pattern(pattern, solutions);
        }

        solutions
    }

    fn evaluate_where_with_solutions(&self, where_clause: &WhereClause, solutions: Vec<HashMap<String, Term>>) -> Vec<HashMap<String, Term>> {
        let mut result = Vec::new();

        for solution in solutions {
            let sols = self.evaluate_where_with_bindings(where_clause, solution);
            result.extend(sols);
        }

        result
    }

    fn match_triple(&self, pattern: &TriplePattern, triple: &Triple, bindings: &HashMap<String, Term>) -> Option<HashMap<String, Term>> {
        let mut new_bindings = bindings.clone();

        // Match subject
        if !self.match_term_pattern(&pattern.subject, &triple.subject, &mut new_bindings) {
            return None;
        }

        // Match predicate
        if !self.match_term_pattern(&pattern.predicate, &triple.predicate, &mut new_bindings) {
            return None;
        }

        // Match object
        if !self.match_term_pattern(&pattern.object, &triple.object, &mut new_bindings) {
            return None;
        }

        Some(new_bindings)
    }

    fn match_term_pattern(&self, pattern: &TermPattern, term: &Term, bindings: &mut HashMap<String, Term>) -> bool {
        match pattern {
            TermPattern::Variable(var) => {
                if let Some(existing) = bindings.get(var) {
                    existing == term
                } else {
                    bindings.insert(var.clone(), term.clone());
                    true
                }
            }
            TermPattern::Term(t) => t == term,
        }
    }

    fn evaluate_filter(&self, expr: &FilterExpr, bindings: &HashMap<String, Term>) -> bool {
        match expr {
            FilterExpr::Bound(var) => bindings.contains_key(var),
            FilterExpr::NotBound(var) => !bindings.contains_key(var),
            FilterExpr::Equals(left, right) => {
                let l = self.evaluate_expr_to_term(left, bindings);
                let r = self.evaluate_expr_to_term(right, bindings);
                l == r
            }
            FilterExpr::NotEquals(left, right) => {
                let l = self.evaluate_expr_to_term(left, bindings);
                let r = self.evaluate_expr_to_term(right, bindings);
                l != r
            }
            FilterExpr::LessThan(left, right) => {
                self.compare_expr(left, right, bindings, |a, b| a < b)
            }
            FilterExpr::GreaterThan(left, right) => {
                self.compare_expr(left, right, bindings, |a, b| a > b)
            }
            FilterExpr::LessOrEqual(left, right) => {
                self.compare_expr(left, right, bindings, |a, b| a <= b)
            }
            FilterExpr::GreaterOrEqual(left, right) => {
                self.compare_expr(left, right, bindings, |a, b| a >= b)
            }
            FilterExpr::And(left, right) => {
                self.evaluate_filter(left, bindings) && self.evaluate_filter(right, bindings)
            }
            FilterExpr::Or(left, right) => {
                self.evaluate_filter(left, bindings) || self.evaluate_filter(right, bindings)
            }
            FilterExpr::Not(inner) => !self.evaluate_filter(inner, bindings),
            FilterExpr::Regex(text, pattern, flags) => {
                if let Some(text_term) = self.evaluate_expr_to_term(text, bindings) {
                    if let Term::Literal(lit) = text_term {
                        let case_insensitive = flags.as_ref().map(|f| f.contains('i')).unwrap_or(false);
                        let re_pattern = if case_insensitive {
                            format!("(?i){}", pattern)
                        } else {
                            pattern.clone()
                        };
                        if let Ok(re) = regex::Regex::new(&re_pattern) {
                            return re.is_match(lit.value());
                        }
                    }
                }
                false
            }
            FilterExpr::Contains(text, substr) => {
                if let (Some(t), Some(s)) = (self.evaluate_expr_to_term(text, bindings), self.evaluate_expr_to_term(substr, bindings)) {
                    if let (Term::Literal(t_lit), Term::Literal(s_lit)) = (t, s) {
                        return t_lit.value().contains(s_lit.value());
                    }
                }
                false
            }
            FilterExpr::StrStarts(text, prefix) => {
                if let (Some(t), Some(p)) = (self.evaluate_expr_to_term(text, bindings), self.evaluate_expr_to_term(prefix, bindings)) {
                    if let (Term::Literal(t_lit), Term::Literal(p_lit)) = (t, p) {
                        return t_lit.value().starts_with(p_lit.value());
                    }
                }
                false
            }
            FilterExpr::StrEnds(text, suffix) => {
                if let (Some(t), Some(s)) = (self.evaluate_expr_to_term(text, bindings), self.evaluate_expr_to_term(suffix, bindings)) {
                    if let (Term::Literal(t_lit), Term::Literal(s_lit)) = (t, s) {
                        return t_lit.value().ends_with(s_lit.value());
                    }
                }
                false
            }
            FilterExpr::IsIri(inner) => {
                if let Some(term) = self.evaluate_expr_to_term(inner, bindings) {
                    matches!(term, Term::Uri(_))
                } else {
                    false
                }
            }
            FilterExpr::IsLiteral(inner) => {
                if let Some(term) = self.evaluate_expr_to_term(inner, bindings) {
                    matches!(term, Term::Literal(_))
                } else {
                    false
                }
            }
            FilterExpr::IsBlank(inner) => {
                if let Some(term) = self.evaluate_expr_to_term(inner, bindings) {
                    matches!(term, Term::BlankNode(_))
                } else {
                    false
                }
            }
            _ => true, // Default to true for unimplemented expressions
        }
    }

    fn evaluate_expr_to_term(&self, expr: &FilterExpr, bindings: &HashMap<String, Term>) -> Option<Term> {
        match expr {
            FilterExpr::Var(var) => bindings.get(var).cloned(),
            FilterExpr::Literal(term) => Some(term.clone()),
            FilterExpr::Str(inner) => {
                if let Some(term) = self.evaluate_expr_to_term(inner, bindings) {
                    match term {
                        Term::Literal(lit) => Some(Term::literal(lit.value().to_string())),
                        Term::Uri(uri) => Some(Term::literal(uri.as_str().to_string())),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            FilterExpr::Lang(inner) => {
                if let Some(Term::Literal(lit)) = self.evaluate_expr_to_term(inner, bindings) {
                    if let crate::term::Datatype::Language(lang) = lit.datatype() {
                        return Some(Term::literal(lang.to_string()));
                    }
                }
                Some(Term::literal("".to_string()))
            }
            FilterExpr::StrLen(inner) => {
                if let Some(Term::Literal(lit)) = self.evaluate_expr_to_term(inner, bindings) {
                    let len = lit.value().len();
                    return Some(Term::typed_literal(len.to_string(), "http://www.w3.org/2001/XMLSchema#integer"));
                }
                None
            }
            _ => None,
        }
    }

    fn compare_expr<F>(&self, left: &FilterExpr, right: &FilterExpr, bindings: &HashMap<String, Term>, cmp: F) -> bool
    where
        F: Fn(f64, f64) -> bool,
    {
        let l = self.evaluate_expr_to_term(left, bindings);
        let r = self.evaluate_expr_to_term(right, bindings);

        if let (Some(Term::Literal(l_lit)), Some(Term::Literal(r_lit))) = (l, r) {
            if let (Ok(l_num), Ok(r_num)) = (l_lit.value().parse::<f64>(), r_lit.value().parse::<f64>()) {
                return cmp(l_num, r_num);
            }
            // String comparison
            return cmp(l_lit.value().len() as f64, r_lit.value().len() as f64);
        }

        false
    }

    fn apply_order(&self, solutions: &mut Vec<HashMap<String, Term>>, order: &[OrderCondition]) {
        solutions.sort_by(|a, b| {
            for cond in order {
                let a_val = a.get(&cond.variable);
                let b_val = b.get(&cond.variable);

                let ordering = match (a_val, b_val) {
                    (None, None) => std::cmp::Ordering::Equal,
                    (None, Some(_)) => std::cmp::Ordering::Less,
                    (Some(_), None) => std::cmp::Ordering::Greater,
                    (Some(a), Some(b)) => {
                        let a_str = format!("{:?}", a);
                        let b_str = format!("{:?}", b);
                        a_str.cmp(&b_str)
                    }
                };

                let ordering = if cond.ascending { ordering } else { ordering.reverse() };

                if ordering != std::cmp::Ordering::Equal {
                    return ordering;
                }
            }
            std::cmp::Ordering::Equal
        });
    }

    fn instantiate_pattern(&self, pattern: &TriplePattern, bindings: &HashMap<String, Term>) -> Option<Triple> {
        let subject = self.instantiate_term(&pattern.subject, bindings)?;
        let predicate = self.instantiate_term(&pattern.predicate, bindings)?;
        let object = self.instantiate_term(&pattern.object, bindings)?;

        Some(Triple::new(subject, predicate, object))
    }

    fn instantiate_term(&self, pattern: &TermPattern, bindings: &HashMap<String, Term>) -> Option<Term> {
        match pattern {
            TermPattern::Variable(var) => bindings.get(var).cloned(),
            TermPattern::Term(t) => Some(t.clone()),
        }
    }
}

/// Parse and execute a SPARQL query
pub fn execute_sparql(store: &Store, query_str: &str) -> Result<QueryResult, String> {
    let mut parser = SparqlParser::new(query_str);
    let query = parser.parse()?;
    let engine = SparqlEngine::new(store);
    Ok(engine.execute(&query))
}

/// Format query results as SPARQL XML
pub fn format_results_xml(result: &QueryResult) -> String {
    let mut output = String::new();
    output.push_str("<?xml version=\"1.0\"?>\n");
    output.push_str("<sparql xmlns=\"http://www.w3.org/2005/sparql-results#\">\n");

    match result {
        QueryResult::Bindings { variables, solutions } => {
            output.push_str("  <head>\n");
            for var in variables {
                output.push_str(&format!("    <variable name=\"{}\"/>\n", var));
            }
            output.push_str("  </head>\n");
            output.push_str("  <results>\n");

            for solution in solutions {
                output.push_str("    <result>\n");
                for var in variables {
                    if let Some(term) = solution.get(var) {
                        output.push_str(&format!("      <binding name=\"{}\">\n", var));
                        output.push_str(&format_term_xml(term));
                        output.push_str("      </binding>\n");
                    }
                }
                output.push_str("    </result>\n");
            }

            output.push_str("  </results>\n");
        }
        QueryResult::Boolean(value) => {
            output.push_str("  <head/>\n");
            output.push_str(&format!("  <boolean>{}</boolean>\n", value));
        }
        QueryResult::Graph(_) => {
            // Graph results should be serialized as RDF, not SPARQL results
            output.push_str("  <head/>\n");
        }
    }

    output.push_str("</sparql>\n");
    output
}

fn format_term_xml(term: &Term) -> String {
    match term {
        Term::Uri(uri) => format!("        <uri>{}</uri>\n", escape_xml(uri.as_str())),
        Term::Literal(lit) => {
            match lit.datatype() {
                crate::term::Datatype::Plain => {
                    format!("        <literal>{}</literal>\n", escape_xml(lit.value()))
                }
                crate::term::Datatype::Language(lang) => {
                    format!("        <literal xml:lang=\"{}\">{}</literal>\n", lang, escape_xml(lit.value()))
                }
                crate::term::Datatype::Typed(dt) => {
                    format!("        <literal datatype=\"{}\">{}</literal>\n", dt, escape_xml(lit.value()))
                }
            }
        }
        Term::BlankNode(bn) => format!("        <bnode>{}</bnode>\n", bn.id()),
        _ => "".to_string(),
    }
}

fn escape_xml(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// Format query results as JSON
pub fn format_results_json(result: &QueryResult) -> String {
    let mut output = String::new();
    output.push_str("{\n");

    match result {
        QueryResult::Bindings { variables, solutions } => {
            output.push_str("  \"head\": {\n");
            output.push_str("    \"vars\": [");
            let vars: Vec<String> = variables.iter().map(|v| format!("\"{}\"", v)).collect();
            output.push_str(&vars.join(", "));
            output.push_str("]\n");
            output.push_str("  },\n");

            output.push_str("  \"results\": {\n");
            output.push_str("    \"bindings\": [\n");

            for (i, solution) in solutions.iter().enumerate() {
                output.push_str("      {\n");
                let mut first = true;
                for var in variables {
                    if let Some(term) = solution.get(var) {
                        if !first {
                            output.push_str(",\n");
                        }
                        first = false;
                        output.push_str(&format!("        \"{}\": {}", var, format_term_json(term)));
                    }
                }
                output.push('\n');
                output.push_str("      }");
                if i < solutions.len() - 1 {
                    output.push(',');
                }
                output.push('\n');
            }

            output.push_str("    ]\n");
            output.push_str("  }\n");
        }
        QueryResult::Boolean(value) => {
            output.push_str("  \"head\": {},\n");
            output.push_str(&format!("  \"boolean\": {}\n", value));
        }
        QueryResult::Graph(_) => {
            output.push_str("  \"head\": {}\n");
        }
    }

    output.push_str("}\n");
    output
}

fn format_term_json(term: &Term) -> String {
    match term {
        Term::Uri(uri) => {
            format!("{{ \"type\": \"uri\", \"value\": \"{}\" }}", escape_json(uri.as_str()))
        }
        Term::Literal(lit) => {
            match lit.datatype() {
                crate::term::Datatype::Plain => {
                    format!("{{ \"type\": \"literal\", \"value\": \"{}\" }}", escape_json(lit.value()))
                }
                crate::term::Datatype::Language(lang) => {
                    format!("{{ \"type\": \"literal\", \"value\": \"{}\", \"xml:lang\": \"{}\" }}", escape_json(lit.value()), lang)
                }
                crate::term::Datatype::Typed(dt) => {
                    format!("{{ \"type\": \"literal\", \"value\": \"{}\", \"datatype\": \"{}\" }}", escape_json(lit.value()), dt)
                }
            }
        }
        Term::BlankNode(bn) => {
            format!("{{ \"type\": \"bnode\", \"value\": \"{}\" }}", bn.id())
        }
        _ => "null".to_string(),
    }
}

fn escape_json(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_select() {
        let query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
        let mut parser = SparqlParser::new(query);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_select_with_prefix() {
        let query = "PREFIX ex: <http://example.org/> SELECT ?s WHERE { ?s ex:name ?o }";
        let mut parser = SparqlParser::new(query);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_ask() {
        let query = "ASK WHERE { ?s ?p ?o }";
        let mut parser = SparqlParser::new(query);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_execute_select() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/name"),
            Term::literal("Alice".to_string()),
        ));

        let result = execute_sparql(&store, "SELECT ?name WHERE { <http://example.org/alice> <http://example.org/name> ?name }");
        assert!(result.is_ok());

        if let QueryResult::Bindings { solutions, .. } = result.unwrap() {
            assert_eq!(solutions.len(), 1);
        }
    }

    #[test]
    fn test_execute_ask() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/name"),
            Term::literal("Alice".to_string()),
        ));

        let result = execute_sparql(&store, "ASK WHERE { <http://example.org/alice> <http://example.org/name> ?name }");
        assert!(result.is_ok());

        if let QueryResult::Boolean(value) = result.unwrap() {
            assert!(value);
        }
    }
}
