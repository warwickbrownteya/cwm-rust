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
        projections: Vec<Projection>,
        distinct: bool,
        where_clause: WhereClause,
        group_by: Option<Vec<String>>,
        having: Option<FilterExpr>,
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

/// Projection in SELECT clause (variable or aggregate)
#[derive(Debug, Clone)]
pub enum Projection {
    /// Simple variable: ?var
    Variable(String),
    /// Aggregate function with optional alias: (COUNT(?x) AS ?count)
    Aggregate {
        function: AggregateFunction,
        variable: Option<String>,
        alias: Option<String>,
        distinct: bool,
    },
}

/// Aggregate functions
#[derive(Debug, Clone)]
pub enum AggregateFunction {
    Count,
    Sum,
    Avg,
    Min,
    Max,
    Sample,
    GroupConcat { separator: Option<String> },
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
    /// SERVICE for federated queries (endpoint URI or variable, patterns, silent flag)
    Service(TermPattern, Box<WhereClause>, bool),
    /// Nested subquery (SELECT inside WHERE)
    Subquery(Box<Query>),
    /// VALUES inline data (variables, data rows)
    Values(Vec<String>, Vec<Vec<Option<Term>>>),
    /// MINUS pattern (set difference)
    Minus(Box<WhereClause>),
    /// GRAPH named graph pattern (graph IRI or variable, patterns)
    Graph(TermPattern, Box<WhereClause>),
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
    /// Property path expression (for predicate position)
    Path(PropertyPath),
}

/// Property path expressions for SPARQL 1.1
#[derive(Debug, Clone)]
pub enum PropertyPath {
    /// Single predicate (iri)
    Predicate(Term),
    /// Inverse path (^path)
    Inverse(Box<PropertyPath>),
    /// Sequence path (path1/path2)
    Sequence(Box<PropertyPath>, Box<PropertyPath>),
    /// Alternative path (path1|path2)
    Alternative(Box<PropertyPath>, Box<PropertyPath>),
    /// Zero or more (*path)
    ZeroOrMore(Box<PropertyPath>),
    /// One or more (+path)
    OneOrMore(Box<PropertyPath>),
    /// Zero or one (?path)
    ZeroOrOne(Box<PropertyPath>),
    /// Negated property set (!(iri1|iri2|...))
    NegatedSet(Vec<Term>),
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

        // Parse projections (variables and/or aggregates) or *
        let (variables, projections) = if self.current_char() == '*' {
            self.pos += 1;
            (Vec::new(), Vec::new()) // Empty means all variables
        } else {
            self.parse_projections()?
        };

        self.skip_whitespace();

        // Parse WHERE clause
        if !self.try_keyword("WHERE") {
            return Err("Expected WHERE".to_string());
        }

        let where_clause = self.parse_where_clause()?;

        // Parse GROUP BY
        self.skip_whitespace();
        let group_by = if self.try_keyword("GROUP") {
            self.skip_whitespace();
            if !self.try_keyword("BY") {
                return Err("Expected BY after GROUP".to_string());
            }
            Some(self.parse_group_by_list()?)
        } else {
            None
        };

        // Parse HAVING
        self.skip_whitespace();
        let having = if self.try_keyword("HAVING") {
            Some(self.parse_filter()?)
        } else {
            None
        };

        // Parse ORDER BY
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
            projections,
            distinct,
            where_clause,
            group_by,
            having,
            order_by,
            limit,
            offset,
        })
    }

    /// Parse projections: variables and/or aggregate expressions
    fn parse_projections(&mut self) -> Result<(Vec<String>, Vec<Projection>), String> {
        let mut variables = Vec::new();
        let mut projections = Vec::new();

        loop {
            self.skip_whitespace();

            // Check for aggregate expression: (COUNT(...) AS ?var)
            if self.current_char() == '(' {
                let projection = self.parse_aggregate_projection()?;
                if let Projection::Aggregate { alias, .. } = &projection {
                    if let Some(a) = alias {
                        variables.push(a.clone());
                    }
                }
                projections.push(projection);
            } else if self.current_char() == '?' || self.current_char() == '$' {
                // Simple variable
                self.pos += 1;
                let start = self.pos;
                while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
                    self.pos += 1;
                }
                let var = self.input[start..self.pos].to_string();
                variables.push(var.clone());
                projections.push(Projection::Variable(var));
            } else {
                break;
            }
        }

        if projections.is_empty() {
            return Err("Expected at least one variable or aggregate".to_string());
        }

        Ok((variables, projections))
    }

    /// Parse an aggregate projection: (COUNT(?x) AS ?count)
    fn parse_aggregate_projection(&mut self) -> Result<Projection, String> {
        if self.current_char() != '(' {
            return Err("Expected '(' for aggregate".to_string());
        }
        self.pos += 1;
        self.skip_whitespace();

        // Parse aggregate function
        let agg_distinct = if self.try_keyword("COUNT") {
            self.parse_aggregate_function("COUNT")?
        } else if self.try_keyword("SUM") {
            self.parse_aggregate_function("SUM")?
        } else if self.try_keyword("AVG") {
            self.parse_aggregate_function("AVG")?
        } else if self.try_keyword("MIN") {
            self.parse_aggregate_function("MIN")?
        } else if self.try_keyword("MAX") {
            self.parse_aggregate_function("MAX")?
        } else if self.try_keyword("SAMPLE") {
            self.parse_aggregate_function("SAMPLE")?
        } else if self.try_keyword("GROUP_CONCAT") {
            self.parse_group_concat_function()?
        } else {
            return Err("Expected aggregate function (COUNT, SUM, AVG, MIN, MAX, SAMPLE, GROUP_CONCAT)".to_string());
        };

        let (function, variable, distinct) = agg_distinct;

        // Parse AS ?alias
        self.skip_whitespace();
        let alias = if self.try_keyword("AS") {
            self.skip_whitespace();
            if self.current_char() != '?' && self.current_char() != '$' {
                return Err("Expected variable after AS".to_string());
            }
            self.pos += 1;
            let start = self.pos;
            while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
                self.pos += 1;
            }
            Some(self.input[start..self.pos].to_string())
        } else {
            None
        };

        self.skip_whitespace();
        if self.current_char() != ')' {
            return Err("Expected ')' after aggregate".to_string());
        }
        self.pos += 1;

        Ok(Projection::Aggregate {
            function,
            variable,
            alias,
            distinct,
        })
    }

    /// Parse aggregate function arguments: (DISTINCT? ?var | *)
    fn parse_aggregate_function(&mut self, name: &str) -> Result<(AggregateFunction, Option<String>, bool), String> {
        self.skip_whitespace();
        if self.current_char() != '(' {
            return Err(format!("Expected '(' after {}", name));
        }
        self.pos += 1;
        self.skip_whitespace();

        let distinct = self.try_keyword("DISTINCT");
        self.skip_whitespace();

        let variable = if self.current_char() == '*' {
            self.pos += 1;
            None // COUNT(*) case
        } else if self.current_char() == '?' || self.current_char() == '$' {
            self.pos += 1;
            let start = self.pos;
            while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
                self.pos += 1;
            }
            Some(self.input[start..self.pos].to_string())
        } else {
            return Err("Expected variable or * in aggregate".to_string());
        };

        self.skip_whitespace();
        if self.current_char() != ')' {
            return Err("Expected ')' after aggregate variable".to_string());
        }
        self.pos += 1;

        let function = match name {
            "COUNT" => AggregateFunction::Count,
            "SUM" => AggregateFunction::Sum,
            "AVG" => AggregateFunction::Avg,
            "MIN" => AggregateFunction::Min,
            "MAX" => AggregateFunction::Max,
            "SAMPLE" => AggregateFunction::Sample,
            _ => return Err(format!("Unknown aggregate function: {}", name)),
        };

        Ok((function, variable, distinct))
    }

    /// Parse GROUP_CONCAT function
    fn parse_group_concat_function(&mut self) -> Result<(AggregateFunction, Option<String>, bool), String> {
        self.skip_whitespace();
        if self.current_char() != '(' {
            return Err("Expected '(' after GROUP_CONCAT".to_string());
        }
        self.pos += 1;
        self.skip_whitespace();

        let distinct = self.try_keyword("DISTINCT");
        self.skip_whitespace();

        let variable = if self.current_char() == '?' || self.current_char() == '$' {
            self.pos += 1;
            let start = self.pos;
            while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
                self.pos += 1;
            }
            Some(self.input[start..self.pos].to_string())
        } else {
            return Err("Expected variable in GROUP_CONCAT".to_string());
        };

        // Parse optional SEPARATOR
        self.skip_whitespace();
        let separator = if self.current_char() == ';' {
            self.pos += 1;
            self.skip_whitespace();
            if !self.try_keyword("SEPARATOR") {
                return Err("Expected SEPARATOR".to_string());
            }
            self.skip_whitespace();
            if self.current_char() != '=' {
                return Err("Expected '=' after SEPARATOR".to_string());
            }
            self.pos += 1;
            self.skip_whitespace();
            if self.current_char() == '"' || self.current_char() == '\'' {
                let sep = self.parse_literal()?;
                if let Term::Literal(lit) = sep {
                    Some(lit.value().to_string())
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        self.skip_whitespace();
        if self.current_char() != ')' {
            return Err("Expected ')' after GROUP_CONCAT".to_string());
        }
        self.pos += 1;

        Ok((AggregateFunction::GroupConcat { separator }, variable, distinct))
    }

    /// Parse GROUP BY variable list
    fn parse_group_by_list(&mut self) -> Result<Vec<String>, String> {
        let mut vars = Vec::new();

        loop {
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
            return Err("Expected at least one variable after GROUP BY".to_string());
        }

        Ok(vars)
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

    #[allow(dead_code)]
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
            if self.try_keyword("SERVICE") {
                self.skip_whitespace();
                let is_silent = self.try_keyword("SILENT");
                if is_silent {
                    self.skip_whitespace();
                }

                // Parse endpoint (URI or variable)
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
                    TermPattern::Term(Term::uri(&uri))
                } else if self.current_char() == '?' || self.current_char() == '$' {
                    // Variable endpoint
                    self.pos += 1;
                    let start = self.pos;
                    while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
                        self.pos += 1;
                    }
                    let var_name = self.input[start..self.pos].to_string();
                    TermPattern::Variable(var_name)
                } else {
                    return Err("Expected URI or variable after SERVICE".to_string());
                };

                // Parse the service pattern
                let service_clause = self.parse_where_clause()?;
                patterns.push(GraphPattern::Service(endpoint, Box::new(service_clause), is_silent));
                continue;
            }

            // Check for VALUES inline data
            if self.try_keyword("VALUES") {
                let values_pattern = self.parse_values_clause()?;
                patterns.push(values_pattern);
                continue;
            }

            // Check for MINUS (set difference)
            if self.try_keyword("MINUS") {
                let minus_clause = self.parse_where_clause()?;
                patterns.push(GraphPattern::Minus(Box::new(minus_clause)));
                continue;
            }

            // Check for GRAPH named graph pattern
            if self.try_keyword("GRAPH") {
                self.skip_whitespace();
                let graph_term = self.parse_term_pattern()?;
                let graph_clause = self.parse_where_clause()?;
                patterns.push(GraphPattern::Graph(graph_term, Box::new(graph_clause)));
                continue;
            }

            // Check for nested group or subquery
            if self.current_char() == '{' {
                // Look ahead to check for SELECT (subquery)
                let saved_pos = self.pos;
                self.pos += 1; // consume '{'
                self.skip_whitespace();

                if self.try_keyword("SELECT") {
                    // It's a subquery - reset and parse as a full SELECT
                    self.pos = saved_pos + 1; // after '{'
                    self.skip_whitespace();
                    // We already consumed SELECT above, re-parse it
                    self.pos = saved_pos + 1;
                    self.skip_whitespace();
                    self.try_keyword("SELECT"); // consume again
                    let subquery = self.parse_select()?;
                    self.skip_whitespace();
                    if self.current_char() == '}' {
                        self.pos += 1;
                    }
                    patterns.push(GraphPattern::Subquery(Box::new(subquery)));
                } else {
                    // Regular nested group
                    self.pos = saved_pos; // reset to '{'
                    let nested = self.parse_where_clause()?;
                    patterns.push(GraphPattern::Group(Box::new(nested)));
                }
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
        // Check for property path or simple predicate
        let predicate = self.parse_predicate_or_path()?;

        self.skip_whitespace();
        let object = self.parse_term_pattern()?;

        Ok(TriplePattern { subject, predicate, object })
    }

    /// Parse a predicate which may be a simple term or a property path
    fn parse_predicate_or_path(&mut self) -> Result<TermPattern, String> {
        self.skip_whitespace();

        // Check for 'a' keyword (rdf:type)
        if self.current_char() == 'a' && !self.peek_char(1).is_alphanumeric() {
            self.pos += 1;
            return Ok(TermPattern::Term(Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")));
        }

        // Check for variable
        if self.current_char() == '?' || self.current_char() == '$' {
            return self.parse_term_pattern();
        }

        // Try to parse as property path
        let path = self.parse_property_path()?;

        // If it's just a simple predicate, return as Term
        if let PropertyPath::Predicate(term) = &path {
            Ok(TermPattern::Term(term.clone()))
        } else {
            Ok(TermPattern::Path(path))
        }
    }

    /// Parse a property path expression
    fn parse_property_path(&mut self) -> Result<PropertyPath, String> {
        self.parse_path_alternative()
    }

    /// Parse alternative paths (path1 | path2)
    fn parse_path_alternative(&mut self) -> Result<PropertyPath, String> {
        let mut left = self.parse_path_sequence()?;

        loop {
            self.skip_whitespace();
            if self.current_char() == '|' {
                self.pos += 1;
                self.skip_whitespace();
                let right = self.parse_path_sequence()?;
                left = PropertyPath::Alternative(Box::new(left), Box::new(right));
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse sequence paths (path1 / path2)
    fn parse_path_sequence(&mut self) -> Result<PropertyPath, String> {
        let mut left = self.parse_path_element()?;

        loop {
            self.skip_whitespace();
            if self.current_char() == '/' {
                self.pos += 1;
                self.skip_whitespace();
                let right = self.parse_path_element()?;
                left = PropertyPath::Sequence(Box::new(left), Box::new(right));
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse a path element with optional modifiers (*, +, ?)
    fn parse_path_element(&mut self) -> Result<PropertyPath, String> {
        self.skip_whitespace();

        let mut path = self.parse_path_primary()?;

        // Check for modifiers
        self.skip_whitespace();
        match self.current_char() {
            '*' => {
                self.pos += 1;
                path = PropertyPath::ZeroOrMore(Box::new(path));
            }
            '+' => {
                self.pos += 1;
                path = PropertyPath::OneOrMore(Box::new(path));
            }
            '?' => {
                // Be careful not to consume ? if it's a variable
                let next = self.peek_char(1);
                if !next.is_alphanumeric() && next != '_' {
                    self.pos += 1;
                    path = PropertyPath::ZeroOrOne(Box::new(path));
                }
            }
            _ => {}
        }

        Ok(path)
    }

    /// Parse primary path element (IRI, ^path, (path), !set)
    fn parse_path_primary(&mut self) -> Result<PropertyPath, String> {
        self.skip_whitespace();

        match self.current_char() {
            '^' => {
                // Inverse path
                self.pos += 1;
                let inner = self.parse_path_primary()?;
                Ok(PropertyPath::Inverse(Box::new(inner)))
            }
            '(' => {
                // Grouped path
                self.pos += 1;
                let inner = self.parse_property_path()?;
                self.skip_whitespace();
                if self.current_char() != ')' {
                    return Err("Expected ')' in property path".to_string());
                }
                self.pos += 1;
                Ok(inner)
            }
            '!' => {
                // Negated property set
                self.pos += 1;
                self.skip_whitespace();
                if self.current_char() == '(' {
                    self.pos += 1;
                    let mut negated = Vec::new();
                    loop {
                        self.skip_whitespace();
                        if self.current_char() == ')' {
                            self.pos += 1;
                            break;
                        }
                        let term = self.parse_path_iri()?;
                        negated.push(term);
                        self.skip_whitespace();
                        if self.current_char() == '|' {
                            self.pos += 1;
                        }
                    }
                    Ok(PropertyPath::NegatedSet(negated))
                } else {
                    // Single negated IRI
                    let term = self.parse_path_iri()?;
                    Ok(PropertyPath::NegatedSet(vec![term]))
                }
            }
            _ => {
                // Simple IRI
                let term = self.parse_path_iri()?;
                Ok(PropertyPath::Predicate(term))
            }
        }
    }

    /// Parse an IRI for property path
    fn parse_path_iri(&mut self) -> Result<Term, String> {
        self.skip_whitespace();

        if self.current_char() == '<' {
            let iri = self.parse_iri()?;
            Ok(Term::uri(&iri))
        } else if self.current_char().is_alphabetic() {
            let iri = self.parse_prefixed_name()?;
            Ok(Term::uri(&iri))
        } else if self.current_char() == 'a' && !self.peek_char(1).is_alphanumeric() {
            self.pos += 1;
            Ok(Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
        } else {
            Err(format!("Expected IRI in property path, found: {}", self.current_char()))
        }
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

    /// Parse VALUES clause: VALUES (?var1 ?var2 ...) { (val1 val2 ...) (val3 val4 ...) ... }
    /// Or single variable form: VALUES ?var { val1 val2 ... }
    fn parse_values_clause(&mut self) -> Result<GraphPattern, String> {
        self.skip_whitespace();

        let mut variables = Vec::new();
        let mut data_rows: Vec<Vec<Option<Term>>> = Vec::new();

        // Check if it's a single variable or a list of variables
        if self.current_char() == '(' {
            // Multiple variables: VALUES (?x ?y) { ... }
            self.pos += 1; // consume '('
            loop {
                self.skip_whitespace();
                if self.current_char() == ')' {
                    self.pos += 1;
                    break;
                }
                if self.current_char() == '?' || self.current_char() == '$' {
                    self.pos += 1;
                    let start = self.pos;
                    while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
                        self.pos += 1;
                    }
                    variables.push(self.input[start..self.pos].to_string());
                } else if self.pos >= self.input.len() {
                    return Err("Unexpected end of input in VALUES variables".to_string());
                } else {
                    return Err(format!("Expected variable in VALUES, got '{}'", self.current_char()));
                }
            }
        } else if self.current_char() == '?' || self.current_char() == '$' {
            // Single variable: VALUES ?x { ... }
            self.pos += 1;
            let start = self.pos;
            while self.pos < self.input.len() && (self.current_char().is_alphanumeric() || self.current_char() == '_') {
                self.pos += 1;
            }
            variables.push(self.input[start..self.pos].to_string());
        } else {
            return Err("Expected variable or ( after VALUES".to_string());
        }

        // Now parse the data block { ... }
        self.skip_whitespace();
        if self.current_char() != '{' {
            return Err("Expected { after VALUES variables".to_string());
        }
        self.pos += 1;

        // Parse data rows
        loop {
            self.skip_whitespace();
            if self.current_char() == '}' {
                self.pos += 1;
                break;
            }

            if variables.len() == 1 {
                // Single variable: each value is a row
                let value = self.parse_values_data_value()?;
                data_rows.push(vec![value]);
            } else {
                // Multiple variables: each row is wrapped in ()
                if self.current_char() != '(' {
                    return Err("Expected ( for VALUES data row".to_string());
                }
                self.pos += 1;

                let mut row = Vec::new();
                for _ in 0..variables.len() {
                    self.skip_whitespace();
                    row.push(self.parse_values_data_value()?);
                }

                self.skip_whitespace();
                if self.current_char() != ')' {
                    return Err("Expected ) after VALUES data row".to_string());
                }
                self.pos += 1;
                data_rows.push(row);
            }
        }

        Ok(GraphPattern::Values(variables, data_rows))
    }

    /// Parse a single value in VALUES data block (or UNDEF)
    fn parse_values_data_value(&mut self) -> Result<Option<Term>, String> {
        self.skip_whitespace();

        // Check for UNDEF
        if self.try_keyword("UNDEF") {
            return Ok(None);
        }

        // Parse a term (IRI, literal, etc.)
        match self.parse_term_pattern() {
            Ok(TermPattern::Term(t)) => Ok(Some(t)),
            Ok(TermPattern::Variable(_)) => Err("Variables not allowed in VALUES data".to_string()),
            Ok(TermPattern::Path(_)) => Err("Paths not allowed in VALUES data".to_string()),
            Err(e) => Err(e),
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
            Query::Select { variables, projections, distinct, where_clause, group_by, having, order_by, limit, offset } => {
                let mut solutions = self.evaluate_where(where_clause);

                // Apply GROUP BY and aggregates
                if group_by.is_some() || projections.iter().any(|p| matches!(p, Projection::Aggregate { .. })) {
                    solutions = self.apply_grouping_and_aggregates(&solutions, group_by.as_ref(), projections, having.as_ref());
                }

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
                // Check if predicate is a property path
                if let TermPattern::Path(path) = &tp.predicate {
                    // Handle property path evaluation
                    return self.evaluate_property_path(path, &tp.subject, &tp.object, solutions);
                }

                // Standard triple pattern matching
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
            GraphPattern::Subquery(subquery) => {
                // Execute the nested subquery and join with current solutions
                self.evaluate_subquery(subquery, solutions)
            }
            GraphPattern::Values(variables, data_rows) => {
                // VALUES provides inline data to join with current solutions
                let mut new_solutions = Vec::new();

                for solution in solutions {
                    for row in data_rows {
                        let mut new_sol = solution.clone();
                        let mut compatible = true;

                        for (i, var) in variables.iter().enumerate() {
                            if let Some(Some(value)) = row.get(i) {
                                // Check if already bound with same value
                                if let Some(existing) = solution.get(var) {
                                    if existing != value {
                                        compatible = false;
                                        break;
                                    }
                                } else {
                                    new_sol.insert(var.clone(), value.clone());
                                }
                            }
                            // UNDEF (None) is compatible with any binding
                        }

                        if compatible {
                            new_solutions.push(new_sol);
                        }
                    }
                }

                new_solutions
            }
            GraphPattern::Minus(minus_clause) => {
                // MINUS removes solutions that match the minus clause
                let minus_solutions = self.evaluate_where_with_solutions(minus_clause, solutions.clone());

                solutions.into_iter().filter(|sol| {
                    // Keep solution if no minus solution is compatible
                    !minus_solutions.iter().any(|minus_sol| {
                        // Check if minus_sol is compatible with sol
                        // Compatible means all shared variables have same values
                        minus_sol.iter().all(|(var, value)| {
                            sol.get(var).map(|v| v == value).unwrap_or(true)
                        })
                    })
                }).collect()
            }
            GraphPattern::Graph(graph_term, graph_clause) => {
                // GRAPH specifies a named graph - for now, we treat all data as default graph
                // A full implementation would require named graph support in the store
                match graph_term {
                    TermPattern::Term(_uri) => {
                        // Fixed graph URI - evaluate patterns against that graph
                        // For now, just evaluate against the store
                        self.evaluate_where_with_solutions(graph_clause, solutions)
                    }
                    TermPattern::Variable(var) => {
                        // Variable graph - would need to enumerate all graphs
                        // For now, bind to a placeholder indicating no named graph support
                        let mut new_solutions = Vec::new();
                        for mut sol in self.evaluate_where_with_solutions(graph_clause, solutions) {
                            // Bind graph variable to default graph URI
                            sol.insert(var.clone(), Term::uri("urn:x-local:default-graph"));
                            new_solutions.push(sol);
                        }
                        new_solutions
                    }
                    TermPattern::Path(_) => {
                        // Invalid - paths in GRAPH position
                        solutions
                    }
                }
            }
        }
    }

    /// Evaluate a nested subquery
    fn evaluate_subquery(
        &self,
        subquery: &Query,
        solutions: Vec<HashMap<String, Term>>,
    ) -> Vec<HashMap<String, Term>> {
        // Execute the subquery
        let subquery_result = self.execute(subquery);

        // Extract bindings from the result
        let subquery_bindings = match subquery_result {
            QueryResult::Bindings { solutions: sub_sols, .. } => sub_sols,
            QueryResult::Boolean(true) => vec![HashMap::new()],
            QueryResult::Boolean(false) => vec![],
            QueryResult::Graph(_) => vec![], // CONSTRUCT subqueries not typical
        };

        // Join current solutions with subquery results
        let mut result = Vec::new();
        for solution in solutions {
            for sub_sol in &subquery_bindings {
                // Check for compatible bindings (no conflicts)
                let mut compatible = true;
                let mut merged = solution.clone();

                for (var, val) in sub_sol {
                    if let Some(existing) = merged.get(var) {
                        if existing != val {
                            compatible = false;
                            break;
                        }
                    } else {
                        merged.insert(var.clone(), val.clone());
                    }
                }

                if compatible {
                    result.push(merged);
                }
            }
        }

        result
    }

    /// Execute a SERVICE query against a remote SPARQL endpoint
    fn evaluate_service(
        &self,
        endpoint_pattern: &TermPattern,
        service_clause: &WhereClause,
        solutions: Vec<HashMap<String, Term>>,
        silent: bool,
    ) -> Vec<HashMap<String, Term>> {
        let mut result = Vec::new();

        for solution in solutions {
            // Resolve endpoint from pattern
            let endpoint_uri = match endpoint_pattern {
                TermPattern::Term(Term::Uri(uri)) => uri.as_str().to_string(),
                TermPattern::Variable(var) => {
                    // Look up variable in current bindings
                    if let Some(Term::Uri(uri)) = solution.get(var) {
                        uri.as_str().to_string()
                    } else {
                        if !silent {
                            eprintln!("SERVICE error: variable ?{} not bound to URI", var);
                        }
                        if silent {
                            result.push(solution.clone());
                        }
                        continue;
                    }
                }
                _ => {
                    if !silent {
                        eprintln!("SERVICE error: invalid endpoint pattern");
                    }
                    if silent {
                        result.push(solution.clone());
                    }
                    continue;
                }
            };

            // Build a SPARQL query from the service clause
            let query = self.build_service_query(service_clause, &solution);

            // Execute against remote endpoint
            match self.execute_remote_query(&endpoint_uri, &query) {
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
                        eprintln!("SERVICE error for {}: {}", endpoint_uri, e);
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
                GraphPattern::Subquery(subquery) => {
                    // Collect variables from subquery projections
                    if let Query::Select { variables, .. } = subquery.as_ref() {
                        for var in variables {
                            vars.insert(var.clone());
                        }
                    }
                }
                GraphPattern::Values(variables, _) => {
                    for v in variables {
                        vars.insert(v.clone());
                    }
                }
                GraphPattern::Minus(inner) | GraphPattern::Graph(_, inner) => {
                    for p in &inner.patterns {
                        collect_vars_from_pattern(p, vars);
                    }
                }
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
            TermPattern::Path(path) => self.property_path_to_sparql(path),
        }
    }

    /// Convert a property path to SPARQL string
    fn property_path_to_sparql(&self, path: &PropertyPath) -> String {
        match path {
            PropertyPath::Predicate(term) => self.term_to_sparql(term),
            PropertyPath::Inverse(inner) => format!("^{}", self.property_path_to_sparql(inner)),
            PropertyPath::Sequence(left, right) => {
                format!("{}/{}", self.property_path_to_sparql(left), self.property_path_to_sparql(right))
            }
            PropertyPath::Alternative(left, right) => {
                format!("({}|{})", self.property_path_to_sparql(left), self.property_path_to_sparql(right))
            }
            PropertyPath::ZeroOrMore(inner) => format!("{}*", self.property_path_to_sparql(inner)),
            PropertyPath::OneOrMore(inner) => format!("{}+", self.property_path_to_sparql(inner)),
            PropertyPath::ZeroOrOne(inner) => format!("{}?", self.property_path_to_sparql(inner)),
            PropertyPath::NegatedSet(terms) => {
                let negated: Vec<String> = terms.iter().map(|t| self.term_to_sparql(t)).collect();
                format!("!({})", negated.join("|"))
            }
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

        // Execute HTTP request using shared client with connection pooling
        let client = crate::http_client::get_sync_client();

        let response = client.get(&url)
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
            TermPattern::Path(path) => {
                // For property paths in predicate position, we need special handling
                // This is called when we're just matching a predicate, not traversing a path
                match path {
                    PropertyPath::Predicate(p) => p == term,
                    _ => false, // Complex paths handled elsewhere
                }
            }
        }
    }

    /// Evaluate a property path pattern
    fn evaluate_property_path(
        &self,
        path: &PropertyPath,
        subject: &TermPattern,
        object: &TermPattern,
        solutions: Vec<HashMap<String, Term>>,
    ) -> Vec<HashMap<String, Term>> {
        let mut new_solutions = Vec::new();

        for solution in solutions {
            // Get bound subject if variable is already bound
            let subj_bound = match subject {
                TermPattern::Variable(v) => solution.get(v).cloned(),
                TermPattern::Term(t) => Some(t.clone()),
                TermPattern::Path(_) => None, // Paths shouldn't be in subject position
            };

            // Get bound object if variable is already bound
            let obj_bound = match object {
                TermPattern::Variable(v) => solution.get(v).cloned(),
                TermPattern::Term(t) => Some(t.clone()),
                TermPattern::Path(_) => None,
            };

            // Evaluate the path and get all (subject, object) pairs
            let pairs = self.evaluate_path_pairs(path, subj_bound.as_ref(), obj_bound.as_ref());

            // For each matching pair, create new bindings
            for (s, o) in pairs {
                let mut new_bindings = solution.clone();
                let mut valid = true;

                // Bind subject if it's a variable
                if let TermPattern::Variable(v) = subject {
                    if let Some(existing) = new_bindings.get(v) {
                        if existing != &s {
                            valid = false;
                        }
                    } else {
                        new_bindings.insert(v.clone(), s.clone());
                    }
                }

                // Bind object if it's a variable
                if let TermPattern::Variable(v) = object {
                    if let Some(existing) = new_bindings.get(v) {
                        if existing != &o {
                            valid = false;
                        }
                    } else {
                        new_bindings.insert(v.clone(), o);
                    }
                }

                if valid {
                    new_solutions.push(new_bindings);
                }
            }
        }

        new_solutions
    }

    /// Evaluate a property path and return all matching (subject, object) pairs
    fn evaluate_path_pairs(
        &self,
        path: &PropertyPath,
        subject: Option<&Term>,
        object: Option<&Term>,
    ) -> Vec<(Term, Term)> {
        match path {
            PropertyPath::Predicate(pred) => {
                // Simple predicate: find all triples with this predicate
                let mut pairs = Vec::new();
                for triple in self.store.iter() {
                    if &triple.predicate == pred {
                        // Check subject constraint
                        if let Some(s) = subject {
                            if &triple.subject != s {
                                continue;
                            }
                        }
                        // Check object constraint
                        if let Some(o) = object {
                            if &triple.object != o {
                                continue;
                            }
                        }
                        pairs.push((triple.subject.clone(), triple.object.clone()));
                    }
                }
                pairs
            }

            PropertyPath::Inverse(inner) => {
                // Inverse: swap subject and object
                let inner_pairs = self.evaluate_path_pairs(inner, object, subject);
                inner_pairs.into_iter().map(|(s, o)| (o, s)).collect()
            }

            PropertyPath::Sequence(left, right) => {
                // Sequence: path1/path2 - find intermediate nodes
                let mut pairs = Vec::new();

                // First, evaluate left path from subject
                let left_pairs = self.evaluate_path_pairs(left, subject, None);

                // For each left result, evaluate right path
                for (s, intermediate) in left_pairs {
                    let right_pairs = self.evaluate_path_pairs(right, Some(&intermediate), object);
                    for (_, o) in right_pairs {
                        pairs.push((s.clone(), o));
                    }
                }

                pairs
            }

            PropertyPath::Alternative(left, right) => {
                // Alternative: path1|path2 - union of both paths
                let mut pairs = self.evaluate_path_pairs(left, subject, object);
                let right_pairs = self.evaluate_path_pairs(right, subject, object);
                pairs.extend(right_pairs);

                // Remove duplicates
                pairs.sort_by(|a, b| format!("{:?}", a).cmp(&format!("{:?}", b)));
                pairs.dedup();
                pairs
            }

            PropertyPath::ZeroOrMore(inner) => {
                // Zero or more: path* - reflexive transitive closure
                let mut pairs = Vec::new();
                let mut visited = std::collections::HashSet::new();

                // Start with identity (zero steps)
                if let Some(s) = subject {
                    if object.is_none() || object == Some(s) {
                        pairs.push((s.clone(), s.clone()));
                    }
                    visited.insert(format!("{:?}", s));
                    self.transitive_closure(inner, s, object, &mut pairs, &mut visited);
                } else {
                    // No subject bound - collect all reachable from all nodes
                    let all_nodes = self.collect_all_nodes();
                    for node in &all_nodes {
                        if object.is_none() || object == Some(node) {
                            pairs.push((node.clone(), node.clone()));
                        }
                        let mut node_visited = std::collections::HashSet::new();
                        node_visited.insert(format!("{:?}", node));
                        self.transitive_closure(inner, node, object, &mut pairs, &mut node_visited);
                    }
                }

                pairs
            }

            PropertyPath::OneOrMore(inner) => {
                // One or more: path+ - transitive closure (at least one step)
                let mut pairs = Vec::new();
                let mut visited = std::collections::HashSet::new();

                if let Some(s) = subject {
                    visited.insert(format!("{:?}", s));
                    self.transitive_closure(inner, s, object, &mut pairs, &mut visited);
                } else {
                    // No subject bound - start from all nodes
                    let all_nodes = self.collect_all_nodes();
                    for node in &all_nodes {
                        let mut node_visited = std::collections::HashSet::new();
                        node_visited.insert(format!("{:?}", node));
                        self.transitive_closure(inner, &node, object, &mut pairs, &mut node_visited);
                    }
                }

                pairs
            }

            PropertyPath::ZeroOrOne(inner) => {
                // Zero or one: path? - optional single step
                let mut pairs = Vec::new();

                // Zero steps (identity)
                if let Some(s) = subject {
                    if object.is_none() || object == Some(s) {
                        pairs.push((s.clone(), s.clone()));
                    }
                }

                // One step
                let one_pairs = self.evaluate_path_pairs(inner, subject, object);
                pairs.extend(one_pairs);

                // Remove duplicates
                pairs.sort_by(|a, b| format!("{:?}", a).cmp(&format!("{:?}", b)));
                pairs.dedup();
                pairs
            }

            PropertyPath::NegatedSet(negated) => {
                // Negated property set: !(p1|p2|...) - any predicate NOT in the set
                let mut pairs = Vec::new();

                for triple in self.store.iter() {
                    // Check if predicate is in the negated set
                    let is_negated = negated.iter().any(|p| &triple.predicate == p);
                    if is_negated {
                        continue;
                    }

                    // Check subject constraint
                    if let Some(s) = subject {
                        if &triple.subject != s {
                            continue;
                        }
                    }
                    // Check object constraint
                    if let Some(o) = object {
                        if &triple.object != o {
                            continue;
                        }
                    }
                    pairs.push((triple.subject.clone(), triple.object.clone()));
                }

                pairs
            }
        }
    }

    /// Helper for transitive closure computation
    fn transitive_closure(
        &self,
        path: &PropertyPath,
        start: &Term,
        target: Option<&Term>,
        pairs: &mut Vec<(Term, Term)>,
        visited: &mut std::collections::HashSet<String>,
    ) {
        // Get direct successors via the path
        let successors = self.evaluate_path_pairs(path, Some(start), None);

        for (_, next) in successors {
            let key = format!("{:?}", next);

            // Add pair if it matches target constraint
            if target.is_none() || target == Some(&next) {
                pairs.push((start.clone(), next.clone()));
            }

            // Continue traversal if not visited
            if !visited.contains(&key) {
                visited.insert(key);
                self.transitive_closure(path, &next, target, pairs, visited);
            }
        }
    }

    /// Collect all nodes (subjects and objects) in the store
    fn collect_all_nodes(&self) -> Vec<Term> {
        let mut nodes = std::collections::HashSet::new();
        for triple in self.store.iter() {
            nodes.insert(format!("{:?}", triple.subject));
            nodes.insert(format!("{:?}", triple.object));
        }

        // Re-collect actual terms
        let mut result = Vec::new();
        let mut seen = std::collections::HashSet::new();
        for triple in self.store.iter() {
            let s_key = format!("{:?}", triple.subject);
            if !seen.contains(&s_key) {
                seen.insert(s_key);
                result.push(triple.subject.clone());
            }
            let o_key = format!("{:?}", triple.object);
            if !seen.contains(&o_key) {
                seen.insert(o_key);
                result.push(triple.object.clone());
            }
        }
        result
    }

    /// Apply grouping and aggregate functions
    fn apply_grouping_and_aggregates(
        &self,
        solutions: &[HashMap<String, Term>],
        group_by: Option<&Vec<String>>,
        projections: &[Projection],
        having: Option<&FilterExpr>,
    ) -> Vec<HashMap<String, Term>> {
        // Group solutions by the GROUP BY variables
        let mut groups: HashMap<String, Vec<&HashMap<String, Term>>> = HashMap::new();

        for solution in solutions {
            let group_key = if let Some(vars) = group_by {
                vars.iter()
                    .map(|v| solution.get(v).map(|t| format!("{:?}", t)).unwrap_or_default())
                    .collect::<Vec<_>>()
                    .join("|")
            } else {
                // No GROUP BY - all solutions in one group
                "".to_string()
            };
            groups.entry(group_key).or_default().push(solution);
        }

        // Process each group
        let mut result = Vec::new();
        for (_, group_solutions) in groups {
            let mut row: HashMap<String, Term> = HashMap::new();

            // Add GROUP BY variables to result (take from first solution)
            if let Some(vars) = group_by {
                if let Some(first) = group_solutions.first() {
                    for var in vars {
                        if let Some(val) = first.get(var) {
                            row.insert(var.clone(), val.clone());
                        }
                    }
                }
            }

            // Compute aggregates
            for projection in projections {
                if let Projection::Aggregate { function, variable, alias, distinct } = projection {
                    let agg_result = self.compute_aggregate(function, variable.as_deref(), &group_solutions, *distinct);
                    let var_name = alias.clone().unwrap_or_else(|| {
                        // Generate a default name
                        format!("agg_{}", variable.as_deref().unwrap_or("*"))
                    });
                    row.insert(var_name, agg_result);
                }
            }

            // Apply HAVING filter
            if let Some(having_expr) = having {
                if !self.evaluate_filter(having_expr, &row) {
                    continue;
                }
            }

            result.push(row);
        }

        result
    }

    /// Compute an aggregate function over a group of solutions
    fn compute_aggregate(
        &self,
        function: &AggregateFunction,
        variable: Option<&str>,
        solutions: &[&HashMap<String, Term>],
        distinct: bool,
    ) -> Term {
        // Collect values for the variable
        let values: Vec<&Term> = if let Some(var) = variable {
            solutions.iter()
                .filter_map(|s| s.get(var))
                .collect()
        } else {
            // COUNT(*) - count all solutions
            vec![]
        };

        // Apply DISTINCT if needed
        let values: Vec<&Term> = if distinct {
            let mut seen = std::collections::HashSet::new();
            values.into_iter()
                .filter(|v| seen.insert(format!("{:?}", v)))
                .collect()
        } else {
            values
        };

        match function {
            AggregateFunction::Count => {
                let count = if variable.is_some() {
                    values.len()
                } else {
                    solutions.len() // COUNT(*)
                };
                Term::typed_literal(count.to_string(), "http://www.w3.org/2001/XMLSchema#integer")
            }

            AggregateFunction::Sum => {
                let sum: f64 = values.iter()
                    .filter_map(|v| {
                        if let Term::Literal(lit) = v {
                            lit.value().parse::<f64>().ok()
                        } else {
                            None
                        }
                    })
                    .sum();
                Term::typed_literal(sum.to_string(), "http://www.w3.org/2001/XMLSchema#decimal")
            }

            AggregateFunction::Avg => {
                let nums: Vec<f64> = values.iter()
                    .filter_map(|v| {
                        if let Term::Literal(lit) = v {
                            lit.value().parse::<f64>().ok()
                        } else {
                            None
                        }
                    })
                    .collect();
                let avg = if nums.is_empty() {
                    0.0
                } else {
                    nums.iter().sum::<f64>() / nums.len() as f64
                };
                Term::typed_literal(avg.to_string(), "http://www.w3.org/2001/XMLSchema#decimal")
            }

            AggregateFunction::Min => {
                let min = values.iter()
                    .filter_map(|v| {
                        if let Term::Literal(lit) = v {
                            lit.value().parse::<f64>().ok()
                        } else {
                            None
                        }
                    })
                    .fold(f64::INFINITY, |a, b| a.min(b));
                if min.is_infinite() {
                    Term::literal("".to_string())
                } else {
                    Term::typed_literal(min.to_string(), "http://www.w3.org/2001/XMLSchema#decimal")
                }
            }

            AggregateFunction::Max => {
                let max = values.iter()
                    .filter_map(|v| {
                        if let Term::Literal(lit) = v {
                            lit.value().parse::<f64>().ok()
                        } else {
                            None
                        }
                    })
                    .fold(f64::NEG_INFINITY, |a, b| a.max(b));
                if max.is_infinite() {
                    Term::literal("".to_string())
                } else {
                    Term::typed_literal(max.to_string(), "http://www.w3.org/2001/XMLSchema#decimal")
                }
            }

            AggregateFunction::Sample => {
                // Return the first value
                if let Some(first) = values.first() {
                    (*first).clone()
                } else {
                    Term::literal("".to_string())
                }
            }

            AggregateFunction::GroupConcat { separator } => {
                let sep = separator.as_deref().unwrap_or(" ");
                let concat: String = values.iter()
                    .map(|v| {
                        if let Term::Literal(lit) = v {
                            lit.value().to_string()
                        } else {
                            format!("{:?}", v)
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(sep);
                Term::literal(concat)
            }
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
            TermPattern::Path(path) => {
                // For property paths in CONSTRUCT, use the first predicate if simple
                match path {
                    PropertyPath::Predicate(p) => Some(p.clone()),
                    _ => None, // Complex paths can't be directly instantiated
                }
            }
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

    #[test]
    fn test_parse_property_path_sequence() {
        let query = "PREFIX ex: <http://example.org/> SELECT ?x WHERE { ?s ex:a/ex:b ?x }";
        let mut parser = SparqlParser::new(query);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_property_path_alternative() {
        let query = "PREFIX ex: <http://example.org/> SELECT ?x WHERE { ?s ex:a|ex:b ?x }";
        let mut parser = SparqlParser::new(query);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_property_path_inverse() {
        let query = "PREFIX ex: <http://example.org/> SELECT ?x WHERE { ?s ^ex:parent ?x }";
        let mut parser = SparqlParser::new(query);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_property_path_transitive() {
        let query = "PREFIX ex: <http://example.org/> SELECT ?x WHERE { ?s ex:parent+ ?x }";
        let mut parser = SparqlParser::new(query);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_execute_property_path_sequence() {
        let mut store = Store::new();
        // alice knows bob, bob knows carol
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/bob"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/bob"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/carol"),
        ));

        let result = execute_sparql(
            &store,
            "SELECT ?x WHERE { <http://example.org/alice> <http://example.org/knows>/<http://example.org/knows> ?x }"
        );
        assert!(result.is_ok());

        if let QueryResult::Bindings { solutions, .. } = result.unwrap() {
            assert_eq!(solutions.len(), 1);
            let carol = solutions[0].get("x").unwrap();
            assert_eq!(carol, &Term::uri("http://example.org/carol"));
        }
    }

    #[test]
    fn test_execute_property_path_transitive() {
        let mut store = Store::new();
        // a -> b -> c -> d (chain via parent)
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/parent"),
            Term::uri("http://example.org/b"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/b"),
            Term::uri("http://example.org/parent"),
            Term::uri("http://example.org/c"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/c"),
            Term::uri("http://example.org/parent"),
            Term::uri("http://example.org/d"),
        ));

        // Find all ancestors of 'a' using transitive path
        let result = execute_sparql(
            &store,
            "SELECT ?x WHERE { <http://example.org/a> <http://example.org/parent>+ ?x }"
        );
        assert!(result.is_ok());

        if let QueryResult::Bindings { solutions, .. } = result.unwrap() {
            // Should find b, c, and d
            assert_eq!(solutions.len(), 3);
        }
    }

    #[test]
    fn test_execute_property_path_inverse() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/parent"),
            Term::uri("http://example.org/bob"),
        ));

        // Find child of bob using inverse path
        let result = execute_sparql(
            &store,
            "SELECT ?x WHERE { <http://example.org/bob> ^<http://example.org/parent> ?x }"
        );
        assert!(result.is_ok());

        if let QueryResult::Bindings { solutions, .. } = result.unwrap() {
            assert_eq!(solutions.len(), 1);
            let alice = solutions[0].get("x").unwrap();
            assert_eq!(alice, &Term::uri("http://example.org/alice"));
        }
    }

    #[test]
    fn test_execute_property_path_alternative() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/bob"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/likes"),
            Term::uri("http://example.org/carol"),
        ));

        // Find people alice knows OR likes
        let result = execute_sparql(
            &store,
            "SELECT ?x WHERE { <http://example.org/alice> <http://example.org/knows>|<http://example.org/likes> ?x }"
        );
        assert!(result.is_ok());

        if let QueryResult::Bindings { solutions, .. } = result.unwrap() {
            assert_eq!(solutions.len(), 2);
        }
    }

    #[test]
    fn test_parse_count_aggregate() {
        let query = "SELECT (COUNT(?x) AS ?count) WHERE { ?s ?p ?x }";
        let mut parser = SparqlParser::new(query);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_group_by() {
        let query = "SELECT ?type (COUNT(?x) AS ?count) WHERE { ?x a ?type } GROUP BY ?type";
        let mut parser = SparqlParser::new(query);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_execute_count() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/bob"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/carol"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/bob"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/dave"),
        ));

        let result = execute_sparql(
            &store,
            "SELECT (COUNT(*) AS ?count) WHERE { ?s <http://example.org/knows> ?o }"
        );
        assert!(result.is_ok());

        if let QueryResult::Bindings { solutions, .. } = result.unwrap() {
            assert_eq!(solutions.len(), 1);
            let count = solutions[0].get("count").unwrap();
            if let Term::Literal(lit) = count {
                assert_eq!(lit.value(), "3");
            }
        }
    }

    #[test]
    fn test_execute_group_by_count() {
        let mut store = Store::new();
        let rdf_type = Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");

        // Add people
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            rdf_type.clone(),
            Term::uri("http://example.org/Person"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/bob"),
            rdf_type.clone(),
            Term::uri("http://example.org/Person"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/fido"),
            rdf_type.clone(),
            Term::uri("http://example.org/Dog"),
        ));

        let result = execute_sparql(
            &store,
            "SELECT ?type (COUNT(?x) AS ?count) WHERE { ?x a ?type } GROUP BY ?type"
        );
        assert!(result.is_ok());

        if let QueryResult::Bindings { solutions, .. } = result.unwrap() {
            assert_eq!(solutions.len(), 2); // Person and Dog groups
        }
    }

    #[test]
    fn test_execute_sum() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/value"),
            Term::typed_literal("10", "http://www.w3.org/2001/XMLSchema#integer"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/b"),
            Term::uri("http://example.org/value"),
            Term::typed_literal("20", "http://www.w3.org/2001/XMLSchema#integer"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/c"),
            Term::uri("http://example.org/value"),
            Term::typed_literal("30", "http://www.w3.org/2001/XMLSchema#integer"),
        ));

        let result = execute_sparql(
            &store,
            "SELECT (SUM(?v) AS ?total) WHERE { ?s <http://example.org/value> ?v }"
        );
        assert!(result.is_ok());

        if let QueryResult::Bindings { solutions, .. } = result.unwrap() {
            assert_eq!(solutions.len(), 1);
            let total = solutions[0].get("total").unwrap();
            if let Term::Literal(lit) = total {
                assert_eq!(lit.value(), "60");
            }
        }
    }

    #[test]
    fn test_execute_avg() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/score"),
            Term::typed_literal("10", "http://www.w3.org/2001/XMLSchema#integer"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/b"),
            Term::uri("http://example.org/score"),
            Term::typed_literal("20", "http://www.w3.org/2001/XMLSchema#integer"),
        ));

        let result = execute_sparql(
            &store,
            "SELECT (AVG(?v) AS ?average) WHERE { ?s <http://example.org/score> ?v }"
        );
        assert!(result.is_ok());

        if let QueryResult::Bindings { solutions, .. } = result.unwrap() {
            assert_eq!(solutions.len(), 1);
            let avg = solutions[0].get("average").unwrap();
            if let Term::Literal(lit) = avg {
                assert_eq!(lit.value(), "15");
            }
        }
    }

    #[test]
    fn test_execute_min_max() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/val"),
            Term::typed_literal("5", "http://www.w3.org/2001/XMLSchema#integer"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/b"),
            Term::uri("http://example.org/val"),
            Term::typed_literal("15", "http://www.w3.org/2001/XMLSchema#integer"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/c"),
            Term::uri("http://example.org/val"),
            Term::typed_literal("10", "http://www.w3.org/2001/XMLSchema#integer"),
        ));

        let result = execute_sparql(
            &store,
            "SELECT (MIN(?v) AS ?minimum) (MAX(?v) AS ?maximum) WHERE { ?s <http://example.org/val> ?v }"
        );
        assert!(result.is_ok());

        if let QueryResult::Bindings { solutions, .. } = result.unwrap() {
            assert_eq!(solutions.len(), 1);
            let min = solutions[0].get("minimum").unwrap();
            let max = solutions[0].get("maximum").unwrap();
            if let (Term::Literal(min_lit), Term::Literal(max_lit)) = (min, max) {
                assert_eq!(min_lit.value(), "5");
                assert_eq!(max_lit.value(), "15");
            }
        }
    }

    #[test]
    fn test_parse_subquery() {
        let query = r#"
            SELECT ?person ?name WHERE {
                ?person <http://example.org/name> ?name .
                { SELECT ?person WHERE { ?person a <http://example.org/Person> } }
            }
        "#;
        let mut parser = SparqlParser::new(query);
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_execute_subquery() {
        let mut store = Store::new();
        let rdf_type = Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");

        // Add people
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            rdf_type.clone(),
            Term::uri("http://example.org/Person"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/bob"),
            rdf_type.clone(),
            Term::uri("http://example.org/Person"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/fido"),
            rdf_type.clone(),
            Term::uri("http://example.org/Dog"),
        ));

        // Add names for all
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/name"),
            Term::literal("Alice".to_string()),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/bob"),
            Term::uri("http://example.org/name"),
            Term::literal("Bob".to_string()),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/fido"),
            Term::uri("http://example.org/name"),
            Term::literal("Fido".to_string()),
        ));

        // Use subquery to get only Person entities with names
        let result = execute_sparql(
            &store,
            r#"
                SELECT ?name WHERE {
                    ?x <http://example.org/name> ?name .
                    { SELECT ?x WHERE { ?x a <http://example.org/Person> } }
                }
            "#
        );
        assert!(result.is_ok());

        if let QueryResult::Bindings { solutions, .. } = result.unwrap() {
            // Should only get Alice and Bob (Persons), not Fido (Dog)
            assert_eq!(solutions.len(), 2);
        }
    }

    #[test]
    fn test_execute_subquery_with_aggregate() {
        let mut store = Store::new();

        // Add scores for different categories
        store.add(Triple::new(
            Term::uri("http://example.org/result1"),
            Term::uri("http://example.org/category"),
            Term::uri("http://example.org/A"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/result1"),
            Term::uri("http://example.org/score"),
            Term::typed_literal("10", "http://www.w3.org/2001/XMLSchema#integer"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/result2"),
            Term::uri("http://example.org/category"),
            Term::uri("http://example.org/A"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/result2"),
            Term::uri("http://example.org/score"),
            Term::typed_literal("20", "http://www.w3.org/2001/XMLSchema#integer"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/result3"),
            Term::uri("http://example.org/category"),
            Term::uri("http://example.org/B"),
        ));
        store.add(Triple::new(
            Term::uri("http://example.org/result3"),
            Term::uri("http://example.org/score"),
            Term::typed_literal("30", "http://www.w3.org/2001/XMLSchema#integer"),
        ));

        // Use subquery with aggregate - find max score per category
        let result = execute_sparql(
            &store,
            r#"
                SELECT ?cat ?maxScore WHERE {
                    { SELECT ?cat (MAX(?score) AS ?maxScore) WHERE {
                        ?r <http://example.org/category> ?cat .
                        ?r <http://example.org/score> ?score
                      } GROUP BY ?cat
                    }
                }
            "#
        );
        assert!(result.is_ok());

        if let QueryResult::Bindings { solutions, .. } = result.unwrap() {
            // Should get 2 categories with their max scores
            assert_eq!(solutions.len(), 2);
        }
    }
}
