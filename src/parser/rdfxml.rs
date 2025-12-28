//! RDF/XML Parser
//!
//! Parses RDF/XML format into triples. Supports:
//! - rdf:Description elements with rdf:about
//! - Typed nodes (e.g., <ex:Person rdf:about="...">)
//! - Property elements with text content or rdf:resource
//! - Blank nodes with rdf:nodeID or anonymous
//! - rdf:parseType="Collection", "Literal", "Resource"
//! - xml:base handling
//! - Namespace prefix declarations

use crate::term::{Term, Triple, BlankNode};
use crate::parser::ParseError;
use indexmap::IndexMap;

/// RDF/XML parser
pub struct RdfXmlParser {
    base: Option<String>,
    prefixes: IndexMap<String, String>,
    triples: Vec<Triple>,
    blank_counter: u64,
}

impl RdfXmlParser {
    /// Create a new RDF/XML parser
    pub fn new() -> Self {
        RdfXmlParser {
            base: None,
            prefixes: IndexMap::new(),
            triples: Vec::new(),
            blank_counter: 0,
        }
    }

    /// Parse RDF/XML content
    pub fn parse(&mut self, content: &str) -> Result<RdfXmlResult, ParseError> {
        // Remove BOM if present
        let content = content.trim_start_matches('\u{feff}');

        // Find the root element
        let content = self.skip_xml_declaration(content);
        let content = self.skip_doctype(content);

        // Parse RDF element
        let content = content.trim();

        if content.starts_with('<') {
            self.parse_element(content)?;
        }

        Ok(RdfXmlResult {
            triples: std::mem::take(&mut self.triples),
            prefixes: std::mem::take(&mut self.prefixes),
        })
    }

    /// Skip XML declaration (<?xml ...?>)
    fn skip_xml_declaration<'a>(&self, content: &'a str) -> &'a str {
        let content = content.trim();
        if content.starts_with("<?xml") {
            if let Some(end) = content.find("?>") {
                return content[end + 2..].trim();
            }
        }
        content
    }

    /// Skip DOCTYPE declaration
    fn skip_doctype<'a>(&self, content: &'a str) -> &'a str {
        let content = content.trim();
        if content.starts_with("<!DOCTYPE") {
            // Simple case: <!DOCTYPE ... >
            let mut depth = 0;
            for (i, c) in content.char_indices() {
                match c {
                    '<' => depth += 1,
                    '>' => {
                        depth -= 1;
                        if depth == 0 {
                            return content[i + 1..].trim();
                        }
                    }
                    _ => {}
                }
            }
        }
        content
    }

    /// Generate a fresh blank node
    fn fresh_blank(&mut self) -> BlankNode {
        self.blank_counter += 1;
        BlankNode::labeled(format!("genid{}", self.blank_counter))
    }

    /// Parse an element (may be rdf:RDF or a top-level node)
    fn parse_element(&mut self, content: &str) -> Result<(), ParseError> {
        let content = content.trim();

        if !content.starts_with('<') {
            return Ok(());
        }

        // Parse opening tag
        let (tag_name, attrs, rest, self_closing) = self.parse_opening_tag(content)?;

        // Handle namespaces from attributes
        for (name, value) in &attrs {
            if name == "xmlns" {
                self.prefixes.insert(String::new(), value.clone());
            } else if name.starts_with("xmlns:") {
                let prefix = &name[6..];
                self.prefixes.insert(prefix.to_string(), value.clone());
            } else if name == "xml:base" {
                self.base = Some(value.clone());
            }
        }

        // Expand the tag name
        let expanded = self.expand_qname(&tag_name);

        // Handle rdf:RDF container
        if expanded == "http://www.w3.org/1999/02/22-rdf-syntax-ns#RDF" {
            if !self_closing {
                self.parse_node_elements(rest)?;
            }
        } else {
            // This is a top-level node element
            self.parse_node_element_content(&tag_name, &attrs, rest, self_closing)?;
        }

        Ok(())
    }

    /// Parse multiple node elements inside rdf:RDF
    fn parse_node_elements(&mut self, content: &str) -> Result<(), ParseError> {
        let mut remaining = content.trim();

        while !remaining.is_empty() {
            remaining = remaining.trim();

            // Check for closing rdf:RDF tag
            if remaining.starts_with("</") {
                break;
            }

            // Skip comments
            if remaining.starts_with("<!--") {
                if let Some(end) = remaining.find("-->") {
                    remaining = &remaining[end + 3..];
                    continue;
                }
            }

            // Parse a node element
            if remaining.starts_with('<') {
                let (tag_name, attrs, rest, self_closing) = self.parse_opening_tag(remaining)?;

                // Process namespaces
                for (name, value) in &attrs {
                    if name == "xmlns" {
                        self.prefixes.insert(String::new(), value.clone());
                    } else if name.starts_with("xmlns:") {
                        let prefix = &name[6..];
                        self.prefixes.insert(prefix.to_string(), value.clone());
                    }
                }

                remaining = self.parse_node_element_content(&tag_name, &attrs, rest, self_closing)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    /// Parse the content of a node element and return remaining content
    fn parse_node_element_content<'a>(
        &mut self,
        tag_name: &str,
        attrs: &[(String, String)],
        content: &'a str,
        self_closing: bool,
    ) -> Result<&'a str, ParseError> {
        // Determine the subject
        let subject = self.subject_from_attrs(attrs)?;

        // If not rdf:Description, add type triple
        let expanded = self.expand_qname(tag_name);
        if expanded != "http://www.w3.org/1999/02/22-rdf-syntax-ns#Description" {
            self.triples.push(Triple::new(
                subject.clone(),
                Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
                Term::uri(expanded),
            ));
        }

        // Add property triples from attributes
        for (name, value) in attrs {
            if name.starts_with("xmlns") || name == "xml:base" {
                continue;
            }
            if name.starts_with("rdf:") || name.starts_with("xml:") {
                let attr_name = name.split(':').last().unwrap();
                if ["about", "ID", "nodeID", "resource", "parseType"].contains(&attr_name) {
                    continue;
                }
            }

            // This is a property attribute
            let prop_uri = self.expand_qname(name);
            self.triples.push(Triple::new(
                subject.clone(),
                Term::uri(prop_uri),
                Term::literal(value.clone()),
            ));
        }

        if self_closing {
            return Ok(content);
        }

        // Parse property elements until closing tag
        let mut remaining = content.trim();
        let closing_tag = format!("</{}", tag_name);

        while !remaining.is_empty() {
            remaining = remaining.trim();

            // Check for closing tag
            if remaining.starts_with(&closing_tag) {
                // Find end of closing tag
                if let Some(end) = remaining.find('>') {
                    return Ok(&remaining[end + 1..]);
                }
            }

            // Skip comments
            if remaining.starts_with("<!--") {
                if let Some(end) = remaining.find("-->") {
                    remaining = &remaining[end + 3..];
                    continue;
                }
            }

            // Parse property element
            if remaining.starts_with('<') && !remaining.starts_with("</") {
                remaining = self.parse_property_element(&subject, remaining)?;
            } else {
                // Skip text content (whitespace usually)
                if let Some(pos) = remaining.find('<') {
                    remaining = &remaining[pos..];
                } else {
                    break;
                }
            }
        }

        Ok(remaining)
    }

    /// Parse a property element
    fn parse_property_element<'a>(
        &mut self,
        subject: &Term,
        content: &'a str,
    ) -> Result<&'a str, ParseError> {
        let (prop_name, attrs, rest, self_closing) = self.parse_opening_tag(content)?;
        let prop_uri = self.expand_qname(&prop_name);

        // Check for rdf:resource attribute
        if let Some((_, resource)) = attrs.iter().find(|(n, _)| n == "rdf:resource") {
            let resource_uri = self.resolve_uri(resource);
            self.triples.push(Triple::new(
                subject.clone(),
                Term::uri(prop_uri),
                Term::uri(resource_uri),
            ));

            if self_closing {
                return Ok(rest);
            }

            // Skip to closing tag
            let closing = format!("</{}", prop_name);
            if let Some(pos) = rest.find(&closing) {
                if let Some(end) = rest[pos..].find('>') {
                    return Ok(&rest[pos + end + 1..]);
                }
            }
            return Ok(rest);
        }

        // Check for rdf:nodeID
        if let Some((_, node_id)) = attrs.iter().find(|(n, _)| n == "rdf:nodeID") {
            self.triples.push(Triple::new(
                subject.clone(),
                Term::uri(prop_uri),
                Term::BlankNode(BlankNode::labeled(node_id.clone())),
            ));

            if self_closing {
                return Ok(rest);
            }

            let closing = format!("</{}", prop_name);
            if let Some(pos) = rest.find(&closing) {
                if let Some(end) = rest[pos..].find('>') {
                    return Ok(&rest[pos + end + 1..]);
                }
            }
            return Ok(rest);
        }

        // Check for rdf:parseType
        if let Some((_, parse_type)) = attrs.iter().find(|(n, _)| n == "rdf:parseType") {
            match parse_type.as_str() {
                "Literal" => {
                    // Content is XML literal
                    let closing = format!("</{}", prop_name);
                    if let Some(pos) = rest.find(&closing) {
                        let xml_content = &rest[..pos];
                        self.triples.push(Triple::new(
                            subject.clone(),
                            Term::uri(prop_uri),
                            Term::typed_literal(
                                xml_content.to_string(),
                                "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral",
                            ),
                        ));
                        if let Some(end) = rest[pos..].find('>') {
                            return Ok(&rest[pos + end + 1..]);
                        }
                    }
                    return Ok(rest);
                }
                "Resource" => {
                    // Nested properties on blank node
                    let blank = self.fresh_blank();
                    self.triples.push(Triple::new(
                        subject.clone(),
                        Term::uri(prop_uri),
                        Term::BlankNode(blank.clone()),
                    ));

                    // Parse nested properties
                    let remaining = self.parse_nested_properties(&Term::BlankNode(blank), rest)?;

                    let closing = format!("</{}", prop_name);
                    if let Some(pos) = remaining.find(&closing) {
                        if let Some(end) = remaining[pos..].find('>') {
                            return Ok(&remaining[pos + end + 1..]);
                        }
                    }
                    return Ok(remaining);
                }
                "Collection" => {
                    // RDF collection
                    let (items, remaining) = self.parse_collection(rest, &prop_name)?;
                    if items.is_empty() {
                        self.triples.push(Triple::new(
                            subject.clone(),
                            Term::uri(prop_uri),
                            Term::uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"),
                        ));
                    } else {
                        let list_head = self.build_rdf_list(&items);
                        self.triples.push(Triple::new(
                            subject.clone(),
                            Term::uri(prop_uri),
                            list_head,
                        ));
                    }
                    return Ok(remaining);
                }
                _ => {}
            }
        }

        if self_closing {
            // Empty property - use empty string literal
            self.triples.push(Triple::new(
                subject.clone(),
                Term::uri(prop_uri),
                Term::literal(""),
            ));
            return Ok(rest);
        }

        // Parse content - could be text or nested node element
        let rest = rest.trim();

        if rest.starts_with('<') && !rest.starts_with(&format!("</{}", prop_name)) {
            // Nested node element
            let (nested_tag, nested_attrs, nested_rest, nested_self_closing) =
                self.parse_opening_tag(rest)?;

            let nested_subject = self.subject_from_attrs(&nested_attrs)?;
            self.triples.push(Triple::new(
                subject.clone(),
                Term::uri(prop_uri),
                nested_subject.clone(),
            ));

            // Parse the nested node element
            let remaining =
                self.parse_node_element_content(&nested_tag, &nested_attrs, nested_rest, nested_self_closing)?;

            // Skip to closing tag
            let closing = format!("</{}", prop_name);
            if let Some(pos) = remaining.find(&closing) {
                if let Some(end) = remaining[pos..].find('>') {
                    return Ok(&remaining[pos + end + 1..]);
                }
            }
            return Ok(remaining);
        } else {
            // Text content
            let closing = format!("</{}", prop_name);
            if let Some(pos) = rest.find(&closing) {
                let text = self.decode_entities(&rest[..pos]);

                // Check for datatype
                let literal = if let Some((_, datatype)) = attrs.iter().find(|(n, _)| n == "rdf:datatype")
                {
                    let dt_uri = self.resolve_uri(datatype);
                    Term::typed_literal(text, dt_uri)
                } else if let Some((_, lang)) = attrs.iter().find(|(n, _)| n == "xml:lang") {
                    Term::lang_literal(text, lang.clone())
                } else {
                    Term::literal(text)
                };

                self.triples.push(Triple::new(
                    subject.clone(),
                    Term::uri(prop_uri),
                    literal,
                ));

                if let Some(end) = rest[pos..].find('>') {
                    return Ok(&rest[pos + end + 1..]);
                }
            }
        }

        Ok(rest)
    }

    /// Parse nested property elements (for parseType="Resource")
    fn parse_nested_properties<'a>(
        &mut self,
        subject: &Term,
        content: &'a str,
    ) -> Result<&'a str, ParseError> {
        let mut remaining = content.trim();

        while !remaining.is_empty() {
            remaining = remaining.trim();

            if remaining.starts_with("</") {
                break;
            }

            if remaining.starts_with("<!--") {
                if let Some(end) = remaining.find("-->") {
                    remaining = &remaining[end + 3..];
                    continue;
                }
            }

            if remaining.starts_with('<') {
                remaining = self.parse_property_element(subject, remaining)?;
            } else {
                if let Some(pos) = remaining.find('<') {
                    remaining = &remaining[pos..];
                } else {
                    break;
                }
            }
        }

        Ok(remaining)
    }

    /// Parse a collection (parseType="Collection")
    fn parse_collection<'a>(
        &mut self,
        content: &'a str,
        prop_name: &str,
    ) -> Result<(Vec<Term>, &'a str), ParseError> {
        let mut items = Vec::new();
        let mut remaining = content.trim();
        let closing = format!("</{}", prop_name);

        while !remaining.is_empty() {
            remaining = remaining.trim();

            if remaining.starts_with(&closing) {
                if let Some(end) = remaining.find('>') {
                    return Ok((items, &remaining[end + 1..]));
                }
            }

            if remaining.starts_with("<!--") {
                if let Some(end) = remaining.find("-->") {
                    remaining = &remaining[end + 3..];
                    continue;
                }
            }

            if remaining.starts_with('<') && !remaining.starts_with("</") {
                let (tag, attrs, rest, self_closing) = self.parse_opening_tag(remaining)?;
                let item_subject = self.subject_from_attrs(&attrs)?;
                items.push(item_subject.clone());

                remaining =
                    self.parse_node_element_content(&tag, &attrs, rest, self_closing)?;
            } else {
                if let Some(pos) = remaining.find('<') {
                    remaining = &remaining[pos..];
                } else {
                    break;
                }
            }
        }

        Ok((items, remaining))
    }

    /// Build RDF list from items
    fn build_rdf_list(&mut self, items: &[Term]) -> Term {
        let rdf_first = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
        let rdf_rest = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
        let rdf_nil = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";

        if items.is_empty() {
            return Term::uri(rdf_nil);
        }

        let mut prev_node: Option<BlankNode> = None;
        let mut first_node: Option<Term> = None;

        for (i, item) in items.iter().enumerate() {
            let node = self.fresh_blank();

            self.triples.push(Triple::new(
                Term::BlankNode(node.clone()),
                Term::uri(rdf_first),
                item.clone(),
            ));

            if i == items.len() - 1 {
                self.triples.push(Triple::new(
                    Term::BlankNode(node.clone()),
                    Term::uri(rdf_rest),
                    Term::uri(rdf_nil),
                ));
            }

            if let Some(prev) = prev_node {
                self.triples.push(Triple::new(
                    Term::BlankNode(prev),
                    Term::uri(rdf_rest),
                    Term::BlankNode(node.clone()),
                ));
            }

            if first_node.is_none() {
                first_node = Some(Term::BlankNode(node.clone()));
            }

            prev_node = Some(node);
        }

        first_node.unwrap_or_else(|| Term::uri(rdf_nil))
    }

    /// Get subject from attributes
    fn subject_from_attrs(&mut self, attrs: &[(String, String)]) -> Result<Term, ParseError> {
        // Check for rdf:about
        if let Some((_, about)) = attrs.iter().find(|(n, _)| n == "rdf:about") {
            let uri = self.resolve_uri(about);
            return Ok(Term::uri(uri));
        }

        // Check for rdf:ID
        if let Some((_, id)) = attrs.iter().find(|(n, _)| n == "rdf:ID") {
            let uri = if let Some(ref base) = self.base {
                format!("{}#{}", base, id)
            } else {
                format!("#{}", id)
            };
            return Ok(Term::uri(uri));
        }

        // Check for rdf:nodeID
        if let Some((_, node_id)) = attrs.iter().find(|(n, _)| n == "rdf:nodeID") {
            return Ok(Term::BlankNode(BlankNode::labeled(node_id.clone())));
        }

        // Anonymous blank node
        Ok(Term::BlankNode(self.fresh_blank()))
    }

    /// Parse an opening tag, returning (name, attrs, rest, is_self_closing)
    fn parse_opening_tag<'a>(
        &self,
        content: &'a str,
    ) -> Result<(String, Vec<(String, String)>, &'a str, bool), ParseError> {
        let content = content.trim_start_matches('<');
        let mut attrs = Vec::new();

        // Find tag name
        let mut chars = content.char_indices();
        let mut name_end = 0;

        for (i, c) in &mut chars {
            if c.is_whitespace() || c == '>' || c == '/' {
                name_end = i;
                break;
            }
        }

        if name_end == 0 {
            // Find end of name
            if let Some(pos) = content.find(|c: char| c.is_whitespace() || c == '>' || c == '/') {
                name_end = pos;
            } else {
                name_end = content.len();
            }
        }

        let tag_name = content[..name_end].to_string();
        let mut rest = &content[name_end..];

        // Parse attributes
        loop {
            rest = rest.trim_start();

            if rest.is_empty() {
                return Err(ParseError::Syntax {
                    position: 0,
                    message: "Unexpected end of tag".to_string(),
                });
            }

            if rest.starts_with("/>") {
                return Ok((tag_name, attrs, &rest[2..], true));
            }

            if rest.starts_with('>') {
                return Ok((tag_name, attrs, &rest[1..], false));
            }

            // Parse attribute
            let attr_end = rest
                .find(|c: char| c == '=' || c.is_whitespace())
                .unwrap_or(rest.len());
            let attr_name = rest[..attr_end].to_string();
            rest = &rest[attr_end..];

            rest = rest.trim_start();
            if rest.starts_with('=') {
                rest = &rest[1..];
            }
            rest = rest.trim_start();

            // Get attribute value
            if rest.starts_with('"') || rest.starts_with('\'') {
                let quote = rest.chars().next().unwrap();
                rest = &rest[1..];
                if let Some(end) = rest.find(quote) {
                    let value = self.decode_entities(&rest[..end]);
                    attrs.push((attr_name, value));
                    rest = &rest[end + 1..];
                }
            }
        }
    }

    /// Expand a QName to a full URI
    fn expand_qname(&self, qname: &str) -> String {
        if let Some(pos) = qname.find(':') {
            let prefix = &qname[..pos];
            let local = &qname[pos + 1..];
            if let Some(ns) = self.prefixes.get(prefix) {
                return format!("{}{}", ns, local);
            }
        } else {
            // Default namespace
            if let Some(ns) = self.prefixes.get("") {
                return format!("{}{}", ns, qname);
            }
        }
        qname.to_string()
    }

    /// Resolve a URI reference against base
    fn resolve_uri(&self, uri: &str) -> String {
        if uri.contains("://") {
            return uri.to_string();
        }

        if uri.starts_with('#') {
            if let Some(ref base) = self.base {
                return format!("{}{}", base, uri);
            }
        }

        if let Some(ref base) = self.base {
            if uri.starts_with('/') {
                // Absolute path - find base scheme and authority
                if let Some(pos) = base.find("://") {
                    if let Some(end) = base[pos + 3..].find('/') {
                        return format!("{}{}", &base[..pos + 3 + end], uri);
                    }
                }
            } else {
                // Relative path
                if let Some(pos) = base.rfind('/') {
                    return format!("{}/{}", &base[..pos], uri);
                }
            }
            return format!("{}/{}", base, uri);
        }

        uri.to_string()
    }

    /// Decode XML entities
    fn decode_entities(&self, text: &str) -> String {
        text.replace("&lt;", "<")
            .replace("&gt;", ">")
            .replace("&amp;", "&")
            .replace("&quot;", "\"")
            .replace("&apos;", "'")
    }
}

/// Result of RDF/XML parsing
pub struct RdfXmlResult {
    pub triples: Vec<Triple>,
    pub prefixes: IndexMap<String, String>,
}

/// Parse RDF/XML content
pub fn parse_rdfxml(content: &str) -> Result<RdfXmlResult, ParseError> {
    let mut parser = RdfXmlParser::new();
    parser.parse(content)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_rdfxml() {
        let xml = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:ex="http://example.org/">
  <rdf:Description rdf:about="http://example.org/subject">
    <ex:predicate>value</ex:predicate>
  </rdf:Description>
</rdf:RDF>"#;

        let result = parse_rdfxml(xml).unwrap();
        assert_eq!(result.triples.len(), 1);

        let t = &result.triples[0];
        assert_eq!(format!("{}", t.subject), "<http://example.org/subject>");
        assert_eq!(format!("{}", t.predicate), "<http://example.org/predicate>");
    }

    #[test]
    fn test_typed_node() {
        let xml = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:ex="http://example.org/">
  <ex:Person rdf:about="http://example.org/alice"/>
</rdf:RDF>"#;

        let result = parse_rdfxml(xml).unwrap();
        assert_eq!(result.triples.len(), 1);

        let t = &result.triples[0];
        assert_eq!(format!("{}", t.predicate), "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>");
        assert_eq!(format!("{}", t.object), "<http://example.org/Person>");
    }

    #[test]
    fn test_resource_attribute() {
        let xml = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:ex="http://example.org/">
  <rdf:Description rdf:about="http://example.org/subject">
    <ex:knows rdf:resource="http://example.org/bob"/>
  </rdf:Description>
</rdf:RDF>"#;

        let result = parse_rdfxml(xml).unwrap();
        assert_eq!(result.triples.len(), 1);

        let t = &result.triples[0];
        assert_eq!(format!("{}", t.object), "<http://example.org/bob>");
    }
}
