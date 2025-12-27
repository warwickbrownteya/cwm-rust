//! RDF List representation

use std::fmt;
use super::{Term, Bindings};

/// An RDF list (collection)
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum List {
    /// Empty list (rdf:nil)
    Nil,
    /// Cons cell (first, rest)
    Cons(Box<Term>, Box<List>),
}

impl List {
    /// Create a list from a vector of terms
    pub fn from_vec(items: Vec<Term>) -> Self {
        items.into_iter().rev().fold(List::Nil, |rest, item| {
            List::Cons(Box::new(item), Box::new(rest))
        })
    }

    /// Check if this is the empty list
    pub fn is_nil(&self) -> bool {
        matches!(self, List::Nil)
    }

    /// Get the first element if present
    pub fn first(&self) -> Option<&Term> {
        match self {
            List::Nil => None,
            List::Cons(first, _) => Some(first),
        }
    }

    /// Get the rest of the list
    pub fn rest(&self) -> Option<&List> {
        match self {
            List::Nil => None,
            List::Cons(_, rest) => Some(rest),
        }
    }

    /// Get the length of the list
    pub fn len(&self) -> usize {
        match self {
            List::Nil => 0,
            List::Cons(_, rest) => 1 + rest.len(),
        }
    }

    /// Check if the list is empty
    pub fn is_empty(&self) -> bool {
        self.is_nil()
    }

    /// Convert to a vector of terms
    pub fn to_vec(&self) -> Vec<Term> {
        let mut result = Vec::new();
        let mut current = self;
        while let List::Cons(first, rest) = current {
            result.push((**first).clone());
            current = rest;
        }
        result
    }

    /// Iterate over list elements
    pub fn iter(&self) -> ListIter<'_> {
        ListIter { current: self }
    }

    /// Check if all elements are ground (no variables)
    pub fn is_ground(&self) -> bool {
        match self {
            List::Nil => true,
            List::Cons(first, rest) => first.is_ground() && rest.is_ground(),
        }
    }

    /// Apply bindings to all elements
    pub fn substitute(&self, bindings: &Bindings) -> List {
        match self {
            List::Nil => List::Nil,
            List::Cons(first, rest) => {
                let new_first = super::substitute(first, bindings);
                let new_rest = rest.substitute(bindings);
                List::Cons(Box::new(new_first), Box::new(new_rest))
            }
        }
    }
}

/// Iterator over list elements
pub struct ListIter<'a> {
    current: &'a List,
}

impl<'a> Iterator for ListIter<'a> {
    type Item = &'a Term;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current {
            List::Nil => None,
            List::Cons(first, rest) => {
                self.current = rest;
                Some(first)
            }
        }
    }
}

impl fmt::Debug for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        let mut first = true;
        for item in self.iter() {
            if !first {
                write!(f, " ")?;
            }
            first = false;
            write!(f, "{:?}", item)?;
        }
        write!(f, ")")
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        let mut first = true;
        for item in self.iter() {
            if !first {
                write!(f, " ")?;
            }
            first = false;
            write!(f, "{}", item)?;
        }
        write!(f, ")")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nil() {
        let list = List::Nil;
        assert!(list.is_nil());
        assert!(list.is_empty());
        assert_eq!(list.len(), 0);
    }

    #[test]
    fn test_from_vec() {
        let items = vec![
            Term::literal("a"),
            Term::literal("b"),
            Term::literal("c"),
        ];
        let list = List::from_vec(items);

        assert_eq!(list.len(), 3);
        assert!(!list.is_empty());

        let back = list.to_vec();
        assert_eq!(back.len(), 3);
    }

    #[test]
    fn test_first_rest() {
        let list = List::from_vec(vec![
            Term::literal("a"),
            Term::literal("b"),
        ]);

        assert!(list.first().is_some());
        assert!(list.rest().is_some());
        assert_eq!(list.rest().unwrap().len(), 1);
    }

    #[test]
    fn test_iter() {
        let list = List::from_vec(vec![
            Term::literal("1"),
            Term::literal("2"),
            Term::literal("3"),
        ]);

        let count = list.iter().count();
        assert_eq!(count, 3);
    }
}
