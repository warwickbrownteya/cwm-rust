//! First-order logic term representation
//!
//! Terms are the basic building blocks of first-order logic formulas.
//! A term is either a variable, a constant, or a function application.

use std::collections::HashSet;
use std::fmt;

/// A first-order logic variable
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    /// Variable name
    pub name: String,
    /// Unique ID for internal use
    pub id: usize,
}

impl Variable {
    pub fn new(name: &str, id: usize) -> Self {
        Variable {
            name: name.to_string(),
            id,
        }
    }

    /// Create a fresh variable with a new ID
    pub fn fresh(name: &str, counter: &mut usize) -> Self {
        *counter += 1;
        Variable::new(name, *counter)
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// A function symbol (includes constants as 0-arity functions)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    /// Function name
    pub name: String,
    /// Arity (number of arguments)
    pub arity: usize,
}

impl Function {
    pub fn new(name: &str, arity: usize) -> Self {
        Function {
            name: name.to_string(),
            arity,
        }
    }

    /// Create a constant (0-arity function)
    pub fn constant(name: &str) -> Self {
        Function::new(name, 0)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.name, self.arity)
    }
}

/// A first-order logic term
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FolTerm {
    /// A variable
    Var(Variable),
    /// A function application (constants are 0-arity functions)
    Func(Function, Vec<FolTerm>),
}

impl FolTerm {
    /// Create a variable term
    pub fn var(name: &str, id: usize) -> Self {
        FolTerm::Var(Variable::new(name, id))
    }

    /// Create a constant term
    pub fn constant(name: &str) -> Self {
        FolTerm::Func(Function::constant(name), vec![])
    }

    /// Create a function application
    pub fn func(name: &str, args: Vec<FolTerm>) -> Self {
        FolTerm::Func(Function::new(name, args.len()), args)
    }

    /// Check if this term is a variable
    pub fn is_var(&self) -> bool {
        matches!(self, FolTerm::Var(_))
    }

    /// Check if this term is a constant
    pub fn is_constant(&self) -> bool {
        matches!(self, FolTerm::Func(f, _) if f.arity == 0)
    }

    /// Check if this term is ground (contains no variables)
    pub fn is_ground(&self) -> bool {
        match self {
            FolTerm::Var(_) => false,
            FolTerm::Func(_, args) => args.iter().all(|a| a.is_ground()),
        }
    }

    /// Get all variables in this term
    pub fn variables(&self) -> HashSet<Variable> {
        let mut vars = HashSet::new();
        self.collect_variables(&mut vars);
        vars
    }

    fn collect_variables(&self, vars: &mut HashSet<Variable>) {
        match self {
            FolTerm::Var(v) => {
                vars.insert(v.clone());
            }
            FolTerm::Func(_, args) => {
                for arg in args {
                    arg.collect_variables(vars);
                }
            }
        }
    }

    /// Get the depth of this term
    pub fn depth(&self) -> usize {
        match self {
            FolTerm::Var(_) => 0,
            FolTerm::Func(_, args) => {
                if args.is_empty() {
                    0
                } else {
                    1 + args.iter().map(|a| a.depth()).max().unwrap_or(0)
                }
            }
        }
    }

    /// Get the size (number of symbols) in this term
    pub fn size(&self) -> usize {
        match self {
            FolTerm::Var(_) => 1,
            FolTerm::Func(_, args) => 1 + args.iter().map(|a| a.size()).sum::<usize>(),
        }
    }

    /// Check if this term contains the given variable
    pub fn contains_var(&self, var: &Variable) -> bool {
        match self {
            FolTerm::Var(v) => v == var,
            FolTerm::Func(_, args) => args.iter().any(|a| a.contains_var(var)),
        }
    }

    /// Rename variables to avoid conflicts
    pub fn rename_variables(&self, suffix: &str) -> FolTerm {
        match self {
            FolTerm::Var(v) => FolTerm::Var(Variable::new(
                &format!("{}_{}", v.name, suffix),
                v.id,
            )),
            FolTerm::Func(f, args) => {
                FolTerm::Func(f.clone(), args.iter().map(|a| a.rename_variables(suffix)).collect())
            }
        }
    }

    /// Standardize variables apart using a counter
    pub fn standardize_apart(&self, counter: &mut usize, mapping: &mut std::collections::HashMap<Variable, Variable>) -> FolTerm {
        match self {
            FolTerm::Var(v) => {
                if let Some(new_var) = mapping.get(v) {
                    FolTerm::Var(new_var.clone())
                } else {
                    let new_var = Variable::fresh(&v.name, counter);
                    mapping.insert(v.clone(), new_var.clone());
                    FolTerm::Var(new_var)
                }
            }
            FolTerm::Func(f, args) => {
                FolTerm::Func(f.clone(), args.iter().map(|a| a.standardize_apart(counter, mapping)).collect())
            }
        }
    }
}

impl fmt::Display for FolTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FolTerm::Var(v) => write!(f, "{}", v),
            FolTerm::Func(func, args) => {
                if args.is_empty() {
                    write!(f, "{}", func.name)
                } else {
                    write!(f, "{}(", func.name)?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ",")?;
                        }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, ")")
                }
            }
        }
    }
}

/// A predicate symbol
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Predicate {
    /// Predicate name
    pub name: String,
    /// Arity
    pub arity: usize,
}

impl Predicate {
    pub fn new(name: &str, arity: usize) -> Self {
        Predicate {
            name: name.to_string(),
            arity,
        }
    }

    /// Special equality predicate
    pub fn equality() -> Self {
        Predicate::new("=", 2)
    }

    pub fn is_equality(&self) -> bool {
        self.name == "=" && self.arity == 2
    }
}

impl fmt::Display for Predicate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.name, self.arity)
    }
}

/// An atomic formula (predicate applied to terms)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Atom {
    /// The predicate
    pub predicate: Predicate,
    /// The arguments
    pub args: Vec<FolTerm>,
}

impl Atom {
    pub fn new(name: &str, args: Vec<FolTerm>) -> Self {
        Atom {
            predicate: Predicate::new(name, args.len()),
            args,
        }
    }

    /// Create an equality atom
    pub fn equality(left: FolTerm, right: FolTerm) -> Self {
        Atom {
            predicate: Predicate::equality(),
            args: vec![left, right],
        }
    }

    /// Get all variables in this atom
    pub fn variables(&self) -> HashSet<Variable> {
        let mut vars = HashSet::new();
        for arg in &self.args {
            for v in arg.variables() {
                vars.insert(v);
            }
        }
        vars
    }

    /// Check if this atom is ground
    pub fn is_ground(&self) -> bool {
        self.args.iter().all(|a| a.is_ground())
    }

    /// Rename variables
    pub fn rename_variables(&self, suffix: &str) -> Atom {
        Atom {
            predicate: self.predicate.clone(),
            args: self.args.iter().map(|a| a.rename_variables(suffix)).collect(),
        }
    }

    /// Standardize variables apart
    pub fn standardize_apart(&self, counter: &mut usize, mapping: &mut std::collections::HashMap<Variable, Variable>) -> Atom {
        Atom {
            predicate: self.predicate.clone(),
            args: self.args.iter().map(|a| a.standardize_apart(counter, mapping)).collect(),
        }
    }

    /// Get the weight of this atom (for clause ordering)
    pub fn weight(&self) -> usize {
        1 + self.args.iter().map(|a| a.size()).sum::<usize>()
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.predicate.is_equality() && self.args.len() == 2 {
            write!(f, "{} = {}", self.args[0], self.args[1])
        } else if self.args.is_empty() {
            write!(f, "{}", self.predicate.name)
        } else {
            write!(f, "{}(", self.predicate.name)?;
            for (i, arg) in self.args.iter().enumerate() {
                if i > 0 {
                    write!(f, ",")?;
                }
                write!(f, "{}", arg)?;
            }
            write!(f, ")")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_term_creation() {
        let x = FolTerm::var("x", 1);
        assert!(x.is_var());

        let c = FolTerm::constant("c");
        assert!(c.is_constant());
        assert!(c.is_ground());

        let f = FolTerm::func("f", vec![x.clone(), c.clone()]);
        assert!(!f.is_var());
        assert!(!f.is_constant());
        assert!(!f.is_ground());
    }

    #[test]
    fn test_term_variables() {
        let x = FolTerm::var("x", 1);
        let y = FolTerm::var("y", 2);
        let f = FolTerm::func("f", vec![x.clone(), y.clone()]);

        let vars = f.variables();
        assert_eq!(vars.len(), 2);
    }

    #[test]
    fn test_atom_creation() {
        let x = FolTerm::var("x", 1);
        let a = FolTerm::constant("a");
        let atom = Atom::new("p", vec![x, a]);

        assert_eq!(atom.predicate.name, "p");
        assert_eq!(atom.predicate.arity, 2);
    }

    #[test]
    fn test_equality_atom() {
        let x = FolTerm::var("x", 1);
        let a = FolTerm::constant("a");
        let eq = Atom::equality(x, a);

        assert!(eq.predicate.is_equality());
        assert_eq!(format!("{}", eq), "x = a");
    }
}
