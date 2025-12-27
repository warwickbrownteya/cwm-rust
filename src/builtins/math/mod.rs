//! Math namespace builtins (math:)
//!
//! Implements arithmetic and comparison predicates.
//!
//! # Bidirectional Predicates
//!
//! Several predicates support bidirectional evaluation:
//! - `math:sum` - (a b) math:sum c, can solve for a, b, or c
//! - `math:difference` - a math:difference b = c, can solve for any
//! - `math:product` - (a b) math:product c, can solve for any
//! - `math:quotient` - (a b) math:quotient c, can solve for any

mod arithmetic;
mod comparison;

pub use arithmetic::*;
pub use comparison::*;

use crate::core::{MutableRegistry, BuiltinNamespace, ns};

/// Math namespace containing all arithmetic and comparison builtins
#[derive(Debug, Default)]
pub struct MathNamespace;

impl BuiltinNamespace for MathNamespace {
    fn prefix(&self) -> &str {
        ns::MATH
    }

    fn name(&self) -> &str {
        "math"
    }

    fn register(&self, registry: &mut dyn MutableRegistry) {
        // Arithmetic
        registry.register(Box::new(Sum));
        registry.register(Box::new(Difference));
        registry.register(Box::new(Product));
        registry.register(Box::new(Quotient));
        registry.register(Box::new(Remainder));
        registry.register(Box::new(Exponentiation));
        registry.register(Box::new(Negation));
        registry.register(Box::new(AbsoluteValue));

        // Rounding
        registry.register(Box::new(Rounded));
        registry.register(Box::new(Floor));
        registry.register(Box::new(Ceiling));

        // Comparison
        registry.register(Box::new(LessThan));
        registry.register(Box::new(GreaterThan));
        registry.register(Box::new(NotLessThan));
        registry.register(Box::new(NotGreaterThan));
        registry.register(Box::new(EqualTo));
        registry.register(Box::new(NotEqualTo));

        // Trigonometry
        registry.register(Box::new(Sin));
        registry.register(Box::new(Cos));
        registry.register(Box::new(Tan));
        registry.register(Box::new(ASin));
        registry.register(Box::new(ACos));
        registry.register(Box::new(ATan));
        registry.register(Box::new(SinH));
        registry.register(Box::new(CosH));
        registry.register(Box::new(TanH));

        // Logarithms
        registry.register(Box::new(Log));

        // Aggregates
        registry.register(Box::new(MemberCount));
    }

    fn predicate_count(&self) -> usize {
        27
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::term::{Term, Bindings};
    use crate::core::{EvalResult, BuiltinPredicate};

    #[test]
    fn test_sum_forward() {
        let sum = Sum;
        let bindings = Bindings::default();

        let list = Term::list(vec![
            Term::typed_literal("2", "http://www.w3.org/2001/XMLSchema#integer"),
            Term::typed_literal("3", "http://www.w3.org/2001/XMLSchema#integer"),
        ]);
        let result_var = Term::universal("x");

        let result = sum.evaluate(&list, &result_var, &bindings);
        assert!(result.is_success());
    }

    #[test]
    fn test_less_than() {
        let lt = LessThan;
        let bindings = Bindings::default();

        let two = Term::typed_literal("2", "http://www.w3.org/2001/XMLSchema#integer");
        let five = Term::typed_literal("5", "http://www.w3.org/2001/XMLSchema#integer");

        assert!(lt.evaluate(&two, &five, &bindings).is_success());
        assert!(lt.evaluate(&five, &two, &bindings).is_failure());
    }
}
