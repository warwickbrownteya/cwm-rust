//! Pluggable reasoning strategies for N3 inference
//!
//! This module provides abstract reasoning strategies that can be composed
//! with different stores and builtin registries.
//!
//! # Available Strategies
//!
//! - `ForwardChaining`: Bottom-up, data-driven inference (default)
//! - `BackwardChaining`: Top-down, goal-directed query answering
//!
//! # Usage
//!
//! ```ignore
//! use cwm::{Store, Rule, BuiltinRegistry, ForwardChaining, BackwardChaining, ReasoningStrategy};
//!
//! // Forward chaining: derive all consequences
//! let mut fc = ForwardChaining::new();
//! fc.infer(&mut store, &rules, &builtins, &config);
//!
//! // Backward chaining: answer specific queries
//! let bc = BackwardChaining::new();
//! let results = bc.query(&store, &goals, &rules, &builtins);
//! ```
//!
//! # Architecture
//!
//! Each strategy implements the `ReasoningStrategy` trait, allowing:
//! - Swappable inference algorithms
//! - Custom optimizations per strategy
//! - Hybrid approaches combining multiple strategies

mod strategy;
mod forward;
mod backward;

pub use strategy::{ReasoningStrategy, StrategyConfig, InferenceStats};
pub use forward::ForwardChaining;
pub use backward::BackwardChaining;
