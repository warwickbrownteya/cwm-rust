//! Pluggable reasoning strategies for N3 inference
//!
//! This module provides abstract reasoning strategies that can be composed
//! with different stores and builtin registries.
//!
//! # Available Strategies
//!
//! - `ForwardChaining`: Bottom-up, data-driven inference (default)
//! - `BackwardChaining`: Top-down, goal-directed query answering (future)
//!
//! # Architecture
//!
//! Each strategy implements the `ReasoningStrategy` trait, allowing:
//! - Swappable inference algorithms
//! - Custom optimizations per strategy
//! - Hybrid approaches combining multiple strategies

mod strategy;
mod forward;

pub use strategy::{ReasoningStrategy, StrategyConfig};
pub use forward::ForwardChaining;
