//! Blank node representation

use std::fmt;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::RwLock;

/// Counter for generating unique blank node IDs
static BLANK_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Run-specific prefix for unique blank node IDs (mode 'u')
static RUN_PREFIX: RwLock<Option<String>> = RwLock::new(None);

/// Set a run-specific prefix for blank node IDs
/// This is used when mode 'u' is enabled to ensure unique IDs across runs
pub fn set_run_prefix(prefix: String) {
    let mut guard = RUN_PREFIX.write().unwrap();
    *guard = Some(prefix);
}

/// Clear the run-specific prefix
pub fn clear_run_prefix() {
    let mut guard = RUN_PREFIX.write().unwrap();
    *guard = None;
}

/// Get the current run prefix if set
fn get_run_prefix() -> Option<String> {
    RUN_PREFIX.read().ok().and_then(|guard| guard.clone())
}

/// A blank node (anonymous node)
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BlankNode {
    /// Internal ID for the blank node
    id: u64,
    /// Optional label (for round-tripping)
    label: Option<String>,
}

impl BlankNode {
    /// Create a fresh blank node with a unique ID
    pub fn fresh() -> Self {
        BlankNode {
            id: BLANK_COUNTER.fetch_add(1, Ordering::SeqCst),
            label: None,
        }
    }

    /// Create a blank node with a label
    pub fn labeled(label: String) -> Self {
        BlankNode {
            id: BLANK_COUNTER.fetch_add(1, Ordering::SeqCst),
            label: Some(label),
        }
    }

    /// Get the internal ID
    pub fn id(&self) -> u64 {
        self.id
    }

    /// Get the label if present
    pub fn label(&self) -> Option<&str> {
        self.label.as_deref()
    }

    /// Get a unique identifier for this blank node
    /// If a run prefix is set (mode 'u'), includes the prefix
    pub fn unique_id(&self) -> String {
        if let Some(prefix) = get_run_prefix() {
            format!("{}_b{}", prefix, self.id)
        } else {
            format!("b{}", self.id)
        }
    }
}

impl fmt::Debug for BlankNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(label) = &self.label {
            write!(f, "_:{}", label)
        } else {
            write!(f, "_:b{}", self.id)
        }
    }
}

impl fmt::Display for BlankNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(label) = &self.label {
            write!(f, "_:{}", label)
        } else {
            write!(f, "_:b{}", self.id)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fresh_blank_nodes_are_unique() {
        let b1 = BlankNode::fresh();
        let b2 = BlankNode::fresh();
        assert_ne!(b1.id(), b2.id());
        assert_ne!(b1, b2);
    }

    #[test]
    fn test_labeled_blank_node() {
        let b = BlankNode::labeled("x".into());
        assert_eq!(b.label(), Some("x"));
        assert_eq!(format!("{}", b), "_:x");
    }
}
