//! Ontology Versioning for N3
//!
//! Version tracking, change detection, and migration support for ontologies.
//!
//! # Features
//!
//! - Semantic versioning for ontologies
//! - Change detection and diff computation
//! - Migration path generation
//! - Backwards compatibility checking
//! - Version history management
//!
//! # Example
//!
//! ```ignore
//! use cwm::reasoning::versioning::{OntologyVersion, VersionedStore, ChangeSet};
//!
//! let mut versioned = VersionedStore::new("http://example.org/ontology");
//!
//! // Make changes and commit
//! versioned.add(triple);
//! versioned.commit("1.1.0", "Added new property");
//!
//! // Get diff between versions
//! let diff = versioned.diff("1.0.0", "1.1.0")?;
//! ```

use std::collections::{HashMap, HashSet, BTreeMap};
use crate::term::{Term, Triple};
use crate::store::Store;

/// Semantic version (major.minor.patch)
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SemanticVersion {
    /// Major version (breaking changes)
    pub major: u32,
    /// Minor version (backwards-compatible additions)
    pub minor: u32,
    /// Patch version (backwards-compatible fixes)
    pub patch: u32,
    /// Pre-release identifier (optional)
    pub prerelease: Option<String>,
}

impl SemanticVersion {
    /// Create a new semantic version
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        SemanticVersion {
            major,
            minor,
            patch,
            prerelease: None,
        }
    }

    /// Create with prerelease tag
    pub fn with_prerelease(major: u32, minor: u32, patch: u32, prerelease: impl Into<String>) -> Self {
        SemanticVersion {
            major,
            minor,
            patch,
            prerelease: Some(prerelease.into()),
        }
    }

    /// Parse from string (e.g., "1.2.3" or "1.2.3-alpha")
    pub fn parse(s: &str) -> Option<Self> {
        let (version_part, prerelease) = if let Some(idx) = s.find('-') {
            (&s[..idx], Some(s[idx + 1..].to_string()))
        } else {
            (s, None)
        };

        let parts: Vec<&str> = version_part.split('.').collect();
        if parts.len() != 3 {
            return None;
        }

        Some(SemanticVersion {
            major: parts[0].parse().ok()?,
            minor: parts[1].parse().ok()?,
            patch: parts[2].parse().ok()?,
            prerelease,
        })
    }

    /// Bump major version
    pub fn bump_major(&self) -> Self {
        SemanticVersion::new(self.major + 1, 0, 0)
    }

    /// Bump minor version
    pub fn bump_minor(&self) -> Self {
        SemanticVersion::new(self.major, self.minor + 1, 0)
    }

    /// Bump patch version
    pub fn bump_patch(&self) -> Self {
        SemanticVersion::new(self.major, self.minor, self.patch + 1)
    }

    /// Check if this version is compatible with another
    pub fn is_compatible_with(&self, other: &SemanticVersion) -> bool {
        // Major version 0 is special (initial development)
        if self.major == 0 || other.major == 0 {
            return self.major == other.major && self.minor == other.minor;
        }
        // Otherwise, major versions must match
        self.major == other.major
    }
}

impl std::fmt::Display for SemanticVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref pre) = self.prerelease {
            write!(f, "{}.{}.{}-{}", self.major, self.minor, self.patch, pre)
        } else {
            write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
        }
    }
}

impl Default for SemanticVersion {
    fn default() -> Self {
        SemanticVersion::new(0, 1, 0)
    }
}

/// A single change to an ontology
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Change {
    /// Triple added
    Add(Triple),
    /// Triple removed
    Remove(Triple),
    /// Triple modified (old, new)
    Modify {
        old: Triple,
        new: Triple,
    },
    /// Class added
    AddClass(String),
    /// Class removed
    RemoveClass(String),
    /// Property added
    AddProperty(String),
    /// Property removed
    RemoveProperty(String),
    /// Domain/range changed
    DomainRangeChange {
        property: String,
        old_domain: Option<String>,
        new_domain: Option<String>,
        old_range: Option<String>,
        new_range: Option<String>,
    },
    /// Subclass relationship added
    AddSubclass {
        subclass: String,
        superclass: String,
    },
    /// Subclass relationship removed
    RemoveSubclass {
        subclass: String,
        superclass: String,
    },
}

impl Change {
    /// Check if this change is breaking (requires major version bump)
    pub fn is_breaking(&self) -> bool {
        match self {
            Change::Remove(_) => true,
            Change::RemoveClass(_) => true,
            Change::RemoveProperty(_) => true,
            Change::DomainRangeChange { .. } => true,
            Change::RemoveSubclass { .. } => true,
            _ => false,
        }
    }

    /// Check if this change is an addition (minor version bump)
    pub fn is_addition(&self) -> bool {
        match self {
            Change::Add(_) => true,
            Change::AddClass(_) => true,
            Change::AddProperty(_) => true,
            Change::AddSubclass { .. } => true,
            _ => false,
        }
    }

    /// Get the category of this change
    pub fn category(&self) -> ChangeCategory {
        match self {
            Change::Add(_) | Change::Remove(_) | Change::Modify { .. } => ChangeCategory::Triple,
            Change::AddClass(_) | Change::RemoveClass(_) => ChangeCategory::Class,
            Change::AddProperty(_) | Change::RemoveProperty(_) | Change::DomainRangeChange { .. } => {
                ChangeCategory::Property
            }
            Change::AddSubclass { .. } | Change::RemoveSubclass { .. } => ChangeCategory::Hierarchy,
        }
    }
}

/// Category of changes
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ChangeCategory {
    /// Triple-level changes
    Triple,
    /// Class definition changes
    Class,
    /// Property definition changes
    Property,
    /// Hierarchy changes
    Hierarchy,
}

/// A set of changes representing a diff between versions
#[derive(Clone, Debug, Default)]
pub struct ChangeSet {
    /// List of changes
    pub changes: Vec<Change>,
    /// Summary statistics
    pub stats: ChangeStats,
}

impl ChangeSet {
    /// Create a new empty changeset
    pub fn new() -> Self {
        ChangeSet::default()
    }

    /// Add a change
    pub fn add(&mut self, change: Change) {
        self.update_stats(&change);
        self.changes.push(change);
    }

    /// Update statistics
    fn update_stats(&mut self, change: &Change) {
        match change {
            Change::Add(_) => self.stats.additions += 1,
            Change::Remove(_) => self.stats.removals += 1,
            Change::Modify { .. } => self.stats.modifications += 1,
            Change::AddClass(_) => {
                self.stats.additions += 1;
                self.stats.classes_added += 1;
            }
            Change::RemoveClass(_) => {
                self.stats.removals += 1;
                self.stats.classes_removed += 1;
            }
            Change::AddProperty(_) => {
                self.stats.additions += 1;
                self.stats.properties_added += 1;
            }
            Change::RemoveProperty(_) => {
                self.stats.removals += 1;
                self.stats.properties_removed += 1;
            }
            Change::DomainRangeChange { .. } => self.stats.modifications += 1,
            Change::AddSubclass { .. } => self.stats.additions += 1,
            Change::RemoveSubclass { .. } => self.stats.removals += 1,
        }

        if change.is_breaking() {
            self.stats.breaking_changes += 1;
        }
    }

    /// Check if there are breaking changes
    pub fn has_breaking_changes(&self) -> bool {
        self.stats.breaking_changes > 0
    }

    /// Get suggested version bump
    pub fn suggested_bump(&self, current: &SemanticVersion) -> SemanticVersion {
        if self.stats.breaking_changes > 0 {
            current.bump_major()
        } else if self.stats.additions > 0 {
            current.bump_minor()
        } else if self.stats.modifications > 0 || self.stats.removals > 0 {
            current.bump_patch()
        } else {
            current.clone()
        }
    }

    /// Filter changes by category
    pub fn filter_by_category(&self, category: ChangeCategory) -> Vec<&Change> {
        self.changes.iter()
            .filter(|c| c.category() == category)
            .collect()
    }

    /// Get all breaking changes
    pub fn breaking_changes(&self) -> Vec<&Change> {
        self.changes.iter()
            .filter(|c| c.is_breaking())
            .collect()
    }
}

/// Statistics about changes
#[derive(Clone, Debug, Default)]
pub struct ChangeStats {
    /// Number of additions
    pub additions: usize,
    /// Number of removals
    pub removals: usize,
    /// Number of modifications
    pub modifications: usize,
    /// Number of breaking changes
    pub breaking_changes: usize,
    /// Classes added
    pub classes_added: usize,
    /// Classes removed
    pub classes_removed: usize,
    /// Properties added
    pub properties_added: usize,
    /// Properties removed
    pub properties_removed: usize,
}

/// A version snapshot
#[derive(Clone, Debug)]
pub struct VersionSnapshot {
    /// Version number
    pub version: SemanticVersion,
    /// Store state at this version
    pub store: Store,
    /// Commit message
    pub message: String,
    /// Timestamp (Unix epoch seconds)
    pub timestamp: u64,
    /// Author (optional)
    pub author: Option<String>,
    /// Changes from previous version
    pub changes: ChangeSet,
}

/// Version history for an ontology
#[derive(Clone, Debug)]
pub struct VersionHistory {
    /// Versions ordered by semantic version
    versions: BTreeMap<SemanticVersion, VersionSnapshot>,
    /// Current working version
    current: SemanticVersion,
}

impl VersionHistory {
    /// Create new version history starting at 0.1.0
    pub fn new() -> Self {
        let initial = SemanticVersion::default();
        VersionHistory {
            versions: BTreeMap::new(),
            current: initial,
        }
    }

    /// Add a version snapshot
    pub fn add(&mut self, snapshot: VersionSnapshot) {
        self.current = snapshot.version.clone();
        self.versions.insert(snapshot.version.clone(), snapshot);
    }

    /// Get a specific version
    pub fn get(&self, version: &SemanticVersion) -> Option<&VersionSnapshot> {
        self.versions.get(version)
    }

    /// Get current version
    pub fn current(&self) -> &SemanticVersion {
        &self.current
    }

    /// Get latest snapshot
    pub fn latest(&self) -> Option<&VersionSnapshot> {
        self.versions.values().last()
    }

    /// List all versions
    pub fn list(&self) -> Vec<&SemanticVersion> {
        self.versions.keys().collect()
    }

    /// Get version count
    pub fn len(&self) -> usize {
        self.versions.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.versions.is_empty()
    }
}

impl Default for VersionHistory {
    fn default() -> Self {
        Self::new()
    }
}

/// Versioned ontology store
#[derive(Clone, Debug)]
pub struct VersionedStore {
    /// Ontology URI
    pub uri: String,
    /// Current working store
    working: Store,
    /// Version history
    history: VersionHistory,
    /// Uncommitted changes
    pending: ChangeSet,
}

impl VersionedStore {
    /// Create a new versioned store
    pub fn new(uri: impl Into<String>) -> Self {
        VersionedStore {
            uri: uri.into(),
            working: Store::new(),
            history: VersionHistory::new(),
            pending: ChangeSet::new(),
        }
    }

    /// Create from existing store
    pub fn from_store(uri: impl Into<String>, store: Store) -> Self {
        let mut versioned = VersionedStore::new(uri);
        versioned.working = store;
        versioned
    }

    /// Get working store
    pub fn store(&self) -> &Store {
        &self.working
    }

    /// Get mutable working store
    pub fn store_mut(&mut self) -> &mut Store {
        &mut self.working
    }

    /// Add a triple
    pub fn add(&mut self, triple: Triple) {
        if !self.working.contains(&triple) {
            self.pending.add(Change::Add(triple.clone()));
            self.working.add(triple);
        }
    }

    /// Remove a triple
    pub fn remove(&mut self, triple: &Triple) {
        if self.working.remove(triple) {
            self.pending.add(Change::Remove(triple.clone()));
        }
    }

    /// Check for uncommitted changes
    pub fn has_pending_changes(&self) -> bool {
        !self.pending.changes.is_empty()
    }

    /// Get pending changes
    pub fn pending_changes(&self) -> &ChangeSet {
        &self.pending
    }

    /// Commit current changes
    pub fn commit(&mut self, version: &str, message: &str) -> Result<SemanticVersion, VersionError> {
        let version = SemanticVersion::parse(version)
            .ok_or(VersionError::InvalidVersion(version.to_string()))?;

        // Check version is greater than current
        if !self.history.is_empty() && version <= *self.history.current() {
            return Err(VersionError::VersionExists(version.to_string()));
        }

        let snapshot = VersionSnapshot {
            version: version.clone(),
            store: self.working.clone(),
            message: message.to_string(),
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            author: None,
            changes: std::mem::take(&mut self.pending),
        };

        self.history.add(snapshot);
        Ok(version)
    }

    /// Commit with automatic version bump
    pub fn commit_auto(&mut self, message: &str) -> SemanticVersion {
        let version = self.pending.suggested_bump(self.history.current());

        let snapshot = VersionSnapshot {
            version: version.clone(),
            store: self.working.clone(),
            message: message.to_string(),
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            author: None,
            changes: std::mem::take(&mut self.pending),
        };

        self.history.add(snapshot);
        version
    }

    /// Discard pending changes
    pub fn discard(&mut self) {
        // Revert to last committed state
        if let Some(latest) = self.history.latest() {
            self.working = latest.store.clone();
        } else {
            self.working = Store::new();
        }
        self.pending = ChangeSet::new();
    }

    /// Checkout a specific version
    pub fn checkout(&mut self, version: &str) -> Result<(), VersionError> {
        let version = SemanticVersion::parse(version)
            .ok_or(VersionError::InvalidVersion(version.to_string()))?;

        let snapshot = self.history.get(&version)
            .ok_or(VersionError::VersionNotFound(version.to_string()))?;

        self.working = snapshot.store.clone();
        self.pending = ChangeSet::new();
        Ok(())
    }

    /// Get diff between two versions
    pub fn diff(&self, from: &str, to: &str) -> Result<ChangeSet, VersionError> {
        let from_version = SemanticVersion::parse(from)
            .ok_or(VersionError::InvalidVersion(from.to_string()))?;
        let to_version = SemanticVersion::parse(to)
            .ok_or(VersionError::InvalidVersion(to.to_string()))?;

        let from_snapshot = self.history.get(&from_version)
            .ok_or(VersionError::VersionNotFound(from.to_string()))?;
        let to_snapshot = self.history.get(&to_version)
            .ok_or(VersionError::VersionNotFound(to.to_string()))?;

        Ok(compute_diff(&from_snapshot.store, &to_snapshot.store))
    }

    /// Get current version
    pub fn current_version(&self) -> &SemanticVersion {
        self.history.current()
    }

    /// Get version history
    pub fn history(&self) -> &VersionHistory {
        &self.history
    }

    /// List all versions
    pub fn versions(&self) -> Vec<&SemanticVersion> {
        self.history.list()
    }
}

/// Compute diff between two stores
pub fn compute_diff(from: &Store, to: &Store) -> ChangeSet {
    let mut changeset = ChangeSet::new();

    // Find additions
    for triple in to.iter() {
        if !from.contains(triple) {
            changeset.add(Change::Add(triple.clone()));
        }
    }

    // Find removals
    for triple in from.iter() {
        if !to.contains(triple) {
            changeset.add(Change::Remove(triple.clone()));
        }
    }

    // Analyze for higher-level changes
    analyze_class_changes(from, to, &mut changeset);
    analyze_property_changes(from, to, &mut changeset);

    changeset
}

/// Analyze class-level changes
fn analyze_class_changes(from: &Store, to: &Store, changeset: &mut ChangeSet) {
    let rdf_type = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
    let owl_class = "http://www.w3.org/2002/07/owl#Class";
    let rdfs_class = "http://www.w3.org/2000/01/rdf-schema#Class";

    let from_classes = find_classes(from, rdf_type, owl_class, rdfs_class);
    let to_classes = find_classes(to, rdf_type, owl_class, rdfs_class);

    for class in &to_classes {
        if !from_classes.contains(class) {
            changeset.add(Change::AddClass(class.clone()));
        }
    }

    for class in &from_classes {
        if !to_classes.contains(class) {
            changeset.add(Change::RemoveClass(class.clone()));
        }
    }
}

/// Find all classes in a store
fn find_classes(store: &Store, rdf_type: &str, owl_class: &str, rdfs_class: &str) -> HashSet<String> {
    let mut classes = HashSet::new();

    for triple in store.iter() {
        if let Term::Uri(pred) = &triple.predicate {
            if pred.as_str() == rdf_type {
                if let Term::Uri(obj) = &triple.object {
                    if obj.as_str() == owl_class || obj.as_str() == rdfs_class {
                        if let Term::Uri(subj) = &triple.subject {
                            classes.insert(subj.as_str().to_string());
                        }
                    }
                }
            }
        }
    }

    classes
}

/// Analyze property-level changes
fn analyze_property_changes(from: &Store, to: &Store, changeset: &mut ChangeSet) {
    let rdf_type = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
    let owl_property_types = [
        "http://www.w3.org/2002/07/owl#ObjectProperty",
        "http://www.w3.org/2002/07/owl#DatatypeProperty",
        "http://www.w3.org/2002/07/owl#AnnotationProperty",
    ];
    let rdf_property = "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property";

    let from_props = find_properties(from, rdf_type, &owl_property_types, rdf_property);
    let to_props = find_properties(to, rdf_type, &owl_property_types, rdf_property);

    for prop in &to_props {
        if !from_props.contains(prop) {
            changeset.add(Change::AddProperty(prop.clone()));
        }
    }

    for prop in &from_props {
        if !to_props.contains(prop) {
            changeset.add(Change::RemoveProperty(prop.clone()));
        }
    }
}

/// Find all properties in a store
fn find_properties(store: &Store, rdf_type: &str, owl_types: &[&str], rdf_property: &str) -> HashSet<String> {
    let mut properties = HashSet::new();

    for triple in store.iter() {
        if let Term::Uri(pred) = &triple.predicate {
            if pred.as_str() == rdf_type {
                if let Term::Uri(obj) = &triple.object {
                    let obj_str = obj.as_str();
                    if owl_types.contains(&obj_str) || obj_str == rdf_property {
                        if let Term::Uri(subj) = &triple.subject {
                            properties.insert(subj.as_str().to_string());
                        }
                    }
                }
            }
        }
    }

    properties
}

/// Version-related errors
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VersionError {
    /// Invalid version string
    InvalidVersion(String),
    /// Version not found
    VersionNotFound(String),
    /// Version already exists
    VersionExists(String),
    /// Migration failed
    MigrationFailed(String),
}

impl std::fmt::Display for VersionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VersionError::InvalidVersion(v) => write!(f, "Invalid version: {}", v),
            VersionError::VersionNotFound(v) => write!(f, "Version not found: {}", v),
            VersionError::VersionExists(v) => write!(f, "Version already exists: {}", v),
            VersionError::MigrationFailed(msg) => write!(f, "Migration failed: {}", msg),
        }
    }
}

impl std::error::Error for VersionError {}

/// Migration between ontology versions
#[derive(Clone, Debug)]
pub struct Migration {
    /// Source version
    pub from: SemanticVersion,
    /// Target version
    pub to: SemanticVersion,
    /// Migration steps
    pub steps: Vec<MigrationStep>,
}

impl Migration {
    /// Create a new migration
    pub fn new(from: SemanticVersion, to: SemanticVersion) -> Self {
        Migration {
            from,
            to,
            steps: Vec::new(),
        }
    }

    /// Add a migration step
    pub fn add_step(&mut self, step: MigrationStep) {
        self.steps.push(step);
    }

    /// Apply migration to a store
    pub fn apply(&self, store: &mut Store) -> Result<(), VersionError> {
        for step in &self.steps {
            step.apply(store)?;
        }
        Ok(())
    }

    /// Generate migration from changeset
    pub fn from_changeset(from: SemanticVersion, to: SemanticVersion, changes: &ChangeSet) -> Self {
        let mut migration = Migration::new(from, to);

        for change in &changes.changes {
            match change {
                Change::Add(triple) => {
                    migration.add_step(MigrationStep::Add(triple.clone()));
                }
                Change::Remove(triple) => {
                    migration.add_step(MigrationStep::Remove(triple.clone()));
                }
                Change::Modify { old, new } => {
                    migration.add_step(MigrationStep::Replace(old.clone(), new.clone()));
                }
                _ => {
                    // Higher-level changes are handled through their constituent triples
                }
            }
        }

        migration
    }
}

/// A single migration step
#[derive(Clone, Debug)]
pub enum MigrationStep {
    /// Add a triple
    Add(Triple),
    /// Remove a triple
    Remove(Triple),
    /// Replace a triple
    Replace(Triple, Triple),
    /// Rename a URI
    RenameUri {
        old: String,
        new: String,
    },
    /// Custom transformation
    Custom(String),
}

impl MigrationStep {
    /// Apply this step to a store
    pub fn apply(&self, store: &mut Store) -> Result<(), VersionError> {
        match self {
            MigrationStep::Add(triple) => {
                store.add(triple.clone());
                Ok(())
            }
            MigrationStep::Remove(triple) => {
                store.remove(triple);
                Ok(())
            }
            MigrationStep::Replace(old, new) => {
                store.remove(old);
                store.add(new.clone());
                Ok(())
            }
            MigrationStep::RenameUri { old, new } => {
                // Collect triples to modify
                let to_modify: Vec<Triple> = store.iter()
                    .filter(|t| {
                        contains_uri(&t.subject, old)
                            || contains_uri(&t.predicate, old)
                            || contains_uri(&t.object, old)
                    })
                    .cloned()
                    .collect();

                for triple in to_modify {
                    store.remove(&triple);
                    let new_triple = Triple::new(
                        rename_uri(&triple.subject, old, new),
                        rename_uri(&triple.predicate, old, new),
                        rename_uri(&triple.object, old, new),
                    );
                    store.add(new_triple);
                }

                Ok(())
            }
            MigrationStep::Custom(desc) => {
                Err(VersionError::MigrationFailed(format!(
                    "Custom migration step '{}' requires manual implementation",
                    desc
                )))
            }
        }
    }
}

/// Check if a term contains a specific URI
fn contains_uri(term: &Term, uri: &str) -> bool {
    match term {
        Term::Uri(u) => u.as_str() == uri,
        _ => false,
    }
}

/// Rename URI in a term
fn rename_uri(term: &Term, old: &str, new: &str) -> Term {
    match term {
        Term::Uri(u) if u.as_str() == old => Term::uri(new),
        _ => term.clone(),
    }
}

/// Compatibility check result
#[derive(Clone, Debug)]
pub struct CompatibilityReport {
    /// Whether versions are compatible
    pub compatible: bool,
    /// List of breaking changes
    pub breaking_changes: Vec<Change>,
    /// Deprecation warnings
    pub deprecations: Vec<String>,
    /// Suggested migrations
    pub suggestions: Vec<String>,
}

impl CompatibilityReport {
    /// Create a compatibility report from a changeset
    pub fn from_changeset(changes: &ChangeSet) -> Self {
        let breaking: Vec<Change> = changes.changes.iter()
            .filter(|c| c.is_breaking())
            .cloned()
            .collect();

        let mut suggestions = Vec::new();

        for change in &breaking {
            match change {
                Change::RemoveClass(class) => {
                    suggestions.push(format!(
                        "Consider deprecating class '{}' before removal",
                        class
                    ));
                }
                Change::RemoveProperty(prop) => {
                    suggestions.push(format!(
                        "Consider deprecating property '{}' before removal",
                        prop
                    ));
                }
                Change::DomainRangeChange { property, .. } => {
                    suggestions.push(format!(
                        "Domain/range change on '{}' may break existing data",
                        property
                    ));
                }
                _ => {}
            }
        }

        CompatibilityReport {
            compatible: breaking.is_empty(),
            breaking_changes: breaking,
            deprecations: Vec::new(),
            suggestions,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_semantic_version_parse() {
        let v = SemanticVersion::parse("1.2.3").unwrap();
        assert_eq!(v.major, 1);
        assert_eq!(v.minor, 2);
        assert_eq!(v.patch, 3);

        let v_pre = SemanticVersion::parse("1.0.0-alpha").unwrap();
        assert_eq!(v_pre.prerelease, Some("alpha".to_string()));
    }

    #[test]
    fn test_semantic_version_bump() {
        let v = SemanticVersion::new(1, 2, 3);

        assert_eq!(v.bump_major(), SemanticVersion::new(2, 0, 0));
        assert_eq!(v.bump_minor(), SemanticVersion::new(1, 3, 0));
        assert_eq!(v.bump_patch(), SemanticVersion::new(1, 2, 4));
    }

    #[test]
    fn test_semantic_version_compatibility() {
        let v1 = SemanticVersion::new(1, 2, 0);
        let v2 = SemanticVersion::new(1, 5, 0);
        let v3 = SemanticVersion::new(2, 0, 0);

        assert!(v1.is_compatible_with(&v2));
        assert!(!v1.is_compatible_with(&v3));
    }

    #[test]
    fn test_versioned_store() {
        let mut store = VersionedStore::new("http://example.org/ontology");

        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        ));

        assert!(store.has_pending_changes());
        assert_eq!(store.pending_changes().changes.len(), 1);

        store.commit("1.0.0", "Initial version").unwrap();

        assert!(!store.has_pending_changes());
        assert_eq!(store.current_version().to_string(), "1.0.0");
    }

    #[test]
    fn test_versioned_store_diff() {
        let mut store = VersionedStore::new("http://example.org/ontology");

        // First version
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        ));
        store.commit("1.0.0", "Version 1").unwrap();

        // Second version - add a triple
        store.add(Triple::new(
            Term::uri("http://example.org/c"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/d"),
        ));
        store.commit("1.1.0", "Version 1.1").unwrap();

        // Get diff
        let diff = store.diff("1.0.0", "1.1.0").unwrap();
        assert_eq!(diff.stats.additions, 1);
        assert_eq!(diff.stats.removals, 0);
    }

    #[test]
    fn test_auto_version_bump() {
        let mut store = VersionedStore::new("http://example.org/ontology");

        // Addition should bump minor
        store.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        ));
        let v = store.commit_auto("Added triple");
        assert_eq!(v.to_string(), "0.2.0");

        // Removal should bump major (but we're at 0.x)
        store.remove(&Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        ));
        let v = store.commit_auto("Removed triple");
        // At 0.x, major bump goes to 1.0.0
        assert_eq!(v.to_string(), "1.0.0");
    }

    #[test]
    fn test_checkout() {
        let mut store = VersionedStore::new("http://example.org/ontology");

        let triple1 = Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        );

        store.add(triple1.clone());
        store.commit("1.0.0", "Version 1").unwrap();

        let triple2 = Triple::new(
            Term::uri("http://example.org/c"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/d"),
        );

        store.add(triple2.clone());
        store.commit("2.0.0", "Version 2").unwrap();

        assert_eq!(store.store().len(), 2);

        store.checkout("1.0.0").unwrap();
        assert_eq!(store.store().len(), 1);
        assert!(store.store().contains(&triple1));
        assert!(!store.store().contains(&triple2));
    }

    #[test]
    fn test_migration() {
        let from = SemanticVersion::new(1, 0, 0);
        let to = SemanticVersion::new(2, 0, 0);

        let mut migration = Migration::new(from, to);

        migration.add_step(MigrationStep::RenameUri {
            old: "http://example.org/oldName".to_string(),
            new: "http://example.org/newName".to_string(),
        });

        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/oldName"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        ));

        migration.apply(&mut store).unwrap();

        assert!(store.contains(&Triple::new(
            Term::uri("http://example.org/newName"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        )));
    }

    #[test]
    fn test_compatibility_report() {
        let mut changeset = ChangeSet::new();

        changeset.add(Change::Add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        )));

        let report = CompatibilityReport::from_changeset(&changeset);
        assert!(report.compatible);

        // Add breaking change
        changeset.add(Change::RemoveClass("http://example.org/OldClass".to_string()));

        let report = CompatibilityReport::from_changeset(&changeset);
        assert!(!report.compatible);
        assert!(!report.suggestions.is_empty());
    }
}
