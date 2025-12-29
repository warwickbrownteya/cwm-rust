//! Temporal reasoning for time-based inference
//!
//! This module provides temporal reasoning capabilities including:
//! - Time intervals and points
//! - Allen's interval algebra relations
//! - Temporal predicates and constraints
//! - Time-stamped assertions
//!
//! # Temporal Relations (Allen's Interval Algebra)
//!
//! - `before`: X ends before Y starts
//! - `after`: X starts after Y ends
//! - `meets`: X ends exactly when Y starts
//! - `metBy`: X starts exactly when Y ends
//! - `overlaps`: X starts before Y and ends during Y
//! - `overlappedBy`: X starts during Y and ends after Y
//! - `during`: X is entirely within Y
//! - `contains`: X entirely contains Y
//! - `starts`: X starts with Y but ends earlier
//! - `startedBy`: X starts with Y but ends later
//! - `finishes`: X finishes with Y but starts later
//! - `finishedBy`: X finishes with Y but starts earlier
//! - `equals`: X and Y have same start and end
//!
//! # Example
//!
//! ```ignore
//! use cwm::{TemporalReasoner, TimeInterval, TemporalStore};
//!
//! let mut reasoner = TemporalReasoner::new();
//!
//! let meeting = TimeInterval::new(
//!     Instant::parse("2024-01-15T09:00:00Z"),
//!     Instant::parse("2024-01-15T10:00:00Z"),
//! );
//!
//! let lunch = TimeInterval::new(
//!     Instant::parse("2024-01-15T12:00:00Z"),
//!     Instant::parse("2024-01-15T13:00:00Z"),
//! );
//!
//! assert!(reasoner.before(&meeting, &lunch));
//! ```

use std::collections::{HashMap, HashSet};
use std::cmp::Ordering;
use crate::term::{Term, Triple};
use crate::store::Store;

/// Temporal namespace URIs
pub mod temporal_ns {
    pub const TIME: &str = "http://www.w3.org/2006/time#";
    pub const INSTANT: &str = "http://www.w3.org/2006/time#Instant";
    pub const INTERVAL: &str = "http://www.w3.org/2006/time#Interval";
    pub const PROPER_INTERVAL: &str = "http://www.w3.org/2006/time#ProperInterval";
    pub const IN_XSD_DATE_TIME: &str = "http://www.w3.org/2006/time#inXSDDateTimeStamp";
    pub const HAS_BEGINNING: &str = "http://www.w3.org/2006/time#hasBeginning";
    pub const HAS_END: &str = "http://www.w3.org/2006/time#hasEnd";

    // Allen's interval relations
    pub const BEFORE: &str = "http://www.w3.org/2006/time#before";
    pub const AFTER: &str = "http://www.w3.org/2006/time#after";
    pub const MEETS: &str = "http://www.w3.org/2006/time#meets";
    pub const MET_BY: &str = "http://www.w3.org/2006/time#metBy";
    pub const OVERLAPS: &str = "http://www.w3.org/2006/time#overlaps";
    pub const OVERLAPPED_BY: &str = "http://www.w3.org/2006/time#overlappedBy";
    pub const STARTS: &str = "http://www.w3.org/2006/time#starts";
    pub const STARTED_BY: &str = "http://www.w3.org/2006/time#startedBy";
    pub const DURING: &str = "http://www.w3.org/2006/time#during";
    pub const CONTAINS: &str = "http://www.w3.org/2006/time#contains";
    pub const FINISHES: &str = "http://www.w3.org/2006/time#finishes";
    pub const FINISHED_BY: &str = "http://www.w3.org/2006/time#finishedBy";
    pub const EQUALS: &str = "http://www.w3.org/2006/time#equals";
}

/// A point in time (instant)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Instant {
    /// Timestamp in milliseconds since Unix epoch
    pub timestamp_ms: i64,
}

impl Instant {
    /// Create a new instant from milliseconds since epoch
    pub fn from_millis(ms: i64) -> Self {
        Instant { timestamp_ms: ms }
    }

    /// Create a new instant from seconds since epoch
    pub fn from_secs(secs: i64) -> Self {
        Instant { timestamp_ms: secs * 1000 }
    }

    /// Parse from ISO 8601 datetime string
    pub fn parse(s: &str) -> Option<Self> {
        // Simple ISO 8601 parser for common formats
        // Format: YYYY-MM-DDTHH:MM:SSZ or YYYY-MM-DDTHH:MM:SS+00:00
        let s = s.trim();

        // Try to parse datetime
        let parts: Vec<&str> = s.split('T').collect();
        if parts.len() != 2 {
            return None;
        }

        let date_parts: Vec<i64> = parts[0].split('-')
            .filter_map(|p| p.parse().ok())
            .collect();

        if date_parts.len() != 3 {
            return None;
        }

        // Remove timezone suffix for time parsing
        let time_str = parts[1]
            .trim_end_matches('Z')
            .split('+').next()
            .unwrap_or(parts[1])
            .split('-').next()
            .unwrap_or(parts[1]);

        let time_parts: Vec<i64> = time_str.split(':')
            .filter_map(|p| p.parse().ok())
            .collect();

        if time_parts.len() < 2 {
            return None;
        }

        let year = date_parts[0];
        let month = date_parts[1];
        let day = date_parts[2];
        let hour = time_parts[0];
        let minute = time_parts[1];
        let second = time_parts.get(2).copied().unwrap_or(0);

        // Convert to timestamp (simplified, ignoring leap seconds, etc.)
        let days_since_epoch = days_from_civil(year, month, day);
        let seconds = days_since_epoch * 86400 + hour * 3600 + minute * 60 + second;

        Some(Instant::from_secs(seconds))
    }

    /// Format as ISO 8601 datetime string
    pub fn to_iso_string(&self) -> String {
        let total_secs = self.timestamp_ms / 1000;
        let days = total_secs / 86400;
        let rem_secs = total_secs % 86400;

        let (year, month, day) = civil_from_days(days);
        let hour = rem_secs / 3600;
        let minute = (rem_secs % 3600) / 60;
        let second = rem_secs % 60;

        format!("{:04}-{:02}-{:02}T{:02}:{:02}:{:02}Z",
                year, month, day, hour, minute, second)
    }

    /// Get milliseconds since epoch
    pub fn millis(&self) -> i64 {
        self.timestamp_ms
    }

    /// Check if this instant is before another
    pub fn is_before(&self, other: &Instant) -> bool {
        self.timestamp_ms < other.timestamp_ms
    }

    /// Check if this instant is after another
    pub fn is_after(&self, other: &Instant) -> bool {
        self.timestamp_ms > other.timestamp_ms
    }

    /// Check if this instant equals another
    pub fn is_equal(&self, other: &Instant) -> bool {
        self.timestamp_ms == other.timestamp_ms
    }
}

/// A time interval with start and end points
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TimeInterval {
    /// Start instant
    pub start: Instant,
    /// End instant
    pub end: Instant,
}

impl TimeInterval {
    /// Create a new interval
    pub fn new(start: Instant, end: Instant) -> Self {
        TimeInterval { start, end }
    }

    /// Create an interval from two timestamps in milliseconds
    pub fn from_millis(start_ms: i64, end_ms: i64) -> Self {
        TimeInterval {
            start: Instant::from_millis(start_ms),
            end: Instant::from_millis(end_ms),
        }
    }

    /// Check if this is a proper interval (start < end)
    pub fn is_proper(&self) -> bool {
        self.start.is_before(&self.end)
    }

    /// Get duration in milliseconds
    pub fn duration_ms(&self) -> i64 {
        self.end.timestamp_ms - self.start.timestamp_ms
    }

    /// Check if an instant is within this interval
    pub fn contains_instant(&self, instant: &Instant) -> bool {
        !instant.is_before(&self.start) && !instant.is_after(&self.end)
    }
}

/// Allen's interval relations
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IntervalRelation {
    Before,
    After,
    Meets,
    MetBy,
    Overlaps,
    OverlappedBy,
    Starts,
    StartedBy,
    During,
    Contains,
    Finishes,
    FinishedBy,
    Equals,
}

impl IntervalRelation {
    /// Get the inverse relation
    pub fn inverse(&self) -> Self {
        match self {
            IntervalRelation::Before => IntervalRelation::After,
            IntervalRelation::After => IntervalRelation::Before,
            IntervalRelation::Meets => IntervalRelation::MetBy,
            IntervalRelation::MetBy => IntervalRelation::Meets,
            IntervalRelation::Overlaps => IntervalRelation::OverlappedBy,
            IntervalRelation::OverlappedBy => IntervalRelation::Overlaps,
            IntervalRelation::Starts => IntervalRelation::StartedBy,
            IntervalRelation::StartedBy => IntervalRelation::Starts,
            IntervalRelation::During => IntervalRelation::Contains,
            IntervalRelation::Contains => IntervalRelation::During,
            IntervalRelation::Finishes => IntervalRelation::FinishedBy,
            IntervalRelation::FinishedBy => IntervalRelation::Finishes,
            IntervalRelation::Equals => IntervalRelation::Equals,
        }
    }

    /// Get the URI for this relation
    pub fn uri(&self) -> &'static str {
        match self {
            IntervalRelation::Before => temporal_ns::BEFORE,
            IntervalRelation::After => temporal_ns::AFTER,
            IntervalRelation::Meets => temporal_ns::MEETS,
            IntervalRelation::MetBy => temporal_ns::MET_BY,
            IntervalRelation::Overlaps => temporal_ns::OVERLAPS,
            IntervalRelation::OverlappedBy => temporal_ns::OVERLAPPED_BY,
            IntervalRelation::Starts => temporal_ns::STARTS,
            IntervalRelation::StartedBy => temporal_ns::STARTED_BY,
            IntervalRelation::During => temporal_ns::DURING,
            IntervalRelation::Contains => temporal_ns::CONTAINS,
            IntervalRelation::Finishes => temporal_ns::FINISHES,
            IntervalRelation::FinishedBy => temporal_ns::FINISHED_BY,
            IntervalRelation::Equals => temporal_ns::EQUALS,
        }
    }

    /// Parse from URI
    pub fn from_uri(uri: &str) -> Option<Self> {
        match uri {
            temporal_ns::BEFORE => Some(IntervalRelation::Before),
            temporal_ns::AFTER => Some(IntervalRelation::After),
            temporal_ns::MEETS => Some(IntervalRelation::Meets),
            temporal_ns::MET_BY => Some(IntervalRelation::MetBy),
            temporal_ns::OVERLAPS => Some(IntervalRelation::Overlaps),
            temporal_ns::OVERLAPPED_BY => Some(IntervalRelation::OverlappedBy),
            temporal_ns::STARTS => Some(IntervalRelation::Starts),
            temporal_ns::STARTED_BY => Some(IntervalRelation::StartedBy),
            temporal_ns::DURING => Some(IntervalRelation::During),
            temporal_ns::CONTAINS => Some(IntervalRelation::Contains),
            temporal_ns::FINISHES => Some(IntervalRelation::Finishes),
            temporal_ns::FINISHED_BY => Some(IntervalRelation::FinishedBy),
            temporal_ns::EQUALS => Some(IntervalRelation::Equals),
            _ => None,
        }
    }

    /// All 13 relations
    pub fn all() -> &'static [IntervalRelation] {
        &[
            IntervalRelation::Before,
            IntervalRelation::After,
            IntervalRelation::Meets,
            IntervalRelation::MetBy,
            IntervalRelation::Overlaps,
            IntervalRelation::OverlappedBy,
            IntervalRelation::Starts,
            IntervalRelation::StartedBy,
            IntervalRelation::During,
            IntervalRelation::Contains,
            IntervalRelation::Finishes,
            IntervalRelation::FinishedBy,
            IntervalRelation::Equals,
        ]
    }
}

/// Compute the interval relation between two intervals
pub fn compute_relation(a: &TimeInterval, b: &TimeInterval) -> IntervalRelation {
    let a_start = a.start.timestamp_ms;
    let a_end = a.end.timestamp_ms;
    let b_start = b.start.timestamp_ms;
    let b_end = b.end.timestamp_ms;

    match (a_start.cmp(&b_start), a_end.cmp(&b_end), a_end.cmp(&b_start), b_end.cmp(&a_start)) {
        // Before: a ends before b starts
        (_, _, Ordering::Less, _) => IntervalRelation::Before,
        // After: b ends before a starts
        (_, _, _, Ordering::Less) => IntervalRelation::After,
        // Meets: a ends exactly when b starts
        (_, _, Ordering::Equal, _) => IntervalRelation::Meets,
        // MetBy: b ends exactly when a starts
        (_, _, _, Ordering::Equal) => IntervalRelation::MetBy,
        // Equals: same start and end
        (Ordering::Equal, Ordering::Equal, _, _) => IntervalRelation::Equals,
        // Starts: a starts with b but ends earlier
        (Ordering::Equal, Ordering::Less, _, _) => IntervalRelation::Starts,
        // StartedBy: a starts with b but ends later
        (Ordering::Equal, Ordering::Greater, _, _) => IntervalRelation::StartedBy,
        // Finishes: a finishes with b but starts later
        (Ordering::Greater, Ordering::Equal, _, _) => IntervalRelation::Finishes,
        // FinishedBy: a finishes with b but starts earlier
        (Ordering::Less, Ordering::Equal, _, _) => IntervalRelation::FinishedBy,
        // During: a is entirely within b
        (Ordering::Greater, Ordering::Less, _, _) => IntervalRelation::During,
        // Contains: a entirely contains b
        (Ordering::Less, Ordering::Greater, _, _) => IntervalRelation::Contains,
        // Overlaps: a starts before b, ends during b, but before b ends
        (Ordering::Less, Ordering::Less, Ordering::Greater, _) if a_end < b_end => IntervalRelation::Overlaps,
        // OverlappedBy: b starts before a, b ends during a
        (Ordering::Greater, Ordering::Greater, _, Ordering::Greater) if b_end < a_end => IntervalRelation::OverlappedBy,
        // Fallback to Overlaps for remaining cases
        _ => IntervalRelation::Overlaps,
    }
}

/// A time-stamped triple (quad with temporal dimension)
#[derive(Clone, Debug)]
pub struct TemporalTriple {
    /// The base triple
    pub triple: Triple,
    /// Valid time interval (when the fact is true)
    pub valid_time: Option<TimeInterval>,
    /// Transaction time (when the fact was recorded)
    pub transaction_time: Option<Instant>,
}

impl TemporalTriple {
    /// Create a new temporal triple
    pub fn new(triple: Triple) -> Self {
        TemporalTriple {
            triple,
            valid_time: None,
            transaction_time: None,
        }
    }

    /// Set valid time
    pub fn with_valid_time(mut self, interval: TimeInterval) -> Self {
        self.valid_time = Some(interval);
        self
    }

    /// Set transaction time
    pub fn with_transaction_time(mut self, instant: Instant) -> Self {
        self.transaction_time = Some(instant);
        self
    }

    /// Check if the triple is valid at a given instant
    pub fn is_valid_at(&self, instant: &Instant) -> bool {
        match &self.valid_time {
            Some(interval) => interval.contains_instant(instant),
            None => true, // Atemporal triples are always valid
        }
    }
}

/// Temporal store for time-stamped triples
#[derive(Clone, Debug, Default)]
pub struct TemporalStore {
    /// Base store for current state
    base: Store,
    /// Temporal triples with time annotations
    temporal_triples: Vec<TemporalTriple>,
    /// Index by valid time start
    by_valid_start: HashMap<i64, Vec<usize>>,
    /// Intervals by subject URI
    intervals: HashMap<String, TimeInterval>,
}

impl TemporalStore {
    /// Create a new temporal store
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a temporal triple
    pub fn add_temporal(&mut self, temporal: TemporalTriple) {
        let idx = self.temporal_triples.len();

        // Index by valid time if present
        if let Some(ref interval) = temporal.valid_time {
            self.by_valid_start
                .entry(interval.start.timestamp_ms)
                .or_default()
                .push(idx);
        }

        // Also add to base store if currently valid
        self.base.add(temporal.triple.clone());
        self.temporal_triples.push(temporal);
    }

    /// Add an atemporal triple
    pub fn add(&mut self, triple: Triple) {
        self.add_temporal(TemporalTriple::new(triple));
    }

    /// Register an interval for a subject
    pub fn register_interval(&mut self, subject: &str, interval: TimeInterval) {
        self.intervals.insert(subject.to_string(), interval);
    }

    /// Get interval for a subject
    pub fn get_interval(&self, subject: &str) -> Option<&TimeInterval> {
        self.intervals.get(subject)
    }

    /// Query triples valid at a specific time
    pub fn query_at(&self, instant: &Instant) -> Vec<&Triple> {
        self.temporal_triples
            .iter()
            .filter(|t| t.is_valid_at(instant))
            .map(|t| &t.triple)
            .collect()
    }

    /// Query triples valid during an interval
    pub fn query_during(&self, interval: &TimeInterval) -> Vec<&Triple> {
        self.temporal_triples
            .iter()
            .filter(|t| {
                if let Some(ref valid_time) = t.valid_time {
                    // Check if intervals overlap
                    !(valid_time.end.is_before(&interval.start) ||
                      valid_time.start.is_after(&interval.end))
                } else {
                    true
                }
            })
            .map(|t| &t.triple)
            .collect()
    }

    /// Get the base store
    pub fn base(&self) -> &Store {
        &self.base
    }

    /// Get mutable base store
    pub fn base_mut(&mut self) -> &mut Store {
        &mut self.base
    }
}

/// Temporal reasoner for time-based inference
pub struct TemporalReasoner {
    /// Inference tolerance in milliseconds (for "meets" relation)
    tolerance_ms: i64,
}

impl TemporalReasoner {
    /// Create a new temporal reasoner
    pub fn new() -> Self {
        TemporalReasoner {
            tolerance_ms: 0,
        }
    }

    /// Create with tolerance for "meets" relation
    pub fn with_tolerance(tolerance_ms: i64) -> Self {
        TemporalReasoner { tolerance_ms }
    }

    /// Compute the relation between two intervals
    pub fn relation(&self, a: &TimeInterval, b: &TimeInterval) -> IntervalRelation {
        compute_relation(a, b)
    }

    /// Check if interval a is before interval b
    pub fn before(&self, a: &TimeInterval, b: &TimeInterval) -> bool {
        a.end.timestamp_ms < b.start.timestamp_ms
    }

    /// Check if interval a is after interval b
    pub fn after(&self, a: &TimeInterval, b: &TimeInterval) -> bool {
        a.start.timestamp_ms > b.end.timestamp_ms
    }

    /// Check if interval a meets interval b (with tolerance)
    pub fn meets(&self, a: &TimeInterval, b: &TimeInterval) -> bool {
        let diff = (a.end.timestamp_ms - b.start.timestamp_ms).abs();
        diff <= self.tolerance_ms
    }

    /// Check if interval a overlaps interval b
    pub fn overlaps(&self, a: &TimeInterval, b: &TimeInterval) -> bool {
        a.start.timestamp_ms < b.start.timestamp_ms &&
        a.end.timestamp_ms > b.start.timestamp_ms &&
        a.end.timestamp_ms < b.end.timestamp_ms
    }

    /// Check if interval a contains interval b
    pub fn contains(&self, a: &TimeInterval, b: &TimeInterval) -> bool {
        a.start.timestamp_ms <= b.start.timestamp_ms &&
        a.end.timestamp_ms >= b.end.timestamp_ms &&
        !(a.start.timestamp_ms == b.start.timestamp_ms && a.end.timestamp_ms == b.end.timestamp_ms)
    }

    /// Check if interval a is during interval b
    pub fn during(&self, a: &TimeInterval, b: &TimeInterval) -> bool {
        self.contains(b, a)
    }

    /// Check if intervals are equal
    pub fn equals(&self, a: &TimeInterval, b: &TimeInterval) -> bool {
        a.start.timestamp_ms == b.start.timestamp_ms &&
        a.end.timestamp_ms == b.end.timestamp_ms
    }

    /// Derive temporal relations for intervals in a store
    pub fn derive_relations(&self, store: &TemporalStore) -> Vec<Triple> {
        let mut derived = Vec::new();

        let intervals: Vec<(&str, &TimeInterval)> = store.intervals
            .iter()
            .map(|(k, v)| (k.as_str(), v))
            .collect();

        for i in 0..intervals.len() {
            for j in (i + 1)..intervals.len() {
                let (subj_a, int_a) = intervals[i];
                let (subj_b, int_b) = intervals[j];

                let relation = self.relation(int_a, int_b);

                // Add the relation triple
                derived.push(Triple::new(
                    Term::uri(subj_a),
                    Term::uri(relation.uri()),
                    Term::uri(subj_b),
                ));

                // Add inverse relation
                derived.push(Triple::new(
                    Term::uri(subj_b),
                    Term::uri(relation.inverse().uri()),
                    Term::uri(subj_a),
                ));
            }
        }

        derived
    }

    /// Extract interval information from RDF triples
    pub fn extract_intervals(&self, store: &Store) -> HashMap<String, TimeInterval> {
        let mut intervals = HashMap::new();
        let mut beginnings: HashMap<String, Instant> = HashMap::new();
        let mut endings: HashMap<String, Instant> = HashMap::new();

        for triple in store.iter() {
            if let Term::Uri(pred) = &triple.predicate {
                match pred.as_str() {
                    temporal_ns::HAS_BEGINNING => {
                        if let (Term::Uri(subj), Term::Uri(obj)) = (&triple.subject, &triple.object) {
                            // Look up the instant's datetime
                            if let Some(instant) = self.find_instant_value(store, obj.as_str()) {
                                beginnings.insert(subj.as_str().to_string(), instant);
                            }
                        }
                    }
                    temporal_ns::HAS_END => {
                        if let (Term::Uri(subj), Term::Uri(obj)) = (&triple.subject, &triple.object) {
                            if let Some(instant) = self.find_instant_value(store, obj.as_str()) {
                                endings.insert(subj.as_str().to_string(), instant);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        // Combine beginnings and endings into intervals
        for (subj, start) in beginnings {
            if let Some(end) = endings.get(&subj) {
                intervals.insert(subj, TimeInterval::new(start, end.clone()));
            }
        }

        intervals
    }

    /// Find the datetime value for an instant
    fn find_instant_value(&self, store: &Store, instant_uri: &str) -> Option<Instant> {
        for triple in store.iter() {
            if let Term::Uri(subj) = &triple.subject {
                if subj.as_str() == instant_uri {
                    if let Term::Uri(pred) = &triple.predicate {
                        if pred.as_str() == temporal_ns::IN_XSD_DATE_TIME {
                            if let Term::Literal(lit) = &triple.object {
                                return Instant::parse(lit.value());
                            }
                        }
                    }
                }
            }
        }
        None
    }
}

impl Default for TemporalReasoner {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert year/month/day to days since Unix epoch
fn days_from_civil(year: i64, month: i64, day: i64) -> i64 {
    let y = if month <= 2 { year - 1 } else { year };
    let m = if month <= 2 { month + 12 } else { month };

    let era = if y >= 0 { y / 400 } else { (y - 399) / 400 };
    let yoe = y - era * 400;
    let doy = (153 * (m - 3) + 2) / 5 + day - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;

    era * 146097 + doe - 719468
}

/// Convert days since Unix epoch to year/month/day
fn civil_from_days(days: i64) -> (i64, i64, i64) {
    let z = days + 719468;
    let era = if z >= 0 { z / 146097 } else { (z - 146096) / 146097 };
    let doe = z - era * 146097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let year = if m <= 2 { y + 1 } else { y };

    (year, m, d)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instant_parsing() {
        let instant = Instant::parse("2024-01-15T10:30:00Z").unwrap();
        assert!(instant.timestamp_ms > 0);

        // Round-trip test
        let iso = instant.to_iso_string();
        let reparsed = Instant::parse(&iso).unwrap();
        assert_eq!(instant.timestamp_ms, reparsed.timestamp_ms);
    }

    #[test]
    fn test_instant_comparison() {
        let earlier = Instant::from_millis(1000);
        let later = Instant::from_millis(2000);

        assert!(earlier.is_before(&later));
        assert!(later.is_after(&earlier));
        assert!(!earlier.is_equal(&later));
    }

    #[test]
    fn test_interval_creation() {
        let start = Instant::from_millis(1000);
        let end = Instant::from_millis(2000);
        let interval = TimeInterval::new(start, end);

        assert!(interval.is_proper());
        assert_eq!(interval.duration_ms(), 1000);
    }

    #[test]
    fn test_interval_contains() {
        let interval = TimeInterval::from_millis(1000, 3000);
        let inside = Instant::from_millis(2000);
        let outside = Instant::from_millis(500);

        assert!(interval.contains_instant(&inside));
        assert!(!interval.contains_instant(&outside));
    }

    #[test]
    fn test_allen_relations() {
        let reasoner = TemporalReasoner::new();

        // Before relation
        let a = TimeInterval::from_millis(100, 200);
        let b = TimeInterval::from_millis(300, 400);
        assert!(reasoner.before(&a, &b));
        assert_eq!(compute_relation(&a, &b), IntervalRelation::Before);

        // Contains relation
        let c = TimeInterval::from_millis(100, 500);
        let d = TimeInterval::from_millis(200, 400);
        assert!(reasoner.contains(&c, &d));
        assert_eq!(compute_relation(&c, &d), IntervalRelation::Contains);

        // Equals relation
        let e = TimeInterval::from_millis(100, 200);
        let f = TimeInterval::from_millis(100, 200);
        assert!(reasoner.equals(&e, &f));
        assert_eq!(compute_relation(&e, &f), IntervalRelation::Equals);
    }

    #[test]
    fn test_interval_relation_inverse() {
        assert_eq!(IntervalRelation::Before.inverse(), IntervalRelation::After);
        assert_eq!(IntervalRelation::During.inverse(), IntervalRelation::Contains);
        assert_eq!(IntervalRelation::Equals.inverse(), IntervalRelation::Equals);
    }

    #[test]
    fn test_temporal_store() {
        let mut store = TemporalStore::new();

        let triple = Triple::new(
            Term::uri("http://example.org/event1"),
            Term::uri("http://example.org/name"),
            Term::literal("Meeting"),
        );

        let interval = TimeInterval::from_millis(1000, 2000);
        let temporal = TemporalTriple::new(triple)
            .with_valid_time(interval);

        store.add_temporal(temporal);

        // Query at valid time
        let during = Instant::from_millis(1500);
        let results = store.query_at(&during);
        assert_eq!(results.len(), 1);

        // Query outside valid time
        let outside = Instant::from_millis(3000);
        let results = store.query_at(&outside);
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_derive_relations() {
        let mut store = TemporalStore::new();

        store.register_interval(
            "http://example.org/meeting",
            TimeInterval::from_millis(1000, 2000),
        );
        store.register_interval(
            "http://example.org/lunch",
            TimeInterval::from_millis(3000, 4000),
        );

        let reasoner = TemporalReasoner::new();
        let derived = reasoner.derive_relations(&store);

        // Should have derived before/after relations
        assert!(!derived.is_empty());
    }

    #[test]
    fn test_meets_with_tolerance() {
        let reasoner = TemporalReasoner::with_tolerance(100);

        let a = TimeInterval::from_millis(1000, 2000);
        let b = TimeInterval::from_millis(2050, 3000);

        // Within tolerance
        assert!(reasoner.meets(&a, &b));

        // Without tolerance
        let strict = TemporalReasoner::new();
        assert!(!strict.meets(&a, &b));
    }
}
