//! Reactive Reasoning for N3
//!
//! Event-driven rule evaluation with reactive streams and real-time inference.
//!
//! # Features
//!
//! - Event-driven rule evaluation
//! - Reactive streams for triple changes
//! - Real-time continuous inference
//! - Subscription-based query monitoring
//! - Push-based notification system
//!
//! # Example
//!
//! ```ignore
//! use cwm::reasoning::reactive::{ReactiveReasoner, Event, Subscription};
//!
//! let mut reasoner = ReactiveReasoner::new();
//!
//! // Subscribe to pattern changes
//! reasoner.subscribe(pattern, |event| {
//!     println!("Pattern matched: {:?}", event);
//! });
//!
//! // Events trigger reactive inference
//! reasoner.emit(Event::Add(triple));
//! ```

use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::{Arc, Mutex};
use crate::term::{Term, Triple, Bindings, substitute_triple};
use crate::store::Store;
use crate::reasoner::{Rule, Reasoner, ReasonerConfig};

/// An event in the reactive system
#[derive(Clone, Debug)]
pub enum Event {
    /// A triple was added
    Add(Triple),
    /// A triple was removed
    Remove(Triple),
    /// Multiple triples were added
    AddBatch(Vec<Triple>),
    /// Multiple triples were removed
    RemoveBatch(Vec<Triple>),
    /// A rule was added
    RuleAdd(Rule),
    /// Inference was triggered
    InferenceTrigger,
    /// A subscription pattern was matched
    PatternMatch {
        subscription_id: SubscriptionId,
        bindings: Bindings,
        triple: Triple,
    },
}

/// Unique identifier for subscriptions
pub type SubscriptionId = u64;

/// A subscription to pattern matches
#[derive(Clone, Debug)]
pub struct Subscription {
    /// Unique identifier
    pub id: SubscriptionId,
    /// Pattern to match
    pub pattern: Triple,
    /// Whether to match new triples only
    pub new_only: bool,
    /// Priority (higher = earlier notification)
    pub priority: i32,
    /// Whether subscription is active
    pub active: bool,
}

impl Subscription {
    /// Create a new subscription
    pub fn new(id: SubscriptionId, pattern: Triple) -> Self {
        Subscription {
            id,
            pattern,
            new_only: true,
            priority: 0,
            active: true,
        }
    }

    /// Match against all triples (not just new ones)
    pub fn match_all(mut self) -> Self {
        self.new_only = false;
        self
    }

    /// Set priority
    pub fn with_priority(mut self, priority: i32) -> Self {
        self.priority = priority;
        self
    }
}

/// Handler for subscription events
pub type EventHandler = Box<dyn Fn(Event) + Send + Sync>;

/// Configuration for reactive reasoning
#[derive(Clone, Debug)]
pub struct ReactiveConfig {
    /// Maximum events in queue before processing
    pub max_queue_size: usize,
    /// Whether to batch process events
    pub batch_mode: bool,
    /// Batch size for processing
    pub batch_size: usize,
    /// Enable automatic inference after events
    pub auto_infer: bool,
    /// Maximum inference depth per event
    pub max_inference_depth: usize,
    /// Enable event history
    pub enable_history: bool,
    /// Maximum history size
    pub max_history_size: usize,
}

impl Default for ReactiveConfig {
    fn default() -> Self {
        ReactiveConfig {
            max_queue_size: 10000,
            batch_mode: true,
            batch_size: 100,
            auto_infer: true,
            max_inference_depth: 100,
            enable_history: true,
            max_history_size: 1000,
        }
    }
}

/// Statistics for reactive reasoning
#[derive(Clone, Debug, Default)]
pub struct ReactiveStats {
    /// Total events processed
    pub events_processed: usize,
    /// Total subscriptions triggered
    pub subscriptions_triggered: usize,
    /// Total inferences triggered
    pub inferences_triggered: usize,
    /// Current queue size
    pub queue_size: usize,
    /// Active subscriptions
    pub active_subscriptions: usize,
    /// Average processing time (ms)
    pub avg_processing_time_ms: f64,
}

/// Thread-safe event queue
#[derive(Clone)]
pub struct EventQueue {
    queue: Arc<Mutex<VecDeque<Event>>>,
    max_size: usize,
}

impl EventQueue {
    /// Create a new event queue
    pub fn new(max_size: usize) -> Self {
        EventQueue {
            queue: Arc::new(Mutex::new(VecDeque::new())),
            max_size,
        }
    }

    /// Push an event to the queue
    pub fn push(&self, event: Event) -> bool {
        let mut queue = self.queue.lock().unwrap();
        if queue.len() >= self.max_size {
            return false;
        }
        queue.push_back(event);
        true
    }

    /// Pop an event from the queue
    pub fn pop(&self) -> Option<Event> {
        let mut queue = self.queue.lock().unwrap();
        queue.pop_front()
    }

    /// Pop multiple events
    pub fn pop_batch(&self, count: usize) -> Vec<Event> {
        let mut queue = self.queue.lock().unwrap();
        let mut batch = Vec::with_capacity(count.min(queue.len()));
        for _ in 0..count {
            if let Some(event) = queue.pop_front() {
                batch.push(event);
            } else {
                break;
            }
        }
        batch
    }

    /// Get queue size
    pub fn len(&self) -> usize {
        self.queue.lock().unwrap().len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.queue.lock().unwrap().is_empty()
    }

    /// Clear the queue
    pub fn clear(&self) {
        self.queue.lock().unwrap().clear();
    }
}

impl std::fmt::Debug for EventQueue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EventQueue")
            .field("len", &self.len())
            .field("max_size", &self.max_size)
            .finish()
    }
}

/// Reactive reasoner with event-driven inference
pub struct ReactiveReasoner {
    /// Underlying store
    store: Store,
    /// Rules for inference
    rules: Vec<Rule>,
    /// Subscriptions
    subscriptions: HashMap<SubscriptionId, Subscription>,
    /// Event handlers
    handlers: HashMap<SubscriptionId, EventHandler>,
    /// Event queue
    queue: EventQueue,
    /// Configuration
    config: ReactiveConfig,
    /// Statistics
    stats: ReactiveStats,
    /// Next subscription ID
    next_id: SubscriptionId,
    /// Event history
    history: VecDeque<Event>,
    /// Derived triple signatures (for duplicate detection)
    derived: HashSet<String>,
}

impl ReactiveReasoner {
    /// Create a new reactive reasoner
    pub fn new() -> Self {
        let config = ReactiveConfig::default();
        ReactiveReasoner {
            store: Store::new(),
            rules: Vec::new(),
            subscriptions: HashMap::new(),
            handlers: HashMap::new(),
            queue: EventQueue::new(config.max_queue_size),
            config,
            stats: ReactiveStats::default(),
            next_id: 1,
            history: VecDeque::new(),
            derived: HashSet::new(),
        }
    }

    /// Create with configuration
    pub fn with_config(config: ReactiveConfig) -> Self {
        let max_queue = config.max_queue_size;
        ReactiveReasoner {
            store: Store::new(),
            rules: Vec::new(),
            subscriptions: HashMap::new(),
            handlers: HashMap::new(),
            queue: EventQueue::new(max_queue),
            config,
            stats: ReactiveStats::default(),
            next_id: 1,
            history: VecDeque::new(),
            derived: HashSet::new(),
        }
    }

    /// Create with existing store
    pub fn with_store(store: Store) -> Self {
        let mut reasoner = Self::new();
        // Initialize derived set
        for triple in store.iter() {
            reasoner.derived.insert(format!("{:?}", triple));
        }
        reasoner.store = store;
        reasoner
    }

    /// Get a reference to the store
    pub fn store(&self) -> &Store {
        &self.store
    }

    /// Get a mutable reference to the store
    pub fn store_mut(&mut self) -> &mut Store {
        &mut self.store
    }

    /// Add a rule
    pub fn add_rule(&mut self, rule: Rule) {
        self.rules.push(rule.clone());
        self.emit(Event::RuleAdd(rule));
    }

    /// Subscribe to pattern matches
    pub fn subscribe<F>(&mut self, pattern: Triple, handler: F) -> SubscriptionId
    where
        F: Fn(Event) + Send + Sync + 'static,
    {
        let id = self.next_id;
        self.next_id += 1;

        let sub = Subscription::new(id, pattern);
        self.subscriptions.insert(id, sub);
        self.handlers.insert(id, Box::new(handler));
        self.stats.active_subscriptions += 1;

        id
    }

    /// Subscribe with options
    pub fn subscribe_with_options<F>(
        &mut self,
        subscription: Subscription,
        handler: F,
    ) -> SubscriptionId
    where
        F: Fn(Event) + Send + Sync + 'static,
    {
        let id = subscription.id;
        self.subscriptions.insert(id, subscription);
        self.handlers.insert(id, Box::new(handler));
        self.stats.active_subscriptions += 1;
        id
    }

    /// Unsubscribe
    pub fn unsubscribe(&mut self, id: SubscriptionId) -> bool {
        if self.subscriptions.remove(&id).is_some() {
            self.handlers.remove(&id);
            self.stats.active_subscriptions -= 1;
            true
        } else {
            false
        }
    }

    /// Pause a subscription
    pub fn pause(&mut self, id: SubscriptionId) -> bool {
        if let Some(sub) = self.subscriptions.get_mut(&id) {
            sub.active = false;
            true
        } else {
            false
        }
    }

    /// Resume a subscription
    pub fn resume(&mut self, id: SubscriptionId) -> bool {
        if let Some(sub) = self.subscriptions.get_mut(&id) {
            sub.active = true;
            true
        } else {
            false
        }
    }

    /// Emit an event
    pub fn emit(&mut self, event: Event) {
        // Add to history
        if self.config.enable_history {
            self.history.push_back(event.clone());
            while self.history.len() > self.config.max_history_size {
                self.history.pop_front();
            }
        }

        // Add to queue
        self.queue.push(event);
        self.stats.queue_size = self.queue.len();

        // Process if needed
        if self.config.batch_mode {
            if self.queue.len() >= self.config.batch_size {
                self.process_batch();
            }
        } else {
            self.process_one();
        }
    }

    /// Add a triple (convenience method)
    pub fn add(&mut self, triple: Triple) {
        self.emit(Event::Add(triple));
    }

    /// Remove a triple (convenience method)
    pub fn remove(&mut self, triple: Triple) {
        self.emit(Event::Remove(triple));
    }

    /// Process a single event
    fn process_one(&mut self) {
        if let Some(event) = self.queue.pop() {
            self.process_event(event);
        }
        self.stats.queue_size = self.queue.len();
    }

    /// Process a batch of events
    fn process_batch(&mut self) {
        let batch = self.queue.pop_batch(self.config.batch_size);
        for event in batch {
            self.process_event(event);
        }
        self.stats.queue_size = self.queue.len();
    }

    /// Process all pending events
    pub fn flush(&mut self) {
        while !self.queue.is_empty() {
            self.process_batch();
        }
    }

    /// Process a single event
    fn process_event(&mut self, event: Event) {
        self.stats.events_processed += 1;

        match &event {
            Event::Add(triple) => {
                let sig = format!("{:?}", triple);
                if !self.derived.contains(&sig) {
                    self.derived.insert(sig);
                    self.store.add(triple.clone());
                    self.check_subscriptions(triple);

                    if self.config.auto_infer {
                        self.run_incremental_inference(triple);
                    }
                }
            }
            Event::Remove(triple) => {
                let sig = format!("{:?}", triple);
                self.derived.remove(&sig);
                self.store.remove(triple);
                // Note: Could trigger re-evaluation of dependent triples
            }
            Event::AddBatch(triples) => {
                for triple in triples {
                    let sig = format!("{:?}", triple);
                    if !self.derived.contains(&sig) {
                        self.derived.insert(sig);
                        self.store.add(triple.clone());
                        self.check_subscriptions(triple);
                    }
                }

                if self.config.auto_infer {
                    self.run_full_inference();
                }
            }
            Event::RemoveBatch(triples) => {
                for triple in triples {
                    let sig = format!("{:?}", triple);
                    self.derived.remove(&sig);
                    self.store.remove(triple);
                }
            }
            Event::RuleAdd(_) => {
                // Re-run inference with new rule
                if self.config.auto_infer {
                    self.run_full_inference();
                }
            }
            Event::InferenceTrigger => {
                self.run_full_inference();
            }
            Event::PatternMatch { .. } => {
                // Already handled by subscription system
            }
        }
    }

    /// Check subscriptions for a new triple
    fn check_subscriptions(&mut self, triple: &Triple) {
        // Collect matching subscriptions (avoid borrow issues)
        let mut matches: Vec<(SubscriptionId, Bindings)> = Vec::new();

        for (id, sub) in &self.subscriptions {
            if !sub.active {
                continue;
            }

            if let Some(bindings) = self.match_pattern(&sub.pattern, triple) {
                matches.push((*id, bindings));
            }
        }

        // Notify handlers
        for (id, bindings) in matches {
            self.stats.subscriptions_triggered += 1;

            let event = Event::PatternMatch {
                subscription_id: id,
                bindings,
                triple: triple.clone(),
            };

            if let Some(handler) = self.handlers.get(&id) {
                handler(event);
            }
        }
    }

    /// Match a pattern against a triple
    fn match_pattern(&self, pattern: &Triple, triple: &Triple) -> Option<Bindings> {
        let mut bindings = Bindings::default();

        if !self.match_term(&pattern.subject, &triple.subject, &mut bindings) {
            return None;
        }
        if !self.match_term(&pattern.predicate, &triple.predicate, &mut bindings) {
            return None;
        }
        if !self.match_term(&pattern.object, &triple.object, &mut bindings) {
            return None;
        }

        Some(bindings)
    }

    /// Match a pattern term against a ground term
    fn match_term(&self, pattern: &Term, ground: &Term, bindings: &mut Bindings) -> bool {
        match pattern {
            Term::Variable(var) => {
                if let Some(existing) = bindings.get(var) {
                    existing == ground
                } else {
                    bindings.insert(var.clone(), ground.clone());
                    true
                }
            }
            _ => pattern == ground,
        }
    }

    /// Run incremental inference triggered by a new triple
    fn run_incremental_inference(&mut self, trigger: &Triple) {
        self.stats.inferences_triggered += 1;

        let mut new_triples = Vec::new();
        let mut depth = 0;

        // Initial derivations from the trigger triple
        let initial = self.derive_from_trigger(trigger);
        new_triples.extend(initial);

        // Iterative deepening
        while !new_triples.is_empty() && depth < self.config.max_inference_depth {
            let mut next_triples = Vec::new();

            for triple in &new_triples {
                let sig = format!("{:?}", triple);
                if !self.derived.contains(&sig) {
                    self.derived.insert(sig);
                    self.store.add(triple.clone());
                    self.check_subscriptions(triple);

                    // Derive more from this triple
                    let derived = self.derive_from_trigger(triple);
                    next_triples.extend(derived);
                }
            }

            new_triples = next_triples;
            depth += 1;
        }
    }

    /// Derive triples triggered by a specific triple
    fn derive_from_trigger(&self, trigger: &Triple) -> Vec<Triple> {
        let mut derived = Vec::new();

        for rule in &self.rules {
            // Check if trigger matches any antecedent pattern
            for (ant_idx, ant_pattern) in rule.antecedent.iter().enumerate() {
                if let Some(initial_bindings) = self.match_pattern(ant_pattern, trigger) {
                    // Try to match remaining antecedent patterns
                    let other_patterns: Vec<&Triple> = rule.antecedent.iter()
                        .enumerate()
                        .filter(|(i, _)| *i != ant_idx)
                        .map(|(_, p)| p)
                        .collect();

                    let matches = self.match_remaining(&other_patterns, initial_bindings);

                    for bindings in matches {
                        // Generate consequent triples
                        for consequent in &rule.consequent {
                            let triple = substitute_triple(consequent, &bindings);
                            if triple.is_ground() {
                                derived.push(triple);
                            }
                        }
                    }
                }
            }
        }

        derived
    }

    /// Match remaining antecedent patterns
    fn match_remaining(&self, patterns: &[&Triple], bindings: Bindings) -> Vec<Bindings> {
        if patterns.is_empty() {
            return vec![bindings];
        }

        let pattern = substitute_triple(patterns[0], &bindings);
        let rest: Vec<&Triple> = patterns[1..].to_vec();

        let matches = self.store.match_pattern(&pattern);
        let mut results = Vec::new();

        for new_bindings in matches {
            let mut merged = bindings.clone();
            for (var, term) in new_bindings {
                merged.insert(var, term);
            }
            results.extend(self.match_remaining(&rest, merged));
        }

        results
    }

    /// Run full inference pass
    fn run_full_inference(&mut self) {
        self.stats.inferences_triggered += 1;

        let config = ReasonerConfig {
            max_steps: self.config.max_inference_depth,
            recursive: true,
            filter: false,
            generate_proof: false,
            enable_tabling: true,
            enable_crypto: false,
        };

        let mut reasoner = Reasoner::with_config(config);
        for rule in &self.rules {
            reasoner.add_rule(rule.clone());
        }

        reasoner.run(&mut self.store);

        // Update derived set
        for triple in self.store.iter() {
            self.derived.insert(format!("{:?}", triple));
        }
    }

    /// Get statistics
    pub fn stats(&self) -> &ReactiveStats {
        &self.stats
    }

    /// Get event history
    pub fn history(&self) -> &VecDeque<Event> {
        &self.history
    }

    /// Clear history
    pub fn clear_history(&mut self) {
        self.history.clear();
    }
}

impl Default for ReactiveReasoner {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for ReactiveReasoner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReactiveReasoner")
            .field("store_size", &self.store.len())
            .field("rules", &self.rules.len())
            .field("subscriptions", &self.subscriptions.len())
            .field("queue", &self.queue)
            .field("stats", &self.stats)
            .finish()
    }
}

/// Reactive query that monitors for results
#[derive(Clone, Debug)]
pub struct ReactiveQuery {
    /// Query patterns
    pub patterns: Vec<Triple>,
    /// Current results
    pub results: Vec<Bindings>,
    /// Whether query is active
    pub active: bool,
}

impl ReactiveQuery {
    /// Create a new reactive query
    pub fn new(patterns: Vec<Triple>) -> Self {
        ReactiveQuery {
            patterns,
            results: Vec::new(),
            active: true,
        }
    }

    /// Execute query against store
    pub fn execute(&mut self, store: &Store) {
        self.results = store.query(&self.patterns);
    }

    /// Check if a triple affects this query
    pub fn is_affected_by(&self, triple: &Triple) -> bool {
        for pattern in &self.patterns {
            // Check if the triple could match this pattern
            if could_match(pattern, triple) {
                return true;
            }
        }
        false
    }
}

/// Check if a triple could potentially match a pattern
fn could_match(pattern: &Triple, triple: &Triple) -> bool {
    matches_term(&pattern.subject, &triple.subject)
        && matches_term(&pattern.predicate, &triple.predicate)
        && matches_term(&pattern.object, &triple.object)
}

/// Check if a pattern term matches a ground term
fn matches_term(pattern: &Term, ground: &Term) -> bool {
    match pattern {
        Term::Variable(_) => true,
        _ => pattern == ground,
    }
}

/// Stream of events for async processing
pub struct EventStream {
    queue: EventQueue,
}

impl EventStream {
    /// Create a new event stream
    pub fn new(max_size: usize) -> Self {
        EventStream {
            queue: EventQueue::new(max_size),
        }
    }

    /// Push an event to the stream
    pub fn push(&self, event: Event) -> bool {
        self.queue.push(event)
    }

    /// Pop the next event
    pub fn next(&self) -> Option<Event> {
        self.queue.pop()
    }

    /// Check if stream has events
    pub fn has_events(&self) -> bool {
        !self.queue.is_empty()
    }

    /// Get number of pending events
    pub fn pending(&self) -> usize {
        self.queue.len()
    }
}

impl std::fmt::Debug for EventStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EventStream")
            .field("pending", &self.pending())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[test]
    fn test_reactive_basic() {
        let mut reasoner = ReactiveReasoner::new();

        let triple = Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        );

        reasoner.add(triple.clone());
        reasoner.flush();

        assert!(reasoner.store().contains(&triple));
    }

    #[test]
    fn test_subscription() {
        let mut reasoner = ReactiveReasoner::new();

        let count = Arc::new(AtomicUsize::new(0));
        let count_clone = count.clone();

        // Subscribe to any triple with predicate "knows"
        let pattern = Triple::new(
            Term::universal("x"),
            Term::uri("http://example.org/knows"),
            Term::universal("y"),
        );

        reasoner.subscribe(pattern, move |_event| {
            count_clone.fetch_add(1, Ordering::SeqCst);
        });

        // Add matching triple
        reasoner.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/bob"),
        ));
        reasoner.flush();

        assert_eq!(count.load(Ordering::SeqCst), 1);

        // Add non-matching triple
        reasoner.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/likes"),
            Term::uri("http://example.org/b"),
        ));
        reasoner.flush();

        // Count should still be 1
        assert_eq!(count.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_reactive_inference() {
        let mut reasoner = ReactiveReasoner::new();

        // Rule: Human(?x) => Mortal(?x)
        let rule = Rule::new(
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Human"),
            )],
            vec![Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/type"),
                Term::uri("http://example.org/Mortal"),
            )],
        );

        reasoner.add_rule(rule);

        // Add a Human
        reasoner.add(Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Human"),
        ));
        reasoner.flush();

        // Should infer Mortal
        let mortal = Triple::new(
            Term::uri("http://example.org/socrates"),
            Term::uri("http://example.org/type"),
            Term::uri("http://example.org/Mortal"),
        );

        assert!(reasoner.store().contains(&mortal));
    }

    #[test]
    fn test_event_queue() {
        let queue = EventQueue::new(10);

        let triple = Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        );

        queue.push(Event::Add(triple.clone()));
        queue.push(Event::Add(triple.clone()));

        assert_eq!(queue.len(), 2);

        let event = queue.pop().unwrap();
        assert!(matches!(event, Event::Add(_)));

        assert_eq!(queue.len(), 1);

        queue.clear();
        assert!(queue.is_empty());
    }

    #[test]
    fn test_batch_processing() {
        let mut config = ReactiveConfig::default();
        config.batch_mode = true;
        config.batch_size = 2;
        config.auto_infer = false;

        let mut reasoner = ReactiveReasoner::with_config(config);

        for i in 0..5 {
            reasoner.add(Triple::new(
                Term::uri(format!("http://example.org/s{}", i)),
                Term::uri("http://example.org/p"),
                Term::literal("o"),
            ));
        }

        // Batch processing should have triggered
        // Flush remaining
        reasoner.flush();

        assert_eq!(reasoner.store().len(), 5);
    }

    #[test]
    fn test_reactive_query() {
        let mut store = Store::new();
        store.add(Triple::new(
            Term::uri("http://example.org/alice"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/bob"),
        ));

        let mut query = ReactiveQuery::new(vec![
            Triple::new(
                Term::universal("x"),
                Term::uri("http://example.org/knows"),
                Term::universal("y"),
            ),
        ]);

        query.execute(&store);
        assert_eq!(query.results.len(), 1);

        // Check if affected by new triple
        let new_triple = Triple::new(
            Term::uri("http://example.org/bob"),
            Term::uri("http://example.org/knows"),
            Term::uri("http://example.org/charlie"),
        );

        assert!(query.is_affected_by(&new_triple));
    }

    #[test]
    fn test_pause_resume() {
        let mut reasoner = ReactiveReasoner::new();

        let count = Arc::new(AtomicUsize::new(0));
        let count_clone = count.clone();

        let pattern = Triple::new(
            Term::universal("x"),
            Term::uri("http://example.org/p"),
            Term::universal("y"),
        );

        let id = reasoner.subscribe(pattern, move |_| {
            count_clone.fetch_add(1, Ordering::SeqCst);
        });

        // Add triple
        reasoner.add(Triple::new(
            Term::uri("http://example.org/a"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/b"),
        ));
        reasoner.flush();

        assert_eq!(count.load(Ordering::SeqCst), 1);

        // Pause subscription
        reasoner.pause(id);

        // Add another triple
        reasoner.add(Triple::new(
            Term::uri("http://example.org/c"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/d"),
        ));
        reasoner.flush();

        // Count should still be 1 (paused)
        assert_eq!(count.load(Ordering::SeqCst), 1);

        // Resume
        reasoner.resume(id);

        // Add yet another triple
        reasoner.add(Triple::new(
            Term::uri("http://example.org/e"),
            Term::uri("http://example.org/p"),
            Term::uri("http://example.org/f"),
        ));
        reasoner.flush();

        // Count should be 2 now
        assert_eq!(count.load(Ordering::SeqCst), 2);
    }
}
