//! Cluster Coordination Module
//!
//! Provides cluster management, node discovery, leader election, and
//! coordination primitives for distributed reasoning.
//!
//! # Features
//!
//! - Node discovery and registration
//! - Raft-based leader election
//! - Barrier synchronization
//! - Distributed locking
//! - Cluster membership management
//! - Health monitoring
//!
//! # Example
//!
//! ```ignore
//! use cwm::reasoning::cluster::{Cluster, ClusterConfig};
//!
//! let config = ClusterConfig {
//!     cluster_name: "cwm-cluster".to_string(),
//!     node_id: "node-1".to_string(),
//!     seed_nodes: vec!["127.0.0.1:9001".to_string()],
//!     ..Default::default()
//! };
//!
//! let mut cluster = Cluster::new(config)?;
//! cluster.join()?;
//!
//! // Wait for leader election
//! let leader = cluster.await_leader(Duration::from_secs(30))?;
//! ```

use std::collections::{HashMap, HashSet, VecDeque};
use std::net::SocketAddr;
use std::sync::{Arc, Mutex, RwLock};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use crate::term::{Term, Triple, Bindings};
use super::ipc::{Node, NodeConfig, IpcMessage, Transport, SerializationFormat};

/// Cluster configuration
#[derive(Clone, Debug)]
pub struct ClusterConfig {
    /// Cluster name
    pub cluster_name: String,
    /// This node's ID
    pub node_id: String,
    /// Bind address for this node
    pub bind_address: SocketAddr,
    /// Seed nodes for discovery
    pub seed_nodes: Vec<String>,
    /// Transport protocol
    pub transport: Transport,
    /// Election timeout range (min, max)
    pub election_timeout: (Duration, Duration),
    /// Heartbeat interval
    pub heartbeat_interval: Duration,
    /// Join timeout
    pub join_timeout: Duration,
    /// Maximum cluster size
    pub max_nodes: usize,
    /// Enable auto-discovery
    pub auto_discovery: bool,
    /// Discovery interval
    pub discovery_interval: Duration,
}

impl Default for ClusterConfig {
    fn default() -> Self {
        ClusterConfig {
            cluster_name: "cwm-cluster".to_string(),
            node_id: format!("node-{}", std::process::id()),
            bind_address: "0.0.0.0:0".parse().unwrap(),
            seed_nodes: Vec::new(),
            transport: Transport::Tcp,
            election_timeout: (Duration::from_millis(150), Duration::from_millis(300)),
            heartbeat_interval: Duration::from_millis(50),
            join_timeout: Duration::from_secs(30),
            max_nodes: 100,
            auto_discovery: true,
            discovery_interval: Duration::from_secs(5),
        }
    }
}

/// Node state in the cluster
#[derive(Clone, Debug, PartialEq)]
pub enum NodeState {
    /// Initial state, not yet joined
    Initializing,
    /// Joining the cluster
    Joining,
    /// Active member
    Active,
    /// Suspected of failure
    Suspect,
    /// Confirmed dead
    Dead,
    /// Leaving the cluster
    Leaving,
    /// Left the cluster
    Left,
}

/// Role in Raft consensus
#[derive(Clone, Debug, PartialEq)]
pub enum RaftRole {
    Follower,
    Candidate,
    Leader,
}

/// Cluster member information
#[derive(Clone, Debug)]
pub struct ClusterMember {
    /// Node ID
    pub node_id: String,
    /// Address
    pub address: SocketAddr,
    /// State
    pub state: NodeState,
    /// Last heartbeat received
    pub last_heartbeat: Instant,
    /// Capabilities
    pub capabilities: Vec<String>,
    /// Metadata
    pub metadata: HashMap<String, String>,
}

impl ClusterMember {
    pub fn new(node_id: String, address: SocketAddr) -> Self {
        ClusterMember {
            node_id,
            address,
            state: NodeState::Initializing,
            last_heartbeat: Instant::now(),
            capabilities: Vec::new(),
            metadata: HashMap::new(),
        }
    }
}

/// Raft state for leader election
#[derive(Clone, Debug)]
pub struct RaftState {
    /// Current term
    pub current_term: u64,
    /// Who we voted for in current term
    pub voted_for: Option<String>,
    /// Current role
    pub role: RaftRole,
    /// Current leader (if known)
    pub leader_id: Option<String>,
    /// Votes received (as candidate)
    pub votes_received: HashSet<String>,
    /// Last heartbeat from leader
    pub last_heartbeat: Instant,
}

impl Default for RaftState {
    fn default() -> Self {
        RaftState {
            current_term: 0,
            voted_for: None,
            role: RaftRole::Follower,
            leader_id: None,
            votes_received: HashSet::new(),
            last_heartbeat: Instant::now(),
        }
    }
}

/// Barrier for synchronizing distributed operations
#[derive(Clone, Debug)]
pub struct Barrier {
    /// Barrier ID
    pub id: u64,
    /// Expected participants
    pub participants: HashSet<String>,
    /// Arrived participants
    pub arrived: HashSet<String>,
    /// Creation time
    pub created_at: Instant,
    /// Timeout
    pub timeout: Duration,
    /// Released flag
    pub released: bool,
}

impl Barrier {
    pub fn new(id: u64, participants: HashSet<String>, timeout: Duration) -> Self {
        Barrier {
            id,
            participants,
            arrived: HashSet::new(),
            created_at: Instant::now(),
            timeout,
            released: false,
        }
    }

    pub fn arrive(&mut self, node_id: &str) -> bool {
        if self.participants.contains(node_id) {
            self.arrived.insert(node_id.to_string());
        }
        self.is_complete()
    }

    pub fn is_complete(&self) -> bool {
        self.arrived == self.participants
    }

    pub fn is_expired(&self) -> bool {
        self.created_at.elapsed() > self.timeout
    }
}

/// Distributed lock
#[derive(Clone, Debug)]
pub struct DistributedLock {
    /// Lock name
    pub name: String,
    /// Current holder
    pub holder: Option<String>,
    /// Acquisition time
    pub acquired_at: Option<Instant>,
    /// Lease duration
    pub lease_duration: Duration,
    /// Waiting queue
    pub waiters: VecDeque<String>,
}

impl DistributedLock {
    pub fn new(name: String, lease_duration: Duration) -> Self {
        DistributedLock {
            name,
            holder: None,
            acquired_at: None,
            lease_duration,
            waiters: VecDeque::new(),
        }
    }

    pub fn try_acquire(&mut self, node_id: &str) -> bool {
        // Check if lock is free or lease expired
        if self.holder.is_none() || self.is_expired() {
            self.holder = Some(node_id.to_string());
            self.acquired_at = Some(Instant::now());
            return true;
        }

        // Already held by this node
        if self.holder.as_ref() == Some(&node_id.to_string()) {
            return true;
        }

        // Add to waiters
        if !self.waiters.contains(&node_id.to_string()) {
            self.waiters.push_back(node_id.to_string());
        }

        false
    }

    pub fn release(&mut self, node_id: &str) -> bool {
        if self.holder.as_ref() == Some(&node_id.to_string()) {
            // Grant to next waiter
            self.holder = self.waiters.pop_front();
            self.acquired_at = if self.holder.is_some() {
                Some(Instant::now())
            } else {
                None
            };
            return true;
        }
        false
    }

    pub fn is_expired(&self) -> bool {
        if let Some(acquired) = self.acquired_at {
            acquired.elapsed() > self.lease_duration
        } else {
            false
        }
    }
}

/// Cluster statistics
#[derive(Clone, Debug, Default)]
pub struct ClusterStats {
    /// Number of active members
    pub active_members: usize,
    /// Number of elections held
    pub elections: u64,
    /// Current term
    pub current_term: u64,
    /// Leader changes
    pub leader_changes: u64,
    /// Messages sent
    pub messages_sent: u64,
    /// Messages received
    pub messages_received: u64,
    /// Barriers completed
    pub barriers_completed: u64,
    /// Locks granted
    pub locks_granted: u64,
}

/// The main cluster manager
pub struct Cluster {
    /// Configuration
    config: ClusterConfig,
    /// Communication node
    node: Arc<Mutex<Node>>,
    /// Cluster members
    members: Arc<RwLock<HashMap<String, ClusterMember>>>,
    /// Raft state
    raft_state: Arc<RwLock<RaftState>>,
    /// Active barriers
    barriers: Arc<RwLock<HashMap<u64, Barrier>>>,
    /// Distributed locks
    locks: Arc<RwLock<HashMap<String, DistributedLock>>>,
    /// Running flag
    running: Arc<Mutex<bool>>,
    /// Background threads
    threads: Vec<JoinHandle<()>>,
    /// Statistics
    stats: Arc<RwLock<ClusterStats>>,
    /// Event handlers
    handlers: Arc<RwLock<ClusterHandlers>>,
}

/// Cluster event handlers
#[derive(Default)]
pub struct ClusterHandlers {
    /// Called when a node joins
    pub on_join: Option<Box<dyn Fn(&str) + Send + Sync>>,
    /// Called when a node leaves
    pub on_leave: Option<Box<dyn Fn(&str) + Send + Sync>>,
    /// Called when leadership changes
    pub on_leader_change: Option<Box<dyn Fn(Option<&str>, &str) + Send + Sync>>,
    /// Called when a barrier is released
    pub on_barrier_release: Option<Box<dyn Fn(u64) + Send + Sync>>,
}

impl Cluster {
    /// Create a new cluster manager
    pub fn new(config: ClusterConfig) -> std::io::Result<Self> {
        let node_config = NodeConfig {
            node_id: config.node_id.clone(),
            bind_address: config.bind_address,
            transport: config.transport.clone(),
            ..Default::default()
        };

        let mut node = Node::new(node_config);
        node.start()?;

        let cluster = Cluster {
            config,
            node: Arc::new(Mutex::new(node)),
            members: Arc::new(RwLock::new(HashMap::new())),
            raft_state: Arc::new(RwLock::new(RaftState::default())),
            barriers: Arc::new(RwLock::new(HashMap::new())),
            locks: Arc::new(RwLock::new(HashMap::new())),
            running: Arc::new(Mutex::new(true)),
            threads: Vec::new(),
            stats: Arc::new(RwLock::new(ClusterStats::default())),
            handlers: Arc::new(RwLock::new(ClusterHandlers::default())),
        };

        Ok(cluster)
    }

    /// Get node ID
    pub fn node_id(&self) -> &str {
        &self.config.node_id
    }

    /// Get bound address
    pub fn address(&self) -> Option<SocketAddr> {
        self.node.lock().unwrap().address()
    }

    /// Join the cluster
    pub fn join(&mut self) -> std::io::Result<()> {
        // Add self to members
        if let Some(addr) = self.address() {
            let mut members = self.members.write().unwrap();
            let mut self_member = ClusterMember::new(self.config.node_id.clone(), addr);
            self_member.state = NodeState::Active;
            members.insert(self.config.node_id.clone(), self_member);
        }

        // Connect to seed nodes
        for seed in &self.config.seed_nodes {
            if let Ok(addr) = seed.parse::<SocketAddr>() {
                let node = self.node.lock().unwrap();
                let _ = node.send(
                    &addr.to_string(),
                    IpcMessage::JoinRequest {
                        node_id: self.config.node_id.clone(),
                        address: self.address().map(|a| a.to_string()).unwrap_or_default(),
                    },
                );
            }
        }

        // Start background threads
        self.start_heartbeat_thread();
        self.start_election_thread();
        self.start_message_handler_thread();

        Ok(())
    }

    /// Leave the cluster gracefully
    pub fn leave(&mut self) -> std::io::Result<()> {
        *self.running.lock().unwrap() = false;

        // Notify other members
        let leave_msg = IpcMessage::Leave {
            node_id: self.config.node_id.clone(),
        };
        let _ = self.node.lock().unwrap().broadcast(leave_msg);

        // Wait for threads
        for handle in self.threads.drain(..) {
            let _ = handle.join();
        }

        Ok(())
    }

    /// Get current leader
    pub fn leader(&self) -> Option<String> {
        self.raft_state.read().unwrap().leader_id.clone()
    }

    /// Check if this node is the leader
    pub fn is_leader(&self) -> bool {
        self.raft_state.read().unwrap().role == RaftRole::Leader
    }

    /// Wait for a leader to be elected
    pub fn await_leader(&self, timeout: Duration) -> Option<String> {
        let start = Instant::now();
        while start.elapsed() < timeout {
            if let Some(leader) = self.leader() {
                return Some(leader);
            }
            thread::sleep(Duration::from_millis(10));
        }
        None
    }

    /// Get cluster members
    pub fn members(&self) -> Vec<ClusterMember> {
        self.members.read().unwrap().values().cloned().collect()
    }

    /// Get active member count
    pub fn active_count(&self) -> usize {
        self.members
            .read()
            .unwrap()
            .values()
            .filter(|m| m.state == NodeState::Active)
            .count()
    }

    /// Create a barrier for synchronization
    pub fn create_barrier(&self, participants: Vec<String>, timeout: Duration) -> u64 {
        let id = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64;

        let barrier = Barrier::new(id, participants.into_iter().collect(), timeout);
        self.barriers.write().unwrap().insert(id, barrier);

        id
    }

    /// Arrive at a barrier
    pub fn arrive_at_barrier(&self, barrier_id: u64) -> bool {
        let mut barriers = self.barriers.write().unwrap();
        if let Some(barrier) = barriers.get_mut(&barrier_id) {
            if barrier.arrive(&self.config.node_id) {
                barrier.released = true;

                // Notify other participants
                let release_msg = IpcMessage::BarrierRelease { barrier_id };
                let _ = self.node.lock().unwrap().broadcast(release_msg);

                self.stats.write().unwrap().barriers_completed += 1;
                return true;
            }
        }
        false
    }

    /// Wait for a barrier to be released
    pub fn wait_barrier(&self, barrier_id: u64, timeout: Duration) -> bool {
        // First arrive at the barrier
        self.arrive_at_barrier(barrier_id);

        let start = Instant::now();
        while start.elapsed() < timeout {
            let barriers = self.barriers.read().unwrap();
            if let Some(barrier) = barriers.get(&barrier_id) {
                if barrier.released || barrier.is_complete() {
                    return true;
                }
            } else {
                return false; // Barrier doesn't exist
            }
            drop(barriers);
            thread::sleep(Duration::from_millis(1));
        }

        false
    }

    /// Try to acquire a distributed lock
    pub fn try_lock(&self, name: &str) -> bool {
        let mut locks = self.locks.write().unwrap();
        let lock = locks
            .entry(name.to_string())
            .or_insert_with(|| DistributedLock::new(name.to_string(), Duration::from_secs(30)));

        let acquired = lock.try_acquire(&self.config.node_id);
        if acquired {
            self.stats.write().unwrap().locks_granted += 1;
        }
        acquired
    }

    /// Release a distributed lock
    pub fn unlock(&self, name: &str) -> bool {
        let mut locks = self.locks.write().unwrap();
        if let Some(lock) = locks.get_mut(name) {
            return lock.release(&self.config.node_id);
        }
        false
    }

    /// Get statistics
    pub fn stats(&self) -> ClusterStats {
        let mut stats = self.stats.read().unwrap().clone();
        stats.active_members = self.active_count();
        stats.current_term = self.raft_state.read().unwrap().current_term;
        stats
    }

    /// Set event handlers
    pub fn set_handlers(&self, handlers: ClusterHandlers) {
        *self.handlers.write().unwrap() = handlers;
    }

    /// Send message to a specific node
    pub fn send_to(&self, node_id: &str, msg: IpcMessage) -> std::io::Result<()> {
        let members = self.members.read().unwrap();
        if let Some(member) = members.get(node_id) {
            let node = self.node.lock().unwrap();
            node.add_known_node(node_id, member.address);
            node.send(node_id, msg)?;
            self.stats.write().unwrap().messages_sent += 1;
        }
        Ok(())
    }

    /// Broadcast message to all nodes
    pub fn broadcast(&self, msg: IpcMessage) -> std::io::Result<usize> {
        let count = self.node.lock().unwrap().broadcast(msg)?;
        self.stats.write().unwrap().messages_sent += count as u64;
        Ok(count)
    }

    /// Send triples to the cluster (to leader or partitioned)
    pub fn send_triples(&self, triples: Vec<Triple>) -> std::io::Result<()> {
        if let Some(leader) = self.leader() {
            if leader != self.config.node_id {
                return self.send_to(&leader, IpcMessage::Triples(triples));
            }
        }
        // We are the leader or no leader - handle locally
        Ok(())
    }

    // Background thread implementations

    fn start_heartbeat_thread(&mut self) {
        let running = self.running.clone();
        let node = self.node.clone();
        let raft_state = self.raft_state.clone();
        let members = self.members.clone();
        let config = self.config.clone();
        let stats = self.stats.clone();

        let handle = thread::spawn(move || {
            while *running.lock().unwrap() {
                let state = raft_state.read().unwrap();
                if state.role == RaftRole::Leader {
                    // Send heartbeat to all followers
                    let ping = IpcMessage::Ping {
                        timestamp: SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_millis() as u64,
                        node_id: config.node_id.clone(),
                    };
                    if let Ok(count) = node.lock().unwrap().broadcast(ping) {
                        stats.write().unwrap().messages_sent += count as u64;
                    }
                }
                drop(state);

                // Check for dead members
                let now = Instant::now();
                let timeout = config.election_timeout.1 * 3;
                let mut members_guard = members.write().unwrap();
                for member in members_guard.values_mut() {
                    if member.node_id != config.node_id && member.state == NodeState::Active {
                        if now.duration_since(member.last_heartbeat) > timeout {
                            member.state = NodeState::Suspect;
                        }
                    }
                }
                drop(members_guard);

                thread::sleep(config.heartbeat_interval);
            }
        });

        self.threads.push(handle);
    }

    fn start_election_thread(&mut self) {
        let running = self.running.clone();
        let raft_state = self.raft_state.clone();
        let members = self.members.clone();
        let node = self.node.clone();
        let config = self.config.clone();
        let stats = self.stats.clone();
        let handlers = self.handlers.clone();

        let handle = thread::spawn(move || {
            let mut rng_state = config.node_id.len() as u64;

            while *running.lock().unwrap() {
                let state = raft_state.read().unwrap();
                let role = state.role.clone();
                let last_heartbeat = state.last_heartbeat;
                drop(state);

                if role != RaftRole::Leader {
                    // Random election timeout
                    rng_state = rng_state.wrapping_mul(1103515245).wrapping_add(12345);
                    let timeout_range = config.election_timeout.1 - config.election_timeout.0;
                    let random_offset = Duration::from_millis(
                        (rng_state % timeout_range.as_millis() as u64) as u64,
                    );
                    let election_timeout = config.election_timeout.0 + random_offset;

                    if last_heartbeat.elapsed() > election_timeout {
                        // Start election
                        let mut state = raft_state.write().unwrap();
                        state.current_term += 1;
                        state.role = RaftRole::Candidate;
                        state.voted_for = Some(config.node_id.clone());
                        state.votes_received.clear();
                        state.votes_received.insert(config.node_id.clone());

                        stats.write().unwrap().elections += 1;

                        let vote_request = IpcMessage::Vote {
                            term: state.current_term,
                            candidate_id: config.node_id.clone(),
                        };
                        drop(state);

                        let _ = node.lock().unwrap().broadcast(vote_request);

                        // Wait for votes
                        thread::sleep(config.election_timeout.0);

                        let mut state = raft_state.write().unwrap();
                        if state.role == RaftRole::Candidate {
                            let member_count = members.read().unwrap().len();
                            let votes = state.votes_received.len();

                            if votes > member_count / 2 {
                                // Won election
                                let old_leader = state.leader_id.clone();
                                state.role = RaftRole::Leader;
                                state.leader_id = Some(config.node_id.clone());

                                stats.write().unwrap().leader_changes += 1;

                                // Notify handlers
                                if let Some(ref handler) = handlers.read().unwrap().on_leader_change
                                {
                                    handler(
                                        old_leader.as_deref(),
                                        &config.node_id,
                                    );
                                }
                            } else {
                                // Lost election, become follower
                                state.role = RaftRole::Follower;
                            }
                        }
                    }
                }

                thread::sleep(Duration::from_millis(10));
            }
        });

        self.threads.push(handle);
    }

    fn start_message_handler_thread(&mut self) {
        let running = self.running.clone();
        let node = self.node.clone();
        let raft_state = self.raft_state.clone();
        let members = self.members.clone();
        let barriers = self.barriers.clone();
        let config = self.config.clone();
        let stats = self.stats.clone();
        let handlers = self.handlers.clone();

        let handle = thread::spawn(move || {
            while *running.lock().unwrap() {
                // Process received messages
                if let Some((sender, msg)) = node.lock().unwrap().receive() {
                    stats.write().unwrap().messages_received += 1;

                    match msg {
                        IpcMessage::Ping { timestamp: _, node_id } => {
                            // Update last heartbeat for sender
                            let mut members_guard = members.write().unwrap();
                            if let Some(member) = members_guard.get_mut(&node_id) {
                                member.last_heartbeat = Instant::now();
                            }

                            // Reset election timer if from leader
                            let mut state = raft_state.write().unwrap();
                            if Some(&node_id) == state.leader_id.as_ref() {
                                state.last_heartbeat = Instant::now();
                            }

                            // Send pong
                            let pong = IpcMessage::Pong {
                                timestamp: SystemTime::now()
                                    .duration_since(UNIX_EPOCH)
                                    .unwrap()
                                    .as_millis() as u64,
                                node_id: config.node_id.clone(),
                            };
                            let _ = node.lock().unwrap().send(&sender, pong);
                        }

                        IpcMessage::Vote { term, candidate_id } => {
                            let mut state = raft_state.write().unwrap();

                            let grant = if term > state.current_term {
                                state.current_term = term;
                                state.voted_for = Some(candidate_id.clone());
                                state.role = RaftRole::Follower;
                                true
                            } else if term == state.current_term && state.voted_for.is_none() {
                                state.voted_for = Some(candidate_id.clone());
                                true
                            } else {
                                false
                            };

                            let response = IpcMessage::VoteResponse {
                                term: state.current_term,
                                granted: grant,
                            };
                            let _ = node.lock().unwrap().send(&sender, response);
                        }

                        IpcMessage::VoteResponse { term, granted } => {
                            let mut state = raft_state.write().unwrap();
                            if term == state.current_term
                                && state.role == RaftRole::Candidate
                                && granted
                            {
                                state.votes_received.insert(sender.clone());
                            }
                        }

                        IpcMessage::JoinRequest { node_id, address } => {
                            // Add new member
                            if let Ok(addr) = address.parse::<SocketAddr>() {
                                let mut members_guard = members.write().unwrap();
                                let mut member = ClusterMember::new(node_id.clone(), addr);
                                member.state = NodeState::Active;
                                members_guard.insert(node_id.clone(), member);

                                // Notify handlers
                                if let Some(ref handler) = handlers.read().unwrap().on_join {
                                    handler(&node_id);
                                }

                                // Send ack with current cluster members
                                let cluster_nodes: Vec<_> = members_guard
                                    .iter()
                                    .map(|(id, m)| (id.clone(), m.address.to_string()))
                                    .collect();
                                drop(members_guard);

                                let ack = IpcMessage::JoinAck {
                                    success: true,
                                    cluster_nodes,
                                };
                                let _ = node.lock().unwrap().send(&sender, ack);
                            }
                        }

                        IpcMessage::JoinAck { success, cluster_nodes } => {
                            if success {
                                let mut members_guard = members.write().unwrap();
                                for (id, addr_str) in cluster_nodes {
                                    if id != config.node_id {
                                        if let Ok(addr) = addr_str.parse::<SocketAddr>() {
                                            if !members_guard.contains_key(&id) {
                                                let mut member = ClusterMember::new(id.clone(), addr);
                                                member.state = NodeState::Active;
                                                members_guard.insert(id, member);
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        IpcMessage::Leave { node_id } => {
                            let mut members_guard = members.write().unwrap();
                            if let Some(member) = members_guard.get_mut(&node_id) {
                                member.state = NodeState::Left;
                            }

                            if let Some(ref handler) = handlers.read().unwrap().on_leave {
                                handler(&node_id);
                            }
                        }

                        IpcMessage::Barrier { barrier_id, node_id } => {
                            let mut barriers_guard = barriers.write().unwrap();
                            if let Some(barrier) = barriers_guard.get_mut(&barrier_id) {
                                if barrier.arrive(&node_id) {
                                    // Barrier complete, notify all
                                    let release = IpcMessage::BarrierRelease { barrier_id };
                                    let _ = node.lock().unwrap().broadcast(release);
                                }
                            }
                        }

                        IpcMessage::BarrierRelease { barrier_id } => {
                            let mut barriers_guard = barriers.write().unwrap();
                            if let Some(barrier) = barriers_guard.get_mut(&barrier_id) {
                                barrier.released = true;

                                if let Some(ref handler) =
                                    handlers.read().unwrap().on_barrier_release
                                {
                                    handler(barrier_id);
                                }
                            }
                        }

                        _ => {}
                    }
                }

                thread::sleep(Duration::from_millis(1));
            }
        });

        self.threads.push(handle);
    }
}

impl Drop for Cluster {
    fn drop(&mut self) {
        let _ = self.leave();
    }
}

/// Builder for cluster configuration
pub struct ClusterBuilder {
    config: ClusterConfig,
}

impl ClusterBuilder {
    pub fn new() -> Self {
        ClusterBuilder {
            config: ClusterConfig::default(),
        }
    }

    pub fn cluster_name(mut self, name: &str) -> Self {
        self.config.cluster_name = name.to_string();
        self
    }

    pub fn node_id(mut self, id: &str) -> Self {
        self.config.node_id = id.to_string();
        self
    }

    pub fn bind_address(mut self, addr: SocketAddr) -> Self {
        self.config.bind_address = addr;
        self
    }

    pub fn seed_node(mut self, addr: &str) -> Self {
        self.config.seed_nodes.push(addr.to_string());
        self
    }

    pub fn seed_nodes(mut self, addrs: Vec<String>) -> Self {
        self.config.seed_nodes = addrs;
        self
    }

    pub fn transport(mut self, transport: Transport) -> Self {
        self.config.transport = transport;
        self
    }

    pub fn election_timeout(mut self, min: Duration, max: Duration) -> Self {
        self.config.election_timeout = (min, max);
        self
    }

    pub fn heartbeat_interval(mut self, interval: Duration) -> Self {
        self.config.heartbeat_interval = interval;
        self
    }

    pub fn build(self) -> std::io::Result<Cluster> {
        Cluster::new(self.config)
    }
}

impl Default for ClusterBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cluster_config_default() {
        let config = ClusterConfig::default();
        assert!(!config.node_id.is_empty());
        assert!(config.seed_nodes.is_empty());
    }

    #[test]
    fn test_barrier() {
        let mut barrier = Barrier::new(
            1,
            vec!["node-1".to_string(), "node-2".to_string()].into_iter().collect(),
            Duration::from_secs(10),
        );

        assert!(!barrier.is_complete());
        assert!(!barrier.arrive("node-1"));
        assert!(barrier.arrive("node-2"));
        assert!(barrier.is_complete());
    }

    #[test]
    fn test_distributed_lock() {
        let mut lock = DistributedLock::new("test-lock".to_string(), Duration::from_secs(30));

        assert!(lock.try_acquire("node-1"));
        assert!(!lock.try_acquire("node-2"));
        assert!(lock.release("node-1"));
        assert!(lock.try_acquire("node-2"));
    }

    #[test]
    fn test_raft_state_default() {
        let state = RaftState::default();
        assert_eq!(state.current_term, 0);
        assert_eq!(state.role, RaftRole::Follower);
        assert!(state.voted_for.is_none());
    }

    #[test]
    fn test_cluster_builder() {
        let config = ClusterBuilder::new()
            .cluster_name("test-cluster")
            .node_id("test-node")
            .seed_node("127.0.0.1:9001")
            .heartbeat_interval(Duration::from_millis(100))
            .config;

        assert_eq!(config.cluster_name, "test-cluster");
        assert_eq!(config.node_id, "test-node");
        assert_eq!(config.seed_nodes.len(), 1);
    }
}
