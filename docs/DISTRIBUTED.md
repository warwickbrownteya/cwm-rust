# CWM-Rust Distributed Reasoning & IPC

Documentation for inter-process communication, clustering, and distributed reasoning capabilities.

## Table of Contents

- [Overview](#overview)
- [Inter-Process Communication](#inter-process-communication)
- [Cluster Coordination](#cluster-coordination)
- [Distributed Reasoning](#distributed-reasoning)
- [Configuration](#configuration)
- [Examples](#examples)

---

## Overview

cwm-rust supports distributed reasoning across multiple nodes via:

- **IPC Module** - Inter-process communication with multiple transports
- **Cluster Module** - Raft-based consensus and coordination
- **Distributed Module** - Graph partitioning and parallel inference

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Application Layer                         │
│         (Distributed Reasoning, Query Federation)            │
├─────────────────────────────────────────────────────────────┤
│                    Cluster Layer                             │
│    (Raft Consensus, Leader Election, Distributed Locks)     │
├─────────────────────────────────────────────────────────────┤
│                    IPC Layer                                 │
│         (TCP, UDP, Unix Sockets, In-Memory)                 │
└─────────────────────────────────────────────────────────────┘
```

---

## Inter-Process Communication

### Transports

cwm-rust supports multiple transport protocols:

| Transport | Use Case | Latency | Reliability |
|-----------|----------|---------|-------------|
| TCP | Remote nodes | Medium | High |
| UDP | Low-latency, loss-tolerant | Low | Medium |
| Unix Socket | Local processes | Very Low | High |
| In-Memory | Testing, same-process | Minimal | Perfect |

### Message Types

The IPC system supports 20+ message types:

```rust
pub enum IpcMessage {
    // Discovery
    Ping { sender: String, timestamp: u64 },
    Pong { sender: String, request_timestamp: u64 },

    // Data Transfer
    TripleData { triples: Vec<Triple>, source: String },
    Query { query_id: String, pattern: Pattern, sender: String },
    QueryResponse { query_id: String, results: Vec<Bindings> },

    // Inference
    InferenceRequest { rule_id: String, bindings: Bindings },
    InferenceResult { rule_id: String, derived: Vec<Triple> },

    // Cluster
    JoinRequest { node_id: String, address: String },
    JoinResponse { accepted: bool, members: Vec<String> },
    LeaveNotification { node_id: String },

    // Raft
    VoteRequest { term: u64, candidate: String, last_log: u64 },
    VoteResponse { term: u64, granted: bool },
    AppendEntries { term: u64, leader: String, entries: Vec<Entry> },
    AppendResponse { term: u64, success: bool, match_index: u64 },

    // Synchronization
    BarrierEnter { barrier_id: String, node_id: String },
    BarrierRelease { barrier_id: String },
    LockRequest { lock_id: String, node_id: String },
    LockGrant { lock_id: String, holder: String },
    LockRelease { lock_id: String },

    // Heartbeat
    Heartbeat { sender: String, timestamp: u64, stats: NodeStats },
}
```

### Serialization Formats

| Format | Size | Speed | Human-Readable |
|--------|------|-------|----------------|
| JSON | Large | Medium | Yes |
| MessagePack | Small | Fast | No |
| Bincode | Smallest | Fastest | No |

### Node Configuration

```rust
pub struct NodeConfig {
    pub node_id: String,
    pub listen_address: String,
    pub transport: TransportType,
    pub serialization: SerializationFormat,
    pub heartbeat_interval_ms: u64,
    pub connection_timeout_ms: u64,
    pub max_connections: usize,
}
```

---

## Cluster Coordination

### Raft Consensus

cwm-rust implements Raft for distributed consensus:

```
┌─────────┐     ┌─────────┐     ┌─────────┐
│  Node A │────▶│  Node B │────▶│  Node C │
│ (Leader)│◀────│(Follower)│◀────│(Follower)│
└─────────┘     └─────────┘     └─────────┘
     │               │               │
     └───────────────┴───────────────┘
           Replicated Log
```

**Features**:
- Leader election with term-based voting
- Log replication with majority consensus
- Automatic failover on leader failure
- Split-brain prevention

### Cluster States

```rust
pub enum NodeState {
    Follower,     // Follows current leader
    Candidate,    // Requesting votes for leadership
    Leader,       // Handles all client requests
    Offline,      // Not participating
}
```

### Distributed Locks

Acquire exclusive access to resources across cluster:

```rust
// Request lock
let lock = cluster.acquire_lock("resource_1", timeout).await?;

// Critical section
// ...

// Release lock
cluster.release_lock(lock).await?;
```

### Barrier Synchronization

Coordinate multiple nodes at synchronization points:

```rust
// All nodes must reach this point before continuing
let barrier = cluster.create_barrier("phase_1", expected_nodes);
barrier.wait().await?;

// All nodes continue together
```

### Cluster Configuration

```rust
pub struct ClusterConfig {
    pub node_id: String,
    pub seed_nodes: Vec<String>,
    pub election_timeout_ms: (u64, u64),  // min, max
    pub heartbeat_interval_ms: u64,
    pub log_compaction_threshold: usize,
    pub snapshot_interval: usize,
}
```

---

## Distributed Reasoning

### Graph Partitioning

Distribute data across nodes for parallel processing:

**Partitioning Strategies**:
- **Hash-based**: Partition by subject hash
- **Predicate-based**: Group by predicate type
- **Graph-based**: Use graph structure for locality
- **Random**: Even distribution

```rust
pub struct DistributedReasoner {
    partitioner: Partitioner,
    local_store: Store,
    remote_nodes: Vec<NodeConnection>,
    coordinator: Coordinator,
}
```

### Parallel Inference

```
Node A: Rules R1, R2    ──┐
Node B: Rules R3, R4    ──┼──▶ Coordinator ──▶ Merged Results
Node C: Rules R5, R6    ──┘
```

**Process**:
1. Partition rules across nodes
2. Apply rules locally in parallel
3. Exchange derived facts
4. Iterate until fixpoint

### Query Federation

Distribute queries across nodes:

```rust
// Query spanning multiple partitions
let results = distributed_query(
    "SELECT ?s ?p ?o WHERE { ?s ?p ?o }",
    &nodes,
    QueryStrategy::Parallel,
).await?;
```

---

## Configuration

### IPC Configuration

```toml
[ipc]
# Node identification
node_id = "node-1"

# Listen address
listen_address = "0.0.0.0:9000"

# Transport protocol
transport = "tcp"  # tcp, udp, unix

# Serialization format
serialization = "msgpack"  # json, msgpack, bincode

# Heartbeat interval in milliseconds
heartbeat_interval_ms = 1000

# Connection timeout in milliseconds
connection_timeout_ms = 5000

# Maximum concurrent connections
max_connections = 100
```

### Cluster Configuration

```toml
[cluster]
# This node's ID (must be unique)
node_id = "node-1"

# Seed nodes for cluster discovery
seed_nodes = [
    "192.168.1.10:9000",
    "192.168.1.11:9000",
    "192.168.1.12:9000",
]

# Raft election timeout range (ms)
election_timeout_min = 150
election_timeout_max = 300

# Leader heartbeat interval (ms)
heartbeat_interval_ms = 50

# Log compaction threshold (entries)
log_compaction_threshold = 10000

# Snapshot interval (applied entries)
snapshot_interval = 5000
```

### Distributed Reasoning Configuration

```toml
[distributed]
# Enable distributed mode
enabled = true

# Partitioning strategy
partition_strategy = "hash"  # hash, predicate, graph, random

# Number of partitions (0 = auto based on nodes)
num_partitions = 0

# Replication factor
replication_factor = 2

# Maximum parallel workers per node
max_workers = 4

# Batch size for fact exchange
exchange_batch_size = 1000
```

---

## Examples

### Basic IPC Setup

```rust
use cwm::reasoning::ipc::{Node, NodeConfig, Transport};

// Create node
let config = NodeConfig {
    node_id: "node-1".to_string(),
    listen_address: "0.0.0.0:9000".to_string(),
    transport: Transport::Tcp,
    ..Default::default()
};

let node = Node::new(config).await?;

// Connect to peer
node.connect("192.168.1.11:9000").await?;

// Send message
node.send("peer-1", IpcMessage::Ping {
    sender: "node-1".to_string(),
    timestamp: now(),
}).await?;

// Receive messages
while let Some(msg) = node.receive().await {
    match msg {
        IpcMessage::Query { query_id, pattern, sender } => {
            let results = local_store.query(&pattern);
            node.send(&sender, IpcMessage::QueryResponse {
                query_id,
                results,
            }).await?;
        }
        _ => {}
    }
}
```

### Cluster Setup

```rust
use cwm::reasoning::cluster::{Cluster, ClusterConfig};

// Create cluster node
let config = ClusterConfig {
    node_id: "node-1".to_string(),
    seed_nodes: vec![
        "192.168.1.10:9000".to_string(),
        "192.168.1.11:9000".to_string(),
    ],
    ..Default::default()
};

let cluster = Cluster::new(config).await?;

// Join cluster
cluster.join().await?;

// Check role
match cluster.role() {
    RaftRole::Leader => println!("I am the leader"),
    RaftRole::Follower => println!("Following leader"),
    RaftRole::Candidate => println!("Election in progress"),
}

// Use distributed lock
let lock = cluster.lock("reasoning_phase").await?;
// Critical section
lock.release().await?;

// Use barrier
let barrier = cluster.barrier("sync_point", 3).await?;
barrier.wait().await?;  // Wait for all 3 nodes
```

### Distributed Query

```rust
use cwm::reasoning::distributed::DistributedReasoner;

// Setup distributed reasoner
let reasoner = DistributedReasoner::new(
    local_store,
    cluster,
    PartitionStrategy::Hash,
).await?;

// Distributed query
let query = "SELECT ?s WHERE { ?s a ex:Person }";
let results = reasoner.query(query).await?;

// Distributed inference
reasoner.think(rules, max_steps).await?;
```

### Multi-Node Deployment

**Node 1 (seed)**:
```bash
cwm --node-id node-1 \
    --listen 0.0.0.0:9000 \
    --cluster-mode \
    data.n3 --think
```

**Node 2**:
```bash
cwm --node-id node-2 \
    --listen 0.0.0.0:9000 \
    --seed-nodes 192.168.1.10:9000 \
    --cluster-mode \
    data.n3 --think
```

**Node 3**:
```bash
cwm --node-id node-3 \
    --listen 0.0.0.0:9000 \
    --seed-nodes 192.168.1.10:9000 \
    --cluster-mode \
    data.n3 --think
```

---

## Monitoring

### Node Statistics

```rust
pub struct NodeStats {
    pub messages_sent: u64,
    pub messages_received: u64,
    pub bytes_sent: u64,
    pub bytes_received: u64,
    pub active_connections: usize,
    pub pending_messages: usize,
    pub uptime_secs: u64,
}
```

### Cluster Statistics

```rust
pub struct ClusterStats {
    pub current_term: u64,
    pub current_leader: Option<String>,
    pub cluster_size: usize,
    pub healthy_nodes: usize,
    pub log_size: usize,
    pub last_applied: u64,
    pub commit_index: u64,
}
```

### Health Endpoint

When running as server:

```bash
curl http://localhost:8080/cluster/health
```

```json
{
  "node_id": "node-1",
  "role": "leader",
  "term": 42,
  "cluster_size": 3,
  "healthy": true,
  "members": [
    {"id": "node-1", "state": "leader", "healthy": true},
    {"id": "node-2", "state": "follower", "healthy": true},
    {"id": "node-3", "state": "follower", "healthy": true}
  ]
}
```

---

## Best Practices

### Network Configuration

1. **Use dedicated network** for cluster traffic
2. **Configure firewall** to allow cluster ports
3. **Use low-latency connections** between nodes
4. **Monitor network partitions**

### Data Partitioning

1. **Choose strategy based on query patterns**
2. **Balance partition sizes**
3. **Consider locality for related data**
4. **Plan for node failures**

### Fault Tolerance

1. **Use odd number of nodes** (3, 5, 7) for quorum
2. **Configure appropriate timeouts**
3. **Monitor cluster health**
4. **Test failover scenarios**

---

## See Also

- [CONFIGURATION.md](CONFIGURATION.md) - Configuration reference
- [REASONING.md](REASONING.md) - Reasoning strategies
- [SERVER.md](SERVER.md) - HTTP server documentation
