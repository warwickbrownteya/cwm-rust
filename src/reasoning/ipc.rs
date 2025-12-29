//! Inter-Process Communication Module
//!
//! Provides communication between cwm-rust processes on the same machine
//! and across the network.
//!
//! # Features
//!
//! - Unix domain sockets for local IPC
//! - TCP/UDP for network communication
//! - Message serialization (JSON, binary)
//! - Connection pooling and management
//! - Async and sync APIs
//!
//! # Example
//!
//! ```ignore
//! use cwm::reasoning::ipc::{Node, NodeConfig, Transport};
//!
//! // Create a node
//! let node = Node::new(NodeConfig {
//!     node_id: "node-1".to_string(),
//!     bind_address: "127.0.0.1:9000".parse().unwrap(),
//!     transport: Transport::Tcp,
//! });
//!
//! // Start listening
//! node.start()?;
//!
//! // Send message to another node
//! node.send("node-2", Message::Triple(triple))?;
//! ```

use std::collections::{HashMap, VecDeque};
use std::io::{self, Read, Write, BufReader, BufWriter};
use std::net::{TcpListener, TcpStream, UdpSocket, SocketAddr, ToSocketAddrs};
use std::sync::{Arc, Mutex, RwLock, mpsc};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};
use std::path::PathBuf;

#[cfg(unix)]
use std::os::unix::net::{UnixListener, UnixStream};

use crate::term::{Term, Triple, Bindings};
use crate::reasoner::Rule;

/// Transport protocol for communication
#[derive(Clone, Debug, PartialEq)]
pub enum Transport {
    /// TCP for reliable network communication
    Tcp,
    /// UDP for fast, unreliable communication
    Udp,
    /// Unix domain socket for local IPC
    Unix,
    /// In-memory channel (same process)
    InMemory,
}

impl Default for Transport {
    fn default() -> Self {
        Transport::Tcp
    }
}

/// Message types for inter-node communication
#[derive(Clone, Debug)]
pub enum IpcMessage {
    /// A single triple
    Triple(Triple),
    /// Multiple triples (batch)
    Triples(Vec<Triple>),
    /// Variable bindings
    Bindings(Bindings),
    /// A rule
    Rule(Rule),
    /// Query request
    Query {
        query_id: u64,
        patterns: Vec<Triple>,
    },
    /// Query response
    QueryResponse {
        query_id: u64,
        results: Vec<Bindings>,
    },
    /// Inference request
    InferRequest {
        request_id: u64,
        triples: Vec<Triple>,
        rules: Vec<Rule>,
    },
    /// Inference response
    InferResponse {
        request_id: u64,
        inferred: Vec<Triple>,
    },
    /// Heartbeat/ping
    Ping {
        timestamp: u64,
        node_id: String,
    },
    /// Heartbeat response
    Pong {
        timestamp: u64,
        node_id: String,
    },
    /// Node announcement (discovery)
    Announce {
        node_id: String,
        address: String,
        capabilities: Vec<String>,
    },
    /// Join cluster request
    JoinRequest {
        node_id: String,
        address: String,
    },
    /// Join acknowledgment
    JoinAck {
        success: bool,
        cluster_nodes: Vec<(String, String)>,
    },
    /// Leave notification
    Leave {
        node_id: String,
    },
    /// Barrier synchronization
    Barrier {
        barrier_id: u64,
        node_id: String,
    },
    /// Barrier release
    BarrierRelease {
        barrier_id: u64,
    },
    /// Leader election vote
    Vote {
        term: u64,
        candidate_id: String,
    },
    /// Vote response
    VoteResponse {
        term: u64,
        granted: bool,
    },
    /// Custom message with payload
    Custom {
        message_type: String,
        payload: Vec<u8>,
    },
    /// Error message
    Error {
        code: u32,
        message: String,
    },
    /// Shutdown request
    Shutdown,
}

/// Serialization format for messages
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SerializationFormat {
    /// JSON format (human-readable, larger)
    Json,
    /// Binary format (compact, fast)
    Binary,
}

impl Default for SerializationFormat {
    fn default() -> Self {
        SerializationFormat::Binary
    }
}

/// Node configuration
#[derive(Clone, Debug)]
pub struct NodeConfig {
    /// Unique node identifier
    pub node_id: String,
    /// Address to bind to
    pub bind_address: SocketAddr,
    /// Transport protocol
    pub transport: Transport,
    /// Serialization format
    pub format: SerializationFormat,
    /// Connection timeout
    pub connect_timeout: Duration,
    /// Read/write timeout
    pub io_timeout: Duration,
    /// Maximum message size
    pub max_message_size: usize,
    /// Enable TLS (for TCP)
    pub tls_enabled: bool,
    /// Heartbeat interval
    pub heartbeat_interval: Duration,
    /// Maximum connections
    pub max_connections: usize,
}

impl Default for NodeConfig {
    fn default() -> Self {
        NodeConfig {
            node_id: format!("node-{}", std::process::id()),
            bind_address: "127.0.0.1:0".parse().unwrap(),
            transport: Transport::Tcp,
            format: SerializationFormat::Binary,
            connect_timeout: Duration::from_secs(5),
            io_timeout: Duration::from_secs(30),
            max_message_size: 16 * 1024 * 1024, // 16MB
            tls_enabled: false,
            heartbeat_interval: Duration::from_secs(10),
            max_connections: 100,
        }
    }
}

/// Connection state
#[derive(Clone, Debug, PartialEq)]
pub enum ConnectionState {
    Connecting,
    Connected,
    Disconnected,
    Failed(String),
}

/// A connection to a remote node
pub struct Connection {
    /// Remote node ID
    pub node_id: String,
    /// Remote address
    pub address: SocketAddr,
    /// Connection state
    pub state: ConnectionState,
    /// Last activity timestamp
    pub last_activity: Instant,
    /// Send queue
    send_queue: VecDeque<IpcMessage>,
    /// TCP stream (if TCP transport)
    tcp_stream: Option<TcpStream>,
    /// Unix stream (if Unix transport)
    #[cfg(unix)]
    unix_stream: Option<UnixStream>,
}

impl Connection {
    /// Create a new connection
    pub fn new(node_id: String, address: SocketAddr) -> Self {
        Connection {
            node_id,
            address,
            state: ConnectionState::Disconnected,
            last_activity: Instant::now(),
            send_queue: VecDeque::new(),
            tcp_stream: None,
            #[cfg(unix)]
            unix_stream: None,
        }
    }

    /// Connect using TCP
    pub fn connect_tcp(&mut self, timeout: Duration) -> io::Result<()> {
        self.state = ConnectionState::Connecting;
        match TcpStream::connect_timeout(&self.address, timeout) {
            Ok(stream) => {
                stream.set_nodelay(true)?;
                self.tcp_stream = Some(stream);
                self.state = ConnectionState::Connected;
                self.last_activity = Instant::now();
                Ok(())
            }
            Err(e) => {
                self.state = ConnectionState::Failed(e.to_string());
                Err(e)
            }
        }
    }

    /// Check if connected
    pub fn is_connected(&self) -> bool {
        self.state == ConnectionState::Connected
    }

    /// Queue a message for sending
    pub fn queue_message(&mut self, msg: IpcMessage) {
        self.send_queue.push_back(msg);
    }

    /// Flush send queue
    pub fn flush(&mut self, format: SerializationFormat) -> io::Result<usize> {
        let mut sent = 0;
        while let Some(msg) = self.send_queue.pop_front() {
            self.send_message(&msg, format)?;
            sent += 1;
        }
        self.last_activity = Instant::now();
        Ok(sent)
    }

    /// Send a single message
    pub fn send_message(&mut self, msg: &IpcMessage, format: SerializationFormat) -> io::Result<()> {
        let data = serialize_message(msg, format)?;

        if let Some(ref mut stream) = self.tcp_stream {
            // Write length prefix (4 bytes, big-endian)
            let len = data.len() as u32;
            stream.write_all(&len.to_be_bytes())?;
            stream.write_all(&data)?;
            stream.flush()?;
        }

        #[cfg(unix)]
        if let Some(ref mut stream) = self.unix_stream {
            let len = data.len() as u32;
            stream.write_all(&len.to_be_bytes())?;
            stream.write_all(&data)?;
            stream.flush()?;
        }

        self.last_activity = Instant::now();
        Ok(())
    }

    /// Receive a message
    pub fn receive_message(&mut self, format: SerializationFormat) -> io::Result<Option<IpcMessage>> {
        if let Some(ref mut stream) = self.tcp_stream {
            // Read length prefix
            let mut len_buf = [0u8; 4];
            match stream.read_exact(&mut len_buf) {
                Ok(_) => {}
                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => return Ok(None),
                Err(e) => return Err(e),
            }

            let len = u32::from_be_bytes(len_buf) as usize;
            let mut data = vec![0u8; len];
            stream.read_exact(&mut data)?;

            let msg = deserialize_message(&data, format)?;
            self.last_activity = Instant::now();
            return Ok(Some(msg));
        }

        #[cfg(unix)]
        if let Some(ref mut stream) = self.unix_stream {
            let mut len_buf = [0u8; 4];
            match stream.read_exact(&mut len_buf) {
                Ok(_) => {}
                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => return Ok(None),
                Err(e) => return Err(e),
            }

            let len = u32::from_be_bytes(len_buf) as usize;
            let mut data = vec![0u8; len];
            stream.read_exact(&mut data)?;

            let msg = deserialize_message(&data, format)?;
            self.last_activity = Instant::now();
            return Ok(Some(msg));
        }

        Ok(None)
    }
}

/// Message handler callback type
pub type MessageHandler = Box<dyn Fn(&IpcMessage, &str) -> Option<IpcMessage> + Send + Sync>;

/// A communication node
pub struct Node {
    /// Node configuration
    config: NodeConfig,
    /// Active connections indexed by node ID
    connections: Arc<RwLock<HashMap<String, Connection>>>,
    /// Known nodes (node_id -> address)
    known_nodes: Arc<RwLock<HashMap<String, SocketAddr>>>,
    /// Incoming message queue
    inbox: Arc<Mutex<VecDeque<(String, IpcMessage)>>>,
    /// Message handlers
    handlers: Arc<RwLock<Vec<MessageHandler>>>,
    /// Running flag
    running: Arc<Mutex<bool>>,
    /// Listener thread handle
    listener_handle: Option<JoinHandle<()>>,
    /// Actual bound address
    bound_address: Arc<Mutex<Option<SocketAddr>>>,
    /// Statistics
    stats: Arc<Mutex<NodeStats>>,
}

/// Node statistics
#[derive(Clone, Debug, Default)]
pub struct NodeStats {
    pub messages_sent: u64,
    pub messages_received: u64,
    pub bytes_sent: u64,
    pub bytes_received: u64,
    pub connections_established: u64,
    pub connections_failed: u64,
    pub errors: u64,
}

impl Node {
    /// Create a new node
    pub fn new(config: NodeConfig) -> Self {
        Node {
            config,
            connections: Arc::new(RwLock::new(HashMap::new())),
            known_nodes: Arc::new(RwLock::new(HashMap::new())),
            inbox: Arc::new(Mutex::new(VecDeque::new())),
            handlers: Arc::new(RwLock::new(Vec::new())),
            running: Arc::new(Mutex::new(false)),
            listener_handle: None,
            bound_address: Arc::new(Mutex::new(None)),
            stats: Arc::new(Mutex::new(NodeStats::default())),
        }
    }

    /// Get node ID
    pub fn node_id(&self) -> &str {
        &self.config.node_id
    }

    /// Get bound address
    pub fn address(&self) -> Option<SocketAddr> {
        *self.bound_address.lock().unwrap()
    }

    /// Register a message handler
    pub fn on_message<F>(&self, handler: F)
    where
        F: Fn(&IpcMessage, &str) -> Option<IpcMessage> + Send + Sync + 'static,
    {
        let mut handlers = self.handlers.write().unwrap();
        handlers.push(Box::new(handler));
    }

    /// Start the node (begin listening)
    pub fn start(&mut self) -> io::Result<()> {
        *self.running.lock().unwrap() = true;

        match self.config.transport {
            Transport::Tcp => self.start_tcp_listener(),
            Transport::Udp => self.start_udp_listener(),
            #[cfg(unix)]
            Transport::Unix => self.start_unix_listener(),
            #[cfg(not(unix))]
            Transport::Unix => Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "Unix sockets not supported on this platform",
            )),
            Transport::InMemory => Ok(()), // No listener needed
        }
    }

    /// Start TCP listener
    fn start_tcp_listener(&mut self) -> io::Result<()> {
        let listener = TcpListener::bind(self.config.bind_address)?;
        let actual_addr = listener.local_addr()?;
        *self.bound_address.lock().unwrap() = Some(actual_addr);

        listener.set_nonblocking(true)?;

        let running = self.running.clone();
        let inbox = self.inbox.clone();
        let handlers = self.handlers.clone();
        let stats = self.stats.clone();
        let format = self.config.format;
        let node_id = self.config.node_id.clone();

        let handle = thread::spawn(move || {
            while *running.lock().unwrap() {
                match listener.accept() {
                    Ok((stream, addr)) => {
                        let inbox = inbox.clone();
                        let handlers = handlers.clone();
                        let stats = stats.clone();
                        let node_id = node_id.clone();

                        // Handle connection in a new thread
                        thread::spawn(move || {
                            handle_tcp_connection(
                                stream, addr, inbox, handlers, stats, format, &node_id,
                            );
                        });
                    }
                    Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                        thread::sleep(Duration::from_millis(10));
                    }
                    Err(e) => {
                        eprintln!("Accept error: {}", e);
                    }
                }
            }
        });

        self.listener_handle = Some(handle);
        Ok(())
    }

    /// Start UDP listener
    fn start_udp_listener(&mut self) -> io::Result<()> {
        let socket = UdpSocket::bind(self.config.bind_address)?;
        let actual_addr = socket.local_addr()?;
        *self.bound_address.lock().unwrap() = Some(actual_addr);

        socket.set_nonblocking(true)?;

        let running = self.running.clone();
        let inbox = self.inbox.clone();
        let handlers = self.handlers.clone();
        let stats = self.stats.clone();
        let format = self.config.format;

        let handle = thread::spawn(move || {
            let mut buf = vec![0u8; 65536];
            while *running.lock().unwrap() {
                match socket.recv_from(&mut buf) {
                    Ok((len, addr)) => {
                        if let Ok(msg) = deserialize_message(&buf[..len], format) {
                            let sender = addr.to_string();

                            // Call handlers
                            let handlers_guard = handlers.read().unwrap();
                            for handler in handlers_guard.iter() {
                                if let Some(response) = handler(&msg, &sender) {
                                    if let Ok(data) = serialize_message(&response, format) {
                                        let _ = socket.send_to(&data, addr);
                                    }
                                }
                            }

                            // Queue message
                            inbox.lock().unwrap().push_back((sender, msg));
                            stats.lock().unwrap().messages_received += 1;
                        }
                    }
                    Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                        thread::sleep(Duration::from_millis(10));
                    }
                    Err(_) => {}
                }
            }
        });

        self.listener_handle = Some(handle);
        Ok(())
    }

    /// Start Unix socket listener
    #[cfg(unix)]
    fn start_unix_listener(&mut self) -> io::Result<()> {
        let path = format!("/tmp/cwm-{}.sock", self.config.node_id);
        let _ = std::fs::remove_file(&path); // Clean up old socket

        let listener = UnixListener::bind(&path)?;
        listener.set_nonblocking(true)?;

        let running = self.running.clone();
        let inbox = self.inbox.clone();
        let handlers = self.handlers.clone();
        let stats = self.stats.clone();
        let format = self.config.format;
        let node_id = self.config.node_id.clone();

        let handle = thread::spawn(move || {
            while *running.lock().unwrap() {
                match listener.accept() {
                    Ok((stream, _addr)) => {
                        let inbox = inbox.clone();
                        let handlers = handlers.clone();
                        let stats = stats.clone();
                        let node_id = node_id.clone();

                        thread::spawn(move || {
                            handle_unix_connection(stream, inbox, handlers, stats, format, &node_id);
                        });
                    }
                    Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                        thread::sleep(Duration::from_millis(10));
                    }
                    Err(_) => {}
                }
            }
            let _ = std::fs::remove_file(&path);
        });

        self.listener_handle = Some(handle);
        Ok(())
    }

    /// Stop the node
    pub fn stop(&mut self) {
        *self.running.lock().unwrap() = false;
        if let Some(handle) = self.listener_handle.take() {
            let _ = handle.join();
        }
    }

    /// Connect to a remote node
    pub fn connect(&self, node_id: &str, address: SocketAddr) -> io::Result<()> {
        let mut conn = Connection::new(node_id.to_string(), address);
        conn.connect_tcp(self.config.connect_timeout)?;

        self.connections.write().unwrap().insert(node_id.to_string(), conn);
        self.known_nodes.write().unwrap().insert(node_id.to_string(), address);
        self.stats.lock().unwrap().connections_established += 1;

        Ok(())
    }

    /// Send a message to a node
    pub fn send(&self, node_id: &str, msg: IpcMessage) -> io::Result<()> {
        let mut connections = self.connections.write().unwrap();

        if let Some(conn) = connections.get_mut(node_id) {
            conn.send_message(&msg, self.config.format)?;
            self.stats.lock().unwrap().messages_sent += 1;
            Ok(())
        } else {
            // Try to connect if we know the address
            let addr = self.known_nodes.read().unwrap().get(node_id).cloned();
            if let Some(address) = addr {
                drop(connections);
                self.connect(node_id, address)?;

                let mut connections = self.connections.write().unwrap();
                if let Some(conn) = connections.get_mut(node_id) {
                    conn.send_message(&msg, self.config.format)?;
                    self.stats.lock().unwrap().messages_sent += 1;
                    return Ok(());
                }
            }

            Err(io::Error::new(
                io::ErrorKind::NotConnected,
                format!("Not connected to node: {}", node_id),
            ))
        }
    }

    /// Broadcast a message to all connected nodes
    pub fn broadcast(&self, msg: IpcMessage) -> io::Result<usize> {
        let mut connections = self.connections.write().unwrap();
        let mut sent = 0;

        for conn in connections.values_mut() {
            if conn.is_connected() {
                if conn.send_message(&msg, self.config.format).is_ok() {
                    sent += 1;
                }
            }
        }

        self.stats.lock().unwrap().messages_sent += sent as u64;
        Ok(sent)
    }

    /// Receive next message from inbox
    pub fn receive(&self) -> Option<(String, IpcMessage)> {
        self.inbox.lock().unwrap().pop_front()
    }

    /// Receive all pending messages
    pub fn receive_all(&self) -> Vec<(String, IpcMessage)> {
        let mut inbox = self.inbox.lock().unwrap();
        inbox.drain(..).collect()
    }

    /// Get statistics
    pub fn stats(&self) -> NodeStats {
        self.stats.lock().unwrap().clone()
    }

    /// Get connected node IDs
    pub fn connected_nodes(&self) -> Vec<String> {
        self.connections
            .read()
            .unwrap()
            .iter()
            .filter(|(_, c)| c.is_connected())
            .map(|(id, _)| id.clone())
            .collect()
    }

    /// Add a known node
    pub fn add_known_node(&self, node_id: &str, address: SocketAddr) {
        self.known_nodes.write().unwrap().insert(node_id.to_string(), address);
    }

    /// Send triples to a node
    pub fn send_triples(&self, node_id: &str, triples: Vec<Triple>) -> io::Result<()> {
        if triples.len() == 1 {
            self.send(node_id, IpcMessage::Triple(triples.into_iter().next().unwrap()))
        } else {
            self.send(node_id, IpcMessage::Triples(triples))
        }
    }

    /// Query a remote node
    pub fn query_remote(&self, node_id: &str, patterns: Vec<Triple>) -> io::Result<Vec<Bindings>> {
        let query_id = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64;

        self.send(node_id, IpcMessage::Query { query_id, patterns })?;

        // Wait for response (with timeout)
        let start = Instant::now();
        let timeout = self.config.io_timeout;

        loop {
            if start.elapsed() > timeout {
                return Err(io::Error::new(io::ErrorKind::TimedOut, "Query timeout"));
            }

            if let Some((sender, msg)) = self.receive() {
                // Check if this is our response
                let is_our_response = sender == node_id && matches!(
                    &msg,
                    IpcMessage::QueryResponse { query_id: qid, .. } if *qid == query_id
                );

                if is_our_response {
                    if let IpcMessage::QueryResponse { results, .. } = msg {
                        return Ok(results);
                    }
                }

                // Put back unrelated messages
                self.inbox.lock().unwrap().push_front((sender, msg));
            }

            thread::sleep(Duration::from_millis(1));
        }
    }
}

impl Drop for Node {
    fn drop(&mut self) {
        self.stop();
    }
}

/// Handle a TCP connection
fn handle_tcp_connection(
    mut stream: TcpStream,
    addr: SocketAddr,
    inbox: Arc<Mutex<VecDeque<(String, IpcMessage)>>>,
    handlers: Arc<RwLock<Vec<MessageHandler>>>,
    stats: Arc<Mutex<NodeStats>>,
    format: SerializationFormat,
    _node_id: &str,
) {
    let sender = addr.to_string();
    let _ = stream.set_read_timeout(Some(Duration::from_secs(30)));

    loop {
        // Read length prefix
        let mut len_buf = [0u8; 4];
        match stream.read_exact(&mut len_buf) {
            Ok(_) => {}
            Err(_) => break,
        }

        let len = u32::from_be_bytes(len_buf) as usize;
        if len > 16 * 1024 * 1024 {
            break; // Message too large
        }

        let mut data = vec![0u8; len];
        if stream.read_exact(&mut data).is_err() {
            break;
        }

        if let Ok(msg) = deserialize_message(&data, format) {
            // Call handlers and collect response
            let handlers_guard = handlers.read().unwrap();
            for handler in handlers_guard.iter() {
                if let Some(response) = handler(&msg, &sender) {
                    if let Ok(resp_data) = serialize_message(&response, format) {
                        let len = resp_data.len() as u32;
                        let _ = stream.write_all(&len.to_be_bytes());
                        let _ = stream.write_all(&resp_data);
                        let _ = stream.flush();
                    }
                }
            }
            drop(handlers_guard);

            // Queue message
            inbox.lock().unwrap().push_back((sender.clone(), msg));
            stats.lock().unwrap().messages_received += 1;
        }
    }
}

/// Handle a Unix socket connection
#[cfg(unix)]
fn handle_unix_connection(
    mut stream: UnixStream,
    inbox: Arc<Mutex<VecDeque<(String, IpcMessage)>>>,
    handlers: Arc<RwLock<Vec<MessageHandler>>>,
    stats: Arc<Mutex<NodeStats>>,
    format: SerializationFormat,
    node_id: &str,
) {
    let sender = format!("unix:{}", node_id);
    let _ = stream.set_read_timeout(Some(Duration::from_secs(30)));

    loop {
        let mut len_buf = [0u8; 4];
        match stream.read_exact(&mut len_buf) {
            Ok(_) => {}
            Err(_) => break,
        }

        let len = u32::from_be_bytes(len_buf) as usize;
        if len > 16 * 1024 * 1024 {
            break;
        }

        let mut data = vec![0u8; len];
        if stream.read_exact(&mut data).is_err() {
            break;
        }

        if let Ok(msg) = deserialize_message(&data, format) {
            let handlers_guard = handlers.read().unwrap();
            for handler in handlers_guard.iter() {
                if let Some(response) = handler(&msg, &sender) {
                    if let Ok(resp_data) = serialize_message(&response, format) {
                        let len = resp_data.len() as u32;
                        let _ = stream.write_all(&len.to_be_bytes());
                        let _ = stream.write_all(&resp_data);
                        let _ = stream.flush();
                    }
                }
            }
            drop(handlers_guard);

            inbox.lock().unwrap().push_back((sender.clone(), msg));
            stats.lock().unwrap().messages_received += 1;
        }
    }
}

/// Serialize a message
fn serialize_message(msg: &IpcMessage, format: SerializationFormat) -> io::Result<Vec<u8>> {
    match format {
        SerializationFormat::Json => serialize_json(msg),
        SerializationFormat::Binary => serialize_binary(msg),
    }
}

/// Deserialize a message
fn deserialize_message(data: &[u8], format: SerializationFormat) -> io::Result<IpcMessage> {
    match format {
        SerializationFormat::Json => deserialize_json(data),
        SerializationFormat::Binary => deserialize_binary(data),
    }
}

/// JSON serialization
fn serialize_json(msg: &IpcMessage) -> io::Result<Vec<u8>> {
    // Simple JSON serialization (in production, use serde_json)
    let json = match msg {
        IpcMessage::Ping { timestamp, node_id } => {
            format!(r#"{{"type":"ping","timestamp":{},"node_id":"{}"}}"#, timestamp, node_id)
        }
        IpcMessage::Pong { timestamp, node_id } => {
            format!(r#"{{"type":"pong","timestamp":{},"node_id":"{}"}}"#, timestamp, node_id)
        }
        IpcMessage::Shutdown => r#"{"type":"shutdown"}"#.to_string(),
        IpcMessage::Error { code, message } => {
            format!(r#"{{"type":"error","code":{},"message":"{}"}}"#, code, message)
        }
        _ => {
            // For complex messages, use debug format as placeholder
            format!(r#"{{"type":"complex","debug":"{:?}"}}"#, msg)
        }
    };
    Ok(json.into_bytes())
}

/// JSON deserialization
fn deserialize_json(data: &[u8]) -> io::Result<IpcMessage> {
    let s = std::str::from_utf8(data)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

    // Simple parsing (in production, use serde_json)
    if s.contains(r#""type":"ping""#) {
        // Extract fields manually
        if let Some(ts_start) = s.find(r#""timestamp":"#) {
            let ts_start = ts_start + 12;
            if let Some(ts_end) = s[ts_start..].find(',') {
                let timestamp: u64 = s[ts_start..ts_start + ts_end].parse().unwrap_or(0);
                if let Some(id_start) = s.find(r#""node_id":""#) {
                    let id_start = id_start + 11;
                    if let Some(id_end) = s[id_start..].find('"') {
                        let node_id = s[id_start..id_start + id_end].to_string();
                        return Ok(IpcMessage::Ping { timestamp, node_id });
                    }
                }
            }
        }
    }

    if s.contains(r#""type":"pong""#) {
        return Ok(IpcMessage::Pong {
            timestamp: 0,
            node_id: String::new(),
        });
    }

    if s.contains(r#""type":"shutdown""#) {
        return Ok(IpcMessage::Shutdown);
    }

    Err(io::Error::new(io::ErrorKind::InvalidData, "Unknown message type"))
}

/// Binary serialization
fn serialize_binary(msg: &IpcMessage) -> io::Result<Vec<u8>> {
    let mut buf = Vec::new();

    match msg {
        IpcMessage::Triple(triple) => {
            buf.push(0x01);
            write_term(&mut buf, &triple.subject)?;
            write_term(&mut buf, &triple.predicate)?;
            write_term(&mut buf, &triple.object)?;
        }
        IpcMessage::Triples(triples) => {
            buf.push(0x02);
            write_u32(&mut buf, triples.len() as u32)?;
            for triple in triples {
                write_term(&mut buf, &triple.subject)?;
                write_term(&mut buf, &triple.predicate)?;
                write_term(&mut buf, &triple.object)?;
            }
        }
        IpcMessage::Ping { timestamp, node_id } => {
            buf.push(0x10);
            write_u64(&mut buf, *timestamp)?;
            write_string(&mut buf, node_id)?;
        }
        IpcMessage::Pong { timestamp, node_id } => {
            buf.push(0x11);
            write_u64(&mut buf, *timestamp)?;
            write_string(&mut buf, node_id)?;
        }
        IpcMessage::Announce { node_id, address, capabilities } => {
            buf.push(0x20);
            write_string(&mut buf, node_id)?;
            write_string(&mut buf, address)?;
            write_u32(&mut buf, capabilities.len() as u32)?;
            for cap in capabilities {
                write_string(&mut buf, cap)?;
            }
        }
        IpcMessage::JoinRequest { node_id, address } => {
            buf.push(0x21);
            write_string(&mut buf, node_id)?;
            write_string(&mut buf, address)?;
        }
        IpcMessage::JoinAck { success, cluster_nodes } => {
            buf.push(0x22);
            buf.push(if *success { 1 } else { 0 });
            write_u32(&mut buf, cluster_nodes.len() as u32)?;
            for (id, addr) in cluster_nodes {
                write_string(&mut buf, id)?;
                write_string(&mut buf, addr)?;
            }
        }
        IpcMessage::Leave { node_id } => {
            buf.push(0x23);
            write_string(&mut buf, node_id)?;
        }
        IpcMessage::Query { query_id, patterns } => {
            buf.push(0x30);
            write_u64(&mut buf, *query_id)?;
            write_u32(&mut buf, patterns.len() as u32)?;
            for p in patterns {
                write_term(&mut buf, &p.subject)?;
                write_term(&mut buf, &p.predicate)?;
                write_term(&mut buf, &p.object)?;
            }
        }
        IpcMessage::Barrier { barrier_id, node_id } => {
            buf.push(0x40);
            write_u64(&mut buf, *barrier_id)?;
            write_string(&mut buf, node_id)?;
        }
        IpcMessage::BarrierRelease { barrier_id } => {
            buf.push(0x41);
            write_u64(&mut buf, *barrier_id)?;
        }
        IpcMessage::Vote { term, candidate_id } => {
            buf.push(0x50);
            write_u64(&mut buf, *term)?;
            write_string(&mut buf, candidate_id)?;
        }
        IpcMessage::VoteResponse { term, granted } => {
            buf.push(0x51);
            write_u64(&mut buf, *term)?;
            buf.push(if *granted { 1 } else { 0 });
        }
        IpcMessage::Error { code, message } => {
            buf.push(0xF0);
            write_u32(&mut buf, *code)?;
            write_string(&mut buf, message)?;
        }
        IpcMessage::Shutdown => {
            buf.push(0xFF);
        }
        _ => {
            // For other message types, use a placeholder
            buf.push(0x00);
        }
    }

    Ok(buf)
}

/// Binary deserialization
fn deserialize_binary(data: &[u8]) -> io::Result<IpcMessage> {
    if data.is_empty() {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "Empty message"));
    }

    let mut pos = 0;
    let msg_type = data[pos];
    pos += 1;

    match msg_type {
        0x01 => {
            let subject = read_term(data, &mut pos)?;
            let predicate = read_term(data, &mut pos)?;
            let object = read_term(data, &mut pos)?;
            Ok(IpcMessage::Triple(Triple { subject, predicate, object }))
        }
        0x02 => {
            let count = read_u32(data, &mut pos)?;
            let mut triples = Vec::with_capacity(count as usize);
            for _ in 0..count {
                let subject = read_term(data, &mut pos)?;
                let predicate = read_term(data, &mut pos)?;
                let object = read_term(data, &mut pos)?;
                triples.push(Triple { subject, predicate, object });
            }
            Ok(IpcMessage::Triples(triples))
        }
        0x10 => {
            let timestamp = read_u64(data, &mut pos)?;
            let node_id = read_string(data, &mut pos)?;
            Ok(IpcMessage::Ping { timestamp, node_id })
        }
        0x11 => {
            let timestamp = read_u64(data, &mut pos)?;
            let node_id = read_string(data, &mut pos)?;
            Ok(IpcMessage::Pong { timestamp, node_id })
        }
        0x20 => {
            let node_id = read_string(data, &mut pos)?;
            let address = read_string(data, &mut pos)?;
            let cap_count = read_u32(data, &mut pos)?;
            let mut capabilities = Vec::with_capacity(cap_count as usize);
            for _ in 0..cap_count {
                capabilities.push(read_string(data, &mut pos)?);
            }
            Ok(IpcMessage::Announce { node_id, address, capabilities })
        }
        0x21 => {
            let node_id = read_string(data, &mut pos)?;
            let address = read_string(data, &mut pos)?;
            Ok(IpcMessage::JoinRequest { node_id, address })
        }
        0x22 => {
            let success = data[pos] != 0;
            pos += 1;
            let count = read_u32(data, &mut pos)?;
            let mut cluster_nodes = Vec::with_capacity(count as usize);
            for _ in 0..count {
                let id = read_string(data, &mut pos)?;
                let addr = read_string(data, &mut pos)?;
                cluster_nodes.push((id, addr));
            }
            Ok(IpcMessage::JoinAck { success, cluster_nodes })
        }
        0x23 => {
            let node_id = read_string(data, &mut pos)?;
            Ok(IpcMessage::Leave { node_id })
        }
        0x30 => {
            let query_id = read_u64(data, &mut pos)?;
            let count = read_u32(data, &mut pos)?;
            let mut patterns = Vec::with_capacity(count as usize);
            for _ in 0..count {
                let subject = read_term(data, &mut pos)?;
                let predicate = read_term(data, &mut pos)?;
                let object = read_term(data, &mut pos)?;
                patterns.push(Triple { subject, predicate, object });
            }
            Ok(IpcMessage::Query { query_id, patterns })
        }
        0x40 => {
            let barrier_id = read_u64(data, &mut pos)?;
            let node_id = read_string(data, &mut pos)?;
            Ok(IpcMessage::Barrier { barrier_id, node_id })
        }
        0x41 => {
            let barrier_id = read_u64(data, &mut pos)?;
            Ok(IpcMessage::BarrierRelease { barrier_id })
        }
        0x50 => {
            let term = read_u64(data, &mut pos)?;
            let candidate_id = read_string(data, &mut pos)?;
            Ok(IpcMessage::Vote { term, candidate_id })
        }
        0x51 => {
            let term = read_u64(data, &mut pos)?;
            let granted = data[pos] != 0;
            Ok(IpcMessage::VoteResponse { term, granted })
        }
        0xF0 => {
            let code = read_u32(data, &mut pos)?;
            let message = read_string(data, &mut pos)?;
            Ok(IpcMessage::Error { code, message })
        }
        0xFF => Ok(IpcMessage::Shutdown),
        _ => Err(io::Error::new(io::ErrorKind::InvalidData, "Unknown message type")),
    }
}

// Helper functions for binary serialization

fn write_u32(buf: &mut Vec<u8>, val: u32) -> io::Result<()> {
    buf.extend_from_slice(&val.to_be_bytes());
    Ok(())
}

fn write_u64(buf: &mut Vec<u8>, val: u64) -> io::Result<()> {
    buf.extend_from_slice(&val.to_be_bytes());
    Ok(())
}

fn write_string(buf: &mut Vec<u8>, s: &str) -> io::Result<()> {
    let bytes = s.as_bytes();
    write_u32(buf, bytes.len() as u32)?;
    buf.extend_from_slice(bytes);
    Ok(())
}

fn write_term(buf: &mut Vec<u8>, term: &Term) -> io::Result<()> {
    match term {
        Term::Uri(uri) => {
            buf.push(0x01);
            write_string(buf, uri.as_str())?;
        }
        Term::Literal(lit) => {
            buf.push(0x02);
            write_string(buf, lit.value())?;
            if let Some(lang) = lit.language() {
                buf.push(0x01);
                write_string(buf, lang)?;
            } else if let Some(dt) = lit.datatype_uri() {
                buf.push(0x02);
                write_string(buf, dt)?;
            } else {
                buf.push(0x00);
            }
        }
        Term::BlankNode(bn) => {
            buf.push(0x03);
            write_string(buf, &bn.id().to_string())?;
        }
        Term::Variable(var) => {
            buf.push(0x04);
            write_string(buf, var.name())?;
            buf.push(if var.is_universal() { 1 } else { 0 });
        }
        _ => {
            buf.push(0x00);
        }
    }
    Ok(())
}

fn read_u32(data: &[u8], pos: &mut usize) -> io::Result<u32> {
    if *pos + 4 > data.len() {
        return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "Truncated u32"));
    }
    let val = u32::from_be_bytes([data[*pos], data[*pos + 1], data[*pos + 2], data[*pos + 3]]);
    *pos += 4;
    Ok(val)
}

fn read_u64(data: &[u8], pos: &mut usize) -> io::Result<u64> {
    if *pos + 8 > data.len() {
        return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "Truncated u64"));
    }
    let val = u64::from_be_bytes([
        data[*pos], data[*pos + 1], data[*pos + 2], data[*pos + 3],
        data[*pos + 4], data[*pos + 5], data[*pos + 6], data[*pos + 7],
    ]);
    *pos += 8;
    Ok(val)
}

fn read_string(data: &[u8], pos: &mut usize) -> io::Result<String> {
    let len = read_u32(data, pos)? as usize;
    if *pos + len > data.len() {
        return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "Truncated string"));
    }
    let s = std::str::from_utf8(&data[*pos..*pos + len])
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?
        .to_string();
    *pos += len;
    Ok(s)
}

fn read_term(data: &[u8], pos: &mut usize) -> io::Result<Term> {
    if *pos >= data.len() {
        return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "Truncated term"));
    }

    let term_type = data[*pos];
    *pos += 1;

    match term_type {
        0x01 => {
            let uri = read_string(data, pos)?;
            Ok(Term::uri(&uri))
        }
        0x02 => {
            let value = read_string(data, pos)?;
            let modifier = data[*pos];
            *pos += 1;
            match modifier {
                0x01 => {
                    let lang = read_string(data, pos)?;
                    Ok(Term::lang_literal(&value, &lang))
                }
                0x02 => {
                    let dt = read_string(data, pos)?;
                    Ok(Term::typed_literal(value, &dt))
                }
                _ => Ok(Term::literal(&value)),
            }
        }
        0x03 => {
            let id = read_string(data, pos)?;
            Ok(Term::blank(&id))
        }
        0x04 => {
            let name = read_string(data, pos)?;
            let is_universal = data[*pos] != 0;
            *pos += 1;
            if is_universal {
                Ok(Term::universal(&name))
            } else {
                Ok(Term::existential(&name))
            }
        }
        _ => Err(io::Error::new(io::ErrorKind::InvalidData, "Unknown term type")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_serialize_ping() {
        let msg = IpcMessage::Ping {
            timestamp: 12345,
            node_id: "test-node".to_string(),
        };

        let data = serialize_binary(&msg).unwrap();
        let decoded = deserialize_binary(&data).unwrap();

        if let IpcMessage::Ping { timestamp, node_id } = decoded {
            assert_eq!(timestamp, 12345);
            assert_eq!(node_id, "test-node");
        } else {
            panic!("Wrong message type");
        }
    }

    #[test]
    fn test_serialize_triple() {
        let triple = Triple::new(
            Term::uri("http://example.org/s"),
            Term::uri("http://example.org/p"),
            Term::literal("object"),
        );
        let msg = IpcMessage::Triple(triple);

        let data = serialize_binary(&msg).unwrap();
        let decoded = deserialize_binary(&data).unwrap();

        if let IpcMessage::Triple(t) = decoded {
            assert!(matches!(t.subject, Term::Uri(_)));
            assert!(matches!(t.predicate, Term::Uri(_)));
            assert!(matches!(t.object, Term::Literal(_)));
        } else {
            panic!("Wrong message type");
        }
    }

    #[test]
    fn test_node_config_default() {
        let config = NodeConfig::default();
        assert!(!config.node_id.is_empty());
        assert_eq!(config.transport, Transport::Tcp);
    }

    #[test]
    fn test_connection_state() {
        let conn = Connection::new("node-1".to_string(), "127.0.0.1:8000".parse().unwrap());
        assert!(!conn.is_connected());
        assert_eq!(conn.state, ConnectionState::Disconnected);
    }
}
