//! Configuration System for cwm-rust
//!
//! Provides a flexible configuration system supporting:
//! - TOML configuration files
//! - Environment variable overrides
//! - Reasoning profiles (rdfs, owl, shacl, etc.)
//! - Multiple config file locations
//!
//! # Configuration File Locations
//!
//! Configuration files are searched in order (first found wins):
//! 1. `./cwm.toml` - Project-local configuration
//! 2. `~/.config/cwm/config.toml` - User configuration (XDG)
//! 3. `~/.cwm/config.toml` - User configuration (legacy)
//! 4. `/etc/cwm/config.toml` - System-wide configuration
//!
//! # Environment Variables
//!
//! All configuration options can be overridden via environment variables:
//! - `CWM_PROFILE` - Reasoning profile (rdfs, owl, shacl, performance)
//! - `CWM_MAX_STEPS` - Maximum inference steps
//! - `CWM_LOG_LEVEL` - Logging verbosity (quiet, normal, verbose, debug)
//! - `CWM_FORMAT` - Default output format (n3, ntriples, rdf, jsonld)
//! - `CWM_BASE_URI` - Default base URI
//! - `CWM_STORE` - Store backend (memory, fuseki, duckdb)
//! - `CWM_FUSEKI_URL` - Fuseki endpoint URL
//! - `CWM_SERVER_PORT` - Default SPARQL server port
//! - `CWM_ENABLE_CRYPTO` - Enable crypto builtins (true/false)
//!
//! # Example Configuration
//!
//! ```toml
//! # cwm.toml
//!
//! [general]
//! base_uri = "http://example.org/"
//! format = "n3"
//! log_level = "normal"
//!
//! [reasoning]
//! profile = "rdfs"
//! max_steps = 10000
//! think_passes = 1
//! enable_proof = false
//!
//! [server]
//! port = 8080
//! host = "0.0.0.0"
//! cors_enabled = true
//!
//! [store]
//! backend = "memory"
//!
//! [prefixes]
//! ex = "http://example.org/"
//! foaf = "http://xmlns.com/foaf/0.1/"
//! ```

use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::PathBuf;

use serde::{Deserialize, Serialize};

// ============================================================================
// Configuration Schema
// ============================================================================

/// Main configuration structure
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(default)]
pub struct CwmConfig {
    /// General settings
    pub general: GeneralConfig,
    /// Reasoning settings
    pub reasoning: ReasoningConfig,
    /// HTTP server settings
    pub server: ServerConfig,
    /// Store backend settings
    pub store: StoreConfig,
    /// Security settings
    pub security: SecurityConfig,
    /// Custom prefix definitions
    pub prefixes: HashMap<String, String>,
    /// Profile-specific overrides
    pub profiles: HashMap<String, ProfileConfig>,
}

/// General configuration options
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct GeneralConfig {
    /// Default base URI for relative references
    pub base_uri: Option<String>,
    /// Default output format (n3, ntriples, rdf, jsonld)
    pub format: OutputFormat,
    /// Logging level
    pub log_level: LogLevel,
    /// Enable colored output
    pub color: bool,
}

impl Default for GeneralConfig {
    fn default() -> Self {
        Self {
            base_uri: None,
            format: OutputFormat::N3,
            log_level: LogLevel::Normal,
            color: true,
        }
    }
}

/// Reasoning configuration options
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct ReasoningConfig {
    /// Reasoning profile to use
    pub profile: ReasoningProfile,
    /// Maximum number of inference steps
    pub max_steps: usize,
    /// Number of reasoning passes
    pub think_passes: usize,
    /// Enable proof generation
    pub enable_proof: bool,
    /// Theorem prover engine (for --engine flag)
    pub engine: Option<String>,
    /// Enable tabling/memoization
    pub enable_tabling: bool,
    /// Enable stratification analysis
    pub enable_stratification: bool,
}

impl Default for ReasoningConfig {
    fn default() -> Self {
        Self {
            profile: ReasoningProfile::Default,
            max_steps: 10000,
            think_passes: 1,
            enable_proof: false,
            engine: None,
            enable_tabling: true,
            enable_stratification: true,
        }
    }
}

/// HTTP server configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct ServerConfig {
    /// Server port
    pub port: u16,
    /// Server host
    pub host: String,
    /// Enable CORS
    pub cors_enabled: bool,
    /// Maximum request body size (bytes)
    pub max_body_size: usize,
    /// Request timeout (seconds)
    pub timeout_secs: u64,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            port: 8080,
            host: "0.0.0.0".to_string(),
            cors_enabled: true,
            max_body_size: 10 * 1024 * 1024, // 10 MB
            timeout_secs: 30,
        }
    }
}

/// Store backend configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct StoreConfig {
    /// Backend type
    pub backend: StoreBackend,
    /// Fuseki endpoint URL (for fuseki backend)
    pub fuseki_url: Option<String>,
    /// Fuseki graph name
    pub fuseki_graph: Option<String>,
    /// Database path (for duckdb/sqlite backends)
    pub database_path: Option<String>,
    /// Connection pool size
    pub pool_size: usize,
}

impl Default for StoreConfig {
    fn default() -> Self {
        Self {
            backend: StoreBackend::Memory,
            fuseki_url: None,
            fuseki_graph: None,
            database_path: None,
            pool_size: 4,
        }
    }
}

/// Security configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct SecurityConfig {
    /// Enable crypto builtins
    pub enable_crypto: bool,
    /// Enable file system access (os:readFile, os:writeFile)
    pub enable_filesystem: bool,
    /// Enable network access (HTTP fetching)
    pub enable_network: bool,
    /// Allowed domains for HTTP fetch (empty = all allowed)
    pub allowed_domains: Vec<String>,
    /// Maximum HTTP timeout (seconds)
    pub http_timeout_secs: u64,
}

impl Default for SecurityConfig {
    fn default() -> Self {
        Self {
            enable_crypto: false,
            enable_filesystem: false,
            enable_network: true,
            allowed_domains: Vec::new(),
            http_timeout_secs: 30,
        }
    }
}

/// Profile-specific configuration overrides
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(default)]
pub struct ProfileConfig {
    /// Override max_steps
    pub max_steps: Option<usize>,
    /// Override think_passes
    pub think_passes: Option<usize>,
    /// Override engine
    pub engine: Option<String>,
    /// Additional prefixes for this profile
    pub prefixes: HashMap<String, String>,
    /// Description of the profile
    pub description: Option<String>,
}

// ============================================================================
// Enums
// ============================================================================

/// Output format options
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum OutputFormat {
    #[default]
    N3,
    NTriples,
    Rdf,
    JsonLd,
    Debug,
}

impl OutputFormat {
    pub fn as_str(&self) -> &'static str {
        match self {
            OutputFormat::N3 => "n3",
            OutputFormat::NTriples => "ntriples",
            OutputFormat::Rdf => "rdf",
            OutputFormat::JsonLd => "jsonld",
            OutputFormat::Debug => "debug",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "n3" | "notation3" => Some(OutputFormat::N3),
            "nt" | "ntriples" | "n-triples" => Some(OutputFormat::NTriples),
            "rdf" | "rdfxml" | "rdf/xml" => Some(OutputFormat::Rdf),
            "jsonld" | "json-ld" => Some(OutputFormat::JsonLd),
            "debug" => Some(OutputFormat::Debug),
            _ => None,
        }
    }
}

/// Log level options
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum LogLevel {
    Quiet,
    #[default]
    Normal,
    Verbose,
    Debug,
}

impl LogLevel {
    pub fn as_str(&self) -> &'static str {
        match self {
            LogLevel::Quiet => "quiet",
            LogLevel::Normal => "normal",
            LogLevel::Verbose => "verbose",
            LogLevel::Debug => "debug",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "quiet" | "q" | "0" => Some(LogLevel::Quiet),
            "normal" | "n" | "1" => Some(LogLevel::Normal),
            "verbose" | "v" | "2" => Some(LogLevel::Verbose),
            "debug" | "d" | "3" => Some(LogLevel::Debug),
            _ => None,
        }
    }
}

/// Reasoning profile presets
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum ReasoningProfile {
    /// Default forward-chaining
    #[default]
    Default,
    /// RDFS entailment rules
    Rdfs,
    /// OWL 2 DL reasoning
    Owl,
    /// SHACL validation mode
    Shacl,
    /// Performance-optimized (fewer checks)
    Performance,
    /// Completeness-focused (more thorough)
    Complete,
    /// Custom profile (use profiles section)
    Custom,
}

impl ReasoningProfile {
    pub fn as_str(&self) -> &'static str {
        match self {
            ReasoningProfile::Default => "default",
            ReasoningProfile::Rdfs => "rdfs",
            ReasoningProfile::Owl => "owl",
            ReasoningProfile::Shacl => "shacl",
            ReasoningProfile::Performance => "performance",
            ReasoningProfile::Complete => "complete",
            ReasoningProfile::Custom => "custom",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "default" | "" => Some(ReasoningProfile::Default),
            "rdfs" => Some(ReasoningProfile::Rdfs),
            "owl" | "owl2" | "owl-dl" => Some(ReasoningProfile::Owl),
            "shacl" => Some(ReasoningProfile::Shacl),
            "performance" | "perf" | "fast" => Some(ReasoningProfile::Performance),
            "complete" | "thorough" | "full" => Some(ReasoningProfile::Complete),
            "custom" => Some(ReasoningProfile::Custom),
            _ => None,
        }
    }

    /// Get the recommended max_steps for this profile
    pub fn default_max_steps(&self) -> usize {
        match self {
            ReasoningProfile::Default => 10000,
            ReasoningProfile::Rdfs => 50000,
            ReasoningProfile::Owl => 100000,
            ReasoningProfile::Shacl => 10000,
            ReasoningProfile::Performance => 1000,
            ReasoningProfile::Complete => 0, // unlimited
            ReasoningProfile::Custom => 10000,
        }
    }

    /// Get a description of this profile
    pub fn description(&self) -> &'static str {
        match self {
            ReasoningProfile::Default => "Standard forward-chaining inference",
            ReasoningProfile::Rdfs => "RDFS entailment (subclass, subproperty, domain, range)",
            ReasoningProfile::Owl => "OWL 2 DL reasoning with description logic",
            ReasoningProfile::Shacl => "SHACL constraint validation mode",
            ReasoningProfile::Performance => "Optimized for speed with limited inference",
            ReasoningProfile::Complete => "Maximum inference completeness",
            ReasoningProfile::Custom => "User-defined profile",
        }
    }
}

/// Store backend options
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum StoreBackend {
    /// In-memory store
    #[default]
    Memory,
    /// Apache Jena Fuseki
    Fuseki,
    /// DuckDB (future)
    DuckDb,
    /// SQLite
    Sqlite,
}

impl StoreBackend {
    pub fn as_str(&self) -> &'static str {
        match self {
            StoreBackend::Memory => "memory",
            StoreBackend::Fuseki => "fuseki",
            StoreBackend::DuckDb => "duckdb",
            StoreBackend::Sqlite => "sqlite",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "memory" | "mem" | "inmemory" => Some(StoreBackend::Memory),
            "fuseki" | "jena" => Some(StoreBackend::Fuseki),
            "duckdb" | "duck" => Some(StoreBackend::DuckDb),
            "sqlite" | "sql" => Some(StoreBackend::Sqlite),
            _ => None,
        }
    }
}

// ============================================================================
// Configuration Loading
// ============================================================================

impl CwmConfig {
    /// Create a new default configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Load configuration from default locations
    ///
    /// Searches for config files in order:
    /// 1. ./cwm.toml
    /// 2. ~/.config/cwm/config.toml
    /// 3. ~/.cwm/config.toml
    /// 4. /etc/cwm/config.toml
    ///
    /// Then applies environment variable overrides.
    pub fn load() -> Result<Self, ConfigError> {
        let mut config = Self::default();

        // Try to load from config file locations
        for path in Self::config_paths() {
            if path.exists() {
                config = Self::load_from_file(&path)?;
                break;
            }
        }

        // Apply environment variable overrides
        config.apply_env_overrides();

        Ok(config)
    }

    /// Load configuration from a specific file
    pub fn load_from_file(path: &PathBuf) -> Result<Self, ConfigError> {
        let content = fs::read_to_string(path)
            .map_err(|e| ConfigError::IoError(path.clone(), e.to_string()))?;

        let config: CwmConfig = toml::from_str(&content)
            .map_err(|e| ConfigError::ParseError(path.clone(), e.to_string()))?;

        Ok(config)
    }

    /// Load configuration from a TOML string
    pub fn load_from_str(content: &str) -> Result<Self, ConfigError> {
        toml::from_str(content)
            .map_err(|e| ConfigError::ParseError(PathBuf::from("<string>"), e.to_string()))
    }

    /// Get the list of config file search paths
    pub fn config_paths() -> Vec<PathBuf> {
        let mut paths = Vec::new();

        // Project-local
        paths.push(PathBuf::from("./cwm.toml"));

        // XDG config directory
        if let Some(config_dir) = dirs::config_dir() {
            paths.push(config_dir.join("cwm").join("config.toml"));
        }

        // Legacy home directory
        if let Some(home_dir) = dirs::home_dir() {
            paths.push(home_dir.join(".cwm").join("config.toml"));
        }

        // System-wide (Unix only)
        #[cfg(unix)]
        paths.push(PathBuf::from("/etc/cwm/config.toml"));

        paths
    }

    /// Apply environment variable overrides
    pub fn apply_env_overrides(&mut self) {
        // CWM_PROFILE
        if let Ok(val) = env::var("CWM_PROFILE") {
            if let Some(profile) = ReasoningProfile::from_str(&val) {
                self.reasoning.profile = profile;
            }
        }

        // CWM_MAX_STEPS
        if let Ok(val) = env::var("CWM_MAX_STEPS") {
            if let Ok(steps) = val.parse::<usize>() {
                self.reasoning.max_steps = steps;
            }
        }

        // CWM_LOG_LEVEL
        if let Ok(val) = env::var("CWM_LOG_LEVEL") {
            if let Some(level) = LogLevel::from_str(&val) {
                self.general.log_level = level;
            }
        }

        // CWM_FORMAT
        if let Ok(val) = env::var("CWM_FORMAT") {
            if let Some(format) = OutputFormat::from_str(&val) {
                self.general.format = format;
            }
        }

        // CWM_BASE_URI
        if let Ok(val) = env::var("CWM_BASE_URI") {
            self.general.base_uri = Some(val);
        }

        // CWM_STORE
        if let Ok(val) = env::var("CWM_STORE") {
            if let Some(backend) = StoreBackend::from_str(&val) {
                self.store.backend = backend;
            }
        }

        // CWM_FUSEKI_URL
        if let Ok(val) = env::var("CWM_FUSEKI_URL") {
            self.store.fuseki_url = Some(val);
        }

        // CWM_SERVER_PORT
        if let Ok(val) = env::var("CWM_SERVER_PORT") {
            if let Ok(port) = val.parse::<u16>() {
                self.server.port = port;
            }
        }

        // CWM_ENABLE_CRYPTO
        if let Ok(val) = env::var("CWM_ENABLE_CRYPTO") {
            self.security.enable_crypto = val == "true" || val == "1" || val == "yes";
        }

        // CWM_ENABLE_FILESYSTEM
        if let Ok(val) = env::var("CWM_ENABLE_FILESYSTEM") {
            self.security.enable_filesystem = val == "true" || val == "1" || val == "yes";
        }

        // CWM_ENABLE_NETWORK
        if let Ok(val) = env::var("CWM_ENABLE_NETWORK") {
            self.security.enable_network = val == "true" || val == "1" || val == "yes";
        }

        // CWM_ENGINE
        if let Ok(val) = env::var("CWM_ENGINE") {
            self.reasoning.engine = Some(val);
        }
    }

    /// Apply a named profile's settings
    pub fn apply_profile(&mut self, name: &str) -> Result<(), ConfigError> {
        // First check built-in profiles
        if let Some(profile) = ReasoningProfile::from_str(name) {
            self.reasoning.profile = profile;
            self.reasoning.max_steps = profile.default_max_steps();

            // Apply profile-specific settings
            match profile {
                ReasoningProfile::Rdfs => {
                    // RDFS needs subclass/subproperty reasoning
                    self.reasoning.think_passes = 2;
                }
                ReasoningProfile::Owl => {
                    // OWL needs DL-tableau
                    self.reasoning.engine = Some("dl-tableau".to_string());
                    self.reasoning.think_passes = 3;
                }
                ReasoningProfile::Performance => {
                    // Performance mode: minimal inference
                    self.reasoning.enable_tabling = false;
                    self.reasoning.enable_stratification = false;
                    self.reasoning.think_passes = 1;
                }
                ReasoningProfile::Complete => {
                    // Complete mode: maximum inference
                    self.reasoning.enable_tabling = true;
                    self.reasoning.enable_stratification = true;
                    self.reasoning.think_passes = 5;
                }
                _ => {}
            }
            return Ok(());
        }

        // Check custom profiles
        if let Some(custom) = self.profiles.get(name).cloned() {
            if let Some(max_steps) = custom.max_steps {
                self.reasoning.max_steps = max_steps;
            }
            if let Some(think_passes) = custom.think_passes {
                self.reasoning.think_passes = think_passes;
            }
            if let Some(engine) = custom.engine {
                self.reasoning.engine = Some(engine);
            }
            for (prefix, uri) in custom.prefixes {
                self.prefixes.insert(prefix, uri);
            }
            return Ok(());
        }

        Err(ConfigError::UnknownProfile(name.to_string()))
    }

    /// Serialize configuration to TOML string
    pub fn to_toml(&self) -> Result<String, ConfigError> {
        toml::to_string_pretty(self)
            .map_err(|e| ConfigError::SerializeError(e.to_string()))
    }

    /// Write configuration to a file
    pub fn save_to_file(&self, path: &PathBuf) -> Result<(), ConfigError> {
        let content = self.to_toml()?;
        fs::write(path, content)
            .map_err(|e| ConfigError::IoError(path.clone(), e.to_string()))
    }

    /// Generate a default configuration file content
    pub fn default_config_content() -> &'static str {
        r#"# CWM Configuration File
# See documentation for all available options

[general]
# Default output format: n3, ntriples, rdf, jsonld
format = "n3"
# Logging level: quiet, normal, verbose, debug
log_level = "normal"
# Enable colored output
color = true
# Default base URI (optional)
# base_uri = "http://example.org/"

[reasoning]
# Reasoning profile: default, rdfs, owl, shacl, performance, complete
profile = "default"
# Maximum inference steps (0 = unlimited)
max_steps = 10000
# Number of reasoning passes
think_passes = 1
# Enable proof generation (for --why flag)
enable_proof = false
# Theorem prover engine (optional): otter, prover9, tableau, etc.
# engine = "otter"
# Enable tabling/memoization for cycle prevention
enable_tabling = true
# Enable stratification analysis for negation
enable_stratification = true

[server]
# SPARQL server port
port = 8080
# Server host
host = "0.0.0.0"
# Enable CORS for browser access
cors_enabled = true
# Maximum request body size (bytes)
max_body_size = 10485760
# Request timeout (seconds)
timeout_secs = 30

[store]
# Backend: memory, fuseki, duckdb, sqlite
backend = "memory"
# Fuseki endpoint URL (for fuseki backend)
# fuseki_url = "http://localhost:3030/dataset"
# Fuseki graph name (optional)
# fuseki_graph = "http://example.org/graph"
# Database path (for duckdb/sqlite backends)
# database_path = "./cwm.db"
# Connection pool size
pool_size = 4

[security]
# Enable crypto builtins (sha256, md5, etc.)
enable_crypto = false
# Enable file system access (os:readFile, os:writeFile)
enable_filesystem = false
# Enable network access (HTTP fetching)
enable_network = true
# Allowed domains for HTTP fetch (empty = all allowed)
allowed_domains = []
# Maximum HTTP timeout (seconds)
http_timeout_secs = 30

[prefixes]
# Custom prefix definitions
# ex = "http://example.org/"
# foaf = "http://xmlns.com/foaf/0.1/"
# dc = "http://purl.org/dc/elements/1.1/"

# Custom profiles can be defined like this:
# [profiles.myprofile]
# max_steps = 50000
# think_passes = 3
# engine = "otter"
# description = "My custom reasoning profile"
# [profiles.myprofile.prefixes]
# custom = "http://example.org/custom#"
"#
    }

    /// List all available profiles
    pub fn available_profiles(&self) -> Vec<(&str, &str)> {
        let mut profiles = vec![
            ("default", ReasoningProfile::Default.description()),
            ("rdfs", ReasoningProfile::Rdfs.description()),
            ("owl", ReasoningProfile::Owl.description()),
            ("shacl", ReasoningProfile::Shacl.description()),
            ("performance", ReasoningProfile::Performance.description()),
            ("complete", ReasoningProfile::Complete.description()),
        ];

        // Add custom profiles
        for (name, config) in &self.profiles {
            let desc = config.description.as_deref().unwrap_or("Custom profile");
            profiles.push((name.as_str(), desc));
        }

        profiles
    }
}

// ============================================================================
// Error Types
// ============================================================================

/// Configuration errors
#[derive(Debug, Clone)]
pub enum ConfigError {
    /// IO error reading/writing config file
    IoError(PathBuf, String),
    /// Parse error in config file
    ParseError(PathBuf, String),
    /// Serialization error
    SerializeError(String),
    /// Unknown profile name
    UnknownProfile(String),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigError::IoError(path, msg) => {
                write!(f, "IO error reading {}: {}", path.display(), msg)
            }
            ConfigError::ParseError(path, msg) => {
                write!(f, "Parse error in {}: {}", path.display(), msg)
            }
            ConfigError::SerializeError(msg) => {
                write!(f, "Serialization error: {}", msg)
            }
            ConfigError::UnknownProfile(name) => {
                write!(f, "Unknown profile: {}", name)
            }
        }
    }
}

impl std::error::Error for ConfigError {}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = CwmConfig::new();
        assert_eq!(config.reasoning.max_steps, 10000);
        assert_eq!(config.server.port, 8080);
        assert_eq!(config.general.format, OutputFormat::N3);
    }

    #[test]
    fn test_parse_config() {
        let toml = r#"
            [general]
            format = "jsonld"
            log_level = "verbose"

            [reasoning]
            max_steps = 50000
            profile = "rdfs"

            [server]
            port = 9000
        "#;

        let config = CwmConfig::load_from_str(toml).unwrap();
        assert_eq!(config.general.format, OutputFormat::JsonLd);
        assert_eq!(config.general.log_level, LogLevel::Verbose);
        assert_eq!(config.reasoning.max_steps, 50000);
        assert_eq!(config.reasoning.profile, ReasoningProfile::Rdfs);
        assert_eq!(config.server.port, 9000);
    }

    #[test]
    fn test_profile_from_str() {
        assert_eq!(ReasoningProfile::from_str("rdfs"), Some(ReasoningProfile::Rdfs));
        assert_eq!(ReasoningProfile::from_str("owl"), Some(ReasoningProfile::Owl));
        assert_eq!(ReasoningProfile::from_str("performance"), Some(ReasoningProfile::Performance));
        assert_eq!(ReasoningProfile::from_str("unknown"), None);
    }

    #[test]
    fn test_output_format_from_str() {
        assert_eq!(OutputFormat::from_str("n3"), Some(OutputFormat::N3));
        assert_eq!(OutputFormat::from_str("jsonld"), Some(OutputFormat::JsonLd));
        assert_eq!(OutputFormat::from_str("ntriples"), Some(OutputFormat::NTriples));
    }

    #[test]
    fn test_log_level_from_str() {
        assert_eq!(LogLevel::from_str("quiet"), Some(LogLevel::Quiet));
        assert_eq!(LogLevel::from_str("verbose"), Some(LogLevel::Verbose));
        assert_eq!(LogLevel::from_str("debug"), Some(LogLevel::Debug));
    }

    #[test]
    fn test_apply_profile() {
        let mut config = CwmConfig::new();

        config.apply_profile("performance").unwrap();
        assert_eq!(config.reasoning.profile, ReasoningProfile::Performance);
        assert_eq!(config.reasoning.max_steps, 1000);
        assert_eq!(config.reasoning.enable_tabling, false);

        config.apply_profile("complete").unwrap();
        assert_eq!(config.reasoning.profile, ReasoningProfile::Complete);
        assert_eq!(config.reasoning.max_steps, 0); // unlimited
    }

    #[test]
    fn test_custom_profile() {
        let toml = r#"
            [profiles.myprofile]
            max_steps = 25000
            think_passes = 4
            engine = "tableau"
            description = "My custom profile"
        "#;

        let mut config = CwmConfig::load_from_str(toml).unwrap();
        config.apply_profile("myprofile").unwrap();

        assert_eq!(config.reasoning.max_steps, 25000);
        assert_eq!(config.reasoning.think_passes, 4);
        assert_eq!(config.reasoning.engine, Some("tableau".to_string()));
    }

    #[test]
    fn test_prefixes() {
        let toml = r#"
            [prefixes]
            ex = "http://example.org/"
            foaf = "http://xmlns.com/foaf/0.1/"
        "#;

        let config = CwmConfig::load_from_str(toml).unwrap();
        assert_eq!(config.prefixes.get("ex"), Some(&"http://example.org/".to_string()));
        assert_eq!(config.prefixes.get("foaf"), Some(&"http://xmlns.com/foaf/0.1/".to_string()));
    }

    #[test]
    fn test_serialize_config() {
        let config = CwmConfig::new();
        let toml = config.to_toml().unwrap();
        assert!(toml.contains("[general]"));
        assert!(toml.contains("[reasoning]"));
        assert!(toml.contains("[server]"));
    }

    #[test]
    fn test_config_paths() {
        let paths = CwmConfig::config_paths();
        assert!(!paths.is_empty());
        assert!(paths[0].ends_with("cwm.toml"));
    }

    #[test]
    fn test_unknown_profile_error() {
        let mut config = CwmConfig::new();
        let result = config.apply_profile("nonexistent");
        assert!(matches!(result, Err(ConfigError::UnknownProfile(_))));
    }
}
