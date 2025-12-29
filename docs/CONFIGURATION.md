# CWM-Rust Configuration Reference

Complete reference for configuration options, profiles, and environment variables.

## Table of Contents

- [Configuration Files](#configuration-files)
- [Configuration Options](#configuration-options)
- [Reasoning Profiles](#reasoning-profiles)
- [Environment Variables](#environment-variables)
- [Command-Line Override](#command-line-override)
- [Examples](#examples)

---

## Configuration Files

### File Locations

cwm-rust searches for configuration in this order (first found wins):

1. `./cwm.toml` - Project-local configuration
2. `~/.config/cwm/config.toml` - XDG user configuration
3. `~/.cwm/config.toml` - Legacy user configuration
4. `/etc/cwm/config.toml` - System-wide configuration

### Generating Default Config

```bash
cwm --init-config
# Creates ~/.config/cwm/config.toml with defaults
```

### Specifying Config File

```bash
cwm --config /path/to/custom.toml data.n3 --think
```

---

## Configuration Options

### Complete Configuration Template

```toml
# cwm.toml - CWM Configuration File

#=============================================================================
# General Settings
#=============================================================================
[general]
# Base URI for relative references
base_uri = "http://example.org/"

# Default output format: n3, ntriples, rdf, jsonld, debug
format = "n3"

# Logging level: quiet, normal, verbose, debug
log_level = "normal"

# Enable colored output
color = true

# Maximum input file size in bytes (0 = unlimited)
max_input_size = 104857600  # 100 MB

#=============================================================================
# Reasoning Settings
#=============================================================================
[reasoning]
# Reasoning profile: default, rdfs, owl, shacl, performance, complete, custom
profile = "default"

# Maximum inference steps (0 = unlimited)
max_steps = 10000

# Number of reasoning passes
think_passes = 1

# Theorem prover engine (optional)
# Options: resolution, otter, dpll, cdcl, tableau, leancop, nanocop,
#          superposition, knuth-bendix, smt, dl-tableau
# engine = "otter"

# Enable proof generation
enable_proof = false

# Enable tabling (memoization) for cycle prevention
enable_tabling = true

# Enable stratification analysis for negation
enable_stratification = true

# Enable rule prioritization
enable_prioritization = false

# Timeout per inference step in milliseconds (0 = no timeout)
step_timeout_ms = 0

#=============================================================================
# Server Settings
#=============================================================================
[server]
# Server port
port = 8080

# Server host (0.0.0.0 = all interfaces)
host = "0.0.0.0"

# Enable CORS headers
cors_enabled = true

# Maximum request body size in bytes
max_body_size = 10485760  # 10 MB

# Request timeout in seconds
timeout_secs = 30

# Enable request logging
access_log = true

# Allowed origins for CORS (empty = all)
cors_origins = []

#-----------------------------------------------------------------------------
# Query Cache Settings
#-----------------------------------------------------------------------------
[server.cache]
# Enable query result caching
enabled = true

# Maximum cached entries
max_entries = 1000

# Cache TTL in seconds
ttl_secs = 300

# Eviction policy: lru, lfu, fifo
eviction_policy = "lru"

#-----------------------------------------------------------------------------
# Rate Limiting
#-----------------------------------------------------------------------------
[server.rate_limit]
# Enable rate limiting
enabled = false

# Requests per second per IP
requests_per_second = 100

# Burst capacity
burst = 50

#=============================================================================
# Store Backend Settings
#=============================================================================
[store]
# Backend type: memory, sqlite, fuseki
backend = "memory"

# SQLite database path (for sqlite backend)
database_path = "./cwm.db"

# Fuseki SPARQL endpoint URL (for fuseki backend)
fuseki_url = "http://localhost:3030/dataset"

# Fuseki named graph URI (empty = default graph)
fuseki_graph = ""

# Fuseki connection timeout in seconds
fuseki_timeout = 30

# Fuseki batch size for bulk operations
fuseki_batch = 1000

# Connection pool size (for database backends)
pool_size = 4

#=============================================================================
# Security Settings
#=============================================================================
[security]
# Enable cryptographic builtins (crypto:*)
enable_crypto = false

# Enable filesystem builtins (os:readFile, os:writeFile, etc.)
enable_filesystem = false

# Enable network operations (log:semantics for remote URIs)
enable_network = true

# Allowed domains for network access (empty = all)
allowed_domains = []

# Blocked domains (takes precedence over allowed)
blocked_domains = []

# HTTP timeout for remote fetches in seconds
http_timeout_secs = 30

# Maximum HTTP response size in bytes
max_http_response_size = 10485760  # 10 MB

#=============================================================================
# Output Settings
#=============================================================================
[output]
# Include comments in output
comments = false

# Use compact output format
compact = false

# Sort output by subject
sort_by_subject = false

# Pretty print JSON output
pretty_json = true

# Include prefixes in output
include_prefixes = true

# Escape unicode characters
escape_unicode = false

#=============================================================================
# Custom Prefixes
#=============================================================================
[prefixes]
# Define custom namespace prefixes
# These are available in addition to standard prefixes (rdf, rdfs, owl, xsd, etc.)
ex = "http://example.org/"
foaf = "http://xmlns.com/foaf/0.1/"
schema = "http://schema.org/"
dc = "http://purl.org/dc/elements/1.1/"
dcterms = "http://purl.org/dc/terms/"
skos = "http://www.w3.org/2004/02/skos/core#"

#=============================================================================
# Profile Definitions
#=============================================================================
# Custom profile overrides (extends built-in profiles)

[profiles.myprofile]
description = "My custom reasoning profile"
max_steps = 25000
think_passes = 3
engine = "otter"
enable_tabling = true
enable_stratification = true

[profiles.myprofile.prefixes]
# Profile-specific prefixes
custom = "http://example.org/custom#"

[profiles.fast]
description = "Speed-optimized profile"
max_steps = 500
think_passes = 1
enable_tabling = false
enable_stratification = false
enable_proof = false

[profiles.thorough]
description = "Complete reasoning profile"
max_steps = 0  # unlimited
think_passes = 10
enable_tabling = true
enable_stratification = true
enable_proof = true
engine = "tableau"
```

---

## Reasoning Profiles

### Built-in Profiles

| Profile | max_steps | passes | Engine | Description |
|---------|-----------|--------|--------|-------------|
| `default` | 10,000 | 1 | - | Standard forward chaining |
| `rdfs` | 50,000 | 2 | - | RDFS entailment rules |
| `owl` | 100,000 | 3 | dl-tableau | OWL 2 RL reasoning |
| `shacl` | 10,000 | 1 | - | SHACL validation |
| `performance` | 1,000 | 1 | - | Speed optimized |
| `complete` | ∞ | 5 | - | Maximum completeness |

### Using Profiles

```bash
# Command line
cwm data.n3 --profile rdfs --think

# Config file
[reasoning]
profile = "rdfs"

# Environment variable
export CWM_PROFILE=rdfs
cwm data.n3 --think
```

### Custom Profiles

Define in config file:

```toml
[profiles.myprofile]
description = "My custom profile"
max_steps = 25000
think_passes = 3
engine = "superposition"
enable_tabling = true

[profiles.myprofile.prefixes]
custom = "http://example.org/custom#"
```

Use with:

```bash
cwm data.n3 --profile myprofile --think
```

---

## Environment Variables

All configuration options can be overridden via environment variables.

### General

| Variable | Description | Default |
|----------|-------------|---------|
| `CWM_BASE_URI` | Default base URI | - |
| `CWM_FORMAT` | Output format | n3 |
| `CWM_LOG_LEVEL` | Log verbosity | normal |
| `CWM_COLOR` | Colored output | true |

### Reasoning

| Variable | Description | Default |
|----------|-------------|---------|
| `CWM_PROFILE` | Reasoning profile | default |
| `CWM_MAX_STEPS` | Max inference steps | 10000 |
| `CWM_THINK_PASSES` | Reasoning passes | 1 |
| `CWM_ENGINE` | Prover engine | - |
| `CWM_ENABLE_PROOF` | Proof generation | false |
| `CWM_ENABLE_TABLING` | Enable tabling | true |

### Server

| Variable | Description | Default |
|----------|-------------|---------|
| `CWM_SERVER_PORT` | Server port | 8080 |
| `CWM_SERVER_HOST` | Server host | 0.0.0.0 |
| `CWM_CORS_ENABLED` | Enable CORS | true |
| `CWM_CACHE_ENABLED` | Query caching | true |
| `CWM_CACHE_TTL` | Cache TTL (seconds) | 300 |

### Store

| Variable | Description | Default |
|----------|-------------|---------|
| `CWM_STORE` | Store backend | memory |
| `CWM_DATABASE_PATH` | SQLite path | ./cwm.db |
| `CWM_FUSEKI_URL` | Fuseki endpoint | - |
| `CWM_FUSEKI_GRAPH` | Fuseki graph URI | - |
| `CWM_FUSEKI_TIMEOUT` | Fuseki timeout | 30 |
| `CWM_FUSEKI_BATCH` | Batch size | 1000 |

### Security

| Variable | Description | Default |
|----------|-------------|---------|
| `CWM_ENABLE_CRYPTO` | Crypto builtins | false |
| `CWM_ENABLE_FILESYSTEM` | File operations | false |
| `CWM_ENABLE_NETWORK` | Network access | true |
| `CWM_HTTP_TIMEOUT` | HTTP timeout | 30 |

### Example: Environment Configuration

```bash
export CWM_PROFILE=owl
export CWM_MAX_STEPS=50000
export CWM_FUSEKI_URL=http://localhost:3030/dataset
export CWM_ENABLE_CRYPTO=true

cwm data.n3 --think
```

---

## Command-Line Override

Command-line options take precedence over config files and environment variables.

### Precedence Order (highest to lowest)

1. Command-line arguments
2. Environment variables
3. Project config (`./cwm.toml`)
4. User config (`~/.config/cwm/config.toml`)
5. System config (`/etc/cwm/config.toml`)
6. Built-in defaults

### Common Overrides

```bash
# Override profile
cwm data.n3 --profile complete --think

# Override max steps
cwm data.n3 --max-steps 50000 --think

# Override output format
cwm data.n3 --format jsonld

# Override base URI
cwm data.n3 --base http://example.org/

# Multiple overrides
cwm data.n3 \
  --profile owl \
  --max-steps 100000 \
  --engine dl-tableau \
  --think
```

---

## Examples

### Development Configuration

```toml
# cwm.toml for development

[general]
log_level = "verbose"
color = true

[reasoning]
profile = "default"
max_steps = 10000
enable_proof = true

[security]
enable_filesystem = true
enable_crypto = true
enable_network = true

[prefixes]
dev = "http://localhost/dev#"
```

### Production Server Configuration

```toml
# /etc/cwm/config.toml for production

[general]
log_level = "normal"
color = false

[reasoning]
profile = "performance"
max_steps = 5000
enable_tabling = true

[server]
port = 80
host = "0.0.0.0"
cors_enabled = true
cors_origins = ["https://example.org"]
timeout_secs = 30

[server.cache]
enabled = true
max_entries = 5000
ttl_secs = 600

[server.rate_limit]
enabled = true
requests_per_second = 50
burst = 100

[store]
backend = "fuseki"
fuseki_url = "http://fuseki:3030/production"
fuseki_timeout = 60

[security]
enable_crypto = false
enable_filesystem = false
enable_network = false
```

### Research Configuration

```toml
# cwm.toml for research/experimentation

[general]
log_level = "debug"
base_uri = "http://research.example.org/"

[reasoning]
profile = "complete"
max_steps = 0  # unlimited
think_passes = 10
engine = "tableau"
enable_proof = true
enable_tabling = true

[security]
enable_crypto = true
enable_filesystem = true
enable_network = true
http_timeout_secs = 120

[prefixes]
exp = "http://research.example.org/experiment#"
data = "http://research.example.org/data#"
```

### Minimal Configuration

```toml
# Minimal cwm.toml

[reasoning]
profile = "default"

[prefixes]
ex = "http://example.org/"
```

---

## See Also

- [REASONING.md](REASONING.md) - Reasoning strategies and engines
- [SPARQL.md](SPARQL.md) - SPARQL reference
- [SERVER.md](SERVER.md) - HTTP server documentation
