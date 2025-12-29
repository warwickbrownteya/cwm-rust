# Makefile for cwm - Closed World Machine
#
# This Makefile wraps Cargo commands and provides XDG-compliant installation.
# Autoconf/Automake is NOT used because:
#   1. Cargo handles build configuration, dependencies, and cross-compilation
#   2. Rust's build system is deterministic without configure scripts
#   3. Platform detection is handled by rustc, not autotools
#   4. Adding autotools complexity provides no benefit for Rust projects
#
# Usage:
#   make              - Build release binary
#   make install      - Install to ~/.local (XDG default)
#   make install PREFIX=/usr/local  - Install system-wide
#   make uninstall    - Remove installed files
#   make clean        - Clean build artifacts

# Version
VERSION := 0.1.0

# Installation directories (XDG Base Directory compliant)
PREFIX      ?= $(HOME)/.local
BINDIR      ?= $(PREFIX)/bin
DATADIR     ?= $(PREFIX)/share
MANDIR      ?= $(DATADIR)/man/man1
BASHCOMPDIR ?= $(DATADIR)/bash-completion/completions
ZSHCOMPDIR  ?= $(DATADIR)/zsh/site-functions
FISHCOMPDIR ?= $(DATADIR)/fish/vendor_completions.d
CONFIGDIR   ?= $(HOME)/.config/cwm

# Build configuration
CARGO       ?= cargo
INSTALL     ?= install
RM          ?= rm -f
MKDIR       ?= mkdir -p

# Cargo flags
CARGO_FLAGS ?=
RELEASE     ?= 1

ifeq ($(RELEASE),1)
    CARGO_BUILD_FLAGS = --release
    TARGET_DIR = target/release
else
    CARGO_BUILD_FLAGS =
    TARGET_DIR = target/debug
endif

# Source files
BINARY      = $(TARGET_DIR)/cwm
MANPAGE     = man/cwm.1
BASH_COMP   = completions/cwm.bash
ZSH_COMP    = completions/cwm.zsh
FISH_COMP   = completions/cwm.fish

# Phony targets
.PHONY: all build release debug install uninstall clean distclean \
        test check bench doc help completions man \
        install-bin install-man install-completions install-config \
        uninstall-bin uninstall-man uninstall-completions

# Default target
all: build

# Build targets
build: $(BINARY)

$(BINARY): Cargo.toml src/**/*.rs
	$(CARGO) build $(CARGO_BUILD_FLAGS) $(CARGO_FLAGS)

release:
	$(CARGO) build --release $(CARGO_FLAGS)

debug:
	$(CARGO) build $(CARGO_FLAGS)

# Test targets
test:
	$(CARGO) test $(CARGO_FLAGS)

check:
	$(CARGO) check $(CARGO_FLAGS)
	$(CARGO) clippy $(CARGO_FLAGS) -- -D warnings

bench:
	$(CARGO) bench $(CARGO_FLAGS)

# Documentation
doc:
	$(CARGO) doc --no-deps $(CARGO_FLAGS)

# Installation targets
install: install-bin install-man install-completions install-config
	@echo ""
	@echo "cwm $(VERSION) installed to $(PREFIX)"
	@echo ""
	@echo "Installed files:"
	@echo "  Binary:      $(BINDIR)/cwm"
	@echo "  Man page:    $(MANDIR)/cwm.1"
	@echo "  Bash comp:   $(BASHCOMPDIR)/cwm"
	@echo "  Zsh comp:    $(ZSHCOMPDIR)/_cwm"
	@echo "  Fish comp:   $(FISHCOMPDIR)/cwm.fish"
	@echo "  Config dir:  $(CONFIGDIR)/"
	@echo ""
	@if ! echo "$$PATH" | grep -q "$(BINDIR)"; then \
		echo "NOTE: $(BINDIR) is not in your PATH"; \
		echo "Add to your shell config:"; \
		echo "  export PATH=\"$(BINDIR):\$$PATH\""; \
		echo ""; \
	fi

install-bin: $(BINARY)
	$(MKDIR) $(BINDIR)
	$(INSTALL) -m 755 $(BINARY) $(BINDIR)/cwm

install-man: $(MANPAGE)
	$(MKDIR) $(MANDIR)
	$(INSTALL) -m 644 $(MANPAGE) $(MANDIR)/cwm.1

install-completions: $(BASH_COMP) $(ZSH_COMP) $(FISH_COMP)
	$(MKDIR) $(BASHCOMPDIR)
	$(MKDIR) $(ZSHCOMPDIR)
	$(MKDIR) $(FISHCOMPDIR)
	$(INSTALL) -m 644 $(BASH_COMP) $(BASHCOMPDIR)/cwm
	$(INSTALL) -m 644 $(ZSH_COMP) $(ZSHCOMPDIR)/_cwm
	$(INSTALL) -m 644 $(FISH_COMP) $(FISHCOMPDIR)/cwm.fish

install-config:
	$(MKDIR) $(CONFIGDIR)

# Uninstallation
uninstall: uninstall-bin uninstall-man uninstall-completions
	@echo "cwm uninstalled from $(PREFIX)"

uninstall-bin:
	$(RM) $(BINDIR)/cwm

uninstall-man:
	$(RM) $(MANDIR)/cwm.1

uninstall-completions:
	$(RM) $(BASHCOMPDIR)/cwm
	$(RM) $(ZSHCOMPDIR)/_cwm
	$(RM) $(FISHCOMPDIR)/cwm.fish

# Clean targets
clean:
	$(CARGO) clean

distclean: clean
	$(RM) -r $(CONFIGDIR)

# Development helpers
completions: $(BASH_COMP) $(ZSH_COMP) $(FISH_COMP)

# Generate completions (requires building first)
completions-generate: build
	$(BINARY) --generate-completion bash > $(BASH_COMP)
	$(BINARY) --generate-completion zsh > $(ZSH_COMP)
	$(BINARY) --generate-completion fish > $(FISH_COMP)

# Package targets
.PHONY: dist deb rpm

dist: release
	$(MKDIR) dist
	tar -czvf dist/cwm-$(VERSION)-$$(uname -s | tr '[:upper:]' '[:lower:]')-$$(uname -m).tar.gz \
		-C $(TARGET_DIR) cwm \
		-C ../.. completions man README.md LICENSE

deb: release
	$(CARGO) deb

rpm: release
	$(CARGO) generate-rpm

# Help
help:
	@echo "cwm $(VERSION) - Closed World Machine"
	@echo ""
	@echo "Build targets:"
	@echo "  make              Build release binary"
	@echo "  make debug        Build debug binary"
	@echo "  make release      Build optimized release binary"
	@echo "  make test         Run tests"
	@echo "  make check        Run clippy and checks"
	@echo "  make bench        Run benchmarks"
	@echo "  make doc          Generate documentation"
	@echo ""
	@echo "Install targets:"
	@echo "  make install      Install to PREFIX (default: ~/.local)"
	@echo "  make uninstall    Remove installed files"
	@echo ""
	@echo "Options:"
	@echo "  PREFIX=<path>     Installation prefix (default: ~/.local)"
	@echo "  RELEASE=0         Build debug instead of release"
	@echo ""
	@echo "Examples:"
	@echo "  make install                      # Install to ~/.local"
	@echo "  make install PREFIX=/usr/local    # System-wide install"
	@echo "  sudo make install PREFIX=/usr     # Install to /usr"
	@echo "  make uninstall PREFIX=/usr/local  # Uninstall from /usr/local"
	@echo ""
	@echo "XDG directories (when PREFIX=~/.local):"
	@echo "  Binary:       ~/.local/bin/cwm"
	@echo "  Man page:     ~/.local/share/man/man1/cwm.1"
	@echo "  Completions:  ~/.local/share/{bash-completion,zsh,fish}/"
	@echo "  Config:       ~/.config/cwm/"
