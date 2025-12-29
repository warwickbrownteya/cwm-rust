#!/usr/bin/env bash
#
# cwm installer script
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/cwm-rust/cwm-rust/main/install.sh | bash
#   curl -fsSL https://raw.githubusercontent.com/cwm-rust/cwm-rust/main/install.sh | bash -s -- --prefix=/usr/local
#   curl -fsSL https://raw.githubusercontent.com/cwm-rust/cwm-rust/main/install.sh | bash -s -- --version v0.1.0
#

set -euo pipefail

# Configuration
REPO="cwm-rust/cwm-rust"
BINARY_NAME="cwm"
DEFAULT_VERSION="latest"
DEFAULT_PREFIX="${HOME}/.local"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
info() {
    printf "${BLUE}info:${NC} %s\n" "$1"
}

success() {
    printf "${GREEN}success:${NC} %s\n" "$1"
}

warn() {
    printf "${YELLOW}warning:${NC} %s\n" "$1"
}

error() {
    printf "${RED}error:${NC} %s\n" "$1" >&2
}

die() {
    error "$1"
    exit 1
}

# Detect OS and architecture
detect_platform() {
    local os arch

    case "$(uname -s)" in
        Linux*)  os="linux" ;;
        Darwin*) os="macos" ;;
        MINGW*|MSYS*|CYGWIN*) os="windows" ;;
        *)       die "Unsupported operating system: $(uname -s)" ;;
    esac

    case "$(uname -m)" in
        x86_64|amd64)  arch="x86_64" ;;
        aarch64|arm64) arch="aarch64" ;;
        armv7l)        arch="armv7" ;;
        *)             die "Unsupported architecture: $(uname -m)" ;;
    esac

    echo "${os}-${arch}"
}

# Get the download URL for a release
get_download_url() {
    local version="$1"
    local platform="$2"
    local base_url="https://github.com/${REPO}/releases"

    if [[ "$version" == "latest" ]]; then
        version=$(curl -fsSL "${base_url}/latest" -o /dev/null -w '%{url_effective}' | grep -oE '[^/]+$')
    fi

    local target
    case "$platform" in
        linux-x86_64)   target="x86_64-unknown-linux-gnu" ;;
        linux-aarch64)  target="aarch64-unknown-linux-gnu" ;;
        macos-x86_64)   target="x86_64-apple-darwin" ;;
        macos-aarch64)  target="aarch64-apple-darwin" ;;
        windows-x86_64) target="x86_64-pc-windows-msvc" ;;
        *)              die "Unsupported platform: $platform" ;;
    esac

    local ext="tar.gz"
    [[ "$platform" == windows-* ]] && ext="zip"

    echo "${base_url}/download/${version}/${BINARY_NAME}-${target}.${ext}"
}

# Download and extract the binary
download_and_extract() {
    local url="$1"
    local dest="$2"
    local tmpdir

    tmpdir=$(mktemp -d)
    trap 'rm -rf "$tmpdir"' EXIT

    info "Downloading from ${url}"

    if command -v curl &>/dev/null; then
        curl -fsSL "$url" -o "${tmpdir}/archive"
    elif command -v wget &>/dev/null; then
        wget -q "$url" -O "${tmpdir}/archive"
    else
        die "Neither curl nor wget found. Please install one of them."
    fi

    info "Extracting..."

    case "$url" in
        *.zip)
            unzip -q "${tmpdir}/archive" -d "${tmpdir}/extracted"
            ;;
        *.tar.gz|*.tgz)
            mkdir -p "${tmpdir}/extracted"
            tar -xzf "${tmpdir}/archive" -C "${tmpdir}/extracted"
            ;;
        *)
            die "Unknown archive format"
            ;;
    esac

    # Find the binary
    local binary
    binary=$(find "${tmpdir}/extracted" -name "${BINARY_NAME}" -o -name "${BINARY_NAME}.exe" | head -n1)

    if [[ -z "$binary" ]]; then
        die "Could not find ${BINARY_NAME} in archive"
    fi

    # Create destination directory
    mkdir -p "$(dirname "$dest")"

    # Copy binary
    cp "$binary" "$dest"
    chmod +x "$dest"

    # Copy completions if present
    local completions_dir
    completions_dir=$(find "${tmpdir}/extracted" -type d -name "completions" | head -n1)
    if [[ -n "$completions_dir" ]]; then
        install_completions "$completions_dir"
    fi

    # Copy man page if present
    local man_dir
    man_dir=$(find "${tmpdir}/extracted" -type d -name "man" | head -n1)
    if [[ -n "$man_dir" ]]; then
        install_man_pages "$man_dir"
    fi
}

# Install shell completions
install_completions() {
    local src_dir="$1"

    # Bash completions
    if [[ -f "${src_dir}/cwm.bash" ]]; then
        local bash_comp_dir="${HOME}/.local/share/bash-completion/completions"
        mkdir -p "$bash_comp_dir"
        cp "${src_dir}/cwm.bash" "${bash_comp_dir}/cwm"
        info "Installed bash completions to ${bash_comp_dir}"
    fi

    # Zsh completions
    if [[ -f "${src_dir}/cwm.zsh" ]]; then
        local zsh_comp_dir="${HOME}/.local/share/zsh/site-functions"
        mkdir -p "$zsh_comp_dir"
        cp "${src_dir}/cwm.zsh" "${zsh_comp_dir}/_cwm"
        info "Installed zsh completions to ${zsh_comp_dir}"
    fi

    # Fish completions
    if [[ -f "${src_dir}/cwm.fish" ]]; then
        local fish_comp_dir="${HOME}/.config/fish/completions"
        mkdir -p "$fish_comp_dir"
        cp "${src_dir}/cwm.fish" "${fish_comp_dir}/cwm.fish"
        info "Installed fish completions to ${fish_comp_dir}"
    fi
}

# Install man pages
install_man_pages() {
    local src_dir="$1"
    local man_dir="${HOME}/.local/share/man/man1"

    if [[ -f "${src_dir}/cwm.1" ]]; then
        mkdir -p "$man_dir"
        cp "${src_dir}/cwm.1" "$man_dir/"
        info "Installed man page to ${man_dir}"
    fi
}

# Update PATH if needed
update_path() {
    local bin_dir="$1"
    local shell_rc

    # Check if already in PATH
    if [[ ":$PATH:" == *":${bin_dir}:"* ]]; then
        return
    fi

    warn "${bin_dir} is not in your PATH"

    # Detect shell config file
    case "$SHELL" in
        */zsh)  shell_rc="${HOME}/.zshrc" ;;
        */bash) shell_rc="${HOME}/.bashrc" ;;
        */fish) shell_rc="${HOME}/.config/fish/config.fish" ;;
        *)      shell_rc="" ;;
    esac

    if [[ -n "$shell_rc" ]]; then
        echo ""
        echo "Add the following to your ${shell_rc}:"
        echo ""
        if [[ "$SHELL" == */fish ]]; then
            echo "  set -gx PATH ${bin_dir} \$PATH"
        else
            echo "  export PATH=\"${bin_dir}:\$PATH\""
        fi
        echo ""
        echo "Then reload your shell or run:"
        echo "  source ${shell_rc}"
    fi
}

# Main installation function
main() {
    local version="$DEFAULT_VERSION"
    local prefix="$DEFAULT_PREFIX"

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --version)
                version="$2"
                shift 2
                ;;
            --version=*)
                version="${1#*=}"
                shift
                ;;
            --prefix)
                prefix="$2"
                shift 2
                ;;
            --prefix=*)
                prefix="${1#*=}"
                shift
                ;;
            --help|-h)
                echo "Usage: $0 [OPTIONS]"
                echo ""
                echo "Options:"
                echo "  --version VERSION  Version to install (default: latest)"
                echo "  --prefix PATH      Installation prefix (default: ~/.local)"
                echo "  --help             Show this help message"
                exit 0
                ;;
            *)
                die "Unknown option: $1"
                ;;
        esac
    done

    echo ""
    echo "╔═══════════════════════════════════════════╗"
    echo "║           CWM Installer                   ║"
    echo "║  Closed World Machine - N3 Reasoner       ║"
    echo "╚═══════════════════════════════════════════╝"
    echo ""

    # Detect platform
    local platform
    platform=$(detect_platform)
    info "Detected platform: ${platform}"

    # Get download URL
    local url
    url=$(get_download_url "$version" "$platform")
    info "Version: ${version}"

    # Determine binary path
    local bin_dir="${prefix}/bin"
    local binary_path="${bin_dir}/${BINARY_NAME}"

    # Check if already installed
    if [[ -f "$binary_path" ]]; then
        local current_version
        current_version=$("$binary_path" --version 2>/dev/null | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' || echo "unknown")
        warn "cwm ${current_version} is already installed at ${binary_path}"
        read -p "Do you want to overwrite it? [y/N] " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            info "Installation cancelled"
            exit 0
        fi
    fi

    # Download and install
    download_and_extract "$url" "$binary_path"

    success "cwm installed to ${binary_path}"

    # Verify installation
    if "${binary_path}" --version &>/dev/null; then
        local installed_version
        installed_version=$("${binary_path}" --version | head -n1)
        success "Verified: ${installed_version}"
    fi

    # Update PATH hint
    update_path "$bin_dir"

    echo ""
    success "Installation complete!"
    echo ""
    echo "Get started:"
    echo "  cwm --help"
    echo "  cwm data.n3 --think"
    echo ""
}

main "$@"
