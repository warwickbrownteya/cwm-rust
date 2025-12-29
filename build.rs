//! Build script for cwm
//!
//! Generates shell completions and man pages during build.

use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    // Only generate completions/man pages for release builds or when explicitly requested
    let profile = env::var("PROFILE").unwrap_or_default();
    let generate = env::var("CWM_GENERATE_COMPLETIONS").is_ok() || profile == "release";

    if !generate {
        return;
    }

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap_or_else(|_| ".".to_string()));
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string()));

    // Create output directories
    let completions_dir = manifest_dir.join("completions");
    let man_dir = manifest_dir.join("man");

    fs::create_dir_all(&completions_dir).ok();
    fs::create_dir_all(&man_dir).ok();

    // We can't easily generate completions at build time without duplicating the CLI definition
    // The completions are generated via `cwm --generate-completions` or the xtask
    // This build script primarily sets up directories and prints cargo instructions

    println!("cargo:rerun-if-changed=src/main.rs");
    println!("cargo:rerun-if-changed=build.rs");

    // Set version for embedding
    if let Ok(version) = env::var("CARGO_PKG_VERSION") {
        println!("cargo:rustc-env=CWM_VERSION={}", version);
    }

    // Emit target info
    if let Ok(target) = env::var("TARGET") {
        println!("cargo:rustc-env=CWM_TARGET={}", target);
    }
}
