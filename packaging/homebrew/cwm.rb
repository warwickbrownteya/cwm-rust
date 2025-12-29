# Homebrew formula for cwm
# Install with: brew install cwm-rust/tap/cwm
# Or: brew tap cwm-rust/tap && brew install cwm

class Cwm < Formula
  desc "Closed World Machine - High-performance N3 reasoner and RDF processor"
  homepage "https://github.com/cwm-rust/cwm-rust"
  version "0.1.0"
  license any_of: ["MIT", "Apache-2.0"]

  on_macos do
    on_arm do
      url "https://github.com/cwm-rust/cwm-rust/releases/download/v#{version}/cwm-macos-aarch64.tar.gz"
      sha256 "PLACEHOLDER_SHA256_MACOS_ARM64"
    end
    on_intel do
      url "https://github.com/cwm-rust/cwm-rust/releases/download/v#{version}/cwm-macos-x86_64.tar.gz"
      sha256 "PLACEHOLDER_SHA256_MACOS_X86_64"
    end
  end

  on_linux do
    on_arm do
      url "https://github.com/cwm-rust/cwm-rust/releases/download/v#{version}/cwm-linux-aarch64.tar.gz"
      sha256 "PLACEHOLDER_SHA256_LINUX_ARM64"
    end
    on_intel do
      url "https://github.com/cwm-rust/cwm-rust/releases/download/v#{version}/cwm-linux-x86_64.tar.gz"
      sha256 "PLACEHOLDER_SHA256_LINUX_X86_64"
    end
  end

  def install
    bin.install "cwm"
    bash_completion.install "completions/cwm.bash" => "cwm"
    zsh_completion.install "completions/cwm.zsh" => "_cwm"
    fish_completion.install "completions/cwm.fish"
    man1.install "man/cwm.1"
  end

  def caveats
    <<~EOS
      cwm has been installed!

      To get started:
        cwm --help
        cwm data.n3 --think

      For SPARQL endpoint:
        cwm --server --port 8080

      Documentation: https://github.com/cwm-rust/cwm-rust
    EOS
  end

  test do
    assert_match "cwm #{version}", shell_output("#{bin}/cwm --version")
    (testpath/"test.n3").write <<~EOS
      @prefix ex: <http://example.org/> .
      ex:subject ex:predicate ex:object .
    EOS
    output = shell_output("#{bin}/cwm #{testpath}/test.n3")
    assert_match "example.org", output
  end
end
