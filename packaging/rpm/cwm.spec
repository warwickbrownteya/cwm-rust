Name:           cwm
Version:        0.1.0
Release:        1%{?dist}
Summary:        Closed World Machine - N3 reasoner and RDF processor

License:        MIT OR Apache-2.0
URL:            https://github.com/cwm-rust/cwm-rust
Source0:        %{name}-%{version}.tar.gz

BuildRequires:  rust >= 1.75
BuildRequires:  cargo
BuildRequires:  gcc

%description
CWM-Rust is a high-performance implementation of the Closed World Machine,
an N3 (Notation3) reasoner and RDF processor originally created by
Tim Berners-Lee at W3C.

Features include:
- 302+ built-in predicates (math, string, list, log, time, crypto)
- Full SPARQL 1.1 query and update support
- 11 theorem proving engines
- Forward and backward chaining inference
- OWL 2 RL profile support
- Advanced reasoning: fuzzy logic, probabilistic, temporal
- Distributed reasoning with Raft consensus

%prep
%autosetup

%build
cargo build --release

%install
install -D -m 755 target/release/cwm %{buildroot}%{_bindir}/cwm
install -D -m 644 completions/cwm.bash %{buildroot}%{_datadir}/bash-completion/completions/cwm
install -D -m 644 completions/cwm.zsh %{buildroot}%{_datadir}/zsh/site-functions/_cwm
install -D -m 644 completions/cwm.fish %{buildroot}%{_datadir}/fish/vendor_completions.d/cwm.fish
install -D -m 644 man/cwm.1 %{buildroot}%{_mandir}/man1/cwm.1

%check
cargo test --release

%files
%license LICENSE
%doc README.md
%{_bindir}/cwm
%{_datadir}/bash-completion/completions/cwm
%{_datadir}/zsh/site-functions/_cwm
%{_datadir}/fish/vendor_completions.d/cwm.fish
%{_mandir}/man1/cwm.1*

%changelog
* Sun Dec 29 2024 CWM-Rust Contributors <cwm-rust@example.org> - 0.1.0-1
- Initial package
