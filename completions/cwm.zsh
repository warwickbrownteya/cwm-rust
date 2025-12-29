#compdef cwm

autoload -U is-at-least

_cwm() {
    typeset -A opt_args
    typeset -a _arguments_options
    local ret=1

    if is-at-least 5.2; then
        _arguments_options=(-s -S -C)
    else
        _arguments_options=(-s -C)
    fi

    local context curcontext="$curcontext" state line
    _arguments "${_arguments_options[@]}" \
'--stdin[Read input from stdin]' \
'--think[Apply rules (forward-chaining inference)]' \
'--filter[Output only inferred triples (requires --think)]' \
'*--filter-rules=[Apply rules and replace store with conclusions only]:FILE:_files' \
'--max-steps=[Maximum number of inference steps]:STEPS: ' \
'-f+[Output format]:FORMAT:(n3 ntriples rdf jsonld debug)' \
'--format=[Output format]:FORMAT:(n3 ntriples rdf jsonld debug)' \
'--base=[Base URI for relative references]:URI: ' \
'-o+[Output file (defaults to stdout)]:FILE:_files' \
'--output=[Output file (defaults to stdout)]:FILE:_files' \
'-v[Verbose output]' \
'--verbose[Verbose output]' \
'-q[Quiet mode (suppress info messages)]' \
'--quiet[Quiet mode (suppress info messages)]' \
'*--apply=[Apply rules from specified file(s)]:RULES:_files' \
'*--rules=[Load rules from specified file(s)]:RULES:_files' \
'--strings[Output strings only (values of matching patterns)]' \
'--purge-rules[Purge rules from output]' \
'--purge-builtins[Purge triples with builtin predicates from output]' \
'--mode=[Operating mode]:MODE:(r s a E m u t f)' \
'*--data=[Data files to load (no rules extracted)]:FILE:_files' \
'--flatten[Flatten formula contents into main graph]' \
'--unflatten[Unflatten: reconstruct nested formulas]' \
'--crypto[Enable cryptographic operations]' \
'--think-passes=[Number of think passes]:N: ' \
'--no[Suppress all output]' \
'--purge[Purge statements with log:Chaff class]' \
'--chatty=[Debug/chatty level]:LEVEL:(0 1 2 3 4 5)' \
'--ugly[Minimal formatting, fastest mode]' \
'--bySubject[Sort output by subject]' \
'*--with=[Pass remaining arguments as os:argv values]:ARGS: ' \
'--pipe[Pipe mode: process without storing intermediate results]' \
'--reify[Reify statements (convert to RDF reification)]' \
'--dereify[Dereify statements (reverse reification)]' \
'--sparql=[Run SPARQL query from file]:QUERY_FILE:_files' \
'--sparql-query=[Run inline SPARQL query]:QUERY: ' \
'--sparql-results=[SPARQL results format]:FORMAT:(xml json csv tsv)' \
'--n3=[N3 output formatting flags]:FLAGS: ' \
'--rdf=[RDF/XML formatting flags]:FLAGS: ' \
'--why[Generate proof trace]' \
'--proof[Output proof in N3]' \
'--closure=[Closure computation flags]:FLAGS: ' \
'--patch=[Apply N3 patch from file]:FILE:_files' \
'--language=[Input language override]:LANG:(n3 turtle ntriples rdfxml jsonld)' \
'--sparqlServer=[Start SPARQL endpoint on port]:PORT: ' \
'--query=[Query file for filtering]:FILE:_files' \
'--triples[Output triples count]' \
'--db=[SQLite database path]:PATH:_files' \
'--fuseki=[Apache Jena Fuseki endpoint URL]:URL: ' \
'--fuseki-graph=[Named graph URI for Fuseki]:URI: ' \
'--fuseki-timeout=[Fuseki operation timeout in seconds]:SECS: ' \
'--fuseki-batch=[Fuseki batch insert size]:SIZE: ' \
'--engine=[Reasoning engine to use]:ENGINE:(forward backward resolution otter prover9 dpll cdcl tableau superposition leancop nanocop)' \
'--profile=[Reasoning profile]:PROFILE:(forward rdfs owl2rl full minimal)' \
'--config=[Configuration file path]:FILE:_files' \
'--init-config[Create default configuration file]' \
'--generate-completions=[Generate shell completions]:SHELL:(bash zsh fish powershell elvish)' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
'*::files -- Input files to process:_files' \
&& ret=0
}

(( $+functions[_cwm_commands] )) ||
_cwm_commands() {
    local commands; commands=()
    _describe -t commands 'cwm commands' commands "$@"
}

_cwm "$@"
