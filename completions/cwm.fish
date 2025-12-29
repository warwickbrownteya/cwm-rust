# Fish completion for cwm

complete -c cwm -f

# Input/Output options
complete -c cwm -l stdin -d 'Read input from stdin'
complete -c cwm -s o -l output -r -F -d 'Output file (defaults to stdout)'
complete -c cwm -s f -l format -x -a 'n3 ntriples rdf jsonld debug' -d 'Output format'
complete -c cwm -l base -x -d 'Base URI for relative references'

# Reasoning options
complete -c cwm -l think -d 'Apply rules (forward-chaining inference)'
complete -c cwm -l filter -d 'Output only inferred triples'
complete -c cwm -l filter-rules -r -F -d 'Apply rules and replace store with conclusions'
complete -c cwm -l max-steps -x -d 'Maximum number of inference steps'
complete -c cwm -l apply -r -F -d 'Apply rules from specified file'
complete -c cwm -l rules -r -F -d 'Load rules from specified file'
complete -c cwm -l data -r -F -d 'Data files to load (no rules extracted)'
complete -c cwm -l think-passes -x -d 'Number of think passes'

# Engine options
complete -c cwm -l engine -x -a 'forward backward resolution otter prover9 dpll cdcl tableau superposition leancop nanocop' -d 'Reasoning engine'
complete -c cwm -l profile -x -a 'forward rdfs owl2rl full minimal' -d 'Reasoning profile'

# SPARQL options
complete -c cwm -l sparql -r -F -d 'Run SPARQL query from file'
complete -c cwm -l sparql-query -x -d 'Run inline SPARQL query'
complete -c cwm -l sparql-results -x -a 'xml json csv tsv' -d 'SPARQL results format'
complete -c cwm -l sparqlServer -x -d 'Start SPARQL endpoint on port'
complete -c cwm -l query -r -F -d 'Query file for filtering'

# Output modifiers
complete -c cwm -l strings -d 'Output strings only'
complete -c cwm -l purge-rules -d 'Purge rules from output'
complete -c cwm -l purge-builtins -d 'Purge triples with builtin predicates'
complete -c cwm -l purge -d 'Purge statements with log:Chaff class'
complete -c cwm -l flatten -d 'Flatten formula contents into main graph'
complete -c cwm -l unflatten -d 'Reconstruct nested formulas'
complete -c cwm -l reify -d 'Reify statements'
complete -c cwm -l dereify -d 'Dereify statements'
complete -c cwm -l ugly -d 'Minimal formatting, fastest mode'
complete -c cwm -l bySubject -d 'Sort output by subject'
complete -c cwm -l triples -d 'Output triples count'
complete -c cwm -l no -d 'Suppress all output'

# Formatting options
complete -c cwm -l n3 -x -d 'N3 output formatting flags'
complete -c cwm -l rdf -x -d 'RDF/XML formatting flags'
complete -c cwm -l language -x -a 'n3 turtle ntriples rdfxml jsonld' -d 'Input language'

# Proof options
complete -c cwm -l why -d 'Generate proof trace'
complete -c cwm -l proof -d 'Output proof in N3'
complete -c cwm -l closure -x -d 'Closure computation flags'

# Database options
complete -c cwm -l db -r -F -d 'SQLite database path'
complete -c cwm -l fuseki -x -d 'Apache Jena Fuseki endpoint URL'
complete -c cwm -l fuseki-graph -x -d 'Named graph URI for Fuseki'
complete -c cwm -l fuseki-timeout -x -d 'Fuseki operation timeout in seconds'
complete -c cwm -l fuseki-batch -x -d 'Fuseki batch insert size'

# Other options
complete -c cwm -l crypto -d 'Enable cryptographic operations'
complete -c cwm -l mode -x -a 'r s a E m u t f' -d 'Operating mode'
complete -c cwm -l chatty -x -a '0 1 2 3 4 5' -d 'Debug/chatty level'
complete -c cwm -l with -x -d 'Pass arguments as os:argv values'
complete -c cwm -l pipe -d 'Pipe mode'
complete -c cwm -l patch -r -F -d 'Apply N3 patch from file'

# Configuration
complete -c cwm -l config -r -F -d 'Configuration file path'
complete -c cwm -l init-config -d 'Create default configuration file'
complete -c cwm -l generate-completions -x -a 'bash zsh fish powershell elvish' -d 'Generate shell completions'

# Verbosity
complete -c cwm -s v -l verbose -d 'Verbose output'
complete -c cwm -s q -l quiet -d 'Quiet mode'

# Help
complete -c cwm -s h -l help -d 'Print help'
complete -c cwm -s V -l version -d 'Print version'
