_cwm() {
    local i cur prev opts cmd
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    cmd=""
    opts=""

    for i in ${COMP_WORDS[@]}
    do
        case "${cmd},${i}" in
            ",$1")
                cmd="cwm"
                ;;
            *)
                ;;
        esac
    done

    case "${cmd}" in
        cwm)
            opts="-f -o -v -q -h -V --stdin --think --filter --filter-rules --max-steps --format --base --output --verbose --quiet --apply --rules --strings --purge-rules --purge-builtins --mode --data --flatten --unflatten --crypto --think-passes --no --purge --chatty --ugly --bySubject --with --pipe --reify --dereify --sparql --sparql-query --sparql-results --n3 --rdf --why --proof --closure --patch --language --sparqlServer --query --triples --db --fuseki --fuseki-graph --fuseki-timeout --fuseki-batch --engine --profile --config --init-config --generate-completions --help --version [FILE]..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 1 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --filter-rules)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --max-steps)
                    COMPREPLY=($(compgen -W "" -- "${cur}"))
                    return 0
                    ;;
                -f|--format)
                    COMPREPLY=($(compgen -W "n3 ntriples rdf jsonld debug" -- "${cur}"))
                    return 0
                    ;;
                --base)
                    COMPREPLY=($(compgen -W "" -- "${cur}"))
                    return 0
                    ;;
                -o|--output)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --apply)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --rules)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --mode)
                    COMPREPLY=($(compgen -W "r s a E m u t f" -- "${cur}"))
                    return 0
                    ;;
                --data)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --think-passes)
                    COMPREPLY=($(compgen -W "" -- "${cur}"))
                    return 0
                    ;;
                --chatty)
                    COMPREPLY=($(compgen -W "0 1 2 3 4 5" -- "${cur}"))
                    return 0
                    ;;
                --sparql)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --sparql-query)
                    COMPREPLY=($(compgen -W "" -- "${cur}"))
                    return 0
                    ;;
                --sparql-results)
                    COMPREPLY=($(compgen -W "xml json csv tsv" -- "${cur}"))
                    return 0
                    ;;
                --n3)
                    COMPREPLY=($(compgen -W "a c d e g i l n p r s t u v B" -- "${cur}"))
                    return 0
                    ;;
                --rdf)
                    COMPREPLY=($(compgen -W "" -- "${cur}"))
                    return 0
                    ;;
                --closure)
                    COMPREPLY=($(compgen -W "" -- "${cur}"))
                    return 0
                    ;;
                --patch)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --language)
                    COMPREPLY=($(compgen -W "n3 turtle ntriples rdfxml jsonld" -- "${cur}"))
                    return 0
                    ;;
                --sparqlServer)
                    COMPREPLY=($(compgen -W "8000 8080 3030" -- "${cur}"))
                    return 0
                    ;;
                --query)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --db)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --fuseki)
                    COMPREPLY=($(compgen -W "http://localhost:3030" -- "${cur}"))
                    return 0
                    ;;
                --fuseki-graph)
                    COMPREPLY=($(compgen -W "" -- "${cur}"))
                    return 0
                    ;;
                --fuseki-timeout)
                    COMPREPLY=($(compgen -W "30 60 120" -- "${cur}"))
                    return 0
                    ;;
                --fuseki-batch)
                    COMPREPLY=($(compgen -W "100 1000 10000" -- "${cur}"))
                    return 0
                    ;;
                --engine)
                    COMPREPLY=($(compgen -W "forward backward resolution otter prover9 dpll cdcl tableau superposition leancop nanocop" -- "${cur}"))
                    return 0
                    ;;
                --profile)
                    COMPREPLY=($(compgen -W "forward rdfs owl2rl full minimal" -- "${cur}"))
                    return 0
                    ;;
                --config)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --generate-completions)
                    COMPREPLY=($(compgen -W "bash zsh fish powershell elvish" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
    esac
}

complete -F _cwm -o bashdefault -o default cwm
