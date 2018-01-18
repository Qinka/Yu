_yu_command_helper() {
    local ret=1

    _arguments \
        '1: :->subcommands' \
        '*::arg:->args' \
        {'-?','--help'}'[Display help message]' \
        {'-V','--version'}'[Print version information]' \
        && ret=0

    case $state in
        (subcommands)
            subcommands=(
                "new:Create new item"
                "make:Generate GNUMakefile"
                "ih:Identification helper"
                "init:Initialize a new repo"
                "nav:Navigating listing"
                "del:Delete item"
                "helper:Helpers"
                "script:Script"
            )
            _describe -t subcommands 'yu subcommands' subcommands && ret=0
            ;;
        (args)
            case $line[1] in
                (new)
                    _arguments \
                        {-i,--id=}'[ID the id of item]' \
                        {-k,--type=}'[TYPE the type of item]' \
                        {-u,--url=}'[URL the url of item]' \
                        {-h,--title=}'[TEXT the title of item]' \
                        {-m,--mime=}'[MIME the mime of item]' \
                        '*'{-t,--tag=}'[TAG the tag of item]' \
                        {-s,--summary=}'[TEXT|FILE the summary of item]' \
                        {-c,--content=}'[FILE the content of item]' \
                        {-w,--whose=}'[OWNER the owner]' \
                        && ret=0
                    ;;
                (ih)
                    _arguments \
                        {-t,--token=}'[TOKEN the token of identifying]' \
                        {-h,--hash=}'[HASH the hash of algorithm]' \
                        {-d,--debug}'[Debug it]' \
                        && ret=0
                    ;;
                (init)
                    _arguments \
                        {-u,--url}'[URL the url of site]' \
                        {-t,--token}'[TOKEN the file where hold token]' \
                        && ret=0
                    ;;
                (del)
                    _arguments \
                        {-d,--id=}'[ID id]' \
                        && ret=0
                    ;;
                (make)
                    _arguments \
                        {-l,--label=}'[label of item]' \
                        {-o,--output=}'[FILE outpu file]' \
                        && ret=0
                    ;;
                (nav)
                    _arguments \
                        {-o,--opt=}'[add|del  opt]' \
                        {-l,--label=}'[LABEL]' \
                        {-r,--order=}'[INT    order]' \
                        {-u,--url=}'[URL      name]' \
                        && ret=0
                    ;;

                (script)
                    _arguments \
                        {-k,--kind=}'[TEXT script]::->scripts' \
                        && ret=0
                    ;;
                (helper)
                    _arguments \
                        {-l,--label=}'[label get item of helper]:helper:->helpers' \
                        && ret=0
                    ;;
            esac
            case $state in
                (scripts)
                    _values 'make-all' 'cfg-rename' 'cfg-upgrade'
                    ;;
                (helpers)
                    _values 'flags' 'path'
                    ;;
            esac
    esac
}

compdef _yu_command_helper yu
