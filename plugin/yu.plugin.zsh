_yu_item_fetch() {
    local -a del_items
    local -a repo_path
    local -a sed_path
    repo_path=`yu helper --label=path`
    sed_path=`echo $repo_path | sed 's/\\//\\\\\\//g'`
    del_items=(`yu helper --label=path`/.yu/*.item.json)
    del_items=`echo $del_items | sed "s/$sed_path\\/\\.yu\///g" | sed 's/\.item\.json//g'`
    echo $del_items
}

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
                        {-k,--type=}'[TYPE the type of item]:new_item_kind:->new_item_kinds' \
                        {-u,--url=}'[URL the url of item]' \
                        {-h,--title=}'[TEXT the title of item]' \
                        {-m,--mime=}'[MIME the mime of item]new_item_mime:->new_item_mimes' \
                        '*'{-t,--tag=}'[TAG the tag of item]' \
                        {-s,--summary=}'[TEXT|FILE the summary of item]:new_item:sum:->new_item_sums' \
                        {-c,--content=}'[FILE the content of item]:new_item_con:->new_item_cons' \
                        {-w,--whose=}'[OWNER the owner]' \
                        && ret=0
                    ;;
                (ih)
                    _arguments \
                        {-t,--token=}'[TOKEN the token of identifying]' \
                        {-h,--hash=}'[HASH the hash of algorithm]:ih_hash:ih_hashs' \
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
                        {-d,--id=}'[ID id]:del_item:->del_items' \
                        && ret=0
                    ;;
                (make)
                    _arguments \
                        {-l,--label=}'[label of item]:make_item:->make_items' \
                        {-o,--output=}'[FILE outpu file]:make_out:->make_outs' \
                        && ret=0
                    ;;
                (nav)
                    _arguments \
                        {-o,--opt=}'[add|del  opt]:nav_opt:->nav_opts' \
                        {-l,--label=}'[LABEL]:nav_label:->nav_labels' \
                        {-r,--order=}'[INT order]' \
                        {-u,--url=}'[URL name]' \
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
                (new_item_kinds)
                    _values "post"     \
                            "binary"   \
                            "text"     \
                            "frame"    \
                            "static"   \
                            "query"
                ;;
                (new_item_mimes)
                    _values "application/pdf"          \
                            "application/x-javascript" \
                            "audio/mpeg"               \
                            "image/bmp"                \
                            "image/gif"                \
                            "image/jpeg"               \
                            "image/svg+xml"            \
                            "text/css"                 \
                            "text/html"                \
                            "text/plain"
                    ;;
                (new_item_sums)
                    local -a sum_files
                    sum_files=(**/**)
                    _multi_parts / sum_files
                ;;
                (new_item_cons)
                    local -a cons_files
                    cons_files=(**/**)
                    _multi_parts / cons_files
                ;;
                (ih_hashs)
                    _values "sha256"
                ;;
                (del_items)
                    local -a del_items
                    del_items=`_yu_item_fetch`
                    _multi_parts / del_items
                ;;
                (make_items)
                    local -a make_items
                    make_items=`_yu_item_fetch`
                    _multi_parts / make_items
                ;;
                (make_outs)
                    local -a make_outs
                    make_ours=(**/*.mk)
                    _multi_parts / make_outs
                ;;
                (nav_opts)
                    _values "add" \
                            "del"
                ;;
                (nav_labels)
                ;;
                (scripts)
                    _values "make-all"    \
                            "cfg-rename"  \
                            "cfg-upgrade"
                    ;;
                (helpers)
                    _values "flags" \
                            "path"
                    ;;
            esac
    esac
}

compdef _yu_command_helper yu
