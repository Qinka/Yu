function _yu_command() {
    local rt=1 state
    _arguments ':subcommand:->subcommand' && rt=0

    case $state in
        subcommand)
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
            _describe -t subcommands 'yu subcommands' subcommands && rt=0
    esac
    return rt
}

compdef _yu_command yu


