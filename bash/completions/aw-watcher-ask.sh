_aw_watcher_ask_completion() {
    local IFS=$'
'
    COMPREPLY=( $( env COMP_WORDS="${COMP_WORDS[*]}" \
                   COMP_CWORD=$COMP_CWORD \
                   _AW_WATCHER_ASK_COMPLETE=complete_bash $1 ) )
    return 0
}

complete -o default -F _aw_watcher_ask_completion aw-watcher-ask