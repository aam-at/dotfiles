alias nmsg="niri msg"
alias na="niri msg action"
alias nws="niri msg workspaces"
alias nwsj="niri msg -j workspaces"
alias nwin="niri msg windows"
alias nwinj="niri msg -j windows"
alias nout="niri msg outputs"
alias nreload="niri msg action load-config-file"
alias noverview="niri msg action toggle-overview"
alias nfloat="niri msg action toggle-window-floating"
alias nbackws="niri msg action focus-workspace-previous"
alias ncw1='niri msg action set-column-width "100%"'
alias nww1='niri msg action set-window-width "100%"'
alias ncw50='niri msg action set-column-width "50%"'
alias nww50='niri msg action set-window-width "50%"'

function __niri_width_change --argument-names change
    if string match -rq '^(0(\.[0-9]+)?|1(\.0+)?)$' -- "$change"
        printf '%s%%' (math "$change * 100")
    else
        printf '%s' "$change"
    end
end

# Accept native niri width strings (e.g. +10%, 66.667%) or proportions (e.g. 1.0).
function ncw --argument-names change
    if test (count $argv) -ne 1
        echo "Usage: ncw <change|proportion>" >&2
        return 1
    end
    set -l normalized (__niri_width_change "$change")
    niri msg action set-column-width "$normalized"
end

# Accept native niri width strings (e.g. +10%, 66.667%) or proportions (e.g. 1.0).
function nww --argument-names change
    if test (count $argv) -ne 1
        echo "Usage: nww <change|proportion>" >&2
        return 1
    end
    set -l normalized (__niri_width_change "$change")
    niri msg action set-window-width "$normalized"
end

function nwsname
    if test (count $argv) -eq 0
        echo "Usage: nwsname <name...>" >&2
        return 1
    end
    niri msg action set-workspace-name (string join ' ' -- $argv)
end

function nwsunname
    if test (count $argv) -eq 0
        niri msg action unset-workspace-name
    else
        niri msg action unset-workspace-name "$argv[1]"
    end
end

function nfocusws --argument-names workspace
    if test (count $argv) -ne 1
        echo "Usage: nfocusws <index|name>" >&2
        return 1
    end
    niri msg action focus-workspace "$workspace"
end

function nmovewinws --argument-names workspace
    if test (count $argv) -ne 1
        echo "Usage: nmovewinws <index|name>" >&2
        return 1
    end
    niri msg action move-window-to-workspace "$workspace"
end

function nmovecolws --argument-names workspace
    if test (count $argv) -ne 1
        echo "Usage: nmovecolws <index|name>" >&2
        return 1
    end
    niri msg action move-column-to-workspace "$workspace"
end

function ntogglews --argument-names workspace
    if test (count $argv) -ne 1
        echo "Usage: ntogglews <workspace-name>" >&2
        return 1
    end
    "$HOME/.config/niri/scripts/toggle-named-workspace.sh" "$workspace"
end
