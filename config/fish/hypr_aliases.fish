# Hyprland aliases / helpers
alias hctl="hyprctl"
alias hd="hyprctl dispatch"
alias hk="hyprctl keyword"
alias hmon="hyprctl monitors"
alias hmonj="hyprctl -j monitors"
alias hws="hyprctl workspaces"
alias hwsj="hyprctl -j workspaces"
alias haws="hyprctl activeworkspace"
alias hawsj="hyprctl -j activeworkspace"
alias hwin="hyprctl activewindow"
alias hwinj="hyprctl -j activewindow"
alias hclients="hyprctl clients"
alias hclientsj="hyprctl -j clients"
alias hbinds="hyprctl binds"
alias hbindsj="hyprctl -j binds"
alias hcfgerr="hyprctl configerrors"
alias hreload="hyprctl reload"
alias hreloadc="hyprctl reload config-only"
alias hfloat="hyprctl dispatch togglefloating"
alias hfull="hyprctl dispatch fullscreen 0"
alias hfullmax="hyprctl dispatch fullscreen 1"
alias hkillactive="hyprctl dispatch killactive"
alias hkillmode="hyprctl kill"
alias hpseudo="hyprctl dispatch pseudo"
alias hpin="hyprctl dispatch pin"
alias hsplit="hyprctl dispatch splitratio"

if type -q dms
    alias hwsrename="dms ipc call workspace-rename open"
end

function hfocusws --argument-names workspace
    if test (count $argv) -ne 1
        echo "Usage: hfocusws <workspace>" >&2
        return 1
    end
    hyprctl dispatch workspace "$workspace"
end

function hmovewinws --argument-names workspace
    if test (count $argv) -ne 1
        echo "Usage: hmovewinws <workspace>" >&2
        return 1
    end
    hyprctl dispatch movetoworkspace "$workspace"
end

function hspecial --argument-names name
    if test (count $argv) -ne 1
        echo "Usage: hspecial <name>" >&2
        return 1
    end
    hyprctl dispatch togglespecialworkspace "$name"
end

function hmovetospecial --argument-names name
    if test (count $argv) -ne 1
        echo "Usage: hmovetospecial <name>" >&2
        return 1
    end
    hyprctl dispatch movetoworkspace "special:$name"
end

function __hypr_wsaction
    set -l script "$HOME/.config/hypr/scripts/wsaction.sh"
    if not test -x "$script"
        echo "Hypr wsaction script not found or not executable: $script" >&2
        return 1
    end
    "$script" $argv
end

# Workspace in current 10-workspace group (uses your wsaction script toggle behavior).
function hwsnum --argument-names workspace_num
    if test (count $argv) -ne 1
        echo "Usage: hwsnum <1-10>" >&2
        return 1
    end
    __hypr_wsaction workspace "$workspace_num"
end

# Move active window to workspace number in current 10-workspace group.
function hmvwsnum --argument-names workspace_num
    if test (count $argv) -ne 1
        echo "Usage: hmvwsnum <1-10>" >&2
        return 1
    end
    __hypr_wsaction movetoworkspace "$workspace_num"
end

# Focus workspace number across workspace groups (1->11/21/... based on current workspace suffix).
function hwsgrp --argument-names group_num
    if test (count $argv) -ne 1
        echo "Usage: hwsgrp <group>" >&2
        return 1
    end
    __hypr_wsaction -g workspace "$group_num"
end

# Move active window across workspace groups (1->11/21/... based on current workspace suffix).
function hmvwsgrp --argument-names group_num
    if test (count $argv) -ne 1
        echo "Usage: hmvwsgrp <group>" >&2
        return 1
    end
    __hypr_wsaction -g movetoworkspace "$group_num"
end
