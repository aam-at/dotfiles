_niri() {
    local i cur prev opts cmd
    COMPREPLY=()
    if [[ "${BASH_VERSINFO[0]}" -ge 4 ]]; then
        cur="$2"
    else
        cur="${COMP_WORDS[COMP_CWORD]}"
    fi
    prev="$3"
    cmd=""
    opts=""

    for i in "${COMP_WORDS[@]:0:COMP_CWORD}"
    do
        case "${cmd},${i}" in
            ",$1")
                cmd="niri"
                ;;
            niri,completions)
                cmd="niri__completions"
                ;;
            niri,help)
                cmd="niri__help"
                ;;
            niri,msg)
                cmd="niri__msg"
                ;;
            niri,panic)
                cmd="niri__panic"
                ;;
            niri,validate)
                cmd="niri__validate"
                ;;
            niri__help,completions)
                cmd="niri__help__completions"
                ;;
            niri__help,help)
                cmd="niri__help__help"
                ;;
            niri__help,msg)
                cmd="niri__help__msg"
                ;;
            niri__help,panic)
                cmd="niri__help__panic"
                ;;
            niri__help,validate)
                cmd="niri__help__validate"
                ;;
            niri__help__msg,action)
                cmd="niri__help__msg__action"
                ;;
            niri__help__msg,event-stream)
                cmd="niri__help__msg__event__stream"
                ;;
            niri__help__msg,focused-output)
                cmd="niri__help__msg__focused__output"
                ;;
            niri__help__msg,focused-window)
                cmd="niri__help__msg__focused__window"
                ;;
            niri__help__msg,keyboard-layouts)
                cmd="niri__help__msg__keyboard__layouts"
                ;;
            niri__help__msg,layers)
                cmd="niri__help__msg__layers"
                ;;
            niri__help__msg,output)
                cmd="niri__help__msg__output"
                ;;
            niri__help__msg,outputs)
                cmd="niri__help__msg__outputs"
                ;;
            niri__help__msg,overview-state)
                cmd="niri__help__msg__overview__state"
                ;;
            niri__help__msg,pick-color)
                cmd="niri__help__msg__pick__color"
                ;;
            niri__help__msg,pick-window)
                cmd="niri__help__msg__pick__window"
                ;;
            niri__help__msg,request-error)
                cmd="niri__help__msg__request__error"
                ;;
            niri__help__msg,version)
                cmd="niri__help__msg__version"
                ;;
            niri__help__msg,windows)
                cmd="niri__help__msg__windows"
                ;;
            niri__help__msg,workspaces)
                cmd="niri__help__msg__workspaces"
                ;;
            niri__help__msg__action,center-column)
                cmd="niri__help__msg__action__center__column"
                ;;
            niri__help__msg__action,center-visible-columns)
                cmd="niri__help__msg__action__center__visible__columns"
                ;;
            niri__help__msg__action,center-window)
                cmd="niri__help__msg__action__center__window"
                ;;
            niri__help__msg__action,clear-dynamic-cast-target)
                cmd="niri__help__msg__action__clear__dynamic__cast__target"
                ;;
            niri__help__msg__action,close-overview)
                cmd="niri__help__msg__action__close__overview"
                ;;
            niri__help__msg__action,close-window)
                cmd="niri__help__msg__action__close__window"
                ;;
            niri__help__msg__action,consume-or-expel-window-left)
                cmd="niri__help__msg__action__consume__or__expel__window__left"
                ;;
            niri__help__msg__action,consume-or-expel-window-right)
                cmd="niri__help__msg__action__consume__or__expel__window__right"
                ;;
            niri__help__msg__action,consume-window-into-column)
                cmd="niri__help__msg__action__consume__window__into__column"
                ;;
            niri__help__msg__action,debug-toggle-damage)
                cmd="niri__help__msg__action__debug__toggle__damage"
                ;;
            niri__help__msg__action,debug-toggle-opaque-regions)
                cmd="niri__help__msg__action__debug__toggle__opaque__regions"
                ;;
            niri__help__msg__action,do-screen-transition)
                cmd="niri__help__msg__action__do__screen__transition"
                ;;
            niri__help__msg__action,expand-column-to-available-width)
                cmd="niri__help__msg__action__expand__column__to__available__width"
                ;;
            niri__help__msg__action,expel-window-from-column)
                cmd="niri__help__msg__action__expel__window__from__column"
                ;;
            niri__help__msg__action,focus-column)
                cmd="niri__help__msg__action__focus__column"
                ;;
            niri__help__msg__action,focus-column-first)
                cmd="niri__help__msg__action__focus__column__first"
                ;;
            niri__help__msg__action,focus-column-last)
                cmd="niri__help__msg__action__focus__column__last"
                ;;
            niri__help__msg__action,focus-column-left)
                cmd="niri__help__msg__action__focus__column__left"
                ;;
            niri__help__msg__action,focus-column-left-or-last)
                cmd="niri__help__msg__action__focus__column__left__or__last"
                ;;
            niri__help__msg__action,focus-column-or-monitor-left)
                cmd="niri__help__msg__action__focus__column__or__monitor__left"
                ;;
            niri__help__msg__action,focus-column-or-monitor-right)
                cmd="niri__help__msg__action__focus__column__or__monitor__right"
                ;;
            niri__help__msg__action,focus-column-right)
                cmd="niri__help__msg__action__focus__column__right"
                ;;
            niri__help__msg__action,focus-column-right-or-first)
                cmd="niri__help__msg__action__focus__column__right__or__first"
                ;;
            niri__help__msg__action,focus-floating)
                cmd="niri__help__msg__action__focus__floating"
                ;;
            niri__help__msg__action,focus-monitor)
                cmd="niri__help__msg__action__focus__monitor"
                ;;
            niri__help__msg__action,focus-monitor-down)
                cmd="niri__help__msg__action__focus__monitor__down"
                ;;
            niri__help__msg__action,focus-monitor-left)
                cmd="niri__help__msg__action__focus__monitor__left"
                ;;
            niri__help__msg__action,focus-monitor-next)
                cmd="niri__help__msg__action__focus__monitor__next"
                ;;
            niri__help__msg__action,focus-monitor-previous)
                cmd="niri__help__msg__action__focus__monitor__previous"
                ;;
            niri__help__msg__action,focus-monitor-right)
                cmd="niri__help__msg__action__focus__monitor__right"
                ;;
            niri__help__msg__action,focus-monitor-up)
                cmd="niri__help__msg__action__focus__monitor__up"
                ;;
            niri__help__msg__action,focus-tiling)
                cmd="niri__help__msg__action__focus__tiling"
                ;;
            niri__help__msg__action,focus-window)
                cmd="niri__help__msg__action__focus__window"
                ;;
            niri__help__msg__action,focus-window-bottom)
                cmd="niri__help__msg__action__focus__window__bottom"
                ;;
            niri__help__msg__action,focus-window-down)
                cmd="niri__help__msg__action__focus__window__down"
                ;;
            niri__help__msg__action,focus-window-down-or-column-left)
                cmd="niri__help__msg__action__focus__window__down__or__column__left"
                ;;
            niri__help__msg__action,focus-window-down-or-column-right)
                cmd="niri__help__msg__action__focus__window__down__or__column__right"
                ;;
            niri__help__msg__action,focus-window-down-or-top)
                cmd="niri__help__msg__action__focus__window__down__or__top"
                ;;
            niri__help__msg__action,focus-window-in-column)
                cmd="niri__help__msg__action__focus__window__in__column"
                ;;
            niri__help__msg__action,focus-window-or-monitor-down)
                cmd="niri__help__msg__action__focus__window__or__monitor__down"
                ;;
            niri__help__msg__action,focus-window-or-monitor-up)
                cmd="niri__help__msg__action__focus__window__or__monitor__up"
                ;;
            niri__help__msg__action,focus-window-or-workspace-down)
                cmd="niri__help__msg__action__focus__window__or__workspace__down"
                ;;
            niri__help__msg__action,focus-window-or-workspace-up)
                cmd="niri__help__msg__action__focus__window__or__workspace__up"
                ;;
            niri__help__msg__action,focus-window-previous)
                cmd="niri__help__msg__action__focus__window__previous"
                ;;
            niri__help__msg__action,focus-window-top)
                cmd="niri__help__msg__action__focus__window__top"
                ;;
            niri__help__msg__action,focus-window-up)
                cmd="niri__help__msg__action__focus__window__up"
                ;;
            niri__help__msg__action,focus-window-up-or-bottom)
                cmd="niri__help__msg__action__focus__window__up__or__bottom"
                ;;
            niri__help__msg__action,focus-window-up-or-column-left)
                cmd="niri__help__msg__action__focus__window__up__or__column__left"
                ;;
            niri__help__msg__action,focus-window-up-or-column-right)
                cmd="niri__help__msg__action__focus__window__up__or__column__right"
                ;;
            niri__help__msg__action,focus-workspace)
                cmd="niri__help__msg__action__focus__workspace"
                ;;
            niri__help__msg__action,focus-workspace-down)
                cmd="niri__help__msg__action__focus__workspace__down"
                ;;
            niri__help__msg__action,focus-workspace-previous)
                cmd="niri__help__msg__action__focus__workspace__previous"
                ;;
            niri__help__msg__action,focus-workspace-up)
                cmd="niri__help__msg__action__focus__workspace__up"
                ;;
            niri__help__msg__action,fullscreen-window)
                cmd="niri__help__msg__action__fullscreen__window"
                ;;
            niri__help__msg__action,load-config-file)
                cmd="niri__help__msg__action__load__config__file"
                ;;
            niri__help__msg__action,maximize-column)
                cmd="niri__help__msg__action__maximize__column"
                ;;
            niri__help__msg__action,maximize-window-to-edges)
                cmd="niri__help__msg__action__maximize__window__to__edges"
                ;;
            niri__help__msg__action,move-column-left)
                cmd="niri__help__msg__action__move__column__left"
                ;;
            niri__help__msg__action,move-column-left-or-to-monitor-left)
                cmd="niri__help__msg__action__move__column__left__or__to__monitor__left"
                ;;
            niri__help__msg__action,move-column-right)
                cmd="niri__help__msg__action__move__column__right"
                ;;
            niri__help__msg__action,move-column-right-or-to-monitor-right)
                cmd="niri__help__msg__action__move__column__right__or__to__monitor__right"
                ;;
            niri__help__msg__action,move-column-to-first)
                cmd="niri__help__msg__action__move__column__to__first"
                ;;
            niri__help__msg__action,move-column-to-index)
                cmd="niri__help__msg__action__move__column__to__index"
                ;;
            niri__help__msg__action,move-column-to-last)
                cmd="niri__help__msg__action__move__column__to__last"
                ;;
            niri__help__msg__action,move-column-to-monitor)
                cmd="niri__help__msg__action__move__column__to__monitor"
                ;;
            niri__help__msg__action,move-column-to-monitor-down)
                cmd="niri__help__msg__action__move__column__to__monitor__down"
                ;;
            niri__help__msg__action,move-column-to-monitor-left)
                cmd="niri__help__msg__action__move__column__to__monitor__left"
                ;;
            niri__help__msg__action,move-column-to-monitor-next)
                cmd="niri__help__msg__action__move__column__to__monitor__next"
                ;;
            niri__help__msg__action,move-column-to-monitor-previous)
                cmd="niri__help__msg__action__move__column__to__monitor__previous"
                ;;
            niri__help__msg__action,move-column-to-monitor-right)
                cmd="niri__help__msg__action__move__column__to__monitor__right"
                ;;
            niri__help__msg__action,move-column-to-monitor-up)
                cmd="niri__help__msg__action__move__column__to__monitor__up"
                ;;
            niri__help__msg__action,move-column-to-workspace)
                cmd="niri__help__msg__action__move__column__to__workspace"
                ;;
            niri__help__msg__action,move-column-to-workspace-down)
                cmd="niri__help__msg__action__move__column__to__workspace__down"
                ;;
            niri__help__msg__action,move-column-to-workspace-up)
                cmd="niri__help__msg__action__move__column__to__workspace__up"
                ;;
            niri__help__msg__action,move-floating-window)
                cmd="niri__help__msg__action__move__floating__window"
                ;;
            niri__help__msg__action,move-window-down)
                cmd="niri__help__msg__action__move__window__down"
                ;;
            niri__help__msg__action,move-window-down-or-to-workspace-down)
                cmd="niri__help__msg__action__move__window__down__or__to__workspace__down"
                ;;
            niri__help__msg__action,move-window-to-floating)
                cmd="niri__help__msg__action__move__window__to__floating"
                ;;
            niri__help__msg__action,move-window-to-monitor)
                cmd="niri__help__msg__action__move__window__to__monitor"
                ;;
            niri__help__msg__action,move-window-to-monitor-down)
                cmd="niri__help__msg__action__move__window__to__monitor__down"
                ;;
            niri__help__msg__action,move-window-to-monitor-left)
                cmd="niri__help__msg__action__move__window__to__monitor__left"
                ;;
            niri__help__msg__action,move-window-to-monitor-next)
                cmd="niri__help__msg__action__move__window__to__monitor__next"
                ;;
            niri__help__msg__action,move-window-to-monitor-previous)
                cmd="niri__help__msg__action__move__window__to__monitor__previous"
                ;;
            niri__help__msg__action,move-window-to-monitor-right)
                cmd="niri__help__msg__action__move__window__to__monitor__right"
                ;;
            niri__help__msg__action,move-window-to-monitor-up)
                cmd="niri__help__msg__action__move__window__to__monitor__up"
                ;;
            niri__help__msg__action,move-window-to-tiling)
                cmd="niri__help__msg__action__move__window__to__tiling"
                ;;
            niri__help__msg__action,move-window-to-workspace)
                cmd="niri__help__msg__action__move__window__to__workspace"
                ;;
            niri__help__msg__action,move-window-to-workspace-down)
                cmd="niri__help__msg__action__move__window__to__workspace__down"
                ;;
            niri__help__msg__action,move-window-to-workspace-up)
                cmd="niri__help__msg__action__move__window__to__workspace__up"
                ;;
            niri__help__msg__action,move-window-up)
                cmd="niri__help__msg__action__move__window__up"
                ;;
            niri__help__msg__action,move-window-up-or-to-workspace-up)
                cmd="niri__help__msg__action__move__window__up__or__to__workspace__up"
                ;;
            niri__help__msg__action,move-workspace-down)
                cmd="niri__help__msg__action__move__workspace__down"
                ;;
            niri__help__msg__action,move-workspace-to-index)
                cmd="niri__help__msg__action__move__workspace__to__index"
                ;;
            niri__help__msg__action,move-workspace-to-monitor)
                cmd="niri__help__msg__action__move__workspace__to__monitor"
                ;;
            niri__help__msg__action,move-workspace-to-monitor-down)
                cmd="niri__help__msg__action__move__workspace__to__monitor__down"
                ;;
            niri__help__msg__action,move-workspace-to-monitor-left)
                cmd="niri__help__msg__action__move__workspace__to__monitor__left"
                ;;
            niri__help__msg__action,move-workspace-to-monitor-next)
                cmd="niri__help__msg__action__move__workspace__to__monitor__next"
                ;;
            niri__help__msg__action,move-workspace-to-monitor-previous)
                cmd="niri__help__msg__action__move__workspace__to__monitor__previous"
                ;;
            niri__help__msg__action,move-workspace-to-monitor-right)
                cmd="niri__help__msg__action__move__workspace__to__monitor__right"
                ;;
            niri__help__msg__action,move-workspace-to-monitor-up)
                cmd="niri__help__msg__action__move__workspace__to__monitor__up"
                ;;
            niri__help__msg__action,move-workspace-up)
                cmd="niri__help__msg__action__move__workspace__up"
                ;;
            niri__help__msg__action,open-overview)
                cmd="niri__help__msg__action__open__overview"
                ;;
            niri__help__msg__action,power-off-monitors)
                cmd="niri__help__msg__action__power__off__monitors"
                ;;
            niri__help__msg__action,power-on-monitors)
                cmd="niri__help__msg__action__power__on__monitors"
                ;;
            niri__help__msg__action,quit)
                cmd="niri__help__msg__action__quit"
                ;;
            niri__help__msg__action,reset-window-height)
                cmd="niri__help__msg__action__reset__window__height"
                ;;
            niri__help__msg__action,screenshot)
                cmd="niri__help__msg__action__screenshot"
                ;;
            niri__help__msg__action,screenshot-screen)
                cmd="niri__help__msg__action__screenshot__screen"
                ;;
            niri__help__msg__action,screenshot-window)
                cmd="niri__help__msg__action__screenshot__window"
                ;;
            niri__help__msg__action,set-column-display)
                cmd="niri__help__msg__action__set__column__display"
                ;;
            niri__help__msg__action,set-column-width)
                cmd="niri__help__msg__action__set__column__width"
                ;;
            niri__help__msg__action,set-dynamic-cast-monitor)
                cmd="niri__help__msg__action__set__dynamic__cast__monitor"
                ;;
            niri__help__msg__action,set-dynamic-cast-window)
                cmd="niri__help__msg__action__set__dynamic__cast__window"
                ;;
            niri__help__msg__action,set-window-height)
                cmd="niri__help__msg__action__set__window__height"
                ;;
            niri__help__msg__action,set-window-urgent)
                cmd="niri__help__msg__action__set__window__urgent"
                ;;
            niri__help__msg__action,set-window-width)
                cmd="niri__help__msg__action__set__window__width"
                ;;
            niri__help__msg__action,set-workspace-name)
                cmd="niri__help__msg__action__set__workspace__name"
                ;;
            niri__help__msg__action,show-hotkey-overlay)
                cmd="niri__help__msg__action__show__hotkey__overlay"
                ;;
            niri__help__msg__action,spawn)
                cmd="niri__help__msg__action__spawn"
                ;;
            niri__help__msg__action,spawn-sh)
                cmd="niri__help__msg__action__spawn__sh"
                ;;
            niri__help__msg__action,swap-window-left)
                cmd="niri__help__msg__action__swap__window__left"
                ;;
            niri__help__msg__action,swap-window-right)
                cmd="niri__help__msg__action__swap__window__right"
                ;;
            niri__help__msg__action,switch-focus-between-floating-and-tiling)
                cmd="niri__help__msg__action__switch__focus__between__floating__and__tiling"
                ;;
            niri__help__msg__action,switch-layout)
                cmd="niri__help__msg__action__switch__layout"
                ;;
            niri__help__msg__action,switch-preset-column-width)
                cmd="niri__help__msg__action__switch__preset__column__width"
                ;;
            niri__help__msg__action,switch-preset-column-width-back)
                cmd="niri__help__msg__action__switch__preset__column__width__back"
                ;;
            niri__help__msg__action,switch-preset-window-height)
                cmd="niri__help__msg__action__switch__preset__window__height"
                ;;
            niri__help__msg__action,switch-preset-window-height-back)
                cmd="niri__help__msg__action__switch__preset__window__height__back"
                ;;
            niri__help__msg__action,switch-preset-window-width)
                cmd="niri__help__msg__action__switch__preset__window__width"
                ;;
            niri__help__msg__action,switch-preset-window-width-back)
                cmd="niri__help__msg__action__switch__preset__window__width__back"
                ;;
            niri__help__msg__action,toggle-column-tabbed-display)
                cmd="niri__help__msg__action__toggle__column__tabbed__display"
                ;;
            niri__help__msg__action,toggle-debug-tint)
                cmd="niri__help__msg__action__toggle__debug__tint"
                ;;
            niri__help__msg__action,toggle-keyboard-shortcuts-inhibit)
                cmd="niri__help__msg__action__toggle__keyboard__shortcuts__inhibit"
                ;;
            niri__help__msg__action,toggle-overview)
                cmd="niri__help__msg__action__toggle__overview"
                ;;
            niri__help__msg__action,toggle-window-floating)
                cmd="niri__help__msg__action__toggle__window__floating"
                ;;
            niri__help__msg__action,toggle-window-rule-opacity)
                cmd="niri__help__msg__action__toggle__window__rule__opacity"
                ;;
            niri__help__msg__action,toggle-window-urgent)
                cmd="niri__help__msg__action__toggle__window__urgent"
                ;;
            niri__help__msg__action,toggle-windowed-fullscreen)
                cmd="niri__help__msg__action__toggle__windowed__fullscreen"
                ;;
            niri__help__msg__action,unset-window-urgent)
                cmd="niri__help__msg__action__unset__window__urgent"
                ;;
            niri__help__msg__action,unset-workspace-name)
                cmd="niri__help__msg__action__unset__workspace__name"
                ;;
            niri__help__msg__output,custom-mode)
                cmd="niri__help__msg__output__custom__mode"
                ;;
            niri__help__msg__output,mode)
                cmd="niri__help__msg__output__mode"
                ;;
            niri__help__msg__output,modeline)
                cmd="niri__help__msg__output__modeline"
                ;;
            niri__help__msg__output,off)
                cmd="niri__help__msg__output__off"
                ;;
            niri__help__msg__output,on)
                cmd="niri__help__msg__output__on"
                ;;
            niri__help__msg__output,position)
                cmd="niri__help__msg__output__position"
                ;;
            niri__help__msg__output,scale)
                cmd="niri__help__msg__output__scale"
                ;;
            niri__help__msg__output,transform)
                cmd="niri__help__msg__output__transform"
                ;;
            niri__help__msg__output,vrr)
                cmd="niri__help__msg__output__vrr"
                ;;
            niri__help__msg__output__position,auto)
                cmd="niri__help__msg__output__position__auto"
                ;;
            niri__help__msg__output__position,set)
                cmd="niri__help__msg__output__position__set"
                ;;
            niri__msg,action)
                cmd="niri__msg__action"
                ;;
            niri__msg,event-stream)
                cmd="niri__msg__event__stream"
                ;;
            niri__msg,focused-output)
                cmd="niri__msg__focused__output"
                ;;
            niri__msg,focused-window)
                cmd="niri__msg__focused__window"
                ;;
            niri__msg,help)
                cmd="niri__msg__help"
                ;;
            niri__msg,keyboard-layouts)
                cmd="niri__msg__keyboard__layouts"
                ;;
            niri__msg,layers)
                cmd="niri__msg__layers"
                ;;
            niri__msg,output)
                cmd="niri__msg__output"
                ;;
            niri__msg,outputs)
                cmd="niri__msg__outputs"
                ;;
            niri__msg,overview-state)
                cmd="niri__msg__overview__state"
                ;;
            niri__msg,pick-color)
                cmd="niri__msg__pick__color"
                ;;
            niri__msg,pick-window)
                cmd="niri__msg__pick__window"
                ;;
            niri__msg,request-error)
                cmd="niri__msg__request__error"
                ;;
            niri__msg,version)
                cmd="niri__msg__version"
                ;;
            niri__msg,windows)
                cmd="niri__msg__windows"
                ;;
            niri__msg,workspaces)
                cmd="niri__msg__workspaces"
                ;;
            niri__msg__action,center-column)
                cmd="niri__msg__action__center__column"
                ;;
            niri__msg__action,center-visible-columns)
                cmd="niri__msg__action__center__visible__columns"
                ;;
            niri__msg__action,center-window)
                cmd="niri__msg__action__center__window"
                ;;
            niri__msg__action,clear-dynamic-cast-target)
                cmd="niri__msg__action__clear__dynamic__cast__target"
                ;;
            niri__msg__action,close-overview)
                cmd="niri__msg__action__close__overview"
                ;;
            niri__msg__action,close-window)
                cmd="niri__msg__action__close__window"
                ;;
            niri__msg__action,consume-or-expel-window-left)
                cmd="niri__msg__action__consume__or__expel__window__left"
                ;;
            niri__msg__action,consume-or-expel-window-right)
                cmd="niri__msg__action__consume__or__expel__window__right"
                ;;
            niri__msg__action,consume-window-into-column)
                cmd="niri__msg__action__consume__window__into__column"
                ;;
            niri__msg__action,debug-toggle-damage)
                cmd="niri__msg__action__debug__toggle__damage"
                ;;
            niri__msg__action,debug-toggle-opaque-regions)
                cmd="niri__msg__action__debug__toggle__opaque__regions"
                ;;
            niri__msg__action,do-screen-transition)
                cmd="niri__msg__action__do__screen__transition"
                ;;
            niri__msg__action,expand-column-to-available-width)
                cmd="niri__msg__action__expand__column__to__available__width"
                ;;
            niri__msg__action,expel-window-from-column)
                cmd="niri__msg__action__expel__window__from__column"
                ;;
            niri__msg__action,focus-column)
                cmd="niri__msg__action__focus__column"
                ;;
            niri__msg__action,focus-column-first)
                cmd="niri__msg__action__focus__column__first"
                ;;
            niri__msg__action,focus-column-last)
                cmd="niri__msg__action__focus__column__last"
                ;;
            niri__msg__action,focus-column-left)
                cmd="niri__msg__action__focus__column__left"
                ;;
            niri__msg__action,focus-column-left-or-last)
                cmd="niri__msg__action__focus__column__left__or__last"
                ;;
            niri__msg__action,focus-column-or-monitor-left)
                cmd="niri__msg__action__focus__column__or__monitor__left"
                ;;
            niri__msg__action,focus-column-or-monitor-right)
                cmd="niri__msg__action__focus__column__or__monitor__right"
                ;;
            niri__msg__action,focus-column-right)
                cmd="niri__msg__action__focus__column__right"
                ;;
            niri__msg__action,focus-column-right-or-first)
                cmd="niri__msg__action__focus__column__right__or__first"
                ;;
            niri__msg__action,focus-floating)
                cmd="niri__msg__action__focus__floating"
                ;;
            niri__msg__action,focus-monitor)
                cmd="niri__msg__action__focus__monitor"
                ;;
            niri__msg__action,focus-monitor-down)
                cmd="niri__msg__action__focus__monitor__down"
                ;;
            niri__msg__action,focus-monitor-left)
                cmd="niri__msg__action__focus__monitor__left"
                ;;
            niri__msg__action,focus-monitor-next)
                cmd="niri__msg__action__focus__monitor__next"
                ;;
            niri__msg__action,focus-monitor-previous)
                cmd="niri__msg__action__focus__monitor__previous"
                ;;
            niri__msg__action,focus-monitor-right)
                cmd="niri__msg__action__focus__monitor__right"
                ;;
            niri__msg__action,focus-monitor-up)
                cmd="niri__msg__action__focus__monitor__up"
                ;;
            niri__msg__action,focus-tiling)
                cmd="niri__msg__action__focus__tiling"
                ;;
            niri__msg__action,focus-window)
                cmd="niri__msg__action__focus__window"
                ;;
            niri__msg__action,focus-window-bottom)
                cmd="niri__msg__action__focus__window__bottom"
                ;;
            niri__msg__action,focus-window-down)
                cmd="niri__msg__action__focus__window__down"
                ;;
            niri__msg__action,focus-window-down-or-column-left)
                cmd="niri__msg__action__focus__window__down__or__column__left"
                ;;
            niri__msg__action,focus-window-down-or-column-right)
                cmd="niri__msg__action__focus__window__down__or__column__right"
                ;;
            niri__msg__action,focus-window-down-or-top)
                cmd="niri__msg__action__focus__window__down__or__top"
                ;;
            niri__msg__action,focus-window-in-column)
                cmd="niri__msg__action__focus__window__in__column"
                ;;
            niri__msg__action,focus-window-or-monitor-down)
                cmd="niri__msg__action__focus__window__or__monitor__down"
                ;;
            niri__msg__action,focus-window-or-monitor-up)
                cmd="niri__msg__action__focus__window__or__monitor__up"
                ;;
            niri__msg__action,focus-window-or-workspace-down)
                cmd="niri__msg__action__focus__window__or__workspace__down"
                ;;
            niri__msg__action,focus-window-or-workspace-up)
                cmd="niri__msg__action__focus__window__or__workspace__up"
                ;;
            niri__msg__action,focus-window-previous)
                cmd="niri__msg__action__focus__window__previous"
                ;;
            niri__msg__action,focus-window-top)
                cmd="niri__msg__action__focus__window__top"
                ;;
            niri__msg__action,focus-window-up)
                cmd="niri__msg__action__focus__window__up"
                ;;
            niri__msg__action,focus-window-up-or-bottom)
                cmd="niri__msg__action__focus__window__up__or__bottom"
                ;;
            niri__msg__action,focus-window-up-or-column-left)
                cmd="niri__msg__action__focus__window__up__or__column__left"
                ;;
            niri__msg__action,focus-window-up-or-column-right)
                cmd="niri__msg__action__focus__window__up__or__column__right"
                ;;
            niri__msg__action,focus-workspace)
                cmd="niri__msg__action__focus__workspace"
                ;;
            niri__msg__action,focus-workspace-down)
                cmd="niri__msg__action__focus__workspace__down"
                ;;
            niri__msg__action,focus-workspace-previous)
                cmd="niri__msg__action__focus__workspace__previous"
                ;;
            niri__msg__action,focus-workspace-up)
                cmd="niri__msg__action__focus__workspace__up"
                ;;
            niri__msg__action,fullscreen-window)
                cmd="niri__msg__action__fullscreen__window"
                ;;
            niri__msg__action,help)
                cmd="niri__msg__action__help"
                ;;
            niri__msg__action,load-config-file)
                cmd="niri__msg__action__load__config__file"
                ;;
            niri__msg__action,maximize-column)
                cmd="niri__msg__action__maximize__column"
                ;;
            niri__msg__action,maximize-window-to-edges)
                cmd="niri__msg__action__maximize__window__to__edges"
                ;;
            niri__msg__action,move-column-left)
                cmd="niri__msg__action__move__column__left"
                ;;
            niri__msg__action,move-column-left-or-to-monitor-left)
                cmd="niri__msg__action__move__column__left__or__to__monitor__left"
                ;;
            niri__msg__action,move-column-right)
                cmd="niri__msg__action__move__column__right"
                ;;
            niri__msg__action,move-column-right-or-to-monitor-right)
                cmd="niri__msg__action__move__column__right__or__to__monitor__right"
                ;;
            niri__msg__action,move-column-to-first)
                cmd="niri__msg__action__move__column__to__first"
                ;;
            niri__msg__action,move-column-to-index)
                cmd="niri__msg__action__move__column__to__index"
                ;;
            niri__msg__action,move-column-to-last)
                cmd="niri__msg__action__move__column__to__last"
                ;;
            niri__msg__action,move-column-to-monitor)
                cmd="niri__msg__action__move__column__to__monitor"
                ;;
            niri__msg__action,move-column-to-monitor-down)
                cmd="niri__msg__action__move__column__to__monitor__down"
                ;;
            niri__msg__action,move-column-to-monitor-left)
                cmd="niri__msg__action__move__column__to__monitor__left"
                ;;
            niri__msg__action,move-column-to-monitor-next)
                cmd="niri__msg__action__move__column__to__monitor__next"
                ;;
            niri__msg__action,move-column-to-monitor-previous)
                cmd="niri__msg__action__move__column__to__monitor__previous"
                ;;
            niri__msg__action,move-column-to-monitor-right)
                cmd="niri__msg__action__move__column__to__monitor__right"
                ;;
            niri__msg__action,move-column-to-monitor-up)
                cmd="niri__msg__action__move__column__to__monitor__up"
                ;;
            niri__msg__action,move-column-to-workspace)
                cmd="niri__msg__action__move__column__to__workspace"
                ;;
            niri__msg__action,move-column-to-workspace-down)
                cmd="niri__msg__action__move__column__to__workspace__down"
                ;;
            niri__msg__action,move-column-to-workspace-up)
                cmd="niri__msg__action__move__column__to__workspace__up"
                ;;
            niri__msg__action,move-floating-window)
                cmd="niri__msg__action__move__floating__window"
                ;;
            niri__msg__action,move-window-down)
                cmd="niri__msg__action__move__window__down"
                ;;
            niri__msg__action,move-window-down-or-to-workspace-down)
                cmd="niri__msg__action__move__window__down__or__to__workspace__down"
                ;;
            niri__msg__action,move-window-to-floating)
                cmd="niri__msg__action__move__window__to__floating"
                ;;
            niri__msg__action,move-window-to-monitor)
                cmd="niri__msg__action__move__window__to__monitor"
                ;;
            niri__msg__action,move-window-to-monitor-down)
                cmd="niri__msg__action__move__window__to__monitor__down"
                ;;
            niri__msg__action,move-window-to-monitor-left)
                cmd="niri__msg__action__move__window__to__monitor__left"
                ;;
            niri__msg__action,move-window-to-monitor-next)
                cmd="niri__msg__action__move__window__to__monitor__next"
                ;;
            niri__msg__action,move-window-to-monitor-previous)
                cmd="niri__msg__action__move__window__to__monitor__previous"
                ;;
            niri__msg__action,move-window-to-monitor-right)
                cmd="niri__msg__action__move__window__to__monitor__right"
                ;;
            niri__msg__action,move-window-to-monitor-up)
                cmd="niri__msg__action__move__window__to__monitor__up"
                ;;
            niri__msg__action,move-window-to-tiling)
                cmd="niri__msg__action__move__window__to__tiling"
                ;;
            niri__msg__action,move-window-to-workspace)
                cmd="niri__msg__action__move__window__to__workspace"
                ;;
            niri__msg__action,move-window-to-workspace-down)
                cmd="niri__msg__action__move__window__to__workspace__down"
                ;;
            niri__msg__action,move-window-to-workspace-up)
                cmd="niri__msg__action__move__window__to__workspace__up"
                ;;
            niri__msg__action,move-window-up)
                cmd="niri__msg__action__move__window__up"
                ;;
            niri__msg__action,move-window-up-or-to-workspace-up)
                cmd="niri__msg__action__move__window__up__or__to__workspace__up"
                ;;
            niri__msg__action,move-workspace-down)
                cmd="niri__msg__action__move__workspace__down"
                ;;
            niri__msg__action,move-workspace-to-index)
                cmd="niri__msg__action__move__workspace__to__index"
                ;;
            niri__msg__action,move-workspace-to-monitor)
                cmd="niri__msg__action__move__workspace__to__monitor"
                ;;
            niri__msg__action,move-workspace-to-monitor-down)
                cmd="niri__msg__action__move__workspace__to__monitor__down"
                ;;
            niri__msg__action,move-workspace-to-monitor-left)
                cmd="niri__msg__action__move__workspace__to__monitor__left"
                ;;
            niri__msg__action,move-workspace-to-monitor-next)
                cmd="niri__msg__action__move__workspace__to__monitor__next"
                ;;
            niri__msg__action,move-workspace-to-monitor-previous)
                cmd="niri__msg__action__move__workspace__to__monitor__previous"
                ;;
            niri__msg__action,move-workspace-to-monitor-right)
                cmd="niri__msg__action__move__workspace__to__monitor__right"
                ;;
            niri__msg__action,move-workspace-to-monitor-up)
                cmd="niri__msg__action__move__workspace__to__monitor__up"
                ;;
            niri__msg__action,move-workspace-up)
                cmd="niri__msg__action__move__workspace__up"
                ;;
            niri__msg__action,open-overview)
                cmd="niri__msg__action__open__overview"
                ;;
            niri__msg__action,power-off-monitors)
                cmd="niri__msg__action__power__off__monitors"
                ;;
            niri__msg__action,power-on-monitors)
                cmd="niri__msg__action__power__on__monitors"
                ;;
            niri__msg__action,quit)
                cmd="niri__msg__action__quit"
                ;;
            niri__msg__action,reset-window-height)
                cmd="niri__msg__action__reset__window__height"
                ;;
            niri__msg__action,screenshot)
                cmd="niri__msg__action__screenshot"
                ;;
            niri__msg__action,screenshot-screen)
                cmd="niri__msg__action__screenshot__screen"
                ;;
            niri__msg__action,screenshot-window)
                cmd="niri__msg__action__screenshot__window"
                ;;
            niri__msg__action,set-column-display)
                cmd="niri__msg__action__set__column__display"
                ;;
            niri__msg__action,set-column-width)
                cmd="niri__msg__action__set__column__width"
                ;;
            niri__msg__action,set-dynamic-cast-monitor)
                cmd="niri__msg__action__set__dynamic__cast__monitor"
                ;;
            niri__msg__action,set-dynamic-cast-window)
                cmd="niri__msg__action__set__dynamic__cast__window"
                ;;
            niri__msg__action,set-window-height)
                cmd="niri__msg__action__set__window__height"
                ;;
            niri__msg__action,set-window-urgent)
                cmd="niri__msg__action__set__window__urgent"
                ;;
            niri__msg__action,set-window-width)
                cmd="niri__msg__action__set__window__width"
                ;;
            niri__msg__action,set-workspace-name)
                cmd="niri__msg__action__set__workspace__name"
                ;;
            niri__msg__action,show-hotkey-overlay)
                cmd="niri__msg__action__show__hotkey__overlay"
                ;;
            niri__msg__action,spawn)
                cmd="niri__msg__action__spawn"
                ;;
            niri__msg__action,spawn-sh)
                cmd="niri__msg__action__spawn__sh"
                ;;
            niri__msg__action,swap-window-left)
                cmd="niri__msg__action__swap__window__left"
                ;;
            niri__msg__action,swap-window-right)
                cmd="niri__msg__action__swap__window__right"
                ;;
            niri__msg__action,switch-focus-between-floating-and-tiling)
                cmd="niri__msg__action__switch__focus__between__floating__and__tiling"
                ;;
            niri__msg__action,switch-layout)
                cmd="niri__msg__action__switch__layout"
                ;;
            niri__msg__action,switch-preset-column-width)
                cmd="niri__msg__action__switch__preset__column__width"
                ;;
            niri__msg__action,switch-preset-column-width-back)
                cmd="niri__msg__action__switch__preset__column__width__back"
                ;;
            niri__msg__action,switch-preset-window-height)
                cmd="niri__msg__action__switch__preset__window__height"
                ;;
            niri__msg__action,switch-preset-window-height-back)
                cmd="niri__msg__action__switch__preset__window__height__back"
                ;;
            niri__msg__action,switch-preset-window-width)
                cmd="niri__msg__action__switch__preset__window__width"
                ;;
            niri__msg__action,switch-preset-window-width-back)
                cmd="niri__msg__action__switch__preset__window__width__back"
                ;;
            niri__msg__action,toggle-column-tabbed-display)
                cmd="niri__msg__action__toggle__column__tabbed__display"
                ;;
            niri__msg__action,toggle-debug-tint)
                cmd="niri__msg__action__toggle__debug__tint"
                ;;
            niri__msg__action,toggle-keyboard-shortcuts-inhibit)
                cmd="niri__msg__action__toggle__keyboard__shortcuts__inhibit"
                ;;
            niri__msg__action,toggle-overview)
                cmd="niri__msg__action__toggle__overview"
                ;;
            niri__msg__action,toggle-window-floating)
                cmd="niri__msg__action__toggle__window__floating"
                ;;
            niri__msg__action,toggle-window-rule-opacity)
                cmd="niri__msg__action__toggle__window__rule__opacity"
                ;;
            niri__msg__action,toggle-window-urgent)
                cmd="niri__msg__action__toggle__window__urgent"
                ;;
            niri__msg__action,toggle-windowed-fullscreen)
                cmd="niri__msg__action__toggle__windowed__fullscreen"
                ;;
            niri__msg__action,unset-window-urgent)
                cmd="niri__msg__action__unset__window__urgent"
                ;;
            niri__msg__action,unset-workspace-name)
                cmd="niri__msg__action__unset__workspace__name"
                ;;
            niri__msg__action__help,center-column)
                cmd="niri__msg__action__help__center__column"
                ;;
            niri__msg__action__help,center-visible-columns)
                cmd="niri__msg__action__help__center__visible__columns"
                ;;
            niri__msg__action__help,center-window)
                cmd="niri__msg__action__help__center__window"
                ;;
            niri__msg__action__help,clear-dynamic-cast-target)
                cmd="niri__msg__action__help__clear__dynamic__cast__target"
                ;;
            niri__msg__action__help,close-overview)
                cmd="niri__msg__action__help__close__overview"
                ;;
            niri__msg__action__help,close-window)
                cmd="niri__msg__action__help__close__window"
                ;;
            niri__msg__action__help,consume-or-expel-window-left)
                cmd="niri__msg__action__help__consume__or__expel__window__left"
                ;;
            niri__msg__action__help,consume-or-expel-window-right)
                cmd="niri__msg__action__help__consume__or__expel__window__right"
                ;;
            niri__msg__action__help,consume-window-into-column)
                cmd="niri__msg__action__help__consume__window__into__column"
                ;;
            niri__msg__action__help,debug-toggle-damage)
                cmd="niri__msg__action__help__debug__toggle__damage"
                ;;
            niri__msg__action__help,debug-toggle-opaque-regions)
                cmd="niri__msg__action__help__debug__toggle__opaque__regions"
                ;;
            niri__msg__action__help,do-screen-transition)
                cmd="niri__msg__action__help__do__screen__transition"
                ;;
            niri__msg__action__help,expand-column-to-available-width)
                cmd="niri__msg__action__help__expand__column__to__available__width"
                ;;
            niri__msg__action__help,expel-window-from-column)
                cmd="niri__msg__action__help__expel__window__from__column"
                ;;
            niri__msg__action__help,focus-column)
                cmd="niri__msg__action__help__focus__column"
                ;;
            niri__msg__action__help,focus-column-first)
                cmd="niri__msg__action__help__focus__column__first"
                ;;
            niri__msg__action__help,focus-column-last)
                cmd="niri__msg__action__help__focus__column__last"
                ;;
            niri__msg__action__help,focus-column-left)
                cmd="niri__msg__action__help__focus__column__left"
                ;;
            niri__msg__action__help,focus-column-left-or-last)
                cmd="niri__msg__action__help__focus__column__left__or__last"
                ;;
            niri__msg__action__help,focus-column-or-monitor-left)
                cmd="niri__msg__action__help__focus__column__or__monitor__left"
                ;;
            niri__msg__action__help,focus-column-or-monitor-right)
                cmd="niri__msg__action__help__focus__column__or__monitor__right"
                ;;
            niri__msg__action__help,focus-column-right)
                cmd="niri__msg__action__help__focus__column__right"
                ;;
            niri__msg__action__help,focus-column-right-or-first)
                cmd="niri__msg__action__help__focus__column__right__or__first"
                ;;
            niri__msg__action__help,focus-floating)
                cmd="niri__msg__action__help__focus__floating"
                ;;
            niri__msg__action__help,focus-monitor)
                cmd="niri__msg__action__help__focus__monitor"
                ;;
            niri__msg__action__help,focus-monitor-down)
                cmd="niri__msg__action__help__focus__monitor__down"
                ;;
            niri__msg__action__help,focus-monitor-left)
                cmd="niri__msg__action__help__focus__monitor__left"
                ;;
            niri__msg__action__help,focus-monitor-next)
                cmd="niri__msg__action__help__focus__monitor__next"
                ;;
            niri__msg__action__help,focus-monitor-previous)
                cmd="niri__msg__action__help__focus__monitor__previous"
                ;;
            niri__msg__action__help,focus-monitor-right)
                cmd="niri__msg__action__help__focus__monitor__right"
                ;;
            niri__msg__action__help,focus-monitor-up)
                cmd="niri__msg__action__help__focus__monitor__up"
                ;;
            niri__msg__action__help,focus-tiling)
                cmd="niri__msg__action__help__focus__tiling"
                ;;
            niri__msg__action__help,focus-window)
                cmd="niri__msg__action__help__focus__window"
                ;;
            niri__msg__action__help,focus-window-bottom)
                cmd="niri__msg__action__help__focus__window__bottom"
                ;;
            niri__msg__action__help,focus-window-down)
                cmd="niri__msg__action__help__focus__window__down"
                ;;
            niri__msg__action__help,focus-window-down-or-column-left)
                cmd="niri__msg__action__help__focus__window__down__or__column__left"
                ;;
            niri__msg__action__help,focus-window-down-or-column-right)
                cmd="niri__msg__action__help__focus__window__down__or__column__right"
                ;;
            niri__msg__action__help,focus-window-down-or-top)
                cmd="niri__msg__action__help__focus__window__down__or__top"
                ;;
            niri__msg__action__help,focus-window-in-column)
                cmd="niri__msg__action__help__focus__window__in__column"
                ;;
            niri__msg__action__help,focus-window-or-monitor-down)
                cmd="niri__msg__action__help__focus__window__or__monitor__down"
                ;;
            niri__msg__action__help,focus-window-or-monitor-up)
                cmd="niri__msg__action__help__focus__window__or__monitor__up"
                ;;
            niri__msg__action__help,focus-window-or-workspace-down)
                cmd="niri__msg__action__help__focus__window__or__workspace__down"
                ;;
            niri__msg__action__help,focus-window-or-workspace-up)
                cmd="niri__msg__action__help__focus__window__or__workspace__up"
                ;;
            niri__msg__action__help,focus-window-previous)
                cmd="niri__msg__action__help__focus__window__previous"
                ;;
            niri__msg__action__help,focus-window-top)
                cmd="niri__msg__action__help__focus__window__top"
                ;;
            niri__msg__action__help,focus-window-up)
                cmd="niri__msg__action__help__focus__window__up"
                ;;
            niri__msg__action__help,focus-window-up-or-bottom)
                cmd="niri__msg__action__help__focus__window__up__or__bottom"
                ;;
            niri__msg__action__help,focus-window-up-or-column-left)
                cmd="niri__msg__action__help__focus__window__up__or__column__left"
                ;;
            niri__msg__action__help,focus-window-up-or-column-right)
                cmd="niri__msg__action__help__focus__window__up__or__column__right"
                ;;
            niri__msg__action__help,focus-workspace)
                cmd="niri__msg__action__help__focus__workspace"
                ;;
            niri__msg__action__help,focus-workspace-down)
                cmd="niri__msg__action__help__focus__workspace__down"
                ;;
            niri__msg__action__help,focus-workspace-previous)
                cmd="niri__msg__action__help__focus__workspace__previous"
                ;;
            niri__msg__action__help,focus-workspace-up)
                cmd="niri__msg__action__help__focus__workspace__up"
                ;;
            niri__msg__action__help,fullscreen-window)
                cmd="niri__msg__action__help__fullscreen__window"
                ;;
            niri__msg__action__help,help)
                cmd="niri__msg__action__help__help"
                ;;
            niri__msg__action__help,load-config-file)
                cmd="niri__msg__action__help__load__config__file"
                ;;
            niri__msg__action__help,maximize-column)
                cmd="niri__msg__action__help__maximize__column"
                ;;
            niri__msg__action__help,maximize-window-to-edges)
                cmd="niri__msg__action__help__maximize__window__to__edges"
                ;;
            niri__msg__action__help,move-column-left)
                cmd="niri__msg__action__help__move__column__left"
                ;;
            niri__msg__action__help,move-column-left-or-to-monitor-left)
                cmd="niri__msg__action__help__move__column__left__or__to__monitor__left"
                ;;
            niri__msg__action__help,move-column-right)
                cmd="niri__msg__action__help__move__column__right"
                ;;
            niri__msg__action__help,move-column-right-or-to-monitor-right)
                cmd="niri__msg__action__help__move__column__right__or__to__monitor__right"
                ;;
            niri__msg__action__help,move-column-to-first)
                cmd="niri__msg__action__help__move__column__to__first"
                ;;
            niri__msg__action__help,move-column-to-index)
                cmd="niri__msg__action__help__move__column__to__index"
                ;;
            niri__msg__action__help,move-column-to-last)
                cmd="niri__msg__action__help__move__column__to__last"
                ;;
            niri__msg__action__help,move-column-to-monitor)
                cmd="niri__msg__action__help__move__column__to__monitor"
                ;;
            niri__msg__action__help,move-column-to-monitor-down)
                cmd="niri__msg__action__help__move__column__to__monitor__down"
                ;;
            niri__msg__action__help,move-column-to-monitor-left)
                cmd="niri__msg__action__help__move__column__to__monitor__left"
                ;;
            niri__msg__action__help,move-column-to-monitor-next)
                cmd="niri__msg__action__help__move__column__to__monitor__next"
                ;;
            niri__msg__action__help,move-column-to-monitor-previous)
                cmd="niri__msg__action__help__move__column__to__monitor__previous"
                ;;
            niri__msg__action__help,move-column-to-monitor-right)
                cmd="niri__msg__action__help__move__column__to__monitor__right"
                ;;
            niri__msg__action__help,move-column-to-monitor-up)
                cmd="niri__msg__action__help__move__column__to__monitor__up"
                ;;
            niri__msg__action__help,move-column-to-workspace)
                cmd="niri__msg__action__help__move__column__to__workspace"
                ;;
            niri__msg__action__help,move-column-to-workspace-down)
                cmd="niri__msg__action__help__move__column__to__workspace__down"
                ;;
            niri__msg__action__help,move-column-to-workspace-up)
                cmd="niri__msg__action__help__move__column__to__workspace__up"
                ;;
            niri__msg__action__help,move-floating-window)
                cmd="niri__msg__action__help__move__floating__window"
                ;;
            niri__msg__action__help,move-window-down)
                cmd="niri__msg__action__help__move__window__down"
                ;;
            niri__msg__action__help,move-window-down-or-to-workspace-down)
                cmd="niri__msg__action__help__move__window__down__or__to__workspace__down"
                ;;
            niri__msg__action__help,move-window-to-floating)
                cmd="niri__msg__action__help__move__window__to__floating"
                ;;
            niri__msg__action__help,move-window-to-monitor)
                cmd="niri__msg__action__help__move__window__to__monitor"
                ;;
            niri__msg__action__help,move-window-to-monitor-down)
                cmd="niri__msg__action__help__move__window__to__monitor__down"
                ;;
            niri__msg__action__help,move-window-to-monitor-left)
                cmd="niri__msg__action__help__move__window__to__monitor__left"
                ;;
            niri__msg__action__help,move-window-to-monitor-next)
                cmd="niri__msg__action__help__move__window__to__monitor__next"
                ;;
            niri__msg__action__help,move-window-to-monitor-previous)
                cmd="niri__msg__action__help__move__window__to__monitor__previous"
                ;;
            niri__msg__action__help,move-window-to-monitor-right)
                cmd="niri__msg__action__help__move__window__to__monitor__right"
                ;;
            niri__msg__action__help,move-window-to-monitor-up)
                cmd="niri__msg__action__help__move__window__to__monitor__up"
                ;;
            niri__msg__action__help,move-window-to-tiling)
                cmd="niri__msg__action__help__move__window__to__tiling"
                ;;
            niri__msg__action__help,move-window-to-workspace)
                cmd="niri__msg__action__help__move__window__to__workspace"
                ;;
            niri__msg__action__help,move-window-to-workspace-down)
                cmd="niri__msg__action__help__move__window__to__workspace__down"
                ;;
            niri__msg__action__help,move-window-to-workspace-up)
                cmd="niri__msg__action__help__move__window__to__workspace__up"
                ;;
            niri__msg__action__help,move-window-up)
                cmd="niri__msg__action__help__move__window__up"
                ;;
            niri__msg__action__help,move-window-up-or-to-workspace-up)
                cmd="niri__msg__action__help__move__window__up__or__to__workspace__up"
                ;;
            niri__msg__action__help,move-workspace-down)
                cmd="niri__msg__action__help__move__workspace__down"
                ;;
            niri__msg__action__help,move-workspace-to-index)
                cmd="niri__msg__action__help__move__workspace__to__index"
                ;;
            niri__msg__action__help,move-workspace-to-monitor)
                cmd="niri__msg__action__help__move__workspace__to__monitor"
                ;;
            niri__msg__action__help,move-workspace-to-monitor-down)
                cmd="niri__msg__action__help__move__workspace__to__monitor__down"
                ;;
            niri__msg__action__help,move-workspace-to-monitor-left)
                cmd="niri__msg__action__help__move__workspace__to__monitor__left"
                ;;
            niri__msg__action__help,move-workspace-to-monitor-next)
                cmd="niri__msg__action__help__move__workspace__to__monitor__next"
                ;;
            niri__msg__action__help,move-workspace-to-monitor-previous)
                cmd="niri__msg__action__help__move__workspace__to__monitor__previous"
                ;;
            niri__msg__action__help,move-workspace-to-monitor-right)
                cmd="niri__msg__action__help__move__workspace__to__monitor__right"
                ;;
            niri__msg__action__help,move-workspace-to-monitor-up)
                cmd="niri__msg__action__help__move__workspace__to__monitor__up"
                ;;
            niri__msg__action__help,move-workspace-up)
                cmd="niri__msg__action__help__move__workspace__up"
                ;;
            niri__msg__action__help,open-overview)
                cmd="niri__msg__action__help__open__overview"
                ;;
            niri__msg__action__help,power-off-monitors)
                cmd="niri__msg__action__help__power__off__monitors"
                ;;
            niri__msg__action__help,power-on-monitors)
                cmd="niri__msg__action__help__power__on__monitors"
                ;;
            niri__msg__action__help,quit)
                cmd="niri__msg__action__help__quit"
                ;;
            niri__msg__action__help,reset-window-height)
                cmd="niri__msg__action__help__reset__window__height"
                ;;
            niri__msg__action__help,screenshot)
                cmd="niri__msg__action__help__screenshot"
                ;;
            niri__msg__action__help,screenshot-screen)
                cmd="niri__msg__action__help__screenshot__screen"
                ;;
            niri__msg__action__help,screenshot-window)
                cmd="niri__msg__action__help__screenshot__window"
                ;;
            niri__msg__action__help,set-column-display)
                cmd="niri__msg__action__help__set__column__display"
                ;;
            niri__msg__action__help,set-column-width)
                cmd="niri__msg__action__help__set__column__width"
                ;;
            niri__msg__action__help,set-dynamic-cast-monitor)
                cmd="niri__msg__action__help__set__dynamic__cast__monitor"
                ;;
            niri__msg__action__help,set-dynamic-cast-window)
                cmd="niri__msg__action__help__set__dynamic__cast__window"
                ;;
            niri__msg__action__help,set-window-height)
                cmd="niri__msg__action__help__set__window__height"
                ;;
            niri__msg__action__help,set-window-urgent)
                cmd="niri__msg__action__help__set__window__urgent"
                ;;
            niri__msg__action__help,set-window-width)
                cmd="niri__msg__action__help__set__window__width"
                ;;
            niri__msg__action__help,set-workspace-name)
                cmd="niri__msg__action__help__set__workspace__name"
                ;;
            niri__msg__action__help,show-hotkey-overlay)
                cmd="niri__msg__action__help__show__hotkey__overlay"
                ;;
            niri__msg__action__help,spawn)
                cmd="niri__msg__action__help__spawn"
                ;;
            niri__msg__action__help,spawn-sh)
                cmd="niri__msg__action__help__spawn__sh"
                ;;
            niri__msg__action__help,swap-window-left)
                cmd="niri__msg__action__help__swap__window__left"
                ;;
            niri__msg__action__help,swap-window-right)
                cmd="niri__msg__action__help__swap__window__right"
                ;;
            niri__msg__action__help,switch-focus-between-floating-and-tiling)
                cmd="niri__msg__action__help__switch__focus__between__floating__and__tiling"
                ;;
            niri__msg__action__help,switch-layout)
                cmd="niri__msg__action__help__switch__layout"
                ;;
            niri__msg__action__help,switch-preset-column-width)
                cmd="niri__msg__action__help__switch__preset__column__width"
                ;;
            niri__msg__action__help,switch-preset-column-width-back)
                cmd="niri__msg__action__help__switch__preset__column__width__back"
                ;;
            niri__msg__action__help,switch-preset-window-height)
                cmd="niri__msg__action__help__switch__preset__window__height"
                ;;
            niri__msg__action__help,switch-preset-window-height-back)
                cmd="niri__msg__action__help__switch__preset__window__height__back"
                ;;
            niri__msg__action__help,switch-preset-window-width)
                cmd="niri__msg__action__help__switch__preset__window__width"
                ;;
            niri__msg__action__help,switch-preset-window-width-back)
                cmd="niri__msg__action__help__switch__preset__window__width__back"
                ;;
            niri__msg__action__help,toggle-column-tabbed-display)
                cmd="niri__msg__action__help__toggle__column__tabbed__display"
                ;;
            niri__msg__action__help,toggle-debug-tint)
                cmd="niri__msg__action__help__toggle__debug__tint"
                ;;
            niri__msg__action__help,toggle-keyboard-shortcuts-inhibit)
                cmd="niri__msg__action__help__toggle__keyboard__shortcuts__inhibit"
                ;;
            niri__msg__action__help,toggle-overview)
                cmd="niri__msg__action__help__toggle__overview"
                ;;
            niri__msg__action__help,toggle-window-floating)
                cmd="niri__msg__action__help__toggle__window__floating"
                ;;
            niri__msg__action__help,toggle-window-rule-opacity)
                cmd="niri__msg__action__help__toggle__window__rule__opacity"
                ;;
            niri__msg__action__help,toggle-window-urgent)
                cmd="niri__msg__action__help__toggle__window__urgent"
                ;;
            niri__msg__action__help,toggle-windowed-fullscreen)
                cmd="niri__msg__action__help__toggle__windowed__fullscreen"
                ;;
            niri__msg__action__help,unset-window-urgent)
                cmd="niri__msg__action__help__unset__window__urgent"
                ;;
            niri__msg__action__help,unset-workspace-name)
                cmd="niri__msg__action__help__unset__workspace__name"
                ;;
            niri__msg__help,action)
                cmd="niri__msg__help__action"
                ;;
            niri__msg__help,event-stream)
                cmd="niri__msg__help__event__stream"
                ;;
            niri__msg__help,focused-output)
                cmd="niri__msg__help__focused__output"
                ;;
            niri__msg__help,focused-window)
                cmd="niri__msg__help__focused__window"
                ;;
            niri__msg__help,help)
                cmd="niri__msg__help__help"
                ;;
            niri__msg__help,keyboard-layouts)
                cmd="niri__msg__help__keyboard__layouts"
                ;;
            niri__msg__help,layers)
                cmd="niri__msg__help__layers"
                ;;
            niri__msg__help,output)
                cmd="niri__msg__help__output"
                ;;
            niri__msg__help,outputs)
                cmd="niri__msg__help__outputs"
                ;;
            niri__msg__help,overview-state)
                cmd="niri__msg__help__overview__state"
                ;;
            niri__msg__help,pick-color)
                cmd="niri__msg__help__pick__color"
                ;;
            niri__msg__help,pick-window)
                cmd="niri__msg__help__pick__window"
                ;;
            niri__msg__help,request-error)
                cmd="niri__msg__help__request__error"
                ;;
            niri__msg__help,version)
                cmd="niri__msg__help__version"
                ;;
            niri__msg__help,windows)
                cmd="niri__msg__help__windows"
                ;;
            niri__msg__help,workspaces)
                cmd="niri__msg__help__workspaces"
                ;;
            niri__msg__help__action,center-column)
                cmd="niri__msg__help__action__center__column"
                ;;
            niri__msg__help__action,center-visible-columns)
                cmd="niri__msg__help__action__center__visible__columns"
                ;;
            niri__msg__help__action,center-window)
                cmd="niri__msg__help__action__center__window"
                ;;
            niri__msg__help__action,clear-dynamic-cast-target)
                cmd="niri__msg__help__action__clear__dynamic__cast__target"
                ;;
            niri__msg__help__action,close-overview)
                cmd="niri__msg__help__action__close__overview"
                ;;
            niri__msg__help__action,close-window)
                cmd="niri__msg__help__action__close__window"
                ;;
            niri__msg__help__action,consume-or-expel-window-left)
                cmd="niri__msg__help__action__consume__or__expel__window__left"
                ;;
            niri__msg__help__action,consume-or-expel-window-right)
                cmd="niri__msg__help__action__consume__or__expel__window__right"
                ;;
            niri__msg__help__action,consume-window-into-column)
                cmd="niri__msg__help__action__consume__window__into__column"
                ;;
            niri__msg__help__action,debug-toggle-damage)
                cmd="niri__msg__help__action__debug__toggle__damage"
                ;;
            niri__msg__help__action,debug-toggle-opaque-regions)
                cmd="niri__msg__help__action__debug__toggle__opaque__regions"
                ;;
            niri__msg__help__action,do-screen-transition)
                cmd="niri__msg__help__action__do__screen__transition"
                ;;
            niri__msg__help__action,expand-column-to-available-width)
                cmd="niri__msg__help__action__expand__column__to__available__width"
                ;;
            niri__msg__help__action,expel-window-from-column)
                cmd="niri__msg__help__action__expel__window__from__column"
                ;;
            niri__msg__help__action,focus-column)
                cmd="niri__msg__help__action__focus__column"
                ;;
            niri__msg__help__action,focus-column-first)
                cmd="niri__msg__help__action__focus__column__first"
                ;;
            niri__msg__help__action,focus-column-last)
                cmd="niri__msg__help__action__focus__column__last"
                ;;
            niri__msg__help__action,focus-column-left)
                cmd="niri__msg__help__action__focus__column__left"
                ;;
            niri__msg__help__action,focus-column-left-or-last)
                cmd="niri__msg__help__action__focus__column__left__or__last"
                ;;
            niri__msg__help__action,focus-column-or-monitor-left)
                cmd="niri__msg__help__action__focus__column__or__monitor__left"
                ;;
            niri__msg__help__action,focus-column-or-monitor-right)
                cmd="niri__msg__help__action__focus__column__or__monitor__right"
                ;;
            niri__msg__help__action,focus-column-right)
                cmd="niri__msg__help__action__focus__column__right"
                ;;
            niri__msg__help__action,focus-column-right-or-first)
                cmd="niri__msg__help__action__focus__column__right__or__first"
                ;;
            niri__msg__help__action,focus-floating)
                cmd="niri__msg__help__action__focus__floating"
                ;;
            niri__msg__help__action,focus-monitor)
                cmd="niri__msg__help__action__focus__monitor"
                ;;
            niri__msg__help__action,focus-monitor-down)
                cmd="niri__msg__help__action__focus__monitor__down"
                ;;
            niri__msg__help__action,focus-monitor-left)
                cmd="niri__msg__help__action__focus__monitor__left"
                ;;
            niri__msg__help__action,focus-monitor-next)
                cmd="niri__msg__help__action__focus__monitor__next"
                ;;
            niri__msg__help__action,focus-monitor-previous)
                cmd="niri__msg__help__action__focus__monitor__previous"
                ;;
            niri__msg__help__action,focus-monitor-right)
                cmd="niri__msg__help__action__focus__monitor__right"
                ;;
            niri__msg__help__action,focus-monitor-up)
                cmd="niri__msg__help__action__focus__monitor__up"
                ;;
            niri__msg__help__action,focus-tiling)
                cmd="niri__msg__help__action__focus__tiling"
                ;;
            niri__msg__help__action,focus-window)
                cmd="niri__msg__help__action__focus__window"
                ;;
            niri__msg__help__action,focus-window-bottom)
                cmd="niri__msg__help__action__focus__window__bottom"
                ;;
            niri__msg__help__action,focus-window-down)
                cmd="niri__msg__help__action__focus__window__down"
                ;;
            niri__msg__help__action,focus-window-down-or-column-left)
                cmd="niri__msg__help__action__focus__window__down__or__column__left"
                ;;
            niri__msg__help__action,focus-window-down-or-column-right)
                cmd="niri__msg__help__action__focus__window__down__or__column__right"
                ;;
            niri__msg__help__action,focus-window-down-or-top)
                cmd="niri__msg__help__action__focus__window__down__or__top"
                ;;
            niri__msg__help__action,focus-window-in-column)
                cmd="niri__msg__help__action__focus__window__in__column"
                ;;
            niri__msg__help__action,focus-window-or-monitor-down)
                cmd="niri__msg__help__action__focus__window__or__monitor__down"
                ;;
            niri__msg__help__action,focus-window-or-monitor-up)
                cmd="niri__msg__help__action__focus__window__or__monitor__up"
                ;;
            niri__msg__help__action,focus-window-or-workspace-down)
                cmd="niri__msg__help__action__focus__window__or__workspace__down"
                ;;
            niri__msg__help__action,focus-window-or-workspace-up)
                cmd="niri__msg__help__action__focus__window__or__workspace__up"
                ;;
            niri__msg__help__action,focus-window-previous)
                cmd="niri__msg__help__action__focus__window__previous"
                ;;
            niri__msg__help__action,focus-window-top)
                cmd="niri__msg__help__action__focus__window__top"
                ;;
            niri__msg__help__action,focus-window-up)
                cmd="niri__msg__help__action__focus__window__up"
                ;;
            niri__msg__help__action,focus-window-up-or-bottom)
                cmd="niri__msg__help__action__focus__window__up__or__bottom"
                ;;
            niri__msg__help__action,focus-window-up-or-column-left)
                cmd="niri__msg__help__action__focus__window__up__or__column__left"
                ;;
            niri__msg__help__action,focus-window-up-or-column-right)
                cmd="niri__msg__help__action__focus__window__up__or__column__right"
                ;;
            niri__msg__help__action,focus-workspace)
                cmd="niri__msg__help__action__focus__workspace"
                ;;
            niri__msg__help__action,focus-workspace-down)
                cmd="niri__msg__help__action__focus__workspace__down"
                ;;
            niri__msg__help__action,focus-workspace-previous)
                cmd="niri__msg__help__action__focus__workspace__previous"
                ;;
            niri__msg__help__action,focus-workspace-up)
                cmd="niri__msg__help__action__focus__workspace__up"
                ;;
            niri__msg__help__action,fullscreen-window)
                cmd="niri__msg__help__action__fullscreen__window"
                ;;
            niri__msg__help__action,load-config-file)
                cmd="niri__msg__help__action__load__config__file"
                ;;
            niri__msg__help__action,maximize-column)
                cmd="niri__msg__help__action__maximize__column"
                ;;
            niri__msg__help__action,maximize-window-to-edges)
                cmd="niri__msg__help__action__maximize__window__to__edges"
                ;;
            niri__msg__help__action,move-column-left)
                cmd="niri__msg__help__action__move__column__left"
                ;;
            niri__msg__help__action,move-column-left-or-to-monitor-left)
                cmd="niri__msg__help__action__move__column__left__or__to__monitor__left"
                ;;
            niri__msg__help__action,move-column-right)
                cmd="niri__msg__help__action__move__column__right"
                ;;
            niri__msg__help__action,move-column-right-or-to-monitor-right)
                cmd="niri__msg__help__action__move__column__right__or__to__monitor__right"
                ;;
            niri__msg__help__action,move-column-to-first)
                cmd="niri__msg__help__action__move__column__to__first"
                ;;
            niri__msg__help__action,move-column-to-index)
                cmd="niri__msg__help__action__move__column__to__index"
                ;;
            niri__msg__help__action,move-column-to-last)
                cmd="niri__msg__help__action__move__column__to__last"
                ;;
            niri__msg__help__action,move-column-to-monitor)
                cmd="niri__msg__help__action__move__column__to__monitor"
                ;;
            niri__msg__help__action,move-column-to-monitor-down)
                cmd="niri__msg__help__action__move__column__to__monitor__down"
                ;;
            niri__msg__help__action,move-column-to-monitor-left)
                cmd="niri__msg__help__action__move__column__to__monitor__left"
                ;;
            niri__msg__help__action,move-column-to-monitor-next)
                cmd="niri__msg__help__action__move__column__to__monitor__next"
                ;;
            niri__msg__help__action,move-column-to-monitor-previous)
                cmd="niri__msg__help__action__move__column__to__monitor__previous"
                ;;
            niri__msg__help__action,move-column-to-monitor-right)
                cmd="niri__msg__help__action__move__column__to__monitor__right"
                ;;
            niri__msg__help__action,move-column-to-monitor-up)
                cmd="niri__msg__help__action__move__column__to__monitor__up"
                ;;
            niri__msg__help__action,move-column-to-workspace)
                cmd="niri__msg__help__action__move__column__to__workspace"
                ;;
            niri__msg__help__action,move-column-to-workspace-down)
                cmd="niri__msg__help__action__move__column__to__workspace__down"
                ;;
            niri__msg__help__action,move-column-to-workspace-up)
                cmd="niri__msg__help__action__move__column__to__workspace__up"
                ;;
            niri__msg__help__action,move-floating-window)
                cmd="niri__msg__help__action__move__floating__window"
                ;;
            niri__msg__help__action,move-window-down)
                cmd="niri__msg__help__action__move__window__down"
                ;;
            niri__msg__help__action,move-window-down-or-to-workspace-down)
                cmd="niri__msg__help__action__move__window__down__or__to__workspace__down"
                ;;
            niri__msg__help__action,move-window-to-floating)
                cmd="niri__msg__help__action__move__window__to__floating"
                ;;
            niri__msg__help__action,move-window-to-monitor)
                cmd="niri__msg__help__action__move__window__to__monitor"
                ;;
            niri__msg__help__action,move-window-to-monitor-down)
                cmd="niri__msg__help__action__move__window__to__monitor__down"
                ;;
            niri__msg__help__action,move-window-to-monitor-left)
                cmd="niri__msg__help__action__move__window__to__monitor__left"
                ;;
            niri__msg__help__action,move-window-to-monitor-next)
                cmd="niri__msg__help__action__move__window__to__monitor__next"
                ;;
            niri__msg__help__action,move-window-to-monitor-previous)
                cmd="niri__msg__help__action__move__window__to__monitor__previous"
                ;;
            niri__msg__help__action,move-window-to-monitor-right)
                cmd="niri__msg__help__action__move__window__to__monitor__right"
                ;;
            niri__msg__help__action,move-window-to-monitor-up)
                cmd="niri__msg__help__action__move__window__to__monitor__up"
                ;;
            niri__msg__help__action,move-window-to-tiling)
                cmd="niri__msg__help__action__move__window__to__tiling"
                ;;
            niri__msg__help__action,move-window-to-workspace)
                cmd="niri__msg__help__action__move__window__to__workspace"
                ;;
            niri__msg__help__action,move-window-to-workspace-down)
                cmd="niri__msg__help__action__move__window__to__workspace__down"
                ;;
            niri__msg__help__action,move-window-to-workspace-up)
                cmd="niri__msg__help__action__move__window__to__workspace__up"
                ;;
            niri__msg__help__action,move-window-up)
                cmd="niri__msg__help__action__move__window__up"
                ;;
            niri__msg__help__action,move-window-up-or-to-workspace-up)
                cmd="niri__msg__help__action__move__window__up__or__to__workspace__up"
                ;;
            niri__msg__help__action,move-workspace-down)
                cmd="niri__msg__help__action__move__workspace__down"
                ;;
            niri__msg__help__action,move-workspace-to-index)
                cmd="niri__msg__help__action__move__workspace__to__index"
                ;;
            niri__msg__help__action,move-workspace-to-monitor)
                cmd="niri__msg__help__action__move__workspace__to__monitor"
                ;;
            niri__msg__help__action,move-workspace-to-monitor-down)
                cmd="niri__msg__help__action__move__workspace__to__monitor__down"
                ;;
            niri__msg__help__action,move-workspace-to-monitor-left)
                cmd="niri__msg__help__action__move__workspace__to__monitor__left"
                ;;
            niri__msg__help__action,move-workspace-to-monitor-next)
                cmd="niri__msg__help__action__move__workspace__to__monitor__next"
                ;;
            niri__msg__help__action,move-workspace-to-monitor-previous)
                cmd="niri__msg__help__action__move__workspace__to__monitor__previous"
                ;;
            niri__msg__help__action,move-workspace-to-monitor-right)
                cmd="niri__msg__help__action__move__workspace__to__monitor__right"
                ;;
            niri__msg__help__action,move-workspace-to-monitor-up)
                cmd="niri__msg__help__action__move__workspace__to__monitor__up"
                ;;
            niri__msg__help__action,move-workspace-up)
                cmd="niri__msg__help__action__move__workspace__up"
                ;;
            niri__msg__help__action,open-overview)
                cmd="niri__msg__help__action__open__overview"
                ;;
            niri__msg__help__action,power-off-monitors)
                cmd="niri__msg__help__action__power__off__monitors"
                ;;
            niri__msg__help__action,power-on-monitors)
                cmd="niri__msg__help__action__power__on__monitors"
                ;;
            niri__msg__help__action,quit)
                cmd="niri__msg__help__action__quit"
                ;;
            niri__msg__help__action,reset-window-height)
                cmd="niri__msg__help__action__reset__window__height"
                ;;
            niri__msg__help__action,screenshot)
                cmd="niri__msg__help__action__screenshot"
                ;;
            niri__msg__help__action,screenshot-screen)
                cmd="niri__msg__help__action__screenshot__screen"
                ;;
            niri__msg__help__action,screenshot-window)
                cmd="niri__msg__help__action__screenshot__window"
                ;;
            niri__msg__help__action,set-column-display)
                cmd="niri__msg__help__action__set__column__display"
                ;;
            niri__msg__help__action,set-column-width)
                cmd="niri__msg__help__action__set__column__width"
                ;;
            niri__msg__help__action,set-dynamic-cast-monitor)
                cmd="niri__msg__help__action__set__dynamic__cast__monitor"
                ;;
            niri__msg__help__action,set-dynamic-cast-window)
                cmd="niri__msg__help__action__set__dynamic__cast__window"
                ;;
            niri__msg__help__action,set-window-height)
                cmd="niri__msg__help__action__set__window__height"
                ;;
            niri__msg__help__action,set-window-urgent)
                cmd="niri__msg__help__action__set__window__urgent"
                ;;
            niri__msg__help__action,set-window-width)
                cmd="niri__msg__help__action__set__window__width"
                ;;
            niri__msg__help__action,set-workspace-name)
                cmd="niri__msg__help__action__set__workspace__name"
                ;;
            niri__msg__help__action,show-hotkey-overlay)
                cmd="niri__msg__help__action__show__hotkey__overlay"
                ;;
            niri__msg__help__action,spawn)
                cmd="niri__msg__help__action__spawn"
                ;;
            niri__msg__help__action,spawn-sh)
                cmd="niri__msg__help__action__spawn__sh"
                ;;
            niri__msg__help__action,swap-window-left)
                cmd="niri__msg__help__action__swap__window__left"
                ;;
            niri__msg__help__action,swap-window-right)
                cmd="niri__msg__help__action__swap__window__right"
                ;;
            niri__msg__help__action,switch-focus-between-floating-and-tiling)
                cmd="niri__msg__help__action__switch__focus__between__floating__and__tiling"
                ;;
            niri__msg__help__action,switch-layout)
                cmd="niri__msg__help__action__switch__layout"
                ;;
            niri__msg__help__action,switch-preset-column-width)
                cmd="niri__msg__help__action__switch__preset__column__width"
                ;;
            niri__msg__help__action,switch-preset-column-width-back)
                cmd="niri__msg__help__action__switch__preset__column__width__back"
                ;;
            niri__msg__help__action,switch-preset-window-height)
                cmd="niri__msg__help__action__switch__preset__window__height"
                ;;
            niri__msg__help__action,switch-preset-window-height-back)
                cmd="niri__msg__help__action__switch__preset__window__height__back"
                ;;
            niri__msg__help__action,switch-preset-window-width)
                cmd="niri__msg__help__action__switch__preset__window__width"
                ;;
            niri__msg__help__action,switch-preset-window-width-back)
                cmd="niri__msg__help__action__switch__preset__window__width__back"
                ;;
            niri__msg__help__action,toggle-column-tabbed-display)
                cmd="niri__msg__help__action__toggle__column__tabbed__display"
                ;;
            niri__msg__help__action,toggle-debug-tint)
                cmd="niri__msg__help__action__toggle__debug__tint"
                ;;
            niri__msg__help__action,toggle-keyboard-shortcuts-inhibit)
                cmd="niri__msg__help__action__toggle__keyboard__shortcuts__inhibit"
                ;;
            niri__msg__help__action,toggle-overview)
                cmd="niri__msg__help__action__toggle__overview"
                ;;
            niri__msg__help__action,toggle-window-floating)
                cmd="niri__msg__help__action__toggle__window__floating"
                ;;
            niri__msg__help__action,toggle-window-rule-opacity)
                cmd="niri__msg__help__action__toggle__window__rule__opacity"
                ;;
            niri__msg__help__action,toggle-window-urgent)
                cmd="niri__msg__help__action__toggle__window__urgent"
                ;;
            niri__msg__help__action,toggle-windowed-fullscreen)
                cmd="niri__msg__help__action__toggle__windowed__fullscreen"
                ;;
            niri__msg__help__action,unset-window-urgent)
                cmd="niri__msg__help__action__unset__window__urgent"
                ;;
            niri__msg__help__action,unset-workspace-name)
                cmd="niri__msg__help__action__unset__workspace__name"
                ;;
            niri__msg__help__output,custom-mode)
                cmd="niri__msg__help__output__custom__mode"
                ;;
            niri__msg__help__output,mode)
                cmd="niri__msg__help__output__mode"
                ;;
            niri__msg__help__output,modeline)
                cmd="niri__msg__help__output__modeline"
                ;;
            niri__msg__help__output,off)
                cmd="niri__msg__help__output__off"
                ;;
            niri__msg__help__output,on)
                cmd="niri__msg__help__output__on"
                ;;
            niri__msg__help__output,position)
                cmd="niri__msg__help__output__position"
                ;;
            niri__msg__help__output,scale)
                cmd="niri__msg__help__output__scale"
                ;;
            niri__msg__help__output,transform)
                cmd="niri__msg__help__output__transform"
                ;;
            niri__msg__help__output,vrr)
                cmd="niri__msg__help__output__vrr"
                ;;
            niri__msg__help__output__position,auto)
                cmd="niri__msg__help__output__position__auto"
                ;;
            niri__msg__help__output__position,set)
                cmd="niri__msg__help__output__position__set"
                ;;
            niri__msg__output,custom-mode)
                cmd="niri__msg__output__custom__mode"
                ;;
            niri__msg__output,help)
                cmd="niri__msg__output__help"
                ;;
            niri__msg__output,mode)
                cmd="niri__msg__output__mode"
                ;;
            niri__msg__output,modeline)
                cmd="niri__msg__output__modeline"
                ;;
            niri__msg__output,off)
                cmd="niri__msg__output__off"
                ;;
            niri__msg__output,on)
                cmd="niri__msg__output__on"
                ;;
            niri__msg__output,position)
                cmd="niri__msg__output__position"
                ;;
            niri__msg__output,scale)
                cmd="niri__msg__output__scale"
                ;;
            niri__msg__output,transform)
                cmd="niri__msg__output__transform"
                ;;
            niri__msg__output,vrr)
                cmd="niri__msg__output__vrr"
                ;;
            niri__msg__output__help,custom-mode)
                cmd="niri__msg__output__help__custom__mode"
                ;;
            niri__msg__output__help,help)
                cmd="niri__msg__output__help__help"
                ;;
            niri__msg__output__help,mode)
                cmd="niri__msg__output__help__mode"
                ;;
            niri__msg__output__help,modeline)
                cmd="niri__msg__output__help__modeline"
                ;;
            niri__msg__output__help,off)
                cmd="niri__msg__output__help__off"
                ;;
            niri__msg__output__help,on)
                cmd="niri__msg__output__help__on"
                ;;
            niri__msg__output__help,position)
                cmd="niri__msg__output__help__position"
                ;;
            niri__msg__output__help,scale)
                cmd="niri__msg__output__help__scale"
                ;;
            niri__msg__output__help,transform)
                cmd="niri__msg__output__help__transform"
                ;;
            niri__msg__output__help,vrr)
                cmd="niri__msg__output__help__vrr"
                ;;
            niri__msg__output__help__position,auto)
                cmd="niri__msg__output__help__position__auto"
                ;;
            niri__msg__output__help__position,set)
                cmd="niri__msg__output__help__position__set"
                ;;
            niri__msg__output__position,auto)
                cmd="niri__msg__output__position__auto"
                ;;
            niri__msg__output__position,help)
                cmd="niri__msg__output__position__help"
                ;;
            niri__msg__output__position,set)
                cmd="niri__msg__output__position__set"
                ;;
            niri__msg__output__position__help,auto)
                cmd="niri__msg__output__position__help__auto"
                ;;
            niri__msg__output__position__help,help)
                cmd="niri__msg__output__position__help__help"
                ;;
            niri__msg__output__position__help,set)
                cmd="niri__msg__output__position__help__set"
                ;;
            *)
                ;;
        esac
    done

    case "${cmd}" in
        niri)
            opts="-c -h -V --config --session --help --version [COMMAND]... msg validate panic completions help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 1 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --config)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -c)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__completions)
            opts="-h --help bash elvish fish power-shell zsh nushell"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help)
            opts="msg validate panic completions help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__completions)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg)
            opts="outputs workspaces windows layers keyboard-layouts focused-output focused-window pick-window pick-color action output event-stream version request-error overview-state"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action)
            opts="quit power-off-monitors power-on-monitors spawn spawn-sh do-screen-transition screenshot screenshot-screen screenshot-window toggle-keyboard-shortcuts-inhibit close-window fullscreen-window toggle-windowed-fullscreen focus-window focus-window-in-column focus-window-previous focus-column-left focus-column-right focus-column-first focus-column-last focus-column-right-or-first focus-column-left-or-last focus-column focus-window-or-monitor-up focus-window-or-monitor-down focus-column-or-monitor-left focus-column-or-monitor-right focus-window-down focus-window-up focus-window-down-or-column-left focus-window-down-or-column-right focus-window-up-or-column-left focus-window-up-or-column-right focus-window-or-workspace-down focus-window-or-workspace-up focus-window-top focus-window-bottom focus-window-down-or-top focus-window-up-or-bottom move-column-left move-column-right move-column-to-first move-column-to-last move-column-left-or-to-monitor-left move-column-right-or-to-monitor-right move-column-to-index move-window-down move-window-up move-window-down-or-to-workspace-down move-window-up-or-to-workspace-up consume-or-expel-window-left consume-or-expel-window-right consume-window-into-column expel-window-from-column swap-window-right swap-window-left toggle-column-tabbed-display set-column-display center-column center-window center-visible-columns focus-workspace-down focus-workspace-up focus-workspace focus-workspace-previous move-window-to-workspace-down move-window-to-workspace-up move-window-to-workspace move-column-to-workspace-down move-column-to-workspace-up move-column-to-workspace move-workspace-down move-workspace-up move-workspace-to-index set-workspace-name unset-workspace-name focus-monitor-left focus-monitor-right focus-monitor-down focus-monitor-up focus-monitor-previous focus-monitor-next focus-monitor move-window-to-monitor-left move-window-to-monitor-right move-window-to-monitor-down move-window-to-monitor-up move-window-to-monitor-previous move-window-to-monitor-next move-window-to-monitor move-column-to-monitor-left move-column-to-monitor-right move-column-to-monitor-down move-column-to-monitor-up move-column-to-monitor-previous move-column-to-monitor-next move-column-to-monitor set-window-width set-window-height reset-window-height switch-preset-column-width switch-preset-column-width-back switch-preset-window-width switch-preset-window-width-back switch-preset-window-height switch-preset-window-height-back maximize-column maximize-window-to-edges set-column-width expand-column-to-available-width switch-layout show-hotkey-overlay move-workspace-to-monitor-left move-workspace-to-monitor-right move-workspace-to-monitor-down move-workspace-to-monitor-up move-workspace-to-monitor-previous move-workspace-to-monitor-next move-workspace-to-monitor toggle-debug-tint debug-toggle-opaque-regions debug-toggle-damage toggle-window-floating move-window-to-floating move-window-to-tiling focus-floating focus-tiling switch-focus-between-floating-and-tiling move-floating-window toggle-window-rule-opacity set-dynamic-cast-window set-dynamic-cast-monitor clear-dynamic-cast-target toggle-overview open-overview close-overview toggle-window-urgent set-window-urgent unset-window-urgent load-config-file"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__center__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__center__visible__columns)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__center__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__clear__dynamic__cast__target)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__close__overview)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__close__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__consume__or__expel__window__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__consume__or__expel__window__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__consume__window__into__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__debug__toggle__damage)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__debug__toggle__opaque__regions)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__do__screen__transition)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__expand__column__to__available__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__expel__window__from__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__column__first)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__column__last)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__column__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__column__left__or__last)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__column__or__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__column__or__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__column__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__column__right__or__first)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__floating)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__monitor__next)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__monitor__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__tiling)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__bottom)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__down__or__column__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__down__or__column__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__down__or__top)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__in__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__or__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__or__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__or__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__or__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__top)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__up__or__bottom)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__up__or__column__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__window__up__or__column__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__workspace)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__workspace__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__focus__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__fullscreen__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__load__config__file)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__maximize__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__maximize__window__to__edges)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__left__or__to__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__right__or__to__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__to__first)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__to__index)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__to__last)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__to__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__to__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__to__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__to__monitor__next)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__to__monitor__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__to__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__to__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__to__workspace)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__to__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__column__to__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__floating__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__down__or__to__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__to__floating)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__to__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__to__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__to__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__to__monitor__next)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__to__monitor__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__to__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__to__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__to__tiling)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__to__workspace)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__to__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__to__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__window__up__or__to__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__workspace__to__index)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__workspace__to__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__workspace__to__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__workspace__to__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__workspace__to__monitor__next)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__workspace__to__monitor__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__workspace__to__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__workspace__to__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__move__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__open__overview)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__power__off__monitors)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__power__on__monitors)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__quit)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__reset__window__height)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__screenshot)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__screenshot__screen)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__screenshot__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__set__column__display)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__set__column__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__set__dynamic__cast__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__set__dynamic__cast__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__set__window__height)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__set__window__urgent)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__set__window__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__set__workspace__name)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__show__hotkey__overlay)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__spawn)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__spawn__sh)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__swap__window__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__swap__window__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__switch__focus__between__floating__and__tiling)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__switch__layout)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__switch__preset__column__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__switch__preset__column__width__back)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__switch__preset__window__height)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__switch__preset__window__height__back)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__switch__preset__window__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__switch__preset__window__width__back)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__toggle__column__tabbed__display)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__toggle__debug__tint)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__toggle__keyboard__shortcuts__inhibit)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__toggle__overview)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__toggle__window__floating)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__toggle__window__rule__opacity)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__toggle__window__urgent)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__toggle__windowed__fullscreen)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__unset__window__urgent)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__action__unset__workspace__name)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__event__stream)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__focused__output)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__focused__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__keyboard__layouts)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__layers)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__output)
            opts="off on mode custom-mode modeline scale transform position vrr"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__output__custom__mode)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__output__mode)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__output__modeline)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__output__off)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__output__on)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__output__position)
            opts="auto set"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__output__position__auto)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__output__position__set)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__output__scale)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__output__transform)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__output__vrr)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__outputs)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__overview__state)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__pick__color)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__pick__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__request__error)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__version)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__windows)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__msg__workspaces)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__panic)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__help__validate)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg)
            opts="-j -h --json --help outputs workspaces windows layers keyboard-layouts focused-output focused-window pick-window pick-color action output event-stream version request-error overview-state help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action)
            opts="-h --help quit power-off-monitors power-on-monitors spawn spawn-sh do-screen-transition screenshot screenshot-screen screenshot-window toggle-keyboard-shortcuts-inhibit close-window fullscreen-window toggle-windowed-fullscreen focus-window focus-window-in-column focus-window-previous focus-column-left focus-column-right focus-column-first focus-column-last focus-column-right-or-first focus-column-left-or-last focus-column focus-window-or-monitor-up focus-window-or-monitor-down focus-column-or-monitor-left focus-column-or-monitor-right focus-window-down focus-window-up focus-window-down-or-column-left focus-window-down-or-column-right focus-window-up-or-column-left focus-window-up-or-column-right focus-window-or-workspace-down focus-window-or-workspace-up focus-window-top focus-window-bottom focus-window-down-or-top focus-window-up-or-bottom move-column-left move-column-right move-column-to-first move-column-to-last move-column-left-or-to-monitor-left move-column-right-or-to-monitor-right move-column-to-index move-window-down move-window-up move-window-down-or-to-workspace-down move-window-up-or-to-workspace-up consume-or-expel-window-left consume-or-expel-window-right consume-window-into-column expel-window-from-column swap-window-right swap-window-left toggle-column-tabbed-display set-column-display center-column center-window center-visible-columns focus-workspace-down focus-workspace-up focus-workspace focus-workspace-previous move-window-to-workspace-down move-window-to-workspace-up move-window-to-workspace move-column-to-workspace-down move-column-to-workspace-up move-column-to-workspace move-workspace-down move-workspace-up move-workspace-to-index set-workspace-name unset-workspace-name focus-monitor-left focus-monitor-right focus-monitor-down focus-monitor-up focus-monitor-previous focus-monitor-next focus-monitor move-window-to-monitor-left move-window-to-monitor-right move-window-to-monitor-down move-window-to-monitor-up move-window-to-monitor-previous move-window-to-monitor-next move-window-to-monitor move-column-to-monitor-left move-column-to-monitor-right move-column-to-monitor-down move-column-to-monitor-up move-column-to-monitor-previous move-column-to-monitor-next move-column-to-monitor set-window-width set-window-height reset-window-height switch-preset-column-width switch-preset-column-width-back switch-preset-window-width switch-preset-window-width-back switch-preset-window-height switch-preset-window-height-back maximize-column maximize-window-to-edges set-column-width expand-column-to-available-width switch-layout show-hotkey-overlay move-workspace-to-monitor-left move-workspace-to-monitor-right move-workspace-to-monitor-down move-workspace-to-monitor-up move-workspace-to-monitor-previous move-workspace-to-monitor-next move-workspace-to-monitor toggle-debug-tint debug-toggle-opaque-regions debug-toggle-damage toggle-window-floating move-window-to-floating move-window-to-tiling focus-floating focus-tiling switch-focus-between-floating-and-tiling move-floating-window toggle-window-rule-opacity set-dynamic-cast-window set-dynamic-cast-monitor clear-dynamic-cast-target toggle-overview open-overview close-overview toggle-window-urgent set-window-urgent unset-window-urgent load-config-file help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__center__column)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__center__visible__columns)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__center__window)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__clear__dynamic__cast__target)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__close__overview)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__close__window)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__consume__or__expel__window__left)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__consume__or__expel__window__right)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__consume__window__into__column)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__debug__toggle__damage)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__debug__toggle__opaque__regions)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__do__screen__transition)
            opts="-d -h --delay-ms --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --delay-ms)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -d)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__expand__column__to__available__width)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__expel__window__from__column)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__column)
            opts="-h --help <INDEX>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__column__first)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__column__last)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__column__left)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__column__left__or__last)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__column__or__monitor__left)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__column__or__monitor__right)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__column__right)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__column__right__or__first)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__floating)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__monitor)
            opts="-h --help <OUTPUT>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__monitor__down)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__monitor__left)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__monitor__next)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__monitor__previous)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__monitor__right)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__monitor__up)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__tiling)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__bottom)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__down)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__down__or__column__left)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__down__or__column__right)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__down__or__top)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__in__column)
            opts="-h --help <INDEX>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__or__monitor__down)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__or__monitor__up)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__or__workspace__down)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__or__workspace__up)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__previous)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__top)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__up)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__up__or__bottom)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__up__or__column__left)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__window__up__or__column__right)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__workspace)
            opts="-h --help <REFERENCE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__workspace__down)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__workspace__previous)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__focus__workspace__up)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__fullscreen__window)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help)
            opts="quit power-off-monitors power-on-monitors spawn spawn-sh do-screen-transition screenshot screenshot-screen screenshot-window toggle-keyboard-shortcuts-inhibit close-window fullscreen-window toggle-windowed-fullscreen focus-window focus-window-in-column focus-window-previous focus-column-left focus-column-right focus-column-first focus-column-last focus-column-right-or-first focus-column-left-or-last focus-column focus-window-or-monitor-up focus-window-or-monitor-down focus-column-or-monitor-left focus-column-or-monitor-right focus-window-down focus-window-up focus-window-down-or-column-left focus-window-down-or-column-right focus-window-up-or-column-left focus-window-up-or-column-right focus-window-or-workspace-down focus-window-or-workspace-up focus-window-top focus-window-bottom focus-window-down-or-top focus-window-up-or-bottom move-column-left move-column-right move-column-to-first move-column-to-last move-column-left-or-to-monitor-left move-column-right-or-to-monitor-right move-column-to-index move-window-down move-window-up move-window-down-or-to-workspace-down move-window-up-or-to-workspace-up consume-or-expel-window-left consume-or-expel-window-right consume-window-into-column expel-window-from-column swap-window-right swap-window-left toggle-column-tabbed-display set-column-display center-column center-window center-visible-columns focus-workspace-down focus-workspace-up focus-workspace focus-workspace-previous move-window-to-workspace-down move-window-to-workspace-up move-window-to-workspace move-column-to-workspace-down move-column-to-workspace-up move-column-to-workspace move-workspace-down move-workspace-up move-workspace-to-index set-workspace-name unset-workspace-name focus-monitor-left focus-monitor-right focus-monitor-down focus-monitor-up focus-monitor-previous focus-monitor-next focus-monitor move-window-to-monitor-left move-window-to-monitor-right move-window-to-monitor-down move-window-to-monitor-up move-window-to-monitor-previous move-window-to-monitor-next move-window-to-monitor move-column-to-monitor-left move-column-to-monitor-right move-column-to-monitor-down move-column-to-monitor-up move-column-to-monitor-previous move-column-to-monitor-next move-column-to-monitor set-window-width set-window-height reset-window-height switch-preset-column-width switch-preset-column-width-back switch-preset-window-width switch-preset-window-width-back switch-preset-window-height switch-preset-window-height-back maximize-column maximize-window-to-edges set-column-width expand-column-to-available-width switch-layout show-hotkey-overlay move-workspace-to-monitor-left move-workspace-to-monitor-right move-workspace-to-monitor-down move-workspace-to-monitor-up move-workspace-to-monitor-previous move-workspace-to-monitor-next move-workspace-to-monitor toggle-debug-tint debug-toggle-opaque-regions debug-toggle-damage toggle-window-floating move-window-to-floating move-window-to-tiling focus-floating focus-tiling switch-focus-between-floating-and-tiling move-floating-window toggle-window-rule-opacity set-dynamic-cast-window set-dynamic-cast-monitor clear-dynamic-cast-target toggle-overview open-overview close-overview toggle-window-urgent set-window-urgent unset-window-urgent load-config-file help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__center__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__center__visible__columns)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__center__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__clear__dynamic__cast__target)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__close__overview)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__close__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__consume__or__expel__window__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__consume__or__expel__window__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__consume__window__into__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__debug__toggle__damage)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__debug__toggle__opaque__regions)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__do__screen__transition)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__expand__column__to__available__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__expel__window__from__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__column__first)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__column__last)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__column__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__column__left__or__last)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__column__or__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__column__or__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__column__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__column__right__or__first)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__floating)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__monitor__next)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__monitor__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__tiling)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__bottom)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__down__or__column__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__down__or__column__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__down__or__top)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__in__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__or__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__or__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__or__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__or__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__top)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__up__or__bottom)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__up__or__column__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__window__up__or__column__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__workspace)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__workspace__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__focus__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__fullscreen__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__load__config__file)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__maximize__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__maximize__window__to__edges)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__left__or__to__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__right__or__to__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__to__first)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__to__index)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__to__last)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__to__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__to__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__to__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__to__monitor__next)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__to__monitor__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__to__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__to__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__to__workspace)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__to__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__column__to__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__floating__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__down__or__to__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__to__floating)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__to__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__to__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__to__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__to__monitor__next)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__to__monitor__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__to__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__to__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__to__tiling)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__to__workspace)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__to__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__to__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__window__up__or__to__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__workspace__to__index)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__workspace__to__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__workspace__to__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__workspace__to__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__workspace__to__monitor__next)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__workspace__to__monitor__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__workspace__to__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__workspace__to__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__move__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__open__overview)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__power__off__monitors)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__power__on__monitors)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__quit)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__reset__window__height)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__screenshot)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__screenshot__screen)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__screenshot__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__set__column__display)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__set__column__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__set__dynamic__cast__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__set__dynamic__cast__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__set__window__height)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__set__window__urgent)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__set__window__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__set__workspace__name)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__show__hotkey__overlay)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__spawn)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__spawn__sh)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__swap__window__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__swap__window__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__switch__focus__between__floating__and__tiling)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__switch__layout)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__switch__preset__column__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__switch__preset__column__width__back)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__switch__preset__window__height)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__switch__preset__window__height__back)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__switch__preset__window__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__switch__preset__window__width__back)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__toggle__column__tabbed__display)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__toggle__debug__tint)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__toggle__keyboard__shortcuts__inhibit)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__toggle__overview)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__toggle__window__floating)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__toggle__window__rule__opacity)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__toggle__window__urgent)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__toggle__windowed__fullscreen)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__unset__window__urgent)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__help__unset__workspace__name)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__load__config__file)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__maximize__column)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__maximize__window__to__edges)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__left)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__left__or__to__monitor__left)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__right)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__right__or__to__monitor__right)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__to__first)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__to__index)
            opts="-h --help <INDEX>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__to__last)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__to__monitor)
            opts="-h --help <OUTPUT>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__to__monitor__down)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__to__monitor__left)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__to__monitor__next)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__to__monitor__previous)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__to__monitor__right)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__to__monitor__up)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__to__workspace)
            opts="-h --focus --help <REFERENCE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --focus)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__to__workspace__down)
            opts="-h --focus --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --focus)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__column__to__workspace__up)
            opts="-h --focus --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --focus)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__floating__window)
            opts="-x -y -h --id --x --y --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --x)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -x)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --y)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -y)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__down)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__down__or__to__workspace__down)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__to__floating)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__to__monitor)
            opts="-h --id --help <OUTPUT>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__to__monitor__down)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__to__monitor__left)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__to__monitor__next)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__to__monitor__previous)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__to__monitor__right)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__to__monitor__up)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__to__tiling)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__to__workspace)
            opts="-h --window-id --focus --help <REFERENCE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --window-id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --focus)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__to__workspace__down)
            opts="-h --focus --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --focus)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__to__workspace__up)
            opts="-h --focus --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --focus)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__up)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__window__up__or__to__workspace__up)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__workspace__down)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__workspace__to__index)
            opts="-h --reference --help <INDEX>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --reference)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__workspace__to__monitor)
            opts="-h --reference --help <OUTPUT>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --reference)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__workspace__to__monitor__down)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__workspace__to__monitor__left)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__workspace__to__monitor__next)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__workspace__to__monitor__previous)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__workspace__to__monitor__right)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__workspace__to__monitor__up)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__move__workspace__up)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__open__overview)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__power__off__monitors)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__power__on__monitors)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__quit)
            opts="-s -h --skip-confirmation --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__reset__window__height)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__screenshot)
            opts="-p -h --show-pointer --path --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --show-pointer)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                --path)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__screenshot__screen)
            opts="-d -p -h --write-to-disk --show-pointer --path --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --write-to-disk)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                -d)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                --show-pointer)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                --path)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__screenshot__window)
            opts="-d -h --id --write-to-disk --path --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --write-to-disk)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                -d)
                    COMPREPLY=($(compgen -W "true false" -- "${cur}"))
                    return 0
                    ;;
                --path)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__set__column__display)
            opts="-h --help <DISPLAY>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__set__column__width)
            opts="-h --help <CHANGE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__set__dynamic__cast__monitor)
            opts="-h --help [OUTPUT]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__set__dynamic__cast__window)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__set__window__height)
            opts="-h --id --help <CHANGE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__set__window__urgent)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__set__window__width)
            opts="-h --id --help <CHANGE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__set__workspace__name)
            opts="-h --workspace --help <NAME>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --workspace)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__show__hotkey__overlay)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__spawn)
            opts="-h --help <COMMAND>..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__spawn__sh)
            opts="-h --help <COMMAND>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__swap__window__left)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__swap__window__right)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__switch__focus__between__floating__and__tiling)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__switch__layout)
            opts="-h --help <LAYOUT>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__switch__preset__column__width)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__switch__preset__column__width__back)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__switch__preset__window__height)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__switch__preset__window__height__back)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__switch__preset__window__width)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__switch__preset__window__width__back)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__toggle__column__tabbed__display)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__toggle__debug__tint)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__toggle__keyboard__shortcuts__inhibit)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__toggle__overview)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__toggle__window__floating)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__toggle__window__rule__opacity)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__toggle__window__urgent)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__toggle__windowed__fullscreen)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__unset__window__urgent)
            opts="-h --id --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__action__unset__workspace__name)
            opts="-h --help [REFERENCE]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__event__stream)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__focused__output)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__focused__window)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help)
            opts="outputs workspaces windows layers keyboard-layouts focused-output focused-window pick-window pick-color action output event-stream version request-error overview-state help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action)
            opts="quit power-off-monitors power-on-monitors spawn spawn-sh do-screen-transition screenshot screenshot-screen screenshot-window toggle-keyboard-shortcuts-inhibit close-window fullscreen-window toggle-windowed-fullscreen focus-window focus-window-in-column focus-window-previous focus-column-left focus-column-right focus-column-first focus-column-last focus-column-right-or-first focus-column-left-or-last focus-column focus-window-or-monitor-up focus-window-or-monitor-down focus-column-or-monitor-left focus-column-or-monitor-right focus-window-down focus-window-up focus-window-down-or-column-left focus-window-down-or-column-right focus-window-up-or-column-left focus-window-up-or-column-right focus-window-or-workspace-down focus-window-or-workspace-up focus-window-top focus-window-bottom focus-window-down-or-top focus-window-up-or-bottom move-column-left move-column-right move-column-to-first move-column-to-last move-column-left-or-to-monitor-left move-column-right-or-to-monitor-right move-column-to-index move-window-down move-window-up move-window-down-or-to-workspace-down move-window-up-or-to-workspace-up consume-or-expel-window-left consume-or-expel-window-right consume-window-into-column expel-window-from-column swap-window-right swap-window-left toggle-column-tabbed-display set-column-display center-column center-window center-visible-columns focus-workspace-down focus-workspace-up focus-workspace focus-workspace-previous move-window-to-workspace-down move-window-to-workspace-up move-window-to-workspace move-column-to-workspace-down move-column-to-workspace-up move-column-to-workspace move-workspace-down move-workspace-up move-workspace-to-index set-workspace-name unset-workspace-name focus-monitor-left focus-monitor-right focus-monitor-down focus-monitor-up focus-monitor-previous focus-monitor-next focus-monitor move-window-to-monitor-left move-window-to-monitor-right move-window-to-monitor-down move-window-to-monitor-up move-window-to-monitor-previous move-window-to-monitor-next move-window-to-monitor move-column-to-monitor-left move-column-to-monitor-right move-column-to-monitor-down move-column-to-monitor-up move-column-to-monitor-previous move-column-to-monitor-next move-column-to-monitor set-window-width set-window-height reset-window-height switch-preset-column-width switch-preset-column-width-back switch-preset-window-width switch-preset-window-width-back switch-preset-window-height switch-preset-window-height-back maximize-column maximize-window-to-edges set-column-width expand-column-to-available-width switch-layout show-hotkey-overlay move-workspace-to-monitor-left move-workspace-to-monitor-right move-workspace-to-monitor-down move-workspace-to-monitor-up move-workspace-to-monitor-previous move-workspace-to-monitor-next move-workspace-to-monitor toggle-debug-tint debug-toggle-opaque-regions debug-toggle-damage toggle-window-floating move-window-to-floating move-window-to-tiling focus-floating focus-tiling switch-focus-between-floating-and-tiling move-floating-window toggle-window-rule-opacity set-dynamic-cast-window set-dynamic-cast-monitor clear-dynamic-cast-target toggle-overview open-overview close-overview toggle-window-urgent set-window-urgent unset-window-urgent load-config-file"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__center__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__center__visible__columns)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__center__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__clear__dynamic__cast__target)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__close__overview)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__close__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__consume__or__expel__window__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__consume__or__expel__window__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__consume__window__into__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__debug__toggle__damage)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__debug__toggle__opaque__regions)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__do__screen__transition)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__expand__column__to__available__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__expel__window__from__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__column__first)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__column__last)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__column__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__column__left__or__last)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__column__or__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__column__or__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__column__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__column__right__or__first)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__floating)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__monitor__next)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__monitor__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__tiling)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__bottom)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__down__or__column__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__down__or__column__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__down__or__top)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__in__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__or__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__or__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__or__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__or__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__top)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__up__or__bottom)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__up__or__column__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__window__up__or__column__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__workspace)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__workspace__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__focus__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__fullscreen__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__load__config__file)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__maximize__column)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__maximize__window__to__edges)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__left__or__to__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__right__or__to__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__to__first)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__to__index)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__to__last)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__to__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__to__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__to__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__to__monitor__next)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__to__monitor__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__to__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__to__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__to__workspace)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__to__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__column__to__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__floating__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__down__or__to__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__to__floating)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__to__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__to__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__to__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__to__monitor__next)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__to__monitor__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__to__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__to__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__to__tiling)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__to__workspace)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__to__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__to__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__window__up__or__to__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__workspace__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__workspace__to__index)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__workspace__to__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__workspace__to__monitor__down)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__workspace__to__monitor__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__workspace__to__monitor__next)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__workspace__to__monitor__previous)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__workspace__to__monitor__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__workspace__to__monitor__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__move__workspace__up)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__open__overview)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__power__off__monitors)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__power__on__monitors)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__quit)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__reset__window__height)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__screenshot)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__screenshot__screen)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__screenshot__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__set__column__display)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__set__column__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__set__dynamic__cast__monitor)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__set__dynamic__cast__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__set__window__height)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__set__window__urgent)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__set__window__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__set__workspace__name)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__show__hotkey__overlay)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__spawn)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__spawn__sh)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__swap__window__left)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__swap__window__right)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__switch__focus__between__floating__and__tiling)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__switch__layout)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__switch__preset__column__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__switch__preset__column__width__back)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__switch__preset__window__height)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__switch__preset__window__height__back)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__switch__preset__window__width)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__switch__preset__window__width__back)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__toggle__column__tabbed__display)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__toggle__debug__tint)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__toggle__keyboard__shortcuts__inhibit)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__toggle__overview)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__toggle__window__floating)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__toggle__window__rule__opacity)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__toggle__window__urgent)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__toggle__windowed__fullscreen)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__unset__window__urgent)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__action__unset__workspace__name)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__event__stream)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__focused__output)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__focused__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__keyboard__layouts)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__layers)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__output)
            opts="off on mode custom-mode modeline scale transform position vrr"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__output__custom__mode)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__output__mode)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__output__modeline)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__output__off)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__output__on)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__output__position)
            opts="auto set"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__output__position__auto)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__output__position__set)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__output__scale)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__output__transform)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__output__vrr)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__outputs)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__overview__state)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__pick__color)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__pick__window)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__request__error)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__version)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__windows)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__help__workspaces)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__keyboard__layouts)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__layers)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output)
            opts="-h --help <OUTPUT> off on mode custom-mode modeline scale transform position vrr help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__custom__mode)
            opts="-h --help <MODE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__help)
            opts="off on mode custom-mode modeline scale transform position vrr help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__help__custom__mode)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__help__mode)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__help__modeline)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__help__off)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__help__on)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__help__position)
            opts="auto set"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__help__position__auto)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__help__position__set)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__help__scale)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__help__transform)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__help__vrr)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__mode)
            opts="-h --help <MODE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__modeline)
            opts="-h --help <CLOCK> <HDISPLAY> <HSYNC_START> <HSYNC_END> <HTOTAL> <VDISPLAY> <VSYNC_START> <VSYNC_END> <VTOTAL> <HSYNC_POLARITY> <VSYNC_POLARITY>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__off)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__on)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__position)
            opts="-h --help auto set help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__position__auto)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__position__help)
            opts="auto set help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__position__help__auto)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__position__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__position__help__set)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__position__set)
            opts="-h --help <X> <Y>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__scale)
            opts="-h --help <SCALE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__transform)
            opts="-h --help normal 90 180 270 flipped flipped-90 flipped-180 flipped-270"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__output__vrr)
            opts="-h --on-demand --help y yes t true on 1 n no f false off 0"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__outputs)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__overview__state)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__pick__color)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__pick__window)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__request__error)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__version)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__windows)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__msg__workspaces)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__panic)
            opts="-h --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        niri__validate)
            opts="-c -h --config --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --config)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -c)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
    esac
}

if [[ "${BASH_VERSINFO[0]}" -eq 4 && "${BASH_VERSINFO[1]}" -ge 4 || "${BASH_VERSINFO[0]}" -gt 4 ]]; then
    complete -F _niri -o nosort -o bashdefault -o default niri
else
    complete -F _niri -o bashdefault -o default niri
fi
