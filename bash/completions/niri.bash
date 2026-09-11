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

  for i in "${COMP_WORDS[@]:0:COMP_CWORD}"; do
    case "${cmd},${i}" in
    ",$1")
      cmd="niri"
      ;;
    niri,completions)
      cmd="niri__subcmd__completions"
      ;;
    niri,help)
      cmd="niri__subcmd__help"
      ;;
    niri,msg)
      cmd="niri__subcmd__msg"
      ;;
    niri,panic)
      cmd="niri__subcmd__panic"
      ;;
    niri,validate)
      cmd="niri__subcmd__validate"
      ;;
    niri__subcmd__help,completions)
      cmd="niri__subcmd__help__subcmd__completions"
      ;;
    niri__subcmd__help,help)
      cmd="niri__subcmd__help__subcmd__help"
      ;;
    niri__subcmd__help,msg)
      cmd="niri__subcmd__help__subcmd__msg"
      ;;
    niri__subcmd__help,panic)
      cmd="niri__subcmd__help__subcmd__panic"
      ;;
    niri__subcmd__help,validate)
      cmd="niri__subcmd__help__subcmd__validate"
      ;;
    niri__subcmd__help__subcmd__msg,action)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action"
      ;;
    niri__subcmd__help__subcmd__msg,casts)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__casts"
      ;;
    niri__subcmd__help__subcmd__msg,event-stream)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__event__subcmd__stream"
      ;;
    niri__subcmd__help__subcmd__msg,focused-output)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__focused__subcmd__output"
      ;;
    niri__subcmd__help__subcmd__msg,focused-window)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__focused__subcmd__window"
      ;;
    niri__subcmd__help__subcmd__msg,keyboard-layouts)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__keyboard__subcmd__layouts"
      ;;
    niri__subcmd__help__subcmd__msg,layers)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__layers"
      ;;
    niri__subcmd__help__subcmd__msg,output)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__output"
      ;;
    niri__subcmd__help__subcmd__msg,outputs)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__outputs"
      ;;
    niri__subcmd__help__subcmd__msg,overview-state)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__overview__subcmd__state"
      ;;
    niri__subcmd__help__subcmd__msg,pick-color)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__pick__subcmd__color"
      ;;
    niri__subcmd__help__subcmd__msg,pick-window)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__pick__subcmd__window"
      ;;
    niri__subcmd__help__subcmd__msg,request-error)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__request__subcmd__error"
      ;;
    niri__subcmd__help__subcmd__msg,version)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__version"
      ;;
    niri__subcmd__help__subcmd__msg,windows)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__windows"
      ;;
    niri__subcmd__help__subcmd__msg,workspaces)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__workspaces"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,center-column)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__center__subcmd__column"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,center-visible-columns)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__center__subcmd__visible__subcmd__columns"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,center-window)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__center__subcmd__window"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,clear-dynamic-cast-target)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__clear__subcmd__dynamic__subcmd__cast__subcmd__target"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,close-overview)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__close__subcmd__overview"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,close-window)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__close__subcmd__window"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,consume-or-expel-window-left)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__left"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,consume-or-expel-window-right)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__right"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,consume-window-into-column)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__consume__subcmd__window__subcmd__into__subcmd__column"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,debug-toggle-damage)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__debug__subcmd__toggle__subcmd__damage"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,debug-toggle-opaque-regions)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__debug__subcmd__toggle__subcmd__opaque__subcmd__regions"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,do-screen-transition)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__do__subcmd__screen__subcmd__transition"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,expand-column-to-available-width)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__expand__subcmd__column__subcmd__to__subcmd__available__subcmd__width"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,expel-window-from-column)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__expel__subcmd__window__subcmd__from__subcmd__column"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-column)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-column-first)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__first"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-column-last)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__last"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-column-left)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-column-left-or-last)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__left__subcmd__or__subcmd__last"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-column-or-monitor-left)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-column-or-monitor-right)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-column-right)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-column-right-or-first)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__right__subcmd__or__subcmd__first"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-floating)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__floating"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-monitor)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-monitor-down)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-monitor-left)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-monitor-next)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-monitor-previous)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-monitor-right)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-monitor-up)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-tiling)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__tiling"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-bottom)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__bottom"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-down)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-down-or-column-left)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-down-or-column-right)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-down-or-top)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__top"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-in-column)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__in__subcmd__column"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-or-monitor-down)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-or-monitor-up)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-or-workspace-down)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-or-workspace-up)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-previous)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__previous"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-top)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__top"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-up)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-up-or-bottom)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__bottom"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-up-or-column-left)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-window-up-or-column-right)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-workspace)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-workspace-down)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-workspace-previous)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__previous"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,focus-workspace-up)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,fullscreen-window)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__fullscreen__subcmd__window"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,load-config-file)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__load__subcmd__config__subcmd__file"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,maximize-column)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__maximize__subcmd__column"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,maximize-window-to-edges)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__maximize__subcmd__window__subcmd__to__subcmd__edges"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-left)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-left-or-to-monitor-left)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__left__subcmd__or__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-right)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-right-or-to-monitor-right)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__right__subcmd__or__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-to-first)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__first"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-to-index)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__index"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-to-last)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__last"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-to-monitor)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-to-monitor-down)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-to-monitor-left)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-to-monitor-next)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-to-monitor-previous)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-to-monitor-right)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-to-monitor-up)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-to-workspace)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-to-workspace-down)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-column-to-workspace-up)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-floating-window)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__floating__subcmd__window"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-down)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__down"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-down-or-to-workspace-down)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__down__subcmd__or__subcmd__to__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-to-floating)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__floating"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-to-monitor)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-to-monitor-down)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-to-monitor-left)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-to-monitor-next)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-to-monitor-previous)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-to-monitor-right)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-to-monitor-up)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-to-tiling)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__tiling"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-to-workspace)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-to-workspace-down)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-to-workspace-up)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-up)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__up"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-window-up-or-to-workspace-up)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__up__subcmd__or__subcmd__to__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-workspace-down)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-workspace-to-index)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__index"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-workspace-to-monitor)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-workspace-to-monitor-down)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-workspace-to-monitor-left)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-workspace-to-monitor-next)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-workspace-to-monitor-previous)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-workspace-to-monitor-right)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-workspace-to-monitor-up)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,move-workspace-up)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,open-overview)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__open__subcmd__overview"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,power-off-monitors)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__power__subcmd__off__subcmd__monitors"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,power-on-monitors)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__power__subcmd__on__subcmd__monitors"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,quit)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__quit"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,reset-window-height)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__reset__subcmd__window__subcmd__height"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,screenshot)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__screenshot"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,screenshot-screen)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__screenshot__subcmd__screen"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,screenshot-window)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__screenshot__subcmd__window"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,set-column-display)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__column__subcmd__display"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,set-column-width)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__column__subcmd__width"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,set-dynamic-cast-monitor)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__monitor"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,set-dynamic-cast-window)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__window"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,set-window-height)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__window__subcmd__height"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,set-window-urgent)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__window__subcmd__urgent"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,set-window-width)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__window__subcmd__width"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,set-workspace-name)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__workspace__subcmd__name"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,show-hotkey-overlay)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__show__subcmd__hotkey__subcmd__overlay"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,spawn)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__spawn"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,spawn-sh)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__spawn__subcmd__sh"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,stop-cast)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__stop__subcmd__cast"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,swap-window-left)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__swap__subcmd__window__subcmd__left"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,swap-window-right)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__swap__subcmd__window__subcmd__right"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,switch-focus-between-floating-and-tiling)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__focus__subcmd__between__subcmd__floating__subcmd__and__subcmd__tiling"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,switch-layout)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__layout"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,switch-preset-column-width)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,switch-preset-column-width-back)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width__subcmd__back"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,switch-preset-window-height)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,switch-preset-window-height-back)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height__subcmd__back"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,switch-preset-window-width)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,switch-preset-window-width-back)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width__subcmd__back"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,toggle-column-tabbed-display)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__column__subcmd__tabbed__subcmd__display"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,toggle-debug-tint)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__debug__subcmd__tint"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,toggle-keyboard-shortcuts-inhibit)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__keyboard__subcmd__shortcuts__subcmd__inhibit"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,toggle-overview)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__overview"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,toggle-window-floating)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__window__subcmd__floating"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,toggle-window-rule-opacity)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__window__subcmd__rule__subcmd__opacity"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,toggle-window-urgent)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__window__subcmd__urgent"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,toggle-windowed-fullscreen)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__windowed__subcmd__fullscreen"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,unset-window-urgent)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__unset__subcmd__window__subcmd__urgent"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__action,unset-workspace-name)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__unset__subcmd__workspace__subcmd__name"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__output,custom-mode)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__custom__subcmd__mode"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__output,mode)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__mode"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__output,modeline)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__modeline"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__output,off)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__off"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__output,on)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__on"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__output,position)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__position"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__output,scale)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__scale"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__output,transform)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__transform"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__output,vrr)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__vrr"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__position,auto)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__position__subcmd__auto"
      ;;
    niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__position,set)
      cmd="niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__position__subcmd__set"
      ;;
    niri__subcmd__msg,action)
      cmd="niri__subcmd__msg__subcmd__action"
      ;;
    niri__subcmd__msg,casts)
      cmd="niri__subcmd__msg__subcmd__casts"
      ;;
    niri__subcmd__msg,event-stream)
      cmd="niri__subcmd__msg__subcmd__event__subcmd__stream"
      ;;
    niri__subcmd__msg,focused-output)
      cmd="niri__subcmd__msg__subcmd__focused__subcmd__output"
      ;;
    niri__subcmd__msg,focused-window)
      cmd="niri__subcmd__msg__subcmd__focused__subcmd__window"
      ;;
    niri__subcmd__msg,help)
      cmd="niri__subcmd__msg__subcmd__help"
      ;;
    niri__subcmd__msg,keyboard-layouts)
      cmd="niri__subcmd__msg__subcmd__keyboard__subcmd__layouts"
      ;;
    niri__subcmd__msg,layers)
      cmd="niri__subcmd__msg__subcmd__layers"
      ;;
    niri__subcmd__msg,output)
      cmd="niri__subcmd__msg__subcmd__output"
      ;;
    niri__subcmd__msg,outputs)
      cmd="niri__subcmd__msg__subcmd__outputs"
      ;;
    niri__subcmd__msg,overview-state)
      cmd="niri__subcmd__msg__subcmd__overview__subcmd__state"
      ;;
    niri__subcmd__msg,pick-color)
      cmd="niri__subcmd__msg__subcmd__pick__subcmd__color"
      ;;
    niri__subcmd__msg,pick-window)
      cmd="niri__subcmd__msg__subcmd__pick__subcmd__window"
      ;;
    niri__subcmd__msg,request-error)
      cmd="niri__subcmd__msg__subcmd__request__subcmd__error"
      ;;
    niri__subcmd__msg,version)
      cmd="niri__subcmd__msg__subcmd__version"
      ;;
    niri__subcmd__msg,windows)
      cmd="niri__subcmd__msg__subcmd__windows"
      ;;
    niri__subcmd__msg,workspaces)
      cmd="niri__subcmd__msg__subcmd__workspaces"
      ;;
    niri__subcmd__msg__subcmd__action,center-column)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__center__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__action,center-visible-columns)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__center__subcmd__visible__subcmd__columns"
      ;;
    niri__subcmd__msg__subcmd__action,center-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__center__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action,clear-dynamic-cast-target)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__clear__subcmd__dynamic__subcmd__cast__subcmd__target"
      ;;
    niri__subcmd__msg__subcmd__action,close-overview)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__close__subcmd__overview"
      ;;
    niri__subcmd__msg__subcmd__action,close-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__close__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action,consume-or-expel-window-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action,consume-or-expel-window-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action,consume-window-into-column)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__consume__subcmd__window__subcmd__into__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__action,debug-toggle-damage)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__debug__subcmd__toggle__subcmd__damage"
      ;;
    niri__subcmd__msg__subcmd__action,debug-toggle-opaque-regions)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__debug__subcmd__toggle__subcmd__opaque__subcmd__regions"
      ;;
    niri__subcmd__msg__subcmd__action,do-screen-transition)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__do__subcmd__screen__subcmd__transition"
      ;;
    niri__subcmd__msg__subcmd__action,expand-column-to-available-width)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__expand__subcmd__column__subcmd__to__subcmd__available__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__action,expel-window-from-column)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__expel__subcmd__window__subcmd__from__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__action,focus-column)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__action,focus-column-first)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__first"
      ;;
    niri__subcmd__msg__subcmd__action,focus-column-last)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__last"
      ;;
    niri__subcmd__msg__subcmd__action,focus-column-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action,focus-column-left-or-last)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__left__subcmd__or__subcmd__last"
      ;;
    niri__subcmd__msg__subcmd__action,focus-column-or-monitor-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action,focus-column-or-monitor-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action,focus-column-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action,focus-column-right-or-first)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__right__subcmd__or__subcmd__first"
      ;;
    niri__subcmd__msg__subcmd__action,focus-floating)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__floating"
      ;;
    niri__subcmd__msg__subcmd__action,focus-monitor)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__action,focus-monitor-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action,focus-monitor-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action,focus-monitor-next)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__msg__subcmd__action,focus-monitor-previous)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__action,focus-monitor-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action,focus-monitor-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action,focus-tiling)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__tiling"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-bottom)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__bottom"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-down-or-column-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-down-or-column-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-down-or-top)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__top"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-in-column)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__in__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-or-monitor-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-or-monitor-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-or-workspace-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-or-workspace-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-previous)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-top)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__top"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-up-or-bottom)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__bottom"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-up-or-column-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action,focus-window-up-or-column-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action,focus-workspace)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace"
      ;;
    niri__subcmd__msg__subcmd__action,focus-workspace-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action,focus-workspace-previous)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__action,focus-workspace-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action,fullscreen-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__fullscreen__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action,help)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help"
      ;;
    niri__subcmd__msg__subcmd__action,load-config-file)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__load__subcmd__config__subcmd__file"
      ;;
    niri__subcmd__msg__subcmd__action,maximize-column)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__maximize__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__action,maximize-window-to-edges)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__maximize__subcmd__window__subcmd__to__subcmd__edges"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-left-or-to-monitor-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__left__subcmd__or__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-right-or-to-monitor-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__right__subcmd__or__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-to-first)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__first"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-to-index)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__index"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-to-last)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__last"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-to-monitor)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-to-monitor-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-to-monitor-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-to-monitor-next)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-to-monitor-previous)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-to-monitor-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-to-monitor-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-to-workspace)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-to-workspace-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action,move-column-to-workspace-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action,move-floating-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__floating__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-down-or-to-workspace-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__down__subcmd__or__subcmd__to__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-to-floating)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__floating"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-to-monitor)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-to-monitor-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-to-monitor-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-to-monitor-next)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-to-monitor-previous)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-to-monitor-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-to-monitor-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-to-tiling)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__tiling"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-to-workspace)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-to-workspace-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-to-workspace-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action,move-window-up-or-to-workspace-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__up__subcmd__or__subcmd__to__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action,move-workspace-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action,move-workspace-to-index)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__index"
      ;;
    niri__subcmd__msg__subcmd__action,move-workspace-to-monitor)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__action,move-workspace-to-monitor-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action,move-workspace-to-monitor-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action,move-workspace-to-monitor-next)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__msg__subcmd__action,move-workspace-to-monitor-previous)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__action,move-workspace-to-monitor-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action,move-workspace-to-monitor-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action,move-workspace-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action,open-overview)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__open__subcmd__overview"
      ;;
    niri__subcmd__msg__subcmd__action,power-off-monitors)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__power__subcmd__off__subcmd__monitors"
      ;;
    niri__subcmd__msg__subcmd__action,power-on-monitors)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__power__subcmd__on__subcmd__monitors"
      ;;
    niri__subcmd__msg__subcmd__action,quit)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__quit"
      ;;
    niri__subcmd__msg__subcmd__action,reset-window-height)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__reset__subcmd__window__subcmd__height"
      ;;
    niri__subcmd__msg__subcmd__action,screenshot)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__screenshot"
      ;;
    niri__subcmd__msg__subcmd__action,screenshot-screen)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__screenshot__subcmd__screen"
      ;;
    niri__subcmd__msg__subcmd__action,screenshot-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__screenshot__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action,set-column-display)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__column__subcmd__display"
      ;;
    niri__subcmd__msg__subcmd__action,set-column-width)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__column__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__action,set-dynamic-cast-monitor)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__action,set-dynamic-cast-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action,set-window-height)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__window__subcmd__height"
      ;;
    niri__subcmd__msg__subcmd__action,set-window-urgent)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__window__subcmd__urgent"
      ;;
    niri__subcmd__msg__subcmd__action,set-window-width)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__window__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__action,set-workspace-name)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__workspace__subcmd__name"
      ;;
    niri__subcmd__msg__subcmd__action,show-hotkey-overlay)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__show__subcmd__hotkey__subcmd__overlay"
      ;;
    niri__subcmd__msg__subcmd__action,spawn)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__spawn"
      ;;
    niri__subcmd__msg__subcmd__action,spawn-sh)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__spawn__subcmd__sh"
      ;;
    niri__subcmd__msg__subcmd__action,stop-cast)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__stop__subcmd__cast"
      ;;
    niri__subcmd__msg__subcmd__action,swap-window-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__swap__subcmd__window__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action,swap-window-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__swap__subcmd__window__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action,switch-focus-between-floating-and-tiling)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__focus__subcmd__between__subcmd__floating__subcmd__and__subcmd__tiling"
      ;;
    niri__subcmd__msg__subcmd__action,switch-layout)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__layout"
      ;;
    niri__subcmd__msg__subcmd__action,switch-preset-column-width)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__action,switch-preset-column-width-back)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width__subcmd__back"
      ;;
    niri__subcmd__msg__subcmd__action,switch-preset-window-height)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height"
      ;;
    niri__subcmd__msg__subcmd__action,switch-preset-window-height-back)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height__subcmd__back"
      ;;
    niri__subcmd__msg__subcmd__action,switch-preset-window-width)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__action,switch-preset-window-width-back)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width__subcmd__back"
      ;;
    niri__subcmd__msg__subcmd__action,toggle-column-tabbed-display)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__column__subcmd__tabbed__subcmd__display"
      ;;
    niri__subcmd__msg__subcmd__action,toggle-debug-tint)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__debug__subcmd__tint"
      ;;
    niri__subcmd__msg__subcmd__action,toggle-keyboard-shortcuts-inhibit)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__keyboard__subcmd__shortcuts__subcmd__inhibit"
      ;;
    niri__subcmd__msg__subcmd__action,toggle-overview)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__overview"
      ;;
    niri__subcmd__msg__subcmd__action,toggle-window-floating)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__window__subcmd__floating"
      ;;
    niri__subcmd__msg__subcmd__action,toggle-window-rule-opacity)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__window__subcmd__rule__subcmd__opacity"
      ;;
    niri__subcmd__msg__subcmd__action,toggle-window-urgent)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__window__subcmd__urgent"
      ;;
    niri__subcmd__msg__subcmd__action,toggle-windowed-fullscreen)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__windowed__subcmd__fullscreen"
      ;;
    niri__subcmd__msg__subcmd__action,unset-window-urgent)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__unset__subcmd__window__subcmd__urgent"
      ;;
    niri__subcmd__msg__subcmd__action,unset-workspace-name)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__unset__subcmd__workspace__subcmd__name"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,center-column)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__center__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,center-visible-columns)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__center__subcmd__visible__subcmd__columns"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,center-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__center__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,clear-dynamic-cast-target)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__clear__subcmd__dynamic__subcmd__cast__subcmd__target"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,close-overview)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__close__subcmd__overview"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,close-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__close__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,consume-or-expel-window-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,consume-or-expel-window-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,consume-window-into-column)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__consume__subcmd__window__subcmd__into__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,debug-toggle-damage)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__debug__subcmd__toggle__subcmd__damage"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,debug-toggle-opaque-regions)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__debug__subcmd__toggle__subcmd__opaque__subcmd__regions"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,do-screen-transition)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__do__subcmd__screen__subcmd__transition"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,expand-column-to-available-width)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__expand__subcmd__column__subcmd__to__subcmd__available__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,expel-window-from-column)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__expel__subcmd__window__subcmd__from__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-column)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-column-first)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__first"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-column-last)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__last"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-column-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-column-left-or-last)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__left__subcmd__or__subcmd__last"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-column-or-monitor-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-column-or-monitor-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-column-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-column-right-or-first)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__right__subcmd__or__subcmd__first"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-floating)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__floating"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-monitor)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-monitor-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-monitor-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-monitor-next)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-monitor-previous)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-monitor-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-monitor-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-tiling)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__tiling"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-bottom)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__bottom"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-down-or-column-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-down-or-column-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-down-or-top)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__top"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-in-column)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__in__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-or-monitor-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-or-monitor-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-or-workspace-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-or-workspace-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-previous)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-top)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__top"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-up-or-bottom)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__bottom"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-up-or-column-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-window-up-or-column-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-workspace)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__workspace"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-workspace-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-workspace-previous)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__workspace__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,focus-workspace-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,fullscreen-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__fullscreen__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,help)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__help"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,load-config-file)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__load__subcmd__config__subcmd__file"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,maximize-column)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__maximize__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,maximize-window-to-edges)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__maximize__subcmd__window__subcmd__to__subcmd__edges"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-left-or-to-monitor-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__left__subcmd__or__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-right-or-to-monitor-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__right__subcmd__or__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-to-first)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__first"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-to-index)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__index"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-to-last)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__last"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-to-monitor)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-to-monitor-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-to-monitor-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-to-monitor-next)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-to-monitor-previous)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-to-monitor-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-to-monitor-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-to-workspace)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-to-workspace-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-column-to-workspace-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-floating-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__floating__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-down-or-to-workspace-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__down__subcmd__or__subcmd__to__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-to-floating)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__floating"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-to-monitor)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-to-monitor-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-to-monitor-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-to-monitor-next)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-to-monitor-previous)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-to-monitor-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-to-monitor-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-to-tiling)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__tiling"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-to-workspace)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-to-workspace-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-to-workspace-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-window-up-or-to-workspace-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__up__subcmd__or__subcmd__to__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-workspace-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-workspace-to-index)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__index"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-workspace-to-monitor)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-workspace-to-monitor-down)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-workspace-to-monitor-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-workspace-to-monitor-next)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-workspace-to-monitor-previous)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-workspace-to-monitor-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-workspace-to-monitor-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,move-workspace-up)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,open-overview)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__open__subcmd__overview"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,power-off-monitors)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__power__subcmd__off__subcmd__monitors"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,power-on-monitors)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__power__subcmd__on__subcmd__monitors"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,quit)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__quit"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,reset-window-height)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__reset__subcmd__window__subcmd__height"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,screenshot)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__screenshot"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,screenshot-screen)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__screenshot__subcmd__screen"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,screenshot-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__screenshot__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,set-column-display)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__column__subcmd__display"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,set-column-width)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__column__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,set-dynamic-cast-monitor)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,set-dynamic-cast-window)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,set-window-height)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__window__subcmd__height"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,set-window-urgent)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__window__subcmd__urgent"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,set-window-width)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__window__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,set-workspace-name)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__workspace__subcmd__name"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,show-hotkey-overlay)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__show__subcmd__hotkey__subcmd__overlay"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,spawn)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__spawn"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,spawn-sh)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__spawn__subcmd__sh"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,stop-cast)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__stop__subcmd__cast"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,swap-window-left)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__swap__subcmd__window__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,swap-window-right)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__swap__subcmd__window__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,switch-focus-between-floating-and-tiling)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__focus__subcmd__between__subcmd__floating__subcmd__and__subcmd__tiling"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,switch-layout)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__layout"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,switch-preset-column-width)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,switch-preset-column-width-back)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width__subcmd__back"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,switch-preset-window-height)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,switch-preset-window-height-back)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height__subcmd__back"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,switch-preset-window-width)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,switch-preset-window-width-back)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width__subcmd__back"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,toggle-column-tabbed-display)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__column__subcmd__tabbed__subcmd__display"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,toggle-debug-tint)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__debug__subcmd__tint"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,toggle-keyboard-shortcuts-inhibit)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__keyboard__subcmd__shortcuts__subcmd__inhibit"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,toggle-overview)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__overview"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,toggle-window-floating)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__window__subcmd__floating"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,toggle-window-rule-opacity)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__window__subcmd__rule__subcmd__opacity"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,toggle-window-urgent)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__window__subcmd__urgent"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,toggle-windowed-fullscreen)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__windowed__subcmd__fullscreen"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,unset-window-urgent)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__unset__subcmd__window__subcmd__urgent"
      ;;
    niri__subcmd__msg__subcmd__action__subcmd__help,unset-workspace-name)
      cmd="niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__unset__subcmd__workspace__subcmd__name"
      ;;
    niri__subcmd__msg__subcmd__help,action)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action"
      ;;
    niri__subcmd__msg__subcmd__help,casts)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__casts"
      ;;
    niri__subcmd__msg__subcmd__help,event-stream)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__event__subcmd__stream"
      ;;
    niri__subcmd__msg__subcmd__help,focused-output)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__focused__subcmd__output"
      ;;
    niri__subcmd__msg__subcmd__help,focused-window)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__focused__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__help,help)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__help"
      ;;
    niri__subcmd__msg__subcmd__help,keyboard-layouts)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__keyboard__subcmd__layouts"
      ;;
    niri__subcmd__msg__subcmd__help,layers)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__layers"
      ;;
    niri__subcmd__msg__subcmd__help,output)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__output"
      ;;
    niri__subcmd__msg__subcmd__help,outputs)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__outputs"
      ;;
    niri__subcmd__msg__subcmd__help,overview-state)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__overview__subcmd__state"
      ;;
    niri__subcmd__msg__subcmd__help,pick-color)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__pick__subcmd__color"
      ;;
    niri__subcmd__msg__subcmd__help,pick-window)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__pick__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__help,request-error)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__request__subcmd__error"
      ;;
    niri__subcmd__msg__subcmd__help,version)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__version"
      ;;
    niri__subcmd__msg__subcmd__help,windows)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__windows"
      ;;
    niri__subcmd__msg__subcmd__help,workspaces)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__workspaces"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,center-column)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__center__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,center-visible-columns)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__center__subcmd__visible__subcmd__columns"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,center-window)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__center__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,clear-dynamic-cast-target)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__clear__subcmd__dynamic__subcmd__cast__subcmd__target"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,close-overview)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__close__subcmd__overview"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,close-window)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__close__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,consume-or-expel-window-left)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,consume-or-expel-window-right)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,consume-window-into-column)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__consume__subcmd__window__subcmd__into__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,debug-toggle-damage)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__debug__subcmd__toggle__subcmd__damage"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,debug-toggle-opaque-regions)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__debug__subcmd__toggle__subcmd__opaque__subcmd__regions"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,do-screen-transition)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__do__subcmd__screen__subcmd__transition"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,expand-column-to-available-width)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__expand__subcmd__column__subcmd__to__subcmd__available__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,expel-window-from-column)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__expel__subcmd__window__subcmd__from__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-column)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-column-first)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__first"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-column-last)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__last"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-column-left)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-column-left-or-last)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__left__subcmd__or__subcmd__last"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-column-or-monitor-left)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-column-or-monitor-right)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-column-right)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-column-right-or-first)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__right__subcmd__or__subcmd__first"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-floating)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__floating"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-monitor)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-monitor-down)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-monitor-left)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-monitor-next)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-monitor-previous)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-monitor-right)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-monitor-up)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-tiling)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__tiling"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-bottom)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__bottom"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-down)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-down-or-column-left)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-down-or-column-right)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-down-or-top)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__top"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-in-column)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__in__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-or-monitor-down)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-or-monitor-up)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-or-workspace-down)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-or-workspace-up)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-previous)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-top)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__top"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-up)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-up-or-bottom)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__bottom"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-up-or-column-left)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-window-up-or-column-right)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-workspace)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__workspace"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-workspace-down)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-workspace-previous)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,focus-workspace-up)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,fullscreen-window)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__fullscreen__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,load-config-file)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__load__subcmd__config__subcmd__file"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,maximize-column)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__maximize__subcmd__column"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,maximize-window-to-edges)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__maximize__subcmd__window__subcmd__to__subcmd__edges"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-left)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-left-or-to-monitor-left)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__left__subcmd__or__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-right)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-right-or-to-monitor-right)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__right__subcmd__or__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-to-first)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__first"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-to-index)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__index"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-to-last)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__last"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-to-monitor)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-to-monitor-down)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-to-monitor-left)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-to-monitor-next)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-to-monitor-previous)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-to-monitor-right)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-to-monitor-up)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-to-workspace)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-to-workspace-down)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-column-to-workspace-up)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-floating-window)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__floating__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-down)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-down-or-to-workspace-down)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__down__subcmd__or__subcmd__to__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-to-floating)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__floating"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-to-monitor)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-to-monitor-down)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-to-monitor-left)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-to-monitor-next)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-to-monitor-previous)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-to-monitor-right)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-to-monitor-up)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-to-tiling)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__tiling"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-to-workspace)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-to-workspace-down)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-to-workspace-up)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-up)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-window-up-or-to-workspace-up)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__up__subcmd__or__subcmd__to__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-workspace-down)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-workspace-to-index)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__index"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-workspace-to-monitor)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-workspace-to-monitor-down)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__down"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-workspace-to-monitor-left)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-workspace-to-monitor-next)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__next"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-workspace-to-monitor-previous)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__previous"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-workspace-to-monitor-right)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-workspace-to-monitor-up)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,move-workspace-up)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__up"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,open-overview)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__open__subcmd__overview"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,power-off-monitors)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__power__subcmd__off__subcmd__monitors"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,power-on-monitors)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__power__subcmd__on__subcmd__monitors"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,quit)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__quit"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,reset-window-height)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__reset__subcmd__window__subcmd__height"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,screenshot)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__screenshot"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,screenshot-screen)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__screenshot__subcmd__screen"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,screenshot-window)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__screenshot__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,set-column-display)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__column__subcmd__display"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,set-column-width)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__column__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,set-dynamic-cast-monitor)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__monitor"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,set-dynamic-cast-window)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__window"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,set-window-height)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__window__subcmd__height"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,set-window-urgent)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__window__subcmd__urgent"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,set-window-width)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__window__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,set-workspace-name)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__workspace__subcmd__name"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,show-hotkey-overlay)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__show__subcmd__hotkey__subcmd__overlay"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,spawn)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__spawn"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,spawn-sh)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__spawn__subcmd__sh"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,stop-cast)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__stop__subcmd__cast"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,swap-window-left)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__swap__subcmd__window__subcmd__left"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,swap-window-right)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__swap__subcmd__window__subcmd__right"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,switch-focus-between-floating-and-tiling)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__focus__subcmd__between__subcmd__floating__subcmd__and__subcmd__tiling"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,switch-layout)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__layout"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,switch-preset-column-width)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,switch-preset-column-width-back)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width__subcmd__back"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,switch-preset-window-height)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,switch-preset-window-height-back)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height__subcmd__back"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,switch-preset-window-width)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,switch-preset-window-width-back)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width__subcmd__back"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,toggle-column-tabbed-display)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__column__subcmd__tabbed__subcmd__display"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,toggle-debug-tint)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__debug__subcmd__tint"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,toggle-keyboard-shortcuts-inhibit)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__keyboard__subcmd__shortcuts__subcmd__inhibit"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,toggle-overview)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__overview"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,toggle-window-floating)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__window__subcmd__floating"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,toggle-window-rule-opacity)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__window__subcmd__rule__subcmd__opacity"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,toggle-window-urgent)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__window__subcmd__urgent"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,toggle-windowed-fullscreen)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__windowed__subcmd__fullscreen"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,unset-window-urgent)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__unset__subcmd__window__subcmd__urgent"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__action,unset-workspace-name)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__unset__subcmd__workspace__subcmd__name"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__output,custom-mode)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__custom__subcmd__mode"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__output,mode)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__mode"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__output,modeline)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__modeline"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__output,off)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__off"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__output,on)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__on"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__output,position)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__position"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__output,scale)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__scale"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__output,transform)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__transform"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__output,vrr)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__vrr"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__position,auto)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__position__subcmd__auto"
      ;;
    niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__position,set)
      cmd="niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__position__subcmd__set"
      ;;
    niri__subcmd__msg__subcmd__output,custom-mode)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__custom__subcmd__mode"
      ;;
    niri__subcmd__msg__subcmd__output,help)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__help"
      ;;
    niri__subcmd__msg__subcmd__output,mode)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__mode"
      ;;
    niri__subcmd__msg__subcmd__output,modeline)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__modeline"
      ;;
    niri__subcmd__msg__subcmd__output,off)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__off"
      ;;
    niri__subcmd__msg__subcmd__output,on)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__on"
      ;;
    niri__subcmd__msg__subcmd__output,position)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__position"
      ;;
    niri__subcmd__msg__subcmd__output,scale)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__scale"
      ;;
    niri__subcmd__msg__subcmd__output,transform)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__transform"
      ;;
    niri__subcmd__msg__subcmd__output,vrr)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__vrr"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__help,custom-mode)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__custom__subcmd__mode"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__help,help)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__help"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__help,mode)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__mode"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__help,modeline)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__modeline"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__help,off)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__off"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__help,on)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__on"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__help,position)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__position"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__help,scale)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__scale"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__help,transform)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__transform"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__help,vrr)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__vrr"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__position,auto)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__position__subcmd__auto"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__position,set)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__position__subcmd__set"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__position,auto)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__auto"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__position,help)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__help"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__position,set)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__set"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__help,auto)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__help__subcmd__auto"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__help,help)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__help__subcmd__help"
      ;;
    niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__help,set)
      cmd="niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__help__subcmd__set"
      ;;
    *)
      ;;
    esac
  done

  case "${cmd}" in
  niri)
    opts="-c -h -V --config --session --help --version [COMMAND]... msg validate panic completions help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 1 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__completions)
    opts="-h --help bash elvish fish power-shell zsh nushell"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help)
    opts="msg validate panic completions help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__completions)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__help)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg)
    opts="outputs workspaces windows layers keyboard-layouts focused-output focused-window pick-window pick-color action output event-stream version request-error overview-state casts"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action)
    opts="quit power-off-monitors power-on-monitors spawn spawn-sh do-screen-transition screenshot screenshot-screen screenshot-window toggle-keyboard-shortcuts-inhibit close-window fullscreen-window toggle-windowed-fullscreen focus-window focus-window-in-column focus-window-previous focus-column-left focus-column-right focus-column-first focus-column-last focus-column-right-or-first focus-column-left-or-last focus-column focus-window-or-monitor-up focus-window-or-monitor-down focus-column-or-monitor-left focus-column-or-monitor-right focus-window-down focus-window-up focus-window-down-or-column-left focus-window-down-or-column-right focus-window-up-or-column-left focus-window-up-or-column-right focus-window-or-workspace-down focus-window-or-workspace-up focus-window-top focus-window-bottom focus-window-down-or-top focus-window-up-or-bottom move-column-left move-column-right move-column-to-first move-column-to-last move-column-left-or-to-monitor-left move-column-right-or-to-monitor-right move-column-to-index move-window-down move-window-up move-window-down-or-to-workspace-down move-window-up-or-to-workspace-up consume-or-expel-window-left consume-or-expel-window-right consume-window-into-column expel-window-from-column swap-window-right swap-window-left toggle-column-tabbed-display set-column-display center-column center-window center-visible-columns focus-workspace-down focus-workspace-up focus-workspace focus-workspace-previous move-window-to-workspace-down move-window-to-workspace-up move-window-to-workspace move-column-to-workspace-down move-column-to-workspace-up move-column-to-workspace move-workspace-down move-workspace-up move-workspace-to-index set-workspace-name unset-workspace-name focus-monitor-left focus-monitor-right focus-monitor-down focus-monitor-up focus-monitor-previous focus-monitor-next focus-monitor move-window-to-monitor-left move-window-to-monitor-right move-window-to-monitor-down move-window-to-monitor-up move-window-to-monitor-previous move-window-to-monitor-next move-window-to-monitor move-column-to-monitor-left move-column-to-monitor-right move-column-to-monitor-down move-column-to-monitor-up move-column-to-monitor-previous move-column-to-monitor-next move-column-to-monitor set-window-width set-window-height reset-window-height switch-preset-column-width switch-preset-column-width-back switch-preset-window-width switch-preset-window-width-back switch-preset-window-height switch-preset-window-height-back maximize-column maximize-window-to-edges set-column-width expand-column-to-available-width switch-layout show-hotkey-overlay move-workspace-to-monitor-left move-workspace-to-monitor-right move-workspace-to-monitor-down move-workspace-to-monitor-up move-workspace-to-monitor-previous move-workspace-to-monitor-next move-workspace-to-monitor toggle-debug-tint debug-toggle-opaque-regions debug-toggle-damage toggle-window-floating move-window-to-floating move-window-to-tiling focus-floating focus-tiling switch-focus-between-floating-and-tiling move-floating-window toggle-window-rule-opacity set-dynamic-cast-window set-dynamic-cast-monitor clear-dynamic-cast-target stop-cast toggle-overview open-overview close-overview toggle-window-urgent set-window-urgent unset-window-urgent load-config-file"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__center__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__center__subcmd__visible__subcmd__columns)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__center__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__clear__subcmd__dynamic__subcmd__cast__subcmd__target)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__close__subcmd__overview)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__close__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__consume__subcmd__window__subcmd__into__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__debug__subcmd__toggle__subcmd__damage)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__debug__subcmd__toggle__subcmd__opaque__subcmd__regions)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__do__subcmd__screen__subcmd__transition)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__expand__subcmd__column__subcmd__to__subcmd__available__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__expel__subcmd__window__subcmd__from__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__first)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__last)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__left__subcmd__or__subcmd__last)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__right__subcmd__or__subcmd__first)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__floating)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__next)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__tiling)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__bottom)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__top)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__in__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__top)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__bottom)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__fullscreen__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__load__subcmd__config__subcmd__file)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__maximize__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__maximize__subcmd__window__subcmd__to__subcmd__edges)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__left__subcmd__or__subcmd__to__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__right__subcmd__or__subcmd__to__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__first)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__index)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__last)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__next)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__floating__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__down__subcmd__or__subcmd__to__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__floating)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__next)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__tiling)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__up__subcmd__or__subcmd__to__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__index)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__next)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__open__subcmd__overview)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__power__subcmd__off__subcmd__monitors)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__power__subcmd__on__subcmd__monitors)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__quit)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__reset__subcmd__window__subcmd__height)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__screenshot)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__screenshot__subcmd__screen)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__screenshot__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__column__subcmd__display)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__column__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__window__subcmd__height)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__window__subcmd__urgent)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__window__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__set__subcmd__workspace__subcmd__name)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__show__subcmd__hotkey__subcmd__overlay)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__spawn)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__spawn__subcmd__sh)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__stop__subcmd__cast)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__swap__subcmd__window__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__swap__subcmd__window__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__focus__subcmd__between__subcmd__floating__subcmd__and__subcmd__tiling)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__layout)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width__subcmd__back)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height__subcmd__back)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width__subcmd__back)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__column__subcmd__tabbed__subcmd__display)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__debug__subcmd__tint)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__keyboard__subcmd__shortcuts__subcmd__inhibit)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__overview)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__window__subcmd__floating)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__window__subcmd__rule__subcmd__opacity)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__window__subcmd__urgent)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__windowed__subcmd__fullscreen)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__unset__subcmd__window__subcmd__urgent)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__action__subcmd__unset__subcmd__workspace__subcmd__name)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__casts)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__event__subcmd__stream)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__focused__subcmd__output)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__focused__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__keyboard__subcmd__layouts)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__layers)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__output)
    opts="off on mode custom-mode modeline scale transform position vrr"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__custom__subcmd__mode)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__mode)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__modeline)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__off)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__on)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__position)
    opts="auto set"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__position__subcmd__auto)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__position__subcmd__set)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__scale)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__transform)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__output__subcmd__vrr)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__outputs)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__overview__subcmd__state)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__pick__subcmd__color)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__pick__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__request__subcmd__error)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__version)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__windows)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__msg__subcmd__workspaces)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__panic)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__help__subcmd__validate)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg)
    opts="-j -h --json --help outputs workspaces windows layers keyboard-layouts focused-output focused-window pick-window pick-color action output event-stream version request-error overview-state casts help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action)
    opts="-h --help quit power-off-monitors power-on-monitors spawn spawn-sh do-screen-transition screenshot screenshot-screen screenshot-window toggle-keyboard-shortcuts-inhibit close-window fullscreen-window toggle-windowed-fullscreen focus-window focus-window-in-column focus-window-previous focus-column-left focus-column-right focus-column-first focus-column-last focus-column-right-or-first focus-column-left-or-last focus-column focus-window-or-monitor-up focus-window-or-monitor-down focus-column-or-monitor-left focus-column-or-monitor-right focus-window-down focus-window-up focus-window-down-or-column-left focus-window-down-or-column-right focus-window-up-or-column-left focus-window-up-or-column-right focus-window-or-workspace-down focus-window-or-workspace-up focus-window-top focus-window-bottom focus-window-down-or-top focus-window-up-or-bottom move-column-left move-column-right move-column-to-first move-column-to-last move-column-left-or-to-monitor-left move-column-right-or-to-monitor-right move-column-to-index move-window-down move-window-up move-window-down-or-to-workspace-down move-window-up-or-to-workspace-up consume-or-expel-window-left consume-or-expel-window-right consume-window-into-column expel-window-from-column swap-window-right swap-window-left toggle-column-tabbed-display set-column-display center-column center-window center-visible-columns focus-workspace-down focus-workspace-up focus-workspace focus-workspace-previous move-window-to-workspace-down move-window-to-workspace-up move-window-to-workspace move-column-to-workspace-down move-column-to-workspace-up move-column-to-workspace move-workspace-down move-workspace-up move-workspace-to-index set-workspace-name unset-workspace-name focus-monitor-left focus-monitor-right focus-monitor-down focus-monitor-up focus-monitor-previous focus-monitor-next focus-monitor move-window-to-monitor-left move-window-to-monitor-right move-window-to-monitor-down move-window-to-monitor-up move-window-to-monitor-previous move-window-to-monitor-next move-window-to-monitor move-column-to-monitor-left move-column-to-monitor-right move-column-to-monitor-down move-column-to-monitor-up move-column-to-monitor-previous move-column-to-monitor-next move-column-to-monitor set-window-width set-window-height reset-window-height switch-preset-column-width switch-preset-column-width-back switch-preset-window-width switch-preset-window-width-back switch-preset-window-height switch-preset-window-height-back maximize-column maximize-window-to-edges set-column-width expand-column-to-available-width switch-layout show-hotkey-overlay move-workspace-to-monitor-left move-workspace-to-monitor-right move-workspace-to-monitor-down move-workspace-to-monitor-up move-workspace-to-monitor-previous move-workspace-to-monitor-next move-workspace-to-monitor toggle-debug-tint debug-toggle-opaque-regions debug-toggle-damage toggle-window-floating move-window-to-floating move-window-to-tiling focus-floating focus-tiling switch-focus-between-floating-and-tiling move-floating-window toggle-window-rule-opacity set-dynamic-cast-window set-dynamic-cast-monitor clear-dynamic-cast-target stop-cast toggle-overview open-overview close-overview toggle-window-urgent set-window-urgent unset-window-urgent load-config-file help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__center__subcmd__column)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__center__subcmd__visible__subcmd__columns)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__center__subcmd__window)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__clear__subcmd__dynamic__subcmd__cast__subcmd__target)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__close__subcmd__overview)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__close__subcmd__window)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__left)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__right)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__consume__subcmd__window__subcmd__into__subcmd__column)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__debug__subcmd__toggle__subcmd__damage)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__debug__subcmd__toggle__subcmd__opaque__subcmd__regions)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__do__subcmd__screen__subcmd__transition)
    opts="-d -h --delay-ms --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__expand__subcmd__column__subcmd__to__subcmd__available__subcmd__width)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__expel__subcmd__window__subcmd__from__subcmd__column)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column)
    opts="-h --help <INDEX>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__first)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__last)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__left)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__left__subcmd__or__subcmd__last)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__left)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__right)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__right)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__column__subcmd__right__subcmd__or__subcmd__first)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__floating)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor)
    opts="-h --help <OUTPUT>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__down)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__left)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__next)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__previous)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__right)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__up)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__tiling)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__bottom)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__left)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__right)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__top)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__in__subcmd__column)
    opts="-h --help <INDEX>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__down)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__up)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__down)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__up)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__previous)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__top)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__bottom)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__left)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__right)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace)
    opts="-h --help <REFERENCE>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__down)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__previous)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__up)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__fullscreen__subcmd__window)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help)
    opts="quit power-off-monitors power-on-monitors spawn spawn-sh do-screen-transition screenshot screenshot-screen screenshot-window toggle-keyboard-shortcuts-inhibit close-window fullscreen-window toggle-windowed-fullscreen focus-window focus-window-in-column focus-window-previous focus-column-left focus-column-right focus-column-first focus-column-last focus-column-right-or-first focus-column-left-or-last focus-column focus-window-or-monitor-up focus-window-or-monitor-down focus-column-or-monitor-left focus-column-or-monitor-right focus-window-down focus-window-up focus-window-down-or-column-left focus-window-down-or-column-right focus-window-up-or-column-left focus-window-up-or-column-right focus-window-or-workspace-down focus-window-or-workspace-up focus-window-top focus-window-bottom focus-window-down-or-top focus-window-up-or-bottom move-column-left move-column-right move-column-to-first move-column-to-last move-column-left-or-to-monitor-left move-column-right-or-to-monitor-right move-column-to-index move-window-down move-window-up move-window-down-or-to-workspace-down move-window-up-or-to-workspace-up consume-or-expel-window-left consume-or-expel-window-right consume-window-into-column expel-window-from-column swap-window-right swap-window-left toggle-column-tabbed-display set-column-display center-column center-window center-visible-columns focus-workspace-down focus-workspace-up focus-workspace focus-workspace-previous move-window-to-workspace-down move-window-to-workspace-up move-window-to-workspace move-column-to-workspace-down move-column-to-workspace-up move-column-to-workspace move-workspace-down move-workspace-up move-workspace-to-index set-workspace-name unset-workspace-name focus-monitor-left focus-monitor-right focus-monitor-down focus-monitor-up focus-monitor-previous focus-monitor-next focus-monitor move-window-to-monitor-left move-window-to-monitor-right move-window-to-monitor-down move-window-to-monitor-up move-window-to-monitor-previous move-window-to-monitor-next move-window-to-monitor move-column-to-monitor-left move-column-to-monitor-right move-column-to-monitor-down move-column-to-monitor-up move-column-to-monitor-previous move-column-to-monitor-next move-column-to-monitor set-window-width set-window-height reset-window-height switch-preset-column-width switch-preset-column-width-back switch-preset-window-width switch-preset-window-width-back switch-preset-window-height switch-preset-window-height-back maximize-column maximize-window-to-edges set-column-width expand-column-to-available-width switch-layout show-hotkey-overlay move-workspace-to-monitor-left move-workspace-to-monitor-right move-workspace-to-monitor-down move-workspace-to-monitor-up move-workspace-to-monitor-previous move-workspace-to-monitor-next move-workspace-to-monitor toggle-debug-tint debug-toggle-opaque-regions debug-toggle-damage toggle-window-floating move-window-to-floating move-window-to-tiling focus-floating focus-tiling switch-focus-between-floating-and-tiling move-floating-window toggle-window-rule-opacity set-dynamic-cast-window set-dynamic-cast-monitor clear-dynamic-cast-target stop-cast toggle-overview open-overview close-overview toggle-window-urgent set-window-urgent unset-window-urgent load-config-file help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__center__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__center__subcmd__visible__subcmd__columns)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__center__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__clear__subcmd__dynamic__subcmd__cast__subcmd__target)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__close__subcmd__overview)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__close__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__consume__subcmd__window__subcmd__into__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__debug__subcmd__toggle__subcmd__damage)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__debug__subcmd__toggle__subcmd__opaque__subcmd__regions)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__do__subcmd__screen__subcmd__transition)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__expand__subcmd__column__subcmd__to__subcmd__available__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__expel__subcmd__window__subcmd__from__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__first)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__last)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__left__subcmd__or__subcmd__last)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__column__subcmd__right__subcmd__or__subcmd__first)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__floating)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor__subcmd__next)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__tiling)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__bottom)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__top)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__in__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__top)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__bottom)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__workspace)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__workspace__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__focus__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__fullscreen__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__help)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__load__subcmd__config__subcmd__file)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__maximize__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__maximize__subcmd__window__subcmd__to__subcmd__edges)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__left__subcmd__or__subcmd__to__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__right__subcmd__or__subcmd__to__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__first)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__index)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__last)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__next)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__floating__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__down__subcmd__or__subcmd__to__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__floating)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__next)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__tiling)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__window__subcmd__up__subcmd__or__subcmd__to__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__index)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__next)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__move__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__open__subcmd__overview)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__power__subcmd__off__subcmd__monitors)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__power__subcmd__on__subcmd__monitors)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__quit)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__reset__subcmd__window__subcmd__height)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__screenshot)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__screenshot__subcmd__screen)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__screenshot__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__column__subcmd__display)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__column__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__window__subcmd__height)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__window__subcmd__urgent)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__window__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__set__subcmd__workspace__subcmd__name)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__show__subcmd__hotkey__subcmd__overlay)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__spawn)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__spawn__subcmd__sh)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__stop__subcmd__cast)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__swap__subcmd__window__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__swap__subcmd__window__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__focus__subcmd__between__subcmd__floating__subcmd__and__subcmd__tiling)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__layout)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width__subcmd__back)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height__subcmd__back)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width__subcmd__back)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__column__subcmd__tabbed__subcmd__display)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__debug__subcmd__tint)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__keyboard__subcmd__shortcuts__subcmd__inhibit)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__overview)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__window__subcmd__floating)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__window__subcmd__rule__subcmd__opacity)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__window__subcmd__urgent)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__toggle__subcmd__windowed__subcmd__fullscreen)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__unset__subcmd__window__subcmd__urgent)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__help__subcmd__unset__subcmd__workspace__subcmd__name)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__load__subcmd__config__subcmd__file)
    opts="-h --path --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --path)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__maximize__subcmd__column)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__maximize__subcmd__window__subcmd__to__subcmd__edges)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__left)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__left__subcmd__or__subcmd__to__subcmd__monitor__subcmd__left)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__right)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__right__subcmd__or__subcmd__to__subcmd__monitor__subcmd__right)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__first)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__index)
    opts="-h --help <INDEX>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__last)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor)
    opts="-h --help <OUTPUT>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__down)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__left)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__next)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__previous)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__right)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__up)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace)
    opts="-h --focus --help <REFERENCE>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__down)
    opts="-h --focus --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__up)
    opts="-h --focus --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__floating__subcmd__window)
    opts="-x -y -h --id --x --y --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__down)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__down__subcmd__or__subcmd__to__subcmd__workspace__subcmd__down)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__floating)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor)
    opts="-h --id --help <OUTPUT>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__down)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__left)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__next)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__previous)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__right)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__up)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__tiling)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace)
    opts="-h --window-id --focus --help <REFERENCE>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__down)
    opts="-h --focus --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__up)
    opts="-h --focus --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__up)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__window__subcmd__up__subcmd__or__subcmd__to__subcmd__workspace__subcmd__up)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__down)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__index)
    opts="-h --reference --help <INDEX>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor)
    opts="-h --reference --help <OUTPUT>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__down)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__left)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__next)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__previous)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__right)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__up)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__move__subcmd__workspace__subcmd__up)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__open__subcmd__overview)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__power__subcmd__off__subcmd__monitors)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__power__subcmd__on__subcmd__monitors)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__quit)
    opts="-s -h --skip-confirmation --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__reset__subcmd__window__subcmd__height)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__screenshot)
    opts="-p -h --show-pointer --path --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__screenshot__subcmd__screen)
    opts="-d -p -h --write-to-disk --show-pointer --path --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__screenshot__subcmd__window)
    opts="-d -p -h --id --write-to-disk --show-pointer --path --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__column__subcmd__display)
    opts="-h --help <DISPLAY>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__column__subcmd__width)
    opts="-h --help <CHANGE>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__monitor)
    opts="-h --help [OUTPUT]"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__window)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__window__subcmd__height)
    opts="-h --id --help <CHANGE>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__window__subcmd__urgent)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__window__subcmd__width)
    opts="-h --id --help <CHANGE>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__set__subcmd__workspace__subcmd__name)
    opts="-h --workspace --help <NAME>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__show__subcmd__hotkey__subcmd__overlay)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__spawn)
    opts="-h --help <COMMAND>..."
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__spawn__subcmd__sh)
    opts="-h --help <COMMAND>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__stop__subcmd__cast)
    opts="-h --session-id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --session-id)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__swap__subcmd__window__subcmd__left)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__swap__subcmd__window__subcmd__right)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__focus__subcmd__between__subcmd__floating__subcmd__and__subcmd__tiling)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__layout)
    opts="-h --help <LAYOUT>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width__subcmd__back)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height__subcmd__back)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width__subcmd__back)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__column__subcmd__tabbed__subcmd__display)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__debug__subcmd__tint)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__keyboard__subcmd__shortcuts__subcmd__inhibit)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__overview)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__window__subcmd__floating)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__window__subcmd__rule__subcmd__opacity)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__window__subcmd__urgent)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__toggle__subcmd__windowed__subcmd__fullscreen)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__unset__subcmd__window__subcmd__urgent)
    opts="-h --id --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__action__subcmd__unset__subcmd__workspace__subcmd__name)
    opts="-h --help [REFERENCE]"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__casts)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__event__subcmd__stream)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__focused__subcmd__output)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__focused__subcmd__window)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help)
    opts="outputs workspaces windows layers keyboard-layouts focused-output focused-window pick-window pick-color action output event-stream version request-error overview-state casts help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action)
    opts="quit power-off-monitors power-on-monitors spawn spawn-sh do-screen-transition screenshot screenshot-screen screenshot-window toggle-keyboard-shortcuts-inhibit close-window fullscreen-window toggle-windowed-fullscreen focus-window focus-window-in-column focus-window-previous focus-column-left focus-column-right focus-column-first focus-column-last focus-column-right-or-first focus-column-left-or-last focus-column focus-window-or-monitor-up focus-window-or-monitor-down focus-column-or-monitor-left focus-column-or-monitor-right focus-window-down focus-window-up focus-window-down-or-column-left focus-window-down-or-column-right focus-window-up-or-column-left focus-window-up-or-column-right focus-window-or-workspace-down focus-window-or-workspace-up focus-window-top focus-window-bottom focus-window-down-or-top focus-window-up-or-bottom move-column-left move-column-right move-column-to-first move-column-to-last move-column-left-or-to-monitor-left move-column-right-or-to-monitor-right move-column-to-index move-window-down move-window-up move-window-down-or-to-workspace-down move-window-up-or-to-workspace-up consume-or-expel-window-left consume-or-expel-window-right consume-window-into-column expel-window-from-column swap-window-right swap-window-left toggle-column-tabbed-display set-column-display center-column center-window center-visible-columns focus-workspace-down focus-workspace-up focus-workspace focus-workspace-previous move-window-to-workspace-down move-window-to-workspace-up move-window-to-workspace move-column-to-workspace-down move-column-to-workspace-up move-column-to-workspace move-workspace-down move-workspace-up move-workspace-to-index set-workspace-name unset-workspace-name focus-monitor-left focus-monitor-right focus-monitor-down focus-monitor-up focus-monitor-previous focus-monitor-next focus-monitor move-window-to-monitor-left move-window-to-monitor-right move-window-to-monitor-down move-window-to-monitor-up move-window-to-monitor-previous move-window-to-monitor-next move-window-to-monitor move-column-to-monitor-left move-column-to-monitor-right move-column-to-monitor-down move-column-to-monitor-up move-column-to-monitor-previous move-column-to-monitor-next move-column-to-monitor set-window-width set-window-height reset-window-height switch-preset-column-width switch-preset-column-width-back switch-preset-window-width switch-preset-window-width-back switch-preset-window-height switch-preset-window-height-back maximize-column maximize-window-to-edges set-column-width expand-column-to-available-width switch-layout show-hotkey-overlay move-workspace-to-monitor-left move-workspace-to-monitor-right move-workspace-to-monitor-down move-workspace-to-monitor-up move-workspace-to-monitor-previous move-workspace-to-monitor-next move-workspace-to-monitor toggle-debug-tint debug-toggle-opaque-regions debug-toggle-damage toggle-window-floating move-window-to-floating move-window-to-tiling focus-floating focus-tiling switch-focus-between-floating-and-tiling move-floating-window toggle-window-rule-opacity set-dynamic-cast-window set-dynamic-cast-monitor clear-dynamic-cast-target stop-cast toggle-overview open-overview close-overview toggle-window-urgent set-window-urgent unset-window-urgent load-config-file"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__center__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__center__subcmd__visible__subcmd__columns)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__center__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__clear__subcmd__dynamic__subcmd__cast__subcmd__target)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__close__subcmd__overview)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__close__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__consume__subcmd__or__subcmd__expel__subcmd__window__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__consume__subcmd__window__subcmd__into__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__debug__subcmd__toggle__subcmd__damage)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__debug__subcmd__toggle__subcmd__opaque__subcmd__regions)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__do__subcmd__screen__subcmd__transition)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__expand__subcmd__column__subcmd__to__subcmd__available__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__expel__subcmd__window__subcmd__from__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__first)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__last)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__left__subcmd__or__subcmd__last)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__or__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__column__subcmd__right__subcmd__or__subcmd__first)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__floating)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__next)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__tiling)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__bottom)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__column__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__down__subcmd__or__subcmd__top)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__in__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__or__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__top)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__bottom)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__window__subcmd__up__subcmd__or__subcmd__column__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__workspace)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__focus__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__fullscreen__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__load__subcmd__config__subcmd__file)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__maximize__subcmd__column)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__maximize__subcmd__window__subcmd__to__subcmd__edges)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__left__subcmd__or__subcmd__to__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__right__subcmd__or__subcmd__to__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__first)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__index)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__last)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__next)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__column__subcmd__to__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__floating__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__down__subcmd__or__subcmd__to__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__floating)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__next)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__tiling)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__to__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__window__subcmd__up__subcmd__or__subcmd__to__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__index)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__down)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__next)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__previous)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__to__subcmd__monitor__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__move__subcmd__workspace__subcmd__up)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__open__subcmd__overview)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__power__subcmd__off__subcmd__monitors)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__power__subcmd__on__subcmd__monitors)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__quit)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__reset__subcmd__window__subcmd__height)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__screenshot)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__screenshot__subcmd__screen)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__screenshot__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__column__subcmd__display)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__column__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__monitor)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__dynamic__subcmd__cast__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__window__subcmd__height)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__window__subcmd__urgent)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__window__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__set__subcmd__workspace__subcmd__name)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__show__subcmd__hotkey__subcmd__overlay)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__spawn)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__spawn__subcmd__sh)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__stop__subcmd__cast)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__swap__subcmd__window__subcmd__left)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__swap__subcmd__window__subcmd__right)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__focus__subcmd__between__subcmd__floating__subcmd__and__subcmd__tiling)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__layout)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__preset__subcmd__column__subcmd__width__subcmd__back)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__height__subcmd__back)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__switch__subcmd__preset__subcmd__window__subcmd__width__subcmd__back)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__column__subcmd__tabbed__subcmd__display)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__debug__subcmd__tint)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__keyboard__subcmd__shortcuts__subcmd__inhibit)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__overview)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__window__subcmd__floating)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__window__subcmd__rule__subcmd__opacity)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__window__subcmd__urgent)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__toggle__subcmd__windowed__subcmd__fullscreen)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__unset__subcmd__window__subcmd__urgent)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__action__subcmd__unset__subcmd__workspace__subcmd__name)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__casts)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__event__subcmd__stream)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__focused__subcmd__output)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__focused__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__help)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__keyboard__subcmd__layouts)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__layers)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__output)
    opts="off on mode custom-mode modeline scale transform position vrr"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__custom__subcmd__mode)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__mode)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__modeline)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__off)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__on)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__position)
    opts="auto set"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__position__subcmd__auto)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__position__subcmd__set)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__scale)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__transform)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__output__subcmd__vrr)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__outputs)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__overview__subcmd__state)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__pick__subcmd__color)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__pick__subcmd__window)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__request__subcmd__error)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__version)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__windows)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__help__subcmd__workspaces)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__keyboard__subcmd__layouts)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__layers)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output)
    opts="-h --help <OUTPUT> off on mode custom-mode modeline scale transform position vrr help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__custom__subcmd__mode)
    opts="-h --help <MODE>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__help)
    opts="off on mode custom-mode modeline scale transform position vrr help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__custom__subcmd__mode)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__help)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__mode)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__modeline)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__off)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__on)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__position)
    opts="auto set"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__position__subcmd__auto)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__position__subcmd__set)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__scale)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__transform)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__help__subcmd__vrr)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__mode)
    opts="-h --help <MODE>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__modeline)
    opts="-h --help <CLOCK> <HDISPLAY> <HSYNC_START> <HSYNC_END> <HTOTAL> <VDISPLAY> <VSYNC_START> <VSYNC_END> <VTOTAL> <HSYNC_POLARITY> <VSYNC_POLARITY>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__off)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__on)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__position)
    opts="-h --help auto set help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__auto)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__help)
    opts="auto set help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__help__subcmd__auto)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__help__subcmd__help)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__help__subcmd__set)
    opts=""
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 6 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__position__subcmd__set)
    opts="-h --help <X> <Y>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__scale)
    opts="-h --help <SCALE>"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__transform)
    opts="-h --help normal 90 180 270 flipped flipped-90 flipped-180 flipped-270"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__output__subcmd__vrr)
    opts="-h --on-demand --help y yes t true on 1 n no f false off 0"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__outputs)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__overview__subcmd__state)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__pick__subcmd__color)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__pick__subcmd__window)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__request__subcmd__error)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__version)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__windows)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__msg__subcmd__workspaces)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__panic)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  niri__subcmd__validate)
    opts="-c -h --config --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
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
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  esac
}

if [[ "${BASH_VERSINFO[0]}" -eq 4 && "${BASH_VERSINFO[1]}" -ge 4 || "${BASH_VERSINFO[0]}" -gt 4 ]]; then
  complete -F _niri -o nosort -o bashdefault -o default niri
else
  complete -F _niri -o bashdefault -o default niri
fi
