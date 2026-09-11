_tv() {
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
      cmd="tv"
      ;;
    tv,completions)
      cmd="tv__completions"
      ;;
    tv,help)
      cmd="tv__help"
      ;;
    tv,init)
      cmd="tv__init"
      ;;
    tv,list-channels)
      cmd="tv__list__channels"
      ;;
    tv,update-channels)
      cmd="tv__update__channels"
      ;;
    tv__help,completions)
      cmd="tv__help__completions"
      ;;
    tv__help,help)
      cmd="tv__help__help"
      ;;
    tv__help,init)
      cmd="tv__help__init"
      ;;
    tv__help,list-channels)
      cmd="tv__help__list__channels"
      ;;
    tv__help,update-channels)
      cmd="tv__help__update__channels"
      ;;
    *)
      ;;
    esac
  done

  case "${cmd}" in
  tv)
    opts="-s -p -i -t -k -h -V --source-command --ansi --no-sort --source-display --source-output --source-entry-delimiter --preview-command --preview-header --preview-footer --cache-preview --preview-offset --no-preview --hide-preview --show-preview --preview-border --preview-padding --preview-word-wrap --hide-preview-scrollbar --preview-size --input --input-header --input-prompt --input-position --input-border --input-padding --no-status-bar --hide-status-bar --show-status-bar --results-border --results-padding --layout --no-remote --hide-remote --show-remote --no-help-panel --hide-help-panel --show-help-panel --ui-scale --height --width --inline --tick-rate --watch --autocomplete-prompt --exact --select-1 --take-1 --take-1-fast --keybindings --expect --config-file --cable-dir --global-history --help --version [CHANNEL] [PATH] list-channels init completions update-channels help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 1 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --source-command)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    -s)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --source-display)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --source-output)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --source-entry-delimiter)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --preview-command)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    -p)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --preview-header)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --preview-footer)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --preview-offset)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --preview-border)
      COMPREPLY=($(compgen -W "none plain rounded thick" -- "${cur}"))
      return 0
      ;;
    --preview-padding)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --preview-size)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --input)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    -i)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --input-header)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --input-prompt)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --input-position)
      COMPREPLY=($(compgen -W "top bottom" -- "${cur}"))
      return 0
      ;;
    --input-border)
      COMPREPLY=($(compgen -W "none plain rounded thick" -- "${cur}"))
      return 0
      ;;
    --input-padding)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --results-border)
      COMPREPLY=($(compgen -W "none plain rounded thick" -- "${cur}"))
      return 0
      ;;
    --results-padding)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --layout)
      COMPREPLY=($(compgen -W "landscape portrait" -- "${cur}"))
      return 0
      ;;
    --ui-scale)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --height)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --width)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --tick-rate)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    -t)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --watch)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --autocomplete-prompt)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --keybindings)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    -k)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --expect)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --config-file)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --cable-dir)
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
  tv__completions)
    opts="-h --help bash zsh fish power-shell cmd nu"
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
  tv__help)
    opts="list-channels init completions update-channels help"
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
  tv__help__completions)
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
  tv__help__help)
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
  tv__help__init)
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
  tv__help__list__channels)
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
  tv__help__update__channels)
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
  tv__init)
    opts="-h --help bash zsh fish power-shell cmd nu"
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
  tv__list__channels)
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
  tv__update__channels)
    opts="-h --force --help"
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
  esac
}

if [[ "${BASH_VERSINFO[0]}" -eq 4 && "${BASH_VERSINFO[1]}" -ge 4 || "${BASH_VERSINFO[0]}" -gt 4 ]]; then
  complete -F _tv -o nosort -o bashdefault -o default tv
else
  complete -F _tv -o bashdefault -o default tv
fi
