_uv() {
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
                cmd="uv"
                ;;
            uv,add)
                cmd="uv__add"
                ;;
            uv,cache)
                cmd="uv__cache"
                ;;
            uv,clean)
                cmd="uv__clean"
                ;;
            uv,generate-shell-completion)
                cmd="uv__generate__shell__completion"
                ;;
            uv,help)
                cmd="uv__help"
                ;;
            uv,lock)
                cmd="uv__lock"
                ;;
            uv,pip)
                cmd="uv__pip"
                ;;
            uv,remove)
                cmd="uv__remove"
                ;;
            uv,run)
                cmd="uv__run"
                ;;
            uv,self)
                cmd="uv__self"
                ;;
            uv,sync)
                cmd="uv__sync"
                ;;
            uv,tool)
                cmd="uv__tool"
                ;;
            uv,toolchain)
                cmd="uv__toolchain"
                ;;
            uv,venv)
                cmd="uv__venv"
                ;;
            uv,version)
                cmd="uv__version"
                ;;
            uv__cache,clean)
                cmd="uv__cache__clean"
                ;;
            uv__cache,dir)
                cmd="uv__cache__dir"
                ;;
            uv__cache,help)
                cmd="uv__cache__help"
                ;;
            uv__cache,prune)
                cmd="uv__cache__prune"
                ;;
            uv__cache__help,clean)
                cmd="uv__cache__help__clean"
                ;;
            uv__cache__help,dir)
                cmd="uv__cache__help__dir"
                ;;
            uv__cache__help,help)
                cmd="uv__cache__help__help"
                ;;
            uv__cache__help,prune)
                cmd="uv__cache__help__prune"
                ;;
            uv__help,add)
                cmd="uv__help__add"
                ;;
            uv__help,cache)
                cmd="uv__help__cache"
                ;;
            uv__help,clean)
                cmd="uv__help__clean"
                ;;
            uv__help,generate-shell-completion)
                cmd="uv__help__generate__shell__completion"
                ;;
            uv__help,help)
                cmd="uv__help__help"
                ;;
            uv__help,lock)
                cmd="uv__help__lock"
                ;;
            uv__help,pip)
                cmd="uv__help__pip"
                ;;
            uv__help,remove)
                cmd="uv__help__remove"
                ;;
            uv__help,run)
                cmd="uv__help__run"
                ;;
            uv__help,self)
                cmd="uv__help__self"
                ;;
            uv__help,sync)
                cmd="uv__help__sync"
                ;;
            uv__help,tool)
                cmd="uv__help__tool"
                ;;
            uv__help,toolchain)
                cmd="uv__help__toolchain"
                ;;
            uv__help,venv)
                cmd="uv__help__venv"
                ;;
            uv__help,version)
                cmd="uv__help__version"
                ;;
            uv__help__cache,clean)
                cmd="uv__help__cache__clean"
                ;;
            uv__help__cache,dir)
                cmd="uv__help__cache__dir"
                ;;
            uv__help__cache,prune)
                cmd="uv__help__cache__prune"
                ;;
            uv__help__pip,check)
                cmd="uv__help__pip__check"
                ;;
            uv__help__pip,compile)
                cmd="uv__help__pip__compile"
                ;;
            uv__help__pip,freeze)
                cmd="uv__help__pip__freeze"
                ;;
            uv__help__pip,install)
                cmd="uv__help__pip__install"
                ;;
            uv__help__pip,list)
                cmd="uv__help__pip__list"
                ;;
            uv__help__pip,show)
                cmd="uv__help__pip__show"
                ;;
            uv__help__pip,sync)
                cmd="uv__help__pip__sync"
                ;;
            uv__help__pip,uninstall)
                cmd="uv__help__pip__uninstall"
                ;;
            uv__help__self,update)
                cmd="uv__help__self__update"
                ;;
            uv__help__tool,run)
                cmd="uv__help__tool__run"
                ;;
            uv__help__toolchain,find)
                cmd="uv__help__toolchain__find"
                ;;
            uv__help__toolchain,install)
                cmd="uv__help__toolchain__install"
                ;;
            uv__help__toolchain,list)
                cmd="uv__help__toolchain__list"
                ;;
            uv__pip,check)
                cmd="uv__pip__check"
                ;;
            uv__pip,compile)
                cmd="uv__pip__compile"
                ;;
            uv__pip,freeze)
                cmd="uv__pip__freeze"
                ;;
            uv__pip,help)
                cmd="uv__pip__help"
                ;;
            uv__pip,install)
                cmd="uv__pip__install"
                ;;
            uv__pip,list)
                cmd="uv__pip__list"
                ;;
            uv__pip,show)
                cmd="uv__pip__show"
                ;;
            uv__pip,sync)
                cmd="uv__pip__sync"
                ;;
            uv__pip,uninstall)
                cmd="uv__pip__uninstall"
                ;;
            uv__pip__help,check)
                cmd="uv__pip__help__check"
                ;;
            uv__pip__help,compile)
                cmd="uv__pip__help__compile"
                ;;
            uv__pip__help,freeze)
                cmd="uv__pip__help__freeze"
                ;;
            uv__pip__help,help)
                cmd="uv__pip__help__help"
                ;;
            uv__pip__help,install)
                cmd="uv__pip__help__install"
                ;;
            uv__pip__help,list)
                cmd="uv__pip__help__list"
                ;;
            uv__pip__help,show)
                cmd="uv__pip__help__show"
                ;;
            uv__pip__help,sync)
                cmd="uv__pip__help__sync"
                ;;
            uv__pip__help,uninstall)
                cmd="uv__pip__help__uninstall"
                ;;
            uv__self,help)
                cmd="uv__self__help"
                ;;
            uv__self,update)
                cmd="uv__self__update"
                ;;
            uv__self__help,help)
                cmd="uv__self__help__help"
                ;;
            uv__self__help,update)
                cmd="uv__self__help__update"
                ;;
            uv__tool,help)
                cmd="uv__tool__help"
                ;;
            uv__tool,run)
                cmd="uv__tool__run"
                ;;
            uv__tool__help,help)
                cmd="uv__tool__help__help"
                ;;
            uv__tool__help,run)
                cmd="uv__tool__help__run"
                ;;
            uv__toolchain,find)
                cmd="uv__toolchain__find"
                ;;
            uv__toolchain,help)
                cmd="uv__toolchain__help"
                ;;
            uv__toolchain,install)
                cmd="uv__toolchain__install"
                ;;
            uv__toolchain,list)
                cmd="uv__toolchain__list"
                ;;
            uv__toolchain__help,find)
                cmd="uv__toolchain__help__find"
                ;;
            uv__toolchain__help,help)
                cmd="uv__toolchain__help__help"
                ;;
            uv__toolchain__help,install)
                cmd="uv__toolchain__help__install"
                ;;
            uv__toolchain__help,list)
                cmd="uv__toolchain__help__list"
                ;;
            *)
                ;;
        esac
    done

    case "${cmd}" in
        uv)
            opts="-q -v -n -h -V --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version pip tool toolchain run sync lock add remove venv cache self clean version generate-shell-completion help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 1 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__add)
            opts="-i -f -U -P -C -p -q -v -n -h -V --dev --workspace --index-url --extra-index-url --find-links --no-index --upgrade --no-upgrade --upgrade-package --reinstall --no-reinstall --reinstall-package --index-strategy --keyring-provider --resolution --prerelease --pre --config-setting --exclude-newer --link-mode --compile-bytecode --no-compile-bytecode --no-build --build --no-build-package --no-binary --binary --no-binary-package --refresh --no-refresh --refresh-package --python --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version <REQUIREMENTS>..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -i)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --extra-index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --find-links)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --upgrade-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -P)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --reinstall-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-strategy)
                    COMPREPLY=($(compgen -W "first-index unsafe-first-match unsafe-best-match" -- "${cur}"))
                    return 0
                    ;;
                --keyring-provider)
                    COMPREPLY=($(compgen -W "disabled subprocess" -- "${cur}"))
                    return 0
                    ;;
                --resolution)
                    COMPREPLY=($(compgen -W "highest lowest lowest-direct" -- "${cur}"))
                    return 0
                    ;;
                --prerelease)
                    COMPREPLY=($(compgen -W "disallow allow if-necessary explicit if-necessary-or-explicit" -- "${cur}"))
                    return 0
                    ;;
                --config-setting)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -C)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --exclude-newer)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --link-mode)
                    COMPREPLY=($(compgen -W "clone copy hardlink" -- "${cur}"))
                    return 0
                    ;;
                --no-build-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --no-binary-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --refresh-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__cache)
            opts="-q -v -n -h -V --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version clean prune dir help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__cache__clean)
            opts="-q -v -n -h -V --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version [PACKAGE]..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__cache__dir)
            opts="-q -v -n -h -V --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__cache__help)
            opts="clean prune dir help"
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
        uv__cache__help__clean)
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
        uv__cache__help__dir)
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
        uv__cache__help__help)
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
        uv__cache__help__prune)
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
        uv__cache__prune)
            opts="-q -v -n -h -V --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__clean)
            opts="-q -v -n -h -V --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version [PACKAGE]..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__generate__shell__completion)
            opts="-q -v -n -h -V --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version bash elvish fig fish nushell powershell zsh"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__help)
            opts="pip tool toolchain run sync lock add remove venv cache self clean version generate-shell-completion help"
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
        uv__help__add)
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
        uv__help__cache)
            opts="clean prune dir"
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
        uv__help__cache__clean)
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
        uv__help__cache__dir)
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
        uv__help__cache__prune)
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
        uv__help__clean)
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
        uv__help__generate__shell__completion)
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
        uv__help__help)
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
        uv__help__lock)
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
        uv__help__pip)
            opts="compile sync install uninstall freeze list show check"
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
        uv__help__pip__check)
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
        uv__help__pip__compile)
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
        uv__help__pip__freeze)
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
        uv__help__pip__install)
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
        uv__help__pip__list)
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
        uv__help__pip__show)
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
        uv__help__pip__sync)
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
        uv__help__pip__uninstall)
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
        uv__help__remove)
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
        uv__help__run)
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
        uv__help__self)
            opts="update"
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
        uv__help__self__update)
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
        uv__help__sync)
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
        uv__help__tool)
            opts="run"
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
        uv__help__tool__run)
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
        uv__help__toolchain)
            opts="list install find"
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
        uv__help__toolchain__find)
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
        uv__help__toolchain__install)
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
        uv__help__toolchain__list)
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
        uv__help__venv)
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
        uv__help__version)
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
        uv__lock)
            opts="-i -f -U -P -C -p -q -v -n -h -V --index-url --extra-index-url --find-links --no-index --upgrade --no-upgrade --upgrade-package --index-strategy --keyring-provider --resolution --prerelease --pre --config-setting --exclude-newer --link-mode --no-build --build --no-build-package --no-binary --binary --no-binary-package --refresh --no-refresh --refresh-package --python --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -i)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --extra-index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --find-links)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --upgrade-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -P)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-strategy)
                    COMPREPLY=($(compgen -W "first-index unsafe-first-match unsafe-best-match" -- "${cur}"))
                    return 0
                    ;;
                --keyring-provider)
                    COMPREPLY=($(compgen -W "disabled subprocess" -- "${cur}"))
                    return 0
                    ;;
                --resolution)
                    COMPREPLY=($(compgen -W "highest lowest lowest-direct" -- "${cur}"))
                    return 0
                    ;;
                --prerelease)
                    COMPREPLY=($(compgen -W "disallow allow if-necessary explicit if-necessary-or-explicit" -- "${cur}"))
                    return 0
                    ;;
                --config-setting)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -C)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --exclude-newer)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --link-mode)
                    COMPREPLY=($(compgen -W "clone copy hardlink" -- "${cur}"))
                    return 0
                    ;;
                --no-build-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --no-binary-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --refresh-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__pip)
            opts="-q -v -n -h -V --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version compile sync install uninstall freeze list show check help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__pip__check)
            opts="-p -q -v -n -h -V --python --system --no-system --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__pip__compile)
            opts="-c -i -f -U -P -C -o -p -q -v -n -h -V --constraint --override --extra --all-extras --no-all-extras --index-url --extra-index-url --find-links --no-index --upgrade --no-upgrade --upgrade-package --index-strategy --keyring-provider --resolution --prerelease --pre --config-setting --exclude-newer --link-mode --refresh --no-refresh --refresh-package --no-deps --deps --output-file --no-strip-extras --strip-extras --no-annotate --annotate --no-header --header --annotation-style --custom-compile-command --python --system --no-system --generate-hashes --no-generate-hashes --legacy-setup-py --no-legacy-setup-py --no-build-isolation --build-isolation --no-build --build --no-binary --only-binary --python-version --python-platform --no-emit-package --emit-index-url --no-emit-index-url --emit-find-links --no-emit-find-links --emit-marker-expression --no-emit-marker-expression --emit-index-annotation --no-emit-index-annotation --allow-unsafe --no-allow-unsafe --reuse-hashes --no-reuse-hashes --resolver --max-rounds --cert --client-cert --trusted-host --emit-trusted-host --no-emit-trusted-host --config --no-config --emit-options --no-emit-options --pip-args --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version <SRC_FILE>..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --constraint)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -c)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --override)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --extra)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -i)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --extra-index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --find-links)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --upgrade-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -P)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-strategy)
                    COMPREPLY=($(compgen -W "first-index unsafe-first-match unsafe-best-match" -- "${cur}"))
                    return 0
                    ;;
                --keyring-provider)
                    COMPREPLY=($(compgen -W "disabled subprocess" -- "${cur}"))
                    return 0
                    ;;
                --resolution)
                    COMPREPLY=($(compgen -W "highest lowest lowest-direct" -- "${cur}"))
                    return 0
                    ;;
                --prerelease)
                    COMPREPLY=($(compgen -W "disallow allow if-necessary explicit if-necessary-or-explicit" -- "${cur}"))
                    return 0
                    ;;
                --config-setting)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -C)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --exclude-newer)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --link-mode)
                    COMPREPLY=($(compgen -W "clone copy hardlink" -- "${cur}"))
                    return 0
                    ;;
                --refresh-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --output-file)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -o)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --annotation-style)
                    COMPREPLY=($(compgen -W "line split" -- "${cur}"))
                    return 0
                    ;;
                --custom-compile-command)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --no-binary)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --only-binary)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python-version)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python-platform)
                    COMPREPLY=($(compgen -W "windows linux macos x86_64-pc-windows-msvc x86_64-unknown-linux-gnu aarch64-apple-darwin x86_64-apple-darwin aarch64-unknown-linux-gnu aarch64-unknown-linux-musl x86_64-unknown-linux-musl x86_64-manylinux_2_17 x86_64-manylinux_2_28 aarch64-manylinux_2_17 aarch64-manylinux_2_28" -- "${cur}"))
                    return 0
                    ;;
                --no-emit-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --resolver)
                    COMPREPLY=($(compgen -W "backtracking legacy" -- "${cur}"))
                    return 0
                    ;;
                --max-rounds)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cert)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --client-cert)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --trusted-host)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --pip-args)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__pip__freeze)
            opts="-p -q -v -n -h -V --exclude-editable --strict --no-strict --python --system --no-system --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__pip__help)
            opts="compile sync install uninstall freeze list show check help"
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
        uv__pip__help__check)
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
        uv__pip__help__compile)
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
        uv__pip__help__freeze)
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
        uv__pip__help__help)
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
        uv__pip__help__install)
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
        uv__pip__help__list)
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
        uv__pip__help__show)
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
        uv__pip__help__sync)
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
        uv__pip__help__uninstall)
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
        uv__pip__install)
            opts="-r -e -c -i -f -U -P -C -p -q -v -n -h -V --requirement --editable --constraint --override --extra --all-extras --no-all-extras --index-url --extra-index-url --find-links --no-index --upgrade --no-upgrade --upgrade-package --reinstall --no-reinstall --reinstall-package --index-strategy --keyring-provider --resolution --prerelease --pre --config-setting --exclude-newer --link-mode --compile-bytecode --no-compile-bytecode --refresh --no-refresh --refresh-package --no-deps --deps --require-hashes --no-require-hashes --python --system --no-system --break-system-packages --no-break-system-packages --target --prefix --legacy-setup-py --no-legacy-setup-py --no-build-isolation --build-isolation --no-build --build --no-binary --only-binary --python-version --python-platform --strict --no-strict --dry-run --user --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version [PACKAGE]..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --requirement)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -r)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --editable)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -e)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --constraint)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -c)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --override)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --extra)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -i)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --extra-index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --find-links)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --upgrade-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -P)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --reinstall-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-strategy)
                    COMPREPLY=($(compgen -W "first-index unsafe-first-match unsafe-best-match" -- "${cur}"))
                    return 0
                    ;;
                --keyring-provider)
                    COMPREPLY=($(compgen -W "disabled subprocess" -- "${cur}"))
                    return 0
                    ;;
                --resolution)
                    COMPREPLY=($(compgen -W "highest lowest lowest-direct" -- "${cur}"))
                    return 0
                    ;;
                --prerelease)
                    COMPREPLY=($(compgen -W "disallow allow if-necessary explicit if-necessary-or-explicit" -- "${cur}"))
                    return 0
                    ;;
                --config-setting)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -C)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --exclude-newer)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --link-mode)
                    COMPREPLY=($(compgen -W "clone copy hardlink" -- "${cur}"))
                    return 0
                    ;;
                --refresh-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --target)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --prefix)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --no-binary)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --only-binary)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python-version)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python-platform)
                    COMPREPLY=($(compgen -W "windows linux macos x86_64-pc-windows-msvc x86_64-unknown-linux-gnu aarch64-apple-darwin x86_64-apple-darwin aarch64-unknown-linux-gnu aarch64-unknown-linux-musl x86_64-unknown-linux-musl x86_64-manylinux_2_17 x86_64-manylinux_2_28 aarch64-manylinux_2_17 aarch64-manylinux_2_28" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__pip__list)
            opts="-e -p -q -v -n -h -V --editable --exclude-editable --exclude --format --strict --no-strict --python --system --no-system --outdated --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --exclude)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -W "columns freeze json" -- "${cur}"))
                    return 0
                    ;;
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__pip__show)
            opts="-p -q -v -n -h -V --strict --no-strict --python --system --no-system --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version [PACKAGE]..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__pip__sync)
            opts="-c -i -f -C -p -a -q -v -n -h -V --constraint --index-url --extra-index-url --find-links --no-index --reinstall --no-reinstall --reinstall-package --index-strategy --keyring-provider --config-setting --link-mode --compile-bytecode --no-compile-bytecode --refresh --no-refresh --refresh-package --exclude-newer --require-hashes --no-require-hashes --python --system --no-system --break-system-packages --no-break-system-packages --target --prefix --legacy-setup-py --no-legacy-setup-py --no-build-isolation --build-isolation --no-build --build --no-binary --only-binary --python-version --python-platform --strict --no-strict --dry-run --ask --trusted-host --python-executable --user --cert --client-cert --config --no-config --pip-args --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version <SRC_FILE>..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --constraint)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -c)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -i)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --extra-index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --find-links)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --reinstall-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-strategy)
                    COMPREPLY=($(compgen -W "first-index unsafe-first-match unsafe-best-match" -- "${cur}"))
                    return 0
                    ;;
                --keyring-provider)
                    COMPREPLY=($(compgen -W "disabled subprocess" -- "${cur}"))
                    return 0
                    ;;
                --config-setting)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -C)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --link-mode)
                    COMPREPLY=($(compgen -W "clone copy hardlink" -- "${cur}"))
                    return 0
                    ;;
                --refresh-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --exclude-newer)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --target)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --prefix)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --no-binary)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --only-binary)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python-version)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python-platform)
                    COMPREPLY=($(compgen -W "windows linux macos x86_64-pc-windows-msvc x86_64-unknown-linux-gnu aarch64-apple-darwin x86_64-apple-darwin aarch64-unknown-linux-gnu aarch64-unknown-linux-musl x86_64-unknown-linux-musl x86_64-manylinux_2_17 x86_64-manylinux_2_28 aarch64-manylinux_2_17 aarch64-manylinux_2_28" -- "${cur}"))
                    return 0
                    ;;
                --trusted-host)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python-executable)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cert)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --client-cert)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --pip-args)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__pip__uninstall)
            opts="-r -p -q -v -n -h -V --requirement --python --keyring-provider --system --no-system --break-system-packages --no-break-system-packages --target --prefix --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version [PACKAGE]..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --requirement)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -r)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --keyring-provider)
                    COMPREPLY=($(compgen -W "disabled subprocess" -- "${cur}"))
                    return 0
                    ;;
                --target)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --prefix)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__remove)
            opts="-p -q -v -n -h -V --dev --python --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version <REQUIREMENTS>..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__run)
            opts="-i -f -U -P -C -p -q -v -n -h -V --extra --all-extras --no-all-extras --dev --no-dev --with --index-url --extra-index-url --find-links --no-index --upgrade --no-upgrade --upgrade-package --reinstall --no-reinstall --reinstall-package --index-strategy --keyring-provider --resolution --prerelease --pre --config-setting --exclude-newer --link-mode --compile-bytecode --no-compile-bytecode --no-build --build --no-build-package --no-binary --binary --no-binary-package --refresh --no-refresh --refresh-package --python --package --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version [TARGET] [ARGS]..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --extra)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --with)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -i)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --extra-index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --find-links)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --upgrade-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -P)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --reinstall-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-strategy)
                    COMPREPLY=($(compgen -W "first-index unsafe-first-match unsafe-best-match" -- "${cur}"))
                    return 0
                    ;;
                --keyring-provider)
                    COMPREPLY=($(compgen -W "disabled subprocess" -- "${cur}"))
                    return 0
                    ;;
                --resolution)
                    COMPREPLY=($(compgen -W "highest lowest lowest-direct" -- "${cur}"))
                    return 0
                    ;;
                --prerelease)
                    COMPREPLY=($(compgen -W "disallow allow if-necessary explicit if-necessary-or-explicit" -- "${cur}"))
                    return 0
                    ;;
                --config-setting)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -C)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --exclude-newer)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --link-mode)
                    COMPREPLY=($(compgen -W "clone copy hardlink" -- "${cur}"))
                    return 0
                    ;;
                --no-build-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --no-binary-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --refresh-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__self)
            opts="-q -v -n -h -V --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version update help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__self__help)
            opts="update help"
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
        uv__self__help__help)
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
        uv__self__help__update)
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
        uv__self__update)
            opts="-q -v -n -h -V --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__sync)
            opts="-i -f -C -p -q -v -n -h -V --extra --all-extras --no-all-extras --dev --no-dev --no-clean --index-url --extra-index-url --find-links --no-index --reinstall --no-reinstall --reinstall-package --index-strategy --keyring-provider --config-setting --link-mode --compile-bytecode --no-compile-bytecode --no-build --build --no-build-package --no-binary --binary --no-binary-package --refresh --no-refresh --refresh-package --python --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --extra)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -i)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --extra-index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --find-links)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --reinstall-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-strategy)
                    COMPREPLY=($(compgen -W "first-index unsafe-first-match unsafe-best-match" -- "${cur}"))
                    return 0
                    ;;
                --keyring-provider)
                    COMPREPLY=($(compgen -W "disabled subprocess" -- "${cur}"))
                    return 0
                    ;;
                --config-setting)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -C)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --link-mode)
                    COMPREPLY=($(compgen -W "clone copy hardlink" -- "${cur}"))
                    return 0
                    ;;
                --no-build-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --no-binary-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --refresh-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__tool)
            opts="-q -v -n -h -V --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version run help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__tool__help)
            opts="run help"
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
        uv__tool__help__help)
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
        uv__tool__help__run)
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
        uv__tool__run)
            opts="-i -f -U -P -C -p -q -v -n -h -V --from --with --index-url --extra-index-url --find-links --no-index --upgrade --no-upgrade --upgrade-package --reinstall --no-reinstall --reinstall-package --index-strategy --keyring-provider --resolution --prerelease --pre --config-setting --exclude-newer --link-mode --compile-bytecode --no-compile-bytecode --no-build --build --no-build-package --no-binary --binary --no-binary-package --refresh --no-refresh --refresh-package --python --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version <TARGET> [ARGS]..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --from)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --with)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -i)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --extra-index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --find-links)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --upgrade-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -P)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --reinstall-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-strategy)
                    COMPREPLY=($(compgen -W "first-index unsafe-first-match unsafe-best-match" -- "${cur}"))
                    return 0
                    ;;
                --keyring-provider)
                    COMPREPLY=($(compgen -W "disabled subprocess" -- "${cur}"))
                    return 0
                    ;;
                --resolution)
                    COMPREPLY=($(compgen -W "highest lowest lowest-direct" -- "${cur}"))
                    return 0
                    ;;
                --prerelease)
                    COMPREPLY=($(compgen -W "disallow allow if-necessary explicit if-necessary-or-explicit" -- "${cur}"))
                    return 0
                    ;;
                --config-setting)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -C)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --exclude-newer)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --link-mode)
                    COMPREPLY=($(compgen -W "clone copy hardlink" -- "${cur}"))
                    return 0
                    ;;
                --no-build-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --no-binary-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --refresh-package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__toolchain)
            opts="-q -v -n -h -V --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version list install find help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__toolchain__find)
            opts="-q -v -n -h --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help [REQUEST]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__toolchain__help)
            opts="list install find help"
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
        uv__toolchain__help__find)
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
        uv__toolchain__help__help)
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
        uv__toolchain__help__install)
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
        uv__toolchain__help__list)
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
        uv__toolchain__install)
            opts="-f -q -v -n -h -V --force --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version [TARGETS]..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__toolchain__list)
            opts="-q -v -n -h -V --all-versions --all-platforms --only-installed --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__venv)
            opts="-p -i -f -q -v -n -h -V --python --system --no-system --seed --allow-existing --prompt --system-site-packages --index-url --extra-index-url --find-links --no-index --index-strategy --keyring-provider --exclude-newer --link-mode --clear --no-seed --no-pip --no-setuptools --no-wheel --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version [NAME]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --python)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -p)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --prompt)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -i)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --extra-index-url)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --find-links)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --index-strategy)
                    COMPREPLY=($(compgen -W "first-index unsafe-first-match unsafe-best-match" -- "${cur}"))
                    return 0
                    ;;
                --keyring-provider)
                    COMPREPLY=($(compgen -W "disabled subprocess" -- "${cur}"))
                    return 0
                    ;;
                --exclude-newer)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --link-mode)
                    COMPREPLY=($(compgen -W "clone copy hardlink" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
        uv__version)
            opts="-q -v -n -h -V --output-format --quiet --verbose --no-color --color --native-tls --no-native-tls --offline --no-offline --preview --no-preview --isolated --show-settings --no-cache --cache-dir --config-file --help --version"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --output-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --config-file)
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
    complete -F _uv -o nosort -o bashdefault -o default uv
else
    complete -F _uv -o bashdefault -o default uv
fi
