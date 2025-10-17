function ruff_format --description 'Format Python files with ruff (imports + formatter)'
    if not set -q argv[1]
        echo 'Usage: ruff_format <file> [...]' >&2
        return 64
    end

    if not type -q ruff
        echo 'ruff_format: ruff is not installed.' >&2
        return 127
    end

    set -l rc 0
    for target in $argv
        if not test -f $target
            echo "ruff_format: \"$target\" is not a regular file." >&2
            set rc 1
            continue
        end

        command ruff check --select I --fix -- $target
        or return $status

        command ruff format -- $target
        or return $status
    end

    return $rc
end
