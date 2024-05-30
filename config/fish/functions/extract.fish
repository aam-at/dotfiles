# Inspiration from: https://github.com/dideler/dotfiles/blob/master/.config/fish/functions/extract.fish
function extract -d "Extracts and expands a variety of archive files"

    for file in $argv
        if test -f $file
            echo -s "💥 Extracting '" (set_color --bold blue) $file (set_color normal) "' ..."
            switch $file
                case "*.tar"
                    tar -xvf $file
                case "*.tar.xz" "*.txz"
                    set os (uname)
                    if [ $os = "Darwin" ]
                        type gtar >/dev/null
                        and gtar -Jxf $file
                        or echo "💥 Please install GNU tar: brew install gnu-tar"
                    else
                        tar -Jxf $file
                    end
                case "*.tar.bz2" "*.tbz" "*.tbz2"
                    tar -jxf $file
                case "*.tar.gz" "*.tgz"
                    tar -zxvf $file
                case "*.xz"
                    unxz $file
                case "*.bz2"
                    # tar -jxvf $argv[1]
                    bunzip2 $file
                case "*.gz"
                    if test (echo $file[1] | awk -F. '{print $(NF-1)}') = tar  # tar bundle compressed with gzip
                        tar -zxvf $file
                    else  # single gzip
                        gunzip $file
                    end
                case "*.rar"
                    unrar x $file
                case "*.zip"
                    unzip -uo $file -d (basename $file .zip)
                case "*.pax"
                    pax -r < $file
                case "*.Z"
                    uncompress $file
                case "*.7z"
                    7za x $file
                case '*'
                    echo "💥 Extension not recognized, aborting."
            end
        else
            echo "💥 Cannot extract '$file', aborting."
        end
    end

end
