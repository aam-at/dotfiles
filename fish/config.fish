# Path to Oh My Fish install.
set -gx OMF_PATH $HOME/.local/share/omf

# Customize Oh My Fish configuration path.
set -gx OMF_CONFIG $HOME/.config/omf

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish

# Oh My Fish plugins
set fish_plugins vi-mode python pyenv gi git-flow emacs
set fish_plugins thefuck weather
# Oh My Fish themes
set fish_plugins agnoster krisleech zish toaster ocean
# select theme
set fish_theme agnoster

# Load fishmarks (http://github.com/techwizrd/fishmarks)
# install: curl -L https://github.com/techwizrd/fishmarks/raw/master/install.fish | fish
. $HOME/.fishmarks/marks.fish

# autojump utility for easy navigation
if test -f $HOME/.autojump/share/autojump/autojump.fish;
    . $HOME/.autojump/share/autojump/autojump.fish;
end

# use vi-mode by default
set -U fish_key_bindings fish_vi_key_bindings

# colorscheme for fish
source ~/dotfiles/fish/solarized.fish

# Add sbin to PATH variable
set -x PATH $PATH /sbin/

# Source environement variables shared between different shells.
# http://unix.stackexchange.com/questions/176322/share-environment-variables-between-bash-and-fish/176331#176331
function export --description 'Set global variable. Alias for set -gx, made for bash compatibility'
  if test -z "$argv"
    set
    return 0
  end
  for arg in $argv
    set -l v (echo $arg|tr '=' \n)
    switch (count $v)
      case 1
        set -gx $v $$v
      case 2
        if [ $v[1] = PATH ]
          set -gx PATH (echo $v[2]|tr ': ' \n)
        else
          set -gx $v
        end
    end
  end
end
# sources environment variables
source ~/.env

################################
# Custom functions and aliases #
################################
source ~/.aliases
alias -="cd -"

function reload
  source ~/.config/fish/config.fish
end

function fuck -d 'Correct your previous console command'
  set -l exit_code $status
  set -l eval_script (mktemp 2>/dev/null ; or mktemp -t 'thefuck')
  set -l fucked_up_commandd $history[1]
  thefuck $fucked_up_commandd > $eval_script
  . $eval_script
  rm $eval_script
  if test $exit_code -ne 0
    history --delete $fucked_up_commandd
  end
end

if test -d ~/.pyenv
  set -gx PYENV_ROOT $HOME/.pyenv
  set -x PATH $HOME/.pyenv/bin $PATH
  pyenv init - | source
  pyenv virtualenv-init - | source
end
