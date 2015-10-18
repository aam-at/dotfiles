set CONFIG="install.conf.yaml"
set DOTBOT_DIR=dotbot

set DOTBOT_BIN=bin/dotbot

git submodule update --init --recursive %DOTBOT_DIR%

"%DOTBOT_DIR%/%DOTBOT_BIN%" -c "%CONFIG%" %*
