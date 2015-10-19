set CONFIG=install_win.conf.yaml
set DOTBOT_DIR=dotbot

set DOTBOT_BIN=bin\dotbot

REM git submodule update --init --recursive %DOTBOT_DIR%

python "%DOTBOT_DIR%\%DOTBOT_BIN%" -d . -c "%CONFIG%" %*
