@echo off

rem REM Install applications using Chocolatey
rem choco install -y 7zip adb ag anki autohotkey clink conemu curl delta ^
rem far ffmpeg git gnupg gpg4win handbrake helix hunspell jre8 latexdraw ^
rem msys2 nodejs-lts notepadplusplus nvim pandoc python python3 qtpass ripgrep ^
rem sqlite sysinternals totalcommander vim vlc wget WinFsp winrar youtube-dl

winget install Ghisler.TotalCommander

REM Set up symbolic links for configuration files
call :createLink "%HOME%\.ideavimrc" "idea\ideavimrc"
call :createLink "%HOME%\.gitconfig" "git\gitconfig"
call :createLink "%HOME%\.spacemacs" "spacemacs\spacemacs_full"
call :createLink "%HOME%\.emacs.d\core\secure-config.el.gpg" "spacemacs\secure-config.el.gpg"
call :createDirAndLink "%HOME%\.SpaceVim.d" "init.toml" "nvim\init.toml"
call :createDirAndLink "%HOME%\.SpaceVim.d\autoload" "myconfig.vim" "nvim\myconfig.vim"

REM Install nerd and powerline fonts
call :cloneAndInstall "nerd-fonts" "https://github.com/ryanoasis/nerd-fonts"
call :cloneAndInstall "powerline-fonts" "https://github.com/powerline/fonts.git"
git clone https://github.com/sebastiencs/icons-in-terminal "%HOME%\local\tools\icons-fonts"

echo Script completed.
pause

REM Functions
:createLink
if exist "%~1" (
    echo %~1 link already exists.
) else (
    mklink /H "%~1" "%~2"
    echo Created link for %~1
)
exit /b

:createDirAndLink
if not exist "%~1" (
    mkdir "%~1"
    echo Created directory %~1
) else (
    echo %~1 directory already exists.
)
call :createLink "%~1\%~2" "%~3"
exit /b

:cloneAndInstall
if not exist "%HOME%\local\tools\%~1" (
    git clone --depth=1 "%~2" "%HOME%\local\tools\%~1"
)
pushd "%HOME%\local\tools\%~1"
powershell -File install.ps1
popd
exit /b
