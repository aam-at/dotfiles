@echo off

rem REM Install applications using Chocolatey
choco install -y ^
    ag curl delta far ffmpeg gnupg gpg4win msys2 qtpass ripgrep sqlite ^
    sysinternals totalcommander vlc wget WinFsp yt-dlp

rem REM Install applications using Winget
winget install --scope machine ^
    7zip Anki.Anki AutoHotkey.AutoHotkey Git.Git gitui HandBrake Helix.Helix ^
    Hunspell Neovim.Neovim Notepad OpenJS.NodeJS.LTS Sqlite.Sqlite vim.vim ^
    WinRAR

rem REM Set up symbolic links for configuration files
call :createLink "%HOME%/.ideavimrc" "../idea/ideavimrc"
call :createLink "%HOME%/.gitconfig" "../git/gitconfig"
call :createDirAndLink "%HOME%/.spacemacs.d" "init.el" "../spacemacs/spacemacs_full"
call :createGlobLinks "%HOME%/.spacemacs.d/config" "../spacemacs/config" "*"
call :createGlobLinks "%AppData%/helix" "../config/helix" "*"
call :createGlobLinks "%AppData%/gitui" "../config/gitui" "*"
call :createDirAndLink "%HOME%/.SpaceVim.d" "init.toml" "../nvim/init.toml"
call :createDirAndLink "%HOME%/.SpaceVim.d/autoload" "myconfig.vim" "../nvim/myconfig.vim"

rem REM Install nerd, powerline and icons fonts
call :cloneAndInstall "nerd-fonts" "https://github.com/ryanoasis/nerd-fonts"
call :cloneAndInstall "powerline-fonts" "https://github.com/powerline/fonts.git"
call :cloneAndInstall "icons-fonts" "https://github.com/sebastiencs/icons-in-terminal"
call :cloneAndInstall "all-icons-fonts" "https://github.com/domtronn/all-the-icons.el"
rem REM Download, unzip, and install JetBrains Mono font
call :installJetBrainsMono

echo Script completed.
pause

rem Function to create a symbolic link
:createLink
if exist "%~1" (
    echo %~1 link already exists.
) else (
    mklink /H "%~1" "%~2"
    echo Created link for %~1
)
exit /b

rem Function to handle glob linking
:createGlobLinks
if not exist "%~1" (
    mkdir "%~1"
    echo Created directory %~1
) else (
    echo %~1 directory already exists.
)
setlocal enabledelayedexpansion
for %%F in ("%~2/%~3") do (
    set "src=%%F"
    set "filename=%%~nxF"
    call :createLink "%~1/!filename!" "%~2/%%F"
)
endlocal
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

:createDirLink
if not exist "%~2" (
    mkdir "%~2"
    rmdir /S /Q "%~2"
)
junction "%~2" "%~1"
if exist "%~2" (
    echo %~2 directory link already exists.
) else (
    mklink /D "%~2" "%~1"
    echo Created directory link from %~1 to %~2
)
exit /b

:cloneAndInstall
if not exist "%HOME%\local\tools\%~1" (
    git clone --depth=1 "%~2" "%HOME%\local\tools\%~1"
)
powershell -File install_fonts.ps1 -fontFolder "%HOME%\local\tools\%~1"
exit /b

:installJetBrainsMono
set "jetbrainsDir=%HOME%\local\tools\jetbrains-fonts"
set "jetbrainsZip=%jetbrainsDir%\JetBrainsMono.zip"
if not exist "%jetbrainsDir%" mkdir "%jetbrainsDir%"
if not exist "%jetbrainsZip%" (
    powershell -Command "(New-Object Net.WebClient).DownloadFile('https://download.jetbrains.com/fonts/JetBrainsMono-2.304.zip', '%jetbrainsZip%')"
)
powershell -Command "Expand-Archive -Path '%jetbrainsZip%' -DestinationPath '%jetbrainsDir%' -Force"
powershell -File install_fonts.ps1 -fontFolder "%jetbrainsDir%\fonts\ttf"
exit /b
