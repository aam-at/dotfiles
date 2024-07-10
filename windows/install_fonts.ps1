param(
    [Parameter(Mandatory=$true)]
    [string]$fontFolder,

    [Parameter(Mandatory=$false)]
    [switch]$CurrentUser
)

# Function to validate a font file
function Test-FontFile($fontFile) {
    try {
        $privateFont = New-Object System.Drawing.Text.PrivateFontCollection
        $privateFont.AddFontFile($fontFile.FullName)
        $fontFamily = $privateFont.Families[0]
        $fontName = $fontFamily.Name
        $privateFont.Dispose()
        return $true, $fontName
    } catch {
        return $false, $_.Exception.Message
    }
}

# Function to install a font
function Install-Font($fontFile, $isCurrentUser) {
    $isValid, $result = Test-FontFile $fontFile
    if (-not $isValid) {
        Write-Host "Invalid font file: $($fontFile.Name)"
        Write-Host "Error: $result"
        return
    }

    $fontName = $result
    try {
        if ($isCurrentUser) {
            $fontPath = Join-Path $env:LOCALAPPDATA "Microsoft\Windows\Fonts\$($fontFile.Name)"
            $registryPath = "HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts"
        } else {
            $fontPath = Join-Path $env:windir "Fonts\$($fontFile.Name)"
            $registryPath = "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts"
        }

        # Check if the font is already installed
        if (Test-Path $fontPath) {
            Write-Host "Font already installed: $($fontFile.Name)"
            return
        }

        # Copy the font to the appropriate folder
        Copy-Item -Path $fontFile.FullName -Destination $fontPath -Force -ErrorAction Stop

        # Add the font to the registry
        if ($isCurrentUser) {
            New-ItemProperty -Path $registryPath -Name $fontName -Value $fontFile.Name -PropertyType String -Force -ErrorAction Stop | Out-Null
        } else {
            # For all users, we need to use the full path in the registry
            New-ItemProperty -Path $registryPath -Name $fontName -Value $fontPath -PropertyType String -Force -ErrorAction Stop | Out-Null
        }

        # If installing for all users, add the font to the system using AddFontResource
        if (-not $isCurrentUser) {
            $fontResource = [System.Runtime.InteropServices.Marshal]::StringToHGlobalUni($fontPath)
            $result = [FontInstaller]::AddFontResource($fontResource)
            [System.Runtime.InteropServices.Marshal]::FreeHGlobal($fontResource)

            if ($result -eq 0) {
                throw "Failed to add font resource"
            }

            # Notify Windows of the font change
            $HWND_BROADCAST = [IntPtr]0xffff
            $WM_FONTCHANGE = 0x001D
            $result = [FontInstaller]::SendMessage($HWND_BROADCAST, $WM_FONTCHANGE, [IntPtr]::Zero, [IntPtr]::Zero)
        }

        Write-Host "Successfully installed font: $($fontFile.Name)"
    } catch {
        Write-Host "Failed to install font: $($fontFile.Name)"
        Write-Host "Error details: $_"
        Write-Host "Error type: $($_.Exception.GetType().FullName)"
        Write-Host "Error message: $($_.Exception.Message)"
    }
}

# Main script execution
try {
    # Load necessary .NET classes
    Add-Type -AssemblyName System.Drawing

    # Define the P/Invoke signatures for AddFontResource and SendMessage
    $signature = @"
    [DllImport("gdi32.dll", EntryPoint="AddFontResourceW", SetLastError=true, CharSet=CharSet.Unicode)]
    public static extern int AddFontResource(string lpFileName);

    [DllImport("user32.dll", EntryPoint="SendMessageW", SetLastError=true, CharSet=CharSet.Unicode)]
    public static extern IntPtr SendMessage(IntPtr hWnd, uint Msg, IntPtr wParam, IntPtr lParam);
"@
    Add-Type -MemberDefinition $signature -Namespace "FontInstaller" -Name "NativeMethods"

    # Check if running as administrator when installing for all users
    if (-not $CurrentUser) {
        $currentPrincipal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
        $isAdmin = $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

        if (-not $isAdmin) {
            throw "Administrator privileges are required to install fonts for all users. Please run the script as an administrator or use the -CurrentUser switch."
        }
    }

    # Get all font files in the specified folder
    $fontFiles = Get-ChildItem -Path $fontFolder -Include *.ttf,*.otf -Recurse

    # Check if any font files were found
    if ($fontFiles.Count -eq 0) {
        Write-Host "No font files found in the specified folder."
        exit
    }

    # Create a log file
    $logFile = Join-Path $PSScriptRoot "FontInstallation.log"
    Start-Transcript -Path $logFile -Append

    Write-Host "Starting font installation process..."
    Write-Host "Font folder: $fontFolder"
    Write-Host "Total fonts found: $($fontFiles.Count)"
    if ($CurrentUser) {
        Write-Host "Installing for: Current User"
    } else {
        Write-Host "Installing for: All Users"
    }

    # Install each font
    foreach ($font in $fontFiles) {
        Write-Host "Attempting to install: $($font.Name)"
        Install-Font $font $CurrentUser
    }

    Write-Host "Font installation process completed. Please check the log file for details: $logFile"
} catch {
    Write-Host "An error occurred during script execution:"
    Write-Host $_.Exception.Message
} finally {
    Stop-Transcript
}
