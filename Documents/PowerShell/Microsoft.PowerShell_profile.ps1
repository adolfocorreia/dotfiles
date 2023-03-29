# My TMP path
$tmp = "$env:TMP\tmp"
if (-Not (Test-Path $tmp)) { New-Item -ItemType Directory $tmp }

# Set default encoding as UTF-8
# Reference: https://stackoverflow.com/questions/49476326/displaying-unicode-in-powershell/49481797
$OutputEncoding = [Console]::InputEncoding = [Console]::OutputEncoding = New-Object System.Text.UTF8Encoding

# Set proxy credentials
[System.Net.WebRequest]::DefaultWebProxy.Credentials = [System.Net.CredentialCache]::DefaultNetworkCredentials

# Update PATH environment variable
function Refresh-Path {
  $env:PATH = [System.Environment]::GetEnvironmentVariable("PATH","User") + ";" + [System.Environment]::GetEnvironmentVariable("PATH","Machine")
}
Refresh-Path

# Load PSFzf module (https://github.com/kelleyma49/PSFzf)
Import-Module PSFzf



## PSReadLine configuration

Set-PSReadLineOption -EditMode Vi

# Enable bash style completion
Set-PSReadLineKeyHandler -Key Tab -Function Complete
Remove-PSReadLineKeyHandler -Key Shift+Tab

# Enable emacs/readline style mappings on insert mode
# References:
# - https://docs.microsoft.com/en-us/powershell/module/psreadline/about/about_psreadline
# - https://en.wikipedia.org/wiki/GNU_Readline#Editing_modes
Set-PSReadLineKeyHandler -Key Ctrl+b -Function BackwardChar
Set-PSReadLineKeyHandler -Key Ctrl+f -Function ForwardChar
Set-PSReadLineKeyHandler -Key Alt+b  -Function BackwardWord
Set-PSReadLineKeyHandler -Key Alt+f  -Function ForwardWord
Set-PSReadLineKeyHandler -Key Ctrl+a -Function BeginningOfLine
Set-PSReadLineKeyHandler -Key Ctrl+e -Function EndOfLine
Set-PSReadLineKeyHandler -Key Ctrl+h -Function BackwardDeleteChar
Set-PSReadLineKeyHandler -Key Ctrl+d -Function DeleteCharOrExit
Set-PSReadLineKeyHandler -Key Ctrl+w -Function BackwardDeleteWord
Set-PSReadLineKeyHandler -Key Alt+d  -Function DeleteWord
Set-PSReadLineKeyHandler -Key Ctrl+u -Function BackwardDeleteInput
Set-PSReadLineKeyHandler -Key Ctrl+k -Function DeleteToEnd
Set-PSReadLineKeyHandler -Key Ctrl+p -Function HistorySearchBackward
Set-PSReadLineKeyHandler -Key Ctrl+n -Function HistorySearchForward
Set-PSReadLineKeyHandler -Key Ctrl+m -Function ValidateAndAcceptLine
Set-PSReadLineKeyHandler -Key Ctrl+y -Function Paste
Set-PSReadLineKeyHandler -Key Ctrl+_ -Function Undo

# Enable fish style prediction
# Reference: https://devblogs.microsoft.com/powershell/announcing-psreadline-2-1-with-predictive-intellisense
Set-PSReadLineOption -PredictionSource HistoryAndPlugin
Set-PSReadLineOption -HistorySearchCursorMovesToEnd
Set-PSReadLineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadLineKeyHandler -Key DownArrow -Function HistorySearchForward
# Set-PSReadLineKeyHandler -Key Ctrl+f -Function AcceptSuggestion
# Set-PSReadLineKeyHandler -Key Alt+f -Function AcceptNextSuggestionWord

# Increase color contrast of shell elements
# Escape code references:
# - https://en.wikipedia.org/wiki/ANSI_escape_code
# - https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
# - https://docs.microsoft.com/en-us/powershell/module/psreadline/set-psreadlineoption
# - https://upload.wikimedia.org/wikipedia/commons/1/15/Xterm_256color_chart.svg
Set-PSReadLineOption -Colors @{
  "Operator"         = "`e[37m"
  "Parameter"        = "`e[37m"
  "InlinePrediction" = "`e[38;5;244m"
}

# Use cursor shape to display vi mode changes
# References:
# - https://docs.microsoft.com/en-us/powershell/module/psreadline/set-psreadlineoption#example-6--use-vimodechangehandler-to-display-vi-mode-changes
# - https://terminalguide.namepad.de/seq/csi_sq_t_space
# - https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
# - https://github.com/microsoft/terminal/pull/7379
function OnViModeChange {
  if ($args[0] -eq 'Command') {
    # Set the cursor to a blinking block
    Write-Host -NoNewLine "`e[1 q"
  } else {
    # Set the cursor to a blinking underline
    Write-Host -NoNewLine "`e[3 q"
  }
}
Set-PSReadLineOption -ViModeIndicator Script -ViModeChangeHandler $Function:OnViModeChange

# Search paths
Set-PsFzfOption -PSReadlineChordProvider Ctrl+t
# Search command history
Set-PsFzfOption -PSReadlineChordReverseHistory Ctrl+r
# fzf Ctrl-R options
$env:FZF_CTRL_R_OPTS = "--scheme=history"
# Enable 'fkill' alias for Invoke-FuzzyKillProcess
Set-PsFzfOption -EnableAliasFuzzyKillProcess
# Uses the fd command instead of OS specific file and directory commands
Set-PsFzfOption -EnableFd



# Delete default Powershell aliases that conflict with shell commands
# - https://docs.microsoft.com/en-us/powershell/scripting/learn/compatibility-aliases

# Useful PowerShell command:
# - Get-Command: gets all commands installed on computer including cmdlets, aliases,
#                functions, filters, scripts and applications
#   - Use "(Get-Command <function>).Definition" to see the function's definition
# - Get-Alias: gets the aliases in the current session
# - pwsh -NoProfile: open PowerShell with default profile

@(
"cat",
"cp",
"diff",
"ls",
"mv",
"pwd",
"r",
"rm",
"rmdir",
"sort",
"tee"
) | ForEach-Object { if (Test-Path Alias:$_) { Remove-Alias -Force $_ } }


Function ls { & coreutils ls     --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* @args }
Function ll { & coreutils ls -l  --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* @args }
Function la { & coreutils ls -la --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* @args }

Function mkdir { & coreutils mkdir $args }


# Create other shell aliases and functions

Function Start-Explorer { & explorer . }
Set-Alias -Name "e." -Value Start-Explorer
Set-Alias -Name e -Value explorer
Set-Alias -Name trash -Value recycle-bin
Function vi { & nvim --clean $args }

Function Create-Link ([String] $link, [String] $real) {
  if (Test-Path $real -PathType Container) {
    # Create directory junction (must be on the same computer)
    cmd /C mklink /J $link.Replace("/", "\") $real.Replace("/", "\")
  } else {
    # Create file hard link (must be on the same volume)
    cmd /C mklink /H $link.Replace("/", "\") $real.Replace("/", "\")
  }
}


# Echo selected path
$env:_ZO_ECHO=1
# Load zoxide
Invoke-Expression (& { (zoxide init --hook pwd powershell) -join "`n" } )



Function Invoke-Starship-PreCommand {
  # Make Starship change terminal window title
  # Reference: https://github.com/starship/starship/pull/3115
  $CurrentWorkingDirectory = Split-Path -Path (Get-Location) -Leaf
  $CurrentWorkingProcess = (Get-Process -Id $PID).ProcessName
  $Host.UI.RawUI.WindowTitle = "$CurrentWorkingProcess | $CurrentWorkingDirectory"

  # Open new Windows Terminal tab/pane in the same directory
  # References:
  # - https://docs.microsoft.com/en-us/windows/terminal/tutorials/new-tab-same-directory
  # - https://github.com/microsoft/terminal/issues/3158
  $Host.UI.Write("`e]9;9;$pwd`a")
}

# Load Starship
Invoke-Expression $(& starship init powershell)

