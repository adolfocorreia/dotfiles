## My TMP path
$tmp = "$env:TMP\tmp"
if (-Not (Test-Path $tmp)) { New-Item -ItemType Directory $tmp }



# Set default encoding as UTF-8
# Reference: https://stackoverflow.com/questions/49476326/displaying-unicode-in-powershell/49481797
$OutputEncoding = [Console]::InputEncoding = [Console]::OutputEncoding = New-Object System.Text.UTF8Encoding



# Load Pscx module (https://github.com/Pscx/Pscx)
# Import-Module Pscx

# Load PSFzf module (https://github.com/kelleyma49/PSFzf)
Import-Module PSFzf



## PSReadLine configuration

Set-PSReadLineOption -EditMode Vi

# Enable bash style completion
Set-PSReadLineKeyHandler -Key Tab -Function Complete
Remove-PSReadLineKeyHandler -Key Shift+Tab

# Enable fish style prediction
# Reference: https://devblogs.microsoft.com/powershell/announcing-psreadline-2-1-with-predictive-intellisense
Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -HistorySearchCursorMovesToEnd
Set-PSReadLineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadLineKeyHandler -Key DownArrow -Function HistorySearchForward
Set-PSReadLineKeyHandler -Key Ctrl+p -Function HistorySearchBackward
Set-PSReadLineKeyHandler -Key Ctrl+n -Function HistorySearchForward
Set-PSReadLineKeyHandler -Key Ctrl+Spacebar -Function AcceptSuggestion

# Increase color contrast of inline predictions.
# Escape code references:
# - https://en.wikipedia.org/wiki/ANSI_escape_code
# - https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
# - https://upload.wikimedia.org/wikipedia/commons/1/15/Xterm_256color_chart.svg
Set-PSReadLineOption -Colors @{ "InlinePrediction"="`e[38;5;244m" }

# Search paths
Set-PsFzfOption -PSReadlineChordProvider Ctrl+t
# Search command history
Set-PsFzfOption -PSReadlineChordReverseHistory Ctrl+r



# Delete default Powershell aliases that conflict with shell commands
@(
"cat",
"cp",
"curl",
"diff",
"e",
"kill",
"ln",
"ls",
"man",
"mkdir",
"mv",
#"ps",
"pwd",
"r",
"rm",
"rmdir",
#"sleep",
"sort",
"su",
"tail",
"touch",
"tee",
"wget"
) | ForEach-Object { if (Test-Path Alias:$_) { Remove-Alias -Force $_ } }



# Create shell aliases and functions

Function Start-Explorer { & explorer . }
Set-Alias -Name "e." -Value Start-Explorer
Set-Alias -Name e -Value explorer
Set-Alias -Name vi -Value nvim
Set-Alias -Name trash -Value recycle-bin

Function ls { & ls.exe     --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* @args }
Function ll { & ls.exe -l  --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* @args }
Function la { & ls.exe -la --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* @args }

Function mkdir { & mkdir.exe $args }



# Echo selected path
$env:_ZO_ECHO=1
# Load zoxide
Invoke-Expression (& { (zoxide init --hook pwd powershell) -join "`n" } )



# Load Starship
Invoke-Expression $(& starship init powershell)

# Change terminal window title
# Reference: https://github.com/starship/starship/issues/1676
$PromptScript = (Get-Item Function:Prompt).ScriptBlock
Function Prompt {
  # Before doing anything else, capture current $?
  $PreservedExitStatus = $?

  $CurrentWorkingDirectory = Split-Path -Path (Get-Location) -Leaf
  $CurrentWorkingProcess = (Get-Process -Id $PID).ProcessName
  $Host.UI.RawUI.WindowTitle = "$CurrentWorkingProcess / $CurrentWorkingDirectory"

  if ($? -ne $PreservedExitStatus) {
    # Reset $? to False
    Write-Error "" -ErrorAction Ignore # Powershell 7+
  }
  Invoke-Command $PromptScript
}

