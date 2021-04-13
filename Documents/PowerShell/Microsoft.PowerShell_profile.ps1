## My TMP path
$tmp = "$env:TMP\tmp"
if (-Not (Test-Path $tmp)) { New-Item -ItemType Directory $tmp }



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
Set-PSReadLineKeyHandler -Key Ctrl+Spacebar -Function AcceptSuggestion

# Search paths
Set-PsFzfOption -PSReadlineChordProvider 'Ctrl+t'
# Search command history
Set-PsFzfOption -PSReadlineChordReverseHistory 'Ctrl+r'



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
"ps",
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

Function ls { & ls.exe --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* $args }
Function ll { & ls.exe -l --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* $args }
Function la { & ls.exe -la --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* $args }

Function mkdir { & mkdir.exe $args }



# Echo selected path
$env:_ZO_ECHO=1
# Load zoxide
Invoke-Expression (& { (zoxide init --hook pwd powershell) -join "`n" } )



# Load Starship
Invoke-Expression (& starship init powershell)

