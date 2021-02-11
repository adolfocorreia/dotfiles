## My TMP path
$tmp = "$env:TMP\tmp"
if (-Not (Test-Path $tmp)) { New-Item -ItemType Directory $tmp }



# Load Pscx module (https://github.com/Pscx/Pscx)
Import-Module Pscx

# Load z module (https://github.com/badmotorfinger/z)
Import-Module z



## PSReadLine configuration

#Set-PSReadLineOption -EditMode Emacs
#Set-PSReadLineKeyHandler -Key Ctrl+d -Function DeleteCharOrExit

Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -HistorySearchCursorMovesToEnd

Set-PSReadLineKeyHandler -Key Ctrl+d -Function MenuComplete
Set-PSReadLineKeyHandler -Key Ctrl+f -Function ForwardWord
Set-PSReadLineKeyHandler -Key Ctrl+b -Function BackwardWord
Set-PSReadLineKeyHandler -Key Ctrl+z -Function Undo
Set-PSReadLineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadLineKeyHandler -Key DownArrow -Function HistorySearchForward

Set-PSReadLineKeyHandler -Key Tab -Function Complete
Remove-PSReadLineKeyHandler -Key Shift+Tab



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

Set-Alias -Name vi -Value vim

Function ls { & ls.exe --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* $args }
Function ll { & ls.exe -l --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* $args }
Function la { & ls.exe -la --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* $args }



# Load Starship
Invoke-Expression (& starship init powershell)
