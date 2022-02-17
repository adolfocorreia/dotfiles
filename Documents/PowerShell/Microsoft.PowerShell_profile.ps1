# My TMP path
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
Set-PSReadLineKeyHandler -Key Ctrl+f -Function AcceptSuggestion
Set-PSReadLineKeyHandler -Key Alt+f -Function AcceptNextSuggestionWord

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

# Search paths
Set-PsFzfOption -PSReadlineChordProvider Ctrl+t
# Search command history
Set-PsFzfOption -PSReadlineChordReverseHistory Ctrl+r




# Delete default Powershell aliases that conflict with shell commands
# - https://docs.microsoft.com/en-us/powershell/scripting/learn/compatibility-aliases
# - https://github.com/Pscx/Pscx

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
"sort",
"tee"
) | ForEach-Object { if (Test-Path Alias:$_) { Remove-Alias -Force $_ } }


# Create aliases for the coreutil commands
# - https://uutils.github.io/coreutils-docs/user

@(
# "[",
"arch",
"base32",
"base64",
"basename",
"basenc",
"cat",
"cksum",
"comm",
"cp",
"csplit",
"cut",
"date",
# "dd",
"df",
# "dircolors",
"dirname",
# "du",
# "echo",
"env",
# "expand",
"expr",
"factor",
# "false",
"fmt",
"fold",
"hashsum",
"head",
# "hostname",
"join",
# "link",
# "ln",
# "ls",
"md5sum",
# "mkdir",
"mktemp",
# "more",
"mv",
"nl",
"nproc",
"numfmt",
"od",
"paste",
"pr",
"printenv",
"printf",
"ptx",
"pwd",
# "readlink",
"realpath",
# "relpath",
"rm",
# "rmdir",
"seq",
"sha1sum",
"sha224sum",
"sha256sum",
"sha3-224sum",
"sha3-256sum",
"sha3-384sum",
"sha3-512sum",
"sha384sum",
"sha3sum",
"sha512sum",
"shake128sum",
"shake256sum",
# "shred",
"shuf",
# "sleep",
# "sort",
"split",
"sum",
# "sync",
"tac",
"tail",
"tee",
"test",
"touch",
"tr",
# "true",
"truncate",
"tsort",
"unexpand",
"uniq",
# "unlink",
"wc",
# "whoami",
"yes"
) | ForEach-Object {
    # if (Get-Command $_ -ErrorAction SilentlyContinue) {
    #     Write-Output "Command $_ already defined!"
    # }
    $Code = [ScriptBlock]::Create("& coreutils $_ `$args")
    $null = New-Item -Path Function: -Name "script:$_" -Value $Code
  }

Function ls { & coreutils ls     --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* @args }
Function ll { & coreutils ls -l  --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* @args }
Function la { & coreutils ls -la --show-control-chars -F --color --ignore=NTUSER.DAT* --ignore=ntuser.dat* @args }


# Create other shell aliases and functions

Function Start-Explorer { & explorer . }
Set-Alias -Name "e." -Value Start-Explorer
Set-Alias -Name e -Value explorer
Set-Alias -Name vi -Value nvim
Set-Alias -Name trash -Value recycle-bin


# Change working dir in powershell to last dir in lf on exit
# Reference: https://github.com/gokcehan/lf/blob/master/etc/lfcd.ps1
Function lfcd {
  $tmp_file = [System.IO.Path]::GetTempFileName()
  & lf.exe -last-dir-path="$tmp_file" @args
  if (Test-Path -PathType Leaf "$tmp_file") {
    $dir = Get-Content "$tmp_file"
    Remove-Item -Force "$tmp_file"
    if (Test-Path -PathType Container "$dir") {
      if ("$dir" -ne "$pwd") {
        cd "$dir"
      }
    }
  }
}
Set-PSReadLineKeyHandler -Key Ctrl+o -ScriptBlock {
  [Microsoft.PowerShell.PSConsoleReadLine]::RevertLine()
  lfcd
  [Microsoft.PowerShell.PSConsoleReadLine]::AcceptLine()
}
# Set LF_ICONS environment variable
. $env:LOCALAPPDATA\lf\icons.ps1


# Echo selected path
$env:_ZO_ECHO=1
# Load zoxide
Invoke-Expression (& { (zoxide init --hook pwd powershell) -join "`n" } )



Function Invoke-Starship-PreCommand {
  # Make Starship change terminal window title
  # Reference: https://github.com/starship/starship/pull/3115
  $CurrentWorkingDirectory = Split-Path -Path (Get-Location) -Leaf
  $CurrentWorkingProcess = (Get-Process -Id $PID).ProcessName
  $Host.UI.RawUI.WindowTitle = "$CurrentWorkingProcess / $CurrentWorkingDirectory"

  # Open new Windows Terminal tab/pane in the same directory
  # References:
  # - https://docs.microsoft.com/en-us/windows/terminal/tutorials/new-tab-same-directory
  # - https://github.com/microsoft/terminal/issues/3158
  $Host.UI.Write("`e]9;9;$pwd`a")
}

# Load Starship
Invoke-Expression $(& starship init powershell)

