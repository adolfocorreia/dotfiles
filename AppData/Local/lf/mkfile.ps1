# TODO: add '-Prompt File' as a parameter to Read-Host
New-Item -Path . -ItemType File -Name $(Read-Host) | Out-Null
