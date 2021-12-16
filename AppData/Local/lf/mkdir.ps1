# TODO: add '-Prompt Directory' as a parameter to Read-Host
New-Item -Path . -ItemType Directory -Name $(Read-Host) | Out-Null
