# Security tools
AddPackage keepassxc # Cross-platform community-driven port of Keepass password manager
AddPackage lastpass-cli # LastPass command line interface tool
AddPackage pass # Stores, retrieves, generates, and synchronizes passwords securely
AddPackage pwgen # Password generator for creating easily memorable passwords
AddPackage python-gpgme # Python bindings for GPGme
AddPackage yubikey-manager # Python library and command line tool for configuring a YubiKey
AddPackage --foreign warsaw-bin # Banking security tool developed by GAS Tecnologia

IgnorePath '/usr/local/etc/warsaw'
IgnorePath '/usr/local/etc/warsaw/gas.dbd'

# SSH keys
IgnorePath '/etc/ssh/*key*'
