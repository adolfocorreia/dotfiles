# Security tools
AddPackage bitwarden # A secure and free password manager for all of your devices
AddPackage keepassxc # Cross-platform community-driven port of Keepass password manager
AddPackage pass # Stores, retrieves, generates, and synchronizes passwords securely
AddPackage pwgen # Password generator for creating easily memorable passwords
AddPackage python-gpgme # Python bindings for GPGme
AddPackage yubikey-manager # Python library and command line tool for configuring a YubiKey
AddPackage --foreign warsaw-bin # Banking security tool developed by GAS Tecnologia

IgnorePath '/usr/local/etc/warsaw'
IgnorePath '/usr/local/etc/warsaw/*'
IgnorePath '/usr/local/lib/warsaw/*'

# SSH keys
IgnorePath '/etc/ssh/*key*'
