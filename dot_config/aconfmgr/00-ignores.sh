# Boot partition
IgnorePath '/boot'
IgnorePath '/boot/intel-ucode.img'

# Mount files
IgnorePath '/lost+found'
IgnorePath '/swapfile'

# Cache and generated files
IgnorePath '/etc/*.cache'
IgnorePath '/etc/*.gen'
IgnorePath '/usr/lib/*.cache'
IgnorePath '/usr/share/*.cache'

# Desktop environment resources
IgnorePath '/etc/fonts'
IgnorePath '/usr/share/fonts'
IgnorePath '/usr/share/icons'
IgnorePath '/usr/share/mime'

# Certificate databases
IgnorePath '/etc/ca-certificates/extracted'
IgnorePath '/etc/pacman.d/gnupg'
IgnorePath '/etc/ssl/certs'

# User and password files
IgnorePath '/etc/group'
IgnorePath '/etc/group-'
IgnorePath '/etc/gshadow'
IgnorePath '/etc/gshadow-'
IgnorePath '/etc/passwd'
IgnorePath '/etc/passwd-'
IgnorePath '/etc/shadow'
IgnorePath '/etc/shadow-'
IgnorePath '/etc/shells'
IgnorePath '/etc/sudoers'

# Binary libraries
IgnorePath '/usr/lib/ghc*'
IgnorePath '/usr/lib/modules'
IgnorePath '/usr/lib/python*'
IgnorePath '/usr/lib/udev'

# Var databases, logs, swap, spool and temp files
IgnorePath '/var/db/sudo'
IgnorePath '/var/lib'
IgnorePath '/var/log'
IgnorePath '/var/spool'
IgnorePath '/var/tmp'

# Misc
IgnorePath '/etc/adjtime'
IgnorePath '/etc/.pwd.lock'
IgnorePath '/etc/.updated'
IgnorePath '/var/.updated'

IgnorePath '/usr/share/glib-2.0/schemas/gschemas.compiled'
IgnorePath '/usr/share/info/dir'
IgnorePath '/usr/share/perl5/vendor_perl/XML/SAX/ParserDetails.ini'
