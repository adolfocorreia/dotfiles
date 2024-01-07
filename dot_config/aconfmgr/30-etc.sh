IgnorePath '/etc/hostname'
IgnorePath '/etc/logrotate.conf'
IgnorePath '/etc/machine-id'
IgnorePath '/etc/os-release'

# CopyFile /etc/locale.conf
CreateLink /etc/localtime /usr/share/zoneinfo/America/Sao_Paulo
