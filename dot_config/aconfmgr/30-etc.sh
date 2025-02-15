IgnorePath '/etc/machine-id'
IgnorePath '/etc/os-release'
IgnorePath '/usr/lib/locale'

CreateLink /etc/localtime /usr/share/zoneinfo/America/Sao_Paulo

CopyFile /etc/borgmatic/config.yaml
CopyFile /etc/conf.d/snapper
CopyFile /etc/cron.daily/rsnapshot 755
CopyFile /etc/fstab
CopyFile /etc/hostname
CopyFile /etc/locale.conf
CopyFile /etc/logrotate.conf
CopyFile /etc/modprobe.d/hda-jack-retask.conf
CopyFile /etc/pacman.conf
CopyFile /etc/pacman.d/hooks/xmonad.hook
CopyFile /etc/rsnapshot.conf
CopyFile /etc/rsyncd.conf
CopyFile /etc/security/faillock.conf
CopyFile /etc/smartd.conf
CopyFile /etc/snapper/configs/cloud 640
CopyFile /etc/snapper/configs/documents 640
CopyFile /etc/snapper/configs/media 640
CopyFile /etc/snapper/configs/provisional 640
CopyFile /etc/systemd/network/20-wired.network
CopyFile /etc/udev/rules.d/50-qmk.rules
CopyFile /etc/vconsole.conf
CopyFile /etc/xdg/reflector/reflector.conf
CopyFile /usr/lib/firmware/hda-jack-retask.fw '' adcor adcor
CopyFile /usr/share/smartmontools/smartd_warning.d/smartdnotify 755
