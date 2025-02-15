# Kernel
AddPackage linux # The Linux kernel and modules
AddPackage linux-lts # The LTS Linux kernel and modules
AddPackage linux-firmware # Firmware files for Linux
AddPackage intel-ucode # Microcode update files for Intel CPUs
AddPackage nvidia # NVIDIA kernel modules
AddPackage nvidia-lts # NVIDIA drivers for linux-lts
AddPackage nvidia-settings # Tool for configuring the NVIDIA graphics driver

# Boot
AddPackage efibootmgr # Linux user-space application to modify the EFI Boot Manager

IgnorePath '/etc/mkinitcpio.d'
IgnorePath '/etc/nvme'

# File system tools
AddPackage btrfs-progs # Btrfs filesystem utilities
AddPackage dosfstools # DOS filesystem utilities
AddPackage e2fsprogs # Ext2/3/4 filesystem utilities
AddPackage mtools # A collection of utilities to access MS-DOS disks
