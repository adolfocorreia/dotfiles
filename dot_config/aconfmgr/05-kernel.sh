# Kernel
AddPackage linux # The Linux kernel and modules
AddPackage linux-lts # The LTS Linux kernel and modules
AddPackage linux-firmware # Firmware files for Linux
AddPackage intel-ucode # Microcode update files for Intel CPUs

# Video drivers and tools
AddPackage intel-gpu-tools # Tools for development and testing of the Intel DRM driver
AddPackage intel-media-driver # Intel Media Driver for VAAPI — Broadwell+ iGPUs
AddPackage intel-media-sdk # Legacy API for hardware video acceleration on Intel GPUs (Broadwell to Rocket Lake)
AddPackage nvidia-lts # NVIDIA drivers for linux-lts
AddPackage nvidia # NVIDIA kernel modules
AddPackage nvidia-settings # Tool for configuring the NVIDIA graphics driver

CopyFile /etc/X11/xorg.conf.d/10-intel-xorg-nvidia-cuda.conf

# Boot
AddPackage efibootmgr # Linux user-space application to modify the EFI Boot Manager

IgnorePath '/etc/mkinitcpio.d'
IgnorePath '/etc/nvme'

# File system tools
AddPackage btrfs-progs # Btrfs filesystem utilities
AddPackage dosfstools # DOS filesystem utilities
AddPackage e2fsprogs # Ext2/3/4 filesystem utilities
AddPackage mtools # A collection of utilities to access MS-DOS disks
