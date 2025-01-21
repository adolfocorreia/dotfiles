# System management tools
AddPackage gparted # A Partition Magic clone, frontend to GNU Parted
AddPackage gsmartcontrol # A graphical user interface for the smartctl hard disk drive health inspection tool.
AddPackage htop # Interactive process viewer
AddPackage logrotate # Rotates system logs automatically
AddPackage plocate # Alternative to locate, faster and compatible with mlocate's database.
AddPackage smartmontools # Control and monitor S.M.A.R.T. enabled ATA and SCSI Hard Drives
AddPackage snapper # A tool for managing BTRFS and LVM snapshots. It can create, diff and restore snapshots and provides timelined auto-snapping.
AddPackage systemd-ui # Graphical front-end for systemd
AddPackage usbutils # A collection of USB tools to query connected USB devices
AddPackage --foreign btrfs-assistant # An application for managing BTRFS subvolumes and Snapper snapshots

# Backup tools
AddPackage borg # Deduplicating backup program with compression and authenticated encryption
AddPackage borgmatic # Simple, configuration-driven backup software for servers and workstations
AddPackage grsync # GTK+ GUI for rsync to synchronize folders, files and make backups
AddPackage rclone # Sync files to and from Google Drive, S3, Swift, Cloudfiles, Dropbox and Google Cloud Storage
AddPackage rsnapshot # A remote filesystem snapshot utility
AddPackage rsync # A fast and versatile file copying tool for remote and local files
AddPackage vorta # Desktop Client for Borg Backup
AddPackage --foreign backblaze-b2 # Backblaze B2 Command Line Client

# Scheduling tools
AddPackage at # AT and batch delayed command scheduling utility and daemon
AddPackage cronie # Daemon that runs specified programs at scheduled times and related tools

# Default runtimes
CreateLink /usr/lib/jvm/default java-23-openjdk
CreateLink /usr/lib/jvm/default-runtime java-23-openjdk
