# Window manager
AddPackage qtile # A full-featured, pure-Python tiling window manager
AddPackage python-dbus-fast # A faster version of dbus-next
AddPackage --foreign qtile-extras-git # Unofficial mods for qtile that are unlikely to be maintained in the main repo.

# Window manager support
AddPackage dmenu # Generic menu for X
AddPackage dunst # Customizable and lightweight notification-daemon
AddPackage picom # X compositor that may fix tearing issues
AddPackage polkit-gnome # Legacy polkit authentication agent for GNOME
AddPackage redshift # Adjusts the color temperature of your screen according to your surroundings.
AddPackage rofi # A window switcher, application launcher and dmenu replacement
AddPackage wmctrl # Control your EWMH compliant window manager from command line
AddPackage --foreign caffeine-ng # Status bar application able to temporarily inhibit the screensaver and sleep mode.
AddPackage --foreign noti # Monitor a process and trigger a notification

# Folder utils
AddPackage dex # Program to generate and execute DesktopEntry files of type Application
AddPackage tumbler # Thumbnail service implementing the thumbnail management D-Bus specification
AddPackage xdg-user-dirs # Manage user directories like ~/Desktop and ~/Music
AddPackage --foreign autotrash # Tool to automatically purge old trashed files

# File system services
AddPackage gvfs-mtp # Virtual filesystem implementation for GIO (MTP backend; Android, media player)
AddPackage gvfs-smb # Virtual filesystem implementation for GIO (SMB/CIFS backend; Windows client)
AddPackage gvfs # Virtual filesystem implementation for GIO
AddPackage udiskie # Removable disk automounter using udisks

# Application management
AddPackage handlr-regex # Powerful alternative to xdg-utils written in Rust
AddPackage perl-file-mimeinfo # Determine file type, includes mimeopen and mimetype

# Desktop basic apps
AddPackage alacritty # A cross-platform, GPU-accelerated terminal emulator
AddPackage copyq # Clipboard manager with searchable and editable history
AddPackage feh # Fast and light imlib2-based image viewer
AddPackage gucharmap # Gnome Unicode Charmap
AddPackage kitty # A modern, hackable, featureful, OpenGL-based terminal emulator
AddPackage l3afpad # Simple plain text editor for GTK 3
AddPackage pcmanfm-gtk3 # Extremely fast and lightweight file manager (GTK+ 3 version)
AddPackage recoll # Full text search tool based on Xapian backend
AddPackage speedcrunch # Simple but powerful calculator using Qt
AddPackage variety # Changes the wallpaper on a regular interval using user-specified or automatically downloaded images.
AddPackage xarchiver # GTK+ frontend to various command line archivers
AddPackage --foreign gscreenshot # A simple screenshot tool supporting multiple backends

