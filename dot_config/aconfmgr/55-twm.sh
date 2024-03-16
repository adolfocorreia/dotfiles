# Window manager
AddPackage picom # X compositor that may fix tearing issues
AddPackage redshift # Adjusts the color temperature of your screen according to your surroundings.
AddPackage stalonetray # STAnd-aLONE sysTRAY. It has minimal build and run-time dependencies: the Xlib only.
AddPackage wmctrl # Control your EWMH compliant window manager from command line
AddPackage xmobar # Minimalistic Text Based Status Bar
AddPackage xmonad-contrib # Community-maintained extensions for xmonad
AddPackage xmonad # Lightweight X11 tiled window manager written in Haskell
AddPackage rofi # A window switcher, application launcher and dmenu replacement

# Desktop services
AddPackage copyq # Clipboard manager with searchable and editable history
AddPackage dex # Program to generate and execute DesktopEntry files of type Application
AddPackage dunst # Customizable and lightweight notification-daemon
AddPackage gvfs # Virtual filesystem implementation for GIO
AddPackage gvfs-mtp # Virtual filesystem implementation for GIO (MTP backend; Android, media player)
AddPackage gvfs-smb # Virtual filesystem implementation for GIO (SMB/CIFS backend; Windows client)
AddPackage handlr-regex # Powerful alternative to xdg-utils written in Rust
AddPackage perl-file-mimeinfo # Determine file type, includes mimeopen and mimetype
AddPackage polkit-gnome # Legacy polkit authentication agent for GNOME
AddPackage tumbler # Thumbnail service implementing the thumbnail management D-Bus specification
AddPackage udiskie # Removable disk automounter using udisks
AddPackage variety # Changes the wallpaper on a regular interval using user-specified or automatically downloaded images.
AddPackage xdg-user-dirs # Manage user directories like ~/Desktop and ~/Music
AddPackage --foreign autotrash # Tool to automatically purge old trashed files

# Desktop basic apps
AddPackage alacritty # A cross-platform, GPU-accelerated terminal emulator
AddPackage gucharmap # Gnome Unicode Charmap
AddPackage kitty # A modern, hackable, featureful, OpenGL-based terminal emulator
AddPackage l3afpad # Simple plain text editor for GTK 3
AddPackage pcmanfm-gtk3 # Extremely fast and lightweight file manager (GTK+ 3 version)
AddPackage recoll # Full text search tool based on Xapian backend
AddPackage speedcrunch # Simple but powerful calculator using Qt
AddPackage xarchiver # GTK+ frontend to various command line archivers
AddPackage --foreign gscreenshot # A simple screenshot tool supporting multiple backends

# Image viewers and media players
AddPackage feh # Fast and light imlib2-based image viewer
AddPackage gpicview # Lightweight image viewer
AddPackage mpv # a free, open source, and cross-platform media player
AddPackage vlc # Multi-platform MPEG, VCD/DVD, and DivX player
AddPackage --foreign findimagedupes # Tool to find visually similar or duplicate images

AddPackage --foreign vimiv-qt # An image viewer with vim-like keybindings
AddPackage --foreign python-exiv2 # A Python 3 binding to exiv2, the C++ library for manipulation of EXIF, IPTC and XMP image metadata.

IgnorePath '/usr/lib/vlc/plugins/plugins.dat'

# Document viewers
AddPackage xchm # Viewer for CHM files (GUI front-end to chmlib)
AddPackage xreader # Document viewer for files like PDF and Postscript. X-Apps Project.
AddPackage zathura-djvu # DjVu support for Zathura
AddPackage zathura-pdf-mupdf # PDF support for Zathura (MuPDF backend) (Supports PDF, ePub, and OpenXPS)
