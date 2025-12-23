# Base
AddPackage base # Minimal package set to define a basic Arch Linux installation
AddPackage pacman # A library-based package manager with dependency support
AddPackage pacman-contrib # Contributed scripts and tools for pacman systems

# Core utils
AddPackage bash-completion # Programmable completion for the bash shell
AddPackage file # File type identification utility
AddPackage findutils # GNU utilities to locate files
AddPackage gawk # GNU version of awk
AddPackage grep # A string search utility
AddPackage neovim # Fork of Vim aiming to improve user experience, plugins, and GUIs
AddPackage sed # GNU stream editor
AddPackage which # A utility to show the full path of commands

# Documentation
AddPackage man-db # A utility for reading man pages
AddPackage man-pages # Linux man pages
AddPackage texinfo # GNU documentation system for on-line information and printed output
AddPackage arch-wiki-docs # Pages from Arch Wiki optimized for offline browsing
AddPackage arch-wiki-lite # Arch Wiki without HTML. 1/9 as big, easily searched & viewable on console

# Compression
AddPackage 7zip # File archiver for extremely high compression
AddPackage gzip # GNU compression utility
AddPackage unzip # For extracting and viewing files in .zip archives

# Basic tools to build packages
AddPackage base-devel # Basic tools to build Arch Linux packages
AddPackage git # the fast distributed version control system

# Arch utils
AddPackage archlinux-contrib # Collection of contrib scripts used in Arch Linux
AddPackage lostfiles # Find orphaned files not owned by any Arch packages
AddPackage pkgfile # a pacman .files metadata explorer
AddPackage rebuild-detector # Detects which packages need to be rebuilt
AddPackage --foreign aconfmgr-git # A configuration manager for Arch Linux
AddPackage --foreign informant # An Arch Linux News reader and pacman hook
AddPackage --foreign pacman-log-orphans-hook # hook to check whether there are any packages marked as unrequired (orphans) via pacman -Qttdq after every pacman run
AddPackage --foreign paru # Feature packed AUR helper
AddPackage --foreign rate-mirrors # Everyday-use client-side map-aware mirror ranking tool
AddPackage --foreign systemd-boot-pacman-hook # Pacman hook to upgrade systemd-boot after systemd upgrade.
AddPackage --foreign yay # Yet another yogurt. Pacman wrapper and AUR helper written in go.

IgnorePath '/etc/pacman.d/mirrorlist'
