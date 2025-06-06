# Android
AddPackage android-file-transfer # Android MTP client with minimalistic UI
AddPackage android-tools # Android platform tools
AddPackage --foreign universal-android-debloater # Cross-platform GUI written in Rust using ADB to debloat non-rooted Android devices

# Browsers
AddPackage chromium # A web browser built for speed, simplicity, and security
AddPackage firefox # Standalone web browser from mozilla.org
AddPackage nyxt # A keyboard-driven web browser designed for power users
AddPackage torbrowser-launcher # Securely and easily download, verify, install, and launch Tor Browser in Linux
AddPackage --foreign brave-bin # Web browser that blocks ads and trackers by default (binary release)
AddPackage --foreign librewolf-bin # Community-maintained fork of Firefox, focused on privacy, security and freedom.
AddPackage --foreign mullvad-browser-bin # Privacy-focused web browser developed by Mullvad VPN and the Tor Project
AddPackage --foreign zen-browser-bin # Performance oriented Firefox-based web browser

AddPackage qutebrowser # A keyboard-driven, vim-like browser based on Python and Qt
AddPackage pdfjs-legacy # PDF reader in javascript - legacy distribution
AddPackage python-adblock # Brave's adblock library in Python

# Office
AddPackage aspell # A spell checker designed to eventually replace Ispell
AddPackage aspell-en # English dictionary for aspell
AddPackage hunspell-en_us # US English hunspell dictionaries
AddPackage libreoffice-fresh # LibreOffice branch which contains new features and program enhancements
AddPackage --foreign hunspell-pt-br # Brazillian Portuguese grammar, spelling and hyphenation checker to hunspell
AddPackage --foreign proselint # A linter for prose

IgnorePath '/etc/libreoffice/sofficerc'

# Flatpak support
AddPackage flatpak # Linux application sandboxing and distribution framework (formerly xdg-app)
AddPackage xdg-desktop-portal-gtk # A backend implementation for xdg-desktop-portal using GTK

# Document conversion and manipulation
AddPackage pandoc-cli # Conversion between documentation formats
AddPackage --foreign fntsample # PDF and PostScript font samples generator
AddPackage --foreign pdf.tocgen # Automatically generate table of contents for PDF documents.

# AI apps and tools
AddPackage aichat # All-in-one AI CLI tool featuring Chat-REPL, Shell Assistant, RAG, AI tools & agents
AddPackage ollama-cuda # Create, run and share large language models (LLMs) with CUDA
AddPackage tgpt # AI Chatbots in terminal without needing API keys

# Miscelaneous apps and tools
AddPackage bibletime # Bible study tool
AddPackage calibre # Ebook management application
AddPackage handbrake # Multithreaded video transcoder
AddPackage obsidian # A powerful knowledge base that works on top of a local folder of plain text Markdown files
AddPackage transmission-gtk # Fast, easy, and free BitTorrent client (GTK+ GUI)
AddPackage yt-dlp # A youtube-dl fork with additional features and fixes
AddPackage --foreign czkawka-gui-bin # Find and manage duplicate files, empty folders, similar images, and other "lint" (GUI, with HEIF support)
AddPackage --foreign dropbox # A free service that lets you bring your photos, docs, and videos anywhere and share them easily.
AddPackage --foreign irpf # Brazilian physical person income tax (IRPF) program
AddPackage --foreign whatsapp-nativefier # WhatsApp desktop built with nativefier (electron)
