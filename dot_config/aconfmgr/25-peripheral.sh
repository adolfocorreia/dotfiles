# Printer and scanner support
AddPackage cups # OpenPrinting CUPS - daemon package
AddPackage cups-pdf # PDF printer for cups
AddPackage sane-airscan # SANE - SANE backend for AirScan (eSCL) and WSD document scanners
AddPackage system-config-printer # A CUPS printer configuration tool and status applet
AddPackage --foreign hpuld # HP Unified Linux Driver
AddPackage --foreign naps2-bin # NAPS2 - Not Another PDF Scanner. Scan documents to PDF and more, as simply as possible.

IgnorePath '/etc/cups'
IgnorePath '/etc/cups/*.conf'
IgnorePath '/etc/printcap'

# Keyboard customization
AddPackage qmk # CLI tool for customizing supported mechanical keyboards.
AddPackage --foreign vial-appimage # Vial is an open-source cross-platform (Windows, Linux and Mac) GUI and a QMK fork for configuring your keyboard in real time, similar to VIA.
