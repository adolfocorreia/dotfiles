# Printer and scanner support
AddPackage cups # OpenPrinting CUPS - daemon package
AddPackage cups-pdf # PDF printer for cups
AddPackage sane-airscan # SANE - SANE backend for AirScan (eSCL) and WSD document scanners
AddPackage simple-scan # Simple scanning utility
AddPackage system-config-printer # A CUPS printer configuration tool and status applet
AddPackage --foreign epson-inkjet-printer-escpr # Epson Inkjet Printer Driver (ESC/P-R) for Linux

IgnorePath '/etc/cups'
IgnorePath '/etc/cups/*.conf'
IgnorePath '/etc/printcap'

# Keyboard customization
AddPackage qmk # CLI tool for customizing supported mechanical keyboards.
AddPackage --foreign vial-appimage # Vial is an open-source cross-platform (Windows, Linux and Mac) GUI and a QMK fork for configuring your keyboard in real time, similar to VIA.

CreateLink /etc/kanata.kbd /home/adcor/.config/kanata/kanata.kbd
