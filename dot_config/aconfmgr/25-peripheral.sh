# Printer and scanner support
AddPackage cups # OpenPrinting CUPS - daemon package
AddPackage cups-pdf # PDF printer for cups
AddPackage sane-airscan # SANE - SANE backend for AirScan (eSCL) and WSD document scanners
AddPackage simple-scan # Simple scanning utility
AddPackage system-config-printer # A CUPS printer configuration tool and status applet
AddPackage --foreign epson-inkjet-printer-escpr # Epson Inkjet Printer Driver (ESC/P-R) for Linux

IgnorePath '/etc/printcap'

# Keyboard customization
AddPackage qmk # CLI tool for customizing supported mechanical keyboards.
AddPackage --foreign kanata # Bring the customizability of a QMK board to any keyboard near you

CreateLink /etc/kanata.kbd /home/adcor/.config/kanata/kanata.kbd
