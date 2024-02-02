# Network tools
AddPackage bind # A complete, highly portable implementation of the DNS protocol
AddPackage inetutils # A collection of common network programs
AddPackage net-tools # Configuration tools for Linux networking
AddPackage nmap # Utility for network discovery and security auditing
AddPackage openssh # SSH protocol implementation for remote login, command execution and file transfer
AddPackage traceroute # Tracks the route taken by packets over an IP network
AddPackage wget # Network utility to retrieve files from the Web
AddPackage wol # Wake On LAN functionality in a small program. It wakes up hardware that is Magic Packet compliant

# Network file systems
AddPackage cifs-utils # CIFS filesystem user-space tools
AddPackage nfs-utils # Support programs for Network File Systems

CreateFile /etc/samba/smb.conf > /dev/null
