$distro = "Ubuntu"

# WSL host IP address
$host_ip = Get-NetIPAddress -InterfaceAlias "vEthernet (WSL)" | Select -ExpandProperty "IPAddress"
echo "Host IP address:  $host_ip"

# WSL guest IP address
$guest_ip = $(wsl -d $distro hostname -I).Trim()
echo "Guest IP address: $guest_ip"

# Proxy settings
$proxy   = $env:PROXY_SERVER
$port    = "3129"
$noproxy = "127.0.0.0/8,10.0.0.0/8,172.16.0.0/12,192.168.0.0/16"

echo "Running command:"
echo ">> px --proxy=$proxy --port=$port --gateway --listen=$host_ip --allow=$guest_ip --noproxy=$noproxy"
& px --proxy=$proxy --port=$port --gateway --listen=$host_ip --allow=$guest_ip --noproxy=$noproxy

