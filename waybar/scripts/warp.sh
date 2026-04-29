#!/bin/sh

# Check if the wgcf-profile interface exists
if ip link show wgcf-profile > /dev/null 2>&1; then
    # Extract the IPv4 address
    ip_addr=$(ip -4 addr show wgcf-profile | grep -oP '(?<=inet\s)\d+(\.\d+){3}')
    printf '{"text": " %s", "tooltip": "Warp VPN Connected", "class": "connected"}\n' "$ip_addr"
else
    printf '{"text": "", "tooltip": "Warp VPN Disconnected", "class": "disconnected"}\n'
fi
