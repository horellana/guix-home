#!/bin/sh

# Check if the wgcf-profile interface exists
if ip link show wgcf-profile > /dev/null 2>&1; then
    printf '{"text": "", "tooltip": "Warp VPN Connected", "class": "connected"}\n'
else
    printf '{"text": "", "tooltip": "Warp VPN Disconnected", "class": "disconnected"}\n'
fi
