#!/bin/sh

# Run speedtest and capture output as JSON
# We use --json for easy parsing and --secure for https
if ! speedtest_output=$(speedtest-cli --json --secure 2>/dev/null); then
    echo '{"text": "Offline", "tooltip": "Speedtest failed"}'
    exit 0
fi

# Parse the JSON using jq
# speedtest-cli output is in bits/s, so we divide by 1,000,000 to get Mbps
echo "$speedtest_output" | jq --unbuffered --compact-output '
  {
    text: "↓ \((.download / 1000000) | round) Mbps  ↑ \((.upload / 1000000) | round) Mbps",
    tooltip: "Ping: \(.ping)ms\nDownload: \((.download / 1000000) | tostring | .[0:5]) Mbps\nUpload: \((.upload / 1000000) | tostring | .[0:5]) Mbps\nISP: \(.client.isp)"
  }
'
