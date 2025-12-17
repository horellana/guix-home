#!/bin/sh

text=$(curl -s "https://wttr.in/Santiago.Chile?format=1")

if [ -n "$text" ]; then
  printf '{"text": "%s", "tooltip": "Weather in Santiago"}\n' "$text"
else
  printf '{"text": "", "tooltip": "Offline"}\n'
fi
