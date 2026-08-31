#!/bin/sh
# Elige un sink de audio con wofi, lo deja como salida por defecto y mueve
# todos los streams de salida ya existentes a ese sink.
set -eu

sinks=$(pw-dump | jq -r '
  .[]
  | select(.info.props["media.class"] == "Audio/Sink")
  | [ .id,
      .info.props["object.serial"],
      (.info.props["node.description"] // .info.props["node.name"]) ]
  | @tsv')

[ -n "$sinks" ] || exit 0

choice=$(printf '%s\n' "$sinks" | cut -f3- \
  | wofi --show dmenu --define layer=overlay --insensitive) || exit 0
[ -n "$choice" ] || exit 0

line=$(printf '%s\n' "$sinks" | awk -F'\t' -v d="$choice" '$3 == d { print; exit }')
[ -n "$line" ] || exit 1

wpctl set-default "$(printf '%s' "$line" | cut -f1)"

# WirePlumber enruta cada stream al sink cuyo object.serial figure en la
# metadata target.object; es lo que hace 'pactl move-sink-input' por dentro.
serial=$(printf '%s' "$line" | cut -f2)
pw-dump \
  | jq -r '.[] | select(.info.props["media.class"] == "Stream/Output/Audio") | .id' \
  | while read -r id; do
      pw-metadata "$id" target.object "$serial" Spa:Id >/dev/null
    done
