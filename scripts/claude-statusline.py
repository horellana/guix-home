#!/usr/bin/env python3
"""Statusline de Claude Code: la de Governor + cuánto falta para el reset de
los límites de 5 h y 7 días.

Uso (statusLine.command en ~/.claude/settings.json, lo pone governor-setup.py):
    claude-statusline.py [<ruta-a-governor-statusline>]

Lee el JSON que Claude Code pasa por stdin; los resets vienen en
rate_limits.{five_hour,seven_day}.resets_at (epoch en segundos, o ISO 8601).
Si Governor falla o no se pasa, se muestran solo los límites.
"""

import json
import re
import subprocess
import sys
import time
from datetime import datetime

WINDOWS = (("five_hour", "5h"), ("seven_day", "7d"))


def epoch(value):
    if isinstance(value, (int, float)):
        return value / 1000 if value > 1e12 else float(value)
    if isinstance(value, str):
        try:
            return datetime.fromisoformat(value.replace("Z", "+00:00")).timestamp()
        except ValueError:
            return None
    return None


def countdown(seconds):
    seconds = max(0, int(seconds))
    days, rest = divmod(seconds, 86400)
    hours, rest = divmod(rest, 3600)
    minutes = rest // 60
    if days:
        return f"{days}d{hours}h"
    if hours:
        return f"{hours}h{minutes:02d}m"
    return f"{minutes}m"


def main():
    raw = sys.stdin.read()
    try:
        data = json.loads(raw)
    except ValueError:
        data = {}
    limits = data.get("rate_limits") if isinstance(data, dict) else None
    limits = limits if isinstance(limits, dict) else {}

    line = ""
    if len(sys.argv) > 1:
        try:
            line = subprocess.run(
                [sys.argv[1]], input=raw, capture_output=True, text=True, timeout=5
            ).stdout.strip()
        except (OSError, subprocess.SubprocessError):
            line = ""

    now = time.time()
    for key, label in WINDOWS:
        window = limits.get(key)
        if not isinstance(window, dict):
            continue
        reset = epoch(window.get("resets_at"))
        if reset is None:
            continue
        suffix = f"↻{countdown(reset - now)}"
        # Governor ya pinta "5h 42%": le pego el reset al lado.
        pattern = rf"(^|\| ){label} [^|]*?%"
        if re.search(pattern, line):
            line = re.sub(pattern, lambda m: f"{m.group(0)} {suffix}", line, count=1)
        else:
            pct = window.get("used_percentage")
            piece = f"{label} {pct:g}% {suffix}" if isinstance(pct, (int, float)) else f"{label} {suffix}"
            line = f"{line} | {piece}" if line else piece
    print(line)


if __name__ == "__main__":
    main()
