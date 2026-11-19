#!/usr/bin/env python3
"""Registra un marketplace de plugins del store en ~/.claude/settings.json.

Lo invocan servicios de activación de home-config.scm en cada
`guix home reconfigure`.  Idempotente: solo toca la entrada del marketplace en
extraKnownMarketplaces y los plugins indicados en enabledPlugins; respeta el
resto de la configuración (y un plugin que el usuario haya puesto a false).

Uso: claude-marketplace-setup.py <nombre> <ruta-en-el-store> <plugin>...
"""

import json
import os
import sys
from pathlib import Path

name, path, *plugins = sys.argv[1:]

settings_path = Path(os.environ["HOME"]) / ".claude" / "settings.json"
settings_path.parent.mkdir(parents=True, exist_ok=True)

try:
    settings = json.loads(settings_path.read_text(encoding="utf-8"))
    if not isinstance(settings, dict):
        settings = {}
except Exception:
    settings = {}

markets = settings.get("extraKnownMarketplaces")
if not isinstance(markets, dict):
    markets = {}
markets[name] = {"source": {"source": "directory", "path": path}}
settings["extraKnownMarketplaces"] = markets

enabled = settings.get("enabledPlugins")
if not isinstance(enabled, dict):
    enabled = {}
for plugin in plugins:
    enabled.setdefault(f"{plugin}@{name}", True)
settings["enabledPlugins"] = enabled

settings_path.write_text(json.dumps(settings, indent=2) + "\n", encoding="utf-8")
print(f"{name}: registrado en {settings_path}; marketplace -> {path}")
