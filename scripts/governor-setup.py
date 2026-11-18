#!/usr/bin/env python3
"""Registra (o actualiza) el plugin Governor en ~/.claude/settings.json.

Lo invoca el servicio de activación `governor-claude-plugin` de home-config.scm
en cada `guix home reconfigure`.  Idempotente: solo toca las claves de Governor
y respeta el resto de la configuración del usuario (incl. un statusLine propio).

Uso: governor-setup.py <ruta-del-marketplace-en-el-store> [<comando-statusline>]

Sin <comando-statusline> se usa la statusline de Governor tal cual.
"""

import json
import os
import sys
from pathlib import Path

market = sys.argv[1]
statusline_cmd = sys.argv[2] if len(sys.argv) > 2 else None

home = Path(os.environ["HOME"])
claude_dir = home / ".claude"
claude_dir.mkdir(parents=True, exist_ok=True)
settings_path = claude_dir / "settings.json"
data_dir = claude_dir / "plugins" / "governor"
data_dir.mkdir(parents=True, exist_ok=True)
overrides_path = data_dir / "overrides.json"


def load_dict(path):
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
        return value if isinstance(value, dict) else {}
    except Exception:
        return {}


settings = load_dict(settings_path)

enabled = settings.get("enabledPlugins")
if not isinstance(enabled, dict):
    enabled = {}
enabled["governor@governor"] = True
settings["enabledPlugins"] = enabled

markets = settings.get("extraKnownMarketplaces")
if not isinstance(markets, dict):
    markets = {}
markets["governor"] = {"source": {"source": "directory", "path": market}}
settings["extraKnownMarketplaces"] = markets

# Statusline: la pongo si no hay ninguna, y refresco las rutas del store si la
# que hay ya es la nuestra (Governor sola o envuelta por claude-statusline.py).
statusline = statusline_cmd or f'"{market}/bin/governor-statusline"'
current = settings.get("statusLine")
current_cmd = str(current.get("command", "")) if isinstance(current, dict) else ""
if not isinstance(current, dict) or any(
    mark in current_cmd for mark in ("governor-statusline", "claude-statusline")
):
    settings["statusLine"] = {"type": "command", "command": statusline}

settings_path.write_text(json.dumps(settings, indent=2) + "\n", encoding="utf-8")

# Modo por defecto; no piso un modo ya elegido por el usuario.
overrides = load_dict(overrides_path)
if not overrides.get("mode"):
    overrides["mode"] = "compact"
    overrides_path.write_text(
        json.dumps(overrides, indent=2) + "\n", encoding="utf-8"
    )

print(f"governor: registrado en {settings_path}; marketplace -> {market}")
