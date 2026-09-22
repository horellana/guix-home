#!/usr/bin/env bash
# Actualiza el paquete claude-code a la última versión publicada en npm:
# consulta el registry, descarga el tarball para obtener el hash base32 y
# reescribe (version ...) y (base32 ...) en claude-code.scm.  Termina validando
# con `guix home build`; la activación (reconfigure) y el commit quedan a mano.
set -euo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
src="$repo/guix-packages/my-packages/claude-code.scm"
npm_pkg=@anthropic-ai/claude-code-linux-x64

current=$(sed -n 's/.*(version "\([^"]*\)").*/\1/p' "$src")
latest=$(curl -fsSL "https://registry.npmjs.org/$npm_pkg/latest" |
           sed -n 's/.*"version":"\([^"]*\)".*/\1/p')

if [ -z "$latest" ]; then
  echo "error: no se pudo leer la versión desde npm" >&2
  exit 1
fi

echo "actual: $current   npm: $latest"
if [ "$current" = "$latest" ]; then
  echo "ya está en la última versión."
  exit 0
fi

url="https://registry.npmjs.org/$npm_pkg/-/claude-code-linux-x64-$latest.tgz"
hash=$(guix download "$url" 2>/dev/null | tail -n1)

if [ -z "$hash" ]; then
  echo "error: guix download no devolvió hash para $url" >&2
  exit 1
fi

sed -i \
  -e "s/(version \"[^\"]*\")/(version \"$latest\")/" \
  -e "s/(base32 \"[^\"]*\")/(base32 \"$hash\")/" \
  "$src"

echo "actualizado a $latest ($hash)"
guix home build "$repo/home-config.scm" >/dev/null
echo "build ok — aplica con: guix home reconfigure $repo/home-config.scm"
