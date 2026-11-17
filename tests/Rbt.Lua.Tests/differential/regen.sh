#!/usr/bin/env bash
# Regenerate the expected outputs from the reference C Lua 5.4 interpreter.
# Usage: LUA=/path/to/lua5.4 ./regen.sh   (defaults to `lua5.4` on PATH)
set -euo pipefail
cd "$(dirname "$0")"
LUA="${LUA:-lua5.4}"
"$LUA" -v
for f in *.lua; do
  "$LUA" "$f" > "${f%.lua}.expected"
done
