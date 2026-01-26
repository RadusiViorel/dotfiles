#!/usr/bin/env bash

DIR="$HOME/.config/_scripts/startup/displays"

CURRENT=$(xrandr | sha256sum | cut -d' ' -f1)
SAVED=$(cat "$DIR/checksum" 2>/dev/null)

if [ "$CURRENT" != "$SAVED" ] || [ ! -x "$DIR/apply.sh" ]; then
    "$DIR/generate.sh"
else
    "$DIR/apply.sh"
fi
