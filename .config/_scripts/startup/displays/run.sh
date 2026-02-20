#!/usr/bin/env bash

DIR="$HOME/.config/_scripts/startup/displays"


CURRENT=$(xrandr --listmonitors | tail -n +2 | sed -E 's/^[[:space:]]*[0-9]+: //; s/\*//g; s/[+-][0-9]+\+[0-9]+//g' | sort | sha256sum | cut -d' ' -f1)
SAVED=$(cat "$DIR/checksum" 2>/dev/null)

if [ "$CURRENT" != "$SAVED" ] || [ ! -f "$DIR/apply.sh" ]; then
    "$DIR/generate.sh"
else
    "$DIR/apply.sh"
fi
