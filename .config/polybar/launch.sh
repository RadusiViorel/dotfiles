#!/usr/bin/env bash
# Launch polybar, killing any existing instance first

# Export barlib environment so all component scripts resolve correctly
source "${HOME}/.config/polybar/scripts/env"

# Terminate existing polybar instances
pkill -x polybar 2>/dev/null
while pgrep -x polybar >/dev/null; do sleep 0.1; done

# Ensure cache dir exists
mkdir -p "${HOME}/.config/polybar/.cache"

# Start polybar
polybar main --config="${HOME}/.config/polybar/config.ini" 2>/dev/null &

echo "Polybar launched."
