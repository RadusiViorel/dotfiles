#!/usr/bin/env bash

if ! command -v brightnessctl >/dev/null 2>&1; then
  echo "Warning: brightnessctl is not installed or not in PATH."
  exit 1
fi

hour=$(date +%H)
if [ "$hour" -ge $1 ] && [ "$hour" -lt $2 ]; then
    brightnessctl set 100% >/dev/null 2>&1
else
    brightnessctl set 5% >/dev/null 2>&1
fi
