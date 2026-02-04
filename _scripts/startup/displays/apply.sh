#!/usr/bin/env bash
xrandr \
  --output HDMI-1 --mode 2560x1440 --primary --pos 0x0 \
  --output DVI-D-1 --mode 2560x1440 --right-of HDMI-1 \
