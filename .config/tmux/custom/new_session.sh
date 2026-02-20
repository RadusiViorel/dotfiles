#!/bin/bash

NAME="$1"
if [ -z "$NAME" ]; then
  NAME=$(head -c 100 /dev/urandom | tr -dc A-Za-z0-9 | head -c 8)
fi

# Check if session exists
tmux has-session -t "$NAME" 2>/dev/null

if [ $? -ne 0 ]; then
  tmux new-session -ds "$NAME"
fi

tmux switch-client -t "$NAME"
