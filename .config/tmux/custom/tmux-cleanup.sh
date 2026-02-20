#!/bin/bash

# This script is called by tmux when a client detaches
# It kills auto-created sessions (8-char hex names) if they have no attached clients

session_name="$1"

# Only kill if session name is an 8-character hex string (auto-created)
if [[ "$session_name" =~ ^[0-9a-f]{8}$ ]]; then
    # Check if session has 0 attached clients
    attached=$(tmux list-sessions -F "#{session_name}:#{session_attached}" 2>/dev/null | grep "^$session_name:" | cut -d: -f2)
    if [ "$attached" = "0" ]; then
        tmux kill-session -t "$session_name" 2>/dev/null
    fi
fi
