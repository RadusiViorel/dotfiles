#!/usr/bin/env bash
# Example usage patterns for terminal_launcher

source "${HOME}/.config/xmobar/scripts/terminal_launcher"

# Example 1: Open terminal with specific class (auto-detects from $TERM)
# launch_terminal "floating_calc"

# Example 2: Open terminal with class and run a command
# launch_terminal "htop_monitor" "htop"

# Example 3: Open terminal with class and run a complex command
# launch_terminal "system_logs" "journalctl -f"

# Example 4: Explicitly specify which terminal to use
# launch_terminal_explicit "alacritty" "my_class" "nvim"

# Example 5: Open different terminals with the same class
# TERM="kitty" launch_terminal "dev_terminal" "zsh"
# TERM="alacritty" launch_terminal "dev_terminal" "zsh"

# Example 6: In an xmobar action script
example_action() {
  source "${HOME}/.config/xmobar/scripts/terminal_launcher"
  launch_terminal "floating_terminal" "bash"
}

echo "This file contains example usage patterns."
echo "Source it in your action scripts like:"
echo ""
echo '  source "${HOME}/.config/xmobar/scripts/terminal_launcher"'
echo '  launch_terminal "my_class_name" "optional_command"'
