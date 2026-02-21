# Terminal Launcher Integration

## Overview

The `terminal_launcher` script provides a universal way to launch terminals from xmobar component actions, with support for multiple terminal emulators and a modern flag-based API.

## Location

`~/.config/xmobar/scripts/terminal_launcher`

## API

### Modern API: `terminal_launch`

```bash
source "${HOME}/.config/xmobar/scripts/terminal_launcher"

terminal_launch \
  --title "Window Title" \
  --command "command to run" \
  --hold \
  --class "xmobar_terminal"
```

**Flags:**
- `--title`: Set window title (optional)
- `--command`: Command to execute (optional, omit for plain terminal)
- `--hold`: Keep terminal open after command finishes, wait for ENTER (optional)
- `--class`: WM_CLASS for window manager rules (optional, defaults to `xmobar_terminal`)

### Legacy API: `launch_terminal`

```bash
launch_terminal "class_name" "command"
```

Provided for backward compatibility.

## Supported Terminals

Auto-detects from `$TERM` environment variable:
- `st` (default fallback)
- `alacritty`
- `kitty`
- `ghostty`
- `xterm`

## Example: Automount Component

**File:** `.config/xmobar/com/automount/actions/show`

```bash
#!/usr/bin/env bash

AUTOMOUNT_SCRIPT="${HOME}/.config/_scripts/startup/automount/automount.sh"

source "${HOME}/.config/xmobar/scripts/terminal_launcher"

terminal_launch \
  --title "Automount Devices" \
  --command "${AUTOMOUNT_SCRIPT} list" \
  --hold
```

**Wiring:**
- Added `show` case to `index` file
- Added `"2:show"` to `render` file (middle-click)

**Result:** Middle-clicking the automount icon shows device status in a terminal that stays open.

## Features

### Hold Mode

The `--hold` flag wraps the command in a bash script that:
1. Executes the command
2. Prints a blank line
3. Shows "Press ENTER to close..."
4. Waits for user input

Perfect for showing command output that would otherwise disappear instantly.

### Terminal Detection

Uses `$TERM` variable to detect the current terminal emulator. Falls back to `st` if unknown.

### Title Support

Different terminals use different flags for titles:
- st: `-t`
- alacritty: `--title`
- kitty: `--title`
- xterm: `-T`

The launcher handles these differences automatically.

## Use Cases

- Show command output (logs, status, lists)
- Interactive prompts (confirmation dialogs)
- Quick debugging (run test commands)
- Documentation viewers
- System status displays

## Future Enhancements

Potential additions:
- `--geometry` flag for window size
- `--working-dir` flag for initial directory
- `--font` flag for terminal font override
- Custom terminal selection via config file
