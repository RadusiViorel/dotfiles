# Updates Component - Terminal Launcher Integration

## Overview

The updates component now uses `terminal_launcher` for consistent terminal handling across different terminal emulators.

## Component: `.config/xmobar/com/updates`

### Actions

#### 1. `show` - Run System Updates
**File:** `actions/show`

**Purpose:** Shows pending packages and runs the update process

**Terminal Settings:**
- **Class:** `float-medium`
- **Title:** "System Updates"

**Behavior:**
1. Generates fresh package list with `xbps-install -nuM`
2. Displays packages to be updated
3. Runs `sudo xbps-install -Su` interactively
4. Logs the update session
5. Resets update counter in state file

**Click Binding:** Left-click (button 1)

#### 2. `view_log` - View Update History
**File:** `actions/view_log`

**Purpose:** Shows update history and recent session logs

**Terminal Settings:**
- **Class:** `float-large`
- **Title:** "Update History"

**Behavior:**
1. Displays master log (`logs/updates.log`)
2. Lists recent session logs
3. Waits for user to press ENTER

**Click Binding:** Middle-click (button 2)

## Mouse Bindings

- **Left-click:** Run system update (`show`)
- **Middle-click:** View update history (`view_log`)

## Window Classes

Both actions use floating window classes:
- `float-medium` - For the update process (medium-sized floating window)
- `float-large` - For the log viewer (large floating window)

These should be configured in XMonad's `manageHook` to float appropriately.

## State Management

The `show` action updates the state file after a successful update:
```bash
sed -i "s/^$COUNT_KEY=.*/$COUNT_KEY=0/" "$STATE_FILE"
```

This resets the update counter so the icon disappears after updates are installed.

## Logging

Update sessions are logged via:
```bash
LOG_SCRIPT="${HOME}/.config/xmobar/com/updates/scripts/log_update"
```

Logs are stored in:
- `~/.config/xmobar/com/updates/logs/updates.log` (master log)
- `~/.config/xmobar/com/updates/logs/session_*.log` (individual sessions)

## Migration Notes

**Before:**
```bash
$TERM --class float-medium -e bash -c "..."
```

**After:**
```bash
source "${HOME}/.config/xmobar/scripts/terminal_launcher"

terminal_launch \
  --title "System Updates" \
  --class "float-medium" \
  --command "..."
```

## Benefits

1. **Terminal agnostic:** Works with st, alacritty, kitty, ghostty, xterm
2. **Consistent API:** Same interface across all components
3. **Better title support:** Each window has a descriptive title
4. **Cleaner code:** Less terminal-specific logic in actions
