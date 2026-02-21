# Agent Guidelines for Dotfiles Repository

## Overview

Personal dotfiles for **Void Linux** with X11/XMonad desktop environment. The repository contains modular configurations for window management, terminal environment, shell scripting, and system utilities.

**Primary Languages:** Bash (62+ files), Haskell (3 files), C (5 files)
**Target Platform:** Void Linux with X11
**Architecture:** Modular, event-driven component system

---

## Repository Structure

```
.
├── .config/
│   ├── xmonad/          # Haskell window manager config
│   ├── xmobar/          # Status bar with 23+ Bash components
│   ├── st-conf/         # Suckless terminal (C)
│   ├── tmux/            # Terminal multiplexer
│   ├── zsh/             # Shell configuration
│   ├── rofi/            # Application launcher
│   ├── dunst/           # Notification daemon
│   ├── picom/           # Compositor
│   └── _scripts/        # Startup and automation scripts
├── .zshrc, .zprofile    # Shell initialization
├── .xinitrc, .Xresources # X11 configuration
└── .gitconfig           # Git settings
```

---

## Build/Compilation Commands

### XMonad & Xmobar (Haskell)

```bash
# Build all Haskell executables using Stack
cd ~/.config/xmonad
stack build

# Or compile manually with GHC
ghc xmonad.hs      # Produces xmonad binary
ghc --make xmobar.hs  # Rebuild xmobar if modified

# Reload XMonad (preserves session)
xmonad --restart

# Full restart
killall xmonad
startx
```

### ST Terminal (C)

```bash
cd ~/.config/st-conf
make                # Build st binary
make clean          # Clean build artifacts
make install        # Install to system (requires privileges)
```

### Xmobar Components (Bash)

```bash
# Test single component output
~/.config/xmobar/com/<component>/render

# Test component action
~/.config/xmobar/com/<component>/index <action_name>

# Examples:
~/.config/xmobar/com/battery/render
~/.config/xmobar/com/updates/index show

# Reload xmobar after changes
killall xmobar && xmonad --restart
```

---

## Testing Commands

### Shell Script Validation

```bash
# Check bash syntax without executing
bash -n path/to/script.sh

# Validate all xmobar components
find ~/.config/xmobar/com -type f \( -name "index" -o -name "render" \) | \
  xargs -I{} bash -n {}

# Test with shellcheck (if installed)
shellcheck path/to/script.sh
```

### Haskell Type Checking

```bash
cd ~/.config/xmonad
stack build --fast --test --no-run-tests  # Type check only
ghc -fno-code xmonad.hs  # Type check without linking
```

### Single Test Execution

```bash
# No central test suite - test components individually
# For xmobar components:
./com/<component>/render  # Should output formatted text

# For scripts:
bash -x path/to/script.sh  # Run with trace enabled
```

---

## Code Style Guidelines

### Bash Scripts

#### File Header
```bash
#!/usr/bin/env bash
```

**Never use** `#!/bin/bash` - always use portable shebang.

#### Naming Conventions

**Variables:**
- `snake_case` for all variables
- `__double_underscore` prefix for state keys
- `UPPERCASE` for constants, paths, and exported variables

**Files:**
- `snake_case.sh` for standalone scripts
- `index` for component entry points (no extension)
- `render` for component output wrappers (no extension)

**Examples:**
```bash
COM="${HOME}/.config/xmobar/com/battery"
STATE_FILE="${HOME}/.config/xmobar/scripts/state"
__timestamp="battery_timestamp"
CACHE_TTL=80
now=$(date +%s)
```

#### Conditionals

```bash
# Arithmetic - use (( ))
(( value < 10 ))
(( count >= threshold ))

# String comparison - use [[ ]]
[[ "$state" == "Active" ]]
[[ -n "$variable" ]]

# Numeric in [[ ]] - use -eq, -gt, -lt
[[ $num -eq 0 ]]
[[ $value -gt 100 ]]

# Early returns
(( count == 0 )) && exit 0
[[ -z "$data" ]] && exit 0
```

#### Arrays

```bash
# Declaration
thresholds=(10 33 66)
icons=("󰂎" "󱊡" "󱊢" "󱊣")

# Last element as default
icon="${icons[-1]}"

# Iteration with index
for i in "${!thresholds[@]}"; do
  if (( value < thresholds[i] )); then
    icon="${icons[i]}"
    break
  fi
done
```

#### Error Handling

**Always check dependencies:**
```bash
if ! command -v acpi >/dev/null 2>&1; then
  echo "Warning: acpi is not installed or not in PATH."
  exit 1
fi
```

**Graceful degradation:**
```bash
if ! pgrep -x wireplumber >/dev/null; then
  echo "Service Down"
  exit 0
fi
```

**Validate data before use:**
```bash
echo "$output" | grep -q "Battery" || exit 0
```

#### Sourcing Files

```bash
source "${HOME}/.config/xmobar/scripts/vars"   # Colors
source "${HOME}/.config/xmobar/scripts/state"  # State variables
```

**Always** use `"${HOME}"` instead of `~` for portability.

#### Quoting

- **Always quote** variable expansions: `"$var"`
- Use `${var}` for clarity in complex strings
- Array elements: `"${array[@]}"` for all, `"${array[0]}"` for single

### Haskell Style

#### Imports

```haskell
import XMonad
import Control.Monad (when)

import qualified XMonad.StackSet as W
import qualified Data.Map as M
```

**Order:** Standard libs → XMonad → Qualified imports

#### Naming

- `camelCase` for functions and variables
- `PascalCase` for types and constructors
- Descriptive names: `getEnvDefault`, `readSlackNotificationsFromFile`

#### Type Signatures

**Always** include explicit type signatures:
```haskell
getEnvDefault :: String -> String -> String
color_ok :: String
```

#### Formatting

- **Indentation:** 2 spaces
- **Line length:** Keep under 80 characters when practical
- **Alignment:** Align multi-line patterns and assignments

### C Style (ST Terminal)

- **Indentation:** Tabs (following suckless conventions)
- **Braces:** K&R style (opening on same line)
- **Comments:** `/* block */` for multi-line, `//` for single
- Follow existing st.c patterns for consistency

---

## State Management (Xmobar)

### Reading State
```bash
source "${HOME}/.config/xmobar/scripts/state"
echo "$battery_timestamp"  # Auto-loaded from state file
```

### Initializing State
```bash
source "${HOME}/.config/xmobar/scripts/state_loader"
state_init "battery_level_icon" 0
state_init "battery_timestamp" $((now - 81))
```

### Updating State
```bash
state_update "battery_info" "\"$battery_info\""
state_update "battery_timestamp" "$now"
```

**State Keys:** Use component prefix: `battery_timestamp`, `network_enabled`, `vpn_loading`

**Warning:** State file is shared - avoid race conditions. No file locking implemented.

---

## Common Patterns

### TTL Caching
```bash
now=$(date +%s)
CACHE_TTL=3600

if (( now - last_timestamp >= CACHE_TTL )); then
  data=$(expensive_command)
  state_update "timestamp" "$now"
  state_update "cached_data" "\"$data\""
fi
```

### Threshold-Based Display
```bash
thresholds=(10 33 66)
icons=("󰂎" "󱊡" "󱊢" "󱊣")
icon="${icons[-1]}"

for i in "${!thresholds[@]}"; do
  if (( value < thresholds[i] )); then
    icon="${icons[i]}"
    break
  fi
done
```

### Toggle Actions
```bash
new_value=$(( ! current_value ))        # Boolean toggle
new_value=$(( (current + 1) % max ))    # Cycle through values
```

---

## File Permissions

All scripts **must** be executable:
```bash
chmod +x script.sh
chmod +x com/<component>/index
chmod +x com/<component>/render
```

---

## Documentation Guidelines

### Where to Put Documentation

**NEVER** create documentation files (`.md`, README, etc.) in the main codebase unless explicitly requested.

**ALWAYS** put documentation in `__ai/{subject_path}`:

```bash
# Examples:
__ai/.config/xmobar/TERMINAL_LAUNCHER.md
__ai/.config/xmonad/KEYBINDINGS.md
__ai/AGENTS.md  # General guidelines
```

The `__ai/` directory mirrors the repository structure and is excluded from stow operations via `.stow-local-ignore`.

### When to Document

- Implementation notes for complex features
- Architecture decisions and patterns
- Bug fixes and their explanations
- Performance optimizations
- Agent-specific guidelines for components

### Documentation Style

- Use clear, concise markdown
- Include code examples where helpful
- Reference specific file paths and line numbers when relevant
- Keep implementation details separate from user-facing docs

---

## Important Notes

### Xmobar Components
- See `.config/xmobar/AGENTS.md` for detailed component guidelines
- 23+ modular components with mouse click actions
- Shared state file with no locking mechanism
- Color palette via environment: `XMB_COLOR_OK`, `XMB_COLOR_WARNING`, etc.

### Dependencies
- **Haskell:** Stack (LTS 24.24), GHC, xmonad, xmonad-contrib, xmobar
- **Shell:** bash, zsh
- **System:** acpi, brightnessctl, bluetoothctl, wpctl, sensors
- **Utilities:** rofi, dunst, picom, feh, scrot, tmux

### Platform-Specific
- **Package Manager:** xbps (Void Linux)
- **Init System:** runit
- **Privilege Escalation:** doas (not sudo)

### Best Practices
1. **Always validate dependencies** before use
2. **Exit early** if no output needed
3. **Use caching** for expensive operations
4. **Quote all variables** to prevent word splitting
5. **Check command success** before using output
6. **Preserve existing patterns** when modifying code
7. **Test components individually** before system integration

---

## Adding New Components

### Xmobar Component
1. `mkdir -p ~/.config/xmobar/com/<component>/actions`
2. Create `index` file with logic
3. Create `render` file for output
4. Add to `~/.config/xmobar/scripts/bar`
5. `chmod +x` all scripts
6. Test with `./com/<component>/render`
7. Reload: `killall xmobar && xmonad --restart`

### Startup Script
1. Create in `~/.config/_scripts/startup/`
2. Use `#!/usr/bin/env bash` shebang
3. Make executable: `chmod +x`
4. Add to `.xinitrc` or startup sequence if needed

---

## Color Palette

Environment variables (override in shell profile):
```bash
export XMB_COLOR_OK="#84f098"       # Success/normal
export XMB_COLOR_WARNING="#e6c15c"  # Warning state
export XMB_COLOR_DANGER="#f58989"   # Error state
export XMB_COLOR_CRITIC="#d94141"   # Critical state
export XMB_COLOR_INFO="#0000FF"     # Informational
export XMB_COLOR_MUTE="#666666"     # Inactive/muted
export XMB_COLOR_WHITE="#FFFFFF"    # Default text
```

---

## Quick Reference

**Reload Config:**
```bash
xmonad --restart               # Reload XMonad
killall xmobar && xmonad --restart  # Reload xmobar
source ~/.zshrc                # Reload zsh
tmux source ~/.config/tmux/tmux.conf  # Reload tmux
```

**Debug:**
```bash
bash -x script.sh              # Trace execution
stack build --trace            # Verbose Haskell build
xmonad --recompile --verbose   # Debug XMonad compilation
```

**Common Locations:**
- Logs: `~/.xsession-errors`
- State: `~/.config/xmobar/scripts/state`
- XMonad binary: `~/.local/bin/xmonad`
