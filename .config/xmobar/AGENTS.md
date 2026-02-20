# Agent Guidelines for Xmobar Configuration

## Overview

This is a modular xmobar status bar configuration written in **Bash** (99%) and **Haskell** (1%). It consists of 21+ self-contained components that display system information and provide interactive controls via mouse clicks.

**Target Platform:** Void Linux with X11/XMonad
**Architecture:** Event-driven, state-based component system

---

## Build/Compilation Commands

### Build Xmobar Binary
```bash
# Compile the Haskell configuration
cd ~/.config/xmobar
ghc xmobar.hs
# Output: xmobar binary (44MB), xmobar.hi, xmobar.o

# Or rebuild if xmobar.hs changes
ghc --make xmobar.hs
```

### Reload Xmobar
```bash
# After making changes to any component
killall xmobar
xmonad --restart
```

### Test Single Component
```bash
# Test component output directly
~/.config/xmobar/com/<component>/render

# Test component action
~/.config/xmobar/com/<component>/index <action_name>

# Examples:
~/.config/xmobar/com/battery/render
~/.config/xmobar/com/updates/index show
~/.config/xmobar/com/volume/index volume_up
```

### Validate Script Syntax
```bash
# Check bash syntax without executing
bash -n ~/.config/xmobar/com/<component>/index

# Check all scripts
find ~/.config/xmobar/com -type f -name "index" -o -name "render" | \
  xargs -I{} bash -n {}
```

### Test State Management
```bash
# View current state
cat ~/.config/xmobar/scripts/state

# Reset state (use with caution)
rm ~/.config/xmobar/scripts/state

# Monitor state changes
watch -n 1 cat ~/.config/xmobar/scripts/state
```

---

## Code Style Guidelines

### File Structure

Every component follows this pattern:
```
com/<component>/
├── index          # Main logic, state management, action routing
├── render         # Wraps index output with mouse actions
├── actions/       # Action handler scripts (optional)
│   └── <action>
└── scripts/       # Helper scripts (optional)
    └── <helper>
```

### Naming Conventions

**Files:**
- `index` - main component entry point
- `render` - output wrapper
- `action_name` - snake_case for actions
- `helper_script` - snake_case for helpers

**Variables:**
- `snake_case` for all variables
- `__double_underscore` prefix for state keys
- `UPPERCASE` for constants and paths
- Semantic color names: `COLOR_OK`, `COLOR_WARNING`, `COLOR_DANGER`, `COLOR_CRITIC`

**Examples:**
```bash
COM="${HOME}/.config/xmobar/com/battery"
STATE_FILE="${HOME}/.config/xmobar/scripts/state"
__timestamp="battery_timestamp"
CACHE_TTL=80
```

### Bash Script Template

**index file:**
```bash
#!/usr/bin/env bash

source "${HOME}/.config/xmobar/scripts/vars"
source "${HOME}/.config/xmobar/scripts/state"
source "${HOME}/.config/xmobar/scripts/page/index" [1|2]  # Optional: page filter

COM="${HOME}/.config/xmobar/com/<component>"
STATE_FILE="${HOME}/.config/xmobar/scripts/state"

# State initialization
__variable="state_key"
grep -q "^$__variable=" "$STATE_FILE" || ( echo "$__variable=default" >> "$STATE_FILE" && state_key=default )

# Dependency checks (REQUIRED for external commands)
if ! command -v tool >/dev/null 2>&1; then
  echo "Warning: tool is not installed or not in PATH."
  exit 1
fi

# Action routing
case "$1" in
  action_name) ${COM}/actions/action_name \
    --param=${value} ;;
  *) ;;
esac

# Display logic
echo "<fc=${COLOR_NAME}>󰀀 text</fc>"
```

**render file:**
```bash
#!/usr/bin/env bash

source "${HOME}/.config/xmobar/scripts/render"
COM="${HOME}/.config/xmobar/com/<component>/index"

xmobar_actions "$COM" \
  "1:primary_action" \
  "3:secondary_action"
```

**action file:**
```bash
#!/usr/bin/env bash

# Parse named parameters
for arg in "$@"; do
  case $arg in
    --param=*) param="${arg#*=}" ;;
    *) echo "Unknown argument: $arg" ; exit 1 ;;
  esac
done

# Perform action
# Update state if needed
sed -i "s/^$KEY=.*/$KEY=$new_value/" "$STATE_FILE"
```

### Formatting Standards

**Shebang:** Always use `#!/usr/bin/env bash`

**Sourcing:**
```bash
source "${HOME}/.config/xmobar/scripts/vars"      # Colors
source "${HOME}/.config/xmobar/scripts/state"     # State variables
```

**Conditionals:**
- Use `(( ))` for arithmetic: `(( value < 10 ))`
- Use `[[ ]]` for strings: `[[ "$state" == "Active" ]]`
- Use `-eq`, `-gt` for numeric comparisons in `[[ ]]`

**Early Returns:**
```bash
(( count == 0 )) && exit 0
[[ -z "$data" ]] && exit 0
```

**Arrays:**
```bash
thresholds=(10 33 66)
icons=("󰂎" "󱊡" "󱊢" "󱊣")
icon="${icons[-1]}"  # Last element as default
```

### State Management

**Reading State:**
State variables are auto-loaded by sourcing `scripts/state`. Access directly:
```bash
echo "$battery_timestamp"
```

**Initializing State:**
```bash
__var="state_key"
grep -q "^$__var=" "$STATE_FILE" || ( echo "$__var=default" >> "$STATE_FILE" && state_key=default )
```

**Updating State:**
```bash
sed -i "s/^$KEY=.*/$KEY=$new_value/" "$STATE_FILE"
```

**State Keys:** Use component prefix: `battery_timestamp`, `network_enabled`, `vpn_loading`

### Error Handling

**ALWAYS check dependencies:**
```bash
if ! command -v acpi >/dev/null 2>&1; then
  echo "Warning: acpi is not installed or not in PATH."
  exit 1
fi
```

**Graceful degradation:**
```bash
if ! pgrep -x wireplumber >/dev/null; then
  echo "<fc=${COLOR_DANGER}>󰖁 Service Down</fc>"
  exit 0
fi
```

**Validate data before use:**
```bash
echo "$output" | grep -q "Battery" || exit 0
```

### Output Formatting

**Color Coding:**
```bash
echo "<fc=${COLOR_OK}>✓ Success</fc>"
echo "<fc=${COLOR_WARNING}>⚠ Warning</fc>"
echo "<fc=${COLOR_DANGER}>⚠ Error</fc>"
echo "<fc=${COLOR_CRITIC}>✗ Critical</fc>"
```

**Icons:** Use Nerd Font icons (already configured in xmobar.hs)

**Visibility:** Exit early if nothing to show:
```bash
(( updates_count == 0 )) && exit 0
```

### Common Patterns

**Threshold-Based Icons:**
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

**TTL Caching:**
```bash
now=$(date +%s)
CACHE_TTL=3600

if (( now - last_timestamp >= CACHE_TTL )); then
  data=$(expensive_command)
  sed -i "s/^$__timestamp=.*/$__timestamp=$now/" "$STATE_FILE"
  sed -i "s/^$__data=.*/$__data=\"$data\"/" "$STATE_FILE"
fi
```

**Loading Spinners:**
```bash
if (( loading == 1 )); then
  spinner_icons=(⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏)
  idx=$(( ( spinner + 1 ) % ${#spinner_icons[@]} ))
  state_update "spinner" "${idx}"
  echo "${spinner_icons[$idx]} Loading..."
  exit 0
fi
```

**Toggle Actions:**
```bash
new_value=$(( ! current_value ))  # Boolean toggle
new_value=$(( (current + 1) % max ))  # Cycle through values
```

---

## File Permissions

ALL scripts must be executable:
```bash
chmod +x com/<component>/index
chmod +x com/<component>/render
chmod +x com/<component>/actions/*
```

---

## Adding a New Component

1. Create directory: `mkdir -p com/<component>/actions`
2. Create `index` file with standard template
3. Create `render` file
4. Add to `scripts/bar` component list
5. Make executable: `chmod +x com/<component>/{index,render}`
6. Test: `./com/<component>/render`
7. Reload xmobar

---

## Mouse Button Mapping

- **Button 1:** Left click (primary action)
- **Button 2:** Middle click (secondary action)
- **Button 3:** Right click (tertiary action)
- **Button 4:** Scroll up
- **Button 5:** Scroll down

---

## Color Palette

Defined in `scripts/vars`, override via environment:
```bash
export XMB_COLOR_OK="#84f098"
export XMB_COLOR_WARNING="#e6c15c"
export XMB_COLOR_DANGER="#f58989"
export XMB_COLOR_CRITIC="#d94141"
```

---

## Important Notes

- **State File:** Shared across all components - avoid race conditions
- **No Locking:** State updates use `sed -i` without locks (potential issue in high-frequency components)
- **Page System:** Components can filter by page (1 or 2) via `scripts/page/index`
- **Terminal:** `$TERM` variable used for popups (set in environment)
- **Performance:** Use caching for expensive operations (network, disk I/O)
- **Icons:** Ensure Nerd Font is installed (JetBrainsMono Nerd Font)

---

## Dependencies by Component

See full dependency list in component `index` files. Common tools:
- `acpi`, `sensors`, `wpctl`, `brightnessctl`, `bluetoothctl`
- `wpa_cli`, `xbps-install`, `rofi`, `scrot`, `feh`
- `yt-dlp`, `speedtest-cli`, `openvpn`, `htop`
