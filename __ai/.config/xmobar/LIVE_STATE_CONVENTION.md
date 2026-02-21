# Live State Convention - `_live` Suffix

## Overview

A naming convention for state variables that trigger automatic, uncached bar refreshes. Any state variable ending with `_live` will bypass the cache and force immediate xmobar updates.

## Convention Rule

**State variables ending in `_live` automatically trigger bar refresh when updated.**

This ensures real-time UI updates for dynamic states like loading spinners, progress indicators, or live data.

## Implementation

### In `state_loader` Script

```bash
state_update() {
  local key="$1"
  local value="$2"

  sed -i "s|^${key}=.*|${key}=${value}|" "$STATE_FILE"
  
  # Convention: Variables ending in _live trigger immediate bar refresh
  # This prevents caching and ensures real-time updates for loading states
  if [[ "$key" =~ _live$ ]]; then
    "${HOME}/.config/xmobar/scripts/refresh" 2>/dev/null &
  fi
}
```

### In `bar` Script

The bar script checks for active `_live` states and bypasses cache when any are set to `1`:

```bash
spinner_active=0
STATE_FILE="${HOME}/.config/xmobar/scripts/state"

# Check for any *_live=1 in state file (live states bypass cache)
if grep -q "^[a-z_]*_live=1$" "$STATE_FILE" 2>/dev/null; then
  spinner_active=1
fi

# Fast path: if cache exists and is newer than trigger AND no live state is active, just output it
if [[ $spinner_active -eq 0 && -f "$CACHE_FILE" && -f "$TRIGGER_FILE" && "$CACHE_FILE" -nt "$TRIGGER_FILE" ]]; then
  cat "$CACHE_FILE"
  exit 0
fi
```

**Behavior:**
- **When any `*_live=1`**: Cache bypassed, bar regenerates every poll (live spinner animation)
- **When all `*_live=0`**: Cache used, bar outputs instantly without regeneration

## Components Using `_live` Convention

### 1. network - `network_loading_live`
**Purpose:** Live spinner animation while searching for WiFi networks

**Usage:**
```bash
state_init "network_loading_live" 0

# In connect action - set to 1
state_update "network_loading_live" 1

# Spinner display when loading
if (( network_loading_live == 1 )); then
  spinner_icons=(⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏)
  idx=$(( ( network_spinner + 1 ) % ${#spinner_icons[@]} ))
  state_update "network_spinner" "${idx}"
  echo "<fc=${COLOR_INFO}>${spinner_icons[$idx]} Searching... 󱛇</fc>"
  exit 0
fi
```

### 2. vpn - `vpn_loading_live`
**Purpose:** Live spinner animation while connecting to VPN

**Actions:** `up`, `down`

### 3. weather - `weather_loading_live`
**Purpose:** Live spinner animation while fetching weather data

**Actions:** `run`

### 4. speedtest - `speedtest_loading_live`
**Purpose:** Live spinner animation while running speed test

**Actions:** `run`

### 5. yt_dl - `yt_loading_live`
**Purpose:** Live spinner animation while downloading YouTube videos

**Actions:** `download`

## Benefits

### Before (Manual Refresh)
```bash
# Had to explicitly call refresh after each spinner update
state_update "loading_flag" 1
"${HOME}/.config/xmobar/scripts/refresh"
```

### After (Automatic)
```bash
# Refresh happens automatically due to _live suffix
state_update "network_loading_live" 1
```

## Naming Pattern

All loading states now follow this pattern:
- `{component}_loading_live` - Main loading flag (0 or 1)
- `{component}_spinner` - Spinner animation index (0-9)

## When to Use `_live`

Use the `_live` suffix for state variables that:
1. **Change frequently** (multiple times per second)
2. **Require immediate visual feedback** (spinners, progress bars)
3. **Should bypass cache** (real-time data display)
4. **Control animations** (loading states, transitions)

## When NOT to Use `_live`

Avoid `_live` for:
1. Static configuration (e.g., `yt_type`, `vpn_toggle`)
2. Cached data (e.g., `weather_value`, `speedtest_isp`)
3. Timestamps (e.g., `weather_timestamp`)
4. Counters that don't need instant updates

## Performance Impact

- **Refresh triggers:** Each `_live` update spawns a background refresh process
- **Frequency:** Spinners typically update every 0.5-1 second
- **Mitigation:** Refresh script uses caching to avoid full bar regeneration

## Migration Summary

Renamed all loading states to follow `_live` convention:

| Component | Old Variable | New Variable |
|-----------|-------------|--------------|
| network | `network_loading` | `network_loading_live` |
| vpn | `vpn_loading` | `vpn_loading_live` |
| weather | `weather_loading` | `weather_loading_live` |
| speedtest | `speedtest_loading` | `speedtest_loading_live` |
| yt_dl | `yt_loading` | `yt_loading_live` |

## Future Usage

When creating new components with live states:

```bash
# Initialize with _live suffix
state_init "mycomponent_loading_live" 0
state_init "mycomponent_spinner" 0

# Start loading animation
state_update "mycomponent_loading_live" 1  # Auto-triggers refresh

# Update spinner (also auto-refreshes)
if (( mycomponent_loading_live == 1 )); then
  spinner_icons=(⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏)
  idx=$(( ( mycomponent_spinner + 1 ) % ${#spinner_icons[@]} ))
  state_update "mycomponent_spinner" "$idx"  # Updates but doesn't refresh
  echo "${spinner_icons[$idx]} Loading..."
  exit 0
fi

# Stop loading animation
state_update "mycomponent_loading_live" 0  # Auto-triggers final refresh
```

## Additional `_live` Use Cases

Beyond loading states, `_live` can be used for:
- **Real-time counters** (download progress percentages)
- **Live status indicators** (microphone muted/unmuted)
- **Dynamic badges** (notification counts that update frequently)
- **Animated icons** (pulsing indicators for active processes)
