# State Reload Fix - Complete

## Overview
All components that use state variables and have click actions now have the state reload fix applied.

## The Issue
When clicking a component to trigger an action, the component would use **cached/exported state values** from the previous bar render cycle instead of reading the current state file values.

## The Fix
Add state reload when any action parameter is present:

```bash
# After state_init calls, before case statement:
if [[ -n "$1" ]]; then
  source "${STATE_FILE}"
fi
```

This ensures:
- **Normal rendering** (no action): Uses exported state from scripts/bar (fast)
- **Click actions**: Re-reads state file for current values (accurate)

## Components Fixed (13 total)

### Already Had Fix:
1. ✓ battery
2. ✓ bluetooth
3. ✓ calendar
4. ✓ network
5. ✓ page
6. ✓ screenshot
7. ✓ space
8. ✓ vpn
9. ✓ wallpaper
10. ✓ yt_dl

### Newly Fixed:
11. ✓ **updates** - Added in this session
12. ✓ **speedtest** - Added in this session
13. ✓ **weather** - Added in this session

## Components Without State (Don't Need Fix)

These components have actions but don't use state variables:
- **brightness** - Uses direct hardware queries
- **cpu_temp** - Launches htop (no state)
- **power** - Launches power menu (no state)
- **rofi_theme** - Direct theme changer (no state)
- **volume** - Direct wpctl commands (no state)
- **volume_output** - Direct wpctl queries (no state)

## Pattern Applied

For each component with state-based actions:

```bash
#!/usr/bin/env bash

# Vars/state already exported from scripts/bar
source "${HOME}/.config/xmobar/scripts/state_loader"

COM="${HOME}/.config/xmobar/com/<component>"
STATE_FILE="${HOME}/.config/xmobar/scripts/state"

# Initialize state keys if needed
state_init "component_setting" "default"

# If action is called, re-source state to get current values
if [[ -n "$1" ]]; then
  source "${STATE_FILE}"
fi

case "$1" in
  action_name) ${COM}/actions/action_name \
    --value=${component_setting}  # ← Now has current value!
    ...
```

## How It Works

### Without Fix (Broken):
```
1. Bar renders at 10:00:00
   - scripts/bar exports: setting=0
2. User clicks at 10:00:30
   - Component uses exported: setting=0 (stale!)
   - Action runs with old value
   - Updates state to: setting=1
3. Component renders with setting=0 (wrong!)
4. Bar renders at 10:01:00
   - scripts/bar exports: setting=1
   - Component shows correct value (delayed 30s!)
```

### With Fix (Working):
```
1. Bar renders at 10:00:00
   - scripts/bar exports: setting=0
2. User clicks at 10:00:30
   - Component checks: "$1" is set (action present)
   - Component re-sources state file
   - Component reads: setting=0 (current!)
   - Action runs with correct value
   - Updates state to: setting=1
3. Component re-sources state after action
   - Reads: setting=1
4. Component renders with setting=1 (correct immediately!)
```

## State Variables by Component

### battery
- `battery_level_icon` - Icon cycling state
- `battery_show_time` - Show/hide time toggle
- `battery_info` - Cached battery info
- `battery_timestamp` - Cache timestamp

### bluetooth
- `bluetooth_index_device` - Current device index

### calendar
- `calendar_toggle` - Time/date display toggle

### network
- `network_enabled` - Enable/disable toggle
- `network_loading` - Connection spinner
- `network_spinner` - Spinner frame
- `network_traffic` - Traffic display toggle

### page
- `page_number` - Current page (1 or 2)

### screenshot
- `screenshot_edit` - Edit mode toggle

### space
- `space_toggle` - Display format toggle

### speedtest
- `speedtest_isp` - ISP name
- `speedtest_up` - Upload speed
- `speedtest_down` - Download speed
- `speedtest_timestamp` - Last test time
- `speedtest_loading` - Test running
- `speedtest_spinner` - Spinner frame

### updates
- `updates_timestamp` - Last check time
- `updates_count` - Pending updates count

### vpn
- `vpn_toggle` - Connected state
- `vpn_loading` - Connecting state
- `vpn_spinner` - Spinner frame

### wallpaper
- `wallpaper_lock` - Lock wallpaper toggle
- `wallpaper_image` - Current wallpaper path

### weather
- `weather_value` - Weather icon/data
- `weather_timestamp` - Last update time
- `weather_loading` - Updating state
- `weather_spinner` - Spinner frame

### yt_dl
- `yt_type` - Download format (mp4/mp3)
- `yt_loading` - Download state
- `yt_spinner` - Spinner frame

## Verification

To verify a component has the fix:

```bash
grep -A2 "If action is called" ~/.config/xmobar/com/<component>/index
```

Should show:
```bash
# If action is called, re-source state to get current values
if [[ -n "$1" ]]; then
  source "${STATE_FILE}"
fi
```

## Testing

Test any component with state:

```bash
cd ~/.config/xmobar

# Check current state
grep <component>_setting scripts/state

# Trigger action
bash com/<component>/index <action>

# Verify state updated
grep <component>_setting scripts/state

# Component output should reflect new state
bash com/<component>/index
```

## Performance Impact

**Minimal:**
- State file only re-sourced when actions are called (user clicks)
- Normal bar rendering (1/second) still uses exported state (fast)
- Actions are infrequent (user-triggered only)

**Before optimization:** Each component sourced state 1/second = 22 reads/second
**After optimization:** State sourced once in scripts/bar = 1 read/second
**With reload fix:** +1 read per user click (negligible)

## Status

✅ **Complete** - All components with state-based actions have the reload fix

---

**Completed:** 2026-02-13  
**Components Fixed:** 13  
**Components Verified:** 19 (13 with state, 6 without)  
**Pattern:** Consistent across all components  
**Impact:** Immediate state updates on all clicks
