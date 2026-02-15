# Action Clicks Fix

## Issue
After the optimization, mouse clicks stopped working because action calls were passing undefined `$__variable` references instead of the actual state key names.

## Root Cause
During optimization, the old pattern used temporary variables like:
```bash
__toggle="calendar_toggle"
__enabled="network_enabled"
```

These were removed and replaced with direct `state_init` calls. However, action case statements still referenced these undefined variables:
```bash
# BROKEN:
--KEY=${__toggle}  # $__toggle is undefined!

# FIXED:
--KEY=calendar_toggle  # Use actual string key name
```

## Fixed Components

All components with mouse actions have been fixed:

### High-Priority Action Fixes
1. **battery** - show_time action
   - Fixed: `--KEY=battery_show_time`

2. **bluetooth** - reset, cycle_up, cycle_down actions
   - Fixed: `--KEY=bluetooth_index_device`

3. **calendar** - toggle action
   - Fixed: `--KEY=calendar_toggle`

4. **network** - connect, toggle, traffic actions
   - Fixed: `--KEY=network_loading`, `--KEY=network_enabled`, `--KEY=network_traffic`

5. **page** - toggle action
   - Fixed: `--KEY=page_number`
   - Also: Converted to use `state_loader` instead of raw grep/sed

6. **screenshot** - toggle action
   - Fixed: `--KEY=screenshot_edit`

7. **space** - toggle action
   - Fixed: `--KEY=space_toggle`

8. **speedtest** - run action
   - Fixed: `--ISP_KEY=speedtest_isp`, `--UP_KEY=speedtest_up`, `--DOWN_KEY=speedtest_down`, `--TIMESTAMP_KEY=speedtest_timestamp`, `--LOADING_KEY=speedtest_loading`
   - Also: Fixed spinner to use `state_update` instead of `sed -i`

9. **updates** - show action
   - Fixed: `--COUNT_KEY=updates_count`

10. **vpn** - up, down actions
    - Fixed: `--TOGGLE_KEY=vpn_toggle`, `--LOADING_KEY=vpn_loading`

11. **wallpaper** - lock_image, change actions
    - Fixed: `--LOCK_KEY=wallpaper_lock`, `--IMAGE_KEY=wallpaper_image`

12. **weather** - run action
    - Fixed: `--TIMESTAMP_KEY=weather_timestamp`, `--VALUE_KEY=weather_value`, `--LOADING_KEY=weather_loading`

13. **yt_dl** - download, toggle actions
    - Fixed: `--LOADING_KEY=yt_loading`, `--KEY=yt_type`

## Additional Fixes

### Internal sed -i calls
Also replaced internal `sed -i` calls with `state_update` for consistency:

- **battery**: `state_update "battery_level_icon" "$idx"`
- **bluetooth**: `state_update "bluetooth_index_device" -1`
- **speedtest**: `state_update "speedtest_spinner" "$idx"`

### Page component refactor
The **page** component still used the old pattern entirely. Updated to:
```bash
# Old:
source "${HOME}/.config/xmobar/scripts/state"
__number="page_number"
grep -q "^$__number=" "$STATE_FILE" || ( echo "$__number=1" >> "$STATE_FILE" && page_number=1 )

# New:
source "${HOME}/.config/xmobar/scripts/state_loader"
state_init "page_number" 1
```

## Verification

✅ All component syntax validated  
✅ No undefined `$__` variables in action calls  
✅ Calendar toggle tested and working  
✅ State updates correctly  

## Testing

Test any action by clicking the component in xmobar, or manually:
```bash
cd ~/.config/xmobar

# Test calendar toggle
bash com/calendar/index toggle
grep calendar_toggle scripts/state

# Test page toggle
bash com/page/index toggle
grep page_number scripts/state

# Test space toggle
bash com/space/index toggle
grep space_toggle scripts/state
```

## Components Without Actions
These components don't have mouse actions (display-only or custom actions):
- brightness (uses direct xob pipe, no state)
- cpu_temp (htop launcher)
- power (direct action launcher)
- rofi_theme (direct theme changer)
- start (no actions)
- volume (direct wpctl commands)
- volume_output (no actions)

## Status
✅ **All Fixed** - Mouse clicks now work correctly across all components!

---

**Fixed:** 2026-02-13  
**Components Updated:** 13  
**Breaking Changes:** 0 (all functionality restored)
