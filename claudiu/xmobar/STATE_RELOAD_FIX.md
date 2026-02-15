# State Reload Fix

## Issue
After fixing the action KEY parameters, clicks still showed stale data. When clicking a component to toggle state:
1. Click triggers action
2. Action updates state file
3. Component renders with **old** state value from exported environment
4. Next bar cycle (1 second later) shows updated value

**Example:** Clicking calendar to toggle date/time would update the state file, but display wouldn't change until next refresh.

## Root Cause
The optimization exported state variables from `scripts/bar` at the start of each render cycle:

```bash
# In scripts/bar:
set -a  
source "${HOME}/.config/xmobar/scripts/state"
set +a
```

This meant when xmobar called a component with an action:
```bash
com/calendar/index toggle
```

The component would use `$calendar_toggle` from the **exported environment** (from previous bar cycle), not from the current state file.

## Solution

### Add State Reload When Action Is Called

For components with actions that modify state, we now reload the state file when any action parameter is present:

```bash
# Initialize state if needed
state_init "calendar_toggle" 0

# If action is called, re-source state to get current values
if [[ -n "$1" ]]; then
  source "${STATE_FILE}"
fi
```

This ensures:
1. When called from bar (no `$1`): Uses exported state (fast, cached)
2. When called with action (`$1` set): Re-reads state file (current values)
3. After action executes: Re-reads state again (updated values)

### Components Fixed

All components with state-modifying actions:

1. **battery** - show_time toggle
2. **bluetooth** - device cycling, reset
3. **calendar** - time/date toggle  
4. **network** - toggle enable, toggle traffic, connect
5. **page** - page toggle
6. **screenshot** - edit mode toggle
7. **space** - display format toggle
8. **vpn** - connect/disconnect
9. **wallpaper** - lock/change wallpaper
10. **yt_dl** - format toggle

### Components NOT Fixed

These components don't modify state or don't need the fix:

- **speedtest** - triggers background process, doesn't need immediate update
- **weather** - triggers background process
- **updates** - display action, doesn't modify toggle state  
- **brightness** - uses direct xob pipe, no state
- **cpu_temp** - launches htop, no state
- **power** - launches menu, no state
- **rofi_theme** - direct action, no state
- **start** - no actions
- **volume** - direct wpctl commands, no state
- **volume_output** - no actions

## Testing

```bash
cd ~/.config/xmobar

# Test calendar toggle (should alternate immediately)
bash com/calendar/index toggle  # Shows date
bash com/calendar/index toggle  # Shows time

# Test page toggle
bash com/page/index toggle  # Shows page 2 icon
bash com/page/index toggle  # Shows page 1 icon

# Test space toggle
bash com/space/index toggle  # Shows used space
bash com/space/index toggle  # Shows available space
```

## How It Works

### Without Fix (Broken):
```bash
1. User clicks calendar (state=0 in file, but $calendar_toggle=1 in env from old export)
2. index reads $calendar_toggle → gets 1 (wrong!)
3. Passes --value=1 to action
4. Action toggles: 1 → 0, writes to file
5. index renders with $calendar_toggle=1 (still wrong!)
6. Next bar cycle: exports state=0, component shows correctly
```

### With Fix (Working):
```bash
1. User clicks calendar (state=0 in file)
2. index checks: "$1" is set (action present)
3. index sources state file → calendar_toggle=0 (correct!)
4. Passes --value=0 to action
5. Action toggles: 0 → 1, writes to file
6. index sources state file again → calendar_toggle=1
7. index renders with correct value immediately!
```

## Performance Impact

**Minimal** - State file is only re-sourced when:
- An action is called (user click)
- After action completes

During normal bar rendering (no actions), the exported state is used (no extra I/O).

**Tradeoff:**
- **Before fix:** Fast but wrong (stale data on clicks)
- **After fix:** Slightly slower on clicks, but correct (2 extra state file reads per click)

Since clicks are infrequent (user-triggered), this is acceptable.

## Alternative Solutions Considered

### 1. Don't export state from bar
**Rejected:** Would require every component to source state file every second (much slower).

### 2. Always reload state in all components
**Rejected:** Defeats the purpose of the optimization (caching).

### 3. Clear exported state before action execution
**Rejected:** Can't modify parent shell's environment from child process.

### 4. Use trap to reload state after action
**Rejected:** Overly complex, harder to maintain.

**Chosen solution** is the best balance of performance and correctness.

## Verification

✅ All fixed components pass syntax validation  
✅ Calendar toggle works immediately  
✅ Page toggle works immediately  
✅ Space toggle works immediately  
✅ No performance regression during normal bar rendering  

---

**Fixed:** 2026-02-13  
**Components Updated:** 10  
**Performance Impact:** Negligible (only on user clicks)  
**Status:** ✅ Complete - clicks now show immediate feedback
