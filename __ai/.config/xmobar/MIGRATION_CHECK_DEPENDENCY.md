# Migration to check_dependency Helper - Complete

## Summary

Successfully migrated **all 9 components** from manual dependency checks to the centralized `check_dependency` helper function.

## Components Migrated

### 1. battery
- **Before:** 5 lines for `acpi` check
- **After:** 2 lines (source + check)
- **Mode:** `--exit` (default)

### 2. bluetooth
- **Before:** 15 lines for 3 checks (`bluetoothctl`, `upower`, `bluetuith`)
- **After:** 4 lines (source + 3 checks)
- **Mode:** 
  - `bluetoothctl`: `--exit`
  - `upower`: `--exit`
  - `bluetuith`: `--no-exit` (optional dependency)

### 3. brightness
- **Before:** 12 lines for 2 checks (`brightnessctl`, `xob`)
- **After:** 3 lines (source + 2 checks)
- **Mode:** `--exit` for both

### 4. calendar
- **Before:** 4 lines for `yad` check
- **After:** 2 lines (source + check)
- **Mode:** `--no-exit` (optional dependency)

### 5. capslock
- **Before:** 5 lines for `xset` check
- **After:** 2 lines (source + check)
- **Mode:** `--exit`

### 6. cpu_temp
- **Before:** 5 lines for `sensors` check
- **After:** 2 lines (source + check)
- **Mode:** `--exit`

### 7. screenshot
- **Before:** 20 lines for 4 checks (`scrot`, `xrandr`, `swappy`, `xclip`)
- **After:** 5 lines (source + 4 checks)
- **Mode:** `--exit` for all

### 8. speedtest
- **Before:** 5 lines for `speedtest` check
- **After:** 2 lines (source + check)
- **Mode:** `--exit`

### 9. wallpaper
- **Before:** 5 lines for `nsxiv` check
- **After:** 2 lines (source + check)
- **Mode:** `--exit`

### 10. yt_dl
- **Before:** 10 lines for 2 checks (`yt-dlp`, `ffmpeg`)
- **After:** 3 lines (source + 2 checks)
- **Mode:** `--exit` for both

## Total Impact

- **Lines removed:** ~86 lines of boilerplate code
- **Lines added:** ~29 lines (including source statements)
- **Net reduction:** ~57 lines of code
- **Consistency:** All dependency checks now use the same format and error messages

## Pattern Used

### For Required Dependencies (--exit)
```bash
source "${HOME}/.config/xmobar/scripts/check_dependency"
check_dependency "command_name"
```

### For Optional Dependencies (--no-exit)
```bash
source "${HOME}/.config/xmobar/scripts/check_dependency"
check_dependency "command_name" --no-exit
```

## Verification

All components tested and working:
- No old `if ! command -v` patterns remain in component index files
- All components properly source the helper
- Error messages are consistent across all components

## Future Additions

When adding new components, always use `check_dependency` instead of manual checks:

```bash
#!/usr/bin/env bash

source "${HOME}/.config/xmobar/scripts/check_dependency"
check_dependency "required_tool"
check_dependency "optional_tool" --no-exit

# ... rest of component logic
```
