# Sys Usage CPU Parsing Fix

## Issue
The `sys_usage` component was stuck showing 100% CPU usage regardless of actual system load.

## Root Cause
During optimization, I attempted to consolidate the original `sed` + `awk` pipeline into a single `awk` command, but introduced a bug in the regex:

### Original (Working):
```bash
cpu_usage=$(top -bn1 | grep "Cpu(s)" | sed "s/.*, *\([0-9.]*\)%* id.*/\1/" | awk '{print 100 - $1}')
```

This extracts the idle percentage (e.g., `92.7` from `92.7 id`) and subtracts from 100.

### Broken Optimization:
```bash
cpu_usage=$(cache_cmd "top" 2 top -bn1 | awk '/Cpu\(s\)/ {sub(/.*,/, ""); sub(/%.*/, ""); print 100 - $1}')
```

**Problem:** The regex `sub(/.*,/, "")` is **greedy** and removes everything up to the **last comma**, leaving only the final field:

```
Input:  %Cpu(s):  5.9 us,  1.1 sy,  0.0 ni, 92.7 id,  0.0 wa,  0.0 hi,  0.3 si,  0.0 st
After:  " 0.0 st "  ← Everything before last comma removed!
$1 =    0.0
Result: 100 - 0.0 = 100  ← Always 100%!
```

## Fix

Reverted to the original `sed` + `awk` approach, but kept the caching:

```bash
# Extract idle % with sed (works correctly)
cpu_idle=$(cache_cmd "top" 2 top -bn1 | grep "Cpu(s)" | sed "s/.*, *\([0-9.]*\)%* id.*/\1/")

# Calculate CPU usage
cpu_usage=$(awk "BEGIN {printf \"%.1f\", 100 - $cpu_idle}")
cpu_int=${cpu_usage%.*}
```

### How It Works:
1. `cache_cmd "top" 2 top -bn1` - Caches top output for 2 seconds
2. `grep "Cpu(s)"` - Extracts CPU line
3. `sed "s/.*, *\([0-9.]*\)%* id.*/\1/"` - Extracts idle percentage (e.g., `92.7`)
4. `awk "BEGIN {printf \"%.1f\", 100 - $cpu_idle}"` - Calculates: 100 - 92.7 = 7.3% usage

## Testing

```bash
# Clear cache and test
rm ~/.config/xmobar/.cache/cmd_top*
bash ~/.config/xmobar/com/sys_usage/index

# Should show realistic CPU usage (not stuck at 100%)
```

### Example Output:
```
Before fix: <fc=>󰻠 100%</fc>  ← Always 100%
After fix:  <fc=>󰻠 9%</fc>    ← Actual usage
```

## Alternative Fix Considered

Could have fixed the awk regex to properly extract idle percentage:

```bash
awk '/Cpu\(s\)/ {for(i=1;i<=NF;i++){if($i=="id,"){print 100-$(i-1)}}}'
```

**Rejected** because:
- More complex than necessary
- Original `sed` approach is clearer and proven
- Performance difference negligible with caching

## Performance Impact
**None** - Still using cached `top` output (2 second TTL). Just fixed the parsing logic.

## Status
✅ **Fixed** - CPU usage now shows correct values

---

**Fixed:** 2026-02-13  
**Component:** sys_usage  
**Issue:** Broken awk regex during optimization  
**Impact:** Critical (always showed 100%)  
**Solution:** Reverted to original sed parsing method
