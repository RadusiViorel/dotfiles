# Network Connection Spinner Fix

## Issue
When clicking the WiFi/network icon to connect, rofi appears but no spinner is shown during connection.

## Root Cause

The network connect action had **two different code paths**:

### Path 1: Known Network (Had Bug)
```bash
# If known network:
wpa_cli enable_network "$nid"
wpa_cli select_network "$nid"
# ❌ No loading state set!
# ❌ Exits immediately
exit 0
```

Result: User selects network in rofi → Action exits → No spinner shown

### Path 2: New/Unknown Network (Worked)
```bash
# If unknown network:
# Ask for password in rofi
wpa_cli add_network...
wpa_cli enable_network "$nid"

# ✓ Set loading state
sed -i "s/^$KEY=.*/$KEY=1/" "$STATE_FILE"

# ✓ Monitor in background
(wait for connection...) &
```

Result: User enters password → Action sets loading → Shows spinner

## The Problem

For **known networks** (networks you've connected to before):
1. User clicks network icon
2. Rofi appears with network list
3. User selects known network
4. Action connects and **exits immediately**
5. No loading state ever set
6. No spinner shown
7. Network connects silently

For **new networks**:
1. User clicks network icon  
2. Rofi appears with network list
3. User selects new network
4. Rofi asks for password
5. Action connects, sets loading=1, backgrounds monitor
6. Spinner shows while connecting

**Issue:** Known networks didn't show spinner because they connected "instantly" without setting loading state.

## Fix

Now **both paths** set loading state and monitor connection in background:

### Known Networks (Fixed):
```bash
if [ -n "$nid" ]; then
  wpa_cli enable_network "$nid"
  wpa_cli select_network "$nid"
  
  # ✓ Set loading state
  sed -i "s/^$KEY=.*/$KEY=1/" "$STATE_FILE"
  
  # ✓ Monitor connection in background
  (
    for i in {1..10}; do
      STATUS=$(wpa_cli status ...)
      if [ "$STATUS" = "COMPLETED" ]; then
        notify-send "WiFi" "Connected to $ssid"
        sed -i "s/^$KEY=.*/$KEY=0/" "$STATE_FILE"
        exit 0
      fi
      sleep 1
    done
    
    # Timeout
    notify-send "WiFi" "Connection timeout"
    sed -i "s/^$KEY=.*/$KEY=0/" "$STATE_FILE"
  ) &
  
  exit 0
fi
```

### New Networks (Already Worked):
```bash
# Same monitoring code (unchanged)
sed -i "s/^$KEY=.*/$KEY=1/" "$STATE_FILE"
(monitor connection...) &
```

## Expected Behavior Now

### Connecting to Known Network:
1. Click network icon
2. Rofi shows network list
3. Select known network
4. Rofi closes
5. **Within 1 second** → Spinner appears: `⠋ Searching... 󱛇`
6. Spinner animates while connecting (1-3 seconds typically)
7. Connection completes → Shows WiFi info: `<SSID> 42󰏰 󰤢`
8. Notification: "Connected to <SSID>"

### Connecting to New Network:
1. Click network icon
2. Rofi shows network list
3. Select unknown network
4. Rofi asks for password
5. Enter password
6. Rofi closes
7. **Within 1 second** → Spinner appears: `⠋ Searching... 󱛇`
8. Spinner animates while connecting (1-10 seconds)
9. Connection completes → Shows WiFi info
10. Notification: "Connected to <SSID>"

### Connection Timeout (Both):
- If connection takes > 10 seconds
- Spinner stops
- Notification: "Connection timeout" or "error"
- Component shows previous state

## Timeline Example

### Before Fix (Known Network):
```
0.0s: Click network icon
0.0s: Rofi appears
2.0s: User selects "Home WiFi"
2.0s: Action connects
2.0s: Action exits ← No loading state!
3.0s: Bar refreshes → Shows old WiFi info (no change visible)
4.0s: Connection complete (silently)
4.0s: Bar refreshes → Shows "Home WiFi" (suddenly appeared)
```

### After Fix (Known Network):
```
0.0s: Click network icon
0.0s: Rofi appears
2.0s: User selects "Home WiFi"
2.0s: Action sets loading=1
2.0s: Action starts background monitor
2.0s: Action exits
3.0s: Bar refreshes → Shows ⠋ Searching... 󱛇 (spinner!)
3.5s: Connection complete
3.5s: Background sets loading=0
4.0s: Bar refreshes → Shows "Home WiFi 42󰏰 󰤢"
```

## Code Consistency

Both code paths now use identical monitoring logic:
- Set `loading=1` before backgrounding
- Monitor connection status in loop (max 10 seconds)
- Send notification on success/failure
- Clear `loading=0` when done
- Use same timeout (10 seconds)

## Testing

### Test Known Network:
```bash
# Prerequisites:
# - Have at least one saved WiFi network
# - Be disconnected or on different network

cd ~/.config/xmobar

# Manually trigger connect
bash com/network/index connect

# In rofi:
# 1. Select a known network (one with 󰦕 icon)
# 2. Rofi should close
# 3. Wait 1 second
# 4. Check component output:
bash com/network/index

# Should show:
# ⠋ Searching... 󱛇  (or similar spinner)

# Wait a few more seconds
bash com/network/index

# Should show:
# <SSID> XX󰏰 󰤢
```

### Test New Network:
```bash
bash com/network/index connect

# In rofi:
# 1. Select an unknown network (no 󰦕 icon)
# 2. Enter password when prompted
# 3. Rofi closes
# 4. Within 1 second, spinner should appear
# 5. Connection completes, shows WiFi info
```

### Test Connection Timeout:
```bash
# Connect to network with wrong password
# Or network that's out of range

bash com/network/index connect

# After rofi:
# 1. Spinner appears
# 2. After 10 seconds, timeout notification
# 3. Spinner disappears
# 4. Shows previous network state
```

## Spinner Animation

The network spinner shows: `⠋ Searching... 󱛇`

Animation frames (10 frames, updates every bar refresh):
```
⠋ → ⠙ → ⠹ → ⠸ → ⠼ → ⠴ → ⠦ → ⠧ → ⠇ → ⠏ → (repeat)
```

## Performance Impact

**None** - Same monitoring code, just moved to both code paths.

Background process runs until:
- Connection succeeds (usually 1-3 seconds)
- Timeout after 10 seconds
- Then exits cleanly

## Status

✅ **Fixed** - Network connection now shows spinner for both known and new networks

---

**Fixed:** 2026-02-13  
**Component:** network  
**Issue:** No spinner when connecting to known networks  
**Solution:** Set loading state and monitor connection for all connection attempts  
**Impact:** Consistent visual feedback for all network connections
