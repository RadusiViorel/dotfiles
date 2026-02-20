# Spinner Background Fix

## Issue
Loading spinners (VPN, speedtest, weather, yt_dl, network) were not showing when actions were triggered because the actions **blocked** execution until completion.

## Root Cause

When you click a component to trigger a long-running action:

### Before Fix (Blocking):
```
1. User clicks VPN connect
2. Action sets vpn_loading=1
3. Action starts openvpn
4. Action WAITS for connection (blocks for 5-10 seconds)
5. Action sets vpn_loading=0
6. Action returns
7. Component renders → vpn_loading is already 0, no spinner shown!
```

The spinner would never be visible because by the time the component rendered, the action had already completed and set `loading=0`.

### After Fix (Background):
```
1. User clicks VPN connect
2. Action sets vpn_loading=1
3. Action starts openvpn
4. Action starts background monitor process
5. Action returns IMMEDIATELY
6. Component renders → vpn_loading=1, spinner shows! ✓
7. Background process monitors connection
8. Background process sets vpn_loading=0 when done
9. Next render shows connected state
```

## Fixed Components

### 1. VPN (com/vpn/actions/up)
**Before:**
```bash
sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=1/" "$STATE_FILE"
doas /usr/sbin/openvpn --config "$CONF_FILE" --daemon --writepid $PID_FILE
while ! ip a | grep -q tun0; do sleep 0.4; done  # BLOCKS HERE
sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=0/" "$STATE_FILE"
```

**After:**
```bash
sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=1/" "$STATE_FILE"
doas /usr/sbin/openvpn --config "$CONF_FILE" --daemon --writepid $PID_FILE

# Monitor in background
(
  while ! ip a | grep -q tun0; do sleep 0.4; done
  sed -i "s/^$TOGGLE_KEY=.*/$TOGGLE_KEY=1/" "$STATE_FILE"
  sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=0/" "$STATE_FILE"
) &  # Returns immediately!
```

### 2. Weather (com/weather/actions/run)
**Before:**
```bash
sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=1/" "$STATE_FILE"
${COM}/scripts/runner ...  # BLOCKS HERE
sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=0/" "$STATE_FILE"
```

**After:**
```bash
sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=1/" "$STATE_FILE"

# Run in background
(
  ${COM}/scripts/runner ...
  sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=0/" "$STATE_FILE"
) &
```

### 3. Speedtest (com/speedtest/actions/run)
**Before:**
```bash
sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=1/" "$STATE_FILE"
${COM}/scripts/runner ...  # Runs speedtest-cli (blocks 30+ seconds!)
sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=0/" "$STATE_FILE"
```

**After:**
```bash
sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=1/" "$STATE_FILE"

# Run in background
(
  ${COM}/scripts/runner ...
  sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=0/" "$STATE_FILE"
) &
```

### 4. YT-DL (com/yt_dl/actions/download)
**Before:**
```bash
sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=1/" "$STATE_FILE"
TITLE=$(yt-dlp ...)  # BLOCKS during download
# ... more processing ...
sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=0/" "$STATE_FILE"
```

**After:**
```bash
sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=1/" "$STATE_FILE"

# Run in background
(
  TITLE=$(yt-dlp ...)
  # ... processing ...
  sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=0/" "$STATE_FILE"
  notify-send "Finished: ${FILENAME}.${type}"
) &
```

### 5. Network (com/network/actions/connect)
**Special case:** This action has interactive parts (rofi dialogs) that must run synchronously, but the connection monitoring should be async.

**Before:**
```bash
# User selects network via rofi (synchronous - OK)
# User enters password via rofi (synchronous - OK)
wpa_cli enable_network "$nid"

# Wait for connection (BLOCKS - NOT OK)
for i in {1..10}; do
  STATUS=$(wpa_cli status ...)
  if [ "$STATUS" = "COMPLETED" ]; then
    sed -i "s/^$KEY=.*/$KEY=0/" "$STATE_FILE"
    exit 0
  fi
  sleep 1
done
sed -i "s/^$KEY=.*/$KEY=0/" "$STATE_FILE"
```

**After:**
```bash
# User selects network via rofi (synchronous)
# User enters password via rofi (synchronous)
wpa_cli enable_network "$nid"

# Set loading state
sed -i "s/^$KEY=.*/$KEY=1/" "$STATE_FILE"

# Monitor connection in background
(
  for i in {1..10}; do
    STATUS=$(wpa_cli status ...)
    if [ "$STATUS" = "COMPLETED" ]; then
      sed -i "s/^$KEY=.*/$KEY=0/" "$STATE_FILE"
      exit 0
    fi
    sleep 1
  done
  sed -i "s/^$KEY=.*/$KEY=0/" "$STATE_FILE"
) &
```

## Testing

### VPN Spinner
```bash
# Click VPN icon
# Should immediately show: ⠋ Connecting 󰇠
# Spinner should animate: ⠋ → ⠙ → ⠹ → ⠸ → ...
# After connection: shows connected icon
```

### Speedtest Spinner
```bash
# Click speedtest to run
# Should immediately show: ⠋ Testing 󰓅
# Spinner animates during 20-30 second test
# After completion: shows results
```

### Weather Spinner
```bash
# Click weather to update
# Should immediately show: ⠋ Update...󱣶
# After API call: shows weather icon
```

### YT-DL Spinner
```bash
# Click to download video
# Enter URL in rofi
# Should immediately show: ⠋ Downloading 󰦗
# Spinner animates during download
# After complete: shows notification
```

### Network Spinner
```bash
# Click network to connect
# Select WiFi network from rofi
# Enter password in rofi
# Should immediately show: ⠋ Searching... 󱛇
# Spinner animates while connecting (up to 10 seconds)
# After connection: shows SSID and signal
```

## Pattern for Future Actions

If an action performs a long-running task, use this pattern:

```bash
#!/usr/bin/env bash

# Parse parameters...

# Set loading state
sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=1/" "$STATE_FILE"

# Run long task in background
(
  # Do expensive work here
  expensive_command
  
  # Update state when done
  sed -i "s/^$RESULT_KEY=.*/$RESULT_KEY=$result/" "$STATE_FILE"
  sed -i "s/^$LOADING_KEY=.*/$LOADING_KEY=0/" "$STATE_FILE"
  
  # Optional: notify user
  notify-send "Task Complete"
) &  # ← Critical: backgrounding with &

# Action returns immediately
# Component will show spinner because loading=1
```

## Performance Impact

**Positive:**
- Actions return immediately (better responsiveness)
- Background processes don't block xmobar
- User gets visual feedback (spinner) that task is running

**Neutral:**
- Background processes continue after action returns (expected behavior)
- No additional resource usage (same work, just async)

## Status
✅ **Fixed** - All loading spinners now show correctly

---

**Fixed:** 2026-02-13  
**Components Updated:** 5 (vpn, weather, speedtest, yt_dl, network)  
**Pattern:** Moved long-running tasks to background subshells  
**Impact:** Critical UX improvement - spinners now visible
