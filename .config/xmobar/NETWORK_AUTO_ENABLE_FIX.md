# Network Auto-Enable on Connect Fix

## Issue
When network was disabled (middle-click toggle), clicking to connect wouldn't re-enable the network display, so after connecting you'd still see the disabled icon instead of WiFi info.

## User Flow Problem

### Before Fix:
1. **Middle-click** network → Toggles `network_enabled=0` → Disconnects WiFi
2. Component shows: `󰤮` (disabled/muted icon)
3. **Left-click** to connect → Rofi appears, user selects network
4. Network connects successfully
5. Component **still shows**: `󰤮` (disabled icon)
6. **Problem:** User is connected but can't see WiFi info!

User would need to middle-click again to toggle `network_enabled=1` to see WiFi info.

## Root Cause

The connect action only managed the loading spinner, not the enabled state:

```bash
# In com/network/actions/connect (before fix):
# Only sets network_loading
sed -i "s/^$KEY=.*/$KEY=1/" "$STATE_FILE"  # KEY = network_loading

# Never touches network_enabled!
# So if network_enabled=0, it stays 0 even after connecting
```

## Fix

Now the connect action **automatically enables** network display:

```bash
# At the start of connect action:
sed -i "s/^network_enabled=.*/network_enabled=1/" "$STATE_FILE"

# Then proceed with connection...
```

**Rationale:** If the user clicks to connect to a network, they obviously want to see the network information. Auto-enabling makes logical sense.

## User Flow After Fix

1. **Middle-click** network → `network_enabled=0` → Disconnects WiFi
2. Component shows: `󰤮` (disabled icon)
3. **Left-click** to connect → **Auto-enables network display**
4. Rofi appears, user selects network
5. Network connects
6. Component shows: `YourSSID 42󰏰 󰤢` (WiFi info visible!)

## Mouse Button Actions

The network component has three mouse actions:

| Button | Action | Description |
|--------|--------|-------------|
| **1** (Left) | `connect` | Open WiFi selection, **auto-enables display** |
| **2** (Middle) | `toggle` | Enable/disable network display & WiFi |
| **3** (Right) | `traffic` | Toggle bandwidth display |

## State Variables

- `network_enabled` - 0=disabled (show 󰤮), 1=enabled (show WiFi info)
- `network_loading` - 0=idle, 1=connecting (show spinner)
- `network_traffic` - 0=hide bandwidth, 1=show bandwidth

## Toggle Behavior

### Middle-Click Toggle:
```bash
# Toggles network_enabled between 0 and 1

If enabling (0→1):
  wpa_cli reconnect  # Reconnect to last network

If disabling (1→0):
  wpa_cli disconnect  # Disconnect WiFi
```

### Connect Always Enables:
```bash
# Always sets network_enabled=1
# Then shows rofi for network selection
# User intent: "I want to connect" implies "I want to see network info"
```

## Use Cases

### Use Case 1: Temporarily Hide WiFi Info
**Action:** Middle-click to disable
**Result:** Disconnects WiFi, shows 󰤮
**To re-enable:** Middle-click again OR left-click to connect

### Use Case 2: Connect After Disable
**Action:** 
1. Middle-clicked to disable (now showing 󰤮)
2. Left-click to connect
**Result:** 
- Network display auto-enables
- Rofi shows networks
- After connecting, shows WiFi info

### Use Case 3: Switch Networks
**Action:** Left-click when already connected
**Result:**
- Rofi shows available networks
- Select different network
- Connects and updates display

### Use Case 4: Quick Disconnect/Reconnect
**Action:**
1. Middle-click to disconnect
2. Middle-click again to reconnect
**Result:**
- Reconnects to last network
- Shows WiFi info

## Testing

### Test 1: Disable Then Connect
```bash
cd ~/.config/xmobar

# Disable network
sed -i 's/network_enabled=1/network_enabled=0/' scripts/state
bash com/network/index
# Output: <fc=>󰤮</fc>

# Simulate connect (will auto-enable)
sed -i "s/^network_enabled=.*/network_enabled=1/" scripts/state
grep network_enabled scripts/state
# Output: network_enabled=1

bash com/network/index
# Output: <SSID> XX󰏰 󰤢
```

### Test 2: Toggle On/Off
```bash
# Check current state
grep network_enabled scripts/state

# Toggle
bash com/network/index toggle
grep network_enabled scripts/state
# Should be opposite of before
```

## Edge Cases

### Already Enabled
- If `network_enabled=1` and user clicks connect
- Setting it to 1 again is harmless (idempotent)
- No side effects

### During Connection
- If already connecting (`network_loading=1`)
- User clicks connect again
- Old connection monitoring continues
- New rofi dialog appears
- Auto-enables display (safe)

### After Timeout
- If connection times out
- `network_enabled` stays 1 (enabled)
- Shows previous network info or disconnected state
- User can try again

## Implementation Details

**Location of change:** `com/network/actions/connect` line 14

**Code added:**
```bash
# Enable network display when connecting
# (If user clicked connect, they want to see network info)
sed -i "s/^network_enabled=.*/network_enabled=1/" "$STATE_FILE"
```

**Timing:** Before WiFi scan, so:
1. User clicks connect
2. Network display enabled immediately
3. Scan happens (2 seconds)
4. Rofi appears
5. User selects network
6. Connection happens
7. Component shows info (because enabled=1)

## Alternative Considered

**Option:** Pass `--ENABLED_KEY` parameter to connect action

**Rejected because:**
- More complex (extra parameter)
- Not needed (STATE_FILE location is known)
- Direct sed update is simpler
- Connect always wants to enable anyway

## Status

✅ **Fixed** - Connecting to WiFi now auto-enables network display

---

**Fixed:** 2026-02-13  
**Component:** network  
**Issue:** Connect didn't re-enable display after toggle-off  
**Solution:** Auto-enable `network_enabled=1` at start of connect action  
**Impact:** Better UX - clicking connect implies wanting to see network info
