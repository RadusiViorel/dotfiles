# Network LAN Detection Fix

## Issue
Network component was showing "LAN 󰈀" even when connected via WiFi.

## Root Cause

The original logic checked if any non-wireless interface had `state UP`:

```bash
# Old logic (broken):
if ip link show ${lan_interface} | grep -q 'state UP'; then
  echo "LAN 󰈀"
  exit 0
fi
```

**Problem:** Many interfaces can be `state UP` without having an active network connection:
- Docker bridges (`br-*`, `docker0`)
- Docker virtual ethernet (`veth*`)
- VPN tunnels (`tun*`, `tap*`)
- Virtual bridges (`virbr*`)
- Ethernet with no cable (`eno1` with `NO-CARRIER`)

In this case, the component detected `br-7606a42e4040` (Docker bridge) as the "LAN interface" and since it was UP, showed "LAN" instead of WiFi info.

## Example System State

```
eno1:    Ethernet, state UP, but NO-CARRIER (no cable)
wlp5s0:  WiFi, state UP, connected ✓
docker0: Docker bridge, state UP
br-*:    Docker bridge, state UP  
veth*:   Docker virtual ethernet (many), state UP
tun0:    VPN tunnel, state UP
```

Old logic would detect `docker0` or `br-*` first and show "LAN".

## Fix

### New Logic:

1. **Skip virtual interfaces:**
   - `docker*`, `veth*`, `br-*` (Docker)
   - `tun*`, `tap*` (VPN)
   - `virbr*` (libvirt)

2. **Check carrier signal:**
   - Read `/sys/class/net/<interface>/carrier`
   - Only show "LAN" if carrier = 1 (cable connected)

3. **Fall through to WiFi:**
   - If no active LAN connection, show WiFi info

### Implementation:

```bash
while IFS=': ' read -r _ iface _; do
  # Skip loopback and virtual interfaces
  if [[ $iface != "lo" ]] && [[ $iface != docker* ]] && [[ $iface != veth* ]] && \
     [[ $iface != br-* ]] && [[ $iface != tun* ]] && [[ $iface != tap* ]] && \
     [[ $iface != virbr* ]]; then
    if [ ! -d "/sys/class/net/$iface/wireless" ]; then
      # Check if this is a physical ethernet interface with carrier
      if [ -f "/sys/class/net/$iface/carrier" ]; then
        carrier=$(cat "/sys/class/net/$iface/carrier" 2>/dev/null || echo 0)
        if [[ $carrier == "1" ]]; then
          # Found physical LAN with active connection
          echo "LAN 󰈀"
          exit 0
        fi
      fi
    else
      interface=$iface  # WiFi interface
    fi
  fi
done

# No active LAN connection, show WiFi info
```

## Testing

### Test 1: WiFi Connected (No LAN Cable)
```bash
cd ~/.config/xmobar
bash com/network/index

# Expected output:
# <Your WiFi SSID> 42󰏰 󰤢
# (Shows WiFi name, signal strength, and icon)
```

### Test 2: LAN Cable Connected
```bash
# Plug in ethernet cable
# carrier file should show 1:
cat /sys/class/net/eno1/carrier  # Should be 1

bash com/network/index

# Expected output:
# LAN 󰈀
```

### Test 3: Check Interface Detection
```bash
# List WiFi interfaces:
ls -d /sys/class/net/*/wireless 2>/dev/null | sed 's|/sys/class/net/||; s|/wireless||'

# Check ethernet carrier status:
for iface in /sys/class/net/e*/carrier; do 
  name=$(echo $iface | sed 's|/sys/class/net/||; s|/carrier||')
  carrier=$(cat $iface 2>/dev/null || echo "N/A")
  echo "$name: carrier=$carrier"
done
```

## Interface Priority

1. **Physical Ethernet with carrier = 1** → Show "LAN"
2. **WiFi connected** → Show WiFi SSID and signal
3. **Network disabled** → Show disabled icon

## Virtual Interfaces Excluded

The following interface patterns are ignored:
- `docker*` - Docker daemon
- `veth*` - Docker container virtual ethernet
- `br-*` - Docker/custom bridges
- `tun*` - VPN/tunnel interfaces (OpenVPN, WireGuard)
- `tap*` - TAP network devices
- `virbr*` - libvirt virtual bridges
- `lo` - Loopback (always excluded)

## Common Interface Names

### Physical Interfaces:
- `eth0`, `eth1` - Old-style Ethernet naming
- `eno1`, `eno2` - BIOS-based Ethernet naming
- `enp*` - PCI-based Ethernet naming (e.g., `enp0s31f6`)
- `wlan0`, `wlan1` - Old-style WiFi naming
- `wlp*` - PCI-based WiFi naming (e.g., `wlp5s0`)

### Virtual Interfaces (Excluded):
- `docker0` - Docker default bridge
- `br-1234abcd` - Docker custom bridge
- `veth1234abc` - Docker container interface
- `tun0`, `tun1` - VPN tunnels
- `virbr0` - libvirt default bridge

## Carrier Detection

The carrier file indicates physical connection:

```bash
/sys/class/net/<interface>/carrier

Values:
  1 = Cable connected, link detected
  0 = No cable / no link
  (file doesn't exist = interface doesn't support carrier detection)
```

## Edge Cases Handled

1. **Docker running** → Bridges and veth interfaces ignored
2. **VPN active** → tun/tap interfaces ignored
3. **Ethernet unplugged** → carrier=0, falls through to WiFi
4. **Multiple ethernet ports** → Checks all, uses first with carrier=1
5. **USB ethernet adapters** → Detected if not using virtual naming

## Performance Impact

**Minimal** - Only changes the detection logic:
- Same number of interface checks
- Added carrier file reads (fast, sysfs)
- Still uses 2-second cache for ip output

## Status

✅ **Fixed** - Network component now correctly detects physical LAN vs WiFi

---

**Fixed:** 2026-02-13  
**Component:** network  
**Issue:** Showed "LAN" when on WiFi due to Docker bridges  
**Solution:** Skip virtual interfaces, check carrier signal  
**Impact:** Correct network type detection
