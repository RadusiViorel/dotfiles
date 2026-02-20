# Automount Manager TUI

Terminal User Interface for managing automount devices, inspired by wiremix.

## Features

- **Interactive device list** with status indicators
- **Keyboard-driven navigation** (arrow keys, vim keys, shortcuts)
- **Real-time mount status** (● MOUNTED / ○ UNMOUNTED)
- **Tab-based filtering** (All / NEW / Approved devices)
- **Inline device editing** (GROUP, LABEL, auto-mount)
- **Quick actions** (mount, unmount, delete, rescan)
- **Color-coded interface** with clear visual hierarchy

## Launch

**From xmobar:**
- Click the disk icon in xmobar

**From terminal:**
```bash
~/.config/_scripts/startup/automount/automount-tui.sh
```

**From anywhere:**
```bash
automount-tui  # (if you create an alias)
```

## Keyboard Shortcuts

### Navigation
- `↑` / `k` - Move up
- `↓` / `j` - Move down
- `Tab` - Toggle view (All → NEW → Approved → All)

### Actions
- `Enter` / `e` - Edit selected device (GROUP, LABEL, auto-mount)
- `m` - Mount selected device
- `u` - Unmount selected device
- `d` - Delete selected device from config
- `r` - Rescan for new devices
- `q` - Quit

## Interface Layout

```
╔══════════════════════════════════════════════════════════════════════════════╗
║                          AUTOMOUNT MANAGER - TUI                            ║
║  [Tab] Toggle View  [Enter] Edit  [m] Mount  [u] Unmount  [d] Delete  [q] Quit  ║
╠══════════════════════════════════════════════════════════════════════════════╣
IDX  STATUS       GROUP                LABEL           FSTYPE   MOUNT    NEW       
────────────────────────────────────────────────────────────────────────────────
▶ 0  ○ UNMOUNTED  [__new]              home            ntfs     yes      yes       
     Device: /dev/sda1
     Path:   /devices/__new/home
     UUID:   7EB4E172B4E12D79
  1  ● MOUNTED    [desktop-pc]         personal-files  ext4     yes      no        
────────────────────────────────────────────────────────────────────────────────
Total devices: 2
```

## Status Indicators

- `○ UNMOUNTED` - Device is not currently mounted (dimmed)
- `● MOUNTED` - Device is currently mounted (green)
- `▶` - Currently selected device

## Color Coding

- **Cyan** - Selected device / headers
- **Green** - Mounted status / success messages
- **Yellow** - Warnings / prompts
- **Red** - Error messages / delete confirmations
- **Dim** - Secondary information

## Editing Devices

When you press `Enter` or `e`:

1. **GROUP** - Enter new group name (e.g., "desktop", "laptop", "server")
   - Press Enter to keep current value
   
2. **LABEL** - Enter new label (e.g., "windows", "data", "backup")
   - Press Enter to keep current value
   
3. **Auto-mount** - Press `y` for yes, `n` for no
   - Press any other key to keep current value

4. **Confirmation** - Press `y` to save, `n` to discard

**Note:** Changing GROUP or LABEL automatically sets NEW=no (approves the device)

## View Filters (Tab key)

- **All** - Show all devices
- **NEW** - Show only NEW=yes devices (unapproved)
- **Approved** - Show only NEW=no devices (approved)

## Workflow Example

### Approve a NEW device:

1. Launch TUI
2. Press `Tab` to switch to "NEW" view
3. Use `↑`/`↓` to select device
4. Press `e` to edit
5. Enter GROUP: `laptop`
6. Enter LABEL: `windows`
7. Auto-mount: `y`
8. Confirm: `y`
9. Device is now approved and will mount to `/devices/laptop/windows`

### Mount all devices:

1. Launch automount script: `~/.config/_scripts/startup/automount/automount.sh mount`
   
   OR

2. In TUI, manually mount each device with `m`

## Tips

- Use `r` to rescan after connecting new devices
- The selected device shows full details (device path, mount path, UUID)
- Changes are saved immediately to the config file
- The TUI respects the same config as the EWW GUI

## Technical Details

**Script:** `~/.config/_scripts/startup/automount/automount-tui.sh`
**Config:** `~/.config/_scripts/startup/automount/automount.conf`
**Mount Root:** `/devices/{GROUP}/{LABEL}`

## Comparison: TUI vs EWW GUI

| Feature | TUI | EWW GUI |
|---------|-----|---------|
| Launch speed | ⚡ Instant | Medium |
| Resource usage | Minimal | Higher |
| Keyboard driven | ✓ Full support | Partial |
| Mouse support | ✗ None | ✓ Full support |
| Visual polish | Simple, functional | Styled, modern |
| Pending changes | Immediate save | Staged with undo |
| Search | Filter by view | Full-text search |

Both versions use the same backend scripts and config file, so you can switch between them freely!
