# Automount Script - User Guide

## Overview

This script automatically discovers and mounts external storage devices across different PCs using UUID-based identification.

## Key Features

- **Portable**: Devices identified by UUID, not device path
- **Auto-discovery**: New devices automatically detected and mounted to `/devices/__new/`
- **Configurable**: Easy-to-edit config file to customize labels and mounting behavior
- **Safe**: Skips system partitions (root, home, boot, swap)

## Directory Structure

```
/devices/                      # Main mount point
├── __new/                     # New devices mount here first
│   ├── auto-sda1/             # Example: newly discovered device
│   └── auto-nvme0n1p5/        # Example: another new device
├── my-data/                   # Example: configured device
├── backup-drive/              # Example: configured device
└── windows-home/              # Example: configured device
```

## First-Time Setup

1. **Add doas permissions** (as root):
   ```bash
   su -
   cat >> /etc/doas.conf << 'EOF'
   permit nopass :void cmd /usr/sbin/mount
   permit nopass :void cmd /usr/sbin/umount
   permit nopass :void cmd /usr/bin/mkdir
   EOF
   doas -C /etc/doas.conf  # Verify syntax
   ```

2. **First run** (discovers all your current devices):
   ```bash
   ~/.config/_scripts/startup/automount/automount.sh mount
   ```

3. **Check what was discovered**:
   ```bash
   ~/.config/_scripts/startup/automount/automount.sh list
   ```

4. **Edit the config** to customize:
   ```bash
   nano ~/.config/_scripts/startup/automount/automount.conf
   ```

## New Device Discovery Workflow

When a new device is discovered:

1. **Script finds it** and mounts to `/devices/__new/{auto-label}`
2. **Config entry is created** with `NEW=yes`:
   ```ini
   [d263e0b2-7e50-4d98-b007-078cd4cc63c4]
   DEVICE=/dev/nvme0n1p5
   LABEL=auto-nvme0n1p5
   MOUNT=yes
   FSTYPE=ext4
   NEW=yes
   ```

3. **You edit the config** to rename and approve:
   ```ini
   [d263e0b2-7e50-4d98-b007-078cd4cc63c4]
   DEVICE=/dev/nvme0n1p5
   LABEL=my-important-data      # Changed from auto-nvme0n1p5
   MOUNT=yes
   FSTYPE=ext4
   NEW=no                       # Changed from yes
   ```

4. **Next boot/mount** it appears at `/devices/my-important-data/`

## Configuration File

Location: `~/.config/_scripts/startup/automount/automount.conf`

### Fields

- **UUID**: Unique device identifier (section header)
- **DEVICE**: Current device path (e.g., `/dev/sda1`) - auto-updated
- **LABEL**: Mount directory name
- **MOUNT**: `yes` to auto-mount, `no` to skip
- **FSTYPE**: Filesystem type (informational)
- **NEW**: `yes` mounts to `__new/`, `no` mounts normally

### Example Config

```ini
# Known and approved device
[7EB4E172B4E12D79]
DEVICE=/dev/sda1
LABEL=windows-home
MOUNT=yes
FSTYPE=ntfs
NEW=no

# New device (not yet renamed)
[884e4691-4638-41e9-a972-fbf2a43433da]
DEVICE=/dev/sdc1
LABEL=auto-sdc1
MOUNT=yes
FSTYPE=ext4
NEW=yes

# Disabled device (won't mount)
[a0769c4f-88d4-4145-9f1c-e9086c9130fa]
DEVICE=/dev/nvme0n1p3
LABEL=old-backup
MOUNT=no
FSTYPE=btrfs
NEW=no
```

## Commands

### Mount all enabled devices
```bash
~/.config/_scripts/startup/automount/automount.sh mount
```

### List all devices and status
```bash
~/.config/_scripts/startup/automount/automount.sh list
```

### Unmount a specific device
```bash
~/.config/_scripts/startup/automount/automount.sh unmount <label>
```
Examples:
```bash
~/.config/_scripts/startup/automount/automount.sh unmount my-data
~/.config/_scripts/startup/automount/automount.sh unmount auto-sda1
```

### Rescan without mounting
```bash
~/.config/_scripts/startup/automount/automount.sh rescan
```

## Common Workflows

### I found a device in __new/ and want to keep it

1. Check what's mounted:
   ```bash
   ls /devices/__new/
   ```

2. Find the device in config:
   ```bash
   cat ~/.config/_scripts/startup/automount/automount.conf
   ```

3. Edit the config:
   - Change `LABEL` to something meaningful
   - Change `NEW=yes` to `NEW=no`

4. Unmount and remount:
   ```bash
   ~/.config/_scripts/startup/automount/automount.sh unmount <old-label>
   ~/.config/_scripts/startup/automount/automount.sh mount
   ```

### I don't want to mount a device anymore

1. Edit config:
   ```bash
   nano ~/.config/_scripts/startup/automount/automount.conf
   ```

2. Find the device section and change:
   ```ini
   MOUNT=yes   →   MOUNT=no
   ```

3. Unmount if currently mounted:
   ```bash
   ~/.config/_scripts/startup/automount/automount.sh unmount <label>
   ```

### Moving to a different PC

Just boot with your NVME enclosure attached. The script will:
- Recognize all devices by UUID
- Update DEVICE paths in config
- Mount to the same `/devices/{LABEL}/` directories

## Automatic Mounting on Boot

Already configured in `~/.xinitrc`:
```bash
$HOME/.config/_scripts/startup/automount/automount.sh mount &
```

This runs automatically when you log in to X11.

## Mount Options

The script uses safe defaults for different filesystems:

- **ext4/btrfs/xfs**: `defaults,noatime`
- **ntfs**: `defaults,uid=1000,gid=1000,dmask=022,fmask=133`
- **vfat/exfat**: `defaults,uid=1000,gid=1000,dmask=022,fmask=133,utf8`

## Troubleshooting

### Script says "Operation not permitted"
- Make sure doas config is properly set up (see First-Time Setup)
- Verify with: `doas mount --help`

### Device not showing up
- Check if it has a filesystem: `lsblk -f`
- Devices without UUID are skipped (whole disks, unformatted partitions)

### Device mounted to wrong location
- Check the `NEW` field in config
- If `NEW=yes`, it mounts to `/devices/__new/{label}`
- If `NEW=no`, it mounts to `/devices/{label}`

### Want to see what would be mounted without actually mounting
```bash
~/.config/_scripts/startup/automount/automount.sh rescan
~/.config/_scripts/startup/automount/automount.sh list
```
