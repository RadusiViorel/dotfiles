#!/bin/bash

# automount.sh - Auto-mount manager for portable Void Linux system
# Scans lsblk for devices, manages mounts via UUID-based config file

set -e

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="${SCRIPT_DIR}/automount.conf"
MOUNT_ROOT="/devices"
NEW_DEVICE_DIR="__new"

# Full paths for commands with doas
DOAS="/usr/sbin/doas"
MOUNT="$DOAS /usr/sbin/mount"
UMOUNT="$DOAS /usr/sbin/umount"
MKDIR="$DOAS /usr/bin/mkdir"
CHMOD="$DOAS /usr/sbin/chmod"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Usage information
usage() {
    cat << EOF
Usage: $0 <command> [options]

Commands:
    mount           Scan and mount all enabled devices
    unmount <label> Unmount device by label
    list            Show all configured devices and mount status
    rescan          Update config with current devices without mounting

Examples:
    $0 mount
    $0 unmount windows-home
    $0 list
EOF
    exit 1
}

# Print colored message
log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $*"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*" >&2; }

# Check if running as root
check_root() {
    if [[ $EUID -ne 0 ]] && ! command -v "$DOAS" &> /dev/null; then
        log_error "This script requires doas or root privileges"
        exit 1
    fi
}

# Create mount root directory if it doesn't exist
ensure_mount_root() {
    if [[ ! -d "$MOUNT_ROOT" ]]; then
        log_info "Creating mount root directory: $MOUNT_ROOT"
        $MKDIR -p "$MOUNT_ROOT"
        $CHMOD 755 "$MOUNT_ROOT"
    fi
    
    # Ensure __new directory exists for newly discovered devices
    if [[ ! -d "$MOUNT_ROOT/$NEW_DEVICE_DIR" ]]; then
        log_info "Creating new device directory: $MOUNT_ROOT/$NEW_DEVICE_DIR"
        $MKDIR -p "$MOUNT_ROOT/$NEW_DEVICE_DIR"
        $CHMOD 755 "$MOUNT_ROOT/$NEW_DEVICE_DIR"
    fi
}

# Create initial config file if it doesn't exist
ensure_config() {
    if [[ ! -f "$CONFIG_FILE" ]]; then
        log_info "Creating initial config file: $CONFIG_FILE"
        cat > "$CONFIG_FILE" << 'EOF'
# Auto-mount Configuration
# This file is auto-updated when new devices are discovered
#
# Format:
#   [UUID]
#   DEVICE=/dev/sdXN     # Current device path (updated each scan)
#   GROUP=pc-name        # Group/PC name (e.g., desktop, laptop, __new)
#   LABEL=mount-name     # Directory name in /devices/{GROUP}/
#   MOUNT=yes|no         # Whether to auto-mount
#   FSTYPE=ext4          # Filesystem type
#   NEW=yes|no           # If 'yes', uses GROUP=__new
#
# New devices are discovered with NEW=yes and GROUP=__new, mounting to /devices/__new/{auto-label}
# Edit GROUP and LABEL to organize by PC, then set NEW=no to finalize
# To disable a device: Set MOUNT=no

EOF
    fi
}

# Sanitize label: lowercase, replace special chars with dashes
sanitize_label() {
    local label="$1"
    echo "$label" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9_-]/-/g' | sed 's/--*/-/g' | sed 's/^-//' | sed 's/-$//'
}

# Generate label for new device
generate_label() {
    local device="$1"
    local fs_label="$2"
    
    if [[ -n "$fs_label" ]]; then
        # Use existing filesystem label if present
        sanitize_label "$fs_label"
    else
        # Extract device name (e.g., nvme0n1p5 from /dev/nvme0n1p5)
        local dev_name
        dev_name=$(basename "$device")
        echo "auto-${dev_name}"
    fi
}

# Check if device is a system partition (should not be auto-mounted)
is_system_partition() {
    local uuid="$1"
    local mountpoint="$2"
    local fstype="$3"
    
    # Skip if no UUID (whole disks, etc.)
    [[ -z "$uuid" ]] && return 0
    
    # Skip swap partitions
    [[ "$fstype" == "swap" ]] && return 0
    
    # Skip if already mounted at system locations
    case "$mountpoint" in
        /|/home|/boot|/boot/efi|"[SWAP]")
            return 0
            ;;
    esac
    
    # Skip if mounted in /devices (we manage these)
    [[ "$mountpoint" == "$MOUNT_ROOT"* ]] && return 0
    
    # Not a system partition
    return 1
}

# Parse config file and load device settings into associative arrays
declare -A CONFIG_DEVICE
declare -A CONFIG_GROUP
declare -A CONFIG_LABEL
declare -A CONFIG_MOUNT
declare -A CONFIG_FSTYPE
declare -A CONFIG_NEW

load_config() {
    CONFIG_DEVICE=()
    CONFIG_GROUP=()
    CONFIG_LABEL=()
    CONFIG_MOUNT=()
    CONFIG_FSTYPE=()
    CONFIG_NEW=()
    
    [[ ! -f "$CONFIG_FILE" ]] && return
    
    local current_uuid=""
    while IFS= read -r line; do
        # Skip comments and empty lines
        [[ "$line" =~ ^[[:space:]]*# ]] && continue
        [[ -z "${line// /}" ]] && continue
        
        # Section header [UUID]
        if [[ "$line" =~ ^\[([^]]+)\] ]]; then
            current_uuid="${BASH_REMATCH[1]}"
            continue
        fi
        
        # Key=Value pairs
        if [[ "$line" =~ ^([A-Z]+)=(.+)$ ]]; then
            local key="${BASH_REMATCH[1]}"
            local value="${BASH_REMATCH[2]}"
            
            case "$key" in
                DEVICE) CONFIG_DEVICE["$current_uuid"]="$value" ;;
                GROUP)  CONFIG_GROUP["$current_uuid"]="$value" ;;
                LABEL)  CONFIG_LABEL["$current_uuid"]="$value" ;;
                MOUNT)  CONFIG_MOUNT["$current_uuid"]="$value" ;;
                FSTYPE) CONFIG_FSTYPE["$current_uuid"]="$value" ;;
                NEW)    CONFIG_NEW["$current_uuid"]="$value" ;;
            esac
        fi
    done < "$CONFIG_FILE"
}

# Save config file
save_config() {
    local temp_file="${CONFIG_FILE}.tmp"
    
    # Write header
    cat > "$temp_file" << 'EOF'
# Auto-mount Configuration
# This file is auto-updated when new devices are discovered
#
# Format:
#   [UUID]
#   DEVICE=/dev/sdXN     # Current device path (updated each scan)
#   GROUP=pc-name        # Group/PC name (e.g., desktop, laptop, __new)
#   LABEL=mount-name     # Directory name in /devices/{GROUP}/
#   MOUNT=yes|no         # Whether to auto-mount
#   FSTYPE=ext4          # Filesystem type
#   NEW=yes|no           # If 'yes', uses GROUP=__new
#
# New devices are discovered with NEW=yes and GROUP=__new, mounting to /devices/__new/{auto-label}
# Edit GROUP and LABEL to organize by PC, then set NEW=no to finalize
# To disable a device: Set MOUNT=no

EOF
    
    # Write each device section
    for uuid in "${!CONFIG_LABEL[@]}"; do
        echo "" >> "$temp_file"
        echo "[$uuid]" >> "$temp_file"
        echo "DEVICE=${CONFIG_DEVICE[$uuid]}" >> "$temp_file"
        echo "GROUP=${CONFIG_GROUP[$uuid]}" >> "$temp_file"
        echo "LABEL=${CONFIG_LABEL[$uuid]}" >> "$temp_file"
        echo "MOUNT=${CONFIG_MOUNT[$uuid]}" >> "$temp_file"
        echo "FSTYPE=${CONFIG_FSTYPE[$uuid]}" >> "$temp_file"
        echo "NEW=${CONFIG_NEW[$uuid]}" >> "$temp_file"
    done
    
    # Atomic replace
    mv "$temp_file" "$CONFIG_FILE"
}

# Get mount options based on filesystem type
get_mount_options() {
    local fstype="$1"
    
    case "$fstype" in
        ext4|btrfs|xfs)
            echo "defaults,noatime"
            ;;
        ntfs)
            echo "defaults,uid=1000,gid=1000,dmask=022,fmask=133"
            ;;
        vfat|exfat)
            echo "defaults,uid=1000,gid=1000,dmask=022,fmask=133,utf8"
            ;;
        *)
            echo "defaults"
            ;;
    esac
}

# Mount a device by UUID
mount_device() {
    local uuid="$1"
    local group="${CONFIG_GROUP[$uuid]}"
    local label="${CONFIG_LABEL[$uuid]}"
    local fstype="${CONFIG_FSTYPE[$uuid]}"
    local is_new="${CONFIG_NEW[$uuid]}"
    local mount_point
    
    # Determine mount point: /devices/{GROUP}/{LABEL}
    mount_point="${MOUNT_ROOT}/${group}/${label}"
    
    # Create mount point if it doesn't exist
    if [[ ! -d "$mount_point" ]]; then
        $MKDIR -p "$mount_point"
    fi
    
    # Check if already mounted
    if mountpoint -q "$mount_point" 2>/dev/null; then
        log_warn "Already mounted: $label at $mount_point"
        return 0
    fi
    
    # Get mount options
    local mount_opts
    mount_opts=$(get_mount_options "$fstype")
    
    # Mount the device
    if [[ "$is_new" == "yes" ]]; then
        log_info "Mounting NEW device: $label (UUID=$uuid) -> $mount_point"
        log_warn "Remember to edit GROUP, LABEL and set NEW=no to finalize"
    else
        log_info "Mounting: [$group] $label (UUID=$uuid) -> $mount_point"
    fi
    
    if $MOUNT -U "$uuid" -o "$mount_opts" "$mount_point" 2>/dev/null; then
        log_success "Mounted: $label"
        return 0
    else
        log_error "Failed to mount: $label (UUID=$uuid)"
        return 1
    fi
}

# Unmount a device by label (searches all groups)
unmount_device() {
    local label="$1"
    
    # Search for mounted device in any group directory
    for group_dir in "${MOUNT_ROOT}"/*; do
        [[ ! -d "$group_dir" ]] && continue
        
        local mount_point="${group_dir}/${label}"
        
        if mountpoint -q "$mount_point" 2>/dev/null; then
            log_info "Unmounting: $label from $mount_point"
            if $UMOUNT "$mount_point" 2>/dev/null; then
                log_success "Unmounted: $label"
                return 0
            else
                log_error "Failed to unmount: $label"
                return 1
            fi
        fi
    done
    
    log_warn "Not mounted: $label"
    return 1
}

# Scan devices and mount according to config
scan_and_mount() {
    local do_mount="${1:-true}"
    
    ensure_mount_root
    ensure_config
    load_config
    
    local updated=false
    
    log_info "Scanning block devices..."
    
    # Parse lsblk output
    while IFS= read -r line; do
        # Parse lsblk -P output
        eval "$line"
        
        # Skip system partitions
        if is_system_partition "$UUID" "$MOUNTPOINT" "$FSTYPE"; then
            continue
        fi
        
        # Check if UUID is in config
        if [[ -n "${CONFIG_LABEL[$UUID]}" ]]; then
            # Known device - update DEVICE path
            if [[ "${CONFIG_DEVICE[$UUID]}" != "$NAME" ]]; then
                CONFIG_DEVICE["$UUID"]="$NAME"
                updated=true
            fi
            
            # Mount if enabled
            if [[ "${CONFIG_MOUNT[$UUID]}" == "yes" ]] && [[ "$do_mount" == "true" ]]; then
                mount_device "$UUID" || true
            fi
        else
            # Unknown device - add to config and mount to __new directory
            local new_label
            new_label=$(generate_label "$NAME" "$LABEL")
            
            log_info "New device discovered: $NAME (UUID=$UUID)"
            log_info "Will mount to /devices/__new/${new_label}"
            log_info "Edit config file to set GROUP, rename LABEL, and set NEW=no"
            
            CONFIG_DEVICE["$UUID"]="$NAME"
            CONFIG_GROUP["$UUID"]="__new"
            CONFIG_LABEL["$UUID"]="$new_label"
            CONFIG_MOUNT["$UUID"]="yes"
            CONFIG_FSTYPE["$UUID"]="$FSTYPE"
            CONFIG_NEW["$UUID"]="yes"
            updated=true
            
            # Mount new device
            if [[ "$do_mount" == "true" ]]; then
                mount_device "$UUID" || true
            fi
        fi
        
    done < <(lsblk -npo NAME,UUID,FSTYPE,LABEL,MOUNTPOINT -P | grep -v 'UUID=""')
    
    # Save config if updated
    if [[ "$updated" == "true" ]]; then
        save_config
        log_success "Configuration updated"
    fi
}

# List all configured devices and their status
list_devices() {
    ensure_config
    load_config
    
    echo ""
    printf "%-36s %-15s %-20s %-6s %-6s %-15s\n" "UUID" "DEVICE" "LABEL" "MOUNT" "NEW" "STATUS"
    printf "%.s=" {1..105}
    echo ""
    
    for uuid in "${!CONFIG_LABEL[@]}"; do
        local device="${CONFIG_DEVICE[$uuid]}"
        local label="${CONFIG_LABEL[$uuid]}"
        local mount="${CONFIG_MOUNT[$uuid]}"
        local is_new="${CONFIG_NEW[$uuid]}"
        local group="${CONFIG_GROUP[$uuid]}"
        local mount_point
        local status="unmounted"
        
        # Determine actual mount point
        mount_point="${MOUNT_ROOT}/${group}/${label}"
        
        if mountpoint -q "$mount_point" 2>/dev/null; then
            status="${GREEN}mounted${NC}"
        elif [[ "$mount" == "no" ]]; then
            status="${YELLOW}disabled${NC}"
        fi
        
        printf "%-36s %-15s %-20s %-6s %-6s " "$uuid" "$device" "$label" "$mount" "$is_new"
        echo -e "$status"
    done
    
    echo ""
}

# Main command dispatcher
main() {
    case "${1:-}" in
        mount)
            scan_and_mount "true"
            ;;
        unmount)
            if [[ -z "${2:-}" ]]; then
                log_error "Please specify a label to unmount"
                usage
            fi
            unmount_device "$2"
            ;;
        list)
            list_devices
            ;;
        rescan)
            scan_and_mount "false"
            ;;
        *)
            usage
            ;;
    esac
}

main "$@"
