#!/bin/bash

# automount-tui.sh - Terminal UI for automount manager
# Similar interface style to wiremix

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="${SCRIPT_DIR}/automount.conf"
MOUNT_ROOT="/devices"

# Get terminal dimensions
TERM_WIDTH=$(tput cols)
TERM_HEIGHT=$(tput lines)

# Box dimensions (use full width)
BOX_WIDTH=$TERM_WIDTH
CONTENT_WIDTH=$((BOX_WIDTH - 2))  # Minus 2 for left and right borders

# Colors
C_RESET='\033[0m'
C_BOLD='\033[1m'
C_DIM='\033[2m'
C_RED='\033[0;31m'
C_GREEN='\033[0;32m'
C_YELLOW='\033[1;33m'
C_BLUE='\033[0;34m'
C_CYAN='\033[0;36m'
C_MAGENTA='\033[0;35m'
C_WHITE='\033[1;37m'

# Terminal dimensions
COLS=$(tput cols)
LINES=$(tput lines)

# Parse config and load into associative arrays
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

# Check if device is mounted
is_mounted() {
    local uuid="$1"
    local group="${CONFIG_GROUP[$uuid]}"
    local label="${CONFIG_LABEL[$uuid]}"
    local mount_path="${MOUNT_ROOT}/${group}/${label}"
    
    mountpoint -q "$mount_path" 2>/dev/null
}

# Get device size (human readable)
get_device_size() {
    local device="$1"
    if [[ -b "$device" ]]; then
        lsblk -dn -o SIZE "$device" 2>/dev/null | tr -d ' '
    else
        echo "N/A"
    fi
}

# Draw header
draw_header() {
    clear
    
    # Top border
    printf "${C_CYAN}%s${C_RESET}\n" "$(printf '%.s═' $(seq 1 $BOX_WIDTH))"
    
    # Title (centered)
    local title="AUTOMOUNT MANAGER - TUI"
    local title_padding=$(( (BOX_WIDTH - ${#title}) / 2 ))
    printf "%*s%s%*s\n" \
        $title_padding "" "$title" $((BOX_WIDTH - title_padding - ${#title})) ""
    
    # Help text
    local help="[Enter/o] Open  [e] Edit  [m] Mount  [u] Unmount  [d] Delete  [r] Rescan  [c] Config  [/] Filter  [q] Quit"
    printf "${C_DIM}%-*s${C_RESET}\n" $BOX_WIDTH "$help"
    
    # Separator
    printf "${C_CYAN}%s${C_RESET}\n" "$(printf '%.s═' $(seq 1 $BOX_WIDTH))"
}

# Draw device list
draw_devices() {
    local filter="${1:-all}"  # all, new, approved
    local selected="${2:-0}"
    local search_filter="${3:-}"  # Search string filter
    
    draw_header
    
    # Show search filter if active
    if [[ -n "$search_filter" ]]; then
        printf "${C_YELLOW}Filter: /%s${C_RESET} ${C_DIM}(ESC to clear)${C_RESET}\n" "$search_filter"
    fi
    
    local count=0
    local idx=0
    
    # Calculate column widths
    # Format: selector(1) + status(12) + space + device(15) + space + space(10) + space + group(18) + space + label(20) + space + uuid(36) + space + fstype(8) + space + new
    # Fixed parts: selector(1) + status(12) + device(15) + space(10) + group(18) + label(20) + uuid(36) + fstype(8) + 7 spaces = 127
    # So: new = BOX_WIDTH - 127
    local col_status=12
    local col_device=15
    local col_space=10
    local col_group=18
    local col_label=20
    local col_uuid=36
    local col_fstype=8
    local fixed=$((1 + 12 + 1 + 15 + 1 + 10 + 1 + 18 + 1 + 20 + 1 + 36 + 1 + 8))  # selector + columns + 7 spaces
    local col_new=$((BOX_WIDTH - fixed))  # Remaining space for new column
    
    # Table header
    printf "${C_BOLD}%-${col_status}s %-${col_device}s %-${col_space}s %-${col_group}s %-${col_label}s %-${col_uuid}s %-${col_fstype}s %-${col_new}s${C_RESET}\n" \
        "STATUS" "DEVICE" "SPACE" "GROUP" "LABEL" "UUID" "FSTYPE" "NEW"
    printf "${C_CYAN}%s${C_RESET}\n" "$(printf '%.s═' $(seq 1 $BOX_WIDTH))"
    
    # Get list of currently connected devices (UUIDs from lsblk)
    local connected_uuids=()
    while IFS= read -r line; do
        eval "$line"
        [[ -n "$UUID" ]] && connected_uuids+=("$UUID")
    done < <(lsblk -npo UUID -P | grep -v 'UUID=""')
    
    # Sort devices by GROUP name, but only show connected ones
    local sorted_uuids=()
    while IFS= read -r uuid; do
        sorted_uuids+=("$uuid")
    done < <(
        for uuid in "${!CONFIG_LABEL[@]}"; do
            # Check if this UUID is in the connected devices list
            local is_connected=false
            for connected in "${connected_uuids[@]}"; do
                if [[ "$uuid" == "$connected" ]]; then
                    is_connected=true
                    break
                fi
            done
            
            # Only include if connected
            if [[ "$is_connected" == true ]]; then
                echo "${CONFIG_GROUP[$uuid]}|$uuid"
            fi
        done | sort -t'|' -k1,1 | cut -d'|' -f2
    )
    
    for uuid in "${sorted_uuids[@]}"; do
        local group="${CONFIG_GROUP[$uuid]}"
        local label="${CONFIG_LABEL[$uuid]}"
        local mount="${CONFIG_MOUNT[$uuid]}"
        local fstype="${CONFIG_FSTYPE[$uuid]}"
        local is_new="${CONFIG_NEW[$uuid]}"
        local device="${CONFIG_DEVICE[$uuid]}"
        
        # Filter
        if [[ "$filter" == "new" && "$is_new" != "yes" ]]; then
            continue
        fi
        if [[ "$filter" == "approved" && "$is_new" != "no" ]]; then
            continue
        fi
        
        # Search filter (case-insensitive, matches GROUP, LABEL, DEVICE, or UUID)
        if [[ -n "$search_filter" ]]; then
            local search_lower="${search_filter,,}"
            local matches=false
            
            [[ "${group,,}" == *"$search_lower"* ]] && matches=true
            [[ "${label,,}" == *"$search_lower"* ]] && matches=true
            [[ "${device,,}" == *"$search_lower"* ]] && matches=true
            [[ "${uuid,,}" == *"$search_lower"* ]] && matches=true
            
            [[ "$matches" == false ]] && continue
        fi
        
        # Status (pad to fixed width for alignment)
        local status_icon="○"
        local status_color="$C_DIM"
        local status_text="UNMOUNTED"
        if is_mounted "$uuid"; then
            status_icon="●"
            status_color="$C_GREEN"
            status_text="MOUNTED  "  # Pad to match UNMOUNTED length
        fi
        
        # Highlight selected
        local line_color="$C_RESET"
        local selector=" "
        if [[ $idx -eq $selected ]]; then
            line_color="$C_CYAN"
            selector="▶"
        fi
        
        # Truncate long values to fit columns
        local group_short="$group"
        local label_short="$label"
        local device_short="$device"
        [[ ${#group} -gt $((col_group - 2)) ]] && group_short="${group:0:$((col_group - 5))}..."
        [[ ${#label} -gt $col_label ]] && label_short="${label:0:$((col_label - 3))}..."
        [[ ${#device} -gt $col_device ]] && device_short="${device:0:$((col_device - 3))}..."
        
        # Get device size
        local device_size=$(get_device_size "$device")
        
        # Print device line
        printf "${selector}${status_color}%-${col_status}s${line_color} %-${col_device}s %-${col_space}s %-${col_group}s %-${col_label}s %-${col_uuid}s %-${col_fstype}s %-${col_new}s${C_RESET}\n" \
            "$status_icon $status_text" \
            "$device_short" \
            "$device_size" \
            "[$group_short]" \
            "$label_short" \
            "${uuid:0:36}" \
            "$fstype" \
            "$is_new"
        
        ((idx++))
    done
    
    printf "${C_CYAN}%s${C_RESET}\n" "$(printf '%.s═' $(seq 1 $BOX_WIDTH))"
    local footer_text="Total devices: $idx"
    local footer_padding=$((BOX_WIDTH - ${#footer_text}))
    printf "${C_DIM}%s%*s${C_RESET}\n" "$footer_text" $footer_padding ""
    printf "${C_CYAN}%s${C_RESET}\n" "$(printf '%.s═' $(seq 1 $BOX_WIDTH))"
}

# Interactive menu
interactive_menu() {
    load_config
    
    local filter="all"
    local selected=0
    local search_filter=""
    
    while true; do
        # Recalculate total devices based on current filter and search
        local total_devices=$(get_filtered_device_count "$filter" "$search_filter")
        
        draw_devices "$filter" "$selected" "$search_filter"
        
        # Read single key
        read -rsn1 key < /dev/tty
        
        case "$key" in
            q|Q)
                clear
                exit 0
                ;;
            $'\x1b')  # Escape sequence
                read -rsn2 -t 0.1 rest
                key="$key$rest"
                case "$key" in
                    $'\x1b[A')  # Up arrow
                        ((selected > 0)) && ((selected--))
                        ;;
                    $'\x1b[B')  # Down arrow
                        ((selected < total_devices - 1)) && ((selected++))
                        ;;
                    $'\x1b')  # ESC key alone - clear search filter
                        search_filter=""
                        selected=0
                        ;;
                esac
                ;;
            /)  # Start search filter
                draw_devices "$filter" "$selected" "$search_filter"
                echo
                echo -n "Filter: /"
                read -r search_input < /dev/tty
                search_filter="$search_input"
                selected=0
                ;;
            j)  # Vim down
                ((selected < total_devices - 1)) && ((selected++))
                ;;
            k)  # Vim up
                ((selected > 0)) && ((selected--))
                ;;
            m|M)  # Mount
                mount_selected "$selected" "$filter" "$search_filter"
                load_config
                ;;
            u|U)  # Unmount
                unmount_selected "$selected" "$filter" "$search_filter"
                load_config
                ;;
            '')  # Enter key (shows as empty string with -n1)
                open_selected "$selected" "$filter" "$search_filter"
                ;;
            o|O)  # Open in file explorer
                open_selected "$selected" "$filter" "$search_filter"
                ;;
            e|E)  # Edit device config
                edit_selected "$selected" "$filter" "$search_filter"
                load_config
                ;;
            d|D)  # Delete
                delete_selected "$selected" "$filter" "$search_filter"
                load_config
                selected=0
                ;;
            r|R)  # Rescan
                rescan_devices
                load_config
                search_filter=""
                ;;
            c|C)  # Open config in editor
                clear
                ${EDITOR:-vi} "$CONFIG_FILE"
                load_config
                ;;
        esac
    done
}

# Get filtered device count
get_filtered_device_count() {
    local filter="${1:-all}"
    local search_filter="${2:-}"
    
    # Get list of currently connected devices
    local connected_uuids=()
    while IFS= read -r line; do
        eval "$line"
        [[ -n "$UUID" ]] && connected_uuids+=("$UUID")
    done < <(lsblk -npo UUID -P | grep -v 'UUID=""')
    
    local count=0
    for uuid in "${!CONFIG_LABEL[@]}"; do
        # Check if connected
        local is_connected=false
        for connected in "${connected_uuids[@]}"; do
            if [[ "$uuid" == "$connected" ]]; then
                is_connected=true
                break
            fi
        done
        
        if [[ "$is_connected" == true ]]; then
            # Apply filter
            local is_new="${CONFIG_NEW[$uuid]}"
            if [[ "$filter" == "new" && "$is_new" != "yes" ]]; then
                continue
            fi
            if [[ "$filter" == "approved" && "$is_new" != "no" ]]; then
                continue
            fi
            
            # Apply search filter
            if [[ -n "$search_filter" ]]; then
                local group="${CONFIG_GROUP[$uuid]}"
                local label="${CONFIG_LABEL[$uuid]}"
                local device="${CONFIG_DEVICE[$uuid]}"
                local search_lower="${search_filter,,}"
                local matches=false
                
                [[ "${group,,}" == *"$search_lower"* ]] && matches=true
                [[ "${label,,}" == *"$search_lower"* ]] && matches=true
                [[ "${device,,}" == *"$search_lower"* ]] && matches=true
                [[ "${uuid,,}" == *"$search_lower"* ]] && matches=true
                
                [[ "$matches" == false ]] && continue
            fi
            
            ((count++))
        fi
    done
    
    echo "$count"
}

# Get UUID by index (from the sorted and filtered list)
get_uuid_by_index() {
    local idx="$1"
    local filter="${2:-all}"
    local search_filter="${3:-}"
    
    # Get list of currently connected devices
    local connected_uuids=()
    while IFS= read -r line; do
        eval "$line"
        [[ -n "$UUID" ]] && connected_uuids+=("$UUID")
    done < <(lsblk -npo UUID -P | grep -v 'UUID=""')
    
    # Sort and filter devices the same way as display
    local sorted_uuids=()
    while IFS= read -r uuid; do
        sorted_uuids+=("$uuid")
    done < <(
        for uuid in "${!CONFIG_LABEL[@]}"; do
            local is_connected=false
            for connected in "${connected_uuids[@]}"; do
                if [[ "$uuid" == "$connected" ]]; then
                    is_connected=true
                    break
                fi
            done
            
            if [[ "$is_connected" == true ]]; then
                # Apply filter
                local is_new="${CONFIG_NEW[$uuid]}"
                if [[ "$filter" == "new" && "$is_new" != "yes" ]]; then
                    continue
                fi
                if [[ "$filter" == "approved" && "$is_new" != "no" ]]; then
                    continue
                fi
                
                # Apply search filter
                if [[ -n "$search_filter" ]]; then
                    local group="${CONFIG_GROUP[$uuid]}"
                    local label="${CONFIG_LABEL[$uuid]}"
                    local device="${CONFIG_DEVICE[$uuid]}"
                    local search_lower="${search_filter,,}"
                    local matches=false
                    
                    [[ "${group,,}" == *"$search_lower"* ]] && matches=true
                    [[ "${label,,}" == *"$search_lower"* ]] && matches=true
                    [[ "${device,,}" == *"$search_lower"* ]] && matches=true
                    [[ "${uuid,,}" == *"$search_lower"* ]] && matches=true
                    
                    [[ "$matches" == false ]] && continue
                fi
                
                echo "${CONFIG_GROUP[$uuid]}|$uuid"
            fi
        done | sort -t'|' -k1,1 | cut -d'|' -f2
    )
    
    # Return the UUID at the requested index
    if [[ $idx -lt ${#sorted_uuids[@]} ]]; then
        echo "${sorted_uuids[$idx]}"
    fi
}

# Mount selected device
mount_selected() {
    local idx="$1"
    local filter="$2"
    local search_filter="${3:-}"
    local uuid=$(get_uuid_by_index "$idx" "$filter" "$search_filter")
    [[ -z "$uuid" ]] && return
    
    local group="${CONFIG_GROUP[$uuid]}"
    local label="${CONFIG_LABEL[$uuid]}"
    local fstype="${CONFIG_FSTYPE[$uuid]}"
    local mount_path="${MOUNT_ROOT}/${group}/${label}"
    
    # Create mount point
    [[ ! -d "$mount_path" ]] && /usr/sbin/doas /usr/bin/mkdir -p "$mount_path"
    
    # Mount options
    local mount_opts
    case "$fstype" in
        ext4|btrfs|xfs)
            mount_opts="defaults,noatime"
            ;;
        ntfs)
            mount_opts="defaults,uid=1000,gid=1000,dmask=022,fmask=133"
            ;;
        vfat|exfat)
            mount_opts="defaults,uid=1000,gid=1000,dmask=022,fmask=133,utf8"
            ;;
        *)
            mount_opts="defaults"
            ;;
    esac
    
    echo -e "\n${C_YELLOW}Mounting: [$group] $label...${C_RESET}"
    if /usr/sbin/doas /usr/sbin/mount -U "$uuid" -o "$mount_opts" "$mount_path" 2>/dev/null; then
        echo -e "${C_GREEN}✓ Mounted successfully${C_RESET}"
    else
        echo -e "${C_RED}✗ Failed to mount${C_RESET}"
    fi
    echo -e "${C_DIM}Press any key to continue...${C_RESET}"
    read -rsn1 < /dev/tty
}

# Unmount selected device
unmount_selected() {
    local idx="$1"
    local filter="$2"
    local search_filter="${3:-}"
    local uuid=$(get_uuid_by_index "$idx" "$filter" "$search_filter")
    [[ -z "$uuid" ]] && return
    
    local group="${CONFIG_GROUP[$uuid]}"
    local label="${CONFIG_LABEL[$uuid]}"
    local mount_path="${MOUNT_ROOT}/${group}/${label}"
    
    echo -e "\n${C_YELLOW}Unmounting: [$group] $label...${C_RESET}"
    if /usr/sbin/doas /usr/sbin/umount "$mount_path" 2>/dev/null; then
        echo -e "${C_GREEN}✓ Unmounted successfully${C_RESET}"
    else
        echo -e "${C_RED}✗ Failed to unmount${C_RESET}"
    fi
    echo -e "${C_DIM}Press any key to continue...${C_RESET}"
    read -rsn1 < /dev/tty
}

# Open selected device in file explorer
open_selected() {
    local idx="$1"
    local filter="$2"
    local search_filter="${3:-}"
    local uuid=$(get_uuid_by_index "$idx" "$filter" "$search_filter")
    [[ -z "$uuid" ]] && return
    
    local group="${CONFIG_GROUP[$uuid]}"
    local label="${CONFIG_LABEL[$uuid]}"
    local mount_path="${MOUNT_ROOT}/${group}/${label}"
    
    # Check if mounted
    if mountpoint -q "$mount_path" 2>/dev/null; then
        clear
        lf "$mount_path"
    else
        echo
        echo -e "${C_RED}✗ Device not mounted${C_RESET}"
        echo -e "${C_DIM}Press any key to continue...${C_RESET}"
        read -rsn1 < /dev/tty
    fi
}

# Edit selected device
edit_selected() {
    local idx="$1"
    local filter="$2"
    local search_filter="${3:-}"
    local uuid=$(get_uuid_by_index "$idx" "$filter" "$search_filter")
    [[ -z "$uuid" ]] && return
    
    clear
    echo -e "${C_CYAN}╔══════════════════════════════════════════════════════════════════════════════╗${C_RESET}"
    echo -e "${C_CYAN}║                            EDIT DEVICE                                       ║${C_RESET}"
    echo -e "${C_CYAN}╚══════════════════════════════════════════════════════════════════════════════╝${C_RESET}"
    echo
    
    local group="${CONFIG_GROUP[$uuid]}"
    local label="${CONFIG_LABEL[$uuid]}"
    local mount="${CONFIG_MOUNT[$uuid]}"
    local is_new="${CONFIG_NEW[$uuid]}"
    
    echo -e "${C_WHITE}UUID:${C_RESET} $uuid"
    echo -e "${C_WHITE}Device:${C_RESET} ${CONFIG_DEVICE[$uuid]}"
    echo
    
    # Edit GROUP
    echo -e -n "${C_YELLOW}GROUP${C_RESET} [$group]: "
    read -r new_group < /dev/tty
    [[ -n "$new_group" ]] && group="$new_group"
    
    # Edit LABEL
    echo -e -n "${C_YELLOW}LABEL${C_RESET} [$label]: "
    read -r new_label < /dev/tty
    [[ -n "$new_label" ]] && label="$new_label"
    
    # Edit MOUNT
    echo -e -n "${C_YELLOW}Auto-mount?${C_RESET} [y/n, current: $mount]: "
    read -rsn1 mount_choice < /dev/tty
    echo
    case "$mount_choice" in
        y|Y) mount="yes" ;;
        n|N) mount="no" ;;
    esac
    
    # Auto-set NEW=no if GROUP or LABEL changed
    if [[ "$group" != "${CONFIG_GROUP[$uuid]}" ]] || [[ "$label" != "${CONFIG_LABEL[$uuid]}" ]]; then
        is_new="no"
        echo -e "${C_GREEN}✓ Device approved (NEW=no)${C_RESET}"
    fi
    
    # Confirm
    echo
    echo -e "${C_YELLOW}Save changes?${C_RESET} [y/n]: "
    read -rsn1 confirm < /dev/tty
    echo
    
    if [[ "$confirm" == "y" || "$confirm" == "Y" ]]; then
        CONFIG_GROUP["$uuid"]="$group"
        CONFIG_LABEL["$uuid"]="$label"
        CONFIG_MOUNT["$uuid"]="$mount"
        CONFIG_NEW["$uuid"]="$is_new"
        save_config
        echo -e "${C_GREEN}✓ Changes saved${C_RESET}"
    else
        echo -e "${C_DIM}Changes discarded${C_RESET}"
    fi
    echo -e "${C_DIM}Press any key to continue...${C_RESET}"
    read -rsn1 < /dev/tty
}

# Delete selected device
delete_selected() {
    local idx="$1"
    local filter="$2"
    local search_filter="${3:-}"
    local uuid=$(get_uuid_by_index "$idx" "$filter" "$search_filter")
    [[ -z "$uuid" ]] && return
    
    local label="${CONFIG_LABEL[$uuid]}"
    
    echo
    echo -e "${C_RED}Delete device: $label${C_RESET}"
    echo -e "${C_YELLOW}Are you sure?${C_RESET} [y/n]: "
    read -rsn1 confirm < /dev/tty
    echo
    
    if [[ "$confirm" == "y" || "$confirm" == "Y" ]]; then
        unset CONFIG_DEVICE["$uuid"]
        unset CONFIG_GROUP["$uuid"]
        unset CONFIG_LABEL["$uuid"]
        unset CONFIG_MOUNT["$uuid"]
        unset CONFIG_FSTYPE["$uuid"]
        unset CONFIG_NEW["$uuid"]
        save_config
        echo -e "${C_GREEN}✓ Device deleted${C_RESET}"
    fi
    echo -e "${C_DIM}Press any key to continue...${C_RESET}"
    read -rsn1 < /dev/tty
}

# Save config
save_config() {
    local temp_file="${CONFIG_FILE}.tmp"
    
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
    
    mv "$temp_file" "$CONFIG_FILE"
}

# Rescan devices
rescan_devices() {
    echo
    echo -e "${C_YELLOW}Rescanning devices...${C_RESET}"
    "${SCRIPT_DIR}/automount.sh" rescan
    echo -e "${C_GREEN}✓ Rescan complete${C_RESET}"
    echo -e "${C_DIM}Press any key to continue...${C_RESET}"
    read -rsn1 < /dev/tty
}

# Main
main() {
    # Check if running in a terminal
    if [[ ! -t 0 ]]; then
        echo -e "${C_RED}Error: Must run in an interactive terminal${C_RESET}"
        exit 1
    fi
    
    # Check if config exists
    if [[ ! -f "$CONFIG_FILE" ]]; then
        echo -e "${C_RED}Error: Config file not found: $CONFIG_FILE${C_RESET}"
        exit 1
    fi
    
    # Disable cursor
    tput civis
    
    # Trap to restore cursor on exit
    trap 'tput cnorm; clear' EXIT INT TERM
    
    # Run interactive menu
    interactive_menu
}

main "$@"
