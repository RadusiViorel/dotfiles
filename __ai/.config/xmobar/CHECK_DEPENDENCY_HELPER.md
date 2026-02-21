# Dependency Check Helper

## Overview

A reusable helper function to check for command dependencies in xmobar components, reducing code duplication and standardizing error messages.

## Location

`~/.config/xmobar/scripts/check_dependency`

## Usage

### Basic Syntax

```bash
source "${HOME}/.config/xmobar/scripts/check_dependency"
check_dependency "<command>" [--exit|--no-exit]
```

### Parameters

1. **command** (required): The command to check for (e.g., `xset`, `acpi`, `bluetoothctl`)
2. **exit behavior** (optional): 
   - `--exit` (default): Exit with status 1 if command not found
   - `--no-exit`: Continue execution, just print warning

### Examples

#### Example 1: Check with Exit (Default)

```bash
#!/usr/bin/env bash

source "${HOME}/.config/xmobar/scripts/check_dependency"

# Will exit the script if xset is not found
check_dependency "xset" --exit

# Rest of your code...
xset q | grep "Caps Lock"
```

#### Example 2: Check with Exit (Implicit)

```bash
#!/usr/bin/env bash

source "${HOME}/.config/xmobar/scripts/check_dependency"

# --exit is the default, so you can omit it
check_dependency "acpi"

# Rest of your code...
acpi -b
```

#### Example 3: Check Without Exit

```bash
#!/usr/bin/env bash

source "${HOME}/.config/xmobar/scripts/check_dependency"

# Print warning but continue execution
if ! check_dependency "optional_tool" --no-exit; then
  echo "Using fallback method..."
  # Use alternative approach
else
  # Use the preferred tool
  optional_tool --do-something
fi
```

#### Example 4: Multiple Dependencies

```bash
#!/usr/bin/env bash

source "${HOME}/.config/xmobar/scripts/check_dependency"

# Check multiple required dependencies
check_dependency "bluetoothctl"
check_dependency "upower"

# Check optional dependency
if ! check_dependency "bluetuith" --no-exit; then
  # Disable TUI feature
  has_tui=false
fi
```

## Migration

### Before

```bash
#!/usr/bin/env bash

if ! command -v xset >/dev/null 2>&1; then
  echo "Warning: xset is not installed or not in PATH."
  exit 1
fi

# ... component logic
```

### After

```bash
#!/usr/bin/env bash

source "${HOME}/.config/xmobar/scripts/check_dependency"
check_dependency "xset"

# ... component logic
```

**Lines saved:** 4 lines → 1 line

## Components Using This Helper

- `capslock` - checks for `xset`
- (More to be migrated)

## Return Values

- **0**: Command found
- **1**: Command not found

When using `--exit`, the script will terminate with exit code 1 if the command is not found.

## Benefits

1. **Consistency:** All components use the same error message format
2. **DRY:** Don't repeat the same 5-line check everywhere
3. **Flexibility:** Choose whether to exit or continue
4. **Maintainability:** Update one place to change all dependency checks
5. **Readability:** `check_dependency "xset"` is clearer than the full if statement

## Future Enhancements

Potential additions:
- Check multiple commands at once: `check_dependency "cmd1 cmd2 cmd3"`
- Package name suggestions: "Install with: sudo xbps-install -S acpi"
- Quiet mode for silent checks
- Custom error messages
