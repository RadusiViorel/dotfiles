# Update Logs

This directory contains logs of all package updates performed through xmobar.

## Files

- **updates.log** - Master log file containing all update sessions
- **session_YYYYMMDD_HHMMSS.log** - Individual session logs with detailed package information

## Log Format

Each update session is logged with:
- Timestamp (YYYY-MM-DD HH:MM:SS)
- Number of packages updated
- List of packages that were updated

## Usage

**Left-click** on the updates indicator in xmobar to run updates
**Middle-click** (button 3) on the updates indicator to view the update history

## Log Retention

Logs are kept indefinitely. To clean old logs:
```bash
# Remove logs older than 30 days
find ~/.config/xmobar/com/updates/logs -name "session_*.log" -mtime +30 -delete
```
