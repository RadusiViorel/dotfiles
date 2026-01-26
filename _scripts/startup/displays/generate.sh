#!/usr/bin/env bash

DIR="$HOME/.config/_scripts/startup/displays"
APPLY="$DIR/apply.sh"
CHECKSUM="$DIR/checksum"

# Get connected outputs
mapfile -t OUTPUTS < <(xrandr | grep " connected" | cut -d' ' -f1)

[ ${#OUTPUTS[@]} -eq 0 ] && exit 1

# Priority sorting
priority() {
  case "$1" in
      eDP*)  echo "0 $1" ;;
      HDMI*) echo "1 $1" ;;
      DP*)   echo "2 $1" ;;
      DVI*)  echo "3 $1" ;;
      VGA*)  echo "4 $1" ;;
      *)     echo "9 $1" ;;
  esac
}

SORTED=$(for o in "${OUTPUTS[@]}"; do priority "$o"; done | sort -n | cut -d' ' -f2)

# Build permutations via iterative selection
ORDER=()
REMAINING="$SORTED"

while [ -n "$REMAINING" ]; do
    SEL=$(echo "$REMAINING" | tr ' ' '\n' | rofi -dmenu -p "Select next monitor")
    [ -z "$SEL" ] && exit 1
    ORDER+=("$SEL")
    REMAINING=$(echo "$REMAINING" | tr ' ' '\n' | grep -v "^$SEL$")
done

# Ask resolution per output
RES=()

for o in "${ORDER[@]}"; do
    MODES=$(xrandr | awk "/^$o connected/{f=1;next} /^[A-Z]/{f=0} f{print \$1}")
    MODE=$(echo "$MODES" | rofi -dmenu -p "Resolution for $o")
    [ -z "$MODE" ] && exit 1
    RES+=("$MODE")
done

{
echo "#!/usr/bin/env bash"
echo "xrandr \\"

for i in "${!ORDER[@]}"; do
  if [ "$i" -eq 0 ]; then
    echo "  --output ${ORDER[$i]} --mode ${RES[$i]} --primary --pos 0x0 \\"
  else
    echo "  --output ${ORDER[$i]} --mode ${RES[$i]} --right-of ${ORDER[$((i-1))]} \\"
  fi
done

} > "$APPLY"

chmod +x "$APPLY"
"$APPLY"

# Save checksum of current xrandr state
xrandr | sha256sum | cut -d' ' -f1 > "$CHECKSUM"

notify-send "Screen layout saved"
