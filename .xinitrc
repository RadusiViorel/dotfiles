[ -f "$HOME/.profile" ] && . "$HOME/.profile"

mkdir -p /opt/user/$(id -u)
export XDG_RUNTIME_DIR="/opt/user/$(id -u)"
chmod 700 "$XDG_RUNTIME_DIR"

export XDG_DATA_HOME="$HOME/.config"

$HOME/.config/_scripts/startup/brightness_on_boot.sh 8 20 &
$HOME/.config/xob/generate.sh &
$HOME/.config/xmobar/com/wallpaper/index change &


pipewire & pipewire-pulse & wireplumber &
dunst &
picom -b

$HOME/.config/_scripts/startup/displays/run.sh

exec $HOME/.local/bin/xmonad
