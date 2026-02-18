[ -f "$HOME/.profile" ] && . "$HOME/.profile"

[ -f "$HOME/.Xresources" ] && xrdb -merge "$HOME/.Xresources"

mkdir -p /opt/user/$(id -u)
export XDG_RUNTIME_DIR="/opt/user/$(id -u)"
chmod 700 "$XDG_RUNTIME_DIR"

export XDG_DATA_HOME="$HOME/.config"

# Auto-mount configured external drives
$HOME/.config/_scripts/startup/automount/automount.sh mount &

$HOME/.config/_scripts/startup/brightness_on_boot.sh 8 20 &
$HOME/.config/xob/generate.sh &
$HOME/.config/xmobar/com/wallpaper/index change &
$HOME/.config/_scripts/startup/slack_monitor &

pipewire & pipewire-pulse & wireplumber &
dunst &
#picom -b

docker compose --project-directory /opt/devenv start

$HOME/.config/_scripts/startup/displays/run.sh

exec $HOME/.local/bin/xmonad
