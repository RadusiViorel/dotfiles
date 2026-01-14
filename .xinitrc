mkdir -p /opt/user/$(id -u)
export XDG_RUNTIME_DIR="/opt/user/$(id -u)"
chmod 700 "$XDG_RUNTIME_DIR"

export XDG_DATA_HOME="${HOME}/.config"

${HOME}/.config/_scripts/brightness_on_boot 8 20 &
${HOME}/.config/_scripts/bg &
${HOME}/.config/xob/generate.sh &


pipewire & pipewire-pulse & wireplumber &
dunst &

exec ${HOME}/.local/bin/xmonad
