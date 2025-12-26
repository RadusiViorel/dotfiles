# If XDG_RUNTIME_DIR is not set, fix it for PipeWire
#
export XDG_DATA_HOME="${HOME}/.config"
export TERM=alacritty
export EDITOR=nvim

if [ -z "$XDG_RUNTIME_DIR" ]; then
    export XDG_RUNTIME_DIR="/tmp/user/$(id -u)"
    mkdir -p "$XDG_RUNTIME_DIR"
    chmod 700 "$XDG_RUNTIME_DIR"
fi

${HOME}/.config/_scripts/brightness_on_boot 8 20 &
${HOME}/.config/_scripts/bg &

${HOME}/.config/xob/generate.sh &

pipewire &
pipewire-pulse &
wireplumber &

dunst &

exec ${HOME}/.local/bin/xmonad
