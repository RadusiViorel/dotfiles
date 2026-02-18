export XDG_RUNTIME_DIR="/opt/user/$(id -u)"
export XDG_DATA_HOME="${HOME}/.config"
export XDG_DATA_DIRS="$HOME/.local/desktopIcons:/usr/local/share:/usr/share"

export GOPATH=$HOME/opt/go
export LOCALBIN=$HOME/.local/bin
export PATH=$LOCALBIN:${GOPATH}/bin/:$PATH

export EDITOR=nvim
export TERM=st

export XOB_NORMAL_FG_BRIGHTNESS="#f28f2f"
export XOB_NORMAL_BORDER_BRIGHTNESS="#e6c15c"

export XOB_NORMAL_FG_VOLUME="#1E90FF"
export XOB_NORMAL_BORDER_VOLUME="#888888"

export XOB_OF_FG_VOLUME="#FF6347"
export XOB_OF_BORDER_VOLUME="#FF4500"

export XMB_COLOR_WHITE="#ffffff"
export XMB_COLOR_MUTE=#696969
export XMB_COLOR_INFO=#87c6e6
export XMB_COLOR_OK="#84f098"
export XMB_COLOR_WARNING="#e6c15c"
export XMB_COLOR_CRITIC="#d94141"
export XMB_COLOR_DANGER="#f58989"
export XMB_COLOR_ORANGE="#f28f2f"


export POPCASH_DEVDIR="/opt/work"

export INTELEPHENSE_LICENSE_KEY=00NOKTX8W5P0A3G
export DISTROBOX_BACKEND=docker

. "$HOME/.config/../bin/env"
