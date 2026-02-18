export LC_CTYPE=en_US.UTF-8

# Auto-start tmux if available
if [ -z "$TMUX" ] && command -v tmux &>/dev/null; then
    exec tmux new-session -s "$(cat /proc/sys/kernel/random/uuid | cut -d'-' -f1)"
fi
export COLORTERM=gnome-terminal

setopt auto_cd

fpath=(/opt/zsh/spaceship $fpath)
autoload -U promptinit; promptinit
prompt spaceship

# Enable Colors
autoload -U colors && colors

# History Settings
HISTSIZE=10000
SAVEHIST=10000
HISTFILESIZE=10000
HISTFILE=~/.config/zsh/history
HISTCONTROL=ignoredups
setopt appendhistory     #Append history to the history file (no overwriting)
setopt sharehistory      #Share history across terminals
setopt incappendhistory  #Immediately append to the history file, not just when a term is killed

bindkey -e # use C-a and C-e to just to start and end of prompt

autoload -U compinit && compinit -u
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*' # Auto complete with case insenstivity

zmodload zsh/complist
compinit
_comp_options+=(globdots)   # Include hidden files.

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'left' vi-backward-char
bindkey -M menuselect 'down' vi-down-line-or-history
bindkey -M menuselect 'up' vi-up-line-or-history
bindkey -M menuselect 'right' vi-forward-char
# Fix backspace bug when switching modes
bindkey "^?" backward-delete-char

#Enable searching through history
bindkey '^R' history-incremental-pattern-search-backward

# Search repos for programs that can't be found
source /usr/share/doc/pkgfile/command-not-found.zsh 2>/dev/null

# Basic aliases
alias vi="nvim"
alias @web="cd /opt/work"

# Change ls to ls-icons
alias ls="lsd"
alias ll="ls-icons -tal --color=yes"
alias l="ls-icons --width=1"

# Git commands
alias gst="git status"
alias gcmt="git commit -am "
alias gignore="touch .gitignore "
alias greset="git reset --hard HEAD "
alias glog="git log --stat"
alias gslog="git log --oneline --all --decorate --graph"

alias history='fc -l 1'

alias ssh_pop_clean='aws ssm start-session --target=i-060a966f4b0984c3e --reason="Clean-up docker storage"'
# aws ec2 describe-instances --filters 'Name=tag:Name,Values=jenkins-cd' --output text --query 'Reservations[*].Instances[*].InstanceId'
# AutoComplete zsh-suggestion with ctrl-space:
bindkey '^ ' autosuggest-accept

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^f' edit-command-line

eval "$(atuin init zsh)"
source /opt/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
source /opt/zsh/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null

# Load zsh-syntax-highlighting; should be last.
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

ZSH_HIGHLIGHT_STYLES[default]=none
ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=#e8827b
ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=#ab807d
ZSH_HIGHLIGHT_STYLES[alias]=fg=#a0d9cb,bold
ZSH_HIGHLIGHT_STYLES[builtin]=fg=#7895ff
ZSH_HIGHLIGHT_STYLES[function]=fg=#7895ff
ZSH_HIGHLIGHT_STYLES[command]=fg=#f3ff73,bold
ZSH_HIGHLIGHT_STYLES[precommand]=fg=white,underline
ZSH_HIGHLIGHT_STYLES[commandseparator]=none
ZSH_HIGHLIGHT_STYLES[hashed-command]=fg=009
ZSH_HIGHLIGHT_STYLES[path]=fg=#fc913f,underline
ZSH_HIGHLIGHT_STYLES[globbing]=fg=063
ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=white,underline
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]=none
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]=none
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]=none
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]=fg=#7ad470
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]=fg=#7ad470
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]=fg=#7ad470
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]=fg=#7ad470
ZSH_HIGHLIGHT_STYLES[assign]=none

# opencode
export PATH=/home/void/.opencode/bin:$PATH

. "$HOME/.config/../bin/env"
