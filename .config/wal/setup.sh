#!/usr/bin/env bash
# Sets up pywal template directory by symlinking dotfiles templates
# into ~/.config/wal/templates/
#
# Run once after cloning dotfiles:
#   bash ~/dotfiles/wal/setup.sh

set -euo pipefail

DOTFILES_WAL="$(cd "$(dirname "$0")" && pwd)/templates"
WAL_TEMPLATES="$HOME/.config/wal/templates"

mkdir -p "$WAL_TEMPLATES"

for tmpl in "$DOTFILES_WAL"/*; do
  name="$(basename "$tmpl")"
  target="$WAL_TEMPLATES/$name"
  if [ -e "$target" ] && [ ! -L "$target" ]; then
    echo "WARNING: $target exists and is not a symlink — skipping"
    continue
  fi
  ln -sf "$tmpl" "$target"
  echo "Linked: $target -> $tmpl"
done

echo "Done. Run 'wal -i <wallpaper>' to generate your first palette."
