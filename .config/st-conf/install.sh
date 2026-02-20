#!/bin/sh
# install.sh — build and install st
# All patches are already integrated into the source.
# Just builds and installs to ~/.local/bin (no root needed).

set -e

cd "$(dirname "$0")"

echo "==> Cleaning previous build..."
rm -f config.h st.o x.o st boxdraw.o rowcolumn_diacritics_helpers.o graphics.o

echo "==> Building st..."
make

echo "==> Installing to ~/.local/bin ..."
make install

echo ""
echo "Done. st installed to ~/.local/bin/st"
echo "Make sure ~/.local/bin is in your PATH."
