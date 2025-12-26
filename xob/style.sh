#!/usr/bin/env bash
NS="${1^^}"

VAR="XOB_NORMAL_FG_${NS}"     ; NORMAL_FG=${!VAR:-"#ffffff"}
VAR="XOB_NORMAL_BG_${NS}"     ; NORMAL_BG=${!VAR:-"#00000090"}
VAR="XOB_NORMAL_BORDER_${NS}" ; NORMAL_BORDER=${!VAR:-"#ffffff"}

VAR="XOB_ALT_FG_${NS}"    ; ALT_FG=${!VAR:-"#555555"}
VAR="XOB_ALT_BG_${NS}"    ; ALT_BG=${!VAR:-"#00000090"}
VAR="XOB_ALT_BORDER_${NS}"; ALT_BORDER=${!VAR:-"#555555"}

VAR="XOB_OF_FG_${NS}"     ; OF_FG=${!VAR:-"#ff0000"}
VAR="XOB_OF_BG_${NS}"     ; OF_BG=${!VAR:-"#00000090"}
VAR="XOB_OF_BORDER_${NS}" ; OF_BORDER=${!VAR:-"#ff0000"}

VAR="XOB_OF_FG_${NS}"     ; ALT_OF_FG=${!VAR:-"#550000"}
VAR="XOB_OF_BG_${NS}"     ; ALT_OF_BG=${!VAR:-"#00000090"}
VAR="XOB_OF_BORDER_${NS}" ; ALT_OF_BORDER=${!VAR:-"#550000"}

sed \
  -e "s|@NORMAL_FG@|$NORMAL_FG|g" \
  -e "s|@NORMAL_BG@|$NORMAL_BG|g" \
  -e "s|@NORMAL_BORDER@|$NORMAL_BORDER|g" \
  -e "s|@ALT_FG@|$ALT_FG|g" \
  -e "s|@ALT_BG@|$ALT_BG|g" \
  -e "s|@ALT_BORDER@|$ALT_BORDER|g" \
  -e "s|@OF_FG@|$OF_FG|g" \
  -e "s|@OF_BG@|$OF_BG|g" \
  -e "s|@OF_BORDER@|$OF_BORDER|g" \
  -e "s|@ALT_OF_FG@|$ALT_OF_FG|g" \
  -e "s|@ALT_OF_BG@|$ALT_OF_BG|g" \
  -e "s|@ALT_OF_BORDER@|$ALT_OF_BORDER|g" \
  ${HOME}/.config/xob/style.cfg.template > ${HOME}/.config/xob/${1}/style.cfg

tail -f ~/.config/xob/${1}/value | xob -c ~/.config/xob/${1}/style.cfg &
