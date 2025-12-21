import Xmobar
import Data.Monoid ((<>))

click :: String -> Int -> String -> String
click cmd btn content = "<action=`" <> cmd <> "` button=" <> show btn <> ">" <> content <> "</action>"

main :: IO ()
main = xmobar defaultConfig
  { font = "-misc-fixed-*-*-*-*-*-16-*-*-*-*-*-*-*"
  , bgColor = "black"
  , fgColor = "white"
  , border = TopB
  , borderColor = "black"
  , alpha = 155
  , position = TopW L 100
  , allDesktops = True
  , overrideRedirect = True
  , commands =
      [ Run $ Com "/home/radosiviorel/.config/xmobar/com/capslock/index"   [] "capslock" 10
      , Run $ Com "/home/radusiviorel/.config/xmobar/com/updates/index"    [] "updates" 3600
      , Run $ Com "/home/radusiviorel/.config/xmobar/com/page/index"       [] "page" 10
      , Run $ Com "/home/radusiviorel/.config/xmobar/com/volume/index"     [] "volume" 5
      , Run $ Com "/home/radusiviorel/.config/xmobar/com/brightness/index" [] "brightness" 5
      , Run $ Com "/home/radusiviorel/.config/xmobar/com/bluetooth/index"  [] "bluetooth" 5
      , Run $ Com "/home/radusiviorel/.config/xmobar/com/battery/index"    [] "battery" 5
      , Run $ Com "/home/radusiviorel/.config/xmobar/com/cpu_temp/index"   [] "cpu_temp" 50
      , Run $ Com "/home/radusiviorel/.config/xmobar/com/space/index"      [] "space" 20
      , Run $ Com "/home/radusiviorel/.config/xmobar/com/network/index"    [] "network" 5
      , Run $ Com "/home/radusiviorel/.config/xmobar/com/rofi_theme/index" [] "rofi_theme" 1000
      , Run $ Com "/home/radusiviorel/.config/xmobar/com/calendar/index"   [] "calendar" 10
      , Run $ Com "/home/radusiviorel/.config/xmobar/com/power/index"      [] "power" 1000
      , Run UnsafeStdinReader
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = mconcat
      [ "%UnsafeStdinReader% }{ "
      , "%capslock%"
      , click "$HOME/.config/xmobar/com/updates/index show" 1 "%updates%"
      , click "$HOME/.config/xmobar/com/volume/index volume_jump" 1
      $ click "$HOME/.config/xmobar/com/volume/index mute"        2
      $ click "$HOME/.config/xmobar/com/volume/index toggle"      3
      $ click "$HOME/.config/xmobar/com/volume/index volume_up"   4
      $ click "$HOME/.config/xmobar/com/volume/index volume_down" 5 "%volume%"
      , click "$HOME/.config/xmobar/com/brightness/index up 3"   4
      $ click "$HOME/.config/xmobar/com/brightness/index down 3" 5 "%brightness%"
      , click "$HOME/.config/xmobar/com/bluetooth/index open"       1
      $ click "$HOME/.config/xmobar/com/bluetooth/index toggle"     2
      $ click "$HOME/.config/xmobar/com/bluetooth/index reset"      3
      $ click "$HOME/.config/xmobar/com/bluetooth/index cycle_up"   4
      $ click "$HOME/.config/xmobar/com/bluetooth/index cycle_down" 5 "%bluetooth%"
      , click "$HOME/.config/xmobar/com/battery/index show_time" 1 "%battery%"
      , click "$HOME/.config/xmobar/com/cpu_temp/index top" 1 "%cpu_temp%"
      , click "$HOME/.config/xmobar/com/space/index toggle" 1 "%space%"
      , click "$HOME/.config/xmobar/com/network/index connect" 1 "%network%"
      , click "$HOME/.config/xmobar/com/rofi_theme/index change" 1 "%rofi_theme%"
      , click "$HOME/.config/xmobar/com/page/index toggle" 1 "%page%"
      , click "$HOME/.config/xmobar/com/calendar/index toggle"        1
      $ click "$HOME/.config/xmobar/com/calendar/index show_calendar" 3 "%calendar%"
      , click "$HOME/.config/xmobar/com/power/index open" 1 "%power%"
      ]
  }
