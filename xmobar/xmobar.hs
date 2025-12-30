import Xmobar
import Data.Monoid ((<>))

dir :: String
dir="/home/radusiviorel/.config/xmobar"

cmd :: Bool -> String -> [String] -> Int -> Runnable
cmd True comp args rate =
  Run $ Com (dir <> "/com/" <> comp <> "/index") args comp rate
cmd False comp  _  _  =
  Run $ Com "echo" [""] comp 100000

click :: String -> String -> Int -> String -> String
click cmd action btn content =
  "<action=`" <> dir <> "/com/" <> cmd <> "/index " <> action <> "` button=" <> show btn <> ">" <> content <> "</action>"


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
      [ cmd True  "capslock"   [] 20
      , cmd True  "updates"    [] 3600
      , cmd True  "page"       [] 10
      , cmd True  "volume"     [] 5
      , cmd True  "brightness" [] 5
      , cmd True  "bluetooth"  [] 5
      , cmd True  "battery"    [] 5
      , cmd True  "cpu_temp"   [] 50
      , cmd True  "space"      [] 20
      , cmd True  "network"    [] 5
      , cmd True  "rofi_theme" [] 1000
      , cmd True  "calendar"   [] 10
      , cmd True  "speedtest"  [] 600
      , cmd True  "power"      [] 100000
      , Run UnsafeXMonadLog
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = mconcat
      [ "%UnsafeXMonadLog% }{ "
      ,                                       "%capslock%"
      ,  click "updates"    "show"          1 "%updates%"
      ,  click "volume"     "volume_jump"   1
      $  click "volume"     "mute"          2
      $  click "volume"     "toggle"        3
      $  click "volume"     "volume_up"     4
      $  click "volume"     "volume_down"   5 "%volume%"
      ,  click "brightness" "up 3"          4
      $  click "brightness" "down 3"        5 "%brightness%"
      ,  click "bluetooth"  "open"          1
      $  click "bluetooth"  "toggle"        2
      $  click "bluetooth"  "reset"         3
      $  click "bluetooth"  "cycle_up"      4
      $  click "bluetooth"  "cycle_down"    5 "%bluetooth%"
      ,  click "battery"    "show_time"     1 "%battery%"
      ,  click "cpu_temp"   "top"           1 "%cpu_temp%"
      ,  click "space"      "toggle"        1 "%space%"
      ,  click "network"    "connect"       1
      $  click "network"    "toggle"        2
      $  click "network"    "traffic"       3 "%network%"
      ,  click "rofi_theme" "change"        1 "%rofi_theme%"
      ,  click "page"       "toggle"        1 "%page%"
      ,  click "calendar"   "toggle"        1
      $  click "calendar"   "show_calendar" 3 "%calendar%"
      ,  click "speedtest"  "show"          1 "%speedtest%"
      ,  click "power"      "open"          1 "%power%"
      ]
  }
