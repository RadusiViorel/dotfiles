import Xmobar
import Data.Monoid ((<>))

dir :: String
dir="/home/radusiviorel/.config/xmobar"

click :: String -> Int -> String -> String
click cmd btn content = "<action=`" <> cmd <> "` button=" <> show btn <> ">" <> content <> "</action>"

enabled :: Bool -> Command -> Runnable
enabled True  comp = Run $ comp
enabled False _    = Run $ Com "echo" [] "" 1000000

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
      [ enabled True  $ Com ( dir ++ "/com/capslock/index"  ) [] "capslock" 10
      , enabled True  $ Com ( dir ++ "/com/updates/index"   ) [] "updates" 3600
      , enabled True  $ Com ( dir ++ "/com/page/index"      ) [] "page" 10
      , enabled True  $ Com ( dir ++ "/com/volume/index"    ) [] "volume" 5
      , enabled True  $ Com ( dir ++ "/com/brightness/index") [] "brightness" 5
      , enabled True  $ Com ( dir ++ "/com/bluetooth/index" ) [] "bluetooth" 5
      , enabled True  $ Com ( dir ++ "/com/battery/index"   ) [] "battery" 5
      , enabled True  $ Com ( dir ++ "/com/cpu_temp/index"  ) [] "cpu_temp" 50
      , enabled True  $ Com ( dir ++ "/com/space/index"     ) [] "space" 20
      , enabled True  $ Com ( dir ++ "/com/network/index"   ) [] "network" 5
      , enabled True  $ Com ( dir ++ "/com/rofi_theme/index") [] "rofi_theme" 1000
      , enabled True  $ Com ( dir ++ "/com/calendar/index"  ) [] "calendar" 10
      , enabled True  $ Com ( dir ++ "/com/power/index"     ) [] "power" 1000
      , Run UnsafeXMonadLog
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = mconcat
      [ "%UnsafeXMonadLog% }{ "
      ,                                                        "%capslock%"
      , click ( dir ++ "/com/updates/index show"           ) 1 "%updates%"
      , click ( dir ++ "/com/volume/index volume_jump"     ) 1
      $ click ( dir ++ "/com/volume/index mute"            ) 2
      $ click ( dir ++ "/com/volume/index toggle"          ) 3
      $ click ( dir ++ "/com/volume/index volume_up"       ) 4
      $ click ( dir ++ "/com/volume/index volume_down"     ) 5 "%volume%"
      , click ( dir ++ "/com/brightness/index up 3"        ) 4
      $ click ( dir ++ "/com/brightness/index down 3"      ) 5 "%brightness%"
      , click ( dir ++ "/com/bluetooth/index open"         ) 1
      $ click ( dir ++ "/com/bluetooth/index toggle"       ) 2
      $ click ( dir ++ "/com/bluetooth/index reset"        ) 3
      $ click ( dir ++ "/com/bluetooth/index cycle_up"     ) 4
      $ click ( dir ++ "/com/bluetooth/index cycle_down"   ) 5 "%bluetooth%"
      , click ( dir ++ "/com/battery/index show_time"      ) 1 "%battery%"
      , click ( dir ++ "/com/cpu_temp/index top"           ) 1 "%cpu_temp%"
      , click ( dir ++ "/com/space/index toggle"           ) 1 "%space%"
      , click ( dir ++ "/com/network/index connect"        ) 1 "%network%"
      , click ( dir ++ "/com/rofi_theme/index change"      ) 1 "%rofi_theme%"
      , click ( dir ++ "/com/page/index toggle"            ) 1 "%page%"
      , click ( dir ++ "/com/calendar/index toggle"        ) 1
      $ click ( dir ++ "/com/calendar/index show_calendar" ) 3 "%calendar%"
      , click ( dir ++ "/com/power/index open"             ) 1 "%power%"
      ]
  }
