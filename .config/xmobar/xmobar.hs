import Xmobar

import Data.Monoid ((<>))

dir :: String
dir="/home/void/.config/xmobar"

cmd :: Bool -> String -> [String] -> Int -> Runnable
cmd True comp args rate =
  Run $ Com (dir <> "/com/" <> comp <> "/render") args comp rate
cmd False comp  _  _  =
  Run $ Com "echo" [""] comp 100000

click :: String -> String -> Int -> String -> String
click cmd action btn content =
  "<action=`" <> dir <> "/com/" <> cmd <> "/index " <> action <> "` button=" <> show btn <> ">" <> content <> "</action>"

main :: IO ()
main = xmobar defaultConfig
  { font = "JetBrainsMono Nerd Font 12"
  , additionalFonts = [
    "JetBrainsMono Nerd Font 16"
  ]
  , bgColor = "black"
  , fgColor = "white"
  , border = TopB
  , borderColor = "black"
  , alpha = 155
  --, position = TopW L 100
  , position = TopH 30
  , allDesktops = True
  , overrideRedirect = True
  , commands =
      [ cmd True  "start"   [] 10
      , Run UnsafeXMonadLog
      , cmd True "capslock" [] 10
      , Run $ Com "/home/void/.config/xmobar/scripts/bar" [] "bar" 10
      , cmd True "calendar"   [] 10
      , cmd True "power"      [] 100000
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = mconcat
      [ "%start%"
      , "%UnsafeXMonadLog%"
      , "}{"
      ,  "%capslock%"
      , "%bar%"
      , "%calendar%"
      , "%power%"
      ]
  }
