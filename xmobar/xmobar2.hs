import Xmobar

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
      [ Run UnsafeXMonadLog
      ,Run $ Com "/home/radusiviorel/.config/xmobar/scripts/bar" [] "bar" 10
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = mconcat
      [ "%UnsafeXMonadLog% }{ "
      , "%bar%"
      ]
  }
