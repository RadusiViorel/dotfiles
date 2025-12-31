import XMonad
import Graphics.X11.ExtraTypes.XF86

import Data.List (elemIndex)

import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import XMonad.Actions.MouseResize

import XMonad.Hooks.ServerMode
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ShowWName
import XMonad.Hooks.ScreenCorners
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.Loggers
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare (getSortByIndex, filterOutWs)

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Layout.ToggleLayouts  as TL
  (toggleLayouts, ToggleLayout(..))

import System.Exit
import System.IO


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = xmonad . withSB mySB
           $ ewmhFullscreen
           $ ewmh
           $ docks
           $ myConfig

--------------------------------------------------------------------------------
-- Vars
--------------------------------------------------------------------------------

gap :: Int
gap  = 4

sep :: String
sep="\58913" -- 

leader :: KeyMask
leader = mod4Mask

term :: String
term = "kitty"

useHotCorners :: Bool
useHotCorners = False

hotCorners =
  [
    --   (SCUpperLeft, spawn "rofi -show drun")
    -- , (SCUpperRight, spawn "xterm")
         (SCLowerLeft,  spawn "falkon")
       , (SCLowerRight, spawn "falkon")
  ]


--------------------------------------------------------------------------------
-- ScratchPads
--------------------------------------------------------------------------------

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "term"
       "alacritty --class scratchpad -e tmux new-session -A -s scratch"
       (resource =? "scratchpad")
       (customFloating $ W.RationalRect 0.005 0.014 0.99 0.983)
  ]

scratchPadBorderColor :: String
scratchPadBorderColor = "blue"

--------------------------------------------------------------------------------
-- Workspaces
--------------------------------------------------------------------------------

ws1 = "\58878 "   --  file-explorer
ws2 = "\983727 "  -- 󰊯 browser
ws3 = "\62764 "   --  video
ws4 = "\xf121 "   -- code
ws5 = "\xf03e "   -- media
ws6 = "\xf086 "   -- chat
ws7 = "\60443 "   --  music
ws8 = "\62578 "   --  databse
ws9 = "\59245 "   --  Redis
ws0 = "\984241 "  -- 󰒱 slack

clickableWorkspaces :: Bool
clickableWorkspaces = False

myWorkspaces :: [WorkspaceId]
myWorkspaces = [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws0]

clickWorkspaces :: String -> String
clickWorkspaces ws
  | clickableWorkspaces = "<action=`xdotool key super+" ++ show idx ++ "`>"++ws++"</action>"
  | otherwise           = ws
  where
    idx = case elemIndex ws myWorkspaces of
            Just n | n < 9 -> n + 1
            Just 9         -> 0
            _              -> 1

--------------------------------------------------------------------------------
-- Xmobar
--------------------------------------------------------------------------------
--
mySB :: StatusBarConfig
mySB = statusBarProp "/home/radusiviorel/.local/bin/xmobar" (clickablePP myPP)

myPP :: PP
myPP = def
  { ppCurrent = xmobarColor "#98be65" "" . clickWorkspaces
  , ppVisible = xmobarColor "#98be65" "" . clickWorkspaces
  , ppHidden  = xmobarColor "#bbbbbb" "" . clickWorkspaces
  , ppHiddenNoWindows = xmobarColor "#666666" ""
  , ppWsSep   = "    "
  , ppSep     = "  " ++ sep ++ "  "
  , ppTitle   = xmobarColor "#ffffff" "" . shorten 60
  , ppSort = fmap (filterOutWs [scratchpadWorkspaceTag] .) getSortByIndex
  }

--------------------------------------------------------------------------------
-- Base Config
--------------------------------------------------------------------------------

myConfig = def
    { terminal           = term
    , modMask            = leader
    , borderWidth        = 1
    , normalBorderColor  = "#3B4252"
    , focusedBorderColor = "#BF616A"
    , layoutHook         = myLayouts
    , manageHook         = myManageHook
    , workspaces         = myWorkspaces
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , logHook            = myLogHook
    , handleEventHook    = serverModeEventHook <+> screenCornerEventHook
    , startupHook        = myStartupHook
    }

myStartupHook = do
      whenX (return useHotCorners) $ addScreenCorners hotCorners

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font = "xft:JetBrainsMono Nerd Font:style=Bold:size=90"
    , swn_fade    = 0.7
    , swn_bgcolor = "#1e1e2e"
    , swn_color   = "#cdd6f4"
    }

myLogHook =
    showWNameLogHook myShowWNameTheme


--------------------------------------------------------------------------------
-- Layouts
--------------------------------------------------------------------------------

myLayouts =
      screenCornerLayoutHook
    $ mkToggle (NBFULL ?? EOT)
    $ avoidStruts
    $ smartBorders
    $ spacing gap
    $ windowNavigation
    layoutList
  where
    layoutList =
      TL.toggleLayouts Full $
           renamed [Replace "Tall"]   (Tall 1 (3/100) (1/2))
       ||| renamed [Replace "Grid"]   Grid
       ||| renamed [Replace "Three"]  (ThreeColMid 1 (3/100) (1/2))
       ||| renamed [Replace "Spiral"] (spiral (6/7))

--------------------------------------------------------------------------------
-- Window Rules
--------------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ isDialog --> doFloat
    , className =? "nemo"     -->  doShift ws1

    , className =? "Firefox"  --> doShift ws2
    , className =? "falkon"   --> doShift ws2
    , className =? "chromium" --> doShift ws2

    , className =? "mpv"     --> doShift ws3
    , className =? "vlc"     --> doShift ws3
    , isFullscreen --> doFullFloat
    ]
  <+> namedScratchpadManageHook scratchpads
  <+> manageHook def

--------------------------------------------------------------------------------
-- Keymap
--------------------------------------------------------------------------------

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $

    -- Launch applications
    [ ((leader                , xK_Return), spawn $ terminal conf)
    , ((leader .|. controlMask, xK_Return), spawn "rofi -show drun")
    , ((leader .|. shiftMask, xK_i)       , spawn "$HOME/.config/_scripts/bg")

    , ((leader, xK_e), spawn "nemo")
    , ((leader, xK_b), spawn "falkon")

    -- Kill window
    , ((leader, xK_q), kill)
    , ((leader .|. shiftMask, xK_q), spawn  "stack exec xmonad -- --recompile" >> spawn "stack exec xmonad -- --restart")

    -- Move focus
    , ((leader, xK_j), windows W.focusDown)
    , ((leader, xK_k), windows W.focusUp)
    , ((leader, xK_m), windows W.focusMaster)

    -- Swap windows
    , ((leader .|. shiftMask, xK_j), windows W.swapDown)
    , ((leader .|. shiftMask, xK_k), windows W.swapUp)
    , ((leader .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Resize
    , ((leader, xK_h), sendMessage Shrink)
    , ((leader, xK_l), sendMessage Expand)
    , ((leader .|. shiftMask, xK_h), sendMessage MirrorShrink)
    , ((leader .|. shiftMask, xK_l), sendMessage MirrorExpand)
    ]

    -- Brightness
    ++
    [ ((0, xF86XK_MonBrightnessUp)                , spawn "brightnessctl set +6%")
    , ((0, xF86XK_MonBrightnessDown)              , spawn "brightnessctl set 6%-")
    , ((0 .|. shiftMask, xF86XK_MonBrightnessUp)  , spawn "brightnessctl set 100%")
    , ((0 .|. shiftMask, xF86XK_MonBrightnessDown), spawn "brightnessctl set 1%")
    ]

    -- Volume
    ++
    [ ((0, xF86XK_AudioRaiseVolume)              , spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ((0, xF86XK_AudioLowerVolume)              , spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((0 .|. shiftMask, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@  100%")
    , ((0 .|. shiftMask, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@  0%")
    ]

    -- Layouts switch
    ++
    [ ((leader .|. shiftMask, xK_f), sendMessage $ Toggle NBFULL)
    , ((leader, xK_f), sendMessage TL.ToggleLayout )
    , ((leader, xK_grave), sendMessage NextLayout)
    ]

    ++
    [  ((modMask conf, xK_v), namedScratchpadAction scratchpads "term")
    ]
    ++
    -- Focus window in direction
    [ ((leader, xK_Left ),  sendMessage $ Go L)
    , ((leader, xK_Right),  sendMessage $ Go R)
    , ((leader, xK_Up   ),  sendMessage $ Go U)
    , ((leader, xK_Down ),  sendMessage $ Go D)

    -- Pull (swap) window in direction
    , ((leader .|. shiftMask, xK_Left ),  sendMessage $ Swap L)
    , ((leader .|. shiftMask, xK_Right),  sendMessage $ Swap R)
    , ((leader .|. shiftMask, xK_Up   ),  sendMessage $ Swap U)
    , ((leader .|. shiftMask, xK_Down ),  sendMessage $ Swap D)

    -- Horizontal resize
    , ((leader, xK_less), sendMessage Shrink) -- mod + <
    , ((leader, xK_greater), sendMessage Expand) -- mod + >

    -- Vertical resize
    , ((leader, xK_minus), sendMessage MirrorShrink) -- mod + -
    , ((leader, xK_equal), sendMessage MirrorExpand) -- mod + +
    ]
    -- Workspaces
    ++
    [ ((leader, k), windows $ W.greedyView i)
      | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
    ]
    ++
    [ ((leader .|. shiftMask, k), windows $ W.shift i)
      | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
    ]
    ++
    -- workspace 10 (0)
    [ ((leader, xK_0), windows $ W.greedyView (myWorkspaces !! 9))
      , ((leader .|. shiftMask, xK_0), windows $ W.shift (myWorkspaces !! 9))
    ]

myMouseBindings (XConfig {modMask = modm}) = M.fromList
    [ -- mod + left click → resize window
      ((modm, button1), \w -> focus w >> mouseResizeWindow w)

      -- mod + middle click → pull (move) window
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    ]
