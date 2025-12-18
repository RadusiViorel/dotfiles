import XMonad

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docks, ToggleStruts(..))
import qualified XMonad.StackSet as W

import Graphics.X11.ExtraTypes.XF86

import Data.List (elemIndex)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing

import System.Exit
import System.IO
import XMonad.Util.Run (spawnPipe)
import XMonad.Hooks.ShowWName

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Layout.ToggleLayouts  as TL
  (toggleLayouts, ToggleLayout(..))

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

--------------------------------------------------------------------------------
-- Workspaces
--------------------------------------------------------------------------------

clickableWorkspaces :: Bool
clickableWorkspaces = True

myWorkspaces :: [WorkspaceId]
myWorkspaces =
   map (\i -> i ++ " ")
      [ "\xf120" -- terminal
      , "\xf269" -- browser
      , "\xf07c" -- files
      , "\xf121" -- code
      , "\xf03e" -- media
      , "\xf086" -- chat
      , "\xf001" -- music
      , "\xf085" -- system
      , "\xf11b" -- games
      , "\xf013" -- misc
      ]

clickWorkspaces :: String -> String
clickWorkspaces ws
  | clickableWorkspaces = "<action=`xdotool key super+" ++ show idx ++ "`>"++ws++"</action>"
  | otherwise           = ws  -- non-clickable version
  where
    idx = case elemIndex ws myWorkspaces of
            Just n | n < 9 -> n + 1   -- workspaces 1-9
            Just 9         -> 0       -- 10th workspace → key 0
            _              -> 1



--------------------------------------------------------------------------------
-- xmobar
--------------------------------------------------------------------------------
myPP :: PP
myPP = def
  { ppCurrent = xmobarColor "#98be65" "" . xmobarRaw . clickWorkspaces
  , ppVisible = xmobarColor "#98be65" "" . xmobarRaw . clickWorkspaces
  , ppHidden  = xmobarColor "#bbbbbb" "" . xmobarRaw . clickWorkspaces
  , ppHiddenNoWindows = xmobarColor "#666666" ""
  , ppWsSep   = "    "
  , ppSep     = "  " ++ sep ++ "  "
  , ppTitle   = xmobarColor "#ffffff" "" . shorten 60
  }


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmhFullscreen
           $ ewmh
           $ docks
           $ myConfig xmproc

--------------------------------------------------------------------------------
-- Base Config
--------------------------------------------------------------------------------

myConfig xmproc = def
    { terminal           = term
    , modMask            = leader
    , borderWidth        = 1
    , normalBorderColor  = "#3B4252"
    , focusedBorderColor = "#BF616A"
    , layoutHook         = myLayouts
    , manageHook         = myManageHook
    , workspaces         = myWorkspaces
    , keys               = myKeys
    , logHook            = myLogHook xmproc
    }


--
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font = "xft:JetBrainsMono Nerd Font:style=Bold:size=60"
    , swn_fade    = 0.7
    , swn_bgcolor = "#1e1e2e"
    , swn_color   = "#cdd6f4"
    }

myLogHook xmproc =
    showWNameLogHook myShowWNameTheme
 <> dynamicLogWithPP myPP { ppOutput = hPutStrLn xmproc }

--------------------------------------------------------------------------------
-- Layouts
--------------------------------------------------------------------------------

myLayouts = mkToggle (NBFULL ?? EOT)
    $ avoidStruts
    $ smartBorders
    $ spacing gap
    layoutList
  where
    layoutList = TL.toggleLayouts Full (Tall 1 (3/100) (1/2))

--------------------------------------------------------------------------------
-- Window Rules
--------------------------------------------------------------------------------

myManageHook = composeAll
    [ isDialog --> doFloat
    , className =? "mpv"  --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "Firefox" --> doShift "2"
    , className =? "falkon"  --> doShift "2"
    , isFullscreen --> doFullFloat
    ]
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
    [ ((0, xF86XK_MonBrightnessUp),   spawn "brightnessctl set +6%")
    , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 6%-")
    , ((0 .|. shiftMask, xF86XK_MonBrightnessUp),   spawn "brightnessctl set 100%")
    , ((0 .|. shiftMask, xF86XK_MonBrightnessDown), spawn "brightnessctl set 1%")
    ]

    -- Volume
    ++
    [ ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ((0, xF86XK_AudioLowerVolume),   spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((0 .|. shiftMask, xF86XK_AudioRaiseVolume),   spawn "pactl set-sink-volume @DEFAULT_SINK@  100%")
    , ((0 .|. shiftMask, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@  0%")
    ]

    -- FullScreen
    ++
    [ ((leader .|. shiftMask, xK_f), sendMessage $ Toggle NBFULL)
    , ((leader, xK_f), sendMessage TL.ToggleLayout )
    --((leader, xK_f)              , sendMessage $ JumpToLayout "Full")
    ]

    -- Workspaces
    ++
    [ ((leader,               k), windows $ W.greedyView i)
      | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
    ]
    ++
    [ ((leader .|. shiftMask, k), windows $ W.shift i)
      | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
    ]
    ++
    -- workspace 10 (0)
    [ ((leader,               xK_0), windows $ W.greedyView (myWorkspaces !! 9))
      , ((leader .|. shiftMask, xK_0), windows $ W.shift      (myWorkspaces !! 9))
    ]
