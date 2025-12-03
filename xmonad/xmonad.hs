import XMonad

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docks, ToggleStruts(..))

import Graphics.X11.ExtraTypes.XF86

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

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Layout.ToggleLayouts  as TL
  (toggleLayouts, ToggleLayout(..))

--------------------------------------------------------------------------------
-- Vars
--------------------------------------------------------------------------------

gap :: Int
gap  = 4

leader :: KeyMask
leader = mod4Mask


term :: String
term = "alacritty"

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
    , keys               = myKeys
    , workspaces         = map show [1..9]
    , logHook            = dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle  = xmobarColor "green" "" . shorten 50
        }
    }

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
    , isFullscreen --> doFullFloat
    ]

--------------------------------------------------------------------------------
-- Keymap
--------------------------------------------------------------------------------

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $

    -- Launch applications
    [ ((leader                , xK_Return), spawn $ terminal conf)
    , ((leader .|. controlMask, xK_Return), spawn "rofi -show drun")

    , ((leader, xK_e), spawn "nemo")
    , ((leader, xK_b), spawn "falkon")

    -- Kill window
    , ((leader, xK_q), kill)
    , ((leader .|. shiftMask, xK_q), restart "~/.cache/xmonad/xmonad-x86_64-linux" True)

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
    [ ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 6%-")
    , ((0, xF86XK_MonBrightnessUp),   spawn "brightnessctl set +6%")
    , ((0 .|. shiftMask, xF86XK_MonBrightnessDown), spawn "brightnessctl set 1%")
    , ((0 .|. shiftMask, xF86XK_MonBrightnessUp),   spawn "brightnessctl set 100%")
    ]

    -- FullScreen
    ++
    [ ((leader .|. shiftMask, xK_f), sendMessage $ Toggle NBFULL)
    , ((leader, xK_f), sendMessage TL.ToggleLayout )
    --((leader, xK_f)              , sendMessage $ JumpToLayout "Full")
    ]

    -- Workspaces
    ++
    [ ((leader, k), windows $ W.view i)
    | (i, k) <- zip (map show [1..9]) [xK_1 .. xK_9]
    ]
    ++
    [ ((leader .|. shiftMask, k), windows $ W.shift i)
    | (i, k) <- zip (map show [1..9]) [xK_1 .. xK_9]
    ]
