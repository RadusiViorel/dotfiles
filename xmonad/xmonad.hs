import XMonad
import Control.Monad (when)

import Graphics.X11.ExtraTypes.XF86
import Data.List (elemIndex)

import XMonad.Layout.ZoomRow
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

import XMonad.Actions.UpdatePointer
import XMonad.Actions.MouseResize
import XMonad.Actions.FloatSnap
import XMonad.Actions.WithAll (killAll)
import XMonad.Actions.TiledWindowDragging
import XMonad.Layout.DraggingVisualizer

import XMonad.Hooks.WindowSwallowing
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

import qualified XMonad.Actions.ConstrainedResize as Sqr
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Layout.ToggleLayouts  as TL
  (toggleLayouts, ToggleLayout(..))

import qualified XMonad.Prompt         as P
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S

import System.IO.Unsafe (unsafePerformIO)
import System.Environment (lookupEnv)
import System.Exit
import System.IO

--------------------------------------------------------------------------------
-- ENVS
--------------------------------------------------------------------------------
getEnvDefault :: String -> String -> String
getEnvDefault var def = unsafePerformIO $ fmap (maybe def id) (lookupEnv var)

color_ok :: String
color_ok = getEnvDefault "XMB_COLOR_OK" "#00FF00"

color_info :: String
color_info  = getEnvDefault "XMB_COLOR_INFO" "#0000FF"

color_yellow :: String
color_yellow  = getEnvDefault "XMB_COLOR_WARNING" "#FFFF00"

color_mute :: String
color_mute = getEnvDefault "XMB_COLOR_MUTE" "#666666"

color_white :: String
color_white = getEnvDefault "XMB_COLOR_WHITE" "#FFFFFF"

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

browser :: String
browser = "falkon"

useHotCorners :: Bool
useHotCorners = False

hotCorners =
  [
    --   (SCUpperLeft, spawn "rofi -show drun")
    -- , (SCUpperRight, spawn "xterm")
         (SCLowerLeft,  spawn browser)
       , (SCLowerRight, spawn browser)
  ]

searchEngineMap method = M.fromList $
  [ ((0, xK_g), method S.google)
  , ((0, xK_y), method S.youtube)
  , ((0, xK_m), method S.maps)
  , ((0, xK_p), method S.protondb)
  , ((0, xK_x), method S.voidpgks_x86_64)
  ]

--------------------------------------------------------------------------------
-- ScratchPads
--------------------------------------------------------------------------------

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "term"
       "alacritty --class scratchpad -e tmux new-session -A -s scratch"
       (resource =? "scratchpad")
       (customFloating $ W.RationalRect 0.005 0.028 0.99 0.967)
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


myWorkspaces :: [WorkspaceId]
myWorkspaces = [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws0]

pinnedApps :: [(String, WorkspaceId, Bool)]
pinnedApps =
  [ ("nemo"           , ws1, True)
  , ("Firefox"        , ws2, True)
  , ("falkon"         , ws2, True)
  , ("chromium"       , ws2, True)
  , ("Google-chrome"  , ws2, True)
  , ("mpv"            , ws3, True)
  , ("vlc"            , ws3, True)
  , ("DBeaver"        , ws8, False)
  , ("Redis Insight"  , ws9, False)
  , ("Slack"          , ws0, True)
  ]

clickableWorkspaces :: Bool
clickableWorkspaces = False

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
mySB = statusBarProp "${HOME}/.local/bin/xmobar" (clickablePP myPP)

myPP :: PP
myPP = def
  { ppCurrent         = xmobarColor color_ok   "" . xmobarFont 1 . clickWorkspaces
  , ppVisible         = xmobarColor color_info "" . clickWorkspaces
  , ppHidden          = xmobarColor color_info "" . clickWorkspaces
  , ppHiddenNoWindows = xmobarColor color_mute ""
  , ppWsSep           = "  "
  , ppSep             = " " ++ sep ++ " "
  , ppTitle           = xmobarColor color_white "" . shorten 60
  , ppSort            = fmap (filterOutWs [scratchpadWorkspaceTag] .) getSortByIndex
  , ppLayout          = \l -> case l of
      "Full"   -> xmobarColor color_yellow "" "\989268" -- 󱡔
      "Lines"  -> xmobarColor color_info   "" "\984713" -- 󰚉
      "Tall"   -> xmobarColor color_info   "" "\62750"  -- 
      "Grid"   -> xmobarColor color_info   "" "\987609" -- 󱇙
      "Three"  -> xmobarColor color_info   "" "\986897" -- 󰼑
      "Spiral" -> xmobarColor color_info   "" "\983064" -- 󰀘
      _        -> xmobarColor color_info   "" "\984640" -- 󰙀

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
    , handleEventHook    = myHandleEventHook
    , startupHook        = myStartupHook
    }

myHandleEventHook =
      serverModeEventHook
  <+> screenCornerEventHook
  <+> swallowEventHook (
         className =? "Alacritty"
    <||> className =? "kitty"
  ) (return True)

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
--  >> updatePointer (0.5, 0.5) (0, 0) -- bug with windowDrag

--------------------------------------------------------------------------------
-- Layouts
--------------------------------------------------------------------------------

myLayouts =
      screenCornerLayoutHook
    $ renamed [CutWordsLeft 2]
    $ draggingVisualizer
    $ mkToggle (NBFULL ?? EOT)
    $ avoidStruts
    $ smartBorders
    $ smartSpacing gap
    $ windowNavigation
    layoutList
  where
    layoutList =
      TL.toggleLayouts ( renamed [Replace "Full"] Full ) $
           renamed [Replace "Lines"]  (zoomRow)
       ||| renamed [Replace "Tall"]   (Tall 1 (3/100) (1/2))
       ||| renamed [Replace "Grid"]   (Grid)
       ||| renamed [Replace "Three"]  (ThreeColMid 1 (3/100) (1/2))
--       ||| renamed [Replace "Spiral"] (spiral (6/7))

--------------------------------------------------------------------------------
-- Window Rules
--------------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook =
    composeAll
      ( [ isDialog --> doFloat
        , isFullscreen --> doFullFloat
        ]
        ++ map (\(c, ws, shouldFocus) -> (className =? c <||> appName =? c <||> resource =? c) --> (doShift ws <+> if shouldFocus then doF (W.greedyView ws) else mempty)) pinnedApps
      )
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
--    , ((leader .|. shiftMask  , xK_i     ), randomBg $ HSV 0xff 0x20)

    , ((leader, xK_s), SM.submap $ searchEngineMap $ S.promptSearch P.def)

    , ((leader, xK_e), spawn "nemo" )
    , ((leader, xK_b), spawn browser)

    -- Kill window
    , ((leader, xK_q), kill)
    , ((leader .|. shiftMask, xK_q), spawn " notify-send -t 10000 'XMonad' 'Rebuilding…' && stack build && stack install && xmonad --restart")

--    , ((leader .|. shiftMask, xK_q), spawn "ID=$(dunstify -t 0 'XMonad' 'Rebuilding…' -p) && stack build && stack install && stack exec xmonad -- --recompile && dunstify -C $ID && xmonad --restart" )

    -- Move focus
    , ((leader, xK_j), windows W.focusDown)
    , ((leader, xK_k), windows W.focusUp)
    , ((leader, xK_m), windows W.focusMaster)

    -- Swap windows
    , ((leader .|. shiftMask, xK_j), windows W.swapDown)
    , ((leader .|. shiftMask, xK_k), windows W.swapUp)
    , ((leader .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Resize
    , ((leader, xK_h), whenZoomRow zoomOut Shrink)
    , ((leader, xK_l), whenZoomRow zoomIn Expand)
    , ((leader, xK_equal), sendMessage zoomReset)

    , ((leader .|. shiftMask, xK_h), sendMessage MirrorShrink)
    , ((leader .|. shiftMask, xK_l), sendMessage MirrorExpand)

    ]

    -- Brightness
    ++
    [ ((0, xF86XK_MonBrightnessUp)                , spawn "${HOME}/.config/xmobar/com/brightness/index up")
    , ((0, xF86XK_MonBrightnessDown)              , spawn "${HOME}/.config/xmobar/com/brightness/index down")
    , ((0 .|. shiftMask, xF86XK_MonBrightnessUp)  , spawn "brightnessctl set 100%")
    , ((0 .|. shiftMask, xF86XK_MonBrightnessDown), spawn "brightnessctl set 1%")
    ]

    -- Volume
    ++
    [ ((0, xF86XK_AudioRaiseVolume)              , spawn "${HOME}/.config/xmobar/com/volume/index volume_up")
    , ((0, xF86XK_AudioLowerVolume)              , spawn "${HOME}/.config/xmobar/com/volume/index volume_down")
    , ((0 .|. shiftMask, xF86XK_AudioRaiseVolume), spawn "${HOME}/.config/xmobar/com/volume/index volume_jump")
    , ((0 .|. shiftMask, xF86XK_AudioLowerVolume), spawn "${HOME}/.config/xmobar/com/volume/index mute")
    ]

    -- Screenshot
    ++
    [ ((leader, xK_Print), spawn "$HOME/.config/xmobar/com/screenshot/index take")
    ]

    -- Layouts switch
    ++
    [ ((leader .|. shiftMask, xK_f), sendMessage $ Toggle NBFULL)
    , ((leader, xK_f), sendMessage TL.ToggleLayout )
    , ((leader, xK_t), withFocused toggleCenterFloat)
    , ((leader, xK_grave), do
        sendMessage NextLayout
        withWindowSet $ \ws -> do
          let desc = description . W.layout . W.workspace . W.current $ ws
          when (desc == "Lines") $
            sendMessage zoomReset
          spawn $ "dunstify -r 9999 'Layout: " ++ desc ++ "'"
      )
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
    [ ((leader .|. shiftMask, k), windows $ W.view i . W.shift i)
      | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
    ]
    ++
    -- workspace 10 (0)
    [ ((leader, xK_0), windows $ W.greedyView (myWorkspaces !! 9))
    , ((leader .|. shiftMask, xK_0) , windows $ W.view (myWorkspaces !! 9) . W.shift (myWorkspaces !! 9))
    ]

myMouseBindings (XConfig {modMask = modm}) = M.fromList
    [

    ((modm, button1), \w -> do
       floats <- gets (W.floating . windowset)
       if w `M.member` floats
         then Sqr.mouseResizeWindow w False
           >> afterDrag (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)
         else dragWindow w
    )
    , ((modm .|. controlMask, button1), \w -> do
        floats <- gets (W.floating . windowset)
        when (w `M.member` floats)
          $ Sqr.mouseResizeWindow w True
            >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)
    )
    , ((modm .|. shiftMask, button1), \w -> do
        floats <- gets (W.floating . windowset)
        when (w `M.member` floats)
          $ mouseMoveWindow w
            >> afterDrag (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)
      )

    , ((modm              , button2), \w -> focus w >> killWindow w)
    , ((modm .|. shiftMask, button2), \_ -> killAll)

    , ((modm, button3), \_ -> sendMessage $ Toggle NBFULL)

    -- Scroll up → decrease window size
    , ((modm , button4), \w -> do
        focus w
        whenZoomRow zoomIn Expand
      )
    -- Scroll down → decrease window size
    , ((modm , button5), \w -> do
        focus w
        whenZoomRow zoomOut Shrink
      )
    , ((modm .|. shiftMask, button4), \w -> focus w >> mouseResizeWindow w)
    , ((modm .|. shiftMask, button5), \w -> focus w >> mouseResizeWindow w)
    ]



whenZoomRow :: ZoomMessage -> Resize -> X ()
whenZoomRow zoomAction fallback = do
  desc <- gets (description . W.layout . W.workspace . W.current . windowset)
  if desc == "Lines"
     then sendMessage zoomAction
     else sendMessage fallback

toggleCenterFloat :: Window -> X ()
toggleCenterFloat w = do
  floats <- gets (W.floating . windowset)
  if w `M.member` floats
    then windows (W.sink w)  -- send back to tiling
    else windows (W.float w (W.RationalRect 0.1 0.1 0.8 0.8))  -- float & center
