{-# LANGUAGE OverloadedStrings #-}
  -- Base
import XMonad
import System.Directory
import System.IO (hClose, hPutStr, hPutStrLn)
import System.Exit (exitWith, exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll, killOthers)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.Minimize

    -- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutCombinators (JumpToLayout(..))

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS, FULL))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.Minimize
   -- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook, javaHack, trayerAboveXmobarEventHook, trayAbovePanelEventHook, trayerPaddingXmobarEventHook, trayPaddingXmobarEventHook, trayPaddingEventHook)
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare


-- Polybar
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "polybar --reload xmonad"
  spawnOnce "xwallpaper --stretch ~/.config/wall.png"
  spawnOnce "picom -b --experimental-backends"
  spawnOnce "kitty"
  spawnOnce "udiskie -q -t --no-terminal"
  spawnOnce "/usr/lib/polkit-kde-authentication-agent-1"
  setWMName "LG3D"

myBorderWidth :: Dimension
myBorderWidth = 3

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Sans:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#000000"
    , swn_color             = "#FFFFFF"
    }

myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]


-- Makes setting the spacingRaw simpler to write. The spacingRaw
-- module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall = renamed [Replace "tall"]
    $ limitWindows 5
    $ windowNavigation
    -- $ subLayout [] (smartBorders Simplest)
    $ mySpacing' 2
    $ ResizableTall 1 (3/100) (1/2) []

monocle = renamed [Replace "monocle"]
    $ windowNavigation
    $ Full
floats = renamed [Replace "floats"]
    $ simplestFloat

myLayoutHook = avoidStruts
               $ minimize
               $ mouseResize
               $ windowArrange
               $ T.toggleLayouts monocle
               $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout =  smartBorders   tall
                       ||| noBorders monocle
                       ||| floats

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
  -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
  -- I'm doing it this way because otherwise I would have to write out the full
  -- name of my workspaces and the names would be very long if using clickable workspaces.
  [ className =? "confirm"         --> doFloat
  , className =? "file_progress"   --> doFloat
  , className =? "dialog"          --> doFloat
  , className =? "download"        --> doFloat
  , className =? "error"           --> doFloat
  , className =? "Gimp"            --> doFloat
  , className =? "notification"    --> doFloat
  , className =? "pinentry-gtk-2"  --> doFloat
  , className =? "splash"          --> doFloat
  , className =? "toolbar"         --> doFloat
  , className =? "Arandr"         --> doFloat
  , className =? "Arcolinux-tweak-tool.py"  --> doFloat
  , className =? "Arcolinux-welcome-app.py" --> doFloat
  , className =? "Connman-gtk"      --> doFloat
  , className =? "Arcologout.py"    --> doFloat
  , className =? "albert"           --> doFloat
  , className =? "feh"              --> doFloat
  , className =? "Galculator"       --> doFloat
  , className =? "Qalculate-gtk"    --> doFloat
  , className =? "Gnome-calculator" --> doFloat
  , className =? "Nitrogen"         --> doFloat
  , className =? "Oblogout"         --> doFloat
  , className =? "NoiseTorch"       --> doFloat
  , className =? "Grub-customizer"  --> doFloat
  , className =? "Pavucontrol"      --> doFloat
  , className =? "rofi"             --> doFloat
  , className =? "matplotlib"       --> doFloat
  , className =? "Yad"              --> doCenterFloat
  , title =? "Oracle VM VirtualBox Manager"   --> doFloat
  , title =? "Order Chain - Market Snapshots" --> doFloat
  , title =? "Mozilla Firefox"      --> doShift ( myWorkspaces !! 1 )
  , className =? "firefox"          --> doShift ( myWorkspaces !! 1 )
  , className =? "LibreWolf"        --> doShift ( myWorkspaces !! 1 )
  , className =? "Chromium"         --> doShift ( myWorkspaces !! 1 )
  , className =? "Chromium-browser" --> doShift ( myWorkspaces !! 1 )
  , className =? "waterfox-current" --> doShift ( myWorkspaces !! 1 )
  , className =? "qutebrowser"      --> doShift ( myWorkspaces !! 1 )
  , className =? "Brave-browser"    --> doShift ( myWorkspaces !! 1 )
  , className =? "mpv"              --> doShift ( myWorkspaces !! 3 )
  , className =? "Steam"            --> doShift ( myWorkspaces !! 2 )
  , className =? "Lutris"           --> doShift ( myWorkspaces !! 2 )
  , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 3 )
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
  , isFullscreen -->  doFullFloat
  ]

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput  = dbusOutput dbus
    , ppCurrent = wrap ("%{F" ++ "#da8548" ++ "} ") "%{F-}"
    , ppVisible = wrap ("%{F" ++ "#1c1f24" ++ "} ") "%{F-}"
    , ppUrgent  = wrap ("%{F" ++ "#98be65" ++ "} ") "%{F-}"
    , ppHidden  = wrap ("%{F" ++ "#1c1f24" ++ "} ") "%{F-}"
    , ppTitle   = wrap ("%{F" ++ "#ff6c6b" ++ "}")"%{F-}"
    , ppSep     = "  |  "
    }

shiftAndView :: Int -> X ()
shiftAndView n = windows $ W.greedyView (myWorkspaces !! (n - 1))
                         . W.shift      (myWorkspaces !! (n - 1))

toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s)


main :: IO ()
main = do

  dbus <- D.connectSession
    -- Request access to the DBus name
  D.requestName dbus (D.busName_ "org.xmonad.Log")
      [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad $ docks . ewmhFullscreen . ewmh $ def
    { manageHook         = myManageHook <+> manageDocks
    , handleEventHook    =  windowedFullscreenFixEventHook <> minimizeEventHook <> swallowEventHook (className =? "Alacritty"  <||> className =? "st-256color" <||> className =? "XTerm") (return True)
    , logHook = dynamicLogWithPP (myLogHook dbus)
    , modMask            = mod4Mask
    , terminal           = "alacritty"
    , startupHook        = myStartupHook
    , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , focusedBorderColor = "#ecb3b2"
    , normalBorderColor  = "#282c34"
    } `additionalKeysP` [
      ("M-C-r",        spawn "xmonad --recompile && xmonad --restart")
    , ("M-S-r",        spawn "xmonad --restart")
    , ("M-S-q",        kill)
    , ("M-C-q",        killOthers)
    , ("M-C-x",        io exitSuccess)

    , ("M-<Return>",   spawn  "$TERMINAL || xterm")
    , ("M-S-<Return>", spawn  "kitty")
    , ("M-e",          spawn  "thunar || pcmanfm")
    , ("M-S-e",        spawn  "alacritty -e lfv")
    , ("M-d",          spawn  "dmenu_run_history -i")
    , ("M-S-d",        spawn  "rofi -show run -async-pre-read")
    , ("M-w",          spawn  "firefox")
    , ("M-S-w",        spawn  "brave")
    , ("M-r",          spawn  "rofi -show drun -async-pre-read")
    , ("M-y",          spawn  "clipboard")

    , ("<Print>",      spawn  "flameshot gui")
    , ("S-<Print>",    spawn  "take_ss focus")
    , ("A-<Print>",    spawn  "take_ss full")
    , ("M-g",          spawn  "qalculate-gtk || galculator || gnome-calculator || notify-send \"Error\" \"Calculator App not installed\" -u critical")
    , ("M-C-s",        spawn  "logout_prompt")

    , ("C-A-e",        spawn  "rofie")
    , ("C-A-p",        spawn  "get-class-name")
    , ("C-A-c",        spawn  "xcolor -s")
    , ("C-A-v",        spawn  "pavucontrol")

    , ("<XF86AudioRaiseVolume>", spawn  "audio inc")
    , ("<XF86AudioLowerVolume>", spawn  "audio dec")
    , ("<XF86AudioMute>",        spawn  "audio toggle")

    , ("C-A-v",        spawn  "pavucontrol")

    , ("M-f",          sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
    , ("M-s",          withFocused toggleFloat)
    , ("M-t",          sendMessage $ T.Toggle "tall")
    , ("M-m",          sendMessage $ T.Toggle "monocle")
    , ("M-<Space>",    sendMessage NextLayout)

    , ("A-<Tab>",      windows W.focusUp)

    , ("M-]",          nextWS)
    , ("M-[",          prevWS)
    , ("M-S-]",        shiftToNext <> nextWS)
    , ("M-S-[",        shiftToPrev <> prevWS)
    , ("M-.",          shiftNextScreen)
    , ("M-,",          shiftPrevScreen)
    , ("M-S-.",        shiftNextScreen <> nextScreen )
    , ("M-S-,",        shiftPrevScreen <> prevScreen )
    , ("M-`",          moveTo Next (Not emptyWS))
    , ("M-S-`",        moveTo Prev (Not emptyWS))

    , ("M-S-1",        shiftAndView 1)
    , ("M-S-2",        shiftAndView 2)
    , ("M-S-3",        shiftAndView 3)
    , ("M-S-4",        shiftAndView 4)
    , ("M-S-5",        shiftAndView 5)
    , ("M-S-6",        shiftAndView 6)
    , ("M-S-7",        shiftAndView 7)
    , ("M-S-8",        shiftAndView 8)
    , ("M-S-9",        shiftAndView 9)
    ]
