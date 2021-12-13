{-
Xmonad references:
- https://xmonad.org/TUTORIAL.html
- https://wiki.archlinux.org/title/xmonad
- https://xmonad.org/images/cheat/xmbindings.png
- https://unix.stackexchange.com/questions/288037/xmobar-does-not-appear-on-top-of-window-stack-when-xmonad-starts

Xmonad configuration examples:
- https://github.com/xkozlov1/dotfiles/blob/master/xmonad/xmonad.hs

Partial type signature reference:
- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/partial_type_signatures.html
-}
{-# LANGUAGE PartialTypeSignatures #-}

-- Imports

import qualified Data.Map as M
import Data.Maybe
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.GroupNavigation
import XMonad.Actions.UpdatePointer
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

-- Nord theme colors
myBlack, myBlue, myGreen, myOrange, myPink, myRed, myWhite, myYellow :: String
myBlack = "#2e3440"
myBlue = "#81a1c1"
myGreen = "#a3be8c"
myOrange = "#d08770"
myPink = "#b48ead"
myRed = "#bf616a"
myWhite = "#d8dee9"
myYellow = "#ebcb8b"

myCurrentColor, myVisibleColor, myHiddenColor, myHiddenNoWindowsColor :: String
myCurrentColor = myPink
myVisibleColor = "#8fbcbb"
myHiddenColor = myWhite
myHiddenNoWindowsColor = "#4c566a"

myBarBackgroundColor :: String
myBarBackgroundColor = "#3b4242"

-- Base settings
myModMask :: KeyMask
myModMask = mod4Mask -- Super/Windows key

myTerminal :: String
myTerminal = "alacritty"

-- Custom key bindings
-- References:
-- - https://github.com/xmonad/xmonad/blob/master/src/XMonad/Config.hs
-- - https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Util-EZConfig.html
myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
  [ -- Spawn app launcher (rofi)
    ("M-p", spawn "rofi -show"),
    ("M-\\", spawn "rofi -show"),
    -- Close the focused window
    ("M-S-x", kill),
    -- Toggle workspace layout
    ("M-S-<Space>", sendMessage NextLayout),
    -- Workspace 10/0 bindings
    ("M-0", windows $ W.greedyView $ myWorkspaces !! (10 - 1)),
    ("M-S-0", windows $ W.shift $ myWorkspaces !! (10 - 1)),
    -- TODO: add F1-12 workspaces
    -- Switch to previously displayed workspace
    ("M-<Backspace>", toggleWS),
    -- Swap master window
    ("M-<Return>", dwmpromote),
    -- Swap windows
    ("M-S-<Down>", windows W.swapDown),
    ("M-S-<Up>", windows W.swapUp),
    -- Resize master/slave ratio
    ("M-<Left>", sendMessage Shrink),
    ("M-<Right>", sendMessage Expand),
    -- Switch windows focus
    ("M-<Down>", windows W.focusDown),
    ("M-<Up>", windows W.focusUp),
    -- Switch focus to next monitor
    ("M-C-h", prevScreen),
    ("M-C-l", nextScreen),
    ("M-C-<Left>", prevScreen),
    ("M-C-<Right>", nextScreen),
    -- Shift window to next monitor
    ("M-S-h", shiftPrevScreen >> prevScreen),
    ("M-S-l", shiftNextScreen >> nextScreen),
    ("M-S-<Left>", shiftPrevScreen >> prevScreen),
    ("M-S-<Right>", shiftNextScreen >> nextScreen),
    -- Use Alt-Tab to cycle through visible windows
    ("M1-<Tab>", nextMatch Forward isOnAnyVisibleWS),
    ("M1-S-<Tab>", nextMatch Backward isOnAnyVisibleWS),
    -- Cycle through non-empty workspaces
    ("M-<Tab>", moveTo Next NonEmptyWS),
    ("M-S-<Tab>", moveTo Prev NonEmptyWS),
    -- Volume controls
    ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%- unmute"),
    ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+ unmute"),
    ("<XF86AudioMute>", spawn "amixer set Master toggle")
  ]

myRemoveKeys :: [String]
myRemoveKeys =
  [ -- Win+Space is used to toggle keyboard layouts
    "M-<Space>"
  ]

-- System tray
mySystemTrayCommand :: String
mySystemTrayCommand = unwords ["stalonetray", arguments, "&"]
  where
    arguments = unwords [backgroundArg, geometryArg, growGravityArg, iconGravityArg, iconSizeArg]
    backgroundArg = "--background '" ++ myBarBackgroundColor ++ "'"
    geometryArg = "--geometry 1x1+" ++ show (monitor1 + monitor2 - iconSize - 4) ++ "+3"
    growGravityArg = "--grow-gravity E"
    iconGravityArg = "--icon-gravity E"
    iconSizeArg = "--icon-size " ++ show iconSize
    iconSize = 18
    monitor1 = 2560
    monitor2 = 1440

-- Startup processes
myStartupHook :: X ()
myStartupHook = do
  -- Session manager / polkit authentication agent
  spawnOnce "lxsession --session XMonad &"
  -- Desktop background
  spawnOnce "feh --randomize --bg-fill /usr/share/backgrounds/**/*.jpg &"
  spawnOnce "variety &"
  -- Desktop compositor
  spawnOnce "picom &"
  -- Icon tray
  spawnOnce mySystemTrayCommand

-- Layout configuration
myLayoutHook :: ModifiedLayout _ _ _
myLayoutHook = avoidStruts (tall ||| full)
  where
    tall =
      renamed [Replace "tall"] $ spc $ ResizableTall 1 (5 / 100) (60 / 100) []
    full = renamed [Replace "full"] Full
    spc = spacingRaw False border True border True
    border = Border gap gap gap gap
    gap = 8

-- Workspaces configuration
myWorkspaces :: [String]
myWorkspaces = ["term", "www", "dev", "doc", "file", "mail", "chat", "mus", "vid", "conf"]

workspaceIcon :: String -> String
workspaceIcon "dev" = "<fn=2>\xf05c0</fn>"
workspaceIcon "doc" = "<fn=2>\xf06d3</fn>"
workspaceIcon "mus" = "<fn=2>\xf075a</fn>"
workspaceIcon "vid" = "<fn=2>\xf05a0</fn>"
workspaceIcon "www" = "<fn=2>\xf059f</fn>"
workspaceIcon "chat" = "<fn=2>\xf0b79</fn>"
workspaceIcon "conf" = "<fn=2>\xf0493</fn>"
workspaceIcon "file" = "<fn=2>\xf0770</fn>"
workspaceIcon "mail" = "<fn=2>\xf02ab</fn>"
workspaceIcon "term" = "<fn=2>\xf07b7</fn>"
workspaceIcon str = ""

-- Windows management
-- Check classes using 'xprop' or 'xwininfo -root -tree | grep -i app_name'.
-- Note: some apps (e.g. firefox, libreoffice) change their classes after startup.
myManageHook :: Query (_ WindowSet)
myManageHook =
  composeAll
    . concat
    $ [ [className =? c --> doFloat | c <- myClassFloats],
        [title =? t --> doFloat | t <- myTitleFloats],
        [className =? c --> doShift ws | (ws, c) <- myClassShifts],
        [title =? t --> doShift ws | (ws, t) <- myTitleShifts]
      ]
  where
    myClassFloats = ["mpv", "SpeedCrunch", "Variety", "vlc"]
    myTitleFloats = []
    myClassShifts =
      [ -- Brave
        ("www", "Brave-browser"),
        -- Firefox
        ("www", "Firefox"),
        ("www", "firefox"),
        -- Visual Studio Code
        ("dev", "code-oss"),
        -- Emacs
        ("dev", "Emacs"),
        -- LibreOffice
        ("doc", "Soffice"),
        ("doc", "libreoffice-calc"),
        ("doc", "libreoffice-writer"),
        ("doc", "libreoffice-startcenter"),
        -- Lyx
        ("doc", "lyx"),
        -- PCManFM
        ("file", "Pcmanfm"),
        -- Chromium
        ("mail", "Chromium"),
        -- Whatsapp (nativefier)
        ("chat", "whatsapp-nativefier-d40211"),
        -- Zoom
        ("vid", "zoom")
      ]
    myTitleShifts = []

-- Xmobar configuration
-- Reference:  https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Hooks-DynamicLog.html
clickableWS :: WorkspaceId -> String
clickableWS ws = "<action=`xdotool key super+" ++ show index ++ "`>" ++ icon ++ "</action>"
  where
    icon = workspaceIcon ws
    index = fromJust $ M.lookup ws indices
    indices = M.fromList $ zip myWorkspaces $ [1 .. 9] ++ [0]

myXmobarConfig :: Handle -> Handle -> PP
myXmobarConfig xm0 xm1 =
  xmobarPP
    { ppOutput = \x -> hPutStrLn xm0 x >> hPutStrLn xm1 x,
      ppSep = " | ",
      ppCurrent = xmobarColor myCurrentColor "" . boxed myCurrentColor . clickableWS,
      ppVisible = xmobarColor myVisibleColor "" . boxed myVisibleColor . clickableWS,
      ppHidden = xmobarColor myHiddenColor "" . clickableWS,
      ppHiddenNoWindows = xmobarColor myHiddenNoWindowsColor "" . clickableWS,
      ppUrgent = xmobarColor myRed myYellow . wrap "!" "!",
      ppLayout = clickableLayout,
      ppTitle = xmobarColor myCurrentColor "" . boxed myCurrentColor
    }
  where
    clickableLayout = wrap "<action=`xdotool key super+shift+space`>" "</action>"
    boxed color = wrap ("<box type=Bottom width=3 color=" ++ color ++ ">") "</box>"

-- Main
main :: IO ()
main = do
  xm0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc_primary"
  xm1 <- spawnPipe "xmobar -x 1 ~/.config/xmobar/xmobarrc_secondary"
  xmonad $
    docks $
      ewmh
        desktopConfig
          { modMask = myModMask,
            terminal = myTerminal,
            focusedBorderColor = myCurrentColor,
            normalBorderColor = myBlack,
            borderWidth = 3,
            workspaces = myWorkspaces,
            startupHook = myStartupHook,
            layoutHook = myLayoutHook,
            manageHook = myManageHook <+> manageDocks <+> manageHook def,
            handleEventHook = handleEventHook def <+> docksEventHook,
            logHook =
              dynamicLogWithPP (myXmobarConfig xm0 xm1)
                >> workspaceHistoryHook
                >> updatePointer (0.25, 0.05) (0.0, 0.0)
          }
        `removeKeysP` myRemoveKeys
        `additionalKeysP` myAdditionalKeys
