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

import Control.Lens (indices)
import qualified Data.Map as M
import Data.Maybe
import System.IO (Handle, hPutStrLn)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GroupNavigation
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
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

myCurrentColor, myVisibleColor :: String
myCurrentColor = "#81a1c1"
myVisibleColor = "#8fbcbb"

-- Base settings
myModMask :: KeyMask
myModMask = mod4Mask -- Super/Windows key

myTerminal :: String
myTerminal = "alacritty"

-- Custom key bindings
-- Reference: https://github.com/xmonad/xmonad/blob/master/src/XMonad/Config.hs
myKeys :: [(String, X ())]
myKeys =
  [ ("M-p", spawn "rofi -show"),
    ("M-\\", spawn "rofi -show"),
    -- Close the focused window
    ("M-S-x", kill),
    -- Switch focus to next monitor
    ("M-C-h", prevScreen),
    ("M-C-l", nextScreen),
    -- Shift window to next monitor
    ("M-S-h", shiftPrevScreen),
    ("M-S-l", shiftNextScreen),
    -- Use Alt-Tab to cycle through visible windows
    ("M1-<Tab>", nextMatch Forward isOnAnyVisibleWS),
    ("M1-S-<Tab>", nextMatch Backward isOnAnyVisibleWS),
    -- Cycle through non-empty workspaces
    ("M-<Tab>", moveTo Next NonEmptyWS),
    ("M-S-<Tab>", moveTo Prev NonEmptyWS)
  ]

-- Startup processes
myStartupHook :: X ()
myStartupHook = do
  -- TODO: move X commands to X configuration files
  -- Monitor configuration (check monitors with 'xrandr --listmonitors')
  spawnOnce
    "xrandr --output DP-0 --primary --left-of DP-3 --output DP-3 --auto &"
  -- Keyboard configuration: CapsLock as Ctrl, both Shifts to toggle CapsLock
  spawnOnce "setxkbmap -option ctrl:nocaps,shift:both_capslock &"
  -- Session manager / polkit authentication agent
  spawnOnce "lxsession --session XMonad &"
  -- Desktop background
  spawnOnce "feh --randomize --bg-fill ~/Pictures/Wallpapers/* &"
  -- Desktop compositor
  spawnOnce "picom &"

-- Layout configuration
myLayoutHook :: ModifiedLayout _ _ _
myLayoutHook = avoidStruts (tall ||| full)
  where
    tall =
      renamed [Replace "tall"] $ spc $ ResizableTall 1 (5 / 100) (60 / 100) []
    full = renamed [Replace "full"] $ Full
    spc = spacingRaw False border True border True
    border = Border gap gap gap gap
    gap = 4

-- Workspaces configuration
myWorkspaces :: [String]
myWorkspaces = ["term", "www", "dev", "doc", "file", "mail", "chat", "mus", "vid", "conf"]

workspaceIcon :: String -> String
workspaceIcon "term" = "<fn=2>\xf07b7</fn>"
workspaceIcon "www" = "<fn=2>\xf059f</fn>"
workspaceIcon "dev" = "<fn=2>\xf05c0</fn>"
workspaceIcon "doc" = "<fn=2>\xf06d3</fn>"
workspaceIcon "file" = "<fn=2>\xf0770</fn>"
workspaceIcon "mail" = "<fn=2>\xf02ab</fn>"
workspaceIcon "chat" = "<fn=2>\xf0b79</fn>"
workspaceIcon "mus" = "<fn=2>\xf075a</fn>"
workspaceIcon "vid" = "<fn=2>\xf05a0</fn>"
workspaceIcon "conf" = "<fn=2>\xf0493</fn>"
workspaceIcon str = ""

-- Windows management
myManageHook :: Query (_ WindowSet)
myManageHook =
  composeAll
    . concat
    $ [ [className =? c --> doFloat | c <- myClassFloats],
        [title =? t --> doFloat | t <- myTitleFloats]
      ]
  where
    myClassFloats = ["mpv", "SpeedCrunch", "vlc"]
    myTitleFloats = []

-- Xmobar configuration
clickable :: (WorkspaceId, String) -> String
clickable (ws, icon) = "<action=xdotool key super+" ++ show index ++ ">" ++ icon ++ "</action>"
  where
    index = fromJust $ M.lookup ws indices
    indices = M.fromList $ zip myWorkspaces $ [1 .. 9] ++ [0]

myXmobarConfig :: Handle -> Handle -> PP
myXmobarConfig xm0 xm1 =
  xmobarPP
    { ppOutput = \x -> hPutStrLn xm0 x >> hPutStrLn xm1 x,
      ppSep = " | ",
      ppCurrent = xmobarColor myCurrentColor "" . prepareWS,
      ppVisible = xmobarColor myVisibleColor "" . prepareWS,
      ppHidden = prepareWS,
      ppUrgent = xmobarColor myRed myYellow . wrap "!" "!",
      ppTitle = xmobarColor myCurrentColor ""
    }
  where
    prepareWS ws = clickable (ws, workspaceIcon ws)

-- Main
main :: IO ()
main = do
  xm0 <- spawnPipe "xmobar -x 0"
  xm1 <- spawnPipe "xmobar -x 1"
  xmonad $
    docks $
      ewmh
        desktopConfig
          { modMask = myModMask,
            terminal = myTerminal,
            focusedBorderColor = myWhite,
            normalBorderColor = myBlack,
            borderWidth = 2,
            workspaces = myWorkspaces,
            startupHook = myStartupHook,
            layoutHook = myLayoutHook,
            manageHook = myManageHook <+> manageDocks <+> manageHook def,
            handleEventHook = handleEventHook def <+> docksEventHook,
            logHook = dynamicLogWithPP (myXmobarConfig xm0 xm1)
          }
        `additionalKeysP` myKeys
