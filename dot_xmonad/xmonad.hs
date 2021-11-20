{-
References:
- https://xmonad.org/TUTORIAL.html
- https://wiki.archlinux.org/title/xmonad
- https://xmonad.org/images/cheat/xmbindings.png
- https://unix.stackexchange.com/questions/288037/xmobar-does-not-appear-on-top-of-window-stack-when-xmonad-starts
-}
-- Reference: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/partial_type_signatures.html
{-# LANGUAGE PartialTypeSignatures #-}

-- Imports

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

myBlue, myGreen, myOrange, myPink, myRed, myWhite, myYellow :: String
myBlue = "#81a1c1"
myGreen = "#a3be8c"
myOrange = "#d08770"
myPink = "#b48ead"
myRed = "#bf616a"
myWhite = "#d8dee9"
myYellow = "#ebcb8b"

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

clickable :: WorkspaceId -> String
clickable ws = "<action=xdotool key super+" ++ ws ++ ">" ++ ws ++ "</action>"

myXmobarConfig :: Handle -> Handle -> PP
myXmobarConfig xm0 xm1 =
  xmobarPP
    { ppOutput = \x -> hPutStrLn xm0 x >> hPutStrLn xm1 x,
      ppSep = " <fc=" ++ myWhite ++ ">|</fc> ",
      ppCurrent =
        xmobarColor myBlue ""
          . wrap
            ("<box type=Bottom width=1 mb=1 color=" ++ myBlue ++ ">[")
            "]</box>",
      ppVisible = wrap "<box type=Bottom width=1 mb=1>(" ")</box>" . clickable,
      ppHidden = wrap " " " " . clickable,
      ppUrgent = xmobarColor myRed myYellow . wrap "!" "!",
      ppTitle = xmobarColor myRed ""
    }

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
            focusedBorderColor = myRed,
            normalBorderColor = myWhite,
            borderWidth = 2,
            startupHook = myStartupHook,
            layoutHook = myLayoutHook,
            manageHook = myManageHook <+> manageDocks <+> manageHook def,
            handleEventHook = handleEventHook def <+> docksEventHook,
            logHook = dynamicLogWithPP (myXmobarConfig xm0 xm1)
          }
        `additionalKeysP` myKeys
