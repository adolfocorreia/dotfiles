{-
References:
- https://xmonad.org/TUTORIAL.html
- https://wiki.archlinux.org/title/xmonad
- https://xmonad.org/images/cheat/xmbindings.png
- https://unix.stackexchange.com/questions/288037/xmobar-does-not-appear-on-top-of-window-stack-when-xmonad-starts
-}


-- Imports

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GroupNavigation
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.SpawnOnce


-- Variables

myModMask :: KeyMask
myModMask = mod4Mask  -- Super/Windows key

myTerminal :: String
myTerminal = "alacritty"

myStartupHook :: X ()
myStartupHook = do
  -- Monitor configuration (check monitors with 'xrandr --listmonitors')
  spawnOnce "xrandr --output DP-0 --primary --left-of DP-3 --output DP-3 --auto &"
  -- Keyboard configuration: CapsLock as Ctrl, both Shifts to toggle CapsLock
  spawnOnce "setxkbmap -option ctrl:nocaps,shift:both_capslock &"
  -- Mouse cursor
  spawnOnce "xsetroot -cursor_name left_ptr &"
  -- Desktop background
  spawnOnce "feh --randomize --bg-fill ~/Pictures/Wallpapers/* &"
  -- Desktop compositor
  spawnOnce "picom &"

myKeys :: [(String, X ())]
myKeys =
  [ ("M-p",        spawn "rofi -show")
  , ("M-\\",       spawn "rofi -show")
  -- Alt-Tab
  , ("M1-<Tab>",   nextMatch Forward isOnAnyVisibleWS)
  , ("M1-S-<Tab>", nextMatch Backward isOnAnyVisibleWS)
  -- Cycle through non-empty workspaces
  , ("M-<Tab>",    moveTo Next NonEmptyWS)
  , ("M-S-<Tab>",  moveTo Prev NonEmptyWS)
  ]

myLayoutHook = avoidStruts (tall ||| full)
  where tall   = renamed [Replace "tall"] $ spc $ ResizableTall 1 (5/100) (60/100) []
        full   = renamed [Replace "full"] $ Full
        spc    = spacingRaw False border True border True
        border = Border gap gap gap gap
        gap    = 4


-- Theme colors

-- Nord
myRed    = "#bf616a"
myOrange = "#d08770"
myYellow = "#ebcb8b"
myGreen  = "#a3be8c"
myPink   = "#b48ead"
myBlue   = "#81a1c1"
myWhite  = "#d8dee9"


-- Main

main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar -x 0"
  xmproc1 <- spawnPipe "xmobar -x 1"
  xmonad $ docks $ ewmh desktopConfig
    { modMask            = myModMask
    , terminal           = myTerminal
    , focusedBorderColor = myRed
    , normalBorderColor  = myWhite
    , borderWidth        = 1
    , startupHook        = myStartupHook
    , layoutHook         = myLayoutHook
    , manageHook         = manageDocks <+> manageHook def
    , handleEventHook    = handleEventHook def <+> docksEventHook
    , logHook            = dynamicLogWithPP xmobarPP
        { ppOutput = \x -> hPutStrLn xmproc0 x
                        >> hPutStrLn xmproc1 x
        , ppSep     = " <fc=" ++ myWhite ++ ">|</fc> "
        , ppCurrent = xmobarColor myBlue "" . wrap ("<box type=Bottom width=1 mb=1 color=" ++ myBlue ++ ">[") "]</box>"
        , ppVisible = wrap "<box type=Bottom width=1 mb=1>(" ")</box>"
        , ppUrgent  = xmobarColor myRed myYellow . wrap "!" "!"
        , ppTitle   = xmobarColor myRed "" . shorten 60
        }
    } `additionalKeysP` myKeys
