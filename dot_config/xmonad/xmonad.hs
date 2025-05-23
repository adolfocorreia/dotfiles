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
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import System.Exit (exitSuccess)
import System.IO ()
import XMonad
  ( Default (def),
    Full (Full),
    IncMasterN (IncMasterN),
    KeyMask,
    Query,
    Resize (Expand, Shrink),
    WindowSet,
    WorkspaceId,
    X,
    XConfig
      ( borderWidth,
        focusedBorderColor,
        layoutHook,
        logHook,
        manageHook,
        modMask,
        normalBorderColor,
        startupHook,
        terminal,
        workspaces
      ),
    className,
    composeAll,
    doFloat,
    doShift,
    io,
    kill,
    mod1Mask,
    mod4Mask,
    sendMessage,
    spawn,
    title,
    windows,
    withFocused,
    xmonad,
    (-->),
    (<+>),
    (=?),
    (|||),
  )
import XMonad.Actions.CycleWS
  ( nextScreen,
    prevScreen,
    shiftNextScreen,
    shiftPrevScreen,
    toggleWS,
  )
import XMonad.Actions.DwmPromote (dwmpromote)
import XMonad.Actions.GroupNavigation
  ( Direction (Backward, Forward),
    isOnAnyVisibleWS,
    nextMatch,
  )
import XMonad.Actions.OnScreen (greedyViewOnScreen)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
  ( avoidStruts,
    docks,
    manageDocks,
  )
import XMonad.Hooks.ManageHelpers (doCenterFloat, doRectFloat)
import XMonad.Hooks.StatusBar (StatusBarConfig, statusBarPropTo, withSB)
import XMonad.Hooks.StatusBar.PP
  ( PP,
    ppCurrent,
    ppHidden,
    ppHiddenNoWindows,
    ppLayout,
    ppSep,
    ppTitle,
    ppUrgent,
    ppVisible,
    shorten,
    wrap,
    xmobarColor,
    xmobarPP,
    xmobarStrip,
  )
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile (ResizableTall (ResizableTall))
import XMonad.Layout.Spacing (Border (Border), spacingRaw)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)

-- Tokyo Night theme colors
myBlack, myBlue, myGreen, myOrange, myPink, myRed, myWhite, myYellow :: String
myBlack = "#24283b"
myBlue = "#7aa2f7"
myGreen = "#9ece6a"
myOrange = "#ff9e64"
myPink = "#bb9af7"
myRed = "#f7768e"
myWhite = "#c0caf5"
myYellow = "#e0af68"

myCurrentColor, myVisibleColor, myHiddenColor, myHiddenNoWindowsColor :: String
myCurrentColor = myPink
myVisibleColor = "#7dcfff"
myHiddenColor = myWhite
myHiddenNoWindowsColor = "#414868"

myBarBackgroundColor :: String
myBarBackgroundColor = "#292e42"

-- Base settings
myModMask :: KeyMask
myModMask = mod1Mask -- Alt key
-- myModMask = mod4Mask -- Super/Windows key

myTerminal :: String
myTerminal = "alacritty"

{- Default (not replaced nor removed) Xmonad key bindings
   References:
   - https://github.com/xmonad/xmonad/blob/master/src/XMonad/Config.hs
   - https://xmonad.org/images/cheat/xmbindings.png

   -- Launch terminal
   ("M-S-<Return>", spawn $ XMonad.terminal conf)
   -- Close the focused window
   ("M-S-c", kill)
   -- Rotate through the available layout algorithms
   ("M-<Space>", sendMessage NextLayout)
   -- Reset layouts on the current workspace to default
   ("M-S-<Space>", sendMessage NextLayout)

   -- Move focus up or down the window stack
   ("M-j", windows W.focusDown)
   ("M-k", windows W.focusUp)
   ("M-m", windows W.focusMaster)

   -- Modifying the window order
   ("M-S-j", windows W.swapDown)
   ("M-S-k", windows W.swapUp)

   -- Resizing the master/slave ratio
   ("M-h", sendMessage Shrink)
   ("M-l", sendMessage Expand)

   -- Increase or decrease number of windows in the master area
   ("M-,", sendMessage (IncMasterN 1))
   ("M-.", sendMessage (IncMasterN (-1)))

   -- Switch to workspace N
   ("M-[1..9]")
   -- Move client to workspace N
   ("M-S-[1..9]")

   -- Switch to screen 1, 2 or 3
   ("M-{w,e,r}")
   -- Move client to screen 1, 2 or 3
   ("M-S-{w,e,r}")
-}

-- Custom key bindings
-- Reference: https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Util-EZConfig.html
myAdditionalKeys :: [(String, X ())]
myAdditionalKeys =
  [ -- Spawn app launcher (rofi)
    ("M-\\", spawn "rofi -show"),
    -- Spawn file manager
    ("M-C-S-<Return>", spawn "pcmanfm"),
    -- Close the focused window
    ("M-S-q", kill),
    -- Restart xmonad
    ("M-C-S-q", spawn "xmonad --recompile && xmonad --restart"),
    -- Quit xmonad (this key binding should be hard to trigger by accident)
    ("M-<Pause>", io exitSuccess),
    -- Force window kill with xkill
    ("M-C-S-k", spawn "xkill"),
    -- Lock screen
    ("M-<Escape>", spawn "xset s activate"),
    -- Toggle floating state of focused window
    ("M-t", withFocused toggleFloat),
    -- Workspace 10/0 bindings
    ("M-0", windows $ W.greedyView $ myWorkspaces !! (10 - 1)),
    ("M-S-0", windows $ W.shift $ myWorkspaces !! (10 - 1)),
    -- TODO: add F1-12 workspaces
    -- Switch to previously displayed workspace
    ("M-<Backspace>", toggleWS),
    -- Swap master window
    ("M-<Return>", dwmpromote),
    -- Resize master/slave ratio
    ("M-.", sendMessage Expand),
    ("M-,", sendMessage Shrink),
    -- Increase or decrease number of windows in the master area
    ("M-S-.", sendMessage (IncMasterN 1)),
    ("M-S-,", sendMessage (IncMasterN (-1))),
    -- Switch focus to next monitor
    ("M-C-<Left>", prevScreen),
    ("M-C-<Right>", nextScreen),
    -- Shift window to next monitor
    ("M-S-h", shiftPrevScreen >> prevScreen),
    ("M-S-l", shiftNextScreen >> nextScreen),
    ("M-S-<Left>", shiftPrevScreen >> prevScreen),
    ("M-S-<Right>", shiftNextScreen >> nextScreen),
    -- Cycle through visible windows
    ("M1-<Tab>", nextMatch Forward isOnAnyVisibleWS),
    ("M1-S-<Tab>", nextMatch Backward isOnAnyVisibleWS)
    -- TODO: add key bindings description (see DescriptiveKeys and NamedActions packages)
  ]
  where
    toggleFloat w = windows $ \s ->
      if M.member w (W.floating s)
        then W.sink w s
        else W.float w (W.RationalRect (1 % 8) (1 % 8) (3 % 4) (3 % 4)) s

myRemoveKeys :: [String]
myRemoveKeys =
  [ -- Do not restart Xmonad with M-q
    "M-q",
    -- Do not use dmenu nor gmrun to launch apps
    "M-p",
    "M-S-p",
    -- Do not use refresh/resize windows
    "M-n",
    -- Do not use resize the master/slave ration with M-h and M-l
    "M-h",
    "M-l",
    -- TODO: make toggle bar work
    -- Do not toggle bar with M-b (defToggleStrutsKey)
    "M-b",
    -- Do not show default key bindings message
    "M-S-/",
    "M-?"
  ]

-- Startup processes
myStartupHook :: X ()
myStartupHook = do
  -- Show 'term' and 'time' workspaces at startup
  windows (greedyViewOnScreen 0 "term")
  windows (greedyViewOnScreen 1 "time")

-- Layout configuration
myLayoutHook :: ModifiedLayout _ _ _
myLayoutHook = avoidStruts (tall ||| full)
  where
    tall =
      renamed [Replace "tall"] $ spc $ ResizableTall 1 (5 / 100) (60 / 100) []
    full = renamed [Replace "full"] Full
    spc = spacingRaw False border True border True
    border = Border gap gap gap gap
    gap = 4

-- Workspaces configuration
myWorkspaces :: [String]
myWorkspaces = ["term", "www", "dev", "doc", "file", "mail", "chat", "note", "vid", "time"]

workspaceIcon :: String -> String
workspaceIcon "chat" = "<fn=2>\xf0b79</fn>"
workspaceIcon "dev"  = "<fn=2>\xf05c0</fn>"
workspaceIcon "doc"  = "<fn=2>\xf06d3</fn>"
workspaceIcon "file" = "<fn=2>\xf0770</fn>"
workspaceIcon "mail" = "<fn=2>\xf02ab</fn>"
workspaceIcon "note" = "<fn=2>\xf01c8</fn>"
workspaceIcon "term" = "<fn=2>\xf07b7</fn>"
workspaceIcon "time" = "<fn=2>\xf051b</fn>"
workspaceIcon "vid"  = "<fn=2>\xf05a0</fn>"
workspaceIcon "www"  = "<fn=2>\xf059f</fn>"
workspaceIcon str = ""

-- Windows management
-- Check classes using 'xprop' or 'xwininfo -root -tree | grep -i app_name'.
-- Note: some apps (e.g. firefox, libreoffice) change their classes after startup.
myManageHook :: Query (_ WindowSet)
myManageHook =
  composeAll
    . concat
    $ [ [className =? c --> doFloat | c <- myClassFloats],
        [className =? c --> doCenterFloat | c <- myClassCenterFloats],
        [className =? c --> doRectFloat (W.RationalRect (1 % 8) (1 % 8) (3 % 4) (3 % 4)) | c <- myClassResizeFloats],
        [title =? t --> doFloat | t <- myTitleFloats],
        [className =? c --> doShift ws | (ws, c) <- myClassShifts],
        [title =? t --> doShift ws | (ws, t) <- myTitleShifts]
      ]
  where
    myClassFloats =
      [ "copyq",
        "Gscreenshot",
        "SpeedCrunch",
        "Variety",
        "Xarchiver"
      ]
    myClassCenterFloats =
      [ "Btrfs Assistant",
        "feh",
        "Gpicview",
        "Gsmartcontrol",
        "Hdajackretask",
        "Pavucontrol",
        "superProductivity",
        "Systemadm",
        "Vorta",
        "Xdg-desktop-portal-gtk"
      ]
    myClassResizeFloats =
      [ "gksqt",
        "Matplotlib",
        "mpv",
        "vlc"
      ]
    myTitleFloats = []
    myClassShifts =
      [ -- Emacs
        ("dev", "Emacs"),
        -- Obsidian
        ("note", "obsidian"),
        -- Neovide
        ("dev", "neovide"),
        -- Super Productivity
        ("time", "superProductivity"),
        -- Visual Studio Code
        ("dev", "code"),
        -- Whatsapp (nativefier)
        ("chat", "whatsapp-nativefier-d40211"),
        -- Zoom
        ("vid", "zoom"),
        ("vid", "zoom ")
      ]
    myTitleShifts = []

-- Xmobar configuration
-- References:
-- - https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Hooks-StatusBar.html
-- - https://hackage.haskell.org/package/xmonad-contrib/docs/XMonad-Hooks-StatusBar-PP.html
-- - https://codeberg.org/jao/xmobar/src/branch/master/doc/window-managers.org
clickableWS :: WorkspaceId -> String
clickableWS ws = "<action=`xdotool key " ++ modKey ++ "+" ++ index ++ "`>" ++ icon ++ "</action>"
  where
    icon = workspaceIcon ws
    index = show $ fromJust $ M.lookup ws indices
    indices = M.fromList $ zip myWorkspaces $ [1 .. 9] ++ [0]
    modKey = if myModMask == mod4Mask then "super" else "alt"

myXmobarConfig :: Int -> PP
myXmobarConfig numChars =
  xmobarPP
    { ppSep = " | ",
      ppCurrent = xmobarColor myCurrentColor "" . boxed myCurrentColor . clickableWS,
      ppVisible = xmobarColor myVisibleColor "" . boxed myVisibleColor . clickableWS,
      ppHidden = xmobarColor myHiddenColor "" . clickableWS,
      ppHiddenNoWindows = xmobarColor myHiddenNoWindowsColor "" . clickableWS,
      ppUrgent = xmobarColor myRed myYellow . wrap "!" "!",
      ppLayout = clickableLayout,
      ppTitle = xmobarColor myCurrentColor "" . boxed myCurrentColor . shorten numChars . xmobarStrip
    }
  where
    clickableLayout = wrap "<action=`xdotool key super+shift+space`>" "</action>"
    boxed color = wrap ("<box type=Bottom width=3 color=" ++ color ++ ">") "</box>"

myStatusBar1, myStatusBar2 :: StatusBarConfig
myStatusBar1 = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0 ~/.config/xmobar/xmobarrc_primary" (pure $ myXmobarConfig 85)
myStatusBar2 = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 1 ~/.config/xmobar/xmobarrc_secondary" (pure $ myXmobarConfig 250)

-- TODO: Focus workspace on cursor hover
-- References:
-- - https://reddit.com/r/xmonad/comments/qi1tlm/focus_empty_workspace_on_cursor_hover
-- - https://hackage.haskell.org/package/X11/docs/Graphics-X11-Xlib-Extras.html

-- Main
main :: IO ()
main = do
  xmonad $
    docks $
      withSB (myStatusBar1 <> myStatusBar2) $
        ewmh
          desktopConfig
            { modMask = myModMask,
              terminal = myTerminal,
              focusedBorderColor = myCurrentColor,
              normalBorderColor = myBlack,
              borderWidth = 1,
              workspaces = myWorkspaces,
              startupHook = myStartupHook,
              layoutHook = myLayoutHook,
              manageHook = manageHook def <+> manageDocks <+> myManageHook,
              logHook =
                workspaceHistoryHook
                  >> updatePointer (0.25, 0.1) (0.0, 0.0)
            }
          `removeKeysP` myRemoveKeys
          `additionalKeysP` myAdditionalKeys
