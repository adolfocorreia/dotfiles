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
  ( ChangeLayout (NextLayout),
    Default (def),
    Full (Full),
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
    mod4Mask,
    sendMessage,
    spawn,
    title,
    windows,
    xmonad,
    (-->),
    (<+>),
    (=?),
    (|||),
  )
import XMonad.Actions.CycleWS
  ( Direction1D (Next, Prev),
    WSType (Not),
    emptyWS,
    moveTo,
    nextScreen,
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
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
  ( Direction1D (Next, Prev),
    avoidStruts,
    docks,
    manageDocks,
  )
import XMonad.Hooks.ManageHelpers (doCenterFloat, doRectFloat)
import XMonad.Hooks.StatusBar (StatusBarConfig, statusBarPropTo, withSB)
import XMonad.Hooks.StatusBar.PP
  ( PP,
    def,
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
    -- Spawn file manager (PCManFM)
    ("M-e", spawn "pcmanfm"),
    -- Close the focused window
    ("M-q", kill),
    -- Force window kill with xkill
    ("M-k", spawn "xkill"),
    -- Restart xmonad
    ("M-S-q", spawn "xmonad --recompile && xmonad --restart"),
    -- Quit xmonad (this key binding should be hard to use)
    ("M-<Pause>", io exitSuccess),
    -- Lock screen
    ("M-<Escape>", spawn "xset s activate"),
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
    ("M-<Tab>", moveTo Next (Not emptyWS)),
    ("M-S-<Tab>", moveTo Prev (Not emptyWS)),
    -- Volume controls
    ("<XF86AudioLowerVolume>", spawn "pamixer --decrease 5 --unmute"),
    ("<XF86AudioRaiseVolume>", spawn "pamixer --increase 5 --unmute"),
    ("<XF86AudioMute>", spawn "pamixer --toggle-mute")
    -- TODO: add bindings to change desktop background
  ]

myRemoveKeys :: [String]
myRemoveKeys =
  [ -- Win+Space is used in X to toggle keyboard layouts
    "M-<Space>"
  ]

-- System tray
-- TODO: move config to .stalonetrayrc
mySystemTrayCommand :: String
mySystemTrayCommand = unwords ["stalonetray", arguments, "&"]
  where
    arguments = unwords [backgroundArg, geometryArg, growGravityArg, iconGravityArg, iconSizeArg, slotSizeArg]
    backgroundArg = "--background '" ++ myBarBackgroundColor ++ "'"
    geometryArg = "--geometry 1x1+" ++ show x ++ "+" ++ show y
    growGravityArg = "--grow-gravity E"
    iconGravityArg = "--icon-gravity E"
    iconSizeArg = "--icon-size " ++ show iconSize
    slotSizeArg = "--slot-size " ++ show (iconSize + 2)
    iconSize = 18
    x = monitor1w + monitor2w - iconSize - 4
    y = (monitor1h - monitor2h) `div` 2 + 1
    monitor1w = 2560
    monitor1h = 1440
    monitor2w = 2560
    monitor2h = 1440

-- Startup processes
myStartupHook :: X ()
myStartupHook = do
  -- TODO: evaluate moving these processes to a lxsession (e.g. to handle autostart on crashes)
  -- - https://wiki.lxde.org/en/index.php?title=LXSession
  -- - https://github.com/lxde/lxsession/blob/master/data/desktop.conf.example
  -- Session manager / polkit authentication agent
  spawnOnce "lxsession --session XMonad &"
  -- Desktop compositor
  spawnOnce "picom &"
  -- Notification manager
  spawnOnce "dunst &"
  -- Icon tray
  spawnOnce mySystemTrayCommand
  -- Desktop background
  spawnOnce "feh --randomize --bg-fill /usr/share/backgrounds/**/*.jpg &"
  spawnOnce "variety &"
  -- Clipboard manager
  spawnOnce "copyq &"
  -- Removable media manager
  spawnOnce "udiskie --tray &"
  -- Screen locker
  spawnOnce "betterlockscreen --update ~/Wallpapers &"

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
        "Variety"
      ]
    myClassCenterFloats =
      [ "Hdajackretask",
        "Pavucontrol",
        "Xarchiver",
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
        -- Visual Studio Code
        ("dev", "code"),
        -- Lyx
        ("doc", "lyx"),
        -- PCManFM
        ("file", "Pcmanfm"),
        -- Chromium
        ("mail", "Chromium"),
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
clickableWS ws = "<action=`xdotool key super+" ++ show index ++ "`>" ++ icon ++ "</action>"
  where
    icon = workspaceIcon ws
    index = fromJust $ M.lookup ws indices
    indices = M.fromList $ zip myWorkspaces $ [1 .. 9] ++ [0]

myXmobarConfig :: PP
myXmobarConfig =
  xmobarPP
    { ppSep = " | ",
      ppCurrent = xmobarColor myCurrentColor "" . boxed myCurrentColor . clickableWS,
      ppVisible = xmobarColor myVisibleColor "" . boxed myVisibleColor . clickableWS,
      ppHidden = xmobarColor myHiddenColor "" . clickableWS,
      ppHiddenNoWindows = xmobarColor myHiddenNoWindowsColor "" . clickableWS,
      ppUrgent = xmobarColor myRed myYellow . wrap "!" "!",
      ppLayout = clickableLayout,
      ppTitle = xmobarColor myCurrentColor "" . boxed myCurrentColor . shorten 85 . xmobarStrip
    }
  where
    clickableLayout = wrap "<action=`xdotool key super+shift+space`>" "</action>"
    boxed color = wrap ("<box type=Bottom width=3 color=" ++ color ++ ">") "</box>"

myStatusBar1, myStatusBar2 :: StatusBarConfig
myStatusBar1 = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0 ~/.config/xmobar/xmobarrc_primary" (pure myXmobarConfig)
myStatusBar2 = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 1 ~/.config/xmobar/xmobarrc_secondary" (pure myXmobarConfig)

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
