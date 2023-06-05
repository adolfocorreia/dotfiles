/*
Examples:
- https://github.com/dalyIsaac/workspacer-config/blob/main/workspacer.config.csx
- https://github.com/system32uwu/dotfiles/blob/main/windows10/.workspacer/workspacer.config.csx
- https://gist.github.com/haoxiangliew/fcb925d1e6987d0e1824848cf9659730
*/

#r "C:/apps/workspacer/workspacer.Shared.dll"
#r "C:/apps/workspacer/plugins/workspacer.ActionMenu/workspacer.ActionMenu.dll"
#r "C:/apps/workspacer/plugins/workspacer.Bar/workspacer.Bar.dll"
#r "C:/apps/workspacer/plugins/workspacer.FocusIndicator/workspacer.FocusIndicator.dll"
#r "C:/apps/workspacer/plugins/workspacer.Gap/workspacer.Gap.dll"
#r "C:/apps/workspacer/plugins/workspacer.TitleBar/workspacer.TitleBar.dll"
#r "C:/apps/workspacer/Microsoft.VisualBasic.dll"
#r "C:/apps/workspacer/System.Diagnostics.PerformanceCounter.dll"


using System;
using System.Collections.Generic;
using System.Linq;
using workspacer;
using workspacer.ActionMenu;
using workspacer.Bar;
using workspacer.Bar.Widgets;
using workspacer.FocusIndicator;
using workspacer.Gap;
using workspacer.TitleBar;


class ColorFocusedMonitorWidget : FocusedMonitorWidget {
    public Color ForegroundColor { get; set; }
    public override IBarWidgetPart[] GetParts() {
        string text = Context.MonitorContainer.FocusedMonitor == Context.Monitor ? FocusedText : UnfocusedText;
        return Parts(Part(text, fore: ForegroundColor, fontname: FontName));
    }
}


Action<IConfigContext> doConfig = (context) => {

    /* Tokyo Night theme */
    Color backgroundColor = new Color(0x1f, 0x23, 0x35);
    Color foregroundColor = new Color(0xa9, 0xb1, 0xd6);
    Color gray            = new Color(0x41, 0x48, 0x68);
    Color red             = new Color(0xf7, 0x76, 0x8e);
    Color yellow          = new Color(0xe0, 0xaf, 0x68);
    Color orange          = new Color(0xff, 0x9e, 0x64);
    Color green           = new Color(0x9e, 0xce, 0x6a);
    Color blue            = new Color(0x7d, 0xa2, 0xf7);
    Color purple          = new Color(0xbb, 0x9a, 0xf7);
    Color teal            = new Color(0x73, 0xda, 0xca);

    int barHeight = 16;
    string fontName = "Hack Nerd Font";
    int fontSize = 9;

    KeyModifiers mod   = KeyModifiers.Alt;
    KeyModifiers modC  = mod | KeyModifiers.Control;
    KeyModifiers modS  = mod | KeyModifiers.Shift;
    KeyModifiers modCS = mod | KeyModifiers.Control | KeyModifiers.Shift;

    string ws_mail   = "mail";
    string ws_chat   = "chat";
    string ws_apps   = "apps";
    int ws_mail_num  = 10;
    int ws_chat_num  = 11;
    int ws_apps_num  = 12;

    /* Disable auto-updates */
    context.Branch = Branch.None;

    context.CanMinimizeWindows = true;

    /* Keybindings */
    /* Reference: https://workspacer.org/keybindings */
    Action setKeybindings = () => {
        IKeybindManager k = context.Keybinds;
        IWorkspaceManager w = context.Workspaces;
        IWorkspaceContainer c = context.WorkspaceContainer;

        k.UnsubscribeAll();

        /* Workspacer does not work well with Win key as a modifier:
           https://github.com/workspacer/workspacer/issues/110 */

        /* Mouse keybindings */
        k.Subscribe(MouseEvent.LButtonDown, () => w.SwitchFocusedMonitorToMouseLocation());

        /* Action keybindings */
        k.Subscribe(modS,  Keys.Enter, () => System.Diagnostics.Process.Start("wt.exe"), "launch terminal");
        k.Subscribe(modS,  Keys.C,     () => w.FocusedWorkspace.CloseFocusedWindow(),    "close focused window");
        k.Subscribe(modS,  Keys.Q,     () => context.Restart(),                          "restart workspacer");

        /* Window keybindings */
        // k.Subscribe(mod,  Keys.J,         () => w.FocusedWorkspace.FocusNextWindow(),                 "focus next window");
        // k.Subscribe(mod,  Keys.K,         () => w.FocusedWorkspace.FocusPreviousWindow(),             "focus previous window");
        // k.Subscribe(modS, Keys.J,         () => w.FocusedWorkspace.SwapFocusAndNextWindow(),          "swap focus and next window");
        // k.Subscribe(modS, Keys.K,         () => w.FocusedWorkspace.SwapFocusAndPreviousWindow(),      "swap focus and previous window");
        k.Subscribe(mod,  Keys.Oemcomma,  () => w.FocusedWorkspace.ShrinkPrimaryArea(),               "shrink primary area");
        k.Subscribe(mod,  Keys.OemPeriod, () => w.FocusedWorkspace.ExpandPrimaryArea(),               "expand primary area");
        k.Subscribe(modS, Keys.Oemcomma,  () => w.FocusedWorkspace.DecrementNumberOfPrimaryWindows(), "decrement # primary windows");
        k.Subscribe(modS, Keys.OemPeriod, () => w.FocusedWorkspace.IncrementNumberOfPrimaryWindows(), "increment # primary windows");
        k.Subscribe(mod,  Keys.T,         () => context.Windows.ToggleFocusedWindowTiling(),          "toggle tiling for focused window");

        k.Subscribe(mod, Keys.Enter, () => {
            IList<IWindow> windows = w.FocusedWorkspace.ManagedWindows;
            IWindow primary = windows[0];
            IWindow focused = windows.FirstOrDefault(w => w.IsFocused);
            if (primary.Equals(focused)) {
                w.FocusedWorkspace.SwapFocusAndNextWindow();
            } else {
                w.FocusedWorkspace.SwapFocusAndPrimaryWindow();
            }
        }, "swap focus and primary window");

        /* Workspace keybindings */
        k.Subscribe(mod,  Keys.Back, () => w.SwitchToLastFocusedWorkspace(), "switch to last focused workspace");

        k.Subscribe(mod,  Keys.D1,       () => w.SwitchToWorkspace(0),            "switch to workspace 1");
        k.Subscribe(mod,  Keys.D2,       () => w.SwitchToWorkspace(1),            "switch to workspace 2");
        k.Subscribe(mod,  Keys.D3,       () => w.SwitchToWorkspace(2),            "switch to workspace 3");
        k.Subscribe(mod,  Keys.D4,       () => w.SwitchToWorkspace(3),            "switch to workspace 4");
        k.Subscribe(mod,  Keys.D5,       () => w.SwitchToWorkspace(4),            "switch to workspace 5");
        k.Subscribe(mod,  Keys.D8,       () => w.SwitchToWorkspace(5),            "switch to workspace 6");
        k.Subscribe(mod,  Keys.D9,       () => w.SwitchToWorkspace(6),            "switch to workspace 7");
        k.Subscribe(mod,  Keys.D0,       () => w.SwitchToWorkspace(7),            "switch to workspace 8");
        k.Subscribe(mod,  Keys.OemMinus, () => w.SwitchToWorkspace(8),            "switch to workspace 9");
        k.Subscribe(mod,  Keys.Oemplus,  () => w.SwitchToWorkspace(9),            "switch to workspace 10");
        k.Subscribe(modS, Keys.D1,       () => w.MoveFocusedWindowToWorkspace(0), "move focused window to workspace 1");
        k.Subscribe(modS, Keys.D2,       () => w.MoveFocusedWindowToWorkspace(1), "move focused window to workspace 2");
        k.Subscribe(modS, Keys.D3,       () => w.MoveFocusedWindowToWorkspace(2), "move focused window to workspace 3");
        k.Subscribe(modS, Keys.D4,       () => w.MoveFocusedWindowToWorkspace(3), "move focused window to workspace 4");
        k.Subscribe(modS, Keys.D5,       () => w.MoveFocusedWindowToWorkspace(4), "move focused window to workspace 5");
        k.Subscribe(modS, Keys.D8,       () => w.MoveFocusedWindowToWorkspace(5), "move focused window to workspace 6");
        k.Subscribe(modS, Keys.D9,       () => w.MoveFocusedWindowToWorkspace(6), "move focused window to workspace 7");
        k.Subscribe(modS, Keys.D0,       () => w.MoveFocusedWindowToWorkspace(7), "move focused window to workspace 8");
        k.Subscribe(modS, Keys.OemMinus, () => w.MoveFocusedWindowToWorkspace(8), "move focused window to workspace 9");
        k.Subscribe(modS, Keys.Oemplus,  () => w.MoveFocusedWindowToWorkspace(9), "move focused window to workspace 10");

        k.Subscribe(mod,  Keys.D6,       () => w.SwitchToWorkspace(ws_mail_num),            "switch to workspace mail");
        k.Subscribe(modS, Keys.D6,       () => w.MoveFocusedWindowToWorkspace(ws_mail_num), "move focused window to workspace mail");
        k.Subscribe(mod,  Keys.D7,       () => w.SwitchToWorkspace(ws_chat_num),            "switch to workspace chat");
        k.Subscribe(modS, Keys.D7,       () => w.MoveFocusedWindowToWorkspace(ws_chat_num), "move focused window to workspace chat");

        k.Subscribe(mod,  Keys.W, () => w.SwitchFocusedMonitor(2),              "switch to monitor 1");
        k.Subscribe(mod,  Keys.E, () => w.SwitchFocusedMonitor(0),              "switch to monitor 2");
        k.Subscribe(mod,  Keys.R, () => w.SwitchFocusedMonitor(1),              "switch to monitor 3");
        k.Subscribe(modS, Keys.W, () => w.MoveFocusedWindowToMonitor(2),        "move focused window to monitor 1");
        k.Subscribe(modS, Keys.E, () => w.MoveFocusedWindowToMonitor(0),        "move focused window to monitor 2");
        k.Subscribe(modS, Keys.R, () => w.MoveFocusedWindowToMonitor(1),        "move focused window to monitor 3");

        // k.Subscribe(mod,  Keys.H, () => w.SwitchFocusToPreviousMonitor(),       "switch to previous monitor");
        // k.Subscribe(mod,  Keys.L, () => w.SwitchFocusToNextMonitor(),           "switch to next monitor");
        k.Subscribe(modS, Keys.H,     () => w.MoveFocusedWindowToPreviousMonitor(), "move focused window to previous monitor");
        k.Subscribe(modS, Keys.L,     () => w.MoveFocusedWindowToNextMonitor(),     "move focused window to next monitor");
        k.Subscribe(modS, Keys.Left,  () => w.MoveFocusedWindowToPreviousMonitor(), "move focused window to previous monitor");
        k.Subscribe(modS, Keys.Right, () => w.MoveFocusedWindowToNextMonitor(),     "move focused window to next monitor");

        /* Debug keybindings */
        k.Subscribe(mod,  Keys.O, () => context.Windows.DumpWindowDebugOutput(),            "dump debug info to console for all windows");
        k.Subscribe(modS, Keys.O, () => context.Windows.DumpWindowUnderCursorDebugOutput(), "dump debug info to console for window under cursor");
        k.Subscribe(modS, Keys.I, () => context.ToggleConsoleWindow(),                      "toggle debug console");
        // k.Subscribe(modS, Keys.OemQuestion, () => k.ShowKeybindDialog(),                    "open keybind window");
    };
    setKeybindings();


    /* Menu bar */
    context.AddBar(new BarPluginConfig() {
        BarHeight = barHeight,
        Background = backgroundColor,
        DefaultWidgetBackground = backgroundColor,
        DefaultWidgetForeground = foregroundColor,
        FontSize = fontSize,
        FontName = fontName,
        /* Icon databases:
         * Nerd Fonts: nerdfonts.com/cheat-sheet
         * Font Awesome: fontawesome.com
         * Used icons:
         * - nf-md-monitor:    \udb80\udf79
         * - nf-fa-star:       \uf005
         * - nf-fa-laptop:     \uf109
         * - nf-md-sd:         \udb81\udc79
         * - nf-md-battery_90: \udb80\udc82
         * - nf-fa-calendar_o: \uf133
         * - nf-fa-star:       \uf017
         * */
        LeftWidgets = () => new IBarWidget[] {
            new TextWidget(" 󰍹 "),
            new WorkspaceWidget() {
                WorkspaceHasFocusColor = yellow,
                WorkspaceEmptyColor = gray,
                WorkspaceIndicatingBackColor = teal },
            new ActiveLayoutWidget() { LeftPadding = "[", RightPadding = "]" },
            new TextWidget(" | "),
            new ColorFocusedMonitorWidget() {
                FocusedText = "  ",
                ForegroundColor = yellow },
            new TitleWidget() {
                IsShortTitle = true,
                WindowHasFocusColor = yellow,
                NoWindowMessage = "-" },
            new ColorFocusedMonitorWidget() {
                FocusedText = "  ",
                ForegroundColor = yellow },
        },
        RightWidgets = () => new IBarWidget[] {
            new CpuPerformanceWidget() { Interval = 1000*10, StringFormat = "{0}%" },
            new TextWidget(" "),
            new MemoryPerformanceWidget() { Interval = 1000*10, StringFormat = "󰑹{0}%" },
            /* TODO: Change icon according to battery level */
            new TextWidget(" 󰂂"),
            new BatteryWidget() {
                LowChargeColor = red,
                MedChargeColor = yellow,
                HighChargeColor = green },
            new TimeWidget(1000, "  ddd dd.MMM.yyyy   HH:mm "),
        },
    });


    /* Window focus indicator */
    context.AddFocusIndicator(new FocusIndicatorPluginConfig() {
        BorderColor = red,
        BorderSize = 8,
        TimeToShow = 400,
    });


    /* Gap */
    int gap = 4;
    context.AddGap(new GapPluginConfig() {
        InnerGap = gap,
        OuterGap = gap/2,
        Delta    = gap/2,
    });


    /* Remove windows title bar */
    TitleBarPluginConfig titleBarPluginConfig = new TitleBarPluginConfig(new TitleBarStyle(
        showTitleBar: true,
        showSizingBorder: true
    ));
    context.AddTitleBar(titleBarPluginConfig);


    /* Action menu */
    context.AddActionMenu(new ActionMenuPluginConfig() {
        Background      = backgroundColor,
        FontName        = fontName,
        FontSize        = fontSize,
        MenuWidth       = 1000,
        RegisterKeybind = true,
        KeybindMod      = mod,
        KeybindKey      = Keys.P,
    });


    /* Workspaces */
    Func<ILayoutEngine[]> defaultLayouts = () => new ILayoutEngine[] {
        /* Arguments:
           - number of windows in primary zone
           - proportion of primary zone
           - increment proportion
           - reverse primary and secondary zones */
        new TallLayoutEngine(1, 0.6, 0.025, false),
        new HorzLayoutEngine(),
        /* https://github.com/workspacer/workspacer/issues/262 */
        /* new FullLayoutEngine(), */
    };
    (string, ILayoutEngine[])[] workspaces = {
        ("1st",    defaultLayouts()),
        ("2nd",    defaultLayouts()),
        ("3rd",    defaultLayouts()),
        ("4th",    defaultLayouts()),
        ("5th",    defaultLayouts()),
        ("6th",    defaultLayouts()),
        ("7th",    defaultLayouts()),
        ("8th",    defaultLayouts()),
        ("9th",    defaultLayouts()),
        ("10th",   defaultLayouts()),
        (ws_mail,  defaultLayouts()),
        (ws_chat,  defaultLayouts()),
        (ws_apps,  new ILayoutEngine[] { new VertLayoutEngine(), new FullLayoutEngine() }),
    };
    foreach ((string name, ILayoutEngine[] layouts) in workspaces)  {
        context.WorkspaceContainer.CreateWorkspace(name, layouts);
    }


    /* Filters & Routes */

    // Workspacer cannot properly filter UWP applications:
    // https://github.com/workspacer/workspacer/issues/120
    /* context.WindowRouter.IgnoreProcessName("ApplicationFrameHost"); */

    // MSI installer
    context.WindowRouter.IgnoreWindowClass("MsiDialogCloseClass");

    // 7zip
    context.WindowRouter.IgnoreWindowClass("#32770");
    // AHK
    context.WindowRouter.IgnoreProcessName("AutoHotkey64");
    context.WindowRouter.IgnoreProcessName("AutoHotkeyUX");
    // Ant Renamer
    context.WindowRouter.IgnoreProcessName("Renamer");
    // BIG-IP
    context.WindowRouter.IgnoreProcessName("f5fpclientW");
    // Bloomberg
    context.WindowRouter.AddFilter((w) => !(
        w.ProcessName.Equals("bplus.wtk2") && !w.Class.Equals("wdm-DesktopWindow")
    ));
    context.WindowRouter.AddFilter((w) => !(
        w.ProcessName.Equals("bplus.wtk2") && w.Title.StartsWith("BLOOMBERG")
    ));
    // Colorpicker
    context.WindowRouter.IgnoreProcessName("Colorpicker");
    // DBeaver
    context.WindowRouter.AddFilter((w) => !(
        w.ProcessName.Equals("dbeaver") && w.Class.Equals("Static")
    ));
    // Explorer (operation status window)
    context.WindowRouter.IgnoreWindowClass("OperationStatusWindow");
    // git credential helper
    context.WindowRouter.IgnoreWindowClass("CredentialHelperSelector");
    // Jitsi (sharing window)
    context.WindowRouter.AddFilter((w) => !(
        w.Class.Equals("Chrome_WidgetWin_1") && (
            w.Title.EndsWith("is sharing a window.") ||
            w.Title.EndsWith("is sharing your screen.") ||
            w.Title.EndsWith("está compartilhando uma janela.") ||
            w.Title.EndsWith("está compartilhando sua tela.")
        )
    ));
    // Keypirinha
    context.WindowRouter.IgnoreProcessName("keypirinha-x64");
    // PDFCreator
    context.WindowRouter.IgnoreProcessName("PDFCreator");
    // PortableApps
    context.WindowRouter.IgnoreProcessName("PortableAppsPlatform");
    context.WindowRouter.IgnoreProcessName("PortableAppsUpdater");
    // Python (matplotlib)
    context.WindowRouter.IgnoreWindowClass("TkTopLevel");
    // R
    context.WindowRouter.IgnoreWindowClass("Rterm");
    // SpeedCrunch
    context.WindowRouter.IgnoreProcessName("speedcrunch");
    // Sublime Text
    context.WindowRouter.AddFilter((w) => !(
        w.ProcessName.Equals("sublime_text") && (
          w.Title.Equals("Changelog") || w.Title.Equals("Update")
        )
    ));
    // VLC
    context.WindowRouter.IgnoreProcessName("vlc");
    // XMind
    context.WindowRouter.AddFilter((w) => !(
        w.ProcessName.Equals("XMind") && w.Class.Equals("Static")
    ));

    context.WindowRouter.RouteProcessName("notes2", ws_mail);
    context.WindowRouter.RouteProcessName("olk",    ws_mail);
    context.WindowRouter.RouteProcessName("Teams",  ws_chat);
    context.WindowRouter.RouteTitle("WhatsApp Web", ws_chat);

    context.WindowRouter.RouteTitle("Gmail",                     ws_apps);
    context.WindowRouter.RouteTitle("Google Calendar",           ws_apps);
    context.WindowRouter.RouteTitleMatch(".*gmail.com - Gmail",  ws_apps);
    context.WindowRouter.RouteTitleMatch("Google Calendar - .*", ws_apps);

    context.WindowRouter.RouteProcessName("Amazon Music", "10th");
    context.WindowRouter.RouteProcessName("Spotify",      "10th");

};
return doConfig;

// vi: ft=cs
