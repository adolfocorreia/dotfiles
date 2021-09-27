/*
Examples:
- https://github.com/dalyIsaac/workspacer-config/blob/main/workspacer.config.csx
- https://github.com/system32uwu/dotfiles/blob/main/windows10/.workspacer/workspacer.config.csx
- https://gist.github.com/haoxiangliew/fcb925d1e6987d0e1824848cf9659730
*/

#r "C:\apps\workspacer\workspacer.Shared.dll"
#r "C:\apps\workspacer\plugins\workspacer.ActionMenu\workspacer.ActionMenu.dll"
#r "C:\apps\workspacer\plugins\workspacer.Bar\workspacer.Bar.dll"
#r "C:\apps\workspacer\plugins\workspacer.FocusIndicator\workspacer.FocusIndicator.dll"
#r "C:\apps\workspacer\plugins\workspacer.Gap\workspacer.Gap.dll"
#r "C:\apps\workspacer\plugins\workspacer.TitleBar\workspacer.TitleBar.dll"
#r "C:\apps\workspacer\Microsoft.VisualBasic.dll"
#r "C:\apps\workspacer\System.Diagnostics.PerformanceCounter.dll"


using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Timers;
using Microsoft.VisualBasic.Devices;
using workspacer;
using workspacer.ActionMenu;
using workspacer.Bar;
using workspacer.Bar.Widgets;
using workspacer.FocusIndicator;
using workspacer.Gap;
using workspacer.TitleBar;


class CpuMemWidget : BarWidgetBase {
    private double _interval;
    private string _template;
    private ulong _totalMemory;
    private Timer _timer;
    private PerformanceCounter _cpuCounter;
    private PerformanceCounter _memCounter;
    private string _text;
    public CpuMemWidget(double interval, string template) {
        _interval = interval;
        _template = template;
    }
    public override void Initialize() {
        _totalMemory = new ComputerInfo().TotalPhysicalMemory / (1024*1024);
        _cpuCounter = new PerformanceCounter("Processor", "% Processor Time", "_Total", true);
        _memCounter = new PerformanceCounter("Memory", "Available MBytes", true);
        _text = _GenerateTextFromCounters();
        _timer = new Timer(_interval);
        _timer.Elapsed += (s, e) => {
            _text = _GenerateTextFromCounters();
            Context.MarkDirty();
        };
        _timer.Enabled = true;
    }
    public override IBarWidgetPart[] GetParts() {
        return Parts(Part(_text, fontname: FontName));
    }
    private string _GenerateTextFromCounters() {
        string cpu = Convert.ToInt32(_cpuCounter.NextValue()).ToString();
        string mem = Convert.ToInt32((1 - _memCounter.NextValue()/_totalMemory) * 100).ToString();
        return _template.Replace("[cpu]", cpu).Replace("[mem]", mem);
    }
}


Action<IConfigContext> doConfig = (context) => {

    Color backgroundColor = new Color(0x20, 0x21, 0x24);
    int barHeight = 20;
    string fontName = "Hack NF";
    int fontSize = 10;

    KeyModifiers mod   = KeyModifiers.Win;
    KeyModifiers modA  = mod | KeyModifiers.Alt;
    KeyModifiers modC  = mod | KeyModifiers.Control;
    KeyModifiers modS  = mod | KeyModifiers.Shift;
    KeyModifiers modCS = mod | KeyModifiers.Control | KeyModifiers.Shift;

    context.CanMinimizeWindows = true;

    /* Keybindings */
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
        k.Subscribe(modS, Keys.OemQuestion, () => k.ShowKeybindDialog(),                      "open keybind window");
        k.Subscribe(modS, Keys.Enter,       () => System.Diagnostics.Process.Start("wt.exe"), "launch terminal");
        k.Subscribe(modS, Keys.C,           () => w.FocusedWorkspace.CloseFocusedWindow(),    "close focused window");
        k.Subscribe(modA, Keys.Q,           () => context.Restart(),                          "restart workspacer");
        k.Subscribe(modS, Keys.Q,           () => context.Quit(),                             "quit workspacer");
        k.Subscribe(modA, Keys.Escape,      () => context.Enabled = !context.Enabled,         "enable/disable workspacer");

        /* Window keybindings */
        k.Subscribe(modS, Keys.Space,     () => w.FocusedWorkspace.NextLayoutEngine(),                "next layout");
        k.Subscribe(modA, Keys.N,         () => w.FocusedWorkspace.ResetLayout(),                     "reset layout");
        k.Subscribe(modA, Keys.J,         () => w.FocusedWorkspace.FocusNextWindow(),                 "focus next window");
        k.Subscribe(modA, Keys.K,         () => w.FocusedWorkspace.FocusPreviousWindow(),             "focus previous window");
        k.Subscribe(modA, Keys.Down,      () => w.FocusedWorkspace.FocusNextWindow(),                 "focus next window");
        k.Subscribe(modA, Keys.Up,        () => w.FocusedWorkspace.FocusPreviousWindow(),             "focus previous window");
        k.Subscribe(modA, Keys.M,         () => w.FocusedWorkspace.FocusPrimaryWindow(),              "focus primary window");
        k.Subscribe(modS, Keys.J,         () => w.FocusedWorkspace.SwapFocusAndNextWindow(),          "swap focus and next window");
        k.Subscribe(modS, Keys.K,         () => w.FocusedWorkspace.SwapFocusAndPreviousWindow(),      "swap focus and previous window");
        k.Subscribe(modS, Keys.Down,      () => w.FocusedWorkspace.SwapFocusAndNextWindow(),          "swap focus and next window");
        k.Subscribe(modS, Keys.Up,        () => w.FocusedWorkspace.SwapFocusAndPreviousWindow(),      "swap focus and previous window");
        k.Subscribe(modA, Keys.H,         () => w.FocusedWorkspace.ShrinkPrimaryArea(),               "shrink primary area");
        k.Subscribe(modA, Keys.L,         () => w.FocusedWorkspace.ExpandPrimaryArea(),               "expand primary area");
        k.Subscribe(modA, Keys.T,         () => context.Windows.ToggleFocusedWindowTiling(),          "toggle tiling for focused window");
        k.Subscribe(modA, Keys.Oemcomma,  () => w.FocusedWorkspace.IncrementNumberOfPrimaryWindows(), "increment # primary windows");
        k.Subscribe(modA, Keys.OemPeriod, () => w.FocusedWorkspace.DecrementNumberOfPrimaryWindows(), "decrement # primary windows");

        k.Subscribe(modA, Keys.Enter, () => {
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
        k.Subscribe(modA,  Keys.D1,    () => w.SwitchToWorkspace(0),                            "switch to workspace 1");
        k.Subscribe(modA,  Keys.D2,    () => w.SwitchToWorkspace(1),                            "switch to workspace 2");
        k.Subscribe(modA,  Keys.D3,    () => w.SwitchToWorkspace(2),                            "switch to workspace 3");
        k.Subscribe(modA,  Keys.D4,    () => w.SwitchToWorkspace(3),                            "switch to workspace 4");
        k.Subscribe(modA,  Keys.D5,    () => w.SwitchToWorkspace(4),                            "switch to workspace 5");
        k.Subscribe(modA,  Keys.D6,    () => w.SwitchToWorkspace(5),                            "switch to workspace 6");
        k.Subscribe(modA,  Keys.D7,    () => w.SwitchToWorkspace(6),                            "switch to workspace 7");
        k.Subscribe(modA,  Keys.D8,    () => w.SwitchToWorkspace(7),                            "switch to workspace 8");
        k.Subscribe(modA,  Keys.D9,    () => w.SwitchToWorkspace(8),                            "switch to workpsace 9");
        k.Subscribe(modA,  Keys.D0,    () => w.SwitchToWorkspace(9),                            "switch to workpsace 10");
        k.Subscribe(modC,  Keys.J,     () => w.SwitchToNextWorkspace(),                         "switch to next workspace");
        k.Subscribe(modC,  Keys.K,     () => w.SwitchToPreviousWorkspace(),                     "switch to previous workspace");
        k.Subscribe(modC,  Keys.Down,  () => w.SwitchToNextWorkspace(),                         "switch to next workspace");
        k.Subscribe(modC,  Keys.Up,    () => w.SwitchToPreviousWorkspace(),                     "switch to previous workspace");
        k.Subscribe(modA,  Keys.Back,  () => w.SwitchToLastFocusedWorkspace(),                  "switch to last focused workspace");
        k.Subscribe(modA,  Keys.W,     () => w.SwitchFocusedMonitor(0),                         "siwtch to monitor 1");
        k.Subscribe(modA,  Keys.E,     () => w.SwitchFocusedMonitor(2),                         "siwtch to monitor 2");
        k.Subscribe(modA,  Keys.R,     () => w.SwitchFocusedMonitor(1),                         "siwtch to monitor 3");
        k.Subscribe(modC,  Keys.H,     () => w.SwitchFocusToNextMonitor(),                      "switch to previous monitor");
        k.Subscribe(modC,  Keys.L,     () => w.SwitchFocusToPreviousMonitor(),                  "switch to next monitor");
        k.Subscribe(modC,  Keys.Left,  () => w.SwitchFocusToNextMonitor(),                      "switch to previous monitor");
        k.Subscribe(modC,  Keys.Right, () => w.SwitchFocusToPreviousMonitor(),                  "switch to next monitor");
        k.Subscribe(modS,  Keys.D1,    () => w.MoveFocusedWindowToWorkspace(0),                 "move focused window to workspace 1");
        k.Subscribe(modS,  Keys.D2,    () => w.MoveFocusedWindowToWorkspace(1),                 "move focused window to workspace 2");
        k.Subscribe(modS,  Keys.D3,    () => w.MoveFocusedWindowToWorkspace(2),                 "move focused window to workspace 3");
        k.Subscribe(modS,  Keys.D4,    () => w.MoveFocusedWindowToWorkspace(3),                 "move focused window to workspace 4");
        k.Subscribe(modS,  Keys.D5,    () => w.MoveFocusedWindowToWorkspace(4),                 "move focused window to workspace 5");
        k.Subscribe(modS,  Keys.D6,    () => w.MoveFocusedWindowToWorkspace(5),                 "move focused window to workspace 6");
        k.Subscribe(modS,  Keys.D7,    () => w.MoveFocusedWindowToWorkspace(6),                 "move focused window to workspace 7");
        k.Subscribe(modS,  Keys.D8,    () => w.MoveFocusedWindowToWorkspace(7),                 "move focused window to workspace 8");
        k.Subscribe(modS,  Keys.D9,    () => w.MoveFocusedWindowToWorkspace(8),                 "move focused window to workspace 9");
        k.Subscribe(modS,  Keys.D0,    () => w.MoveFocusedWindowToWorkspace(9),                 "move focused window to workspace 10");
        k.Subscribe(modCS, Keys.J,     () => w.MoveFocusedWindowAndSwitchToNextWorkspace(),     "move focused window to next workspace");
        k.Subscribe(modCS, Keys.K,     () => w.MoveFocusedWindowAndSwitchToPreviousWorkspace(), "move focused window to previous workspace");
        k.Subscribe(modCS, Keys.Down,  () => w.MoveFocusedWindowAndSwitchToNextWorkspace(),     "move focused window to next workspace");
        k.Subscribe(modCS, Keys.Up,    () => w.MoveFocusedWindowAndSwitchToPreviousWorkspace(), "move focused window to previous workspace");
        k.Subscribe(modS,  Keys.W,     () => w.MoveFocusedWindowToMonitor(0),                   "move focused window to monitor 1");
        k.Subscribe(modS,  Keys.E,     () => w.MoveFocusedWindowToMonitor(2),                   "move focused window to monitor 2");
        k.Subscribe(modS,  Keys.R,     () => w.MoveFocusedWindowToMonitor(1),                   "move focused window to monitor 3");
        k.Subscribe(modCS, Keys.H,     () => w.MoveFocusedWindowToNextMonitor(),                "move focused window to previous monitor");
        k.Subscribe(modCS, Keys.L,     () => w.MoveFocusedWindowToPreviousMonitor(),            "move focused window to next monitor");
        k.Subscribe(modCS, Keys.Left,  () => w.MoveFocusedWindowToNextMonitor(),                "move focused window to previous monitor");
        k.Subscribe(modCS, Keys.Right, () => w.MoveFocusedWindowToPreviousMonitor(),            "move focused window to next monitor");

        k.Subscribe(modCS, Keys.D1, () => w.MoveAllWindows(w.FocusedWorkspace, c.GetWorkspaceAtIndex(w.FocusedWorkspace, 0)), "move all windows to workpace 1");
        k.Subscribe(modCS, Keys.D2, () => w.MoveAllWindows(w.FocusedWorkspace, c.GetWorkspaceAtIndex(w.FocusedWorkspace, 1)), "move all windows to workpace 2");
        k.Subscribe(modCS, Keys.D3, () => w.MoveAllWindows(w.FocusedWorkspace, c.GetWorkspaceAtIndex(w.FocusedWorkspace, 2)), "move all windows to workpace 3");
        k.Subscribe(modCS, Keys.D4, () => w.MoveAllWindows(w.FocusedWorkspace, c.GetWorkspaceAtIndex(w.FocusedWorkspace, 3)), "move all windows to workpace 4");
        k.Subscribe(modCS, Keys.D5, () => w.MoveAllWindows(w.FocusedWorkspace, c.GetWorkspaceAtIndex(w.FocusedWorkspace, 4)), "move all windows to workpace 5");
        k.Subscribe(modCS, Keys.D6, () => w.MoveAllWindows(w.FocusedWorkspace, c.GetWorkspaceAtIndex(w.FocusedWorkspace, 5)), "move all windows to workpace 6");
        k.Subscribe(modCS, Keys.D7, () => w.MoveAllWindows(w.FocusedWorkspace, c.GetWorkspaceAtIndex(w.FocusedWorkspace, 6)), "move all windows to workpace 7");
        k.Subscribe(modCS, Keys.D8, () => w.MoveAllWindows(w.FocusedWorkspace, c.GetWorkspaceAtIndex(w.FocusedWorkspace, 7)), "move all windows to workpace 8");
        k.Subscribe(modCS, Keys.D9, () => w.MoveAllWindows(w.FocusedWorkspace, c.GetWorkspaceAtIndex(w.FocusedWorkspace, 8)), "move all windows to workpace 9");
        k.Subscribe(modCS, Keys.D0, () => w.MoveAllWindows(w.FocusedWorkspace, c.GetWorkspaceAtIndex(w.FocusedWorkspace, 9)), "move all windows to workpace 10");

        /* Debug keybindings */
        k.Subscribe(modA, Keys.O, () => context.Windows.DumpWindowDebugOutput(),            "dump debug info to console for all windows");
        k.Subscribe(modS, Keys.O, () => context.Windows.DumpWindowUnderCursorDebugOutput(), "dump debug info to console for window under cursor");
        k.Subscribe(modS, Keys.I, () => context.ToggleConsoleWindow(),                      "toggle debug console");

    };
    setKeybindings();


    /* Menu bar */
    context.AddBar(new BarPluginConfig() {
        BarHeight = barHeight,
        DefaultWidgetBackground = backgroundColor,
        FontSize = fontSize,
        FontName = fontName,
        /* Font Awesome icons: https://fontawesome.com */
        LeftWidgets = () => new IBarWidget[] {
            new WorkspaceWidget(),
            new ActiveLayoutWidget() { LeftPadding = "[", RightPadding = "]" },
            new TextWidget(" | "),
            new FocusedMonitorWidget() { FocusedText = "\uf005", },
            new TitleWidget() { IsShortTitle = true },
            new FocusedMonitorWidget() { FocusedText = "\uf005", },
        },
        RightWidgets = () => new IBarWidget[] {
            new CpuMemWidget(1000 * 15, " \uf2db [cpu]%  \uf538 [mem]%"),
            new TextWidget(" \uf242"),
            new BatteryWidget(),
            new TimeWidget(1000, " \uf133 yyyy-MM-dd  \uf017 HH:mm"),
        },
    });


    /* Window focus indicator */
    context.AddFocusIndicator(new FocusIndicatorPluginConfig() {
        /* BorderColor = new Color(0x8E, 0xAE, 0xCB), */
        BorderColor = new Color(0xDF, 0x5F, 0x00),
        BorderSize = 8,
        /* TimeToShow = 500, */
    });


    /* Gap */
    int gap = 8;
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
        KeybindMod      = modA,
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
        ("1st",   defaultLayouts()),
        ("2nd",   defaultLayouts()),
        ("3rd",   defaultLayouts()),
        ("4th",   defaultLayouts()),
        ("5th",   defaultLayouts()),
        ("term",  defaultLayouts()),
        ("notes", new ILayoutEngine[] { new TallLayoutEngine(1, 0.75, 0.025, false) }),
        ("chat",  defaultLayouts()),
        ("gapps", new ILayoutEngine[] { new VertLayoutEngine(), new FullLayoutEngine() }),
        ("misc",  new ILayoutEngine[] { new HorzLayoutEngine(), new FullLayoutEngine() }),
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
    context.WindowRouter.IgnoreProcessName("AutoHotkeyU64");
    // BIG-IP
    context.WindowRouter.IgnoreProcessName("f5fpclientW");
    // Colorpicker
    context.WindowRouter.IgnoreProcessName("Colorpicker");
    // Jitsi (sharing window)
    context.WindowRouter.AddFilter((window) =>
        !(window.Class.Equals("Chrome_WidgetWin_1") & (
            window.Title.EndsWith("is sharing a window.") |
            window.Title.EndsWith("is sharing your screen.") |
            window.Title.EndsWith("está compartilhando uma janela.") |
            window.Title.EndsWith("está compartilhando sua tela.")
        )));
    // Keypirinha
    context.WindowRouter.IgnoreProcessName("keypirinha-x64");
    // PortableApps
    context.WindowRouter.IgnoreProcessName("PortableAppsPlatform");
    context.WindowRouter.IgnoreProcessName("PortableAppsUpdater");
    // SIGA
    context.WindowRouter.IgnoreProcessName("Siga");
    // SpeedCrunch
    context.WindowRouter.IgnoreProcessName("speedcrunch");

    context.WindowRouter.RouteProcessName("notes2",          "notes");
    context.WindowRouter.RouteProcessName("WhatsApp",        "chat");
    context.WindowRouter.RouteProcessName("WindowsTerminal", "term");

    context.WindowRouter.RouteTitle("Gmail",                     "gapps");
    context.WindowRouter.RouteTitle("Google Calendar",           "gapps");
    context.WindowRouter.RouteTitleMatch(".*gmail.com - Gmail",  "gapps");
    context.WindowRouter.RouteTitleMatch("Google Calendar - .*", "gapps");

};
return doConfig;

// vi: ft=cs
