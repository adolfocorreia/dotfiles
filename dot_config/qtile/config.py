# References:
# - Default config:   https://docs.qtile.org/en/stable/manual/config/default.html
# - Config variables: https://docs.qtile.org/en/stable/manual/config/index.html#configuration-variables
# - Commands API:     https://docs.qtile.org/en/stable/manual/commands/api
# - Lazy commands:    https://docs.qtile.org/en/stable/manual/config/lazy.html

# pyright: reportAny=false
# pyright: reportUnknownLambdaType=false
# pyright: reportUnknownMemberType=false
# pyright: reportUnknownParameterType=false
# pyright: reportUnknownVariableType=false

import re
import typing

from libqtile import bar, hook, layout, qtile, widget
from libqtile.backend.base.window import Window
from libqtile.command.base import expose_command
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.core.manager import Qtile
from libqtile.lazy import lazy

import traverse  # pyright: ignore[reportImplicitRelativeImport]

### General customization and settings ###

M = "mod1"  # Alt
S = "shift"
gap = 10

reverse_screens_order = True
L = 0 if not reverse_screens_order else 1  # Left screen
R = 1 - L  # Right screen

# Behaviour settings
cursor_warp = True  # Cursor follows focus as directed by keyboard


### Custom classes and functions ###


class MonadTall(layout.MonadTall):
    def __init__(self, **config):  # pyright: ignore[reportMissingParameterType]
        super().__init__(**config)

    @expose_command()  # pyright:ignore[reportUntypedFunctionDecorator]
    def promote_main(self) -> None:
        """Swap secondary window to main pane or swap main and first secondary window."""
        if len(self.clients) < 2:
            return
        main_focused = self.clients.current_index == 0
        win0 = typing.cast(Window, self.clients[0])
        win1 = typing.cast(Window, self.clients[1])
        if main_focused:
            self.swap(win1, win0)
        else:
            self.swap_main()


class MonadWide(layout.MonadWide):
    def __init__(self, **config):  # pyright: ignore[reportMissingParameterType]
        super().__init__(**config)

    @expose_command()  # pyright:ignore[reportUntypedFunctionDecorator]
    def promote_main(self) -> None:
        """Swap secondary window to main pane or swap main and first secondary window."""
        if len(self.clients) < 2:
            return
        main_focused = self.clients.current_index == 0
        win0 = typing.cast(Window, self.clients[0])
        win1 = typing.cast(Window, self.clients[1])
        if main_focused:
            self.swap(win1, win0)
        else:
            self.swap_main()


def move_window_to_screen(qtile: Qtile, screen_id: int):
    """Move focused window to specified screen."""
    assert 0 <= screen_id < len(qtile.screens)

    # No focused window or already on target screen
    window: Window | None = qtile.current_window
    if window is None:
        return
    assert window.group is not None
    screen = typing.cast(Screen, window.group.screen)
    if screen.index == screen_id:
        return

    group = qtile.screens[screen_id].group
    assert isinstance(group.name, str)
    window.togroup(group_name=group.name)
    window.focus()


# TODO: understand why this fixes having focus on two windows
def fix_focus(qtile: Qtile):
    current_screen: int = qtile.current_screen.index
    other_screen: int = 1 - current_screen
    qtile.focus_screen(other_screen, warp=False)
    qtile.focus_screen(current_screen, warp=False)


def switch_to_group(qtile: Qtile, group_name: str):
    qtile.groups_map[group_name].toscreen()
    fix_focus(qtile)


def toggle_last_group(qtile: Qtile):
    qtile.current_screen.toggle_group()
    fix_focus(qtile)


### Key bindings ###

# Check available key names at:
# - https://github.com/qtile/qtile/blob/master/libqtile/backend/x11/xkeysyms.py
# fmt: off
keys = [
    # TODO: find out why left and right movements sometimes do not select the same windows
    # Switch between windows (even across monitors), according to position and geometry.
    Key([M], "h", lazy.function(traverse.left),  desc="Move focus left"),
    Key([M], "j", lazy.function(traverse.down),  desc="Move focus down"),
    Key([M], "k", lazy.function(traverse.up),    desc="Move focus up"),
    Key([M], "l", lazy.function(traverse.right), desc="Move focus right"),

    # Switch between windows, according to current layout.
    # Key([M], "h", lazy.layout.left(),  desc="Move focus left"),
    # Key([M], "j", lazy.layout.down(),  desc="Move focus down"),
    # Key([M], "k", lazy.layout.up(),    desc="Move focus up"),
    # Key([M], "l", lazy.layout.right(), desc="Move focus right"),

    # Switch between windows (in current group), according to logical order.
    Key([M],   "tab", lazy.group.next_window(), desc="Next window"),
    Key([M,S], "tab", lazy.group.prev_window(), desc="Previous window"),

    # Move/swap windows in current layout.
    Key([M,S],    "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([M,S],    "k", lazy.layout.shuffle_up(),   desc="Move window up"),
    Key([M,S],    "h", lazy.layout.swap_left(),    desc="Swap window left"),
    Key([M,S],    "l", lazy.layout.swap_right(),   desc="Swap window right"),
    Key([M], "return", lazy.layout.promote_main(), desc="Promote to main window"),

    # Switch between screens (monitors).
    Key([M],   "w", lazy.to_screen(L), desc="Focus on left screen"),
    Key([M],   "e", lazy.to_screen(R), desc="Focus on right screen"),
    Key([M,S], "w", lazy.function(move_window_to_screen, L), desc="Move to left screen"),
    Key([M,S], "e", lazy.function(move_window_to_screen, R), desc="Move to right screen"),

    # Grow/shrink windows (according to layout).
    # Master windows grow/shrink horizontally, stack windows grow/shrink vertically.
    Key([M],  "comma", lazy.layout.shrink(),    desc="Shrink window"),
    Key([M], "period", lazy.layout.grow(),      desc="Grow window"),
    Key([M,S],    "r", lazy.layout.reset(),     desc="Reset all window sizes"),
    Key([M,S],    "n", lazy.layout.normalize(), desc="Normalize secondary window sizes"),
    Key([M,S],    "m", lazy.layout.maximize(),  desc="Maximize/minimize window size"),

    # Toggle/flip layouts.
    Key([M],   "space", lazy.next_layout(), desc="Toggle between layouts"),
    Key([M,S], "space", lazy.layout.flip(), desc="Flip layout"),

    # Toggle and kill commands.
    Key([M], "backspace", lazy.function(toggle_last_group), desc="Toggle between last two groups"),
    Key([M,S],       "f", lazy.window.toggle_fullscreen(),  desc="Toggle fullscreen on the focused window"),
    Key([M,S],       "t", lazy.window.toggle_floating(),    desc="Toggle floating on the focused window"),
    Key([M,S],       "b", lazy.hide_show_bar("all", "all"), desc="Toggle bar"),
    Key([M,S],       "q", lazy.window.kill(),               desc="Kill focused window"),
    Key([M,S],       "c", lazy.window.kill(),               desc="Kill focused window"),

    # Spawn applications and commands.
    Key([M],         "p", lazy.spawncmd(),               desc="Spawn command using prompt widget"),
    Key([M], "backslash", lazy.spawn("rofi -show"),      desc="Spawn launcher"),
    Key([M,S],  "return", lazy.spawn("alacritty"),       desc="Launch terminal"),
    Key([M],    "escape", lazy.spawn("xset s activate"), desc="Lock screen"),
]
# fmt: on


### Groups (workspaces) ###

# fmt: off
group_names = [
    ("term", "\U000f07b7"),
    ("www",  "\U000f059f"),
    ("dev",  "\U000f05c0"),
    ("doc",  "\U000f06d3"),
    ("file", "\U000f0770"),
    ("mail", "\U000f02ab"),
    ("chat", "\U000f0b79"),
    ("note", "\U000f01c8"),
    ("vid",  "\U000f05a0"),
    ("time", "\U000f051b"),
]
# fmt: on
groups = [Group(name=name, label=name.upper()) for (name, _) in group_names]

group_keys = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]
for g, key in zip(groups, group_keys):
    keys.extend(
        [
            # mod + group number = switch to group
            Key(
                [M],
                str(key),
                lazy.function(switch_to_group, g.name),
                desc=f"Switch to group {g.name}",
            ),
            # mod + shift + group number = move focused window to group
            Key(
                [M, S],
                str(key),
                lazy.window.togroup(g.name),
                desc=f"Move focused window to group {g.name}",
            ),
        ]
    )

groups_by_name = {g.name: g for g in groups}

groups_by_name["chat"].matches.append(Match(wm_class=re.compile("whatsapp-nativefier")))
groups_by_name["dev"].matches.append(Match(wm_class="code"))
groups_by_name["dev"].matches.append(Match(wm_class="Emacs"))
groups_by_name["dev"].matches.append(Match(wm_class="neovide"))
groups_by_name["note"].matches.append(Match(wm_class="obsidian"))
groups_by_name["time"].matches.append(Match(wm_class="superProductivity"))

groups_by_name["term"].spawn = "alacritty"
groups_by_name["time"].spawn = "superproductivity"


### Layouts ###

layouts = [
    MonadTall(margin=gap, ratio=0.6, new_client_position="top"),
    MonadWide(margin=gap, ratio=0.6, new_client_position="top"),
    layout.Max(),
]

# Drag floating layouts
# fmt: off
mouse = [
    Drag( [M], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag( [M], "Button3", lazy.window.set_size_floating(),     start=lazy.window.get_size()),
    Click([M], "Button2", lazy.window.bring_to_front()),
]
# fmt: on

# Run the utility of `xprop` to see the wm class and name of an X client.
free_floats = [
    Match(wm_class="copyq"),
    Match(wm_class="Gscreenshot"),
    Match(wm_class="SpeedCrunch"),
    Match(wm_class="Variety"),
    Match(wm_class="Xarchiver"),
]
center_floats = [
    Match(wm_class="Btrfs Assistant"),
    Match(wm_class="feh"),
    Match(wm_class="Gpicview"),
    Match(wm_class="Gsmartcontrol"),
    Match(wm_class="Hdajackretask"),
    Match(wm_class="Pavucontrol"),
    Match(wm_class="superProductivity"),
    Match(wm_class="Systemadm"),
    Match(wm_class="Vorta"),
    Match(wm_class="Xdg-desktop-portal-gtk"),
]
# TODO: add hook to resize these windows on creation
resize_floats = [
    Match(wm_class="gksqt"),
    Match(wm_class="Matplotlib"),
    Match(wm_class="mpv"),
    Match(wm_class="vlc"),
]
floating_layout = layout.Floating(
    float_rules=[
        *layout.Floating.default_float_rules,
        *free_floats,
        *center_floats,
        *resize_floats,
    ],
    no_reposition_rules=[*free_floats],
)


### Bars and widgets ###

widget_defaults = dict(
    font="sans",
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

bar_size = 24
left_screen = Screen(
    top=bar.Bar(
        [
            widget.CurrentLayoutIcon(),
            widget.Spacer(length=5),
            widget.WindowCount(),
            widget.Spacer(length=10),
            widget.GroupBox(),
            widget.Spacer(length=10),
            widget.Prompt(),
            widget.Spacer(length=bar.STRETCH),
            widget.WindowName(),
            widget.Spacer(length=bar.STRETCH),
            widget.Spacer(length=15),
            widget.Clock(format="%Y-%m-%d %a %H:%M"),
        ],
        bar_size,
    ),
)
right_screen = Screen(
    top=bar.Bar(
        [
            widget.CurrentLayoutIcon(),
            widget.Spacer(length=5),
            widget.WindowCount(),
            widget.Spacer(length=10),
            widget.GroupBox(),
            widget.Spacer(length=bar.STRETCH),
            widget.WindowName(),
            widget.Spacer(length=bar.STRETCH),
            widget.Systray(),
            widget.Spacer(length=15),
            widget.QuickExit(),
        ],
        bar_size,
    )
)
if not reverse_screens_order:
    screens = [left_screen, right_screen]
else:
    screens = [right_screen, left_screen]


### Hooks ###


@hook.subscribe.startup_complete
def startup_complete():
    qtile.groups_map["term"].toscreen(L)
    qtile.groups_map["time"].toscreen(R)
