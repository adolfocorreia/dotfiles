"""
This plugin exports four functions - up, down, left and right - that when called will
move window focus to the first window in that general direction. Focussing is based
entirely on position and geometry, so is independent of screens, layouts and whether
windows are floating or tiled. It can also move focus to and from empty screens.

Example usage:

    import traverse

    keys.extend([
        Key([mod], "k", lazy.function(traverse.up)),
        Key([mod], "j", lazy.function(traverse.down)),
        Key([mod], "h", lazy.function(traverse.left)),
        Key([mod], "l", lazy.function(traverse.right)),
    ])
"""

import math
import typing
from collections.abc import Iterator
from dataclasses import dataclass

from libqtile.backend.base.window import Window
from libqtile.config import Screen
from libqtile.core.manager import Qtile


def up(qtile: Qtile):
    """Focus next window or empty screen up"""
    _ = _focus_window(qtile, Transform(exchange=True, flip=True))


def down(qtile: Qtile):
    """Focus next window or empty screen down"""
    _ = _focus_window(qtile, Transform(exchange=True, flip=False))


def left(qtile: Qtile):
    """Focus next window or empty screen to the left"""
    _ = _focus_window(qtile, Transform(exchange=False, flip=True))


def right(qtile: Qtile):
    """Focus next window or empty screen to the right"""
    _ = _focus_window(qtile, Transform(exchange=False, flip=False))


def focusable(qtile: Qtile) -> Iterator[Window | Screen]:
    """Generates potential navigation targets (Screens or Windows).

    Yields Screen objects for empty, non-focused screens. Yields Window
    objects for non-focused windows.

    Crucially, if a screen contains a fullscreen window, only that specific
    fullscreen window (if not already focused) will be yielded for that screen,
    preventing focus shifts to background windows on the same screen. If no
    window is fullscreen on a screen, all its non-focused windows are eligible.
    """

    for screen in qtile.screens:
        windows: list[Window] = (
            screen.group.windows  # pyright: ignore [reportUnknownMemberType]
        )
        if not windows and screen is not qtile.current_screen:
            yield screen
        has_fullscreen_window = any(w.fullscreen for w in windows)
        for window in windows:
            if window is qtile.current_window:
                continue
            if not has_fullscreen_window or window.fullscreen:
                yield window


@dataclass
class Point:
    """Point in virtual coordinates"""

    main: int
    cross: int

    def distance(self, other: "Point") -> float:
        """Calculate the distance to another point"""
        return math.hypot(self.main - other.main, self.cross - other.cross)


@dataclass
class Rectangle:
    """Store the dimensions of a rectangle in virtual coordinates"""

    obj: Window | Screen
    main: int
    cross: int
    length: int
    breadth: int

    @property
    def center(self) -> Point:
        """Return the center point of the rectangle"""
        return Point(
            self.main + self.length // 2,
            self.cross + self.breadth // 2,
        )

    @property
    def is_visible(self) -> bool:
        """Determine whether the rectangle is currently visible on the screen"""
        return isinstance(self.obj, Screen) or self.obj.is_visible()

    def in_cross_band(self, band_min: int, band_max: int) -> bool:
        """Determine whether the rectangle intersects with the given cross band"""
        return (
            band_min <= self.cross <= band_max
            or band_min <= (self.cross + self.breadth) <= band_max
        )


@dataclass
class Transform:
    """Define how to extract coordinates form Window and Screen

    exchange - exchange x and y
    flip - mirror the main axis direction
    """

    exchange: bool
    flip: bool

    def __call__(self, obj: Window | Screen) -> Rectangle:
        tmp_x = -obj.x - obj.width if self.flip else obj.x
        tmp_y = -obj.y - obj.height if self.flip else obj.y
        return Rectangle(
            obj,
            tmp_y if self.exchange else tmp_x,
            tmp_x if self.exchange else tmp_y,
            obj.height if self.exchange else obj.width,
            obj.width if self.exchange else obj.height,
        )


def _focus_window(qtile: Qtile, transform: Transform) -> Window | Screen | None:
    focused: Rectangle = transform(qtile.current_window or qtile.current_screen)

    # Compute focused object reference for distance calculations.
    # The negative offset prioritizes candidates that appear before along the cross axis.
    offset = -10
    focused_reference = Point(
        focused.main + focused.length, focused.cross + focused.breadth // 2 + offset
    )

    # Filter to only consider visible objects in the correct direction.
    # The filter currently assumes windows are tiled (i.e., without superpositioning);
    # in case of floating windows, the reference points should be their centers.
    # TODO: add support for floating windows (lambda rect: rect.center.main > focused.center.main)
    visible: list[Rectangle] = list(
        filter(
            lambda rect: rect.is_visible,
            map(transform, focusable(qtile)),
        )
    )
    candidates: list[Rectangle] = list(
        filter(
            lambda rect: rect.main >= (focused.main + focused.length),
            visible,
        )
    )

    # Then take closest in direction of the main axis, preferring those on the
    # same level as the current window.
    # The candidates are sorted by (each element of the tuples):
    # 1. Whether they are in the same cross band as the focused window;
    # 2. Distance to the focused object reference point;
    # 3. Cross coordinate (tiebreaker criterion).
    band_min = focused.cross
    band_max = focused.cross + focused.breadth
    closest: list[Rectangle] = sorted(
        candidates,
        key=lambda rect: (
            0 if rect.in_cross_band(band_min, band_max) else 1,  # in-band are 'closer'
            focused_reference.distance(
                Point(rect.main, rect.cross + rect.breadth // 2)
            ),
            rect.cross,
        ),
    )

    if not closest:
        # Could not find anything to focus. Maybe this is the edge of the known space.
        return None

    target: Window | Screen = closest[0].obj
    assert target.group is not None
    screen = typing.cast(Screen, target.group.screen)

    if isinstance(target, Screen):
        qtile.focus_screen(screen.index)

    if isinstance(target, Window):
        target.group.focus(target, True)  # pyright: ignore [reportUnknownMemberType]
        target.focus(False)
        target.bring_to_front()

    qtile.core.warp_pointer(target.x + target.width // 2, target.y + target.height // 2)

    return target
