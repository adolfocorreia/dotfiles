# References:
# https://qutebrowser.org/doc/help/configuring.html
# https://qutebrowser.org/doc/help/settings.html
# https://qutebrowser.org/doc/help/commands.html

import os
import platform

# pylint: disable=C0111
config = config  # noqa: F821 pylint: disable=E0602,C0103
c = c            # noqa: F821 pylint: disable=E0602,C0103


# Load existing settings made via :set
config.load_autoconfig()


# Key bindings configuration.
# Default bindings reference:
# - https://qutebrowser.org/doc/help/settings.html#bindings.default
# - qute://help/img/cheatsheet-big.png
# Show current bindings with :bind
# Commands reference: qute://help/commands.html

# Bind Shift-Insert to clipboard selection on Linux.
if platform.system() == "Linux":
    config.bind("<Shift-Ins>", "insert-text -- {clipboard}",  mode="insert")
    config.bind("<Shift-Ins>", "set-cmd-text -a {clipboard}", mode="command")

# Normal mode key bindings
config.bind("<Ctrl+Shift+Tab>", "tab-prev")
config.bind("<Ctrl+Tab>",       "tab-next")

# Command mode key bindings
config.bind("<Ctrl+p>",       "completion-item-focus --history prev", mode="command")
config.bind("<Ctrl+n>",       "completion-item-focus --history next", mode="command")
config.bind("<Ctrl+l>",       "command-accept",                       mode="command")
config.bind("<Shift+PgUp>",   "command-history-prev",                 mode="command")
config.bind("<Shift+PgDown>", "command-history-next",                 mode="command")

# Insert mode key bindings (emacs/readline-like)
config.bind("<Ctrl+b>", "fake-key <Left>",                     mode="insert")
config.bind("<Ctrl+f>", "fake-key <Right>",                    mode="insert")
config.bind("<Alt+b>",  "fake-key <Ctrl+Left>",                mode="insert")
config.bind("<Alt+f>",  "fake-key <Ctrl+Right>",               mode="insert")
config.bind("<Ctrl+a>", "fake-key <Home>",                     mode="insert")
config.bind("<Ctrl+e>", "fake-key <End>",                      mode="insert")
config.bind("<Ctrl+p>", "fake-key <Up>",                       mode="insert")
config.bind("<Ctrl+n>", "fake-key <Down>",                     mode="insert")
config.bind("<Ctrl+u>", "fake-key <Shift+Home><Delete>",       mode="insert")
config.bind("<Ctrl+k>", "fake-key <Shift+End><Delete>",        mode="insert")
config.bind("<Ctrl+d>", "fake-key <Delete>",                   mode="insert")
config.bind("<Alt+d>",  "fake-key <Shift+Ctrl+Right><Delete>", mode="insert")
config.bind("<Ctrl+w>", "fake-key <Shift+Ctrl+Left><Delete>",  mode="insert")


# General configuration
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.preferred_color_scheme = "dark"
c.content.autoplay = False
c.content.cookies.accept = "no-3rdparty"
c.content.cookies.store = False
c.content.notifications.enabled = False
c.content.prefers_reduced_motion = True
c.downloads.position = "bottom"
c.hints.chars = "asdhjkl"
c.qt.args = ["autoplay-policy=user-gesture-required"]
c.qt.process_model = "process-per-site"
c.scrolling.bar = "always"
c.session.lazy_restore = True
c.statusbar.position = "bottom"
c.tabs.background = True
c.tabs.mousewheel_switching = False
c.tabs.position = "top"

# Search engines
c.url.searchengines["DEFAULT"] = "https://search.brave.com/search?q={}"
c.url.searchengines[",b"]      = "https://search.brave.com/search?q={}"
c.url.searchengines[",d"]      = "https://duckduckgo.com/?q={}"
c.url.searchengines[",g"]      = "https://google.com/search?q={}"
c.url.searchengines[",gs"]     = "https://scholar.google.com/scholar?q={}"
c.url.searchengines[",r"]      = "https://reddit.com/search?q={}"
c.url.searchengines[",w"]      = "https://en.wikipedia.org/wiki/{}"
c.url.searchengines[",y"]      = "https://youtube.com/results?search_query={}"

# Platform specific configuration
if platform.system() == "Windows":
    c.fonts.default_family = "SauceCodePro NF"
    c.fonts.default_size = "11pt"

http_proxy = os.getenv("http_proxy")
if http_proxy is not None:
    c.content.proxy = http_proxy

# Load color theme
if platform.system() == "Linux":
    config.source("base16-gruvbox-dark-hard.config.py")
elif platform.system() == "Windows":
    config.source("base16-tomorrow-night.config.py")

# Open external video player keybinding
if platform.system() == "Linux":
    cmd = "flatpak-spawn --host mpv"
elif platform.system() == "Windows":
    cmd = "vlc.bat"
else:
    cmd = "mpv"
config.bind(",M", "hint links spawn %s {hint-url}" % cmd)
config.bind(",m", "spawn %s {url}" % cmd)
