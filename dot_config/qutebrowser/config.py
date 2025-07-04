# References:
# https://qutebrowser.org/doc/help/configuring.html
# https://qutebrowser.org/doc/help/settings.html
# https://qutebrowser.org/doc/help/commands.html
# qute://help

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

# Unbind quit and tab-close
config.unbind("<Ctrl+q>", mode="normal")
config.unbind("<Ctrl+w>", mode="normal")

# Bind Shift-Insert to clipboard selection.
config.bind("<Shift-Ins>", "insert-text -- {clipboard}",  mode="insert")
config.bind("<Shift-Ins>", "cmd-set-text -a {clipboard}", mode="command")

# Normal mode key bindings
config.bind("<Ctrl+Shift+Tab>", "tab-prev")
config.bind("<Ctrl+Tab>",       "tab-next")
config.bind("<Ctrl+p>",         "scroll-page 0 -0.75")
config.bind("<Ctrl+n>",         "scroll-page 0 0.75")
config.bind("<Ctrl+y>",         "scroll up")
config.bind("<Ctrl+e>",         "scroll down")
config.bind("gp",               "tab-clone --private")
config.bind("gs",               "navigate strip")

# Command mode key bindings
config.bind("<Ctrl+p>",       "completion-item-focus --history prev", mode="command")
config.bind("<Ctrl+n>",       "completion-item-focus --history next", mode="command")
config.bind("<Ctrl+m>",       "command-accept",                       mode="command")
config.bind("<Shift+PgUp>",   "command-history-prev",                 mode="command")
config.bind("<Shift+PgDown>", "command-history-next",                 mode="command")

# Insert mode key bindings (emacs/readline-like)
# Reference: https://en.wikipedia.org/wiki/GNU_Readline#Editing_modes
config.bind("<Ctrl+b>", "fake-key <Left>",                     mode="insert")
config.bind("<Ctrl+f>", "fake-key <Right>",                    mode="insert")
config.bind("<Alt+b>",  "fake-key <Ctrl+Left>",                mode="insert")
config.bind("<Alt+f>",  "fake-key <Ctrl+Right>",               mode="insert")
config.bind("<Ctrl+a>", "fake-key <Home>",                     mode="insert")
config.bind("<Ctrl+e>", "fake-key <End>",                      mode="insert")
config.bind("<Ctrl+h>", "fake-key <Backspace>",                mode="insert")
config.bind("<Ctrl+d>", "fake-key <Delete>",                   mode="insert")
config.bind("<Alt+h>",  "fake-key <Shift+Ctrl+Left><Delete>",  mode="insert")
config.bind("<Ctrl+w>", "fake-key <Shift+Ctrl+Left><Delete>",  mode="insert")
config.bind("<Alt+d>",  "fake-key <Shift+Ctrl+Right><Delete>", mode="insert")
config.bind("<Ctrl+u>", "fake-key <Shift+Home><Delete>",       mode="insert")
config.bind("<Ctrl+k>", "fake-key <Shift+End><Delete>",        mode="insert")
config.bind("<Ctrl+p>", "fake-key <Up>",                       mode="insert")
config.bind("<Ctrl+n>", "fake-key <Down>",                     mode="insert")
config.bind("<Alt+v>",  "fake-key <PgUp>",                     mode="insert")
config.bind("<Ctrl+v>", "fake-key <PgDown>",                   mode="insert")
config.bind("<Ctrl+m>", "fake-key <Enter>",                    mode="insert")
config.bind("<Ctrl+y>", "insert-text {primary}",               mode="insert")
config.bind("<Ctrl+/>", "fake-key <Ctrl+z>",                   mode="insert")

# Mode leave key bindings
config.bind("<Ctrl+g>", "mode-leave", mode="caret")
config.bind("<Ctrl+g>", "mode-leave", mode="command")
config.bind("<Ctrl+g>", "mode-leave", mode="hint")
config.bind("<Ctrl+g>", "mode-leave", mode="insert")


# General configuration
c.auto_save.session = True
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.preferred_color_scheme = "dark"
c.confirm_quit = ["always"]
c.content.autoplay = False
c.content.cookies.accept = "no-3rdparty"
c.content.cookies.store = False
c.content.dns_prefetch = False
c.content.geolocation = False
c.content.notifications.enabled = False
c.content.pdfjs = True
c.content.prefers_reduced_motion = True
c.downloads.position = "bottom"
c.hints.chars = "asdfhjkl"
c.hints.uppercase = True
c.qt.args = ["autoplay-policy=user-gesture-required"]
#c.qt.chromium.process_model = "process-per-site"
c.scrolling.bar = "always"
c.session.lazy_restore = True
c.statusbar.position = "bottom"
c.tabs.background = True
c.tabs.mousewheel_switching = False
c.tabs.position = "top"

# Ad-blocking
c.content.blocking.method = "both"
c.content.blocking.adblock.lists = [
    'https://easylist.to/easylist/easylist.txt',
    'https://easylist.to/easylist/easyprivacy.txt',
    'https://easylist.to/easylist/fanboy-social.txt',
    'https://fanboy.co.nz/fanboy-annoyance.txt',
    'https://fanboy.co.nz/fanboy-antifacebook.txt',
    'https://easylist-downloads.adblockplus.org/abp-filters-anti-cv.txt',
    'http://www.i-dont-care-about-cookies.eu/abp/',
    'http://ewpratten.github.io/youtube_ad_blocklist/adblockplus.txt',
]

# Search engines
c.url.searchengines["DEFAULT"] = "https://search.brave.com/search?q={}"
c.url.searchengines[",b"]      = "https://search.brave.com/search?q={}"
c.url.searchengines[",d"]      = "https://duckduckgo.com/?q={}"
c.url.searchengines[",g"]      = "https://google.com/search?q={}"
c.url.searchengines[",gs"]     = "https://scholar.google.com/scholar?q={}"
c.url.searchengines[",r"]      = "https://reddit.com/search?q={}"
c.url.searchengines[",s"]      = "https://startpage.com/sp/search?query={}"
c.url.searchengines[",w"]      = "https://en.wikipedia.org/wiki/{}"
c.url.searchengines[",y"]      = "https://youtube.com/results?search_query={}"
c.url.searchengines[",aw"]     = "https://wiki.archlinux.org/index.php?search={}"
c.url.searchengines[",ap"]     = "https://archlinux.org/packages/?q={}"
c.url.searchengines[",aa"]     = "https://aur.archlinux.org/packages/?K={}"


# Fonts
c.fonts.default_family = "Iosevka Aile"
if platform.system() == "Darwin":
    c.fonts.default_size = "14pt"
    c.fonts.hints = "bold 12pt default_family"
else:
    c.fonts.default_size = "10pt"
    c.fonts.hints = "bold 9pt default_family"



# Platform specific configuration

# Web proxy
http_proxy = os.getenv("http_proxy")
if http_proxy is not None:
    c.content.proxy = http_proxy

# Color theme
# Set colors.webpage.bg to dark to avoid white flashes when loading tabs
# Reference: https://github.com/qutebrowser/qutebrowser/issues/2912
theme_file = "base16-tokyo-night-dark.config.py"
if platform.system() == "Windows":
    config_home = os.getenv("XDG_CONFIG_HOME")
    assert config_home is not None
    theme_file = os.path.join(config_home, "qutebrowser", theme_file)
config.source(theme_file)

# External video player keybinding
if platform.system() == "Windows":
    cmd = "vlc"
else:
    cmd = "mpv --profile=qutebrowser"
config.bind(",V", "hint links spawn %s {hint-url}" % cmd)
config.bind(",v", "spawn %s {url}" % cmd)
