import os
import platform

# Load existing settings made via :set
config.load_autoconfig()

# Normal mode key bindings
config.bind("<Ctrl+Shift+Tab>", "tab-prev")
config.bind("<Ctrl+Tab>", "tab-next")

# Command mode key bindings
config.bind("<Ctrl+p>", "completion-item-focus --history prev", mode="command")
config.bind("<Ctrl+n>", "completion-item-focus --history next", mode="command")
config.bind("<Ctrl+l>", "command-accept", mode="command")
config.bind("<Shift+PgUp>", "command-history-prev", mode="command")
config.bind("<Shift+PgDown>", "command-history-next", mode="command")

# General configuration
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.preferred_color_scheme = "dark"
c.content.autoplay = False
c.content.cookies.accept = "no-3rdparty"
c.content.cookies.store = False
c.content.notifications.enabled = False
c.content.prefers_reduced_motion = True
c.downloads.position = "bottom"
c.editor.command = ["gvim", "-f", "'{file}'", "-c", "normal {line}G{column0}l"]
c.editor.encoding = "utf-8"
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
c.url.searchengines[",b"] = "https://search.brave.com/search?q={}"
c.url.searchengines[",d"] = "https://duckduckgo.com/?q={}"
c.url.searchengines[",g"] = "https://google.com/search?q={}"
c.url.searchengines[",gs"] = "https://scholar.google.com/scholar?q={}"

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
