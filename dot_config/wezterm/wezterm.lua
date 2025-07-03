-- References:
-- - Default keys: https://wezterm.org/config/default-keys
-- - Available actions: https://wezterm.org/config/lua/keyassignment

-- TODO:
-- - evaluate workspaces and launchers (https://wezterm.org/recipes/workspaces)
-- - evaluate ActivateCopyMode
-- - evaluate mouse bindings (https://wezterm.org/config/mouse)
-- - add resize pane bindings
-- - add resize font bindings
-- - evaluate https://github.com/theopn/dotfiles/blob/main/wezterm/.config/wezterm/wezterm.lua

local wezterm = require("wezterm")
local act = wezterm.action
local config = wezterm.config_builder()

if string.find(wezterm.target_triple, "windows") then
	config.default_prog = { "pwsh", "-NoLogo" }
else
	config.default_prog = { "tmux" }
end

config.default_workspace = "main"

config.color_scheme = "tokyonight_night"

config.font_size = 10
config.harfbuzz_features = { "calt=0", "clig=0", "liga=0" } -- disable ligatures

config.disable_default_key_bindings = true
config.leader = { mods = "CTRL", key = "q" }
config.keys = {
	{ key = "Insert", mods = "CTRL", action = act.CopyTo("Clipboard") },
	{ key = "Insert", mods = "SHIFT", action = act.PasteFrom("Clipboard") },
	{ key = "p", mods = "CTRL|SHIFT", action = act.ActivateCommandPalette },
	{ key = "l", mods = "CTRL|SHIFT", action = act.ShowDebugOverlay },
	-- tmux-like key bindings
	{ key = "q", mods = "LEADER|CTRL", action = act.SendKey({ mods = "CTRL", key = "q" }) },
	{ key = "o", mods = "LEADER|CTRL", action = act.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" }) },
	{ key = "c", mods = "LEADER", action = act.SpawnTab("CurrentPaneDomain") },
	{ key = "x", mods = "LEADER", action = act.CloseCurrentTab({ confirm = true }) },
	{ key = "n", mods = "LEADER", action = act.ActivateTabRelative(1) },
	{ key = "p", mods = "LEADER", action = act.ActivateTabRelative(-1) },
	{ key = "w", mods = "LEADER", action = act.ShowTabNavigator },
	{ key = "s", mods = "LEADER", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
	{ key = "v", mods = "LEADER", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
	{ key = "h", mods = "LEADER", action = act.ActivatePaneDirection("Left") },
	{ key = "j", mods = "LEADER", action = act.ActivatePaneDirection("Down") },
	{ key = "k", mods = "LEADER", action = act.ActivatePaneDirection("Up") },
	{ key = "l", mods = "LEADER", action = act.ActivatePaneDirection("Right") },
	{ key = "[", mods = "LEADER", action = act.ActivateCopyMode },
	-- Switch to tab bindings
	{ key = "1", mods = "LEADER", action = act.ActivateTab(0) },
	{ key = "2", mods = "LEADER", action = act.ActivateTab(1) },
	{ key = "3", mods = "LEADER", action = act.ActivateTab(2) },
	{ key = "4", mods = "LEADER", action = act.ActivateTab(3) },
	{ key = "5", mods = "LEADER", action = act.ActivateTab(4) },
	{ key = "6", mods = "LEADER", action = act.ActivateTab(5) },
	{ key = "7", mods = "LEADER", action = act.ActivateTab(6) },
	{ key = "8", mods = "LEADER", action = act.ActivateTab(7) },
	{ key = "9", mods = "LEADER", action = act.ActivateTab(8) },
	{ key = "0", mods = "LEADER", action = act.ActivateTab(9) },
}

return config
