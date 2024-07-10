-- Reference: https://www.hammerspoon.org/docs

-- Tip: Use hs.inspect(table) to print Lua tables on the console

hs = hs

-- Same as in Amethyst
mod1 = { "option", "shift" }
mod2 = { "option", "shift", "control" }

hs.hotkey.bind(mod1, "return", function()
	hs.execute("open -n /Applications/iTerm.app")
end)

hs.hotkey.bind(mod2, "return", function()
	hs.execute("open /Users/adcor")
end)
