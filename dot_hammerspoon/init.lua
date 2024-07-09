-- Reference: https://www.hammerspoon.org/docs

-- Tip: Use hs.inspect(table) to print Lua tables on the console

hs = hs

hs.hotkey.bind({ "option", "shift" }, "return", function()
	hs.execute("open -n /Applications/iTerm.app")
end)

hs.hotkey.bind({ "option", "shift", "ctrl" }, "return", function()
	hs.execute("open /Users/adcor")
end)
