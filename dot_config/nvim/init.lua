-------------------
----- General -----
-------------------
vim = vim

-- Neovim tips:
-- - https://github.com/wincent/vim-university
-- - https://github.com/nanotee/nvim-lua-guide
-- - Use :lua vim.print(<table>) to display table contents.
-- - See kickstart.nvim and lsp-zero as references for overall configuration.

-- TODO: evaluate and replace all vim.cmd statements
-- TODO: convert code to new APIs (e.g. autocmds, key maps, highlight)

-- Define vimrc autocommand group removing all previously set vimrc autocommands
-- when (re)sourcing this file.
-- Reference: https://learnvimscriptthehardway.stevelosh.com/chapters/14.html
vim.api.nvim_create_augroup("vimrc", { clear = true })

-- Select Leader keys.
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Set vim.g.os variable with current OS.
vim.g.os = vim.loop.os_uname().sysname
if string.find(vim.g.os, "Windows") then
  vim.g.os = "Windows"
end

-- Disable external providers support to improve startup time.
vim.g.loaded_python_provider = 0
vim.g.loaded_python3_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0

-- Disable netrw.
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.loaded_netrwSettings = 1
vim.g.loaded_netrwFileHandlers = 1

--------------------------
----- Neovim options -----
--------------------------

-- Use ':set option?' to check current option value.
-- Use ':verbose set option?' to check where it was set.

-- Enable the window title.
vim.opt.title = true

-- Disable showing of partial commands on the last line.
vim.opt.showcmd = false

-- Disable mode indication on last line.
vim.opt.showmode = false

-- Save undo history.
vim.opt.undofile = true

-- Set blinking cursor in normal mode.
if vim.g.os == "Windows" then
  vim.opt.guicursor = "n-v-c-sm:block-blinkwait500-blinkon200-blinkoff150,i-ci-ve:ver25,r-cr-o:hor20"
end

-- Raise dialog when quitting changed buffer.
vim.opt.confirm = true

-- Enable mouse support in all modes.
vim.opt.mouse = "a"

-- Make right mouse button extend selection (equivalent to ':behave xterm')
vim.opt.mousemodel = "extend"

-- Use * and/or + clipboard registers for yank and put operations.
-- Primary selection: "* register / unnamed
-- System clipboard:  "+ register / unnamedplus
vim.opt.clipboard = "unnamed,unnamedplus"

-- Break lines at better places when wrapping lines.
vim.opt.linebreak = true

-- Keep lines above or below the cursor when scrolling.
vim.opt.scrolloff = 2

-- Keep columns to the left or to the right of the cursor.
vim.opt.sidescrolloff = 5

-- Highlight line and column under cursor. It helps with navigation.
vim.opt.cursorline = true
vim.opt.cursorcolumn = false

-- Highlight column 90. It helps identifying long lines.
vim.opt.colorcolumn = "100"

-- Show the line number relative to the cursor in front of each line and the
-- absolute line number for the one with the cursor.
vim.opt.number = true
vim.opt.relativenumber = true

-- Display signs in the 'number' column.
vim.opt.signcolumn = "auto:1"

-- Open new split panes to right and bottom.
vim.opt.splitbelow = true
vim.opt.splitright = true

-- Reduce scroll when splitting windows.
vim.opt.splitkeep = "screen"

-- Ignore case in patterns (unless upper case characters are used).
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Show tabs, trailing and non-breakable spaces characters.
vim.opt.list = true
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }
-- vim.opt.listchars:append({ eol = "↵" })

-- Disable line wrapping.
vim.opt.wrap = false

-- Insert two spaces after a '.', '?' and '!' with a join command.
vim.opt.joinspaces = true

-- Allow virtual editing (e.g. past end of line) in visual block mode.
vim.opt.virtualedit = "block"

-- Round indent to multiples of 'shiftwidth' when using < or >.
vim.opt.shiftround = true

-- Number of spaces that a <Tab> in the file counts for.
vim.opt.tabstop = 4

-- Always open diff windows vertically.
vim.opt.diffopt:append({ "vertical" })

-- Use popup menu for completion.  Do not auto insert nor auto select.
vim.opt.completeopt = "menuone,noinsert,noselect"

-- Complete till longest common string and list all matches.
vim.opt.wildmode = "longest:full,full"

-- File patterns to ignore.
vim.opt.wildignore:append({ "*.swp" })
vim.opt.wildignore:append({ "*.zip,*.7z,*.tar,*.gz" })
vim.opt.wildignore:append({ "*.pdf" })
vim.opt.wildignore:append({ "*.doc,*.docx,*.xls,*.xslx,*.ppt,*.pptx" })
vim.opt.wildignore:append({ "*.png,*.jpg,*.gif" })
vim.opt.wildignore:append({ "*.pyc,*.pyo,*.pyd" })

-- Neovide options.
-- Reference: https://neovide.dev/configuration.html
if vim.g.neovide then
  -- Font settings
  vim.opt.guifont = "Hack Nerd Font:h9"
  vim.opt.linespace = -1

  -- Visual effects settings
  vim.g.neovide_cursor_vfx_mode = "pixiedust"
  vim.g.neovide_hide_mouse_when_typing = true
  vim.g.neovide_transparency = 0.95
  vim.g.neovide_theme = "dark"
  vim.g.neovide_window_blurred = true

  -- Key mappings
  vim.cmd([[map! <S-Insert> <C-r>+]])
end

------------------------
----- Autocommands -----
------------------------

--- General autocommands ---

-- Autobalance windows in each tab on Neovim resize.
vim.cmd([[autocmd vimrc VimResized * tabdo wincmd =]])

-- Use q to close some support windows.
vim.cmd([[autocmd vimrc FileType help,juliadoc,qf nnoremap <silent> <buffer> q :close<CR>]])

-- Send help windows to the right.
vim.cmd([[autocmd vimrc FileType help,juliadoc setlocal bufhidden=unload | wincmd L]])

-- Enable numbering in help buffers.
vim.cmd([[autocmd vimrc FileType help,juliadoc setlocal number relativenumber]])

-- Set name for first tab.
vim.cmd([[autocmd vimrc VimEnter * let t:tabname = 'main']])

--- Terminal autocmds ---

-- Disable numbering in terminal buffers.
vim.cmd([[autocmd vimrc TermOpen * setlocal nonumber norelativenumber]])

-- Set terminal filetype.
vim.cmd([[autocmd vimrc TermOpen * set filetype=terminal]])

-- Leave terminal mode and enter normal mode when process exits.
vim.cmd([[autocmd vimrc TermClose * stopinsert]])

--- Yank autocmds

-- Highlight yanked region and avoid cursor movement when yanking text.
-- Save view on CursorMoved and restore after yank operation.
-- Reference: https://github.com/svban/YankAssassin.vim
vim.api.nvim_create_augroup("YankSteadyView", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  group = "YankSteadyView",
  desc = "Highlight when yanking text",
  callback = function()
    vim.highlight.on_yank({ timeout = 500 })
  end,
})
local pre_yank_view = nil
vim.api.nvim_create_autocmd({ "VimEnter", "CursorMoved" }, {
  group = "YankSteadyView",
  desc = "Save window view on cursor move",
  callback = function()
    pre_yank_view = vim.fn.winsaveview()
  end,
})
vim.api.nvim_create_autocmd("TextYankPost", {
  group = "YankSteadyView",
  desc = "Restore window view after yanking text",
  callback = function()
    if vim.v.event.operator == "y" and pre_yank_view ~= nil then
      vim.fn.winrestview(pre_yank_view)
    end
  end,
})

------------------------
----- Key mappings -----
------------------------

-- - Used keys reference: :help index
-- - Unused keys reference:
--   - https://vim.fandom.com/wiki/Unused_keys
--   - https://skippi.medium.com/ideas-for-non-leader-vim-mappings-fd32a2769c87
-- - Prefer non recursive maps (_noremap)
-- - Plugin maps (<Plug>) must be recursive
-- - Terminal keycodes issues: https://vim.fandom.com/wiki/Mapping_fast_keycodes_in_terminal_Vim

-- Center screen when browsing through search results.
vim.cmd([[
  nnoremap n nzzzv
  nnoremap N Nzzzv
]])

-- Keep selection when indenting in visual mode.
vim.cmd([[
  vnoremap > >gv
  vnoremap < <gv
]])

-- Use @p to paste with a space before the inserted text.
vim.cmd([[
  let @p="a \<Esc>p"
]])

-- Add big j/k jumps to jumplist.
vim.cmd([[
  nnoremap <expr> j (v:count >= 10 ? "m'" . v:count : "") . 'j'
  nnoremap <expr> k (v:count >= 10 ? "m'" . v:count : "") . 'k'
]])

-- Open help for the word under cursor.
vim.cmd([[
  nnoremap <F1> :help <C-r><C-w><CR>
]])

-- Disable C-q (tmux prefix).
vim.cmd([[
  noremap  <C-q> <Nop>
  lnoremap <C-q> <Nop>
  tnoremap <C-q> <Nop>
]])

-- Clear last search highlighting (Esc is not mapped to anything in normal mode).
vim.cmd([[
  nnoremap <silent> <Esc> :noh<CR><Esc>
]])

-- Terminal escaping mapping.
vim.cmd([[
  tnoremap <C-g> <C-\><C-n>
]])

-- Window navigation mappings.
vim.cmd([[
  nnoremap <C-w>1 1<C-w>w
  nnoremap <C-w>2 2<C-w>w
  nnoremap <C-w>3 3<C-w>w
  nnoremap <C-w>4 4<C-w>w
  nnoremap <C-w>5 5<C-w>w
]])

-- Tab navigation mappings.
vim.cmd([[
  nnoremap <C-w><Tab>c :tabclose<CR>
  nnoremap <C-w><Tab>e :tabedit<CR>
  nnoremap <C-w><Tab>n :tabnext<CR>
  nnoremap <C-w><Tab>p :tabprevious<CR>
  nnoremap <C-w><Tab>o :tabonly<CR>
  nnoremap <C-w><Tab>1 1gt
  nnoremap <C-w><Tab>2 2gt
  nnoremap <C-w><Tab>3 3gt
  nnoremap <C-w><Tab>4 4gt
  nnoremap <C-w><Tab>5 5gt
]])

-----------------------------
----- WhichKey mappings -----
-----------------------------

-- Read user input and run vim command
local function rr(prompt, cmd, completion)
  -- TODO: evaluate how to use Telescope UI for input (e.g. dressing.nvim)
  vim.ui.input({ prompt = prompt, completion = completion }, function(value)
    if value == nil then
      return
    else
      vim.cmd(string.gsub(cmd, "{}", value))
    end
  end)
end

-- Go to next tab, creating a second one if only one exists
local function new_or_next_tab()
  if vim.fn.tabpagenr("$") == 1 then
    vim.cmd.tabedit()
  else
    vim.cmd.tabnext()
  end
end

-- Reference: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_formatting
local function format_buffer()
  vim.lsp.buf.format({
    tabSize = vim.fn.shiftwidth(),
    insertSpaces = vim.bo.expandtab,
    trimTrailingWhitespace = true,
    insertFinalNewline = true,
    trimFinalNewlines = true,
  })
end

-- stylua: ignore start
local LEADER_MAPPINGS = {
  { "<Leader><Leader>", "<Cmd>Telescope buffers theme=ivy<CR>", desc = "Find buffer" },
  { "<Leader>`", "<C-^>",                                       desc = "Alternate file" },

  { "<Leader>w", group = "window" },
  { "<Leader>wp",    "<C-w>p",             desc = "Go to alternate window" },
  { "<Leader>ww",    "<C-w>w",             desc = "Go to next window" },
  { "<Leader>wW",    "<C-w>W",             desc = "Go to previous window" },
  { "<Leader>wh",    "<C-w>h",             desc = "Go left" },
  { "<Leader>wl",    "<C-w>l",             desc = "Go right" },
  { "<Leader>wj",    "<C-w>j",             desc = "Go down" },
  { "<Leader>wk",    "<C-w>k",             desc = "Go up" },
  { "<Leader>wt",    "<C-w>t",             desc = "Go to top-left window" },
  { "<Leader>wb",    "<C-w>b",             desc = "Go to bottom-right window" },
  { "<Leader>wH",    "<C-w>H",             desc = "Move far left" },
  { "<Leader>wL",    "<C-w>L",             desc = "Move far right" },
  { "<Leader>wJ",    "<C-w>J",             desc = "Move to bottom" },
  { "<Leader>wK",    "<C-w>K",             desc = "Move to top" },
  { "<Leader>wc",    "<C-w>c",             desc = "Close window" },
  { "<Leader>wq",    "<C-w>q",             desc = "Quit window" },
  { "<Leader>wn",    "<C-w>n",             desc = "New window" },
  { "<Leader>wo",    "<C-w>o",             desc = "Only window" },
  { "<Leader>ws",    "<C-w>s",             desc = "Split horizontally" },
  { "<Leader>wv",    "<C-w>v",             desc = "Split vertically" },
  { "<Leader>wr",    "<C-w>r",             desc = "Rotate downwards" },
  { "<Leader>wR",    "<C-w>R",             desc = "Rotate upwards" },
  { "<Leader>wT",    "<C-w>T",             desc = "Move to new tab" },
  { "<Leader>w=",    "<C-w>=",             desc = "Balance windows" },
  { "<Leader>w+",    "<C-w>5+",            desc = "Increase height" },
  { "<Leader>w-",    "<C-w>5-",            desc = "Decrease height" },
  { "<Leader>w>",    "<C-w>10>",           desc = "Increase width" },
  { "<Leader>w<lt>", "<C-w>10<",           desc = "Decrease width" },
  { "<Leader>w1",    "<Cmd>1wincmd w<CR>", desc = "Go to window 1" },
  { "<Leader>w2",    "<Cmd>2wincmd w<CR>", desc = "Go to window 2" },
  { "<Leader>w3",    "<Cmd>3wincmd w<CR>", desc = "Go to window 3" },
  { "<Leader>w4",    "<Cmd>4wincmd w<CR>", desc = "Go to window 4" },
  { "<Leader>w5",    "<Cmd>5wincmd w<CR>", desc = "Go to window 5" },

  { "<Leader>b", group = "buffer" },
  { "<Leader>bb", "<Cmd>Telescope buffers theme=ivy<CR>",            desc = "Find buffer" },
  { "<Leader>bw", "<Cmd>write<CR>",                                  desc = "Write buffer" },
  { "<Leader>bs", "<Cmd>write<CR>",                                  desc = "Save buffer" },
  { "<Leader>bn", "<Cmd>bnext<CR>",                                  desc = "Next buffer" },
  { "<Leader>bp", "<Cmd>bprevious<CR>",                              desc = "Previous buffer" },
  { "<Leader>b#", "<Cmd>buffer #<CR>",                               desc = "Alternate buffer" },
  { "<Leader>bd", "<Cmd>lua require('mini.bufremove').delete()<CR>", desc = "Delete buffer" },
  { "<Leader>be", "<Cmd>ene<CR>",                                    desc = "Edit new buffer" },
  { "<Leader>bW", "<Cmd>wall<CR>",                                   desc = "Write all buffers" },
  { "<Leader>br", "<Cmd>edit %<CR>",                                 desc = "Reload current buffer" },
  { "<Leader>bR", "<Cmd>checktime<CR>",                              desc = "Reload all buffers" },

  { "<Leader><Tab>", group = "tab" },
  { "<Leader><Tab><Tab>", new_or_next_tab,        desc = "New or next tab" },
  { "<Leader><Tab>e",     "<Cmd>tabedit<CR>",     desc = "New tab" },
  { "<Leader><Tab>c",     "<Cmd>tabclose<CR>",    desc = "Close tab" },
  { "<Leader><Tab>o",     "<Cmd>tabonly<CR>",     desc = "Only tab" },
  { "<Leader><Tab>n",     "<Cmd>tabnext<CR>",     desc = "Next tab" },
  { "<Leader><Tab>p",     "<Cmd>tabprevious<CR>", desc = "Previous tab" },
  { "<Leader><Tab>w",     "<C-w>T",               desc = "Move window to new tab" },
  { "<Leader><Tab>1",     "1gt",                  desc = "Go to tab 1" },
  { "<Leader><Tab>2",     "2gt",                  desc = "Go to tab 2" },
  { "<Leader><Tab>3",     "3gt",                  desc = "Go to tab 3" },
  { "<Leader><Tab>4",     "4gt",                  desc = "Go to tab 4" },
  { "<Leader><Tab>5",     "5gt",                  desc = "Go to tab 5" },
  { "<Leader><Tab>r", function() rr("Tab name: ", "LualineRenameTab {}") end, desc = "Rename tab", },

  { "<Leader>f", group = "file" },
  { "<Leader>ff", "<Cmd>Telescope find_files noignore=true<CR>", desc = "Find project files" },
  { "<Leader>fF", "<Cmd>Telescope file_browser<CR>",             desc = "File browser" },
  { "<Leader>fh", "<Cmd>Telescope find_files cwd=~<CR>",         desc = "Find home folder files" },
  { "<Leader>fr", "<Cmd>Telescope oldfiles<CR>",                 desc = "Recent files" },
  { "<Leader>fg", "<Cmd>Telescope git_files<CR>",                desc = "Find git files" },
  { "<Leader>fp", "<Cmd>Telescope project<CR>",                  desc = "Find projects" },
  { "<Leader>fn", "<Cmd>ene<CR>",                                desc = "New file" },
  { "<Leader>fD", "<Cmd>Delete<CR>",                             desc = "Delete file" },
  { "<Leader>fR", function() rr("New name: ", "Rename {}") end,  desc = "Rename file", },

  { "<Leader>v", group = "neovim" },
  { "<Leader>vv", "<Cmd>edit $MYVIMRC<CR>",             desc = "Open neovim config" },
  { "<Leader>vp", "<Cmd>Lazy<CR>",                      desc = "Open plugins menu (Lazy)" },
  { "<Leader>vt", "<Cmd>Mason<CR>",                     desc = "Open tools menu (Mason)" },
  { "<Leader>vu", "<Cmd>TSUpdate<CR>",                  desc = "Update Treesitter parsers" },
  { "<Leader>vh", "<Cmd>Alpha<CR>",                     desc = "Open home buffer" },
  { "<Leader>vH", "<Cmd>checkhealth<CR>",               desc = "Check neovim health" },
  { "<Leader>vP", "<Cmd>Luapad<CR>",                    desc = "Open lua scratch pad" },
  { "<Leader>vs", "<Cmd>Telescope possession list<CR>", desc = "Load session" },

  { "<Leader>S", group = "session" },
  { "<Leader>Ss", "<Cmd>PossessionSave!<CR>",           desc = "Save current session" },
  { "<Leader>SS", "<Cmd>PossessionShow<CR>",            desc = "Show session info" },
  { "<Leader>Sl", "<Cmd>Telescope possession list<CR>", desc = "Load session" },
  { "<Leader>SL", "<Cmd>PossessionLoad<CR>",            desc = "Load last session" },
  { "<Leader>Sc", "<Cmd>PossessionClose<CR>",           desc = "Close session" },
  { "<Leader>Sr", "<Cmd>PossessionRename<CR>",          desc = "Rename session" },
  { "<Leader>Sd", "<Cmd>PossessionDelete<CR>",          desc = "Delete session" },

  { "<Leader>s", group = "search" },
  { "<Leader>ss", "<Cmd>Telescope live_grep<CR>",                 desc = "Live grep search" },
  { "<Leader>sS", "<Cmd>Telescope resume<CR>",                    desc = "Resume last search" },
  { "<Leader>sw", "<Cmd>Telescope grep_string<CR>",               desc = "Word under cursor" },
  { "<Leader>sb", "<Cmd>Telescope current_buffer_fuzzy_find<CR>", desc = "Current buffer" },
  { "<Leader>sc", "<Cmd>Telescope commands<CR>",                  desc = "Available commands" },
  { "<Leader>s:", "<Cmd>Telescope command_history<CR>",           desc = "Command history" },
  { "<Leader>s/", "<Cmd>Telescope search_history<CR>",            desc = "Search history" },
  { "<Leader>sh", "<Cmd>Telescope help_tags<CR>",                 desc = "Help tags" },
  { "<Leader>sm", "<Cmd>Telescope marks<CR>",                     desc = "Marks" },
  { "<Leader>sr", "<Cmd>Telescope registers<CR>",                 desc = "Registers" },
  { "<Leader>sq", "<Cmd>Telescope quickfix<CR>",                  desc = "Quickfix list" },
  { "<Leader>sQ", "<Cmd>Telescope quickfixhistory<CR>",           desc = "Quickfix history" },
  { "<Leader>sl", "<Cmd>Telescope loclist<CR>",                   desc = "Location list" },
  { "<Leader>sj", "<Cmd>Telescope jumplist<CR>",                  desc = "Jump list" },
  { "<Leader>so", "<Cmd>Telescope vim_options<CR>",               desc = "Vim options" },
  { "<Leader>sa", "<Cmd>Telescope autocommands<CR>",              desc = "Autocommands" },
  { "<Leader>sk", "<Cmd>Telescope keymaps<CR>",                   desc = "Key maps" },
  { "<Leader>sf", "<Cmd>Telescope filetypes<CR>",                 desc = "Filetypes" },
  { "<Leader>sH", "<Cmd>Telescope highlights<CR>",                desc = "Highlights" },
  { "<Leader>st", "<Cmd>Telescope treesitter<CR>",                desc = "Treesitter" },
  { "<Leader>sT", "<Cmd>Telescope tags<CR>",                      desc = "Tags" },
  { "<Leader>sM", "<Cmd>Telescope man_pages<CR>",                 desc = "Man pages" },
  { "<Leader>sp", "<Cmd>Telescope builtin<CR>",                   desc = "Telescope pickers" },

  { "<Leader>c", group = "code" },
  { "<Leader>cc", "<Cmd>Format<CR>",                    desc = "Format buffer" },
  { "<Leader>cC", format_buffer,                        desc = "Format buffer (LSP)" },
  { "<Leader>ca", "<Cmd>FormatEnable<CR>",              desc = "Enable autoformatting" },
  { "<Leader>cA", "<Cmd>FormatDisable<CR>",             desc = "Disable autoformatting" },
  { "<Leader>cw", "<Cmd>lua MiniTrailspace.trim()<CR>", desc = "Strip whitespace" },
  { "<Leader>co", "<Cmd>AerialToggle<CR>",              desc = "Toggle code outline" },
  { "<Leader>cu", "<Cmd>Telescope undo<CR>",            desc = "Search undo tree" },
  { "<Leader>cn", "<Cmd>Neogen<CR>",                    desc = "Generate annotation" },
  { "<Leader>cs", "<Cmd>Spectre<CR>",                   desc = "Search and replace" },
  { "<Leader>cp", "<Cmd>Copilot panel<CR>",             desc = "Open Copilot window" },
  { "<Leader>ct", "<Cmd>Copilot toggle<CR>",            desc = "Toggle Copilot on/off" },

  { "<Leader>t", group = "trouble" },
  { "<Leader>tt", "<Cmd>Trouble diagnostics toggle filter.buf=0<CR>", desc = "Document diagnostics" },
  { "<Leader>tw", "<Cmd>Trouble diagnostics toggle<CR>",              desc = "Workspace diagnostics" },
  { "<Leader>tq", "<Cmd>Trouble quickfix toggle<CR>",                 desc = "Quickfix items" },
  { "<Leader>tl", "<Cmd>Trouble loclist toggle<CR>",                  desc = "Loclist items" },
  { "<Leader>ts", "<Cmd>Trouble symbols toggle<CR>",                  desc = "Document symbols" },
  { "<Leader>tL", "<Cmd>Trouble lsp toggle<CR>",                      desc = "LSP definitions and references" },
  { "<Leader>tT", "<Cmd>Trouble todo toggle<CR>",                     desc = "TODOs" },
  { "<Leader>tS", "<Cmd>TodoTelescope<CR>",                           desc = "Search TODOs" },

  { "<Leader>d", group = "diff" },
  { "<Leader>dt", "<Cmd>diffthis<CR>",     desc = "Diff this window" },
  { "<Leader>do", "<Cmd>diffoff<CR>",      desc = "Switch off diff" },
  { "<Leader>du", "<Cmd>diffupdate<CR>",   desc = "Update diff" },
  { "<Leader>dg", "<Cmd>diffget<CR>",      desc = "Get diff from other" },
  { "<Leader>dp", "<Cmd>diffput<CR>",      desc = "Put diff to other" },
  { "<Leader>dG", "<Cmd>'<,'>diffget<CR>", desc = "Get selection from other" },
  { "<Leader>dP", "<Cmd>'<,'>diffput<CR>", desc = "Put selection to other" },
  { "<Leader>ds", function() rr("Other file: ", "diffsplit {}", "file") end, desc = "Open split diff window", },

  { "<Leader>l", group = "lsp" },
  { "<Leader>ll", "<Cmd>LspInfo<CR>",                                 desc = "LSP information" },
  { "<Leader>lL", "<Cmd>LspLog<CR>",                                  desc = "LSP log" },
  { "<Leader>ld", "<Cmd>Telescope diagnostics bufnr=0<CR>",           desc = "List diagnostics" },
  { "<Leader>lD", "<Cmd>Telescope lsp_definitions<CR>",               desc = "List definitions" },
  { "<Leader>lt", "<Cmd>Telescope lsp_type_definitions<CR>",          desc = "List type definitions" },
  { "<Leader>lr", "<Cmd>Telescope lsp_references<CR>",                desc = "References for word under cursor" },
  { "<Leader>lI", "<Cmd>Telescope lsp_implementations<CR>",           desc = "List implementations" },
  { "<Leader>ls", "<Cmd>Telescope lsp_document_symbols<CR>",          desc = "Document symbols" },
  { "<Leader>lS", "<Cmd>Telescope lsp_workspace_symbols<CR>",         desc = "Workspace symbols" },
  { "<Leader>ly", "<Cmd>Telescope lsp_dynamic_workspace_symbols<CR>", desc = "Dynamically lists workspace symbols" },
  { "<Leader>li", "<Cmd>Telescope lsp_incoming_calls<CR>",            desc = "List incoming calls" },
  { "<Leader>lo", "<Cmd>Telescope lsp_outgoing_calls<CR>",            desc = "List outgoing calls" },
  { "<Leader>lh", "<Cmd>lua vim.lsp.buf.hover()<CR>",                 desc = "Hover help" },
  { "<Leader>lH", "<Cmd>lua vim.lsp.buf.signature_help()<CR>",        desc = "Signature help" },
  { "<Leader>lR", "<Cmd>lua vim.lsp.buf.rename()<CR>",                desc = "Rename" },
  { "<Leader>la", "<Cmd>lua vim.lsp.buf.code_action()<CR>",           desc = "Code actions" },
  { "<Leader>lp", group = "peek" },
  { "<Leader>lpf",                                                    desc = "Peek function definition" },
  { "<Leader>lpc",                                                    desc = "Peek class definition" },

  { "<Leader>D", group = "debug" },
  { "<Leader>DD", "<Cmd>lua require('dap').continue()<CR>",                                    desc = "Start" },
  { "<Leader>Dc", "<Cmd>lua require('dap').continue()<CR>",                                    desc = "Continue" },
  { "<Leader>Dl", "<Cmd>lua require('dap').run_last()<CR>",                                    desc = "Run last debug" },
  { "<Leader>Dt", "<Cmd>lua require('dap').toggle_breakpoint()<CR>",                           desc = "Toggle breakpoint" },
  { "<Leader>DC", "<Cmd>lua require('dap').set_breakpoint(vim.fn.input '[Condition] > ')<CR>", desc = "Conditional breakpoint" },
  { "<Leader>Dr", "<Cmd>lua require('dap').run_to_cursor()<CR>",                               desc = "Run to cursor" },
  { "<Leader>Dg", "<Cmd>lua require('dap').goto_()<CR>",                                       desc = "Go to line (no execute)" },
  { "<Leader>Di", "<Cmd>lua require('dap').step_into()<CR>",                                   desc = "Step into" },
  { "<Leader>Do", "<Cmd>lua require('dap').step_over()<CR>",                                   desc = "Step over" },
  { "<Leader>Du", "<Cmd>lua require('dap').step_out()<CR>",                                    desc = "Step out" },
  { "<Leader>Db", "<Cmd>lua require('dap').step_back()<CR>",                                   desc = "Step back" },
  { "<Leader>Dj", "<Cmd>lua require('dap').down()<CR>",                                        desc = "Down" },
  { "<Leader>Dk", "<Cmd>lua require('dap').up()<CR>",                                          desc = "Up" },
  { "<Leader>Dn", "<Cmd>lua require('dap').session()<CR>",                                     desc = "Get session" },
  { "<Leader>Dp", "<Cmd>lua require('dap').pause.toggle()<CR>",                                desc = "Pause" },
  { "<Leader>DR", "<Cmd>lua require('dap').repl.toggle()<CR>",                                 desc = "Toggle repl" },
  { "<Leader>Dd", "<Cmd>lua require('dap').disconnect()<CR>",                                  desc = "Disconnect" },
  { "<Leader>Dx", "<Cmd>lua require('dap').terminate()<CR>",                                   desc = "Terminate" },
  { "<Leader>Dq", "<Cmd>lua require('dap').close()<CR>",                                       desc = "Quit" },
  { "<Leader>De", "<Cmd>lua require('dapui').eval()<CR>",                                      desc = "Evaluate" },
  { "<Leader>DE", "<Cmd>lua require('dapui').eval(vim.fn.input '[Expression] > ')<CR>",        desc = "Evaluate input" },
  { "<Leader>DT", "<Cmd>lua require('dapui').toggle()<CR>",                                    desc = "Toggle UI" },
  { "<Leader>Dh", "<Cmd>lua require('dap.ui.widgets').hover()<CR>",                            desc = "Hover variables" },
  { "<Leader>DS", "<Cmd>lua require('dap.ui.widgets').scopes()<CR>",                           desc = "Scopes" },
  { "<Leader>Ds", group = "search" },
  { "<Leader>Dsb", "<Cmd>Telescope dap list_breakpoints<CR>",                                  desc = "Breakpoints" },
  { "<Leader>Dsc", "<Cmd>Telescope dap commands<CR>",                                          desc = "Commands" },
  { "<Leader>DsC", "<Cmd>Telescope dap configurations<CR>",                                    desc = "Configurations" },
  { "<Leader>Dsf", "<Cmd>Telescope dap frames<CR>",                                            desc = "Frames" },
  { "<Leader>Dsv", "<Cmd>Telescope dap variables<CR>",                                         desc = "Variables" },

  { "<Leader>T", group = "test" },
  { "<Leader>TT", "<Cmd>lua require('neotest').summary.toggle()<CR>",               desc = "Toggle summary" },
  { "<Leader>Ts", "<Cmd>lua require('neotest').summary.toggle()<CR>",               desc = "Toggle summary" },
  { "<Leader>Tp", "<Cmd>lua require('neotest').output_panel.toggle()<CR>",          desc = "Toggle output panel" },
  { "<Leader>To", "<Cmd>lua require('neotest').output.open({auto_close=true})<CR>", desc = "Show output" },
  { "<Leader>Tt", "<Cmd>lua require('neotest').run.run()<CR>",                      desc = "Run nearest test" },
  { "<Leader>Td", "<Cmd>lua require('neotest').run.run({strategy='dap'})<CR>",      desc = "Debug nearest test" },
  { "<Leader>Tl", "<Cmd>lua require('neotest').run.run_last()<CR>",                 desc = "Run last test" },
  { "<Leader>TL", "<Cmd>lua require('neotest').run.run_last({strategy='dap'})<CR>", desc = "Debug last test" },
  { "<Leader>Tf", "<Cmd>lua require('neotest').run.run(vim.fn.expand('%'))<CR>",    desc = "Run current file" },
  { "<Leader>TA", "<Cmd>lua require('neotest').run.run(vim.uv.cwd())<CR>",          desc = "Run all tests" },
  { "<Leader>TS", "<Cmd>lua require('neotest').run.stop()<CR>",                     desc = "Stop tests" },

  { "<Leader>g", group = "git" },
  { "<Leader>gg", "<Cmd>tab Git<CR>",                desc = "Git status" },
  { "<Leader>gc", "<Cmd>Git commit<CR>",             desc = "Git commit" },
  { "<Leader>gp", "<Cmd>Git push<CR>",               desc = "Git push" },
  { "<Leader>gl", "<Cmd>Git log<Bar>wincmd L<CR>",   desc = "Git log" },
  { "<Leader>gb", "<Cmd>Git blame<CR>",              desc = "Git blame" },
  { "<Leader>gC", "<Cmd>Telescope git_commits<CR>",  desc = "List git commits" },
  { "<Leader>gB", "<Cmd>Telescope git_branches<CR>", desc = "List git branches" },
  { "<Leader>gS", "<Cmd>Telescope git_status<CR>",   desc = "List changes per files" },
  { "<Leader>gT", "<Cmd>Telescope git_stash<CR>",    desc = "List stash items" },
  { "<Leader>gd", "<Cmd>DiffviewOpen<CR>",           desc = "Diff view" },
  { "<Leader>gf", "<Cmd>DiffviewFileHistory<CR>",    desc = "Diff view file history" },
  { "<Leader>gh", group = "hunk" },
  { "<Leader>gh",                                    desc = "Preview hunk" },
  { "<Leader>gs",                                    desc = "Stage hunk" },
  { "<Leader>gr",                                    desc = "Reset hunk" },
  { "<Leader>gS",                                    desc = "Stage buffer" },
  { "<Leader>gu",                                    desc = "Undo stage hank" },
  { "<Leader>gR",                                    desc = "Reset buffer" },
  { "<Leader>gp",                                    desc = "Preview hunk" },
  { "<Leader>gb",                                    desc = "Blame line" },
  { "<Leader>gB",                                    desc = "Toggle line blame" },
  { "<Leader>gd",                                    desc = "Diff this" },
  { "<Leader>gD",                                    desc = "Diff this ~" },
  { "<Leader>gt",                                    desc = "Toggle deleted" },
  { "<Leader>gT",                                    desc = "Open Trouble" },

  { "<Leader>r", group = "repl" },
  { "<Leader>rr", "<Cmd>IronRepl<CR>",            desc = "Open REPL" },
  { "<Leader>rR", "<Cmd>IronReplHere<CR>",        desc = "Open REPL in current window" },
  { "<Leader>rf", "<Cmd>IronFocus<CR>",           desc = "Focus REPL" },
  { "<Leader>rh", "<Cmd>IronHide<CR>",            desc = "Hide REPL" },
  { "<Leader>rs", "<Cmd>vsplit<Bar>terminal<CR>", desc = "Open system shell" },

  { "<Leader>h", group = "harpoon" },
  { "<Leader>hh", "<Cmd>lua require('harpoon').ui:toggle_quick_menu(require('harpoon'):list())<CR>", desc = "Toggle quick menu" },
  { "<Leader>hH", "<Cmd>Telescope harpoon marks<CR>",                                                desc = "Search marks" },
  { "<Leader>ha", "<Cmd>lua require('harpoon'):list():append()<CR>",                                 desc = "Add file" },
  { "<Leader>h1", "<Cmd>lua require('harpoon'):list():select(1)<CR>",                                desc = "Go to selection 1" },
  { "<Leader>h2", "<Cmd>lua require('harpoon'):list():select(2)<CR>",                                desc = "Go to selection 2" },
  { "<Leader>h3", "<Cmd>lua require('harpoon'):list():select(3)<CR>",                                desc = "Go to selection 3" },
  { "<Leader>h4", "<Cmd>lua require('harpoon'):list():select(4)<CR>",                                desc = "Go to selection 4" },
  { "<Leader>h5", "<Cmd>lua require('harpoon'):list():select(5)<CR>",                                desc = "Go to selection 5" },
  { "<Leader>hn", "<Cmd>lua require('harpoon'):list():next()<CR>",                                   desc = "Go to next mark" },
  { "<Leader>hp", "<Cmd>lua require('harpoon'):list():prev()<CR>",                                   desc = "Go to previous mark" },

  { "<Leader>m", group = "misc" },
  { "<Leader>mi", "<Cmd>IBLToggle<CR>",                                 desc = "Toggle indent lines" },
  { "<Leader>mc", "<Cmd>ColorizerToggle<CR>",                           desc = "Toggle color strings highlighting" },
  { "<Leader>ms", "<Cmd>Telescope spell_suggest<CR>",                   desc = "Spell suggest (word on cursor)" },
  { "<Leader>mt", "<Cmd>Telescope colorscheme enable_preview=true<CR>", desc = "Change color theme" },

  { "<Leader>q", group = "quit" },
  { "<Leader>qq", "<Cmd>quitall<CR>", desc = "Quit all windows" },
}
-- stylua: ignore end

--------------------------------
----- Plugin configuration -----
--------------------------------

-- Install lazy.nvim if not present.
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- See plugin specification at `:help lazy.nvim-lazy.nvim-plugin-spec`
local PLUGINS = {

  --- Neovim management and fixes ---

  -- Configure LuaLS for editing neovim configuration.
  {
    "folke/lazydev.nvim",
    ft = "lua",
  },

  -- Delete buffers without losing window layout.
  {
    "echasnovski/mini.bufremove",
    lazy = true,
    config = function()
      require("mini.bufremove").setup()
    end,
  },

  -- Reopen files at last edit position.
  {
    "ethanholz/nvim-lastplace",
    event = "VeryLazy",
    config = function()
      require("nvim-lastplace").setup({
        lastplace_ignore_buftype = { "quickfix", "nofile", "nowrite", "terminal" },
        lastplace_ignore_filetype = { "gitcommit", "gitrebase" },
        lastplace_open_folds = true,
      })
    end,
  },

  -- Disable cursorline and cursorcolumn in inactive windows.
  {
    "tummetott/reticle.nvim",
    event = "VeryLazy",
    config = function()
      local never_ft = {
        "alpha",
        "lazy",
        "mason",
        "terminal",
      }
      require("reticle").setup({
        on_startup = { cursorline = vim.o.cursorline, cursorcolumn = vim.o.cursorcolumn },
        disable_in_insert = false,
        always_highlight_number = true,
        never = {
          cursorline = never_ft,
          cursorcolumn = never_ft,
        },
      })
    end,
  },

  -- Project management.
  {
    "notjedi/nvim-rooter.lua",
    event = "VeryLazy",
    config = function()
      require("nvim-rooter").setup({
        rooter_patterns = { "pyproject.toml", "Project.toml", "Makefile", ".git", ".venv", "init.lua", "init.el" },
      })
    end,
  },

  -- Automated session management.
  {
    "jedrzejboczar/possession.nvim",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim" },
    event = "VeryLazy",
    config = function()
      require("possession").setup({
        autosave = {
          current = true,
          tmp = true,
        },
      })
      require("telescope").load_extension("possession")
    end,
  },

  -- Interactive real time strachpad for the embedded lua engine.
  {
    "rafcamlet/nvim-luapad",
    cmd = { "Luapad", "LuaRun" },
  },

  -- Execute a command and show the output in a temporary buffer.
  -- Use as :Redir <ex-command> or :lua Redir(<lua-expression>)
  {
    "AckslD/messages.nvim",
    cmd = "Redir",
    init = function()
      Redir = function(...)
        require("messages.api").capture_thing(...)
      end
    end,
    config = function()
      require("messages").setup({
        command_name = "Redir",
        prepare_buffer = function(opts)
          local buf = vim.api.nvim_create_buf(false, true)
          vim.keymap.set("n", "q", "<Cmd>close<CR>", { buffer = buf })
          return vim.api.nvim_open_win(buf, true, opts)
        end,
      })
    end,
  },

  --- Useful keybindings ---

  -- Make repeat command (.) plugin compatible.
  {
    "tpope/vim-repeat",
    event = "VeryLazy",
  },

  -- Jump to any forward (s__) or backward (S__) location specified by two characters.
  -- In case of multiple targets, a third character (label) can be used.
  {
    "ggandor/leap.nvim",
    dependencies = { "tpope/vim-repeat" },
    keys = { "s", "S" },
    config = function()
      require("leap")
      for _, mode in ipairs({ "n", "x", "o" }) do
        vim.api.nvim_set_keymap(mode, "s", "<Plug>(leap-forward)", {})
        vim.api.nvim_set_keymap(mode, "S", "<Plug>(leap-backward)", {})
      end
    end,
  },

  -- Add (ys_), change (cs_), remove (ds_) surrounding delimiters (_ss for whole line).
  {
    "kylechui/nvim-surround",
    keys = { "ys", "cs", "ds", { "S", mode = "v" } },
    config = true,
  },

  -- Set basic useful options and mappings:
  -- - go and gO:   Add empty lines after/before line
  -- - gV:          Select latest changed, put or yanked text
  -- - g/, * and #: Search inside current visual selection
  -- - C-s:         Save and go to normal mode
  -- Ignored plugin mappings: j, k, gy, gp, gP
  {
    "echasnovski/mini.basics",
    event = { "VeryLazy" },
    config = function()
      require("mini.basics").setup({
        options = {
          basic = false, -- Too opinionated
          extra_ui = true, -- Make popups transparent
          win_borders = "default",
        },
        mappings = {
          basic = true,
          option_toggle_prefix = [[\]],
          windows = false,
          move_with_alt = false,
        },
        autocommands = {
          basic = false, -- Too simple
          relnum_in_visual_mode = false,
        },
      })
      -- Use gp and gP to paste linewise.
      vim.keymap.set({ "n", "x" }, "gp", "<Cmd>execute 'put ' . v:register<CR>", { desc = "Paste below" })
      vim.keymap.set({ "n", "x" }, "gP", "<Cmd>execute 'put! ' . v:register<CR>", { desc = "Paste above" })
    end,
  },

  -- Useful [_, ]_ keybindings, with capital letters for first/last semantics:
  -- - b: buffer
  -- - k: comment
  -- - x: git conflict marker
  -- - d: diagnostic
  -- - f: file on disk
  -- - i: indent change
  -- - j: jump (in current buffer)
  -- - l: location list
  -- - q: quickfix list
  -- - o: oldfile (see :oldfiles)
  -- - t: treesitter node and parents
  -- - u: undo states
  -- - w: window (in current tab)
  -- - y: yank selection
  {
    "echasnovski/mini.bracketed",
    keys = { "[", "]" },
    config = function()
      require("mini.bracketed").setup({
        comment = { suffix = "k" },
      })
    end,
  },

  -- Use C-k and C-j to move lines up or down.  Also on visual mode.
  {
    "echasnovski/mini.move",
    keys = { "<C-j>", "<C-k>" },
    config = function()
      require("mini.move").setup({
        -- C-l is already taken.  Use < and > for lateral moves instead.
        mappings = {
          -- Move visual selection in Visual mode.
          left = "",
          right = "",
          down = "<C-j>",
          up = "<C-k>",
          -- Move current line in Normal mode
          line_left = "",
          line_right = "",
          line_down = "<C-j>",
          line_up = "<C-k>",
        },
      })
    end,
  },

  -- Text edit operators:
  -- - evaluate (=) text and replace with output
  -- - eXchange text regions (use C-c to clear pending exchanges)
  -- - Multiply (duplicate) text (e.g. gmm to duplicate line)
  -- - Replace text with register's contents
  -- - Sort text (e.g. gsib to sort inside block)
  {
    "echasnovski/mini.operators",
    keys = { "g=", "gm", "gr", "gs", "gx" },
    config = function()
      require("mini.operators").setup({
        evaluate = { prefix = "g=" },
        exchange = { prefix = "gx" },
        multiply = { prefix = "gm" },
        replace = { prefix = "gr" },
        sort = { prefix = "gs" },
      })
    end,
  },

  -- Coerce text cases with: crs (snake_case), crm (MixedCase), crc (camelCase),
  -- cru (UPPER_CASE), cr- (dash-case), cr. (dot.case), cr<space> (space case),
  -- crt (Title Case).
  -- Search and replace words with with variants like plural and case. Use e.g.
  -- :%S/facilit{y,ies}/building{,s}/gc
  {
    "tpope/vim-abolish",
    keys = { "cr" },
    cmd = { "Subvert", "S", "Abolish" },
  },

  -- Readline/Emacs keybindings in insert and command modes:
  -- C-b, C-f:  back/forward character
  -- C-a, C-e:  beginning/end of line
  -- C-h, M-BS: backward delete character/word
  -- C-d, M-d:  forward delete character/word
  -- C-u, C-k:  backward/forward kill line
  -- M-b, M-f:  back/forward word
  -- M-n, M-p:  down/up line (keep C-n/C-p native bindings)
  -- C-w:       unix word rubout (backward delete word)
  -- M-m:       go back to indentation
  {
    "assistcontrol/readline.nvim",
    event = "VeryLazy",
    config = function()
      local readline = require("readline")
      vim.keymap.set({ "i", "c" }, "<C-b>", "<Left>")
      vim.keymap.set({ "i", "c" }, "<C-f>", "<Right>")
      vim.keymap.set({ "i", "c" }, "<C-a>", readline.dwim_beginning_of_line)
      vim.keymap.set({ "i", "c" }, "<C-e>", readline.end_of_line)
      vim.keymap.set({ "i", "c" }, "<C-h>", "<BS>")
      --vim.keymap.set({ "i", "c" }, "<M-BS>", readline.backward_kill_word)
      vim.keymap.set({ "i", "c" }, "<C-d>", "<Delete>")
      vim.keymap.set({ "i", "c" }, "<M-d>", readline.kill_word)
      --vim.keymap.set({ "i", "c" }, "<C-u>", readline.dwim_backward_kill_line)
      vim.keymap.set({ "i", "c" }, "<C-k>", readline.kill_line)
      vim.keymap.set({ "i", "c" }, "<M-b>", readline.backward_word)
      vim.keymap.set({ "i", "c" }, "<M-f>", readline.forward_word)
      vim.keymap.set({ "i", "c" }, "<M-n>", "<Down>")
      vim.keymap.set({ "i", "c" }, "<M-p>", "<Up>")
      --vim.keymap.set({ "i", "c" }, "<C-w>", readline.unix_word_rubout)
      vim.keymap.set({ "i", "c" }, "<M-m>", readline.back_to_indentation)
    end,
  },

  --- Editing helps ---

  -- Align text by some character (or pattern) adding spaces to its left and/or right.
  -- Type ga in visual mode (or ga followed by motion or text object in normal mode) and the split character.
  -- For complex scenarios, use gA for an interactive preview mode.
  {
    "echasnovski/mini.align",
    keys = { "ga", { "ga", mode = "v" }, "gA", { "gA", mode = "v" } },
    config = function()
      require("mini.align").setup()
    end,
  },

  -- Insert and delete brackets, parenthesis and quotes in pairs.
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = function()
      require("nvim-autopairs").setup()
    end,
  },

  -- Navigate between bookmarked files.
  {
    "ThePrimeagen/harpoon",
    branch = "harpoon2",
    lazy = true, -- loaded when require'd
    dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim" },
    config = function()
      require("harpoon"):setup({})
      require("telescope").load_extension("harpoon")
    end,
  },

  --- Custom motions and text objects ---

  -- Several text objects with inside (i) and around (a) semantics.
  -- Pairs: () {} [] <>
  -- Quotes: ' " `
  -- Separators: , . ; : + - = ~ _ * # / | & $
  -- XML/HTML tags: t
  -- Any Block: b (similar to pairs, but skips pairs in nested contexts)
  -- Any Quote: q (similar to quotes, but skips pairs in nested contexts)
  -- Arguments: a (surrounded by braces and/or commas)
  -- Function call: f
  -- Custom chars: ? (prompts for left and right characters)
  -- Use g[a_ and g]a_ to move to the edges of a textobject near the cursor.
  {
    "echasnovski/mini.ai",
    event = "BufReadPre",
    config = function()
      require("mini.ai").setup({
        mappings = {
          around = "a",
          inside = "i",
          goto_left = "g[",
          goto_right = "g]",
        },
        search_method = "cover_or_nearest",
      })
    end,
  },

  -- Several more text objects with inside (i) and around (a) semantics.
  -- Indentation level text object: ii (indentation level), ai (ii and line above), aI (ii with lines above/below).
  {
    "chrisgrieser/nvim-various-textobjs",
    event = "BufReadPre",
    config = function()
      require("various-textobjs").setup()

      -- Set custom mappings.
      vim.keymap.set({ "o", "x" }, "ii", "<Cmd>lua require('various-textobjs').indentation('inner', 'inner')<CR>")
      vim.keymap.set({ "o", "x" }, "ai", "<Cmd>lua require('various-textobjs').indentation('outer', 'inner')<CR>")
      vim.keymap.set({ "o", "x" }, "iI", "<Cmd>lua require('various-textobjs').indentation('inner', 'inner')<CR>")
      vim.keymap.set({ "o", "x" }, "aI", "<Cmd>lua require('various-textobjs').indentation('outer', 'outer')<CR>")
      vim.keymap.set({ "o", "x" }, "iS", "<Cmd>lua require('various-textobjs').subword('inner')<CR>")
      vim.keymap.set({ "o", "x" }, "aS", "<Cmd>lua require('various-textobjs').subword('outer')<CR>")
      vim.keymap.set({ "o", "x" }, "ie", "<Cmd>lua require('various-textobjs').entireBuffer()<CR>")
      vim.keymap.set({ "o", "x" }, "ae", "<Cmd>lua require('various-textobjs').entireBuffer()<CR>")
      vim.keymap.set({ "o", "x" }, "ik", "<Cmd>lua require('various-textobjs').multiCommentedLines()<CR>")
      vim.keymap.set({ "o", "x" }, "ak", "<Cmd>lua require('various-textobjs').multiCommentedLines()<CR>")
      vim.keymap.set({ "o", "x" }, "iV", "<Cmd>lua require('various-textobjs').value('inner')<CR>")
      vim.keymap.set({ "o", "x" }, "aV", "<Cmd>lua require('various-textobjs').value('outer')<CR>")
      vim.keymap.set({ "o", "x" }, "iK", "<Cmd>lua require('various-textobjs').key('inner')<CR>")
      vim.keymap.set({ "o", "x" }, "aK", "<Cmd>lua require('various-textobjs').key('outer')<CR>")
      vim.keymap.set({ "o", "x" }, "iu", "<Cmd>lua require('various-textobjs').url()<CR>")
      vim.keymap.set({ "o", "x" }, "au", "<Cmd>lua require('various-textobjs').url()<CR>")
      vim.keymap.set({ "o", "x" }, "in", "<Cmd>lua require('various-textobjs').number('inner')<CR>")
      vim.keymap.set({ "o", "x" }, "an", "<Cmd>lua require('various-textobjs').number('outer')<CR>")
    end,
  },

  -- Search and replace panel using rg/sed.
  {
    "nvim-pack/nvim-spectre",
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = "Spectre",
    config = function()
      require("spectre").setup()
    end,
  },

  --- Language support ---

  -- Automatic tab/indenting configuration.
  -- TODO: consider using expandtab, tabstop, softtabstop, shiftwidth explicitly
  {
    "tpope/vim-sleuth",
    event = "BufReadPre",
  },

  -- Autoformat code.
  {
    "stevearc/conform.nvim",
    event = { "BufReadPre", "BufNewFile" },
    cmd = "ConformInfo",

    init = function()
      -- Format with gq
      vim.o.formatexpr = "v:lua.require('conform').formatexpr()"

      -- Define :Format command
      vim.api.nvim_create_user_command("Format", function(args)
        local range = nil
        if args.count ~= -1 then
          local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
          range = {
            start = { args.line1, 0 },
            ["end"] = { args.line2, end_line:len() },
          }
        end
        require("conform").format({ async = false, lsp_fallback = true, range = range })
      end, { range = true })

      -- Define :FormatEnable and FormatDisable commands
      vim.api.nvim_create_user_command("FormatEnable", function(args)
        -- Use ! for buffer-local setup
        if args.bang then
          ---@diagnostic disable-next-line: inject-field
          vim.b.enable_autoformat = true
        else
          vim.g.enable_autoformat = true
        end
      end, {
        desc = "Enable autoformat-on-save",
        bang = true,
      })
      vim.api.nvim_create_user_command("FormatDisable", function()
        ---@diagnostic disable-next-line: inject-field
        vim.b.enable_autoformat = false
        vim.g.enable_autoformat = false
      end, { desc = "Disable autoformat-on-save" })
    end,

    config = function()
      require("conform").setup({
        formatters_by_ft = {
          fish = { "fish_indent" },
          julia = { "runic" },
          lua = { "stylua" },
          markdown = { "mdformat" },
          python = { "isort", "black" },
          sh = { "shfmt" },
        },
        formatters = {
          isort = {
            prepend_args = { "--profile", "black" },
          },
          runic = {
            command = "julia",
            args = { "--project=@runic", "--startup-file=no", "-e", "using Runic; exit(Runic.main(ARGS))" },
          },
        },
        format_on_save = function(bufnr)
          if vim.g.enable_autoformat or vim.b[bufnr].enable_autoformat then
            return { timeout_ms = 500, lsp_fallback = true }
          else
            return nil
          end
        end,
      })

      -- Enable auto formatting for some filetypes.
      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "lua" },
        group = "vimrc",
        command = "FormatEnable!",
      })
    end,
  },

  -- Lint code.
  {
    "mfussenegger/nvim-lint",
    event = { "BufWritePost", "BufReadPost", "InsertLeave" },
    config = function()
      require("lint").linters_by_ft = {
        markdown = { "markdownlint" },
        python = { "ruff" },
      }
      vim.api.nvim_create_autocmd("BufWritePost", {
        group = "vimrc",
        desc = "Run linters",
        callback = function()
          require("lint").try_lint()
        end,
      })
    end,
  },

  -- Configure LSP and Mason.
  -- See :help lspconfig-all for a list of all pre-configured servers.
  -- Attention: do not try to lazy load this!
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      -- Mason
      { "williamboman/mason.nvim" },
      { "williamboman/mason-lspconfig.nvim" },
      { "WhoIsSethDaniel/mason-tool-installer.nvim" },

      -- Fidget
      {
        "j-hui/fidget.nvim",
        opts = {},
      },
    },

    config = function()
      -- Setup Mason.
      require("mason").setup()

      -- Language servers to enable.
      -- Use the lsp_servers table below to override the default configuration. Available keys are:
      -- cmd, filetypes, capabilities and settings.
      -- References:
      -- - https://microsoft.github.io/language-server-protocol/specifications/specification-current
      -- - https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
      local lsp_servers = {
        -- The configuration for lua_ls is mostly done by the lazydev plugin.
        lua_ls = {
          settings = {
            Lua = {
              completion = { callSnippet = "Replace" },
              diagnostics = { globals = { "vim" }, disable = { "missing-fields" } },
              telemetry = { enable = false },
            },
          },
        },
        basedpyright = {
          settings = {
            basedpyright = {
              analysis = {
                typeCheckingMode = "standard",
              },
            },
          },
        },
        ruff = {},
        ty = {},
        julials = {},
        fish_lsp = {},
        clangd = {},
        jsonls = {
          settings = {
            json = {
              format = { enable = true },
              validate = { enable = true },
              schemas = require("schemastore").json.schemas(),
            },
          },
        },
        yamlls = {
          capabilities = {
            textDocument = {
              foldingRange = {
                dynamicRegistration = false,
                lineFoldingOnly = true,
              },
            },
          },
          settings = {
            yaml = {
              keyOrdering = false,
              format = { enable = true },
              validate = true,
              schemaStore = { enable = false, url = "" }, -- Disable builtin support
              schemas = require("schemastore").yaml.schemas(),
            },
          },
        },
        marksman = {},
        typos_lsp = {
          filetypes = {
            "awk",
            "bash",
            "c",
            "cpp",
            "fish",
            "gitcommit",
            "gitconfig",
            "gitignore",
            "go",
            "haskell",
            "java",
            "javascript",
            "julia",
            "lisp",
            "lua",
            "ps1",
            "python",
            "r",
            "rst",
            "rust",
            "sh",
            "sql",
            "text",
            "tmux",
            "vim",
          },
        },
      }

      -- Add complete capabilities provided by cmp and broadcast them to the servers.
      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

      require("mason-lspconfig").setup({
        ensure_installed = vim.tbl_keys(lsp_servers),
        handlers = {
          function(server_name)
            local config = lsp_servers[server_name] or {}
            config.capabilities = vim.tbl_deep_extend("force", {}, capabilities, config.capabilities or {})
            require("lspconfig")[server_name].setup(config)
          end,
        },
      })

      -- Auto update installed servers and tools.
      require("mason-tool-installer").setup({
        auto_update = true,
        run_on_start = true,
        start_delay = 10000,
        debounce_hours = 24 * 5,
        ensure_installed = {
          "markdownlint",
          "selene",
          "stylua",
        },
      })

      -- Create autocmd that is run when a LSP attaches to a particular buffer.
      -- See :help lsp-defaults
      vim.api.nvim_create_autocmd("LspAttach", {
        group = "vimrc",
        desc = "Setup buffer for LSP",

        callback = function(event)
          -- Set LSP buffer local key maps
          local map = function(mode, keys, func, desc)
            vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = "LSP " .. desc })
          end
          -- stylua: ignore start
          -- TODO: test all these mapping
          map("n", "gd",     vim.lsp.buf.definition,      "go to definition")
          map("n", "gD",     vim.lsp.buf.declaration,     "go to declaration")
          map("n", "g<C-d>", vim.lsp.buf.type_definition, "go to type definition")
          map("n", "gI",     vim.lsp.buf.implementation,  "go to implementation")
          map("n", "gR",     vim.lsp.buf.references,      "go to references")
          map("n", "gS",     vim.lsp.buf.signature_help,  "display signature help")
          map("n", "<F2>",   vim.lsp.buf.rename,          "rename")
          map("n", "<F3>",   vim.lsp.buf.format,          "format")
          map("x", "<F3>",   vim.lsp.buf.format,          "format")
          map("n", "<F4>",   vim.lsp.buf.code_action,     "code action")
          map("x", "<F4>",   vim.lsp.buf.code_action,     "code action")
          -- stylua: ignore end
        end,
      })

      -- UI settings
      local style = "rounded"
      vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = style })
      vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = style })
      vim.diagnostic.config({ float = { border = style } })
    end,
  },

  -- Autocompletion.
  -- See `:help cmp` and `:help ins-completion`
  {
    "hrsh7th/nvim-cmp",
    event = { "InsertEnter" },
    dependencies = {
      -- Snippets
      { "L3MON4D3/LuaSnip", build = (vim.g.os ~= "Windows" and "make install_jsregexp" or {}) },
      -- TODO: evaluate this better
      {
        "rafamadriz/friendly-snippets",
        config = function()
          require("luasnip.loaders.from_vscode").lazy_load()
        end,
      },

      -- Autocompletion
      { "hrsh7th/cmp-buffer" },
      { "hrsh7th/cmp-nvim-lsp" },
      { "hrsh7th/cmp-path" },
      { "kdheepak/cmp-latex-symbols" },
      { "saadparwaiz1/cmp_luasnip" },
    },

    config = function()
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      luasnip.config.setup()

      cmp.setup({
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },

        -- Make cmp's completion.completeopt consistent with vim.opt.completeopt.
        completion = { completeopt = table.concat(vim.opt.completeopt:get(), ",") },

        -- TODO: add (rounded) borders around documentation windows

        mapping = cmp.mapping.preset.insert({
          -- Select next/previous item.
          ["<C-n>"] = cmp.mapping.select_next_item(),
          ["<C-p>"] = cmp.mapping.select_prev_item(),

          -- Accept (yes) the completion.
          ["<C-y>"] = cmp.mapping.confirm({ select = true }),

          -- Abort (escape) the completion.
          ["<C-e>"] = cmp.mapping(function()
            if cmp.visible() then
              cmp.mapping.abort()
            else
              require("readline").end_of_line()
            end
          end),

          -- Scroll documentation window back/forward.
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),

          -- Move right/left in the snippet expansion.
          ["<C-l>"] = cmp.mapping(function()
            if luasnip.expand_or_locally_jumpable() then
              luasnip.expand_or_jump()
            end
          end, { "i", "s" }),
          ["<C-h>"] = cmp.mapping(function()
            if luasnip.locally_jumpable(-1) then
              luasnip.jump(-1)
            end
          end, { "i", "s" }),
        }),

        sources = {
          { name = "latex_symbols" },
          { name = "nvim_lsp", keyword_length = 2 },
          { name = "luasnip", keyword_length = 2 },
          { name = "buffer", keyword_length = 4 },
          { name = "path" },
        },
      })
    end,
  },

  -- Copilot code suggestions.
  -- M-l: accept suggestion
  -- M-]: next suggestion
  -- M-[: previous suggestion
  -- TODO: evaluate using Copilot as a cmp completion source.
  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
    dependencies = { "hrsh7th/nvim-cmp" },
    config = function()
      require("copilot").setup({
        suggestion = {
          auto_trigger = true,
          keymap = {
            accept = "<C-y>",
            dismiss = "<C-e>",
            next = "<M-]>",
            prev = "<M-[>",
          },
        },
      })
      local cmp = require("cmp")
      cmp.event:on("menu_opened", function()
        vim.b.copilot_suggestion_hidden = true
      end)
      cmp.event:on("menu_closed", function()
        vim.b.copilot_suggestion_hidden = false
      end)
    end,
  },

  -- Configure DAP support.
  {
    "mfussenegger/nvim-dap",
    lazy = true, -- Loaded when require'd
    dependencies = {
      -- DAP-ui
      {
        "rcarriga/nvim-dap-ui",
        dependencies = { "nvim-neotest/nvim-nio" },
        config = function(_, opts)
          local dap = require("dap")
          local dapui = require("dapui")
          dapui.setup(opts)

          dap.listeners.after.event_initialized["dapui_config"] = dapui.open
          dap.listeners.before.event_terminated["dapui_config"] = dapui.close
          dap.listeners.before.event_exited["dapui_config"] = dapui.close
        end,
      },

      -- Virtual text for debugger
      {
        "theHamsta/nvim-dap-virtual-text",
        opts = { commented = true },
      },

      -- Telescope DAP extension
      {
        "nvim-telescope/telescope-dap.nvim",
      },

      -- Mason integration
      {
        "jay-babu/mason-nvim-dap.nvim",
        dependencies = { "williamboman/mason.nvim" },
        cmd = { "DapInstall", "DapUninstall" },
        opts = {
          automatic_installation = true,
          handlers = {},
          ensure_installed = {
            "python",
          },
        },
      },
    },
    config = function()
      local dap = require("dap")
      require("telescope").load_extension("dap")

      -- Debugging keymaps.
      vim.keymap.set("n", "<F5>", dap.continue, { desc = "Debug: start/continue" })
      vim.keymap.set("n", "<F9>", dap.toggle_breakpoint, { desc = "Debug: toggle breakpoint" })
      vim.keymap.set("n", "<F10>", dap.step_over, { desc = "Debug: step over" })
      vim.keymap.set("n", "<F11>", dap.step_into, { desc = "Debug: step into" })
      vim.keymap.set("n", "<S-F11>", dap.step_out, { desc = "Debug: step out" })
    end,
  },

  -- Syntax highlighting, indentation, folding and more using ASTs.
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "master", -- TODO: change to main branch
    event = { "VeryLazy" },
    cmd = { "TSUpdate", "TSUpdateSync", "TSInstall" },
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
    build = function()
      require("nvim-treesitter.install").update({ with_sync = true })
    end,
    init = function()
      -- Use treesitter to manage folds.
      vim.opt.foldmethod = "expr"
      vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
      vim.opt.foldenable = false
    end,
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = {
          "bash",
          "c",
          "comment",
          "cpp",
          "csv",
          "diff",
          "fish",
          "git_config",
          "gitcommit",
          "gitignore",
          "go",
          "haskell",
          "html",
          "java",
          "javascript",
          "json",
          "jsonc",
          "julia",
          "lua",
          "luadoc",
          "markdown",
          "markdown_inline",
          "python",
          "query",
          "r",
          "regex",
          "rst",
          "rust",
          "sql",
          "tmux",
          "toml",
          "vim",
          "vimdoc",
          "xml",
          "yaml",
        },
        sync_install = false,
        auto_install = true,
        ignore_install = {},
        -- https://github.com/nvim-treesitter/nvim-treesitter#i-want-to-use-a-http-proxy-for-downloading-the-parsers
        prefer_git = true,
        -- Modules
        modules = {},
        highlight = { enable = true },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<Tab>",
            node_incremental = "<Tab>",
            node_decremental = "<S-Tab>",
            scope_incremental = false,
          },
        },
        indent = { enable = true },
        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = {
              -- Use upper case bindings to avoid conflicts with other plugins' textobjects.
              ["aA"] = "@parameter.outer",
              ["iA"] = "@parameter.inner",
              ["aF"] = "@function.outer",
              ["iF"] = "@function.inner",
              ["aC"] = "@class.outer",
              ["iC"] = "@class.inner",
            },
            selection_modes = {
              ["@class.outer"] = "V",
              ["@class.inner"] = "V",
            },
            include_surrounding_whitespace = true,
          },
          move = {
            enable = true,
            set_jumps = true,
            goto_next_start = {
              ["]]"] = "@function.outer",
              ["]c"] = "@class.outer",
            },
            goto_next_end = {
              ["]["] = "@function.outer",
              ["]C"] = "@class.outer",
            },
            goto_previous_start = {
              ["[["] = "@function.outer",
              ["[c"] = "@class.outer",
            },
            goto_previous_end = {
              ["[]"] = "@function.outer",
              ["[C"] = "@class.outer",
            },
          },
          lsp_interop = {
            enable = true,
            border = "rounded",
            peek_definition_code = {
              ["<Leader>lpf"] = "@function.outer",
              ["<Leader>lpc"] = "@class.outer",
            },
          },
        },
      })
    end,
  },

  -- Code outline window.
  -- Use :AerialToggle to open/close window and g? to show help.
  {
    "stevearc/aerial.nvim",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
      "nvim-treesitter/nvim-treesitter",
    },
    cmd = { "AerialToggle" },
    config = function()
      require("aerial").setup({})
    end,
  },

  -- Run test suites (e.g. pytest).
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",

      -- Language adapters
      "nvim-neotest/neotest-python",
    },
    lazy = true,
    config = function()
      require("neotest").setup({
        adapters = {
          require("neotest-python"),
        },
      })
      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "neotest-output", "neotest-output-panel", "neotest-summary" },
        group = "vimrc",
        command = "nnoremap <silent> <buffer> q :close<CR>",
      })
    end,
  },

  -- Generate code annotations for function, class, type or file.
  {
    "danymat/neogen",
    cmd = "Neogen",
    config = function()
      require("neogen").setup({
        snippet_engine = "luasnip",
      })
    end,
  },

  --- Language plugins ---

  -- Reference: https://github.com/sheerun/vim-polyglot#language-packs

  -- SchemaStore catalog support for jsonls and yamlls.
  {
    "b0o/SchemaStore.nvim",
    lazy = true,
  },

  --- Terminal and file management support ---

  -- Send code to REPL: send motion in normal mode (gy_) or visual mode (gy) and send line (gyy).
  -- Also check terminal autocommands when configuring REPL behaviour.
  {
    "Vigemus/iron.nvim",
    keys = { "gy", { "gy", mode = "v" } },
    cmd = { "IronRepl", "IronReplHere", "IronFocus", "IronHide", "IronSend" },
    config = function()
      require("iron.core").setup({
        config = {
          repl_definition = {
            python = {
              command = "ipython",
              format = require("iron.fts.common").bracketed_paste,
            },
          },
          repl_open_cmd = require("iron.view").split.vertical.botright(0.4),
          scope = require("iron.scope").tab_based,
          close_window_on_exit = false,
          buflisted = true,
        },
        keymaps = {
          send_motion = "gy",
          visual_send = "gy",
          send_line = "gyy",
        },
      })
    end,
  },

  -- File manager plugin that allows file manipulation by editing the directory as a buffer.
  -- Save buffer to modify filesystem (including new files and file permissions).
  -- Use <CR> to open file, g? to show help, g. to toggle hidden files and <BS> to open parent directory.
  {
    "stevearc/oil.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    keys = { "<BS>" },
    cmd = { "Oil" },
    config = function()
      require("oil").setup({
        columns = { "mtime", "size", "permissions", "icon" },
        delete_to_trash = true,
      })
      vim.api.nvim_set_keymap("n", "<BS>", "<Cmd>Oil<CR>", { desc = "Open parent directory" })
    end,
  },

  -- Shell commands :Delete, :Move, :Rename, :Mkdir, :Chmod, :Wall (save all).
  {
    "tpope/vim-eunuch",
    cmd = { "Delete", "Move", "Rename", "Mkdir", "Chmod", "Wall" },
  },

  --- Git integration ---

  -- Git support (:Git).
  {
    "tpope/vim-fugitive",
    cmd = { "Git" },
    config = function()
      vim.cmd([[autocmd vimrc FileType fugitive nmap <buffer> <Tab> =]])
    end,
  },

  -- Show a git diff in the sign column.
  {
    "lewis6991/gitsigns.nvim",
    event = "BufReadPre",
    cmd = { "Gitsigns" },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      -- TODO: set key bindings: https://github.com/lewis6991/gitsigns.nvim#keymaps
      require("gitsigns").setup({
        on_attach = function(bufnr)
          local gs = package.loaded.gitsigns
          local function map(mode, lhs, rhs, opts)
            opts = opts or {}
            opts.buffer = bufnr
            vim.keymap.set(mode, lhs, rhs, opts)
          end
          -- Navigation
          map("n", "]h", function()
            if vim.wo.diff then
              return "]c"
            end
            vim.schedule(function()
              gs.next_hunk()
            end)
            return "<Ignore>"
          end, { expr = true, desc = "Next hunk" })
          map("n", "[h", function()
            if vim.wo.diff then
              return "[c"
            end
            vim.schedule(function()
              gs.prev_hunk()
            end)
            return "<Ignore>"
          end, { expr = true, desc = "Previous hunk" })
          -- Actions
          map("n", "<Leader>ghh", gs.preview_hunk)
          map({ "n", "v" }, "<Leader>ghs", ":Gitsigns stage_hunk<CR>")
          map({ "n", "v" }, "<Leader>ghr", ":Gitsigns reset_hunk<CR>")
          map("n", "<Leader>ghS", gs.stage_buffer)
          map("n", "<Leader>ghu", gs.undo_stage_hunk)
          map("n", "<Leader>ghR", gs.reset_buffer)
          map("n", "<Leader>ghp", gs.preview_hunk)
          map("n", "<Leader>ghb", function()
            gs.blame_line({ full = true })
          end)
          map("n", "<Leader>ghB", gs.toggle_current_line_blame)
          map("n", "<Leader>ghd", gs.diffthis)
          map("n", "<Leader>ghD", function()
            gs.diffthis("~")
          end)
          map("n", "<Leader>ght", gs.toggle_deleted)
          map("n", "<Leader>ghT", gs.setqflist)
          -- Text object
          map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>")
        end,
      })
    end,
  },

  -- Tab page interface for cycling through diffs
  {
    "sindrets/diffview.nvim",
    cmd = { "DiffviewOpen", "DiffviewFileHistory" },
    dependencies = { "nvim-lua/plenary.nvim" },
    config = true,
  },

  --- Search commands ---

  -- Extendable fuzzy finder.
  -- Use <C-/> and ? (in insert and normal mode) to show keymaps.
  -- Other useful mappings:
  -- - <Esc>, <C-c>: close telescope (in normal/insert mode)
  -- - <C-q>: send items to quickfix list
  -- - <C-u>, <C-d>: scroll up/down in preview window
  -- - <C-x>, <C-v>: open selection in horizontal/vertical split
  -- - <C-t>: open selection in new tab
  {
    "nvim-telescope/telescope.nvim",
    branch = "0.1.x",
    cmd = { "Telescope" },
    dependencies = {
      { "nvim-lua/plenary.nvim" },
      { "nvim-telescope/telescope-file-browser.nvim" },
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
      { "debugloop/telescope-undo.nvim" },
    },
    config = function()
      require("telescope").setup({
        pickers = {
          buffers = {
            mappings = {
              i = {
                ["<C-d>"] = "delete_buffer",
              },
            },
          },
        },
        extensions = {
          undo = {
            side_by_side = true,
            layout_strategy = "vertical",
            layout_config = { preview_height = 0.8 },
          },
        },
      })
      require("telescope").load_extension("file_browser")
      require("telescope").load_extension("fzf")
      require("telescope").load_extension("undo")
    end,
  },

  --- Windows, interface elements, visual editing helpers and themes ---

  -- Show start screen.
  -- Note: some colorschemes (e.g. tokyonight and catppuccin) define the highlights:
  -- AlphaShortcut, AlphaHeader, AlphaHeaderLabel, AlphaFooter and AlphaButtons.
  {
    "goolord/alpha-nvim",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
      { "rubiin/fortune.nvim", opts = { display_format = "mixed", max_width = 52, content_type = "mixed" } },
    },
    event = "VimEnter",
    init = function()
      -- Disable :intro screen
      vim.opt.shortmess:append({ I = true })
    end,
    config = function()
      local dashboard = require("alpha.themes.dashboard")

      -- Header (see https://github.com/MaximilianLloyd/ascii.nvim)
      local header = dashboard.section.header
      header.val = {
        [[                                                                     ]],
        [[       ████ ██████           █████      ██                     ]],
        [[      ███████████             █████                             ]],
        [[      █████████ ███████████████████ ███   ███████████   ]],
        [[     █████████  ███    █████████████ █████ ██████████████   ]],
        [[    █████████ ██████████ █████████ █████ █████ ████ █████   ]],
        [[  ███████████ ███    ███ █████████ █████ █████ ████ █████  ]],
        [[ ██████  █████████████████████ ████ █████ █████ ████ ██████ ]],
      }

      -- Sub-header info (empty placeholder to be filled after plugin loading)
      local info = { type = "padding", val = 2, opts = { position = "center", hl = "Number" } }
      vim.api.nvim_create_autocmd("User", {
        pattern = "LazyVimStarted",
        once = true,
        group = "vimrc",
        callback = function()
          local stats = require("lazy").stats()
          local ms = (math.floor(stats.startuptime * 100 + 0.5) / 100)
          local plugins_line = "⚡ Lazy-loaded " .. stats.loaded .. "/" .. stats.count .. " plugins in " .. ms .. "ms"

          local v = vim.version()
          local version_line = "  v" .. v.major .. "." .. v.minor .. "." .. v.patch
          local width = vim.fn.strdisplaywidth
          local padding = string.rep(" ", (width(plugins_line) - width(version_line)) / 2)

          info.type = "text"
          info.val = { plugins_line, padding .. version_line }
          require("alpha").redraw()
        end,
      })

      -- Recent files
      local mru = require("alpha.themes.theta").config.layout[4]

      -- Center buttons
      local create_button = require("alpha.themes.dashboard").button
      local buttons = dashboard.section.buttons
      buttons.val = {
        { type = "text", val = "Quick links", opts = { hl = "SpecialComment", position = "center" } },
        { type = "padding", val = 1 },
        -- stylua: ignore start
        create_button("n", "  New file",             "<Cmd>ene<Bar>startinsert<CR>"), -- UTF f15b
        create_button("f", "  Find file",            "<Cmd>Telescope find_files<CR>"), -- UTF f002
        create_button("b", "  Browse files",         "<Cmd>Telescope file_browser<CR>"), -- UTF eb86
        create_button("r", "  Recent files",         "<Cmd>Telescope oldfiles<CR>"), -- UTF f0c5
        create_button("g", "  Grep text",            "<Cmd>Telescope live_grep<CR>"), -- UTF eb69
        create_button("l", "  Last session",         "<Cmd>PossessionLoad<CR>"), -- UTF f021
        create_button("s", "  Restore sessions",     "<Cmd>Telescope possession list<CR>"), -- UTF eb85
        create_button("v", "  Neovim configuration", "<Cmd>edit $MYVIMRC<CR>"), -- UTF e615
        create_button("p", "  Setup plugins",        "<Cmd>Lazy<CR>"), -- UTF f1e6
        create_button("t", "  Setup tools",          "<Cmd>Mason<CR>"), -- UTF e20f
        create_button("q", "  Quit",                 "<Cmd>quitall<CR>"), -- UFT f426
        -- stylua: ignore end
      }
      buttons.opts.spacing = 0

      -- Footer
      -- TODO: get vim tips from https://vtip.43z.one
      local fortune = require("fortune").get_fortune
      local footer = dashboard.section.footer
      footer.val = {}
      for _, line in ipairs(fortune()) do
        ---@diagnostic disable-next-line: param-type-mismatch
        table.insert(footer.val, line)
      end

      local layout = {
        { type = "padding", val = 3 },
        header,
        { type = "padding", val = 2 },
        info,
        { type = "padding", val = 3 },
        mru,
        { type = "padding", val = 3 },
        buttons,
        { type = "padding", val = 2 },
        footer,
      }
      dashboard.config.layout = layout

      require("alpha").setup(dashboard.config)
    end,
  },

  -- UI hook for vim.ui.select and vim.ui.input
  {
    "stevearc/dressing.nvim",
    lazy = true,
    init = function()
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.select = function(...)
        require("lazy").load({ plugins = { "dressing.nvim" } })
        return vim.ui.select(...)
      end
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.input = function(...)
        require("lazy").load({ plugins = { "dressing.nvim" } })
        return vim.ui.input(...)
      end
    end,
    opts = {
      input = {
        relative = "editor",
      },
    },
  },

  -- List for showing diagnostics, quickfix and location items, document symbols, search results
  -- and LSP definitions, references, implementations, type definitions and declarations.
  {
    "folke/trouble.nvim",
    cmd = { "Trouble" },
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("trouble").setup({
        mode = "document_diagnostics",
        height = 15,
        win = {
          wo = {
            colorcolumn = "",
          },
        },
      })
    end,
  },

  -- Display popup with key bindings.
  {
    "folke/which-key.nvim",
    dependencies = { "echasnovski/mini.icons" },
    event = "VeryLazy",
    init = function()
      vim.opt.timeout = true
      vim.opt.timeoutlen = 300
    end,
    config = function()
      local wk = require("which-key")
      wk.setup({
        plugins = {
          presets = {
            operators = false,
            motions = false,
            text_objects = false,
            windows = true,
            g = true,
            z = true,
          },
        },
      })
      wk.add(LEADER_MAPPINGS)
    end,
  },

  -- Statusline plugin.
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    event = "VimEnter",
    init = function()
      vim.g.lualine_laststatus = vim.o.laststatus -- Save 'laststatus' value
      if vim.fn.argc(-1) > 0 then
        -- Set empty lines until lualine loads
        vim.o.statusline = " "
        vim.o.winbar = " "
      else
        -- Hide the statusline on the starter page
        vim.o.laststatus = 0
      end
      -- Show tabline to avoid flickering when lualine loads
      vim.opt.showtabline = 2
    end,
    config = function()
      vim.o.laststatus = vim.g.lualine_laststatus -- Restore 'laststatus' value
      local terminal = {
        sections = {
          lualine_a = { "winnr", "mode" },
          lualine_b = { { "filetype", colored = false } },
          lualine_c = { "filename" },
          lualine_x = { { "filetype", icon_only = true, colored = false } },
          lualine_z = { "progress", "location" },
        },
        inactive_sections = {
          lualine_a = { "winnr" },
          lualine_c = { { "filetype", colored = false } },
          lualine_x = { "location" },
        },
        filetypes = { "terminal" },
      }
      require("lualine").setup({
        options = {
          disabled_filetypes = { "alpha" },
          globalstatus = false,
        },
        sections = {
          lualine_a = { "winnr", "mode" },
          lualine_b = { "branch", "diff", "diagnostics" },
          lualine_c = { "filename", "searchcount" },
          lualine_x = { { "filetype", colored = false, icon_only = true } },
          lualine_y = { "encoding", "fileformat" },
          lualine_z = { "progress", "location" },
        },
        inactive_sections = {
          lualine_a = { "winnr" },
        },
        tabline = {
          lualine_a = { { "buffers", mode = 0 } },
          lualine_z = { { "tabs", mode = 2 } },
        },
        winbar = {
          lualine_c = { { "aerial", sep = "  ", depth = 5 } },
          lualine_x = { "filename" },
          lualine_z = { "winnr" },
        },
        inactive_winbar = {
          lualine_c = {},
          lualine_x = { "filename" },
          lualine_z = { "winnr" },
        },
        extensions = {
          "aerial",
          "fugitive",
          "lazy",
          "man",
          "mason",
          "nvim-dap-ui",
          "oil",
          "quickfix",
          "trouble",
          terminal,
        },
      })
    end,
  },

  -- Indent guides.
  {
    "lukas-reineke/indent-blankline.nvim",
    event = "BufReadPre",
    main = "ibl",
    config = function()
      require("ibl").setup({
        scope = { enabled = true },
        indent = {
          char = "┆",
        },
      })
      -- Refresh indent lines after fold operations.
      local fold_keymaps = { "zo", "zO", "zc", "zC", "za", "zA", "zv", "zx", "zX", "zm", "zM", "zr", "zR" }
      for _, keymap in pairs(fold_keymaps) do
        vim.api.nvim_set_keymap(
          "n",
          keymap,
          keymap .. "<CMD>lua require('ibl').refresh()<CR>",
          { noremap = true, silent = true }
        )
      end
    end,
  },

  -- Highlight a unique character in every word when using f/F and t/T.
  {
    "jinh0/eyeliner.nvim",
    event = "BufReadPre",
    config = function()
      require("eyeliner").setup({
        highlight_on_key = true,
        dim = true,
      })
    end,
  },

  -- Whitespace highlighting and removal.
  {
    "echasnovski/mini.trailspace",
    event = "BufReadPre",
    config = function()
      require("mini.trailspace").setup()
    end,
  },

  -- Highlight TODO comments.
  {
    "folke/todo-comments.nvim",
    event = "BufReadPre",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("todo-comments").setup()
    end,
  },

  -- Color highlighter.
  {
    "norcalli/nvim-colorizer.lua",
    cmd = { "ColorizerToggle" },
    config = true,
  },

  -- Treesitter supported colorschemes:
  -- - https://github.com/nvim-treesitter/nvim-treesitter/wiki/Colorschemes
  -- - https://github.com/rockerBOO/awesome-neovim#tree-sitter-supported-colorscheme

  -- Tokyo Night theme.
  {
    "folke/tokyonight.nvim",
    lazy = true,
    priority = 1000,
    config = function()
      require("tokyonight").setup({
        sidebars = { "qf" },
        dim_inactive = true,
        lualine_bold = true,
      })
    end,
  },

  -- Catppuccin theme.
  {
    "catppuccin/nvim",
    name = "catppuccin",
    lazy = true,
    priority = 1000,
    config = function()
      require("catppuccin").setup({
        dim_inactive = { enabled = true },
      })
    end,
  },

  -- Kanagawa theme.
  {
    "rebelot/kanagawa.nvim",
    lazy = true,
    priority = 1000,
    config = function()
      require("kanagawa").setup({
        dimInactive = true,
      })
    end,
  },
}

require("lazy").setup(PLUGINS, {
  concurrency = vim.uv.available_parallelism() * 2,
  install = {
    colorscheme = { "default" },
  },
  checker = {
    enabled = true,
    frequency = 5 * 24 * 60 * 60, -- Check every 5 days
  },
})

-- See https://dotfyle.com/neovim/colorscheme/top for top colorschemes.
vim.cmd.colorscheme("tokyonight-night")
-- vim.cmd.colorscheme("catppuccin-mocha")
-- vim.cmd.colorscheme("kanagawa-wave")

-- vim: tabstop=4
