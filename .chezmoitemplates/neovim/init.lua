-------------------
----- General -----
-------------------

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

-- Set behavior for mouse and selection.
-- Options changed:
--   selectmode = ""
--   mousemodel = "extend"
--   keymodel   = ""
--   selection  = "inclusive"
vim.cmd.behave("xterm")

-- Enable 24-bit RGB colors in terminal mode.
vim.opt.termguicolors = true

-- Enable the window title.
vim.opt.title = true

-- Disable showing of partial commands on the last line.
vim.opt.showcmd = false

-- Disable mode indication on last line.
vim.opt.showmode = false

-- Set blinking cursor in normal mode.
if vim.g.os == "Windows" then
  vim.opt.guicursor = "n-v-c-sm:block-blinkwait500-blinkon200-blinkoff150,i-ci-ve:ver25,r-cr-o:hor20"
end

-- Raise dialog when quitting changed buffer.
vim.opt.confirm = true

-- Enable mouse support in all modes.
vim.opt.mouse = "a"

-- Use * and/or + clipboard registers for yank and put operations.
-- Primary selection: "* register / unnamed
-- System clipboard:  "+ register / unnamedplus
vim.opt.clipboard = "unnamed,unnamedplus"

-- Do not redraw screen while executing macros.
vim.opt.lazyredraw = true

-- Break lines at better places when wrapping lines.
vim.opt.linebreak = true

-- Keep lines above or below the cursor when scrolling.
vim.opt.scrolloff = 2

-- Keep columns to the left or to the right of the cursor.
vim.opt.sidescrolloff = 5

-- Highlight line and column under cursor. It helps with navigation.
vim.opt.cursorline = true
vim.opt.cursorcolumn = true

-- Highlight column 90. It helps identifying long lines.
vim.opt.colorcolumn = "100"

-- Show the line number relative to the cursor in front of each line and the
-- absolute line number for the one with the cursor.
vim.opt.number = true
vim.opt.relativenumber = true

-- Display signs in the 'number' column.
vim.opt.signcolumn = "number"

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

-- Always open diff windows vertically
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

------------------------
----- Autocommands -----
------------------------

--- General autocommands ---

-- Autobalance windows in each tab on Neovim resize.
vim.cmd([[autocmd vimrc VimResized * tabdo wincmd =]])

-- Use q to close some support windows.
vim.cmd([[autocmd vimrc FileType git,help,juliadoc,qf nnoremap <silent> <buffer> q :close<CR>]])

-- Send help windows to the right.
vim.cmd([[autocmd vimrc FileType help,juliadoc setlocal bufhidden=unload | wincmd L]])

-- Enable numbering in help buffers.
vim.cmd([[autocmd vimrc FileType help,juliadoc setlocal number relativenumber]])

-- Set name for first tab.
vim.cmd([[autocmd vimrc VimEnter * let t:tabname = 'main']])

--- Terminal autocmds ---

-- Disable numbering in terminal buffers.
vim.cmd([[autocmd vimrc TermOpen * setlocal nonumber norelativenumber]])

-- Move terminal windows to the right.
vim.cmd([[autocmd vimrc TermOpen * wincmd L]])

-- Set terminal filetype.
vim.cmd([[autocmd vimrc TermOpen * set filetype=terminal]])

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

-- Center screen when browsing through search results.
-- FIXME: these remaps break shortmess-S, since the screen is cleared and redrawn after zz
vim.cmd([[
  nnoremap n nzzzv
  nnoremap N Nzzzv
  set shortmess+=S
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

local LEADER_MAPPINGS = {
  ["<Leader>"] = { "<Cmd>Telescope buffers theme=ivy<CR>", "Find buffer" },
  ["`"] = { "<C-^>", "Alternate file" },

  w = {
    name = "window",
    ["p"] = { "<C-w>p", "Go to alternate window" },
    ["w"] = { "<C-w>w", "Go to next window" },
    ["W"] = { "<C-w>W", "Go to previous window" },
    ["h"] = { "<C-w>h", "Go left" },
    ["l"] = { "<C-w>l", "Go right" },
    ["j"] = { "<C-w>j", "Go down" },
    ["k"] = { "<C-w>k", "Go up" },
    ["t"] = { "<C-w>t", "Go to top-left window" },
    ["b"] = { "<C-w>b", "Go to bottom-right window" },
    ["H"] = { "<C-w>H", "Move far left" },
    ["L"] = { "<C-w>L", "Move far right" },
    ["J"] = { "<C-w>J", "Move to bottom" },
    ["K"] = { "<C-w>K", "Move to top" },
    ["c"] = { "<C-w>c", "Close window" },
    ["q"] = { "<C-w>q", "Quit window" },
    ["u"] = { "<C-w>u", "Undo quit window" },
    ["U"] = { "<C-w>U", "Undo quit windows in tab" },
    ["n"] = { "<C-w>n", "New window" },
    ["o"] = { "<C-w>o", "Only window" },
    ["s"] = { "<C-w>s", "Split horizontally" },
    ["v"] = { "<C-w>v", "Split vertically" },
    ["r"] = { "<C-w>r", "Rotate downwards" },
    ["R"] = { "<C-w>R", "Rotate upwards" },
    ["T"] = { "<C-w>T", "Move to new tab" },
    ["="] = { "<C-w>=", "Balance windows" },
    ["+"] = { "<C-w>5+", "Increase height" },
    ["-"] = { "<C-w>5-", "Decrease height" },
    [">"] = { "<C-w>10>", "Increase width" },
    ["<lt>"] = { "<C-w>10<", "Decrease width" },
    ["1"] = { "<Cmd>1wincmd w<CR>", "Go to window 1" },
    ["2"] = { "<Cmd>2wincmd w<CR>", "Go to window 2" },
    ["3"] = { "<Cmd>3wincmd w<CR>", "Go to window 3" },
    ["4"] = { "<Cmd>4wincmd w<CR>", "Go to window 4" },
    ["5"] = { "<Cmd>5wincmd w<CR>", "Go to window 5" },
  },

  b = {
    name = "buffer",
    ["b"] = { "<Cmd>Telescope buffers theme=ivy<CR>", "Find buffer" },
    ["w"] = { "<Cmd>write<CR>", "Write buffer" },
    ["s"] = { "<Cmd>write<CR>", "Save buffer" },
    ["n"] = { "<Cmd>bnext<CR>", "Next buffer" },
    ["p"] = { "<Cmd>bprevious<CR>", "Previous buffer" },
    ["#"] = { "<Cmd>buffer #<CR>", "Alternate buffer" },
    ["d"] = { "<Cmd>lua MiniBufremove.delete()<CR>", "Delete buffer" },
    ["e"] = { "<Cmd>ene<CR>", "Edit new buffer" },
    ["W"] = { "<Cmd>wall<CR>", "Write all buffers" },
    ["r"] = { "<Cmd>edit %<CR>", "Reload current buffer" },
    ["R"] = { "<Cmd>checktime<CR>", "Reload all buffers" },
  },

  ["<Tab>"] = {
    name = "tab",
    ["<Tab>"] = { new_or_next_tab, "New or next tab" },
    ["e"] = { "<Cmd>tabedit<CR>", "New tab" },
    ["c"] = { "<Cmd>tabclose<CR>", "Close tab" },
    ["o"] = { "<Cmd>tabonly<CR>", "Only tab" },
    ["n"] = { "<Cmd>tabnext<CR>", "Next tab" },
    ["p"] = { "<Cmd>tabprevious<CR>", "Previous tab" },
    ["w"] = { "<C-w>T", "Move window to new tab" },
    ["1"] = { "1gt", "Go to tab 1" },
    ["2"] = { "2gt", "Go to tab 2" },
    ["3"] = { "3gt", "Go to tab 3" },
    ["4"] = { "4gt", "Go to tab 4" },
    ["5"] = { "5gt", "Go to tab 5" },
    ["r"] = {
      function()
        rr("Tab name: ", "LualineRenameTab {}")
      end,
      "Rename tab",
    },
  },

  f = {
    name = "file",
    ["f"] = { "<Cmd>Telescope find_files noignore=true<CR>", "Find project files" },
    ["F"] = { "<Cmd>Telescope file_browser<CR>", "File browser" },
    ["h"] = { "<Cmd>Telescope find_files cwd=~<CR>", "Find home folder files" },
    ["r"] = { "<Cmd>Telescope oldfiles<CR>", "Recent files" },
    ["g"] = { "<Cmd>Telescope git_files<CR>", "Find git files" },
    ["p"] = { "<Cmd>Telescope projects<CR>", "Find projects" },
    ["n"] = { "<Cmd>ene<CR>", "New file" },
    ["D"] = { "<Cmd>Delete<CR>", "Delete file" },
    ["R"] = {
      function()
        rr("New name: ", "Rename {}")
      end,
      "Rename file",
    },
  },

  v = {
    name = "neovim",
    ["v"] = { "<Cmd>edit $MYVIMRC<CR>", "Open vim config" },
    ["l"] = { "<Cmd>Lazy<CR>", "Open Lazy menu" },
    ["m"] = { "<Cmd>Mason<CR>", "Open Mason menu" },
    ["t"] = { "<Cmd>TSUpdate<CR>", "Treesitter update" },
    ["h"] = { "<Cmd>Alpha<CR>", "Open home buffer" },
    ["H"] = { "<Cmd>checkhealth<CR>", "Check health" },
    ["p"] = { "<Cmd>Luapad<CR>", "Open lua scratch pad" },
  },

  t = {
    name = "trouble",
    ["t"] = { "<Cmd>TroubleToggle<CR>", "Toggle Trouble" },
    ["T"] = { "<Cmd>TodoTrouble<CR>", "Toggle TODO Trouble" },
    ["w"] = { "<Cmd>TroubleToggle workspace_diagnostics<CR>", "Workspace diagnostics" },
    ["d"] = { "<Cmd>TroubleToggle document_diagnostics<CR>", "Document diagnostics" },
    ["q"] = { "<Cmd>TroubleToggle quickfix<CR>", "Quickfix items" },
    ["l"] = { "<Cmd>TroubleToggle loclist<CR>", "Loclist items" },
    ["r"] = { "<Cmd>TroubleToggle lsp_references<CR>", "LSP references" },
    ["s"] = { "<Cmd>TodoTelescope<CR>", "Search TODOs" },
  },

  r = {
    name = "repl",
    ["c"] = { "<Cmd>SlimeConfig<CR>", "Configure REPL" },
    ["s"] = { "<Cmd>vsplit<Bar>terminal<CR>", "Open system shell" },
    ["j"] = { "<Cmd>vsplit<Bar>terminal julia<CR>", "Open Julia REPL" },
    ["r"] = { "<Cmd>vsplit<Bar>terminal R<CR>", "Open R REPL" },
    ["p"] = { "<Cmd>vsplit<Bar>terminal python -m IPython --profile=vi<CR>", "Open Python REPL" },
  },

  c = {
    name = "code",
    ["c"] = { "<Cmd>Format<CR>", "Format buffer" },
    ["C"] = { format_buffer, "Format buffer (LSP)" },
    ["a"] = { "<Cmd>FormatEnable<CR>", "Enable autoformatting" },
    ["A"] = { "<Cmd>FormatDisable<CR>", "Disable autoformatting" },
    ["w"] = { "<Cmd>lua MiniTrailspace.trim()<CR>", "Strip whitespace" },
    ["s"] = { "<Cmd>Telescope spell_suggest<CR>", "Spell suggest" },
    ["o"] = { "<Cmd>AerialToggle<CR>", "Toggle code outline" },
  },

  d = {
    name = "diff",
    ["s"] = {
      function()
        rr("Other file: ", "diffsplit {}", "file")
      end,
      "Open split diff window",
    },
    ["t"] = { "<Cmd>diffthis<CR>", "Diff this window" },
    ["o"] = { "<Cmd>diffoff<CR>", "Switch off diff" },
    ["u"] = { "<Cmd>diffupdate<CR>", "Update diff" },
    ["g"] = { "<Cmd>diffget<CR>", "Get diff from other" },
    ["p"] = { "<Cmd>diffput<CR>", "Put diff to other" },
    ["G"] = { "<Cmd>'<,'>diffget<CR>", "Get selection from other" },
    ["P"] = { "<Cmd>'<,'>diffput<CR>", "Put selection to other" },
  },

  l = {
    name = "lsp",
    ["l"] = { "<Cmd>LspInfo<CR>", "LSP information" },
    ["L"] = { "<Cmd>LspLog<CR>", "LSP log" },
    ["d"] = { "<Cmd>Telescope diagnostics bufnr=0<CR>", "List diagnostics" },
    ["r"] = { "<Cmd>Telescope lsp_references<CR>", "References for word under cursor" },
    ["I"] = { "<Cmd>Telescope lsp_implementations<CR>", "List implementations" },
    ["t"] = { "<Cmd>Telescope lsp_type_definitions<CR>", "List type definitions" },
    ["s"] = { "<Cmd>Telescope lsp_document_symbols<CR>", "Document symbols" },
    ["S"] = { "<Cmd>Telescope lsp_workspace_symbols<CR>", "Workspace symbols" },
    ["D"] = { "<Cmd>Telescope lsp_dynamic_workspace_symbols<CR>", "Dynamically lists workspace symbols" },
    ["i"] = { "<Cmd>Telescope lsp_incoming_calls<CR>", "List incoming calls" },
    ["o"] = { "<Cmd>Telescope lsp_outgoing_calls<CR>", "List outgoing calls" },
    ["h"] = { "<Cmd>lua vim.lsp.buf.hover()<CR>", "Hover help" },
    ["H"] = { "<Cmd>lua vim.lsp.buf.signature_help()<CR>", "Signature help" },
    ["R"] = { "<Cmd>lua vim.lsp.buf.rename()<CR>", "Rename" },
    p = {
      name = "peek",
      ["f"] = { "Peek function definition" },
      ["c"] = { "Peek class definition" },
    },
  },

  D = {
    name = "debug",
    ["D"] = { "<Cmd>lua require('dap').continue()<CR>", "Start" },
    ["b"] = { "<Cmd>lua require('dap').step_back()<CR>", "Step Back" },
    ["c"] = { "<Cmd>lua require('dap').continue()<CR>", "Continue" },
    ["C"] = { "<Cmd>lua require('dap').set_breakpoint(vim.fn.input '[Condition] > ')<CR>", "Conditional Breakpoint" },
    ["d"] = { "<Cmd>lua require('dap').disconnect()<CR>", "Disconnect" },
    ["e"] = { "<Cmd>lua require('dapui').eval()<CR>", "Evaluate" },
    ["E"] = { "<Cmd>lua require('dapui').eval(vim.fn.input '[Expression] > ')<CR>", "Evaluate Input" },
    ["g"] = { "<Cmd>lua require('dap').session()<CR>", "Get Session" },
    ["h"] = { "<Cmd>lua require('dap.ui.widgets').hover()<CR>", "Hover Variables" },
    ["i"] = { "<Cmd>lua require('dap').step_into()<CR>", "Step Into" },
    ["o"] = { "<Cmd>lua require('dap').step_over()<CR>", "Step Over" },
    ["p"] = { "<Cmd>lua require('dap').pause.toggle()<CR>", "Pause" },
    ["q"] = { "<Cmd>lua require('dap').close()<CR>", "Quit" },
    ["r"] = { "<Cmd>lua require('dap').repl.toggle()<CR>", "Toggle Repl" },
    ["R"] = { "<Cmd>lua require('dap').run_to_cursor()<CR>", "Run to Cursor" },
    ["S"] = { "<Cmd>lua require('dap.ui.widgets').scopes()<CR>", "Scopes" },
    ["t"] = { "<Cmd>lua require('dap').toggle_breakpoint()<CR>", "Toggle Breakpoint" },
    ["u"] = { "<Cmd>lua require('dap').step_out()<CR>", "Step Out" },
    ["U"] = { "<Cmd>lua require('dapui').toggle()<CR>", "Toggle UI" },
    ["x"] = { "<Cmd>lua require('dap').terminate()<CR>", "Terminate" },
    s = {
      name = "Search",
      ["b"] = { "<Cmd>Telescope dap list_breakpoints<CR>", "Breakpoints" },
      ["c"] = { "<Cmd>Telescope dap commands<CR>", "Commands" },
      ["C"] = { "<Cmd>Telescope dap configurations<CR>", "Configurations" },
      ["f"] = { "<Cmd>Telescope dap frames<CR>", "Frames" },
      ["v"] = { "<Cmd>Telescope dap variables<CR>", "Variables" },
    },
  },

  g = {
    name = "git",
    ["g"] = { "<Cmd>tab Git<CR>", "Git status" },
    ["c"] = { "<Cmd>Git commit<CR>", "Git commit" },
    ["p"] = { "<Cmd>Git push<CR>", "Git push" },
    ["l"] = { "<Cmd>Git log<Bar>wincmd L<CR>", "Git log" },
    ["b"] = { "<Cmd>Git blame<CR>", "Git blame" },
    ["C"] = { "<Cmd>Telescope git_commits<CR>", "List git commits" },
    ["B"] = { "<Cmd>Telescope git_branches<CR>", "List git branches" },
    ["S"] = { "<Cmd>Telescope git_status<CR>", "List changes per files" },
    ["d"] = { "<Cmd>DiffviewOpen<CR>", "Diff view" },
    ["f"] = { "<Cmd>DiffviewFileHistory<CR>", "Diff view file history" },
    h = {
      name = "hunk",
      ["h"] = { "Preview hunk" },
      ["s"] = { "Stage hunk" },
      ["r"] = { "Reset hunk" },
      ["S"] = { "Stage buffer" },
      ["u"] = { "Undo stage hank" },
      ["R"] = { "Reset buffer" },
      ["p"] = { "Preview hunk" },
      ["b"] = { "Blame line" },
      ["B"] = { "Toggle line blame" },
      ["d"] = { "Diff this" },
      ["D"] = { "Diff this ~" },
      ["t"] = { "Toggle deleted" },
      ["T"] = { "Open Trouble" },
    },
  },

  s = {
    name = "search",
    ["s"] = { "<Cmd>Telescope live_grep<CR>", "Live grep search" },
    ["w"] = { "<Cmd>Telescope grep_string<CR>", "Word under cursor" },
    ["b"] = { "<Cmd>Telescope current_buffer_fuzzy_find<CR>", "Current buffer" },
    ["c"] = { "<Cmd>Telescope commands<CR>", "Available commands" },
    [":"] = { "<Cmd>Telescope command_history<CR>", "Command history" },
    ["/"] = { "<Cmd>Telescope search_history<CR>", "Search history" },
    ["h"] = { "<Cmd>Telescope help_tags<CR>", "Help tags" },
    ["m"] = { "<Cmd>Telescope marks<CR>", "Marks" },
    ["r"] = { "<Cmd>Telescope registers<CR>", "Registers" },
    ["q"] = { "<Cmd>Telescope quickfix<CR>", "Quickfix list" },
    ["l"] = { "<Cmd>Telescope loclist<CR>", "Location list" },
    ["j"] = { "<Cmd>Telescope jumplist<CR>", "Jump list" },
    ["o"] = { "<Cmd>Telescope vim_options<CR>", "Vim options" },
    ["a"] = { "<Cmd>Telescope autocommands<CR>", "Autocommands" },
    ["k"] = { "<Cmd>Telescope keymaps<CR>", "Key maps" },
    ["f"] = { "<Cmd>Telescope filetype<CR>", "Filetypes" },
    ["H"] = { "<Cmd>Telescope highlights<CR>", "Highlights" },
    ["p"] = { "<Cmd>Telescope builtin<CR>", "Telescope pickers" },
  },

  m = {
    name = "misc",
    ["c"] = { "<Cmd>ColorizerToggle<CR>", "Toggle color strings highlighting" },
  },

  h = {
    name = "harpoon",
    ["h"] = { "<Cmd>lua require('harpoon').ui:toggle_quick_menu(require('harpoon'):list())<CR>", "Toggle quick menu" },
    ["H"] = { "<Cmd>Telescope harpoon marks<CR>", "Search marks" },
    ["a"] = { "<Cmd>lua require('harpoon'):list():append()<CR>", "Add file" },
    ["1"] = { "<Cmd>lua require('harpoon'):list():select(1)<CR>", "Go to selection 1" },
    ["2"] = { "<Cmd>lua require('harpoon'):list():select(2)<CR>", "Go to selection 2" },
    ["3"] = { "<Cmd>lua require('harpoon'):list():select(3)<CR>", "Go to selection 3" },
    ["4"] = { "<Cmd>lua require('harpoon'):list():select(4)<CR>", "Go to selection 4" },
    ["5"] = { "<Cmd>lua require('harpoon'):list():select(5)<CR>", "Go to selection 5" },
    ["n"] = { "<Cmd>lua require('harpoon'):list():next()<CR>", "Go to next mark" },
    ["p"] = { "<Cmd>lua require('harpoon'):list():prev()<CR>", "Go to previous mark" },
  },

  q = {
    name = "quit",
    ["q"] = { "<Cmd>quitall<CR>", "Quit all windows" },
  },
}

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

  -- Delete buffers without losing window layout.
  {
    "echasnovski/mini.bufremove",
    event = "VeryLazy",
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
        on_startup = { cursorline = true, cursorcolumn = true },
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
    "ahmedkhalf/project.nvim",
    event = "VeryLazy",
    config = function()
      require("project_nvim").setup({
        patterns = { "pyproject.toml", "Project.toml", "Makefile", ".git", "init.lua", "init.el" },
      })
    end,
  },

  -- Interactive real time strachpad for the embedded lua engine.
  {
    "rafcamlet/nvim-luapad",
    cmd = { "Luapad", "LuaRun" },
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

  -- Comment out lines (gcc) or comment out with motions (gc_) or selections (gc).
  -- Use gb_ for block comments.
  {
    "numToStr/Comment.nvim",
    keys = { "gc", "gb", { "gc", "gb", mode = "v" } },
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

  -- Useful [_, ]_ keybindings:
  -- - b: buffer
  -- - x: git conflict marker
  -- - d: diagnostic
  -- - f: file on disk
  -- - i: indent change
  -- - j: jump (in current buffer)
  -- - l: location list
  -- - q: quickfix list
  -- - t: treesitter node and parents
  -- - u: undo states
  -- - w: window (in current tab)
  -- - y: yank selection
  {
    "echasnovski/mini.bracketed",
    keys = { "[", "]" },
    config = function()
      require("mini.bracketed").setup({
        comment = { suffix = "" },
        oldfile = { suffix = "" },
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

  -- Text exchange operator: cx_, cxx (current line), X (in visual mode),
  -- cxc (clear pending exchanges).
  {
    "tommcdo/vim-exchange",
    keys = { "cx" },
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

  -- Emacs keybindings in insert and command modes:
  -- C-b, C-f: back/forward character
  -- M-b, M-f: back/forward word
  -- C-a, C-e: beginning/end of line
  -- M-n, M-p: down/up line
  -- C-d, M-d: delete character/word
  {
    "tpope/vim-rsi",
    event = "VeryLazy",
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
      require("various-textobjs").setup({ useDefaultKeymaps = false })

      -- Set custom mappings.
      vim.keymap.set({ "o", "x" }, "ii", "<Cmd>lua require('various-textobjs').indentation('inner', 'inner')<CR>")
      vim.keymap.set({ "o", "x" }, "ai", "<Cmd>lua require('various-textobjs').indentation('outer', 'inner')<CR>")
      vim.keymap.set({ "o", "x" }, "iI", "<Cmd>lua require('various-textobjs').indentation('inner', 'inner')<CR>")
      vim.keymap.set({ "o", "x" }, "aI", "<Cmd>lua require('various-textobjs').indentation('outer', 'outer')<CR>")
      vim.keymap.set({ "o", "x" }, "iS", "<Cmd>lua require('various-textobjs').subword('inner')<CR>")
      vim.keymap.set({ "o", "x" }, "aS", "<Cmd>lua require('various-textobjs').subword('outer')<CR>")
      vim.keymap.set({ "o", "x" }, "ie", "<Cmd>lua require('various-textobjs').entireBuffer()<CR>")
      vim.keymap.set({ "o", "x" }, "ae", "<Cmd>lua require('various-textobjs').entireBuffer()<CR>")
      vim.keymap.set({ "o", "x" }, "ic", "<Cmd>lua require('various-textobjs').multiCommentedLines()<CR>")
      vim.keymap.set({ "o", "x" }, "ac", "<Cmd>lua require('various-textobjs').multiCommentedLines()<CR>")
      vim.keymap.set({ "o", "x" }, "iv", "<Cmd>lua require('various-textobjs').value('inner')<CR>")
      vim.keymap.set({ "o", "x" }, "av", "<Cmd>lua require('various-textobjs').value('outer')<CR>")
      vim.keymap.set({ "o", "x" }, "ik", "<Cmd>lua require('various-textobjs').key('inner')<CR>")
      vim.keymap.set({ "o", "x" }, "ak", "<Cmd>lua require('various-textobjs').key('outer')<CR>")
      vim.keymap.set({ "o", "x" }, "iu", "<Cmd>lua require('various-textobjs').url()<CR>")
      vim.keymap.set({ "o", "x" }, "au", "<Cmd>lua require('various-textobjs').url()<CR>")
      vim.keymap.set({ "o", "x" }, "in", "<Cmd>lua require('various-textobjs').number('inner')<CR>")
      vim.keymap.set({ "o", "x" }, "an", "<Cmd>lua require('various-textobjs').number('outer')<CR>")
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
          vim.b.enable_autoformat = true
        else
          vim.g.enable_autoformat = true
        end
      end, {
        desc = "Enable autoformat-on-save",
        bang = true,
      })
      vim.api.nvim_create_user_command("FormatDisable", function()
        vim.b.enable_autoformat = false
        vim.g.enable_autoformat = false
      end, { desc = "Disable autoformat-on-save" })
    end,

    config = function()
      require("conform").setup({
        formatters_by_ft = {
          fish = { "fish_indent" },
          lua = { "stylua" },
          python = { "isort", "black" },
          sh = { "shfmt" },
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
      -- neodev
      -- TODO: evaluate this better
      { "folke/neodev.nvim" },

      -- Mason
      { "williamboman/mason.nvim" },
      { "williamboman/mason-lspconfig.nvim" },
      { "WhoIsSethDaniel/mason-tool-installer.nvim" },

      -- Fidget
      { "j-hui/fidget.nvim" },
    },

    config = function()
      -- Setup neovim with signature help, docs and completion for the nvim Lua API.
      require("neodev").setup()

      -- Setup Mason.
      require("mason").setup()

      -- Language servers to enable.
      -- Use the lsp_servers table below to override the default configuration. Available keys are:
      -- cmd, filetypes, capabilities and settings.
      local lsp_servers = {
        -- Neovim specific setup for Lua.
        -- TODO: evaluate this better (lspconfig, kickstarter, lspzero)
        lua_ls = {
          settings = {
            Lua = {
              telemetry = { enable = false },
            },
          },
        },
        pyright = {},
        -- TODO: evaluate this better
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
      if vim.fn.executable("cargo") then
        lsp_servers["pylyzer"] = {}
      end

      -- Add complete capabilities provided by cmp and broadcast them to the servers.
      local capabilities = vim.lsp.protocol.make_client_capabilities()
      -- TODO: evaluate this (dependency between lsp and cmp)
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
        debounce_hours = 12,
      })

      -- Create autocmd that is run when a LSP attaches to a particular buffer.
      vim.api.nvim_create_autocmd("LspAttach", {
        group = "vimrc",
        desc = "Setup buffer for LSP",

        callback = function(event)
          -- Enable completion triggered by <C-x><C-o>
          -- TODO: evaluate this better
          vim.bo[event.buf].omnifunc = "v:lua.vim.lsp.omnifunc"

          -- Set LSP buffer local key maps
          local map = function(mode, keys, func, desc)
            vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = "LSP " .. desc })
          end
          -- stylua: ignore start
          -- TODO: check all these mapping for conflicts
          -- TODO: test all these mapping
          -- TODO: fix K on lua
          map("n", "gK",     vim.lsp.buf.hover,           "hover documentation")
          map("n", "gd",     vim.lsp.buf.definition,      "goto definition")
          map("n", "gD",     vim.lsp.buf.declaration,     "goto declaration")
          map("n", "g<C-d>", vim.lsp.buf.type_definition, "goto type definition")
          map("n", "gI",     vim.lsp.buf.implementation,  "goto implementation")
          map("n", "gr",     vim.lsp.buf.references,      "goto references")
          map("n", "gs",     vim.lsp.buf.signature_help,  "display signature help")
          map("n", "<F2>",   vim.lsp.buf.rename,          "rename")
          map("n", "<F3>",   vim.lsp.buf.format,          "format")
          map("x", "<F3>",   vim.lsp.buf.format,          "format")
          map("n", "<F4>",   vim.lsp.buf.code_action,     "code action")
          map("x", "<F4>",   vim.lsp.buf.code_action,     "code action")
          -- stylua: ignore end
        end,
      })

      -- Setup Fidget
      -- TODO: evaluate this better
      require("fidget").setup({})

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
      { "L3MON4D3/LuaSnip", build = "make install_jsregexp" },
      -- TODO: evaluate this better
      { "rafamadriz/friendly-snippets" },

      -- Autocompletion
      { "hrsh7th/cmp-buffer" },
      { "hrsh7th/cmp-nvim-lsp" },
      { "hrsh7th/cmp-nvim-lua" },
      { "hrsh7th/cmp-path" },
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
          { name = "path" },
          { name = "nvim_lsp", keyword_length = 2 },
          { name = "buffer", keyword_length = 3 },
          { name = "luasnip", keyword_length = 2 },
        },
      })
    end,
  },

  -- Configure DAP support.
  -- TODO: evaluate persistent-breakpoints.nvim, goto-breakpoints.nvim
  {
    "mfussenegger/nvim-dap",
    lazy = true, -- Loaded when require'd
    dependencies = {
      "rcarriga/nvim-dap-ui",
      "theHamsta/nvim-dap-virtual-text",
      "nvim-telescope/telescope-dap.nvim",

      -- Mason setup
      "williamboman/mason.nvim",
      "jay-babu/mason-nvim-dap.nvim",
    },
    config = function()
      local dap = require("dap")

      local dapui = require("dapui")
      dapui.setup()

      -- TODO: open debug session in new tab: https://github.com/rcarriga/nvim-dap-ui/issues/122
      dap.listeners.after.event_initialized["dapui_config"] = function()
        dapui.open()
      end
      dap.listeners.before.event_terminated["dapui_config"] = function()
        dapui.close()
      end
      dap.listeners.before.event_exited["dapui_config"] = function()
        dapui.close()
      end

      require("nvim-dap-virtual-text").setup({
        commented = true,
      })

      require("telescope").load_extension("dap")

      require("mason-nvim-dap").setup({
        ensure_installed = { "python" },
        automatic_installation = false,
        handlers = {
          function(config)
            require("mason-nvim-dap").default_setup(config)
          end,
        },
      })
    end,
  },

  -- Syntax highlighting, indentation, folding and more using ASTs.
  {
    "nvim-treesitter/nvim-treesitter",
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
          keymaps = { init_selection = "<C-space>", node_incremental = "<C-space>" },
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
  -- TODO: evaluate nvim-neotest/neotest
  {
    "vim-test/vim-test",
    cmd = { "TestNearest", "TestFile", "TestSuite" },
  },

  --- Language plugins ---

  -- Reference: https://github.com/sheerun/vim-polyglot#language-packs

  -- Julia. LaTeX to Unicode substitutions.
  {
    "JuliaEditorSupport/julia-vim",
    event = "VeryLazy", -- Cannot be lazy loaded otherwise!
  },

  --- Terminal and file management support ---

  -- Send code to REPL: send motion in normal mode (gy_) or visual mode (gy),
  -- send line (gyy) and send paragraph (gyY).
  {
    "jpalardy/vim-slime",
    event = "VeryLazy", -- Cannot be lazy loaded otherwise!
    init = function()
      vim.g.slime_no_mappings = 1
      vim.g.slime_paste_file = os.tmpname()
      vim.g.slime_target = "neovim"

      -- Slime overrides: https://github.com/jpalardy/vim-slime#advanced-configuration-overrides

      -- IPython REPL
      if vim.g.os ~= "Windows" then
        vim.g.slime_python_ipython = 1
      else
        -- TODO: improve this (check vim-slime issues #123/223/273/283/293)
        vim.cmd([[
          function! SlimeOverride_EscapeText_python(text)
            " Using %paste (as done in neoterm), since %cpaste does not seem to work
            " in neovim's terminal on Windows.
            call setreg('+', a:text, 'l')
            return ['%paste', slime#config#resolve("dispatch_ipython_pause"), "\n"]
          endfunction
        ]])
      end

      -- Use bracketed paste in Julia REPL.
      -- Reference: https://cirw.in/blog/bracketed-paste
      vim.cmd([[
        function! SlimeOverride_EscapeText_julia(text)
          return "\x1b[200~" . a:text . "\x1b[201~"
        endfunction
      ]])
    end,
    config = function()
      vim.api.nvim_set_keymap("x", "gy", "<Plug>SlimeRegionSend", {})
      vim.api.nvim_set_keymap("n", "gy", "<Plug>SlimeMotionSend", {})
      vim.api.nvim_set_keymap("n", "gyy", "<Plug>SlimeLineSend", {})
      vim.api.nvim_set_keymap("n", "gyY", "<Plug>SlimeParagraphSend", {})

      -- Reset vim-slime configuration in all buffers.
      vim.cmd([[autocmd vimrc TermClose * bufdo if exists('b:slime_config') | unlet b:slime_config | endif]])
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
  {
    "nvim-telescope/telescope.nvim",
    cmd = { "Telescope" },
    dependencies = {
      { "nvim-lua/plenary.nvim" },
      { "nvim-telescope/telescope-file-browser.nvim" },
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
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
      })
      require("telescope").load_extension("projects")
      require("telescope").load_extension("file_browser")
      require("telescope").load_extension("fzf")
    end,
  },

  --- Windows, interface elements, visual editing helpers and themes ---

  -- Show start screen.
  {
    "goolord/alpha-nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      local theme = require("alpha.themes.theta")

      local header = {
        [[                                                  ]],
        [[      _______                    .__              ]],
        [[      \      \   ____  _______  _|__| _____       ]],
        [[      /   |   \_/ __ \/  _ \  \/ /  |/     \      ]],
        [[     /    |    \  ___(  <_> )   /|  |  Y Y  \     ]],
        [[     \____|__  /\___  \____/ \_/ |__|__|_|  /     ]],
        [[             \/     \/                    \/      ]],
        [[                                                  ]],
        [[                                                  ]],
      }
      for _, line in ipairs(require("alpha.fortune")()) do
        table.insert(header, line)
      end
      table.insert(header, "")
      theme.header.val = header
      theme.header.opts.hl = "Normal"

      local dashboard = require("alpha.themes.dashboard")
      local buttons = {
        { type = "text", val = "Quick links", opts = { hl = "SpecialComment", position = "center" } },
        { type = "padding", val = 1 },
        dashboard.button("e", "  New file", "<Cmd>ene<CR>"), -- UTF f15b
        dashboard.button("f", "󰈞  Find file", "<Cmd>Telescope find_files<CR>"), -- UTF f021e
        dashboard.button("b", "  Browse files", "<Cmd>Telescope file_browser<CR>"), -- UTF eb86
        dashboard.button("g", "  Live grep", "<Cmd>Telescope live_grep<CR>"), -- UTF f002
        dashboard.button("c", "  Configuration", "<Cmd>edit $MYVIMRC<CR>"), -- UTF e615
        dashboard.button("p", "  Update plugins", "<Cmd>Lazy update<CR>"), -- UTF f1e6
        dashboard.button("t", "  Update tools", "<Cmd>Mason<CR>"), -- UTF e20f
        dashboard.button("q", "  Quit", "<Cmd>qa<CR>"), -- UFT f00d
      }
      theme.buttons.val = buttons

      require("alpha").setup(theme.config)
    end,
  },

  -- List for showing diagnostics, references, search results, quickfix and
  -- location lists.
  {
    "folke/trouble.nvim",
    cmd = { "Trouble", "TroubleToggle" },
    dependencies = { "nvim-tree/nvim-web-devicons" },
    init = function()
      vim.cmd([[autocmd vimrc FileType Trouble setlocal colorcolumn=]])
    end,
    config = function()
      require("trouble").setup({
        mode = "document_diagnostics",
      })
    end,
  },

  -- Display popup with key bindings.
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
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
        key_labels = {
          -- Override label used to display some keys
          ["<space>"] = "SPC",
          ["<CR>"] = "RET",
          ["<Tab>"] = "TAB",
        },
      })
      wk.register(LEADER_MAPPINGS, { prefix = "<Leader>" })
    end,
  },

  -- Statusline plugin.
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      local terminal = {
        sections = {
          lualine_a = { "winnr", "mode" },
          lualine_b = { "vim.opt.filetype._value", "'jobid: ' .. vim.opt.channel._value" },
          lualine_c = { "filename" },
          lualine_x = { { "filetype", icon_only = true, colored = false } },
          lualine_z = { "progress", "location" },
        },
        inactive_sections = {
          lualine_a = { "winnr" },
          lualine_c = { "vim.opt.filetype._value", "'jobid: ' .. vim.opt.channel._value" },
          lualine_x = { "location" },
        },
        filetypes = { "terminal" },
      }
      require("lualine").setup({
        options = {
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
        extensions = { "fugitive", "quickfix", terminal },
      })
    end,
  },

  -- Indent guides.
  {
    "lukas-reineke/indent-blankline.nvim",
    event = "BufReadPre",
    main = "ibl",
    config = function()
      require("ibl").setup({})
      -- Refresh indent lines after fold operations.
      for _, keymap in pairs({
        "zo",
        "zO",
        "zc",
        "zC",
        "za",
        "zA",
        "zv",
        "zx",
        "zX",
        "zm",
        "zM",
        "zr",
        "zR",
      }) do
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

      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "lazy" },
        group = "vimrc",
        desc = "Disable trail spaces highlighting",
        callback = function()
          vim.b.minitrailspace_disable = true
          MiniTrailspace.unhighlight()
        end,
      })
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
    lazy = false,
    priority = 1000,
    config = function()
      -- Load default color scheme.
      vim.g.tokyonight_lualine_bold = 1
      vim.cmd.colorscheme("tokyonight-storm")
      vim.cmd.highlight("Folded guibg=NONE")
    end,
  },
}

require("lazy").setup(PLUGINS, {
  install = {
    colorscheme = { "tokyonight" },
  },
  checker = {
    enabled = true,
  },
})

-- vim: tabstop=4
