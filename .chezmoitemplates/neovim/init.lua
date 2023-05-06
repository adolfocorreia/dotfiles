-------------------
----- General -----
-------------------

-- Neovim tips:
-- - https://github.com/wincent/vim-university
-- - https://github.com/nanotee/nvim-lua-guide
-- - Use :lua print(vim.inspect(<table>)) to display table contents.

-- TODO: evaluate and replace all vim.cmd statements
-- TODO: convert code to v0.7 APIs (e.g. autocmds, key maps, highlight)

-- Define vimrc autocommand group removing all previously set vimrc autocommands
-- when (re)sourcing this file.
-- Reference: https://learnvimscriptthehardway.stevelosh.com/chapters/14.html
vim.cmd([[
  augroup vimrc
    autocmd!
  augroup END
]])

-- Select Leader keys.
vim.g.mapleader      = ' '
vim.g.maplocalleader = '\\'

-- Set vim.g.os variable with current OS.
if vim.fn.exists('g:os') == 0 then
  if vim.fn.has('win64') then
    vim.g.os = 'Windows'
  else
    vim.g.os = vim.fn.substitute(vim.fn.system('uname'), '\n', '', '')
  end
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

-- Enable select mode (with mouse or shift selection) and right click popup.
-- Options changed:
--   selectmode = "mouse,key"
--   mousemodel = "popup"
--   keymodel   = "startsel,stopsel"
--   selection  = "exclusive"
vim.cmd([[behave mswin]])

-- Enable 24-bit RGB colors in terminal mode.
vim.opt.termguicolors = true

-- Enable the window title.
vim.opt.title = true

-- Disable showing of partial commands on the last line.
vim.opt.showcmd = false

-- Disable mode indication on last line.
vim.opt.showmode = false

-- Set blinking cursor in normal mode.
if vim.g.os == 'Windows' then
  vim.opt.guicursor = 'n-v-c-sm:block-blinkwait500-blinkon200-blinkoff150,i-ci-ve:ver25,r-cr-o:hor20'
end

-- Raise dialog when quitting changed buffer.
vim.opt.confirm = true

-- Enable mouse support in all modes.
vim.opt.mouse = 'a'

-- Use * and/or + clipboard registers for yank and put operations.
-- Primary selection: "* register / unnamed
-- System clipboard:  "+ register / unnamedplus
vim.opt.clipboard = 'unnamed,unnamedplus'

-- Do not redraw screen while executing macros.
vim.opt.lazyredraw = true

-- Keep lines above or below the cursor when scrolling.
vim.opt.scrolloff = 2

-- Keep columns to the left or to the right of the cursor.
vim.opt.sidescrolloff = 5

-- Highlight line and column under cursor. It helps with navigation.
vim.opt.cursorline = true
vim.opt.cursorcolumn = true

-- Highlight column 90. It helps identifying long lines.
vim.opt.colorcolumn = '90'

-- Show the line number relative to the cursor in front of each line and the
-- absolute line number for the one with the cursor.
vim.opt.number = true
vim.opt.relativenumber = true

-- Open new split panes to right and bottom.
vim.opt.splitbelow = true
vim.opt.splitright = true

-- Ignore case in patterns (unless upper case characters are used).
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Disable line wrapping.
vim.opt.wrap = false

-- Always open diff windows vertically
vim.opt.diffopt:append { 'vertical' }

-- Complete till longest common string and list all matches.
vim.opt.wildmode = 'longest:full,full'

-- File patterns to ignore.
vim.opt.wildignore:append { '*.swp' }
vim.opt.wildignore:append { '*.zip,*.7z,*.tar,*.gz' }
vim.opt.wildignore:append { '*.pdf' }
vim.opt.wildignore:append { '*.doc,*.docx,*.xls,*.xslx,*.ppt,*.pptx' }
vim.opt.wildignore:append { '*.png,*.jpg,*.gif' }
vim.opt.wildignore:append { '*.pyc,*.pyo,*.pyd' }



------------------------
----- Autocommands -----
------------------------

--- General autocommands.

-- Highlight yanked region.
vim.cmd([[autocmd vimrc TextYankPost * silent! lua vim.highlight.on_yank{timeout=500}]])

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


--- Terminal autocmds.

-- Disable numbering in terminal buffers.
vim.cmd([[autocmd vimrc TermOpen * setlocal nonumber norelativenumber]])

-- Move terminal windows to the right.
vim.cmd([[autocmd vimrc TermOpen * wincmd L]])

-- Set terminal filetype.
vim.cmd([[autocmd vimrc TermOpen * set filetype=terminal]])


-- Avoid cursor movement when yanking text.
-- Save view on CursorMoved and restore after yank operation.
-- Reference: https://github.com/svban/YankAssassin.vim
-- TODO: convert these functions to Lua!
vim.cmd([[
  function! SaveViewOnCursorMove() abort
    let w:pre_yank_view = winsaveview()
  endfunction
  function! RestoreViewAfterYank() abort
    if v:event.operator=='y' && exists('w:pre_yank_view')
      call winrestview(w:pre_yank_view)
    endif
  endfunction
  augroup YankSteadyView
    autocmd!
    autocmd CursorMoved * call SaveViewOnCursorMove()
    autocmd TextYankPost * call RestoreViewAfterYank()
  augroup END
]])


-- Disable nvim-autopairs when entering in Visual Multi mode, since both plugins map
--the <BS> key.
-- References:
-- - *vm-functions*
-- - https://github.com/windwp/nvim-autopairs/blob/master/lua/nvim-autopairs.lua
-- - https://vi.stackexchange.com/questions/7734/how-to-save-and-restore-a-mapping
-- TODO: convert these functions to Lua!
vim.cmd([[
  let g:VM_bs_map = {}
  function! DisableAutopairsMappings() abort
    if mapcheck('<BS>', 'i') !=# ''
      let g:VM_bs_map = maparg('<BS>', 'i', 0 , 1)
      if g:VM_bs_map.buffer == 0 || g:VM_bs_map.expr == 0
        throw 'Unexpected mapping options!'
      endif
      lua require('nvim-autopairs').disable()
      iunmap <buffer> <expr> <BS>
    endif
  endfunction
  function! EnableAutopairsMappings() abort
    if g:VM_bs_map != {}
      execute 'inoremap <buffer> <expr> <BS> ' . g:VM_bs_map.rhs
      lua require('nvim-autopairs').enable()
      let g:VM_bs_map = {}
    endif
  endfunction
  autocmd vimrc User visual_multi_start call DisableAutopairsMappings()
  autocmd vimrc User visual_multi_exit  call EnableAutopairsMappings()
]])



------------------------
----- Key mappings -----
------------------------

-- - Used keys reference: :help index
-- - Unused keys reference:
--   - https://vim.fandom.com/wiki/Unused_keys
--   - https://skippi.medium.com/ideas-for-non-leader-vim-mappings-fd32a2769c87
-- - Prefer non recursive maps (_noremap)
-- - Plugin maps (<Plug>) must be recursive


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


-- Save buffer with C-s.
vim.cmd([[
  nnoremap <silent> <C-s> :update<CR>
  inoremap <silent> <C-s> <C-o>:update<CR>
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



--------------------------------
----- Plugin configuration -----
--------------------------------

-- Install lazy.nvim if not present.
local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)


-- Buffer and file types blacklists (disable visual effects).
BUFTYPE_BL = {
  'help',
  'nofile',
  'nowrite',
  'terminal',
}
FILETYPE_BL = {
  'alpha',
  'checkhealth',
  'dirbuf',
  'fugitive',
  'harpoon',
  'help',
  'lspinfo',
}


PLUGINS = {

  --- Neovim management and fixes ---

  -- Delete buffers without losing window layout.
  {
    'echasnovski/mini.bufremove',
    event = 'VeryLazy',
    config = function()
      require('mini.bufremove').setup({})
    end,
  },

  -- Reopen files at last edit position.
  {
    'ethanholz/nvim-lastplace',
    event = 'VeryLazy',
    config = function()
      require('nvim-lastplace').setup({
          lastplace_ignore_buftype = { 'quickfix', 'nofile', 'nowrite', 'terminal' },
          lastplace_ignore_filetype = { 'gitcommit', 'gitrebase' },
          lastplace_open_folds = true,
      })
    end,
  },

  -- Disable cursorline and cursorcolumn in inactive windows.
  {
    'tummetott/reticle.nvim',
    event = 'VeryLazy',
    config = function()
      local never_ft = {
        'alpha',
        'lazy',
        'mason',
        'TelescopePrompt',
        'terminal',
      }
      require('reticle').setup({
        disable_in_insert = false,
        always_highlight_number = true,
        never = {
          cursorline = never_ft,
          cursorcolumn = never_ft,
        },
      })
    end,
  },

  -- TODO: evaluate this better
  -- Project management.
  {
    'ahmedkhalf/project.nvim',
    event = 'VeryLazy',
    config = function()
      require('project_nvim').setup({
        detection_methods = { 'pattern', 'lsp' },
      })
    end,
  },

  -- Interactive real time strachpad for the embedded lua engine.
  {
    'rafcamlet/nvim-luapad',
    cmd = { 'Luapad', 'LuaRun' },
  },


  --- Useful keybindings ---

  -- Make repeat command (.) plugin compatible.
  {
    'tpope/vim-repeat',
    event = 'VeryLazy',
  },

  -- Jump to any forward (s__) or backward (S__) location specified by two characters.
  -- In case of multiple targets, a third character (label) can be used.
  {
    'ggandor/leap.nvim',
    keys = { 's', 'S' },
    dependencies = { 'unblevable/quick-scope' },
    config = function ()
      require('leap').add_default_mappings()
      vim.keymap.del({'x', 'o'}, 'x')
      vim.keymap.del({'x', 'o'}, 'X')
      vim.api.nvim_create_autocmd('User', {
        pattern = 'LeapEnter',
        command = 'QuickScopeToggle',
      })
      vim.api.nvim_create_autocmd('User', {
        pattern = 'LeapLeave',
        command = 'QuickScopeToggle',
      })
    end,
  },

  -- TODO: evaluate mini.surround
  -- Add (ys_), change (cs_), remove (ds_) surrounding delimiters (_ss for whole line).
  {
    'kylechui/nvim-surround',
    keys = { 'ys', 'cs', 'ds' },
    config = true,
  },

  -- TODO: evaluate mini.comment
  -- Comment out lines (gcc) or comment out with motions (gc_) or selections (gc).
  -- Use gb_ for block comments.
  {
    'numToStr/Comment.nvim',
    keys = { 'gc', 'gb', { 'gc', 'gb', mode='v' } },
    config = true,
  },

  -- TODO: evaluate mini.bracketed
  -- Useful [_, ]_ keybindings: b (change buffers), Space (add blank lines),
  -- e (exchange line), navigate quickfix (q/Q) and location (l/L) lists;
  -- Paste after (]p) or before ([p) linewise.
  -- Toggle common options: _oh (hlsearch), _oi (ignorecase), _ol (list tabs and
  -- trailing spaces), _on (number), _or (relativenumber), _ov (virtualedit),
  -- _ow (wrap), _ox (cursorline and cursorcolumn), _oz (spell).
  -- Also _od for :diffthis and :diffoff.
  {
    'tpope/vim-unimpaired',
    keys = { '[', ']' },
  },

  -- Text exchange operator: cx_, cxx (current line), X (in visual mode),
  -- cxc (clear pending exchanges).
  {
    'tommcdo/vim-exchange',
    keys = { 'cx' },
  },

  -- Coerce text cases with: crs (snake_case), crm (MixedCase), crc (camelCase),
  -- cru (UPPER_CASE), cr- (dash-case), cr. (dot.case), cr<space> (space case),
  -- crt (Title Case).
  -- Search and replace words with with variants like plural and case. Use e.g.
  -- :%S/facilit{y,ies}/building{,s}/gc
  {
    'tpope/vim-abolish',
    keys = { 'cr' },
    cmd = { 'Subvert', 'S', 'Abolish' },
  },

  -- Emacs keybindings in insert and command modes:
  -- C-b, C-f: back/forward character
  -- M-b, M-f: back/forward word
  -- C-a, C-e: beginning/end of line
  -- M-n, M-p: down/up line
  -- C-d, M-d: delete character/word
  {
    'tpope/vim-rsi',
    event = 'VeryLazy',
  },

  -- TODO: evaluate this and svermeulen/vim-subversive better
  -- TODO: https://vim.fandom.com/wiki/Replace_a_word_with_yanked_text
  -- Replace text object (<M-s>), line (<M-s><M-s>) or to end of line (<M-S>) with
  -- contents of the unnamed register "" (e.g. <M-s>iw replaces the word under
  -- the cursor with the current yank).
  {
    'gbprod/substitute.nvim',
    keys = { '<M-s>', '<M-S>' },
    config = function()
      require('substitute').setup({})

      local opts = { noremap = true }
      vim.api.nvim_set_keymap('n', '<M-s>',      "<Cmd>lua require('substitute').operator()<CR>", opts)
      vim.api.nvim_set_keymap('n', '<M-s><M-s>', "<Cmd>lua require('substitute').line()<CR>",     opts)
      vim.api.nvim_set_keymap('n', '<M-S>',      "<Cmd>lua require('substitute').eol()<CR>",      opts)
      vim.api.nvim_set_keymap('x', '<M-s>',      "<Cmd>lua require('substitute').visual()<CR>",   opts)
    end,
  },

  -- TODO: evaluate monaqa/dial.nvim
  -- Use C-a/C-x to increment/decrement dates, times, roman numerals and ordinals
  -- (e.g. 1st, 2nd, 3rd). For letters of the alphabet, use linewise visual
  -- selection on empty lines.
  {
    'tpope/vim-speeddating',
    keys = { '<C-a>', '<C-x>' },
    cmd = { 'SpeedDatingFormat' },
    config = function()
      -- Remove format for non-capital roman numerals.
      vim.cmd([[SpeedDatingFormat! %v]])
    end,
  },


  --- Editing helps ---

  -- Sublime Text-like multiple cursor editing.
  -- To activate, select words with M-d or create cursors vertically with C-j/C-k.
  -- Use C-h/C-l to add characters to the selection and o to go to the other side
  -- of the selection. Use n/N to get more occurrences and [/] to navigate
  -- between selections. Press q to skip current occurrence and get the next one
  -- and Q to remove current selection. Start insert mode with i, a or c.
  {
    'mg979/vim-visual-multi',
    keys = { '<M-d>', '<C-j>', '<C-k>', '<C-h>', '<C-l>' },
    init = function()
      -- Visual Multi plugin key mappings.
      vim.g.VM_maps = {
        ['Find Under']         = '<M-d>',
        ['Find Subword Under'] = '<M-d>',
      }
    end,
  },

  -- TODO: evaluate mini.align
  -- Align text by some character or regex adding spaces to the left and/or right.
  -- 1. Type ga in visual mode, or ga followed by motion or text object in normal
  --    mode to enter interactive mode.
  -- 2. Optionally enter keys to cycle between alignment options (e.g. <C-d> to
  --    cycle between left, right or center alignment).
  -- 3. Optionally enter keys to define delimiter occurrences to consider (e.g.
  --    2: second occurence, *: all occurences, -: last ocurrence).
  -- 4. Type delimiter key (one of " =:.|&#,", which have predefined rules) or an
  --    arbitrary regex followed by <C-x>.
  -- 5. Alternatively, use the :EasyAlign command.
  -- Reference: https://github.com/junegunn/vim-easy-align
  {
    'junegunn/vim-easy-align',
    keys = { 'ga', { 'ga', mode = 'v'} },
    cmd = { 'EasyAlign' },
    config = function()
      vim.api.nvim_set_keymap('n', 'ga', '<Plug>(EasyAlign)', {})
      vim.api.nvim_set_keymap('x', 'ga', '<Plug>(EasyAlign)', {})
    end,
  },

  -- TODO: evaluate 'LunarWatcher/auto-pairs' since it might be compatible with vim-visual-multi
  -- TODO: evaluate mini.pairs
  -- Insert and delete brackets, parenthesis and quotes in pairs.
  {
    'windwp/nvim-autopairs',
    keys = { { '(', '[', '{', '"', "'", '`', mode = 'i' } },
    config = function()
      require('nvim-autopairs').setup({
        map_bs = true,  -- conflicts with vim-visual-multi - check autocmd workaround
        map_c_h = true,
        map_c_w = false,
      })
    end,
  },

  -- TODO: find lua alternative
  -- Show how many times a search pattern occurs in current buffer.
  {
    'google/vim-searchindex',
    keys = { '/', '?', '#', '*', 'n', 'N' },
    config = function()
      local opts = {silent=true}
      vim.api.nvim_set_keymap('n', 'n', 'nzzzv<Plug>SearchIndex', opts)
      vim.api.nvim_set_keymap('n', 'N', 'Nzzzv<Plug>SearchIndex', opts)
    end,
  },

  -- Navigate between bookmarked files.
  {
    'ThePrimeagen/harpoon',
    lazy = true,  -- loaded when required
    dependencies = { 'nvim-lua/plenary.nvim', 'nvim-telescope/telescope.nvim' },
    config = function()
      require('telescope').load_extension('harpoon')
    end,
  },


  --- Custom motions and text objects ---

  -- Several text objects with inside (i) and around (a) semantics.
  -- and last (_l) semantics.
  -- Pairs: () {} [] <>
  -- XML/HTML tags: t
  -- Quotes: ' " `
  -- Separators: , . ; : + - = ~ _ * # / | & $
  -- Arguments: a (surrounded by braces and/or commas)
  -- Any Block: b (similar to pairs, but skips pairs in nested contexts)
  -- Any Quote: q (similar to quotes, but skips pairs in nested contexts)
  {
    'echasnovski/mini.ai',
    event = 'VeryLazy',
    config = function()
      require('mini.ai').setup({})
    end,
  },

  -- TODO: find lua alternative
  -- Indentation level text object: ii (indentation level), ai (ii and line
  -- above), aI (ii with lines above/below).
  {
    'michaeljsmith/vim-indent-object',
    event = 'BufReadPre',
  },

  -- TODO: find lua alternative
  -- Text object for the entire buffer (ae/ie).
  {
    'kana/vim-textobj-entire',
    event = 'BufReadPre',
    dependencies = { 'kana/vim-textobj-user' },
  },

  -- Text object (iv) for variable segments in camelCase or snake_case words.
  {
    'Julian/vim-textobj-variable-segment',
    event = 'BufReadPre',
    dependencies = { 'kana/vim-textobj-user' },
  },

  -- Language syntax text objects: functions (af/if).
  {
    'nvim-treesitter/nvim-treesitter-textobjects',
    event = 'BufReadPre',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
  },


  --- Language support ---

  -- TODO: consider using expandtab, tabstop, softtabstop, shiftwidth explicitly
  -- Automatic tab/indenting configuration.
  {
    'tpope/vim-sleuth',
    event = 'BufReadPre',
  },

  -- Auto setup nvim-lspconfig, nvim-cmp, mason.nvim and dependencies.
  {
    'VonHeikemen/lsp-zero.nvim',
    event = 'BufReadPre',
    cmd = { 'Mason' },
    dependencies = {
      {'nvim-lua/plenary.nvim'},

      -- LSP Support
      {'neovim/nvim-lspconfig'},
      {'williamboman/mason.nvim'},
      {'williamboman/mason-lspconfig.nvim'},
      {'jose-elias-alvarez/null-ls.nvim'},
      {'jay-babu/mason-null-ls.nvim'},

      -- LSP helpers
      {'simrat39/symbols-outline.nvim'},
      {'folke/neodev.nvim'},  -- neovim lua API support

      -- Autocompletion
      {'hrsh7th/cmp-buffer'},
      {'hrsh7th/cmp-nvim-lsp'},
      {'hrsh7th/cmp-nvim-lua'},
      {'hrsh7th/cmp-path'},
      {'hrsh7th/nvim-cmp'},
      {'lukas-reineke/cmp-rg'},
      {'saadparwaiz1/cmp_luasnip'},

      -- Snippets
      {'L3MON4D3/LuaSnip'},
      {'rafamadriz/friendly-snippets'},
    },

    config = function()
      require('neodev').setup({})

      -- Learn the keybindings, see :help lsp-zero-keybindings
      -- Learn to configure LSP servers, see :help lsp-zero-api-showcase
      local lsp_zero = require('lsp-zero')
      lsp_zero.preset({
        name = 'recommended',
        -- Reference: https://github.com/VonHeikemen/lsp-zero.nvim/blob/main/doc/md/lsp.md#default-keybindings
        set_lsp_keymaps = { preserve_mappings = false },
      })

      -- Adjust cmp sources
      lsp_zero.setup_nvim_cmp({
        sources = {
          { name = 'path' },
          { name = 'nvim_lsp', keyword_length = 2 },
          { name = 'buffer',   keyword_length = 3 },
          { name = 'luasnip',  keyword_length = 2 },
          { name = 'rg',       keyword_length = 4 },
        }
      })

      -- Adjust cmp mappings
      lsp_zero.setup_nvim_cmp({
        -- Use C-e to abort, C-y or Enter to confirm and C-n, C-p, Tab, S-Tab, Up or Down to choose
        -- Reference: https://github.com/hrsh7th/nvim-cmp/blob/main/lua/cmp/config/mapping.lua
        mapping = lsp_zero.defaults.cmp_mappings({})
      })

      -- Configure lua language server for neovim
      lsp_zero.nvim_workspace()

      -- Setup lsp-zero
      lsp_zero.setup()

      -- Setup LSP symbols outline
      require('symbols-outline').setup({})

      -- null-ls and mason-null-ls
      local null_ls = require('null-ls')
      local null_opts = lsp_zero.build_options('null-ls', {})
      null_ls.setup({
        on_attach = function(client, bufnr)
          null_opts.on_attach(client, bufnr)
        end,
        sources = {
          null_ls.builtins.diagnostics.pylint,
          null_ls.builtins.formatting.black,
          null_ls.builtins.formatting.isort,
        },
      })
      require('mason-null-ls').setup({
        ensure_installed = {
          'black',
          'isort',
          'pylint',
        },
        automatic_installation = false,
        automatic_setup = true,
      })
    end,
  },

  -- Syntax highlighting, indentation, folding and more using ASTs.
  {
    'nvim-treesitter/nvim-treesitter',
    event = 'BufReadPre',
    cmd = { 'TSUpdate' },
    build = function()
      require('nvim-treesitter.install').update({ with_sync = true })
    end,
    init = function()
       -- Use treesitter to manage folds.
       vim.opt.foldmethod = 'expr'
       vim.opt.foldexpr = 'nvim_treesitter#foldexpr()'
       vim.opt.foldlevelstart = 5
    end,
    config = function()
      require('nvim-treesitter.configs').setup({
        ensure_installed = {
          'bash',
          'comment',
          'fish',
          'haskell',
          'help',
          'json',
          'julia',
          'lua',
          'markdown',
          'org',
          'python',
          'r',
          'rst',
          'toml',
          'vim',
          'yaml',
        },
        -- https://github.com/nvim-treesitter/nvim-treesitter#i-want-to-use-a-http-proxy-for-downloading-the-parsers
        prefer_git = true,
        -- Modules
        highlight = { enable = true },
        indent = { enable = true },
        incremental_selection = { enable = true },
        rainbow = { enable = true },
        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = {
              ['aa'] = '@parameter.outer',
              ['ia'] = '@parameter.inner',
              ['ac'] = '@class.outer',
              ['ic'] = '@class.inner',
              ['af'] = '@function.outer',
              ['if'] = '@function.inner',
            },
          },
          move = {
            enable = true,
            set_jumps = true,
            goto_next_start = {
              [']]'] = '@function.outer',
              [']c'] = '@class.outer',
            },
            goto_next_end = {
              [']['] = '@function.outer',
              [']C'] = '@class.outer',
            },
            goto_previous_start = {
              ['[['] = '@function.outer',
              ['[c'] = '@class.outer',
            },
            goto_previous_end = {
              ['[]'] = '@function.outer',
              ['[C'] = '@class.outer',
            },
          },
          lsp_interop = {
            enable = true,
            border = 'none',
            peek_definition_code = {
              ['<Leader>lpf'] = '@function.outer',
              ['<Leader>lpc'] = '@class.outer',
            },
          },
        },
      })
    end,
  },

  -- Run test suites (e.g. pytest).
  -- TODO: evaluate nvim-neotest/neotest
  {
    'vim-test/vim-test',
    cmd = { 'TestNearest', 'TestFile', 'TestSuite' },
  },

  -- AI powered code suggestions.
  {
    'Exafunction/codeium.vim',
    cmd = 'Codeium',
    init = function ()
      vim.g.codeium_disable_bindings = 1
    end,
    config = function()
      vim.keymap.set('i', '<M-;>', function() return vim.fn['codeium#Accept']() end, { expr = true, desc = 'Accept completion' })
      vim.keymap.set('i', '<M-]>', function() return vim.fn['codeium#CycleCompletions']( 1) end, { expr = true, desc = 'Next completion' })
      vim.keymap.set('i', '<M-[>', function() return vim.fn['codeium#CycleCompletions'](-1) end, { expr = true, desc = 'Previous completion' })
    end,
  },


  --- Language plugins ---

  -- Reference: https://github.com/sheerun/vim-polyglot#language-packs

  -- TODO: Evaluate zeavim.vim, vim-dasht and vim-devdocs

  -- Julia. LaTeX to Unicode substitutions.
  {
    'JuliaEditorSupport/julia-vim',
    event = 'VeryLazy', -- Cannot be lazy loaded otherwise!
  },


  --- Terminal and file management support ---

  -- TODO: Evaluate 'Olical/conjure'

  -- Send code to REPL: send motion in normal mode (gy_) or visual mode (gy),
  -- send line (gyy) and send paragraph (gyY).
  {
    'jpalardy/vim-slime',
    event = 'VeryLazy', -- Cannot be lazy loaded otherwise!
    init = function()
      vim.g.slime_no_mappings = 1
      vim.g.slime_paste_file = os.tmpname()
      vim.g.slime_target = 'neovim'

      -- Slime overrides: https://github.com/jpalardy/vim-slime#advanced-configuration-overrides

      -- IPython REPL
      if vim.g.os ~= 'Windows' then
        vim.g.slime_python_ipython = 1
      else
        -- TODO: improve this (check vim-slime issues #123/223/273/283/293)
        vim.cmd([[
          function! SlimeOverride_EscapeText_python(text)
            " Using %paste (as done in neoterm), since %cpaste does not seem to work
            " in neovim's terminal on Windows.
            call setreg('+', a:text, 'l')
            return ['%paste', g:slime_dispatch_ipython_pause, "\n"]
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
      vim.api.nvim_set_keymap('x', 'gy',  '<Plug>SlimeRegionSend',    {})
      vim.api.nvim_set_keymap('n', 'gy',  '<Plug>SlimeMotionSend',    {})
      vim.api.nvim_set_keymap('n', 'gyy', '<Plug>SlimeLineSend',      {})
      vim.api.nvim_set_keymap('n', 'gyY', '<Plug>SlimeParagraphSend', {})

      -- Reset vim-slime configuration in all buffers.
      vim.cmd([[autocmd vimrc TermClose * bufdo if exists('b:slime_config') | unlet b:slime_config | endif]])
    end,
  },

  -- File manager for Neovim with a directory buffer that allows file manipulation
  -- by editing text. Save buffer to modify filesystem. Use <CR> to open file or
  -- directory, gh to toggle hidden files and - to open parent directory.
  {
    'elihunter173/dirbuf.nvim',
    keys = { '<BS>' },
    cmd = 'Dirbuf',
    init = function()
      -- Hack to avoid the plugin automatically mapping '-'
      vim.api.nvim_set_keymap('n', '-', '-', { noremap=true })
    end,
    config = function()
      require('dirbuf').setup({})
      vim.api.nvim_set_keymap('n', '<BS>', '<Plug>(dirbuf_up)', {})
    end,
  },

  -- Shell commands :Delete, :Move, :Rename, :Mkdir, :Chmod, :Wall (save all).
  {
    'tpope/vim-eunuch',
    cmd = { 'Delete', 'Move', 'Rename', 'Mkdir', 'Chmod', 'Wall' },
  },


  --- Git integration ---

  -- Git support (:Git).
  {
    'tpope/vim-fugitive',
    cmd = { 'Git' },
    config = function()
      vim.cmd([[autocmd vimrc FileType fugitive nmap <buffer> <Tab> =]])
    end,
  },

  -- Show a git diff in the sign column.
  {
    'lewis6991/gitsigns.nvim',
    event = 'BufReadPre',
    cmd = { 'Gitsigns' },
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function ()
      -- TODO: set key bindings: https://github.com/lewis6991/gitsigns.nvim#keymaps
      require('gitsigns').setup({
        on_attach = function(bufnr)
          local gs = package.loaded.gitsigns
          local function map(mode, lhs, rhs, opts)
            opts = opts or {}
            opts.buffer = bufnr
            vim.keymap.set(mode, lhs, rhs, opts)
          end
          -- Navigation
          map('n', ']h', function()
            if vim.wo.diff then return ']c' end
            vim.schedule(function() gs.next_hunk() end)
            return '<Ignore>'
          end, {expr = true, desc = 'Next hunk'})
          map('n', '[h', function()
            if vim.wo.diff then return '[c' end
            vim.schedule(function() gs.prev_hunk() end)
            return '<Ignore>'
          end, {expr = true, desc = 'Previous hunk'})
          -- Actions
          map('n', '<Leader>ghh', gs.preview_hunk)
          map({'n', 'v'}, '<Leader>ghs', ':Gitsigns stage_hunk<CR>')
          map({'n', 'v'}, '<Leader>ghr', ':Gitsigns reset_hunk<CR>')
          map('n', '<Leader>ghS', gs.stage_buffer)
          map('n', '<Leader>ghu', gs.undo_stage_hunk)
          map('n', '<Leader>ghR', gs.reset_buffer)
          map('n', '<Leader>ghp', gs.preview_hunk)
          map('n', '<Leader>ghb', function() gs.blame_line{full=true} end)
          map('n', '<Leader>ghB', gs.toggle_current_line_blame)
          map('n', '<Leader>ghd', gs.diffthis)
          map('n', '<Leader>ghD', function() gs.diffthis('~') end)
          map('n', '<Leader>ght', gs.toggle_deleted)
          map('n', '<Leader>ghT', gs.setqflist)
          -- Text object
          map({'o', 'x'}, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
        end
      })
    end
  },

  -- Tab page interface for cycling through diffs
  {
    'sindrets/diffview.nvim',
    cmd = { 'DiffviewOpen', 'DiffviewFileHistory' },
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = true,
  },


  --- Search commands ---

  -- Extendable fuzzy finder.
  {
    'nvim-telescope/telescope.nvim',
    cmd = { 'Telescope' },
    dependencies = {
      {'nvim-lua/plenary.nvim'},
      {'nvim-telescope/telescope-file-browser.nvim'},
      {'nvim-telescope/telescope-fzf-native.nvim', build = 'make'},
    },
    config = function()
      require('telescope').setup({
        pickers = {
          buffers = {
            mappings = {
              i = {
                ['<C-d>'] = 'delete_buffer',
              },
            },
          },
        },
      })
      require('telescope').load_extension('projects')
      require('telescope').load_extension('file_browser')
      require('telescope').load_extension('fzf')
    end,
  },


  --- Windows, interface elements, visual editing helpers and themes ---

  -- Show start screen.
  {
    'goolord/alpha-nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      local theme = require('alpha.themes.theta')

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
      for _, line in ipairs(require('alpha.fortune')()) do
        table.insert(header, line)
      end
      table.insert(header, '')
      theme.header.val = header
      theme.header.opts.hl = 'Normal'

      local dashboard = require('alpha.themes.dashboard')
      local buttons = {
        { type = 'text', val = 'Quick links', opts = { hl = 'SpecialComment', position = 'center' } },
        { type = 'padding', val = 1 },
        dashboard.button('e', '  New file',       '<Cmd>enew<CR>'),
        dashboard.button('f', '  Find file',      '<Cmd>Telescope find_files<CR>'),
        dashboard.button('b', '  Browse files',   '<Cmd>Telescope file_browser<CR>'),
        dashboard.button('g', '  Live grep',      '<Cmd>Telescope live_grep<CR>'),
        dashboard.button('c', '  Configuration',  '<Cmd>edit $MYVIMRC<CR>'),
        dashboard.button('p', '  Update plugins', '<Cmd>Lazy update<CR>'),
        dashboard.button('t', '  Update tools',   '<Cmd>Mason<CR>'),
        dashboard.button('q', '  Quit',           '<Cmd>qa<CR>'),
      }
      theme.buttons.val = buttons

      require('alpha').setup(theme.config)
    end,
  },

  -- List for showing diagnostics, references, search results, quickfix and
  -- location lists.
  {
    'folke/trouble.nvim',
    cmd = { 'Trouble', 'TroubleToggle' },
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    init = function()
      vim.cmd([[autocmd vimrc FileType Trouble setlocal colorcolumn=]])
    end,
    config = function()
      require('trouble').setup({
        mode = 'document_diagnostics',
      })
    end,
  },

  -- Display popup with key bindings.
  {
    'folke/which-key.nvim',
    event = 'VeryLazy',
    config = function()
      local wk = require('which-key')
      wk.setup({
        plugins = {
          presets = {
            operators = false,
            motions = false,
            text_objects = false,
            windows = true,
            g = false,
            z = true,
          },
        },
        key_labels = {
          -- Override label used to display some keys
          ['<space>'] = 'SPC',
          ['<CR>']    = 'RET',
          ['<Tab>']   = 'TAB',
        },
      })
      wk.register(LEADER_MAPPINGS, { prefix = '<Leader>' })
    end,
  },

  -- Statusline plugin.
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      local terminal = {
        sections = {
          lualine_a = { 'winnr', 'mode' },
          lualine_b = { 'vim.opt.filetype._value', "'jobid: ' .. vim.opt.channel._value" },
          lualine_c = { 'filename' },
          lualine_x = { {'filetype', icon_only = true, colored = false} },
          lualine_z = { 'progress', 'location' },
        },
        inactive_sections = {
          lualine_a = { 'winnr' },
          lualine_c = { 'vim.opt.filetype._value', "'jobid: ' .. vim.opt.channel._value" },
          lualine_x = { 'location' },
        },
        filetypes = { 'terminal' },
      }
      require('lualine').setup({
        options = {
          globalstatus = false,
        },
        sections = {
          lualine_a = { 'winnr', 'mode' },
          lualine_b = { 'branch', 'diagnostics' },
          lualine_c = { 'filename' },
          lualine_x = { { 'filetype', colored = false, icon_only = true } },
          lualine_y = { 'encoding', 'fileformat' },
          lualine_z = { 'progress', 'location' },
        },
        inactive_sections = {
          lualine_a = { 'winnr' },
        },
        tabline = {
          lualine_a = { { 'buffers', mode = 0 } },
          lualine_z = { { 'tabs', mode = 2 } },
        },
        extensions = { 'fugitive', 'quickfix', terminal },
      })
    end,
  },

  -- TODO: evaluate mini.indentscope
  -- Indent guides.
  {
    'lukas-reineke/indent-blankline.nvim',
    event = 'BufReadPre',
    config = function()
      require('indent_blankline').setup({
        show_current_context = true,
        use_treesitter = true,
        buftype_exclude = BUFTYPE_BL,
        filetype_exclude = FILETYPE_BL,
      })
      -- Refresh screen after folder commands
      for _, key in pairs({'o','O','c','C','a','A','v','x','X','m','M','r','R'}) do
        vim.api.nvim_set_keymap('n', 'z'..key, 'z'..key..'<Cmd>IndentBlanklineRefresh<CR>', {noremap=true, silent=true})
      end
    end,
  },

  -- Highlight a unique character in every word when using f/F.
  {
    'unblevable/quick-scope',
    event = 'BufReadPre',
    init = function()
      vim.g.qs_buftype_blacklist = BUFTYPE_BL
      vim.g.qs_filetype_blacklist = FILETYPE_BL
    end,
    config = function()
      -- Add underline to quick-scope highlighted characters.
      -- Reference: https://stackoverflow.com/questions/18774910/how-to-partially-link-highlighting-groups
      vim.cmd([[
        execute 'highlight QuickScopePrimary gui=underline' .
          \' guifg='   . synIDattr(synIDtrans(hlID('ErrorMsg')), 'fg', 'gui')
        execute 'highlight QuickScopeSecondary gui=underline' .
          \' guifg='   . synIDattr(synIDtrans(hlID('WarningMsg')), 'fg', 'gui')
      ]])
    end,
  },

  -- TODO: evaluate mini.trailspace
  -- Whitespace highlighting and removal.
  {
    'ntpeters/vim-better-whitespace',
    event = 'BufReadPre',
    init = function()
      -- Deactivate '<Leader>s' key mapping
      vim.g.better_whitespace_operator = ''
      vim.g.better_whitespace_filetypes_blacklist = vim.fn.extend({
        'diff', 'git', 'gitcommit', 'unite', 'qf', 'markdown'
      }, FILETYPE_BL)
    end,
    config = function()
      local opts = {noremap = true, silent = true}
      vim.api.nvim_set_keymap('n', ']w', '<Cmd>NextTrailingWhitespace<CR>', opts)
      vim.api.nvim_set_keymap('n', '[w', '<Cmd>PrevTrailingWhitespace<CR>', opts)

      -- vim-better-whitespace color settings.
      vim.cmd([[
        execute 'highlight ExtraWhitespace' .
          \' guibg=' . synIDattr(synIDtrans(hlID('Error')), 'fg', 'gui')
      ]])
    end,
  },

  -- Color highlighter.
  {
    'norcalli/nvim-colorizer.lua',
    cmd = { 'ColorizerToggle' },
    config = true,
  },

  --[[
  Treesitter supported colorschemes:
  - https://github.com/nvim-treesitter/nvim-treesitter/wiki/Colorschemes
  - https://github.com/rockerBOO/awesome-neovim#tree-sitter-supported-colorscheme
  --]]

  -- Tokyo Night theme.
  {
    'folke/tokyonight.nvim',
    lazy = false,
    priority = 1000,
    init = function()
      -- Set theme properties.
      vim.g.tokyonight_style = 'storm'
      vim.g.tokyonight_lualine_bold = 1
    end,
    config = function()
      -- Load default color scheme.
      vim.cmd([[
        colorscheme tokyonight
        highlight Folded guibg=NONE
      ]])
    end,
  },

}


-----------------------------
----- WhichKey mappings -----
-----------------------------

-- Read user input and run vim command
local function rr(prompt, cmd, completion)
  -- TODO: evaluate how to use Telescope UI for input (e.g. dressing.nvim)
  vim.ui.input({prompt=prompt, completion=completion}, function(value)
    if value == nil then
      return
    else
      vim.cmd(string.gsub(cmd, '{}', value))
    end
  end)
end

-- Go to next tab, creating a second one if only one exists
local function new_or_next_tab()
  if vim.fn.tabpagenr('$') == 1 then
    vim.cmd('tabedit')
  else
    vim.cmd('tabnext')
  end
end

-- Reference: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_formatting
function GetFormattingOptions()
  return {
    tabSize = vim.fn.shiftwidth(),
    insertSpaces = vim.bo.expandtab,
    trimTrailingWhitespace = true,
    insertFinalNewline = true,
    trimFinalNewlines= true,
  }
end


LEADER_MAPPINGS = {

  ['<Leader>'] = {'<Cmd>Telescope buffers theme=ivy<CR>', 'Find buffer'},
  ['`']        = {'<C-^>',                                'Alternate file'},

  w = {
    name     = 'window',
    ['p']    = {'<C-w>p',             'Go to alternate window'},
    ['w']    = {'<C-w>w',             'Go to next window'},
    ['W']    = {'<C-w>W',             'Go to previous window'},
    ['h']    = {'<C-w>h',             'Go left'},
    ['l']    = {'<C-w>l',             'Go right'},
    ['j']    = {'<C-w>j',             'Go down'},
    ['k']    = {'<C-w>k',             'Go up'},
    ['t']    = {'<C-w>t',             'Go to top-left window'},
    ['b']    = {'<C-w>b',             'Go to bottom-right window'},
    ['H']    = {'<C-w>H',             'Move far left'},
    ['L']    = {'<C-w>L',             'Move far right'},
    ['J']    = {'<C-w>J',             'Move to bottom'},
    ['K']    = {'<C-w>K',             'Move to top'},
    ['c']    = {'<C-w>c',             'Close window'},
    ['q']    = {'<C-w>q',             'Quit window'},
    ['u']    = {'<C-w>u',             'Undo quit window'},
    ['U']    = {'<C-w>U',             'Undo quit windows in tab'},
    ['n']    = {'<C-w>n',             'New window'},
    ['o']    = {'<C-w>o',             'Only window'},
    ['s']    = {'<C-w>s',             'Split horizontally'},
    ['v']    = {'<C-w>v',             'Split vertically'},
    ['r']    = {'<C-w>r',             'Rotate downwards'},
    ['R']    = {'<C-w>R',             'Rotate upwards'},
    ['T']    = {'<C-w>T',             'Move to new tab'},
    ['=']    = {'<C-w>=',             'Balance windows'},
    ['+']    = {'<C-w>5+',            'Increase height'},
    ['-']    = {'<C-w>5-',            'Decrease height'},
    ['>']    = {'<C-w>10>',           'Increase width'},
    ['<lt>'] = {'<C-w>10<',           'Decrease width'},
    ['1']    = {'<Cmd>1wincmd w<CR>', 'Go to window 1'},
    ['2']    = {'<Cmd>2wincmd w<CR>', 'Go to window 2'},
    ['3']    = {'<Cmd>3wincmd w<CR>', 'Go to window 3'},
    ['4']    = {'<Cmd>4wincmd w<CR>', 'Go to window 4'},
    ['5']    = {'<Cmd>5wincmd w<CR>', 'Go to window 5'},
  },

  b = {
    name  = 'buffer',
    ['b'] = {'<Cmd>Telescope buffers theme=ivy<CR>', 'Find buffer'},
    ['w'] = {'<Cmd>write<CR>',                       'Write buffer'},
    ['s'] = {'<Cmd>write<CR>',                       'Save buffer'},
    ['n'] = {'<Cmd>bnext<CR>',                       'Next buffer'},
    ['p'] = {'<Cmd>bprevious<CR>',                   'Previous buffer'},
    ['#'] = {'<Cmd>buffer #<CR>',                    'Alternate buffer'},
    ['d'] = {'<Cmd>lua MiniBufremove.delete()<CR>',  'Delete buffer'},
    ['e'] = {'<Cmd>enew<CR>',                        'Edit new buffer'},
    ['W'] = {'<Cmd>wall<CR>',                        'Write all buffers'},
    ['r'] = {'<Cmd>edit %<CR>',                      'Reload current buffer'},
    -- TODO: evaluate this
    ['R'] = {'<Cmd>checktime<CR>',                   'Reload all buffers'},
  },

  ['<Tab>'] = {
    name      = 'tab',
    ['<Tab>'] = {new_or_next_tab,        'New or next tab'},
    ['e']     = {'<Cmd>tabedit<CR>',     'New tab'},
    ['c']     = {'<Cmd>tabclose<CR>',    'Close tab'},
    ['o']     = {'<Cmd>tabonly<CR>',     'Only tab'},
    ['n']     = {'<Cmd>tabnext<CR>',     'Next tab'},
    ['p']     = {'<Cmd>tabprevious<CR>', 'Previous tab'},
    ['w']     = {'<C-w>T',               'Move window to new tab'},
    ['1']     = {'1gt',                  'Go to tab 1'},
    ['2']     = {'2gt',                  'Go to tab 2'},
    ['3']     = {'3gt',                  'Go to tab 3'},
    ['4']     = {'4gt',                  'Go to tab 4'},
    ['5']     = {'5gt',                  'Go to tab 5'},
    ['r']     = {function() rr('Tab name: ', 'LualineRenameTab {}') end, 'Rename tab'},
  },

  f = {
    name  = 'file',
    ['f'] = {'<Cmd>Telescope find_files noignore=true<CR>', 'Find project files'},
    ['F'] = {'<Cmd>Telescope file_browser<CR>',             'File browser'},
    ['h'] = {'<Cmd>Telescope find_files cwd=~<CR>',         'Find home folder files'},
    ['r'] = {'<Cmd>Telescope oldfiles<CR>',                 'Recent files'},
    ['g'] = {'<Cmd>Telescope git_files<CR>',                'Find git files'},
    ['p'] = {'<Cmd>Telescope projects<CR>',                 'Find projects'},
    ['n'] = {'<Cmd>enew<CR>',                               'New file'},
    ['D'] = {'<Cmd>Delete<CR>',                             'Delete file'},
    ['R'] = {function() rr('New name: ', 'Rename {}') end,  'Rename file'},
  },

  v = {
    name = 'neovim',
    ['v'] = {'<Cmd>edit $MYVIMRC<CR>', 'Open vim config'},
    ['l'] = {'<Cmd>Lazy<CR>',          'Open Lazy menu'},
    ['m'] = {'<Cmd>Mason<CR>',         'Open Mason menu'},
    ['t'] = {'<Cmd>TSUpdate<CR>',      'Treesitter update'},
    ['h'] = {'<Cmd>Alpha<CR>',         'Open home buffer'},
    ['H'] = {'<Cmd>checkhealth<CR>',   'Check health'},
    ['p'] = {'<Cmd>Luapad<CR>',        'Open lua scratch pad'},
  },

  -- TODO: evaluate mini.sessions
  S = {
    name = 'session',
    ['l'] = {'<Cmd>SLoad<CR>',   'Load session'},
    ['s'] = {'<Cmd>SSave<CR>',   'Save session'},
    ['d'] = {'<Cmd>SDelete<CR>', 'Delete session'},
    ['c'] = {'<Cmd>SClose<CR>',  'Close session'},
  },

  t = {
    name = 'trouble',
    ['t'] = {'<Cmd>TroubleToggle<CR>',                       'Toggle Trouble'},
    ['w'] = {'<Cmd>TroubleToggle workspace_diagnostics<CR>', 'Workspace diagnostics'},
    ['d'] = {'<Cmd>TroubleToggle document_diagnostics<CR>',  'Document diagnostics'},
    ['q'] = {'<Cmd>TroubleToggle quickfix<CR>',              'Quickfix items'},
    ['l'] = {'<Cmd>TroubleToggle loclist<CR>',               'Loclist items'},
    ['r'] = {'<Cmd>TroubleToggle lsp_references<CR>',        'LSP references'},
    ['s'] = {'<Cmd>SymbolsOutline<CR>',                      'Symbols outline'},
  },

  r = {
    name = 'repl',
    ['c'] = {'<Cmd>SlimeConfig<CR>',                              'Configure REPL'},
    ['s'] = {'<Cmd>vsplit<Bar>terminal<CR>',                      'Open system shell'},
    ['j'] = {'<Cmd>vsplit<Bar>terminal julia<CR>',                'Open Julia REPL'},
    ['r'] = {'<Cmd>vsplit<Bar>terminal R<CR>',                    'Open R REPL'},
    ['p'] = {'<Cmd>vsplit<Bar>terminal ipython --profile=vi<CR>', 'Open Python REPL'},
  },

  c = {
    name  = 'code',
    ['f']  = {'<Cmd>lua vim.lsp.buf.formatting(GetFormattingOptions())<CR>', 'Format buffer'},
    ['b']  = {'<Cmd>!black %<CR>',                'Format with black'},
    ['i']  = {'<Cmd>!isort %<CR>',                'Format with isort'},
    ['w']  = {'<Cmd>StripWhitespace<CR>',         'Strip whitespace'},
    ['s']  = {'<Cmd>Telescope spell_suggest<CR>', 'Spell suggest'},
    c = {
      name  = 'codeium',
      ['e'] = {'<Cmd>Codeium Enable<CR>', 'Enable Codeium'},
      ['E'] = {'<Cmd>Codeium EnableBuffer<CR>', 'Enable Codeium in buffer'},
      ['d'] = {'<Cmd>Codeium Disable<CR>', 'Disable Codeium'},
      ['D'] = {'<Cmd>Codeium DisableBuffer<CR>', 'Disable Codeium in buffer'},
      ['a'] = {'<Cmd>Codeium Auth<CR>', 'Authenticate Codeium'},
    },
  },

  d = {
    name = 'diff',
    ['s'] = {function() rr('Other file: ','diffsplit {}','file') end, 'Open split diff window'},
    ['t'] = {'<Cmd>diffthis<CR>',     'Diff this window'},
    ['o'] = {'<Cmd>diffoff<CR>',      'Switch off diff'},
    ['u'] = {'<Cmd>diffupdate<CR>',   'Update diff'},
    ['g'] = {'<Cmd>diffget<CR>',      'Get diff from other'},
    ['p'] = {'<Cmd>diffput<CR>',      'Put diff to other'},
    ['G'] = {"<Cmd>'<,'>diffget<CR>", 'Get selection from other'},
    ['P'] = {"<Cmd>'<,'>diffput<CR>", 'Put selection to other'},
  },

  l = {
    name = 'lsp',
    ['d'] = {'<Cmd>Telescope diagnostics bufnr=0<CR>',           'List diagnostics'},
    ['r'] = {'<Cmd>Telescope lsp_references<CR>',                'References for word under cursor'},
    ['s'] = {'<Cmd>Telescope lsp_document_symbols<CR>',          'Document symbols'},
    ['S'] = {'<Cmd>Telescope lsp_workspace_symbols<CR>',         'Workspace symbols'},
    ['D'] = {'<Cmd>Telescope lsp_dynamic_workspace_symbols<CR>', 'Dynamically lists workspace symbols'},
    ['i'] = {'<Cmd>Telescope lsp_incoming_calls<CR>',            'List incoming calls'},
    ['o'] = {'<Cmd>Telescope lsp_outgoing_calls<CR>',            'List outgoing calls'},
    ['h'] = {'<Cmd>lua vim.lsp.buf.hover()<CR>',                 'Hover help'},
    ['H'] = {'<Cmd>lua vim.lsp.buf.signature_help()<CR>',        'Signature help'},
    ['R'] = {'<Cmd>lua vim.lsp.buf.rename()<CR>',                'Rename'},
    ['I'] = {'<Cmd>LspInfo<CR>',                                 'LSP information'},
    p = {
      name = 'peek',
      ['f'] = {'Peek function definition'},
      ['c'] = {'Peek class definition'},
    },
  },

  g = {
    name  = 'git',
    ['g'] = {'<Cmd>tab Git<CR>',                'Git status' },
    ['c'] = {'<Cmd>Git commit<CR>',             'Git commit' },
    ['p'] = {'<Cmd>Git push<CR>',               'Git push' },
    ['l'] = {'<Cmd>Git log<Bar>wincmd L<CR>',   'Git log' },
    ['b'] = {'<Cmd>Git blame<CR>',              'Git blame' },
    ['C'] = {'<Cmd>Telescope git_commits<CR>',  'List git commits'},
    ['B'] = {'<Cmd>Telescope git_branches<CR>', 'List git branches'},
    ['S'] = {'<Cmd>Telescope git_status<CR>',   'List changes per files'},
    ['d'] = {'<Cmd>DiffviewOpen<CR>',           'Diff view' },
    ['f'] = {'<Cmd>DiffviewFileHistory<CR>',    'Diff view file history' },
    h = {
      name = 'hunk',
      ['h'] = {'Preview hunk'},
      ['s'] = {'Stage hunk'},
      ['r'] = {'Reset hunk'},
      ['S'] = {'Stage buffer'},
      ['u'] = {'Undo stage hank'},
      ['R'] = {'Reset buffer'},
      ['p'] = {'Preview hunk'},
      ['b'] = {'Blame line'},
      ['B'] = {'Toggle line blame'},
      ['d'] = {'Diff this'},
      ['D'] = {'Diff this ~'},
      ['t'] = {'Toggle deleted'},
      ['T'] = {'Open Trouble'},
    }
  },

  s = {
    name  = 'search',
    ['s'] = {'<Cmd>Telescope live_grep<CR>',                 'Live grep search'},
    ['w'] = {'<Cmd>Telescope grep_string<CR>',               'Word under cursor'},
    ['b'] = {'<Cmd>Telescope current_buffer_fuzzy_find<CR>', 'Current buffer'},
    ['c'] = {'<Cmd>Telescope commands<CR>',                  'Available commands'},
    [':'] = {'<Cmd>Telescope command_history<CR>',           'Command history'},
    ['/'] = {'<Cmd>Telescope search_history<CR>',            'Search history'},
    ['h'] = {'<Cmd>Telescope help_tags<CR>',                 'Help tags'},
    ['m'] = {'<Cmd>Telescope marks<CR>',                     'Marks'},
    ['r'] = {'<Cmd>Telescope registers<CR>',                 'Registers'},
    ['q'] = {'<Cmd>Telescope quickfix<CR>',                  'Quickfix list'},
    ['l'] = {'<Cmd>Telescope loclist<CR>',                   'Location list'},
    ['j'] = {'<Cmd>Telescope jumplist<CR>',                  'Jump list'},
    ['o'] = {'<Cmd>Telescope vim_options<CR>',               'Vim options'},
    ['a'] = {'<Cmd>Telescope autocommands<CR>',              'Autocommands'},
    ['k'] = {'<Cmd>Telescope keymaps<CR>',                   'Key maps'},
    ['f'] = {'<Cmd>Telescope filetype<CR>',                  'Filetypes'},
    ['H'] = {'<Cmd>Telescope highlights<CR>',                'Highlights'},
    ['p'] = {'<Cmd>Telescope builtin<CR>',                   'Telescope pickers'},
  },

  m = {
    name = 'misc',
    ['c'] = {'<Cmd>ColorizerToggle<CR>', 'Color strings highlighting'},
  },

  h = {
    name = 'harpoon',
    ['h'] = {'<Cmd>lua require("harpoon.ui").toggle_quick_menu()<CR>', 'Toggle quick menu'},
    ['a'] = {'<Cmd>lua require("harpoon.mark").add_file()<CR>',        'Add file'},
    ['1'] = {'<Cmd>lua require("harpoon.ui").nav_file(1)<CR>',         'Go to file 1'},
    ['2'] = {'<Cmd>lua require("harpoon.ui").nav_file(2)<CR>',         'Go to file 2'},
    ['3'] = {'<Cmd>lua require("harpoon.ui").nav_file(3)<CR>',         'Go to file 3'},
    ['4'] = {'<Cmd>lua require("harpoon.ui").nav_file(4)<CR>',         'Go to file 4'},
    ['5'] = {'<Cmd>lua require("harpoon.ui").nav_file(5)<CR>',         'Go to file 5'},
    ['n'] = {'<Cmd>lua require("harpoon.ui").nav_next()<CR>',          'Go to next mark'},
    ['p'] = {'<Cmd>lua require("harpoon.ui").nav_prev()<CR>',          'Go to previous mark'},
    ['m'] = {'<Cmd>Telescope harpoon marks<CR>',                       'Search marks'},
  },

  q = {
    name = 'quit',
    ['q'] = {'<Cmd>quitall<CR>', 'Quit all windows'},
  }
}


require('lazy').setup(PLUGINS, {
  install = {
    colorscheme = { 'tokyonight' },
  },
})

