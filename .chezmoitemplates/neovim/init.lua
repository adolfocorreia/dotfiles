-------------------
----- General -----
-------------------

-- Neovim Lua tips:
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

-- Enable filetype.lua and disable filetype.vim.
vim.g.do_filetype_lua = 1
vim.g.did_load_filetypes = 0

-- Buffer and file types blacklists (disable visual effects).
BUFTYPE_BL = {
  'help',
  'nofile',
  'nowrite',
  'terminal',
}
FILETYPE_BL = {
  'checkhealth',
  'dirbuf',
  'fugitive',
  'harpoon',
  'help',
  'lspinfo',
  'packer',
  'startify',
}


------------------------------
----- Plugin declaration -----
------------------------------

-- TODO: evaluate this better
-- Impatient must be setup before any other plugin is loaded.
local ok_impatient, impatient = pcall(require, 'impatient')
if ok_impatient then impatient.enable_profile() end

-- Install packer if not present.
-- Reference: https://github.com/wbthomason/packer.nvim#bootstrapping
local install_path = vim.fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if vim.fn.empty(vim.fn.glob(install_path, nil, nil)) > 0 then
  local url = 'https://github.com/wbthomason/packer.nvim'
  PACKER_BOOTSTRAP = vim.fn.system({'git', 'clone', '--depth', '1', url, install_path})
  vim.cmd('packadd packer.nvim')
end

-- When setting plugins, use 'setup' or 'config' keys to specify code that should be run
-- either before or after the plugin is loaded, respectively. Usually Vimscript plugins
-- require 'setup' and Lua plugins require 'config'.
require('packer').startup({function(use)

  -- TODO: try to lazy load each plugin (check NvChad and LunarVim)

  --[[
  General guidelines for lazy loading plugins (start/opt):
  - Base plugins (e.g. packer, impatient, colorscheme, bar, which-key) should be loaded at startup
  - Don't bother making light plugins optional (no significant benefit)
  - Heavy or uncommonly used plugins are good candidates for lazy loading
  - Editing help plugins can be loaded when first reading a buffer (BufRead/BufNewFile events)
  - Lua dependency plugins can be loaded when required using 'module' key
  -- TODO: is it really the case that loading at VimEnter makes vi more responsive at startup?
  - Heavy and commonly used plugins can be loaded right after startup (VimEnter event)

  Special cases:
  - Treesitter: loaded at VimEnter to force :TSUpdate after startup
  - LSP: loaded with filetypes of configured LSP servers
  - Completion: loaded at InsertEnter and CmdlineEnter (because of ':' and '/' completions)
  --]]


  --- Neovim management and fixes ---

  -- Packer can manage itself.
  use 'wbthomason/packer.nvim'

  -- Improve startup time for Neovim.
  use 'lewis6991/impatient.nvim'

  -- Delete buffers without losing window layout.
  use {
    'famiu/bufdelete.nvim',
    cmd = 'Bwipeout',
  }

  -- Fix CursorHold Performance.
  use {
    'antoinemadec/FixCursorHold.nvim',
    setup = function()
      vim.g.cursorhold_updatetime = 100
    end,
  }

  -- TODO: evaluate https://github.com/ethanholz/nvim-lastplace
  -- Reopen files at last edit position.
  use 'farmergreg/vim-lastplace'

  -- Startup profiling.
  use {
    'dstein64/vim-startuptime',
    cmd = 'StartupTime',
  }

  -- TODO: evaluate this better
  -- Project management.
  use {
    'ahmedkhalf/project.nvim',
    config = function()
      require('project_nvim').setup({
        detection_methods = { "pattern", "lsp" },
      })
    end,
  }

  -- TODO: evaluate this
  -- REPL and debug console for nvim lua.
  -- Plug 'bfredl/nvim-luadev'
  -- Plug 'rafcamlet/nvim-luapad'



  --- Useful keybingings ---

  -- Make repeat command (.) plugin compatible.
  use 'tpope/vim-repeat'

  -- Jump to any forward (s__) or backward (S__) location specified by two characters.
  -- In case of multiple targets, a third character (label) can be used.
  use {
    'ggandor/lightspeed.nvim',
    keys = { 's', 'S' },
  }

  -- TODO: evaluate machakann/vim-sandwich
  -- Add (ys_), change (cs_), remove (ds_) surrounding delimiters (_ss for whole line).
  use 'tpope/vim-surround'

  -- TODO: evaluate this better
  -- Comment out lines (gcc) or comment out with motions (gc_) or selections (gc).
  -- Use gb_ for block comments.
  use {
    'numToStr/Comment.nvim',
    keys = 'gc',
    config = function() require('Comment').setup({}) end,
  }

  -- Useful [_, ]_ keybindings: b (change buffers), Space (add blank lines),
  -- e (exchange line), navigate quickfix (q/Q) and location (l/L) lists;
  -- Paste after (]p) or before ([p) linewise.
  -- Toggle common options: _oh (hlsearch), _oi (ignorecase), _ol (list tabs and
  -- trailing spaces), _on (number), _or (relativenumber), _ov (virtualedit),
  -- _ow (wrap), _ox (cursorline and cursorcolumn), _oz (spell).
  -- Also _od for :diffthis and :diffoff.
  use {
    'tpope/vim-unimpaired',
    event = { 'BufRead', 'BufNewFile' },
  }

  -- Text exchange operator: cx_, cxx (current line), X (in visual mode),
  -- cxc (clear pending exchanges).
  use {
    'tommcdo/vim-exchange',
    keys = 'cx',
  }

  -- Coerce text cases with: crs (snake_case), crm (MixedCase), crc (camelCase),
  -- cru (UPPER_CASE), cr- (dash-case), cr. (dot.case), cr<space> (space case),
  -- crt (Title Case).
  -- Search and replace words with with variants like plural and case. Use e.g.
  -- :%S/facilit{y,ies}/building{,s}/gc
  use {
    'tpope/vim-abolish',
    keys = 'cr',
    cmd = { 'Subvert', 'S', 'Abolish' },
  }

  -- Emacs keybindings in insert and command modes:
  -- C-b, C-f: back/forward character
  -- M-b, M-f: back/forward word
  -- C-a, C-e: beginning/end of line
  -- M-n, M-p: down/up line
  -- C-d, M-d: delete character/word
  use 'tpope/vim-rsi'

  -- TODO: evaluate this and svermeulen/vim-subversive better
  -- TODO: possibly superfluous since we can substitute a region with a previouly yanked text with v<text object>p (e.g. viwp)
  --       https://vim.fandom.com/wiki/Replace_a_word_with_yanked_text
  -- Replace text object (<M-s>), line (<M-s><M-s>) or to end of line ({M-S) with
  -- contents of the unnamed register "" (e.g. <M-s>iw replaces the word under
  -- the cursor with the current yank).
  use {
    'gbprod/substitute.nvim',
    -- Not working with lazy loading.
    opt = false,
    config = function()
      require('substitute').setup({})

      local opts = { noremap = true }
      vim.api.nvim_set_keymap('n', '<M-s>',      "<Cmd>lua require('substitute').operator()<CR>", opts)
      vim.api.nvim_set_keymap('n', '<M-s><M-s>', "<Cmd>lua require('substitute').line()<CR>",     opts)
      vim.api.nvim_set_keymap('n', '<M-S>',      "<Cmd>lua require('substitute').eol()<CR>",      opts)
      vim.api.nvim_set_keymap('x', '<M-s>',      "<Cmd>lua require('substitute').visual()<CR>",   opts)
    end,
  }

  -- TODO: evaluate monaqa/dial.nvim
  -- Use C-a/C-x to increment/decrement dates, times, roman numerals and ordinals
  -- (e.g. 1st, 2nd, 3rd). For letters of the alphabet, use linewise visual
  -- selection on empty lines.
  use {
    'tpope/vim-speeddating',
    keys = { '<C-a>', '<C-x>' },
    cmd = 'SpeedDatingFormat',
    config = function()
      -- Remove format for non-capital roman numerals.
      vim.cmd([[SpeedDatingFormat! %v]])
    end,
  }


  --- Editing helps ---

  -- Sublime Text-like multiple cursor editing.
  -- To activate, select words with M-d or create cursors vertically with C-j/C-k.
  -- Use C-h/C-l to add characters to the selection and o to go to the other side
  -- of the selection. Use n/N to get more occurrences and [/] to navigate
  -- between selections. Press q to skip current occurrence and get the next one
  -- and Q to remove current selection. Start insert mode with i, a or c.
  use {
    'mg979/vim-visual-multi',
    keys = { '<M-d>', '<C-j>', '<C-k>', '<C-h>', '<C-l>' },
    setup = function()
      -- Visual Multi plugin key mappings.
      vim.g.VM_maps = {
        ['Find Under']         = '<M-d>',
        ['Find Subword Under'] = '<M-d>',
      }
    end,
  }

  -- Align text by some character or regex adding spaces to the left and/or right.
  -- 1. Type gl in visual mode, or gl followed by motion or text object in normal
  --    mode to enter interactive mode.
  -- 2. Optionally enter keys to cycle between alignment options (e.g. <C-d> to
  --    cycle between left, right or center alignment).
  -- 3. Optionally enter keys to define delimiter occurrences to consider (e.g.
  --    2: second occurence, *: all occurences, -: last ocurrence).
  -- 4. Type delimiter key (one of " =:.|&#,", which have predefined rules) or an
  --    arbitrary regex followed by <C-x>.
  -- 5. Alternatively, use the :EasyAlign command.
  -- Reference: https://github.com/junegunn/vim-easy-align
  use {
    'junegunn/vim-easy-align',
    keys = 'gl',
    cmd = 'EasyAlign',
    config = function()
      -- Map vim-easy-align to gl (since ga is already used).
      vim.api.nvim_set_keymap('n', 'gl', '<Plug>(EasyAlign)', {})
      vim.api.nvim_set_keymap('x', 'gl', '<Plug>(EasyAlign)', {})
    end,
  }

  -- TODO: evaluate this better
  -- TODO: evaluate 'LunarWatcher/auto-pairs' since it might be compatible with vim-visual-multi
  -- Insert and delete brackets, parenthesis and quotes in pairs.
  use {
    'windwp/nvim-autopairs',
    event = { 'BufRead', 'BufNewFile' },
    config = function()
      require('nvim-autopairs').setup({
        map_bs = true,  -- conflicts with vim-visual-multi - check workaround below
        map_c_h = true,
        map_c_w = false,
      })
    end,
  }

  -- Show how many times a search pattern occurs in current buffer.
  use {
    'google/vim-searchindex',
    event = { 'BufRead', 'BufNewFile' },
    config = function()
      local opts = {silent=true}
      vim.api.nvim_set_keymap('n', 'n', 'nzzzv<Plug>SearchIndex', opts)
      vim.api.nvim_set_keymap('n', 'N', 'Nzzzv<Plug>SearchIndex', opts)
    end,
  }

  --Harpoon
  use {
    'ThePrimeagen/harpoon',
    requires = 'nvim-lua/plenary.nvim',
    module = { 'harpoon', 'harpoon.mark', 'harpoon.ui', 'harpoon.term', 'harpoon.cmd-ui' },
    wants = 'telescope.nvim',
    config = function()
      require('telescope').load_extension('harpoon')
    end,
  }


  --- Custom motions and text objects ---

  -- CamelCase and snake_case motions (M-w, M-b, M-e).
  use {
    'chaoren/vim-wordmotion',
    keys = { '<M-w>', '<M-b>', '<M-e>' },
    setup = function()
      vim.g.wordmotion_nomap = 1
    end,
    config = function()
      vim.api.nvim_set_keymap('n', '<M-w>', '<Plug>WordMotion_w', {})
      vim.api.nvim_set_keymap('n', '<M-b>', '<Plug>WordMotion_b', {})
      vim.api.nvim_set_keymap('n', '<M-e>', '<Plug>WordMotion_e', {})
    end,
  }

  -- Several text objects with in (i), a (a), inside (I), around (A), next (_n)
  -- and last (_l) semantics.
  -- Pairs: () {} [] <> t (XML/HTML tags)
  -- Quotes: ' " `
  -- Separators: , . ; : + - = ~ _ * # / | \ & $
  -- Arguments: a (surrounded by braces and/or commas)
  -- Any Block: b (similar to pairs, but skips pairs in nested contexts)
  -- Any Quote: q (similar to quotes, but skips pairs in nested contexts)
  -- Reference: https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
  use {
    'wellle/targets.vim',
    -- Not working with lazy loading.
    opt = false,
  }

  -- Indentation level text object: ii (indentation level), ai (ii and line
  -- above), aI (ii with lines above/below).
  use {
    'michaeljsmith/vim-indent-object',
    event = { 'BufRead', 'BufNewFile' },
  }

  -- Text object for the entire buffer (ae/ie).
  --TODO: evaluate alternative plugins / using a%/i% as keybindings
  use {
    'kana/vim-textobj-entire',
    event = { 'BufRead', 'BufNewFile' },
    wants = 'vim-textobj-user',
    requires = 'kana/vim-textobj-user',
  }

  -- Text object (iv) for variable segments in camelCase or snake_case words.
  use {
    'Julian/vim-textobj-variable-segment',
    event = { 'BufRead', 'BufNewFile' },
    wants = 'vim-textobj-user',
    requires = 'kana/vim-textobj-user',
  }

  -- Text object dependency plugin.
  use {
    'kana/vim-textobj-user',
    opt = true,
  }

  -- Language syntax text objects: functions (af/if).
  use {
    'nvim-treesitter/nvim-treesitter-textobjects',
    event = { 'BufRead', 'BufNewFile' },
    after = 'nvim-treesitter',
    requires = 'nvim-treesitter/nvim-treesitter',
  }


  --- Language support ---

  -- Syntax highlighting, indentation, folding and more using ASTs.
  use {
    'nvim-treesitter/nvim-treesitter',
    event = 'VimEnter',
    run = ':TSUpdate',
    config = function()
      require('nvim-treesitter.configs').setup({
        ensure_installed = {
          'bash',
          'fish',
          'haskell',
          'help',
          'json',
          'julia',
          'lua',
          'python',
          'r',
          'toml',
          'vim',
          'yaml',
        },
        sync_install = false,
        -- https://github.com/nvim-treesitter/nvim-treesitter#i-want-to-use-a-http-proxy-for-downloading-the-parsers
        prefer_git = true,
        -- Modules
        highlight = { enable = true },
        indent = { enable = true },
        rainbow = { enable = true },
        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = {
              ['af'] = '@function.outer',
              ['if'] = '@function.inner',
              ['ac'] = '@class.outer',
              ['ic'] = '@class.inner',
              -- TODO: evaluate using ia/aa for arguments/parameters instead of targets.vim
            },
          },
          move = {
            enable = true,
            set_jumps = true,
            goto_next_start = {
              [']m'] = '@function.outer',
              [']]'] = '@class.outer',
            },
            goto_next_end = {
              [']M'] = '@function.outer',
              [']['] = '@class.outer',
            },
            goto_previous_start = {
              ['[m'] = '@function.outer',
              ['[['] = '@class.outer',
            },
            goto_previous_end = {
              ['[M'] = '@function.outer',
              ['[]'] = '@class.outer',
            },
          },
          lsp_interop = {
            enable = true,
            border = 'none',
            peek_definition_code = {
              ['<Leader>cpf'] = '@function.outer',
              ['<Leader>cpc'] = '@class.outer',
            },
          },
        },
      })
    end
  }

  -- TODO: evaluate this better
  -- Show context of currently visible buffer contents.
  use {
    'romgrk/nvim-treesitter-context',
    after = 'nvim-treesitter',
    requires = 'nvim-treesitter/nvim-treesitter',
    config = function() require('treesitter-context').setup({}) end,
  }

  -- TODO: evaluate this better
  -- Show current scope in status line.
  use {
    'SmiteshP/nvim-gps',
    after = { 'nvim-treesitter', 'lualine.nvim' },
    event = { 'BufRead', 'BufNewFile' },
    requires = 'nvim-treesitter/nvim-treesitter',
    config = function()
      local gps = require('nvim-gps')
      gps.setup({})

      local lualine_config = require('lualine').get_config()
      lualine_config['sections']['lualine_c'] = {
        'filename', { gps.get_location, cond = gps.is_available }
      }
      require('lualine').setup(lualine_config)
    end,
  }

  -- TODO: evaluate if sleuth is really necessary with treesitter
  -- TODO: consider using expandtab, tabstop, softtabstop, shiftwidth explicitly
  -- Automatic tab/indenting configuration.
  use 'tpope/vim-sleuth'

  -- TODO: read :h lsp
  -- LSP configuration.
  use {
    'neovim/nvim-lspconfig',
    ft = { 'julia', 'lua', 'python', 'r', 'rmd' },
    config = function()
      -- Reference: https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md

      local lsp_opts = {
        capabilities = require('cmp_nvim_lsp').update_capabilities(
          vim.lsp.protocol.make_client_capabilities()
        ),
      }

      -- Python (pyright)
      -- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#pyright
      require('lspconfig').pyright.setup(lsp_opts)

      -- Julia (julials)
      -- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#julials
      -- TODO: set virtual_text to false (check Neovim-from-Scratch LSP config)
      require('lspconfig').julials.setup(lsp_opts)

      -- R (r_language_server)
      -- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#r_language_server
      require('lspconfig').r_language_server.setup(lsp_opts)

      -- Lua (sumneko)
      -- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#sumneko_lua
      local luadev = require('lua-dev').setup({})
      require('lspconfig').sumneko_lua.setup(luadev)
      -- TODO: evaluate if lsp_opt must also be passed to sumneko
      -- require('lspconfig').sumneko_lua.setup(vim.tbl_extend('error', {lsp_opts, luadev}))
    end,
  }

  -- TODO: evaluate null-ls plugin
  -- Inject LSP diagnostics, code actions and more from non-LSP tools.
  use {
    'jose-elias-alvarez/null-ls.nvim',
    ft = 'python',
    requires = 'nvim-lua/plenary.nvim',
    config = function()
      -- Reference: https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md
      local null_ls = require('null-ls')
      null_ls.setup({
        debug = false,
        sources = {
          -- Code actions
          -- TODO: evaluate this
          -- null_ls.builtins.code_actions.gitsigns,
          -- Python
          -- TODO: make pylint and isort work on windows
          null_ls.builtins.diagnostics.pylint,
          null_ls.builtins.formatting.black,
          -- null_ls.builtins.formatting.isort,
        }
      })
    end,
  }

  -- TODO: evaluate aerial & vista.vim
  -- Tree like view for code symbols.
  use {
    -- 'stevearc/aerial.nvim',
    -- module = "aerial",
    'simrat39/symbols-outline.nvim',
    after = 'nvim-lspconfig',
  }

  -- Lua development setup for init.lua and plugins.
  use {
    'folke/lua-dev.nvim',
    module = 'lua-dev',
  }

  -- TODO: set correct lazy dependencies for completion plugins
  -- Completion.
  use {
    'hrsh7th/nvim-cmp',
    event = { 'InsertEnter', 'CmdlineEnter' },
    module = 'cmp_nvim_lsp',
    config = function()
      local cmp = require('cmp')

      cmp.setup.global({
        snippet = {
          expand = function(args)
            require('luasnip').lsp_expand(args.body)
          end,
        },
        -- TODO: improve mappings / read :h ins-completion
        -- Default mappings: https://github.com/hrsh7th/nvim-cmp/blob/main/lua/cmp/config/mapping.lua
        mapping = cmp.mapping.preset.insert({
          ['<PageDown>'] = cmp.mapping.scroll_docs(4),
          ['<PageUp>']   = cmp.mapping.scroll_docs(-4),
        }),
        sources = cmp.config.sources(
        {
          { name = 'nvim_lsp' },
          -- TODO: evaluate removal
          { name = 'nvim_lua' },
          { name = 'luasnip' },
          { name = 'cmp_tabnine' },
          { name = 'buffer', keyword_length = 4 },
          { name = 'rg', keyword_length = 4 },
          { name = 'path' },
          }
        ),
        -- Reference: https://github.com/hrsh7th/nvim-cmp/wiki/Menu-Appearance
        formatting = {
          format = require('lspkind').cmp_format({
            mode = 'symbol',
            maxwidth = 50,
            menu = ({
              buffer      = '[buf]',
              cmdline     = '[nvim]',
              cmp_tabnine = '[tab9]',
              luasnip     = '[snip]',
              nvim_lsp    = '[lsp]',
              nvim_lua    = '[lua]',
              path        = '[path]',
              rg          = '[rg]',
            })
          }),
        },
      })

      cmp.setup.cmdline('/', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'buffer', keyword_length = 2 },
        },
      })

      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources(
        {
            -- Reference: https://github.com/hrsh7th/cmp-cmdline/issues/24
          { name = 'cmdline', keyword_pattern = [=[[^[:blank:]\!]*]=] },
          { name = 'path' },
          { name = 'nvim_lua' },
          }
        ),
      })
    end,
  }
  use {
    'onsails/lspkind-nvim',
    module = 'lspkind',
  }

  -- Completion sources.
  use { 'hrsh7th/cmp-buffer',   after = 'nvim-cmp', requires = 'hrsh7th/nvim-cmp' }
  use { 'hrsh7th/cmp-cmdline',  after = 'nvim-cmp', requires = 'hrsh7th/nvim-cmp' }
  use { 'hrsh7th/cmp-path',     after = 'nvim-cmp', requires = 'hrsh7th/nvim-cmp' }
  use { 'hrsh7th/cmp-nvim-lua', after = 'nvim-cmp', requires = 'hrsh7th/nvim-cmp' }

  use { 'hrsh7th/cmp-nvim-lsp', after = 'nvim-cmp', requires = 'hrsh7th/nvim-cmp' }
  use { 'lukas-reineke/cmp-rg', after = 'nvim-cmp', requires = 'hrsh7th/nvim-cmp' }

  -- TODO: evaluate loading this plugin at VimEnter to speedup its external engine
  local tabnine = {
    'tzachar/cmp-tabnine',
    -- event = 'VimEnter',
    after = 'nvim-cmp',
    requires = 'hrsh7th/nvim-cmp',
    config = function()
      require('cmp_tabnine.config'):setup({
        max_lines = 1000;
        max_num_results = 100;
        sort = true;
        run_on_every_keystroke = true;
      })
    end,
  }
  if vim.g.os == 'Windows' then
    tabnine['run'] = 'pwsh ./install.ps1'
    use(tabnine)
  else
    tabnine['run'] = 'sh ./install.sh'
    use(tabnine)
  end

  -- Snippets.
  -- TODO: evaluate this better
  use {
    'saadparwaiz1/cmp_luasnip',
    after = 'nvim-cmp',
    wants = 'LuaSnip',
    requires = 'hrsh7th/nvim-cmp',
  }
  use {
    'L3MON4D3/LuaSnip',
    wants = 'friendly-snippets',
    module = 'luasnip',
    config = function() require('luasnip.loaders.from_vscode').lazy_load() end,
  }
  use {
    'rafamadriz/friendly-snippets',
    opt = true,
  }

  --- Language plugins.
  -- Reference: https://github.com/sheerun/vim-polyglot#language-packs

  -- TODO: Evaluate zeavim.vim, vim-dasht and vim-devdocs

  -- TODO: evaluate better way to lazy load this plugin
  -- Julia. LaTeX to Unicode substitutions.
  use {
    'JuliaEditorSupport/julia-vim',
    -- ft = 'julia',
    -- event = 'VimEnter'
    opt = true,
    setup = function()
      vim.g.latex_to_unicode_tab = 'off'
    end,
  }

  -- TODO: evaluate 'nvim-neorg/neorg' and 'nvim-orgmode/orgmode'
  -- Org-mode.


  --- Terminal and file management support ---

  -- Send code to REPL: send motion in normal mode (gr_) or visual mode (gr),
  -- send line (grr) and send paragraph (grR).
  use {
    'jpalardy/vim-slime',
    keys = 'gr',
    cmd = 'SlimeConfig',
    -- ft = { 'python', 'julia' },
    setup = function()
      vim.g.slime_target = 'neovim'
      vim.g.slime_no_mappings = 1

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
      vim.api.nvim_set_keymap('x', 'gr',  '<Plug>SlimeRegionSend',    {})
      vim.api.nvim_set_keymap('n', 'gr',  '<Plug>SlimeMotionSend',    {})
      vim.api.nvim_set_keymap('n', 'grr', '<Plug>SlimeLineSend',      {})
      vim.api.nvim_set_keymap('n', 'grR', '<Plug>SlimeParagraphSend', {})

      -- Reset vim-slime configuration in all buffers.
      vim.cmd([[autocmd vimrc TermClose * bufdo if exists('b:slime_config') | unlet b:slime_config | endif]])
    end,
  }

  -- File manager for Neovim with a directory buffer that allows file manipulation
  -- by editing text. Save buffer to modify filesystem. Use <CR> to open file or
  -- directory, gh to toggle hidden files and - to open parent directory.
  use 'elihunter173/dirbuf.nvim'

  -- Shell commands :Delete, :Move, :Rename, :Mkdir, :Chmod, :Wall (save all).
  use {
    'tpope/vim-eunuch',
    cmd = { 'Delete', 'Move', 'Rename', 'Mkdir', 'Chmod', 'Wall' },
  }


  --- Git integration ---

  -- TODO: evaluate neogit
  -- TODO: make this opt
  -- Git support (:Git).
  use {
    'tpope/vim-fugitive',
    -- Lazy loading does not work for fugitive.
    -- Reference: https://github.com/wbthomason/packer.nvim/issues/530
    opt = false,
    config = function()
      vim.cmd([[autocmd vimrc FileType fugitive nmap <buffer> <Tab> =]])
    end,
  }

  -- Show a git diff in the sign column.
  use {
    'lewis6991/gitsigns.nvim',
    event = { 'BufRead', 'BufNewFile' },
    requires = 'nvim-lua/plenary.nvim',
    config = function ()
      -- TODO: set key bindings: https://github.com/lewis6991/gitsigns.nvim#keymaps
      require('gitsigns').setup({
        on_attach = function(bufnr)
          local function map(mode, lhs, rhs, opts)
            opts = vim.tbl_extend('force', {noremap = true, silent = true}, opts or {})
            vim.api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts)
          end
          -- Navigation
          map('n', ']c', "&diff ? ']c' : '<Cmd>Gitsigns next_hunk<CR>'", {expr = true})
          map('n', '[c', "&diff ? '[c' : '<Cmd>Gitsigns prev_hunk<CR>'", {expr = true})
          -- Text object
          map('o', 'ih', ':<C-u>Gitsigns select_hunk<CR>')
          map('x', 'ih', ':<C-u>Gitsigns select_hunk<CR>')
          -- Actions
          map('n', '<Leader>ghh', '<Cmd>Gitsigns preview_hunk<CR>')
          map('n', '<Leader>ghs', '<Cmd>Gitsigns stage_hunk<CR>')
          map('v', '<Leader>ghs', '<Cmd>Gitsigns stage_hunk<CR>')
          map('n', '<Leader>ghr', '<Cmd>Gitsigns reset_hunk<CR>')
          map('v', '<Leader>ghr', '<Cmd>Gitsigns reset_hunk<CR>')
          map('n', '<Leader>ghS', '<Cmd>Gitsigns stage_buffer<CR>')
          map('n', '<Leader>ghu', '<Cmd>Gitsigns undo_stage_hunk<CR>')
          map('n', '<Leader>ghR', '<Cmd>Gitsigns reset_buffer<CR>')
          map('n', '<Leader>ghb', '<Cmd>lua require("gitsigns").blame_line{full=true}<CR>')
          map('n', '<Leader>ghB', '<Cmd>Gitsigns toggle_current_line_blame<CR>')
          map('n', '<Leader>ghD', '<Cmd>Gitsigns toggle_deleted<CR>')
          map('n', '<Leader>ght', '<Cmd>Gitsigns setloclist<CR>')
        end
      })
    end
  }

  -- Tab page interface for cycling through diffs
  use {
    'sindrets/diffview.nvim',
    requires = 'nvim-lua/plenary.nvim',
    cmd = { 'DiffviewOpen', 'DiffviewFileHistory' },
    config = function() require('diffview').setup({}) end,
  }


  --- Search commands ---

  -- Extendable fuzzy finder.
  use {
    'nvim-telescope/telescope.nvim',
    cmd = 'Telescope',
    module = 'telescope',
    requires = 'nvim-lua/plenary.nvim',
    config = function()
      require('telescope').setup({
        pickers = {
          buffers = {
            mappings = {
              i = {
                ['<C-d>'] = "delete_buffer",
              },
            },
          },
        },
      })
      require('telescope').load_extension('projects')
    end,
  }
  use {
    'nvim-telescope/telescope-file-browser.nvim',
    after = 'telescope.nvim',
    requires = 'nvim-telescope/telescope.nvim',
    config = function() require('telescope').load_extension('file_browser') end,
  }
  use {
    'nvim-telescope/telescope-fzf-native.nvim',
    after = 'telescope.nvim',
    requires = 'nvim-telescope/telescope.nvim',
    run = 'make',
    config = function() require('telescope').load_extension('fzf') end,
  }


  --- Windows, interface elements, visual editing helpers and themes ---

  -- TODO: evaluate goolord/alpha-nvim
  -- Show start screen.
  use {
    'mhinz/vim-startify',
    setup = function()
      vim.g.startify_files_number = 5
      vim.g.startify_fortune_use_unicode = 1
      vim.g.startify_session_persistence = 1
      vim.g.ascii = {
        [[   _______                    .__         ]],
        [[   \      \   ____  _______  _|__| _____  ]],
        [[   /   |   \_/ __ \/  _ \  \/ /  |/     \ ]],
        [[  /    |    \  ___(  <_> )   /|  |  Y Y  \]],
        [[  \____|__  /\___  >____/ \_/ |__|__|_|  /]],
        [[          \/     \/                    \/ ]],
      }
      vim.g.startify_custom_header = [[startify#pad(g:ascii + [''] + startify#fortune#boxed())]]
    end,
  }

  -- List for showing diagnostics, references, search results, quickfix and
  -- location lists.
  use {
    'folke/trouble.nvim',
    cmd = { 'Trouble', 'TroubleToggle' },
    requires = 'kyazdani42/nvim-web-devicons',
    setup = function()
      vim.cmd([[autocmd vimrc FileType Trouble setlocal colorcolumn=]])
    end,
    config = function()
      require('trouble').setup({
        mode = 'document_diagnostics',
      })
    end,
  }

  -- Display popup with key bindings.
  use {
    'folke/which-key.nvim',
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
  }

  -- Status line.
  use {
    'nvim-lualine/lualine.nvim',
    requires = 'kyazdani42/nvim-web-devicons',
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
  }

  -- TODO: evaluate this better
  -- Indent guides.
  use {
    'lukas-reineke/indent-blankline.nvim',
    event = { 'BufRead', 'BufNewFile' },
    config = function() require('indent_blankline').setup({
      show_current_context = true,
      use_treesitter = true,
      buftype_exclude = BUFTYPE_BL,
      filetype_exclude = FILETYPE_BL,
    }) end,
  }

  -- Highlight a unique character in every word when using f/F.
  use {
    'unblevable/quick-scope',
    event = { 'BufRead', 'BufNewFile' },
    -- highlight commands for this plugin depend on colorscheme being loaded before.
    wants = 'tokyonight.nvim',
    setup = function()
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
  }

  -- TODO: add key mappings
  -- Whitespace highlighting and removal.
  use {
    'ntpeters/vim-better-whitespace',
    event = { 'BufRead', 'BufNewFile' },
    -- highlight commands for this plugin depend on colorscheme being loaded before.
    wants = 'tokyonight.nvim',
    setup = function()
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
  }

  -- Color highlighter.
  use {
    'norcalli/nvim-colorizer.lua',
    event = { 'BufRead', 'BufNewFile' },
    config = function() require('colorizer').setup({}) end,
  }

  -- Treesitter supported colorschemes:
  -- - https://github.com/nvim-treesitter/nvim-treesitter/wiki/Colorschemes
  -- - https://github.com/rockerBOO/awesome-neovim#tree-sitter-supported-colorscheme

  -- Tokyo Night theme.
  use {
    'folke/tokyonight.nvim',
    setup = function()
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
  }

  -- Night Fox themes.
  use {
    'EdenEast/nightfox.nvim',
    opt = true,
  }


  -- Automatically set up configuration after cloning packer.nvim.
  if PACKER_BOOTSTRAP then
    require('packer').sync()
  end
end,

config = {
  display = {
    open_cmd = '90vnew \\[packer\\]',
  },
  profile = {
    enable = true,
    threshold = 0,
  },
}})

vim.cmd([[autocmd vimrc FileType packer setlocal colorcolumn=]])



--------------------------
----- Neovim options -----
--------------------------

-- Use ':set option?' to check current option value.
-- Use ':verbose set option?' to check where it was set.

-- Enable 24-bit RGB colors in terminal mode.
vim.opt.termguicolors = true

-- Enable the window title.
vim.opt.title = true

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
-- Sytem clipboard:   "+ register / unnamedplus
vim.opt.clipboard = 'unnamedplus'

-- Do not redraw screen while executing macros.
vim.opt.lazyredraw = true

-- Use treesitter to manage folds.
vim.opt.foldmethod = 'expr'
vim.opt.foldexpr = 'nvim_treesitter#foldexpr()'
vim.opt.foldlevelstart = 5

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

-- Use popup menu for completion (one or more matches) and do not select any
-- match at first.
vim.opt.completeopt = 'menu,menuone,noselect'

-- Always open diff windows vertically
vim.opt.diffopt:append { 'vertical' }

-- Also consider letters in C-a and C-x commands.
vim.opt.nrformats:append { 'alpha' }

-- List all matches and complete till longest common string.
vim.opt.wildmode = 'list:longest'

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

-- Set name for first tab.
vim.cmd([[autocmd vimrc VimEnter * let t:tabname = 'main']])


--- Terminal autocmds.

-- Disable numbering and cursor highlighting in terminal buffers.
vim.cmd([[autocmd vimrc TermOpen * setlocal nonumber norelativenumber nocursorline nocursorcolumn]])

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
  tnoremap <M-Left>  <C-\><C-n><C-w>h
  tnoremap <M-Down>  <C-\><C-n><C-w>j
  tnoremap <M-Up>    <C-\><C-n><C-w>k
  tnoremap <M-Right> <C-\><C-n><C-w>l
  inoremap <M-Left>  <C-\><C-n><C-w>h
  inoremap <M-Down>  <C-\><C-n><C-w>j
  inoremap <M-Up>    <C-\><C-n><C-w>k
  inoremap <M-Right> <C-\><C-n><C-w>l
  nnoremap <M-Left>  <C-w>h
  nnoremap <M-Down>  <C-w>j
  nnoremap <M-Up>    <C-w>k
  nnoremap <M-Right> <C-w>l
]])

vim.cmd([[
  nnoremap <C-w>1 1<C-w>w
  nnoremap <C-w>2 2<C-w>w
  nnoremap <C-w>3 3<C-w>w
  nnoremap <C-w>4 4<C-w>w
  nnoremap <C-w>5 5<C-w>w
]])


-- Goto next/previous diagnostic.
vim.cmd([[
  nnoremap <silent> ]d <Cmd>lua vim.diagnostic.goto_next()<CR>
  nnoremap <silent> [d <Cmd>lua vim.diagnostic.goto_prev()<CR>
]])



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

local packer_loader = vim.fn.stdpath('config') .. '/plugin/packer_compiled.lua'
local plugin_path = vim.fn.stdpath('data') .. '/site/pack/packer'

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
    ['d'] = {'<Cmd>Bwipeout<CR>',                    'Delete buffer'},
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
    ['-'] = {'<Cmd>Dirbuf<CR>',                             'Open directory'},
    ['R'] = {function() rr('New name: ', 'Rename {}') end,  'Rename file'},
  },

  v = {
    name = 'neovim',
    ['v'] = {'<Cmd>edit $MYVIMRC<CR>',            'Open vim config'},
    ['l'] = {'<Cmd>edit '..packer_loader..'<CR>', 'Open packer loader'},
    ['r'] = {'<Cmd>source $MYVIMRC<CR>',          'Reload neovim config'},
    ['t'] = {'<Cmd>TSUpdate<CR>',                 'Treesitter update'},
    ['h'] = {'<Cmd>Startify<CR>',                 'Open home buffer'},
    ['s'] = {'<Cmd>tab StartupTime<CR>',          'View startup time'},
    ['H'] = {'<Cmd>checkhealth<CR>',              'Check health'},
    ['P'] = {'<Cmd>Dirbuf '..plugin_path..'<CR>', 'Check health'},
  },

  p = {
    name = 'packer',
    ['u'] = {'<Cmd>PackerSync<CR>',    'Update plugins'},
    ['c'] = {'<Cmd>PackerCompile<CR>', 'Compile loader'},
    ['i'] = {'<Cmd>PackerInstall<CR>', 'Install plugins'},
    ['s'] = {'<Cmd>PackerStatus<CR>',  'Plugin status'},
    ['p'] = {'<Cmd>PackerProfile<CR>', 'Profile plugins'},
    -- TODO: Use telescope-packer for plugin loading
    -- ['l'] = {'<Cmd>PackerLoad<CR>', 'Load plugin'},
  },

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
    ['pf'] = {'Peek function definition'},
    ['pc'] = {'Peek class definition'},
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
    ['r'] = {'<Cmd>Telescope lsp_references<CR>',                'References for word under cursor'},
    ['s'] = {'<Cmd>Telescope lsp_document_symbols<CR>',          'Document symbols'},
    ['S'] = {'<Cmd>Telescope lsp_workspace_symbols<CR>',         'Workspace symbols'},
    ['D'] = {'<Cmd>Telescope lsp_dynamic_workspace_symbols<CR>', 'Dynamically lists workspace symbols'},
    ['a'] = {'<Cmd>Telescope lsp_code_actions<CR>',              'Code actions for word under cursor'},
    ['A'] = {'<Cmd>Telescope lsp_range_code_actions<CR>',        'Code actions for given range'},
    ['i'] = {'<Cmd>Telescope lsp_implementations<CR>',           'Goto implementation of word under cursor'},
    ['d'] = {'<Cmd>Telescope lsp_definitions<CR>',               'Goto definition of word under cursor'},
    ['t'] = {'<Cmd>Telescope lsp_type_definitions<CR>',          'Goto type definition of word under cursor'},
    ['h'] = {'<Cmd>lua vim.lsp.buf.hover()<CR>',                 'Hover help'},
    ['H'] = {'<Cmd>lua vim.lsp.buf.signature_help()<CR>',        'Signature help'},
    ['R'] = {'<Cmd>lua vim.lsp.buf.rename()<CR>',                'Rename'},
    -- TODO: add Telescope-like border to LspInfo
    ['I'] = {'<Cmd>LspInfo<CR>',                                 'LSP information'},
  },

  g = {
    name  = 'git',
    ['g'] = {'<Cmd>tab Git<Bar>LualineRenameTab git<CR>', 'Git status' },
    ['c'] = {'<Cmd>Git commit<CR>',                       'Git commit' },
    ['p'] = {'<Cmd>Git push<CR>',                         'Git push' },
    ['l'] = {'<Cmd>Git log<Bar>wincmd L<CR>',             'Git log' },
    ['b'] = {'<Cmd>Git blame<CR>',                        'Git blame' },
    ['C'] = {'<Cmd>Telescope git_commits<CR>',            'List git commits'},
    ['B'] = {'<Cmd>Telescope git_branches<CR>',           'List git branches'},
    ['S'] = {'<Cmd>Telescope git_status<CR>',             'List changes per files'},
    ['d'] = {'<Cmd>DiffviewOpen<CR>',                     'Diff view' },
    ['f'] = {'<Cmd>DiffviewFileHistory<CR>',              'Diff view file history' },
    h = {
      name = 'hunk',
      ['h'] = {'Preview hunk'},
      ['s'] = {'Stage hunk'},
      ['S'] = {'Stage buffer'},
      ['u'] = {'Undo stage'},
      ['r'] = {'Reset hunk'},
      ['R'] = {'Reset buffer'},
      ['b'] = {'Blame line'},
      ['B'] = {'Toggle line blame'},
      ['D'] = {'Toggle deleted'},
      ['t'] = {'Open hunks in Trouble'},
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
    -- TODO: implement find current word in help (maybe yanking first and then pasting in help_tags)
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
    -- TODO: implement diagnostic toggle solution
    ['d'] = {'<Cmd>lua vim.diagnostic.disable(0)<CR>', 'Turn off diagnostics in buffer'},
    ['D'] = {'<Cmd>lua vim.diagnostic.enable(0)<CR>',  'Turn on diagnostics in buffer'},
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
    -- TODO: add terminal commands
    ['j'] = {'<Cmd>lua require("harpoon.term").gotoTerminal({idx=9,create_with=":vsplit|terminal julia"})<CR>', 'Open Julia REPL'},
  },

  q = {
    name = 'quit',
    ['q'] = {'<Cmd>quitall<CR>', 'Quit all windows'},
  }
}

