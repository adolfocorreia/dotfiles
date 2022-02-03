--- LSP configuration.
-- Reference: https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md

-- Python (pyright)
-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#pyright
require('lspconfig').pyright.setup({
  require('coq').lsp_ensure_capabilities()
})

-- Julia (julials)
-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#julials
require('lspconfig').julials.setup({})

-- R (r_language_server)
-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#r_language_server
require('lspconfig').r_language_server.setup({})



--- Plugin settings.

-- whick-key.nvim settings.
local wk = require('which-key')
wk.setup({})

-- TODO: check whichkey warnings on :checkhealth
-- TODO: create descriptions for keymaps defined elsewhere (builtin or plugins)
-- e.g. gc, gl, gr, g_, z_, [_, ]_, c_, y_, d_, v_
wk.register({

  ['<Leader>'] = {'<Cmd>Telescope find_files theme=ivy<CR>', 'Find file'},
  ['`']        = {'<C-^>',                                   'Alternate file'},

  w = {
    name     = 'window',
    ['w']    = {'<C-w>p',   'Other window'},
    ['h']    = {'<C-w>h',   'Go left'},
    ['l']    = {'<C-w>l',   'Go right'},
    ['j']    = {'<C-w>j',   'Go down'},
    ['k']    = {'<C-w>k',   'Go up'},
    ['H']    = {'<C-w>H',   'Move far left'},
    ['L']    = {'<C-w>L',   'Move far right'},
    ['J']    = {'<C-w>J',   'Move to bottom'},
    ['K']    = {'<C-w>K',   'Move to top'},
    ['c']    = {'<C-w>c',   'Close window'},
    ['d']    = {'<C-w>c',   'Delete window'},
    ['q']    = {'<C-w>q',   'Quit window'},
    ['n']    = {'<C-w>n',   'New window'},
    ['o']    = {'<C-w>o',   'Only window'},
    ['s']    = {'<C-w>s',   'Split horizontally'},
    ['v']    = {'<C-w>v',   'Split vertically'},
    ['r']    = {'<C-w>r',   'Rotate downwards'},
    ['R']    = {'<C-w>R',   'Rotate upwards'},
    ['T']    = {'<C-w>T',   'Move to new tab'},
    ['=']    = {'<C-w>=',   'Balance windows'},
    ['+']    = {'<C-w>5+',  'Increase height'},
    ['-']    = {'<C-w>5-',  'Decrease height'},
    ['>']    = {'<C-w>10>', 'Increase width'},
    ['<lt>'] = {'<C-w>10<', 'Decrease width'},
  },

  b = {
    name  = 'buffer',
    ['b'] = {'<Cmd>Telescope buffers<CR>', 'Find buffer'},
    ['w'] = {'<Cmd>write<CR>',             'Write buffer'},
    ['s'] = {'<Cmd>write<CR>',             'Save buffer'},
    ['n'] = {'<Cmd>bnext<CR>',             'Next buffer'},
    ['p'] = {'<Cmd>bprevious<CR>',         'Previous buffer'},
    ['d'] = {'<Cmd>Bdelete<CR>',           'Delete buffer'},
    ['e'] = {'<Cmd>enew<CR>',              'Edit new buffer'},
    ['a'] = {'<Cmd>wall<CR>',              'Write all buffers'},
    ['r'] = {'<Cmd>edit %<CR>',            'Reload current buffer'},
    ['R'] = {'<Cmd>checktime<CR>',         'Reload all buffers'},
  },

  ['<Tab>'] = {
    name      = 'tab',
    ['<Tab>'] = {'<Cmd>tabedit<CR>',     'New tab'},
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
  },

  f = {
    name  = 'file',
    ['f'] = {'<Cmd>Telescope find_files noignore=true<CR>', 'Find all files'},
    ['r'] = {'<Cmd>Telescope oldfiles<CR>',                 'Recent files'},
    ['g'] = {'<Cmd>Telescope git_files<CR>',                'Find git files'},
    ['p'] = {'<Cmd>Telescope projects<CR>',                 'Find projects'},
  },

  v = {
    name = 'neovim',
    ['v'] = {'<Cmd>edit $MYVIMRC<CR>',                                  'Open vim config'},
    ['l'] = {'<Cmd>edit '..vim.fn.stdpath('config')..'/config.lua<CR>', 'Open lua config'},
    ['r'] = {'<Cmd>source $MYVIMRC<CR>',                                'Reload neovim config'},
    ['u'] = {'<Cmd>PlugUpdate<CR>',                                     'Update plugins'},
    ['c'] = {'<Cmd>PlugClean<CR>',                                      'Clean plugins'},
    ['i'] = {'<Cmd>PlugInstall<CR>',                                    'Install plugins'},
    ['g'] = {'<Cmd>PlugUpgrade<CR>',                                    'Upgrade plugin manager'},
    ['t'] = {'<Cmd>TSUpdate<CR>',                                       'Treesitter update'},
    ['h'] = {'<Cmd>Startify<CR>',                                       'Open home buffer'},
    ['s'] = {'<Cmd>StartupTime<CR>',                                    'View startup time'},
    ['H'] = {'<Cmd>checkhealth<CR>',                                    'Check health'},
  },

  o = {
    name = 'open',
    ['-'] = {'<Plug>(dirvish_up)',                           'Directory tree'},
    ['T'] = {'<Cmd>terminal<CR>',                            'Terminal'},
    ['t'] = {'<Cmd>TroubleToggle<CR>',                       'Toggle Trouble'},
    ['w'] = {'<Cmd>TroubleToggle workspace_diagnostics<CR>', 'Workspace diagnostics'},
    ['d'] = {'<Cmd>TroubleToggle document_diagnostics<CR>',  'Document diagnostics'},
    ['q'] = {'<Cmd>TroubleToggle quickfix<CR>',              'Quickfix items'},
    ['l'] = {'<Cmd>TroubleToggle loclist<CR>',               'Loclist items'},
    ['r'] = {'<Cmd>TroubleToggle lsp_references<CR>',        'LSP references'},
    ['Q'] = {'<Cmd>copen<CR>',                               'Quickfix list'},
    ['L'] = {'<Cmd>lopen<CR>',                               'Location list'},
  },

  c = {
    name  = 'code',
    ['f'] = {'<Cmd>lua vim.lsp.buf.formatting()<CR>', 'Format buffer'},
    ['w'] = {'<Cmd>StripWhitespace<CR>',              'Strip whitespace'},
    ['s'] = {'<Cmd>Telescope spell_suggest<CR>',      'Spell suggest'},
  },

  l = {
    name = 'LSP',
    ['r'] = {'<Cmd>Telescope lsp_references<CR>',                'References for word under cursor'},
    ['s'] = {'<Cmd>Telescope lsp_document_symbols<CR>',          'Document symbols'},
    ['S'] = {'<Cmd>Telescope lsp_workspace_symbols<CR>',         'Workspace symbols'},
    ['D'] = {'<Cmd>Telescope lsp_dynamic_workspace_symbols<CR>', 'Dynamically lists workspace symbols'},
    ['a'] = {'<Cmd>Telescope lsp_code_actions<CR>',              'Code actions for word under cursor'},
    ['A'] = {'<Cmd>Telescope lsp_range_code_actions<CR>',        'Code actions for given range'},
    ['i'] = {'<Cmd>Telescope lsp_implementations<CR>',           'Goto implementation of word under cursor'},
    ['d'] = {'<Cmd>Telescope lsp_definitions<CR>',               'Goto definition of word under cursor'},
    ['t'] = {'<Cmd>Telescope lsp_type_definitions<CR>',          'Goto type definition of word under cursor'},
  },

  g = {
    name  = 'git',
    ['g'] = {'<Cmd>Git<CR>',                    'Git status' },
    ['c'] = {'<Cmd>Git commit<CR>',             'Git commit' },
    ['p'] = {'<Cmd>Git push<CR>',               'Git push' },
    ['d'] = {'<Cmd>Git diff<CR>',               'Git diff' },
    ['l'] = {'<Cmd>Git log<CR>',                'Git log' },
    ['b'] = {'<Cmd>Git blame<CR>',              'Git blame' },
    ['C'] = {'<Cmd>Telescope git_commits<CR>',  'List git commits'},
    ['B'] = {'<Cmd>Telescope git_branches<CR>', 'List git branches'},
    ['S'] = {'<Cmd>Telescope git_status<CR>',   'List changes per files'},
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

  t = {
    name = 'toogle',
    ['c'] = {'<Cmd>ColorizerToggle<CR>', 'Color strings highlighting'},
  },

}, { prefix = '<Leader>' })


-- substitute.nvim settings.
require('substitute').setup({})


-- nvim-colorizer settings.
require('colorizer').setup({})


-- nvim-treesitter settings.
require('nvim-treesitter.configs').setup({
  ensure_installed = "maintained",
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
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
      },
    },
  },
})


-- lualine.nvim settings.
require('lualine').setup({
  sections = {
    lualine_a = { 'mode' },
    lualine_b = { 'branch', 'diff', 'diagnostics' },
    lualine_c = { 'filename' },
    lualine_x = { { 'filetype', colored = false } },
    lualine_y = { 'encoding', 'fileformat' },
    lualine_z = { 'progress', 'location' },
  },
  tabline = {
    lualine_a = { { 'buffers', mode = 0 } },
    lualine_z = { { 'tabs', mode = 0 } },
  }
})


-- nvim-autopairs settings.
require('nvim-autopairs').setup({
  map_bs = false,  -- conflicts with vim-visual-multi
  map_c_h = true,
})


-- null-ls.nvim settings.
-- Reference: https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md
require('null-ls').setup({
  sources = {
    -- Code actions
    require('null-ls').builtins.code_actions.gitsigns,
    -- Python
    -- TODO: make pylint and isort work on windows
    -- require('null-ls').builtins.diagnostics.pylint,
    require('null-ls').builtins.formatting.black,
    -- require('null-ls').builtins.formatting.isort,
  }
})


-- gitsigns.nvim settings.
-- TODO: set key bindings: https://github.com/lewis6991/gitsigns.nvim#keymaps
require('gitsigns').setup({})


-- indent-blankline.nvim settings.
require('indent_blankline').setup({
  show_current_context = true,
})


-- trouble.nvim settings.
require('trouble').setup({})


-- project.nvim settings.
require('project_nvim').setup({})


-- telescope.nvim settings.
require('telescope').setup({})
require('telescope').load_extension('fzf')
require('telescope').load_extension('projects')

