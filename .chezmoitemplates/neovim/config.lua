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



--- Plugin settings.

-- whick-key.nvim settings.
local wk = require('which-key')
wk.setup()

wk.register({

  ['`'] = {'<C-^>', 'Edit alternate file'},

  w = {
    name     = 'window',
    ['w']    = {'<C-w>p',  'Other window'},
    ['h']    = {'<C-w>h',  'Go left'},
    ['l']    = {'<C-w>l',  'Go right'},
    ['j']    = {'<C-w>j',  'Go down'},
    ['k']    = {'<C-w>k',  'Go up'},
    ['H']    = {'<C-w>H',  'Move far left'},
    ['L']    = {'<C-w>L',  'Move far right'},
    ['J']    = {'<C-w>J',  'Move to bottom'},
    ['K']    = {'<C-w>K',  'Move to top'},
    ['c']    = {'<C-w>c',  'Close window'},
    ['d']    = {'<C-w>c',  'Delete window'},
    ['q']    = {'<C-w>q',  'Quit window'},
    ['n']    = {'<C-w>n',  'New window'},
    ['o']    = {'<C-w>o',  'Only window'},
    ['s']    = {'<C-w>s',  'Split horizontally'},
    ['v']    = {'<C-w>v',  'Split vertically'},
    ['r']    = {'<C-w>r',  'Rotate downwards'},
    ['R']    = {'<C-w>R',  'Rotate upwards'},
    ['T']    = {'<C-w>T',  'Move to new tab'},
    ['=']    = {'<C-w>=',  'Balance windows'},
    ['+']    = {'<C-w>+',  'Increase height'},
    ['-']    = {'<C-w>-',  'Decrease height'},
    ['>']    = {'<C-w>2>', 'Increase width'},
    ['<lt>'] = {'<C-w>2<', 'Decrease width'},
  },

  b = {
    name  = 'buffer',
    ['b'] = {'<Cmd>Buffers<CR>',   'Find buffer'},
    ['w'] = {'<Cmd>write<CR>',     'Write buffer'},
    ['n'] = {'<Cmd>bnext<CR>',     'Next buffer'},
    ['p'] = {'<Cmd>bprevious<CR>', 'Previous buffer'},
    ['d'] = {'<Cmd>Bdelete<CR>',   'Delete buffer'},
    ['e'] = {'<Cmd>enew<CR>',      'Edit new buffer'},
    ['a'] = {'<Cmd>wall<CR>',      'Write all buffers'},
  },

  ['<Tab>'] = {
    name      = 'tab',
    ['<Tab>'] = {'<Cmd>tabedit<CR>',     'Edit new tab'},
    ['e']     = {'<Cmd>tabedit<CR>',     'Edit new tab'},
    ['c']     = {'<Cmd>tabclose<CR>',    'Close tab'},
    ['d']     = {'<Cmd>tabclose<CR>',    'Delete tab'},
    ['o']     = {'<Cmd>tabonly<CR>',     'Only tab'},
    ['n']     = {'<Cmd>tabnext<CR>',     'Next tab'},
    ['p']     = {'<Cmd>tabprevious<CR>', 'Previous tab'},
    ['1']     = {'1gt',                  'Go to tab 1'},
    ['2']     = {'2gt',                  'Go to tab 2'},
    ['3']     = {'3gt',                  'Go to tab 3'},
    ['4']     = {'4gt',                  'Go to tab 4'},
    ['5']     = {'5gt',                  'Go to tab 5'},
  },

  f = {
    name  = 'file',
    ['f'] = {'<Cmd>Files<CR>',   'Find file'},
    ['r'] = {'<Cmd>History<CR>', 'Recent files'},
    ['g'] = {'<Cmd>GFiles<CR>',  'Find git file'},
  },

  v = {
    name = 'neovim',
    ['v'] = {'<Cmd>Files '..vim.fn.stdpath('config')..'<CR>', 'Open neovim config'},
    ['r'] = {'<Cmd>source $MYVIMRC<CR>',                      'Reload neovim config'},
    ['u'] = {'<Cmd>PlugUpdate<CR>',                           'Update plugins'},
    ['c'] = {'<Cmd>PlugClean<CR>',                            'Clean plugins'},
    ['i'] = {'<Cmd>PlugInstall<CR>',                          'Install plugins'},
    ['g'] = {'<Cmd>PlugUpgrade<CR>',                          'Upgrade plugin manager'},
    ['h'] = {'<Cmd>Startify<CR>',                             'Open home buffer'},
  },

  o = {
    name = 'open/options',
    ['-'] = {'<Plug>(dirvish_up)', 'Directory tree'},
    ['t'] = {'<Cmd>terminal<CR>',  'Terminal'},
    ['q'] = {'<Cmd>copen<CR>',     'Quickfix list'},
    ['l'] = {'<Cmd>lopen<CR>',     'Location list'},
    ['c'] = {'<Cmd>Colors<CR>',    'Colorscheme'},
  },

  c = {
    name  = 'code',
    ['f'] = {'<Cmd>lua vim.lsp.buf.formatting()<CR>', 'Format buffer'},
  },

  g = {
    name  = 'git',
    ['g'] = {'<Cmd>Git<CR>', 'Git status' },
  },

  s = {
    name  = 'search',
    ['p'] = {'<Cmd>Rg<CR>',       'Project'},
    ['l'] = {'<Cmd>Lines<CR>',    'Lines'},
    ['b'] = {'<Cmd>BLines<CR>',   'Buffer lines'},
    ['/'] = {'<Cmd>History/<CR>', 'Search history'},
    [':'] = {'<Cmd>History:<CR>', 'Command history'},
    ['m'] = {'<Cmd>Maps<CR>',     'Maps'},
  },

  t = {
    name = 'trouble',
    ['t'] = {'<Cmd>TroubleToggle<CR>', 'Toggle Trouble'},
    ['w'] = {'<Cmd>TroubleToggle workspace_diagnostics<CR>', 'Workspace diagnostics'},
    ['d'] = {'<Cmd>TroubleToggle document_diagnostics<CR>', 'Document diagnostics'},
    ['q'] = {'<Cmd>TroubleToggle quickfix<CR>', 'Quickfix'},
    ['l'] = {'<Cmd>TroubleToggle loclist<CR>', 'Loclist'},
    ['r'] = {'<Cmd>TroubleToggle lsp_references<CR>', 'LSP references'},
  },

}, { prefix = '<Leader>' })


-- nvim-colorizer settings.
require('colorizer').setup()


-- nvim-treesitter settings.
require('nvim-treesitter.configs').setup({
  ensure_installed = "maintained",
  sync_install = false,
  highlight = { enable = true },
  indent = { enable = true },
  rainbow = { enable = true },
  -- https://github.com/nvim-treesitter/nvim-treesitter#i-want-to-use-a-http-proxy-for-downloading-the-parsers
  prefer_git = true,
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
  map_bs = false,
})


-- null-ls.nvim settings.
-- Reference: https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md
require('null-ls').setup({
  sources = {
    -- Code actions
    require('null-ls').builtins.code_actions.gitsigns,
    -- Python
    require('null-ls').builtins.diagnostics.pylint,
    require('null-ls').builtins.formatting.black,
    -- require('null-ls').builtins.formatting.isort,
  }
})


-- gitsigns.nvim settings.
-- TODO: set key bindings: https://github.com/lewis6991/gitsigns.nvim#keymaps
require('gitsigns').setup()


-- neogit settings.
-- require('neogit').setup({})


-- trouble.nvim settings.
require('trouble').setup({})

