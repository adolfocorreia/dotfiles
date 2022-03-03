-- Neovim Lua tips:
-- - Use :lua print(vim.inspect(<table>)) to display table contents.

--- Completion engine configuration.

local cmp = require('cmp')
cmp.setup.global({
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end,
  },
  -- TODO: improve mappings / read :h ins-completion
  -- Default mappings: https://github.com/hrsh7th/nvim-cmp/blob/main/lua/cmp/config/default.lua
  mapping = {
    ['<C-d>'] = cmp.mapping.scroll_docs(4),
    ['<C-u>'] = cmp.mapping.scroll_docs(-4),
    -- <C-e> conflicts with vim-rsi
    ['<C-e>'] = cmp.config.disable,
    ['<C-z>'] = cmp.mapping(cmp.mapping.abort(), { 'i' }),
  },
  sources = cmp.config.sources(
    {
      { name = 'nvim_lsp' },
      -- TODO: evaluate removal
      -- { name = 'nvim_lua' },
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
  sources = {
    { name = 'buffer', keyword_length = 2 },
  },
})

cmp.setup.cmdline(':', {
  sources = cmp.config.sources(
    {
      -- Reference: https://github.com/hrsh7th/cmp-cmdline/issues/24
      { name = 'cmdline', keyword_pattern = [=[[^[:blank:]\!]*]=] },
      { name = 'path' },
      { name = 'nvim_lua' },
    }
  ),
})

-- Snippets configuration.
require('luasnip.loaders.from_vscode').lazy_load()


--- LSP configuration.
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


--- Plugin settings.

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
  if vim.fn.tabpagenr('#') == 0 then
    vim.cmd('tabedit')
  else
    vim.cmd('tabnext')
  end
end

-- which-key.nvim settings.
local wk = require('which-key')
wk.setup({
  plugins = {
    presets = {
      motions = false,
    },
  },
  key_labels = {
    -- Override label used to display some keys
    ['<space>'] = 'SPC',
    ['<CR>']    = 'RET',
    ['<Tab>']   = 'TAB',
  },
})

-- TODO: create descriptions for keymaps defined elsewhere (builtin or plugins)
-- e.g. gc, gl, gr, g_, z_, [_, ]_, c_, y_, d_, v_
wk.register({

  ['<Leader>'] = {'<Cmd>Telescope buffers theme=ivy<CR>', 'Find buffer'},
  ['`']        = {'<C-^>',                                'Alternate file'},

  w = {
    name     = 'window',
    ['p']    = {'<C-w>p',   'Go to alternate window'},
    ['w']    = {'<C-w>w',   'Go to next window'},
    ['W']    = {'<C-w>W',   'Go to previous window'},
    ['h']    = {'<C-w>h',   'Go left'},
    ['l']    = {'<C-w>l',   'Go right'},
    ['j']    = {'<C-w>j',   'Go down'},
    ['k']    = {'<C-w>k',   'Go up'},
    ['t']    = {'<C-w>t',   'Go to top-left window'},
    ['b']    = {'<C-w>b',   'Go to bottom-right window'},
    ['H']    = {'<C-w>H',   'Move far left'},
    ['L']    = {'<C-w>L',   'Move far right'},
    ['J']    = {'<C-w>J',   'Move to bottom'},
    ['K']    = {'<C-w>K',   'Move to top'},
    ['c']    = {'<C-w>c',   'Close window'},
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
    ['b'] = {'<Cmd>Telescope buffers theme=ivy<CR>', 'Find buffer'},
    ['w'] = {'<Cmd>write<CR>',                       'Write buffer'},
    ['s'] = {'<Cmd>write<CR>',                       'Save buffer'},
    ['n'] = {'<Cmd>bnext<CR>',                       'Next buffer'},
    ['p'] = {'<Cmd>bprevious<CR>',                   'Previous buffer'},
    ['#'] = {'<Cmd>buffer #<CR>',                    'Alternate buffer'},
    ['d'] = {'<Cmd>Bdelete<CR>',                     'Delete buffer'},
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
    ['R'] = {function() rr('New file name: ', 'Rename {}') end, 'Rename file'},
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

  S = {
    name = 'session',
    ['l'] = {'<Cmd>SLoad<CR>',   'Load session'},
    ['s'] = {'<Cmd>SSave<CR>',   'Save session'},
    ['d'] = {'<Cmd>SDelete<CR>', 'Delete session'},
    ['c'] = {'<Cmd>SClose<CR>',  'Close session'},
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
    ['f']  = {'<Cmd>lua vim.lsp.buf.formatting()<CR>', 'Format buffer'},
    ['w']  = {'<Cmd>StripWhitespace<CR>',              'Strip whitespace'},
    ['s']  = {'<Cmd>Telescope spell_suggest<CR>',      'Spell suggest'},
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
    ['l'] = {'<Cmd>Git log<CR>',                          'Git log' },
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
    ['z'] = {'<Cmd>Zeavim<CR>',                              'Zeal documentation'},
  },

  t = {
    name = 'toogle',
    ['c'] = {'<Cmd>ColorizerToggle<CR>', 'Color strings highlighting'},
    -- TODO: implement diagnostic toggle solution
    ['d'] = {'<Cmd>lua vim.diagnostic.disable(0)<CR>', 'Turn off diagnostics'},
    ['D'] = {'<Cmd>lua vim.diagnostic.enable(0)<CR>',  'Turn on diagnostics'},
  },

  h = {
    name = 'help',
    ['h'] = {'<Cmd>Telescope help_tags<CR>', 'Help tags'},
  },

  q = {
    name = 'quit',
    ['q'] = {'<Cmd>quitall<CR>', 'Quit all windows'},
  }

}, { prefix = '<Leader>' })


-- Comment.nvim settings.
require('Comment').setup({})


-- substitute.nvim settings.
require('substitute').setup({})


-- nvim-colorizer settings.
require('colorizer').setup({})


-- nvim-treesitter settings.
require('nvim-treesitter.configs').setup({
  ensure_installed = 'maintained',
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


-- nvim-treesitter-context settings.
require('treesitter-context').setup({})


-- nvim-gps settings.
local gps = require('nvim-gps')
gps.setup({})


-- lualine.nvim settings.
local neoterm = {
  sections = {
    lualine_a = { 'mode' },
    lualine_b = { 'filetype' },
    lualine_c = { 'filename' },
    lualine_z = { 'progress', 'location' },
  },
  inactive_sections = {
    lualine_c = { 'filetype' },
    lualine_x = { 'location' },
  },
  filetypes = { 'neoterm' },
}
require('lualine').setup({
  sections = {
    lualine_a = { 'mode' },
    lualine_b = { 'branch', 'diff', 'diagnostics' },
    lualine_c = { 'filename', { gps.get_location, cond = gps.is_available } },
    lualine_x = { { 'filetype', colored = false } },
    lualine_y = { 'encoding', 'fileformat' },
    lualine_z = { 'progress', 'location' },
  },
  tabline = {
    lualine_a = { { 'buffers', mode = 0 } },
    lualine_z = { { 'tabs', mode = 2 } },
  },
  extensions = { 'fugitive', 'quickfix', neoterm },
})
-- Set name for first tab.
vim.cmd([[ autocmd vimrc VimEnter * let t:tabname = 'main' ]])


-- nvim-autopairs settings.
require('nvim-autopairs').setup({
  map_bs = true,  -- conflicts with vim-visual-multi
  map_c_h = true,
  map_c_w = false,
})


-- null-ls.nvim settings.
-- Reference: https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md
require('null-ls').setup({
  debug = true,
  sources = {
    -- Code actions
    require('null-ls').builtins.code_actions.gitsigns,
    -- Python
    -- TODO: make pylint and isort work on windows
    require('null-ls').builtins.diagnostics.pylint,
    require('null-ls').builtins.formatting.black,
    -- require('null-ls').builtins.formatting.isort,
  }
})


-- gitsigns.nvim settings.
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
    map('n', '<Leader>ghs', ':Gitsigns stage_hunk<CR>')
    map('v', '<Leader>ghs', ':Gitsigns stage_hunk<CR>')
    map('n', '<Leader>ghr', ':Gitsigns reset_hunk<CR>')
    map('v', '<Leader>ghr', ':Gitsigns reset_hunk<CR>')
    map('n', '<Leader>ghS', '<Cmd>Gitsigns stage_buffer<CR>')
    map('n', '<Leader>ghu', '<Cmd>Gitsigns undo_stage_hunk<CR>')
    map('n', '<Leader>ghR', '<Cmd>Gitsigns reset_buffer<CR>')
    map('n', '<Leader>ghb', '<Cmd>lua require("gitsigns").blame_line{full=true}<CR>')
    map('n', '<Leader>ghB', '<Cmd>Gitsigns toggle_current_line_blame<CR>')
    map('n', '<Leader>ghD', '<Cmd>Gitsigns toggle_deleted<CR>')
    map('n', '<Leader>ght', '<Cmd>Gitsigns setloclist<CR>')
  end
})


-- indent-blankline.nvim settings.
require('indent_blankline').setup({
  show_current_context = true,
})


-- trouble.nvim settings.
require('trouble').setup({})


-- project.nvim settings.
require('project_nvim').setup({})


-- telescope.nvim settings.
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
require('telescope').load_extension('file_browser')
require('telescope').load_extension('fzf')
require('telescope').load_extension('projects')


-- diffview.nvim settings.
require('diffview').setup({})


-- cmp-tabnine settings.
require('cmp_tabnine.config'):setup({
  max_lines = 1000;
  max_num_results = 100;
  sort = true;
  run_on_every_keystroke = true;
})

