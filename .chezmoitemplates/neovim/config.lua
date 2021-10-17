-- nvim-colorizer settings.
require('colorizer').setup()


-- nvim-cmp settings.
require('cmp').setup({
    sources = {
      {name = 'nvim_lsp'},
      {name = 'luasnip'},
      {name = 'buffer'},
      {name = 'path'},
      {name = 'nvim_lua'},
    },
    snippet = {
      expand = function(args)
        require('luasnip').lsp_expand(args.body)
      end
    },
  })


-- LSP settings.

-- Python
require('lspconfig').pyright.setup({
    capabilities = require'cmp_nvim_lsp'.update_capabilities(
      vim.lsp.protocol.make_client_capabilities()
      )
  })
