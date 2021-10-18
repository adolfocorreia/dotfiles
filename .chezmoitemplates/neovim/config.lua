-- nvim-colorizer settings.
require('colorizer').setup()


-- LSP configuration.

-- Python
require('lspconfig').pyright.setup({
  require('coq').lsp_ensure_capabilities()
})
