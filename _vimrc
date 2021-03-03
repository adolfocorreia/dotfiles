""""" General """""
" Select Leader key
let mapleader = "\<Space>"


"""""" vim-plug """"""
" Installation directory for vim-plug plugins
call plug#begin('~/vimfiles/plugged')

" Base (sane) configuration (superset of sensible.vim)
Plug 'sheerun/vimrc'

" Syntax highlighting for several languages
Plug 'sheerun/vim-polyglot'

" Git support
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Add (y), change (c), remove (d) surrounding chars/strings
Plug 'tpope/vim-surround'

" base16 colorschemes
Plug 'chriskempson/base16-vim'

" Status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Initialize plugin system
call plug#end()


"""""" Theme """"""
colorscheme base16-tomorrow-night
set termguicolors


"""""" Airline """"""
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme = 'base16_vim'

" gVim font configuration
" https://vim.fandom.com/wiki/Change_font
" http://vimdoc.sourceforge.net/htmldoc/options.html#'guifont'
if has('gui_running')
  set guifont=SauceCodePro\ NF:h14
endif


"""""" Misc """"""
" Enable tab completion
set wildmenu

