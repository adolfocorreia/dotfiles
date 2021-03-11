""""" General """""

" Select Leader key
let mapleader = "\<Space>"



"""""" vim-plug """"""
" Installation directory for vim-plug plugins
call plug#begin('~/vimfiles/plugged')

" Base (sane) configuration
Plug 'tpope/vim-sensible'

" Syntax highlighting for several languages
Plug 'sheerun/vim-polyglot'

" Git support
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Add (y), change (c), remove (d) surrounding chars/strings
Plug 'tpope/vim-surround'

" Jump to any forward (s) or backward (S) location specified by two characters
Plug 'justinmk/vim-sneak'

" base16 colorschemes
Plug 'chriskempson/base16-vim'

" Status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Nerdtree
Plug 'preservim/nerdtree'

" Display a vim tip at startup
Plug 'michaelb/vim-tips'

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
" Keep 5 lines above or below the cursor when scrolling.
set scrolloff=5

" Highlight line under cursor. It helps with navigation.
set cursorline

"	Print the line number in front of each line.
set number

" Show the line number relative to the line with the cursor in front of each line.
set relativenumber


"""""" Misc plugins """"""
" Enable sneak labels when moving
let g:sneak#label = 1

nnoremap <C-n> :NERDTreeToggle<CR>

