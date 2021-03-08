""""" General """""

" Select Leader key
let mapleader = "\<Space>"



"""""" vim-plug """"""

" Install vim-plug (if not present)
" Reference: https://github.com/junegunn/vim-plug/wiki/tips
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Installation directory for vim-plug plugins
call plug#begin('~/.vim/plugged')

" Base (sane) configuration (superset of sensible.vim)
Plug 'sheerun/vimrc'

" Syntax highlighting for several languages
Plug 'sheerun/vim-polyglot'

" Git support
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Add (y), change (c), remove (d) surrounding chars/strings
Plug 'tpope/vim-surround'

" Jump to any forward (s) or backward (S) location specified by two characters
Plug 'justinmk/vim-sneak'

" Status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Display a vim tip at startup
Plug 'michaelb/vim-tips'

" gruvbox theme
Plug 'sainnhe/gruvbox-material'

" Initialize plugin system
call plug#end()



"""""" Airline """"""

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

" gVim font configuration
" https://vim.fandom.com/wiki/Change_font
" http://vimdoc.sourceforge.net/htmldoc/options.html#'guifont'
if has('gui_running')
  set guifont=FiraCode\ Nerd\ Font\ Mono\ Medium\ 12
endif



"""""" Theme """"""

if has('termguicolors')
  set termguicolors
endif

set background=dark

let g:gruvbox_material_background = 'hard'
let g:gruvbox_material_palette = 'original'

colorscheme gruvbox-material

let g:airline_theme = 'gruvbox_material'



"""""" Misc """"""

" Enable tab completion
set wildmenu

