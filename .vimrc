" Select Leader key
let mapleader = "\<Space>"


" => vim-plug

" Install vim-plug (if not present)
" Reference: https://github.com/junegunn/vim-plug/wiki/tips
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Load plugins
call plug#begin('~/.vim/plugged')
Plug 'sheerun/vimrc'
Plug 'sheerun/vim-polyglot'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'sainnhe/gruvbox-material'
call plug#end()



" => Airline plugin

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

" gVim font configuration
" https://vim.fandom.com/wiki/Change_font
" http://vimdoc.sourceforge.net/htmldoc/options.html#'guifont'
if has('gui_running')
  set guifont=FiraCode\ Nerd\ Font\ Mono\ Medium\ 12
endif



" => gruvbox theme

if has('termguicolors')
  set termguicolors
endif

set background=dark

let g:gruvbox_material_background = 'hard'
let g:gruvbox_material_palette = 'original'

colorscheme gruvbox-material

let g:airline_theme = 'gruvbox_material'

