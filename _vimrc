""""" General """""
" Set same encoding as qutebrowser
set encoding=utf-8


"""""" vim-plug """"""
" Installation directory for vim-plug plugins
call plug#begin('~/vimfiles/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'dracula/vim', { 'as': 'dracula' }

" Initialize plugin system
call plug#end()


"""""" Theme """"""
" https://vim.fandom.com/wiki/256_colors_in_vim
set t_Co=256

colorscheme dracula


"""""" Airline """"""
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

" gVim font configuration
" https://vim.fandom.com/wiki/Change_font
" http://vimdoc.sourceforge.net/htmldoc/options.html#'guifont'
if has('gui_running')
  set guifont=FiraMono\ Nerd\ Font:h14
endif

