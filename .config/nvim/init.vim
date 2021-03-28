""""" General """""

" Select Leader key
let mapleader = "\<Space>"



"""""" vim-plug """"""

" Install vim-plug (if not present)
" Reference: https://github.com/junegunn/vim-plug/wiki/tips
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Installation directory for vim-plug plugins
call plug#begin(stdpath('data') . '/plugged')

" Comment out lines (gcc) or comment out with motions (gc_) or selections (gc)
Plug 'tpope/vim-commentary'

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

" Nerdtree
Plug 'preservim/nerdtree'

" Fuzzy find :Files, :GFiles (git files), :Buffers, :Colors, :Lines, :Marks,
" :Windows, :History (old files), :History: (commands), :History/ (search),
" :Commits, :Commands, :Maps
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-rooter'

" Start screen
Plug 'mhinz/vim-startify'

" Display a vim tip at startup
Plug 'michaelb/vim-tips'

" gruvbox theme
Plug 'sainnhe/gruvbox-material'

" Initialize plugin system
call plug#end()



"""""" Theme """"""

set termguicolors

let g:gruvbox_material_background = 'hard'
let g:gruvbox_material_palette = 'original'
let g:gruvbox_material_transparent_background = 1

colorscheme gruvbox-material



"""""" Airline """"""

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme = 'gruvbox_material'



"""""" Misc """"""

" Enable mouse support in all modes
set mouse=a

" Keep 5 lines above or below the cursor when scrolling.
set scrolloff=5

" Highlight line under cursor. It helps with navigation.
set cursorline

" Highlight column 100. It helps identifying long lines.
set colorcolumn=100

" Print the line number in front of each line.
set number

" Show the line number relative to the line with the cursor in front of each line.
set relativenumber



"""""" Misc plugins """"""

" Enable sneak labels when moving
let g:sneak#label = 1

nnoremap <C-n> :NERDTreeToggle<CR>

nnoremap <Leader>f :Files<CR>

