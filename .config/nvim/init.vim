""""" General """""

" Define vimrc autocommand group
augroup vimrc
  " Remove all previously set vimrc autocommands when (re)sourcing this file
  " Reference: https://learnvimscriptthehardway.stevelosh.com/chapters/14.html
  autocmd!
augroup END

" Select Leader key
let g:mapleader = "\<Space>"



"""""" Plugins """"""

" Install vim-plug (if not present)
" Reference: https://github.com/junegunn/vim-plug/wiki/tips
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd vimrc VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Installation directory for vim-plug plugins
call plug#begin(stdpath('data') . '/plugged')


""" Useful keybingings """

" Make repeat command (.) plugin compatible
Plug 'tpope/vim-repeat'

" Jump to any forward (s__) or backward (S__) location specified by two characters
Plug 'justinmk/vim-sneak'

" Highlight a unique character in every word when using f/F
Plug 'unblevable/quick-scope'

" Add (ys_), change (cs_), remove (ds_) surrounding delimiters (_ss for whole line)
Plug 'tpope/vim-surround'

" Comment out lines (gcc) or comment out with motions (gc_) or selections (gc)
Plug 'tpope/vim-commentary'

" Useful [_, ]_ keybindings: b (change buffers), Space (add blank lines),
" e (exchange line), navigate quickfix (q/Q) and location (l/L) lists;
" Paste after (]p) or before ([p) linewise, also increasing (>_) or
" decreasing (<_) indentation or reindenting (=_), after (_p) or before (_P)
" linewise (e.g. >p, <P, =P);
" Toggle common options: _oh (hlsearch), _oi (ignorecase), _ow (wrap)
Plug 'tpope/vim-unimpaired'

" Text exchange operator: cx_, cxx (current line), X (in visual mode),
" cxc (clear pending exchanges)
Plug 'tommcdo/vim-exchange'

" Coerce text cases with: crs (snake_case), crm (MixedCase), crc (camelCase),
" cru (UPPER_CASE), cr- (dash-case), cr. (dot.case), cr<space> (space case),
" crt (Title Case)
Plug 'tpope/vim-abolish'


""" Editing helps """

" Insert and delete brackets, parenthesis and quotes in pairs
Plug 'Raimondi/delimitMate'

" Align text vertically (e.g. :Tab /=)
" Cheatsheet: https://devhints.io/tabular
Plug 'godlygeek/tabular'


""" Language support plugins """

" Syntax highlighting for several languages
Plug 'sheerun/vim-polyglot'

" Syntax checking
Plug 'vim-syntastic/syntastic'


""" Yank management """

" Maintain history of yanks
Plug 'svermeulen/vim-yoink'

" Prevent delete operations (c, cc, C, d, dd, D, x, X) from yanking
Plug 'svermeulen/vim-cutlass'


""" Commands """

" Shell commands :Delete, :Move, :Rename, :Mkdir, :Chmod, :Wall (save all)
Plug 'tpope/vim-eunuch'

" Git support (:Git)
Plug 'tpope/vim-fugitive'

" Fuzzy find :Files, :GFiles (git files), :Buffers, :Colors, :Lines, :Marks,
" :Windows, :History (old files), :History: (commands), :History/ (search),
" :Commits, :Commands, :Maps
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-rooter'


""" Windows and themes """

" Open scratch buffer window with gs and empty buffer with gS
Plug 'mtth/scratch.vim'

" Start screen
Plug 'mhinz/vim-startify'

" Display a vim tip at startup
Plug 'michaelb/vim-tips'

" Status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Show a git diff in the sign column
Plug 'airblade/vim-gitgutter'

" Nerdtree
Plug 'preservim/nerdtree'

" Add icons
Plug 'ryanoasis/vim-devicons'

" gruvbox theme
Plug 'sainnhe/gruvbox-material'


""" Check later """

" Kick off builds and test suites using asynchronous adapters (e.g. tmux)
" https://github.com/tpope/vim-dispatch
" https://github.com/preservim/vimux
" https://github.com/jpalardy/vim-slime

" vim session manager
" https://github.com/tpope/vim-obsession
" https://github.com/dhruvasagar/vim-prosession

" Auto close
" https://github.com/cohama/lexima.vim
" https://github.com/tmsvg/pear-tree
" https://github.com/alvan/vim-closetag

" Switch between single and multi line statements
" https://github.com/AndrewRadev/splitjoin.vim

" LSP support
" https://github.com/neoclide/coc.nvim

" Initialize plugin system
call plug#end()



"""""" Theme settings """"""

set termguicolors

let g:gruvbox_material_background = 'hard'
let g:gruvbox_material_palette = 'original'
let g:gruvbox_material_transparent_background = 0
colorscheme gruvbox-material

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme = 'gruvbox_material'



"""""" Misc settings """"""

" Enable mouse support in all modes
set mouse=a

" Use * and/or + clipboard registers for yank and put operations
" Primary selection: "* register / unnamed
" Sytem clipboard: "+ register / unnamedplus
set clipboard=unnamedplus

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

" Open new split panes to right and bottom
set splitbelow
set splitright



"""""" Plugin settings """"""

" Enable sneak labels when moving
let g:sneak#label = 1
" Move to next match using s
let g:sneak#s_next = 1

" Only trigger quick-scope when pressing f or F
let g:qs_highlight_on_keys = ['f', 'F']

" Make delimitMate ignore double quotes (") on vim files
autocmd vimrc FileType vim let b:delimitMate_quotes = "' `"

" Sync numbered :registers with yank history
let g:yoinkSyncNumberedRegisters = 1

" Necessary for Yoink/Cutlass integration
let g:yoinkIncludeDeleteOperations = 1

" Scratch buffer window autohide
let g:scratch_autohide = 1
let g:scratch_insert_autohide = 0



"""""" Key mappings """"""

" Yank from cursor to end of line (by default Y is synonym to yy)
nnoremap Y y$


" Syntastic settings
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_shell = '/bin/sh'

" vimt reference: https://github.com/Vimjas/vint/wiki/Vint-linting-policy-summary
let g:syntastic_vim_checkers = ['vint']

let g:syntastic_python_checkers = ['pylint']


" Map DelimitMateSwitch
nnoremap <Leader>d :DelimitMateSwitch<CR>


" Map Yoink commands
nnoremap <Leader>y :Yanks<CR>
nmap [h <Plug>(YoinkRotateBack)
nmap ]h <Plug>(YoinkRotateForward)


" Cutlass cut operation remaps

" In visual mode just use x for cut
xnoremap x d

" Map delete-and-yank (cut) operations (normal mode)
nnoremap dy d
nnoremap dyy dd
nnoremap dY D
nnoremap yd d
nnoremap ydd dd
nnoremap yD D

" Map change-and-yank operations (normal mode)
nnoremap cy c
nnoremap cyy cc
nnoremap cY C
nnoremap yc c
nnoremap ycc cc
nnoremap yC C


" Map fzf file search
nnoremap <Leader>f :Files<CR>


" Map NERDTreeToggle
nnoremap <Leader>t :NERDTreeToggle<CR>

