""""" General """""

" Define vimrc autocommand group.
augroup vimrc
  " Remove all previously set vimrc autocommands when (re)sourcing this file.
  " Reference: https://learnvimscriptthehardway.stevelosh.com/chapters/14.html
  autocmd!
augroup END

" Select Leader key.
let g:mapleader = "\<Space>"

" Set g:os variable with current OS.
if !exists("g:os")
  if has("win64") || has("win32") || has("win16")
    let g:os = "Windows"
  else
    let g:os = substitute(system('uname'), '\n', '', '')
  endif
endif



"""""" Plugins """"""

" Install vim-plug (if not present).
" Reference: https://github.com/junegunn/vim-plug/wiki/tips
let data_dir = stdpath('data') . '/site'
if empty(glob(data_dir . '/autoload/plug.vim'))
  let url = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  silent execute '!curl -fLo ' . data_dir . '/autoload/plug.vim --create-dirs ' . url
  autocmd vimrc VimEnter * PlugInstall --sync | source $MYVIMRC
endif


" Installation directory for vim-plug plugins.
call plug#begin(stdpath('data') . '/plugged')


""" Useful keybingings """

" Make repeat command (.) plugin compatible.
Plug 'tpope/vim-repeat'

" Jump to any forward (s__) or backward (S__) location specified by two characters.
Plug 'justinmk/vim-sneak'

" Highlight a unique character in every word when using f/F.
Plug 'unblevable/quick-scope'

" Add (ys_), change (cs_), remove (ds_) surrounding delimiters (_ss for whole line).
Plug 'tpope/vim-surround'

" Comment out lines (gcc) or comment out with motions (gc_) or selections (gc).
Plug 'tpope/vim-commentary'

" Useful [_, ]_ keybindings: b (change buffers), Space (add blank lines),
" e (exchange line), navigate quickfix (q/Q) and location (l/L) lists;
" Paste after (]p) or before ([p) linewise, also increasing (>_) or
" decreasing (<_) indentation or reindenting (=_), after (_p) or before (_P)
" linewise (e.g. >p, <P, =P);
" Toggle common options: _oh (hlsearch), _oi (ignorecase), _ow (wrap).
Plug 'tpope/vim-unimpaired'

" Text exchange operator: cx_, cxx (current line), X (in visual mode),
" cxc (clear pending exchanges).
Plug 'tommcdo/vim-exchange'

" Coerce text cases with: crs (snake_case), crm (MixedCase), crc (camelCase),
" cru (UPPER_CASE), cr- (dash-case), cr. (dot.case), cr<space> (space case),
" crt (Title Case).
Plug 'tpope/vim-abolish'

" Emacs keybindings in insert and command modes:
" C-b, C-f: back/forward character
" M-b, M-f: back/forward word
" C-a, C-e: beginning/end of line
" M-n, M-p: down/up line
" C-d, M-d: delete character/word
Plug 'tpope/vim-rsi'


""" Editing helps """

" Insert and delete brackets, parenthesis and quotes in pairs.
Plug 'Raimondi/delimitMate'

" Align text by some character or regex adding spaces to the left and/or right.
" 1. Type gl in visual mode, or gl followed by motion or text object in normal
"    mode to enter interactive mode.
" 2. Optionally enter keys to cycle between alignment options (e.g. <C-d> to
"    cycle between left, right or center alignment).
" 3. Optionally enter keys to define delimiter occurrences to consider (e.g.
"    2: second occurence, *: all occurences, -: last ocurrence).
" 4. Type delimiter key (one of " =:.|&#,", which have predefined rules) or an
"    arbitrary regex followed by <C-x>.
" 5. Alternatively, use the :EasyAlign command.
" Reference: https://github.com/junegunn/vim-easy-align
Plug 'junegunn/vim-easy-align'


""" Custom motions and text objects """

" CamelCase and snake_case motions.
Plug 'bkad/CamelCaseMotion'

" Several text objects with in (i), a (a), inside (I), around (A), next (_n)
" and last (_l) semantics.
" Pairs: () {} [] <> t (XML/HTML tags)
" Quotes: ' " `
" Separators: , . ; : + - = ~ _ * # / | \ & $
" Arguments: a (surrounded by braces and/or commas)
" Any Block: b (similar to pairs, but skips pairs in nested contexts)
" Any Quote: q (similar to quotes, but skips pairs in nested contexts)
" Reference: https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
Plug 'wellle/targets.vim'

" Indentation level object: ii (indentation level, ai (ii and line above),
" aI (ii with lines above/below).
Plug 'michaeljsmith/vim-indent-object'

" Provide operator motions to the beginning ([) or end (]) of text objects
" (e.g. d]ip deletes from the cursor to the end of the paragraph).
Plug 'tommcdo/vim-ninja-feet'


""" Language support plugins """

" Syntax highlighting for several languages.
Plug 'sheerun/vim-polyglot'

" Syntax checking.
if g:os != "Windows"
  Plug 'vim-syntastic/syntastic'
endif


""" Yank management """

" Show registers' contents when using ", @ or <C-r>.
Plug 'junegunn/vim-peekaboo'


""" Commands """

" Shell commands :Delete, :Move, :Rename, :Mkdir, :Chmod, :Wall (save all).
Plug 'tpope/vim-eunuch'

" Git support (:Git).
Plug 'tpope/vim-fugitive'

" Fuzzy find :Files, :GFiles (git files), :Buffers, :Colors, :Lines, :Marks,
" :Windows, :History (old files), :History: (commands), :History/ (search),
" :Commits, :Commands, :Maps.
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-rooter'


""" Windows and themes """

" Show start screen.
Plug 'mhinz/vim-startify'

" Display a vim tip at startup.
Plug 'michaelb/vim-tips'

" Status line.
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Show a git diff in the sign column.
Plug 'airblade/vim-gitgutter'

" Add icons.
Plug 'ryanoasis/vim-devicons'

" Color theme.
if g:os == "Linux"
  " gruvbox theme.
  Plug 'sainnhe/gruvbox-material'
elseif g:os == "Windows"
  " Use base16 colorschemes.
  Plug 'chriskempson/base16-vim'
endif


" Initialize plugin system.
call plug#end()



"""""" Theme settings """"""

set termguicolors

if g:os == "Linux"
  let g:gruvbox_material_background = 'hard'
  let g:gruvbox_material_palette = 'original'
  let g:gruvbox_material_transparent_background = 0
  colorscheme gruvbox-material
  let g:airline_theme = 'gruvbox_material'
elseif g:os == "Windows"
  colorscheme base16-tomorrow-night-eighties
  let g:airline_theme = 'base16_vim'
endif

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1



"""""" Misc settings """"""

" Enable mouse support in all modes.
set mouse=a

" Use * and/or + clipboard registers for yank and put operations.
" Primary selection: "* register / unnamed
" Sytem clipboard: "+ register / unnamedplus
set clipboard=unnamedplus

" Do not redraw screen while executing macros.
set lazyredraw

" Lines with equal indent form a fold.
set foldmethod=indent
set foldnestmax=4
set foldlevel=2
set foldcolumn=1

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

" Open new split panes to right and bottom.
set splitbelow
set splitright

" Ignore case in patterns (unless upper case characters are used).
set ignorecase
set smartcase

" List all matches and complete till longest common string.
set wildmode=list:longest



"""""" Plugin settings """"""

" Enable sneak labels when moving.
let g:sneak#label = 1
" Move to next match using s.
let g:sneak#s_next = 1

" Only trigger quick-scope when pressing f or F.
let g:qs_highlight_on_keys = ['f', 'F']

" Make delimitMate ignore double quotes (") on vim files.
autocmd vimrc FileType vim let b:delimitMate_quotes = "' `"

" Syntastic settings.
if exists('g:loaded_syntastic_plugin')
  let g:syntastic_always_populate_loc_list = 1
  let g:syntastic_auto_loc_list = 1
  let g:syntastic_check_on_open = 1
  let g:syntastic_check_on_wq = 0
  let g:syntastic_shell = '/bin/sh'
  " vimt reference: https://github.com/Vimjas/vint/wiki/Vint-linting-policy-summary
  let g:syntastic_vim_checkers = ['vint']
  let g:syntastic_python_checkers = ['pylint']
endif



"""""" Key mappings """"""

" - Unused keys reference: https://vim.fandom.com/wiki/Unused_keys
" - Prefer non recursive maps (_noremap)
" - Plugin maps (<Plug>) must be recursive


" Yank from cursor to end of line (by default Y is synonym to yy).
nnoremap Y y$


" Map DelimitMateSwitch.
nnoremap <Leader>d :DelimitMateSwitch<CR>


" CamelCaseMotion maps.
map <silent> <M-w> <Plug>CamelCaseMotion_w
map <silent> <M-b> <Plug>CamelCaseMotion_b
map <silent> <M-e> <Plug>CamelCaseMotion_e


" Map fzf file search.
nnoremap <Leader>f :Files<CR>


" Map vim-easy-align to gl (since ga is already used).
nmap gl <Plug>(EasyAlign)
xmap gl <Plug>(EasyAlign)

