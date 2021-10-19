scriptencoding utf-8


""""" General """""

" Define vimrc autocommand group.
augroup vimrc
  " Remove all previously set vimrc autocommands when (re)sourcing this file.
  " Reference: https://learnvimscriptthehardway.stevelosh.com/chapters/14.html
  autocmd!
augroup END

" Select Leader keys.
let g:mapleader      = "\<Space>"
let g:maplocalleader = '\\'

" Set g:os variable with current OS.
if !exists('g:os')
  if has('win64') || has('win32') || has('win16')
    let g:os = 'Windows'
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
" TODO: evaluate hop.nvim

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

" Sublime Text-like multiple cursor editing.
" To activate, select words with M-d, characters with S-Arrows or create cursors
" vertically with C-Up/C-Down. Use n/N to get more occurrences and [/] to navigate
" between selections. Press q to skip current occurrence and get the next one
" and Q to remove current selection. Start insert mode with i, a or c.
Plug 'mg979/vim-visual-multi'

" Insert and delete brackets, parenthesis and quotes in pairs.
Plug 'Raimondi/delimitMate'
" TODO: evaluate alternatives (e.g. https://github.com/windwp/nvim-autopairs)

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

" Highlight yanked region.
Plug 'machakann/vim-highlightedyank'


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

" Indentation level object: ii (indentation level), ai (ii and line above),
" aI (ii with lines above/below).
Plug 'michaeljsmith/vim-indent-object'


""" Language support """

" Syntax highlighting for several languages.
Plug 'sheerun/vim-polyglot'
" TODO: evaluate treesitter

" Syntax checking.
if g:os !=# 'Windows'
  Plug 'vim-syntastic/syntastic'
endif
" TODO: profile syntastic

" Code formatting.
Plug 'sbdchd/neoformat'

" LSP configuration.
Plug 'neovim/nvim-lspconfig'

" LSP completion.
Plug 'ms-jpq/coq_nvim', {'branch': 'coq'}


""" Terminal, shell and file management support """

" Send code to REPL: open window (g<CR>), send motion in normal mode (gr_)
" or visual mode (gr), send previous region (grp), send line (grr) and send
" buffer (gr<CR>).
Plug 'urbainvaes/vim-ripple'

" Shell commands :Delete, :Move, :Rename, :Mkdir, :Chmod, :Wall (save all).
Plug 'tpope/vim-eunuch'

" Vinegar-like path navigator. Use - to open, gq to quit and g? for help.
" Go to parent directory (-), reload (R), open file at cursor (i) or selected
" (I), show file info (K), preview file at cursor (p), next (C-n), previous
" (C-p) and go to home (~).
Plug 'justinmk/vim-dirvish'
" File manipulation commands for dirvish. Create file (a), directory (A),
" delete (dd), rename (r), yank (yy), copy (pp) and move (PP).
Plug 'roginfarrer/vim-dirvish-dovish'


""" Git integration """

" Git support (:Git).
Plug 'tpope/vim-fugitive'

" Show a git diff in the sign column.
Plug 'airblade/vim-gitgutter'


""" Search commands """

" Fuzzy find :Files, :GFiles (git files), :Buffers, :Colors, :Lines, :Marks,
" :Windows, :History (old files), :History: (commands), :History/ (search),
" :Commits, :Commands, :Maps.
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-rooter'
" TODO: evaluate telescope


""" Windows and themes """

" Show start screen.
Plug 'mhinz/vim-startify'
" TODO: evaluate nvim alternative

" Display a vim tip at startup.
Plug 'michaelb/vim-tips'

" Display popup with key bindings.
Plug 'folke/which-key.nvim'

" Status line.
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Add icons.
Plug 'ryanoasis/vim-devicons'

" Color highlighter.
Plug 'norcalli/nvim-colorizer.lua'

" Fade inactive buffers.
Plug 'TaDaa/vimade'

" Color themes.
" - base16
Plug 'chriskempson/base16-vim'
" - edge
Plug 'sainnhe/edge'
" - everforest
Plug 'sainnhe/everforest'
" - gruvbox
Plug 'gruvbox-community/gruvbox'
" - gruvbox-material
Plug 'sainnhe/gruvbox-material'
" - nord
Plug 'arcticicestudio/nord-vim'
" - onedark
Plug 'joshdick/onedark.vim'
" - palenight
Plug 'drewtempelmeyer/palenight.vim'
" - sonokai
Plug 'sainnhe/sonokai'
" - tender
Plug 'jacoborus/tender.vim'

" Theme integration between vim airline and tmux status line.
if executable('tmux')
  Plug 'edkolev/tmuxline.vim'
endif


" Initialize plugin system.
call plug#end()



"""""" Theme settings """"""

" Enable 24-bit RGB colors in terminal mode.
set termguicolors

" Set theme properties.
" - edge
let g:edge_background = 'hard'
" - everforest
let g:everforest_background = 'hard'
" - gruvbox
let g:gruvbox_italic = 1
let g:gruvbox_contrast_dark = 'hard'
" - gruvbox-material
let g:gruvbox_material_background = 'hard'
" - onedark
let g:onedark_terminal_italics = 1
" - palenight
let g:palenight_terminal_italics = 1
" - sonokai
let g:sonokai_background = 'hard'

" Set vim-airline properties.
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#tab_nr_type = 1

" Load default color scheme.
if g:os ==# 'Linux'
  colorscheme sonokai
elseif g:os ==# 'Windows'
  colorscheme palenight
endif


" Use tmuxline to configure tmux's status line.
" References:
" - https://github.com/wfxr/tmux-power
" - https://github.com/tmux-plugins/tmux-prefix-highlight
if executable('tmux')
  let g:tmuxline_preset = {
    \'a'    : '',
    \'b'    : ' #(whoami)@#h',
    \'c'    : ' #S',
    \'win'  : '#I:#W#F',
    \'cwin' : '#I:#W#F',
    \'x'    : '#{?client_prefix,#[reverse]prefix#[noreverse],#{?pane_in_mode,  #[reverse]copy#[noreverse],       }}',
    \'y'    : ' %R',
    \'z'    : ' %a %F'
  \}
endif


"""""" Misc settings """"""

" Use ':set option?' to check current option value.
" Use ':verbose set option?' to check where it was set.

" Buffers become hidden when abandoned.
set hidden
" TODO: evaluate how to avoid exiting without confirmation when many buffers are open (e.g. :q!)

" Raise dialog when quitting changed buffer.
set confirm

" Enable mouse support in all modes.
set mouse=a

" Use * and/or + clipboard registers for yank and put operations.
" Primary selection: "* register / unnamed
" Sytem clipboard:   "+ register / unnamedplus
set clipboard=unnamedplus

" Do not redraw screen while executing macros.
set lazyredraw

" Lines with equal indent form a fold.
set foldmethod=indent
set foldnestmax=4
set foldlevel=2

" Keep 5 lines above or below the cursor when scrolling.
set scrolloff=5

" Keep 5 columns to the left or to the right of the cursor.
set sidescrolloff=5

" Highlight line under cursor. It helps with navigation.
set cursorline
let cul_blacklist = ['terminal']
autocmd vimrc WinEnter * if index(cul_blacklist, &buftype) < 0 | setlocal cursorline
autocmd vimrc WinLeave * if index(cul_blacklist, &buftype) < 0 | setlocal nocursorline

" Highlight column 80. It helps identifying long lines.
set colorcolumn=80
let cc_blacklist = ['nofile']
autocmd vimrc WinEnter * if index(cc_blacklist, &buftype) < 0 | setlocal colorcolumn+=80
autocmd vimrc WinLeave * if index(cc_blacklist, &buftype) < 0 | setlocal colorcolumn-=80

" Print the line number in front of each line.
set number

" Show the line number relative to the line with the cursor in front of each line.
set relativenumber
let rnu_blacklist = ['help', 'terminal']
autocmd vimrc WinEnter * if index(rnu_blacklist, &buftype) < 0 | setlocal relativenumber
autocmd vimrc WinLeave * if index(rnu_blacklist, &buftype) < 0 | setlocal norelativenumber

" Open new split panes to right and bottom.
set splitbelow
set splitright

" Ignore case in patterns (unless upper case characters are used).
set ignorecase
set smartcase

" Disable line wrapping.
set nowrap

" List all matches and complete till longest common string.
set wildmode=list:longest

" File patterns to ignore.
set wildignore+=*.swp
set wildignore+=*.zip,*.7z,*.tar,*.gz
set wildignore+=*.pdf
set wildignore+=*.doc,*.docx,*.xls,*.xslx,*.ppt,*.pptx
set wildignore+=*.png,*.jpg,*.gif
set wildignore+=*.pyc,*.pyo,*.pyd

" Disable numbering and cursor highlighting in terminal buffers.
autocmd vimrc TermOpen * setlocal nonumber norelativenumber nocursorline



"""""" Plugin settings """"""

" Disable netrw.
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1

" Enable sneak labels when moving.
let g:sneak#label = 1

" Disable quick-scope highlighting for certain buffers and file types.
let g:qs_buftype_blacklist = ['terminal', 'nofile', 'help']
let g:qs_filetype_blacklist = ['startify', 'fugitive']

" Add underline to quick-scope highlighted characters.
" References:
" - https://github.com/unblevable/quick-scope
" - https://stackoverflow.com/questions/18774910/how-to-partially-link-highlighting-groups
execute 'highlight QuickScopePrimary gui=underline cterm=underline' .
  \' guifg='   . synIDattr(synIDtrans(hlID('Function')), 'fg', 'gui') .
  \' ctermfg=' . synIDattr(synIDtrans(hlID('Function')), 'fg', 'cterm')
execute 'highlight QuickScopeSecondary gui=underline cterm=underline' .
  \' guifg=' .   synIDattr(synIDtrans(hlID('Define')), 'fg', 'gui') .
  \' ctermfg=' . synIDattr(synIDtrans(hlID('Define')), 'fg', 'cterm')

" Visual Multi plugin key mappings.
let g:VM_maps = {}
let g:VM_maps['Find Under']         = '<M-d>'
let g:VM_maps['Find Subword Under'] = '<M-d>'

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
  " let g:syntastic_python_checkers = ['pylint']
endif

" Vim-ripple settings.
let g:ripple_enable_mappings = 0
let g:ripple_always_return = 1
let g:ripple_repls = {
  \ 'python': {
    \ 'command': 'ipython --profile=vi',
    \ 'pre': "\<esc>[200~",
    \ 'post': "\<esc>[201~",
    \ 'addcr': 1,
    \ }
  \ }

" Load lua plugins' settings.
execute 'luafile ' . stdpath('config') . '/config.lua'



"""""" Key mappings """"""

" - Used keys reference: :help index
" - Unused keys reference: https://vim.fandom.com/wiki/Unused_keys
" - Prefer non recursive maps (_noremap)
" - Plugin maps (<Plug>) must be recursive


" Yank from cursor to end of line (by default Y is synonym to yy).
nnoremap Y y$


" Use @p to paste with a space before the inserted text.
let @p="a \<Esc>p"


" Disable C-q (tmux prefix).
noremap  <C-q> <Nop>
lnoremap <C-q> <Nop>
tnoremap <C-q> <Nop>


" Clear last search highlighting (Esc is not mapped to anything in normal mode).
nnoremap <silent> <Esc> :noh<CR><Esc>


" Map DelimitMateSwitch.
" TODO: remap to something else
" nnoremap <Leader>d :DelimitMateSwitch<CR>


" CamelCaseMotion maps.
map <silent> <M-w> <Plug>CamelCaseMotion_w
map <silent> <M-b> <Plug>CamelCaseMotion_b
map <silent> <M-e> <Plug>CamelCaseMotion_e


" Map fzf search commands.
" TODO: remap to something else
" nnoremap <Leader>f :Files<CR>
" nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>l :Lines<CR>
nnoremap <Leader>c :Colors<CR>
nnoremap <Leader>/ :History/<CR>
nnoremap <Leader>: :History:<CR>


" Map vim-easy-align to gl (since ga is already used).
nmap gl <Plug>(EasyAlign)
xmap gl <Plug>(EasyAlign)


" Map vim-ripple commands.
nmap g<CR>  <Plug>(ripple_open_repl)
nmap gr     <Plug>(ripple_send_motion)
nmap gr<CR> <Plug>(ripple_send_buffer)
nmap grr    <Plug>(ripple_send_line)
nmap grp    <Plug>(ripple_send_previous)
xmap gr     <Plug>(ripple_send_selection)


" Window navigation mappings.
tnoremap <M-h> <C-\><C-n><C-w>h
tnoremap <M-j> <C-\><C-n><C-w>j
tnoremap <M-k> <C-\><C-n><C-w>k
tnoremap <M-l> <C-\><C-n><C-w>l
inoremap <M-h> <C-\><C-n><C-w>h
inoremap <M-j> <C-\><C-n><C-w>j
inoremap <M-k> <C-\><C-n><C-w>k
inoremap <M-l> <C-\><C-n><C-w>l
nnoremap <M-h> <C-w>h
nnoremap <M-j> <C-w>j
nnoremap <M-k> <C-w>k
nnoremap <M-l> <C-w>l


" Leader key mappings.
" TODO: add mappings

" o - open/options
" o -:  open dirvish
" o t:  open terminal
" o q:  open quickfix list
" o l:  open location list
" o s:  open saved sessions? - Startify
" o d:  toggle DelimitMateSwitch?
" o g:  toggle GoldenRatioResize? (https://github.com/roman/golden-ratio)

" v - vi
" v s:     save session? - Startify

" s - search
" s l:   fzf :Lines/:BLines
" s p:   search project fzf :Rg
" s sc:  clear search

" l - LSP
" format buffer
" lsp info, stop, start etc.
" go to definition, find references
" toggle showing errors/warnings
" TODO: trim lines when saving

" h - help
" options, keys, commands etc.
" h t:  tips

" q - quit
" q q:  quit - :confirm qall
" q s:  save session and quit

" g - git
" g p:   :Git push

