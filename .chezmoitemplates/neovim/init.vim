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

" Provide operator motions to the beginning ([) or end (]) of text objects
" (e.g. d]ip deletes from the cursor to the end of the paragraph).
Plug 'tommcdo/vim-ninja-feet'


""" Language support """

" Syntax highlighting for several languages.
Plug 'sheerun/vim-polyglot'

" Syntax checking.
if g:os !=# 'Windows'
  Plug 'vim-syntastic/syntastic'
endif
" TODO: profile syntastic
" TODO: evaluate treesitter

" Code formatting.
Plug 'sbdchd/neoformat'

" LSP configuration.
Plug 'neovim/nvim-lspconfig'

" LSP completion.
Plug 'nvim-lua/completion-nvim'


""" External process interaction """

" Send code to REPL: open window (y<CR>), send motion in normal mode (yr_)
" or visual mode (R), send previous region (yp), send line (yrr) and send
" buffer (yr<CR>).
Plug 'urbainvaes/vim-ripple'
" TODO: change mappings to gr_


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
" TODO: evaluate telescope


""" Windows and themes """

" Show start screen.
Plug 'mhinz/vim-startify'

" Display a vim tip at startup.
Plug 'michaelb/vim-tips'

" Vinegar-like path navigator. Use - to open, gq to quit and g? for help.
" Go to parent directory (-), reload (R), open file at cursor (i) or selected
" (I), show file info (K), preview file at cursor (p), next (C-n), previous
" (C-p) and go to home (~).
Plug 'justinmk/vim-dirvish'
" File manipulation commands for dirvish. Create file (a), directory (A),
" delete (dd), rename (r), yank (yy), copy (pp) and move (PP).
Plug 'roginfarrer/vim-dirvish-dovish'

" Status line.
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Show a git diff in the sign column.
Plug 'airblade/vim-gitgutter'

" Add icons.
Plug 'ryanoasis/vim-devicons'

" Color highlighter.
Plug 'norcalli/nvim-colorizer.lua'

" Color theme.
if g:os ==# 'Linux'
  " gruvbox theme.
  Plug 'sainnhe/gruvbox-material'
elseif g:os ==# 'Windows'
  " Use onedark theme.
  Plug 'joshdick/onedark.vim'
endif

" TODO: evaluate 'TaDaa/vimade'


" Initialize plugin system.
call plug#end()



"""""" Theme settings """"""

set termguicolors

if g:os ==# 'Linux'
  let g:gruvbox_material_background = 'hard'
  let g:gruvbox_material_palette = 'original'
  let g:gruvbox_material_transparent_background = 0
  colorscheme gruvbox-material
  let g:airline_theme = 'gruvbox_material'
elseif g:os ==# 'Windows'
  let g:onedark_terminal_italics = 1
  colorscheme onedark
  let g:airline_theme = 'onedark'
endif

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

" TODO: improve this
" " Set colors for non-current windows (NormalNC highlight group).
" exec 'highlight NormalNC' .
"   \' guibg='   . synIDattr(synIDtrans(hlID('ColorColumn')), 'bg', 'gui') .
"   \' ctermbg=' . synIDattr(synIDtrans(hlID('ColorColumn')), 'bg', 'cterm') .
"   \' guifg='   . synIDattr(synIDtrans(hlID('Normal')),      'fg', 'gui') .
"   \' ctermfg=' . synIDattr(synIDtrans(hlID('Normal')),      'fg', 'cterm')



"""""" Misc settings """"""

" Use ':set option?' to check current option value.
" Use ':verbose set option?' to check where it was set.

" Buffers become hidden when abandoned.
set hidden
" TODO: evaluate how to avoid exiting without saving (e.g. :q!)
set confirm

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

" Keep 5 lines above or below the cursor when scrolling.
set scrolloff=5

" Keep 5 columns to the left or to the right of the cursor.
set sidescrolloff=5

" Highlight line and column under cursor. It helps with navigation.
set cursorline
autocmd vimrc WinEnter * setlocal cursorline
autocmd vimrc WinLeave * setlocal nocursorline

" Highlight column 80. It helps identifying long lines.
set colorcolumn=80

" Print the line number in front of each line.
set number

" Show the line number relative to the line with the cursor in front of each line.
set relativenumber
let rnu_blacklist = ['terminal']
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

" Disable numbering in terminal buffers.
autocmd vimrc TermOpen * setlocal nonumber norelativenumber



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
exec 'highlight QuickScopePrimary gui=underline cterm=underline' .
  \' guifg='   . synIDattr(synIDtrans(hlID('Function')), 'fg', 'gui') .
  \' ctermfg=' . synIDattr(synIDtrans(hlID('Function')), 'fg', 'cterm')
exec 'highlight QuickScopeSecondary gui=underline cterm=underline' .
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
  let g:syntastic_python_checkers = ['pylint']
endif

" Vim-ripple settings.
let g:ripple_always_return = 1
let g:ripple_repls = {
  \ 'python': {
    \ 'command': 'ipython --profile=vi',
    \ 'pre': "\<esc>[200~",
    \ 'post': "\<esc>[201~",
    \ 'addcr': 1,
    \ }
  \ }

" nvim-colorizer settings.
lua require'colorizer'.setup()



"""""" LSP settings """"""

" Completion.
set completeopt=menuone,noinsert,noselect
set shortmess+=c

" Python
lua require'lspconfig'.pyright.setup{on_attach=require'completion'.on_attach}



"""""" Key mappings """"""

" - Used keys reference: :help index
" - Unused keys reference: https://vim.fandom.com/wiki/Unused_keys
" - Prefer non recursive maps (_noremap)
" - Plugin maps (<Plug>) must be recursive


" Yank from cursor to end of line (by default Y is synonym to yy).
nnoremap Y y$


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
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>l :Lines<CR>


" Map vim-easy-align to gl (since ga is already used).
nmap gl <Plug>(EasyAlign)
xmap gl <Plug>(EasyAlign)


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
" TODO: add bindings to which-keys plugin

" w - windows
" w hjkl:      window switching
" w cd:        close/delete window
" w o:         only window
" w vs\-VS_|:  window splitting
" w =:         make windows equal in size
" w HJKL:      swapping windows?
" w w:         next window
" w T:         move buffer to new tab
" w rR:        rotate windows
" w <>+-:      increase/decrease window

" b - buffers
" b b:       fzf :Buffers
" b np:      next/previous buffer
" b cd:      close buffer
" b ws:      write/save buffer
" b x:       open scratch buffer (e.g. :enew or /tmp/xxx)
" b m:       open messages buffer

" t - tabs
" t ne:      new edit tab
" t cd:      tabclose
" t 123...:  go to tab
" t o:       tab only
" t m:       tab move

" f - files
" f f:  fzf :Files
" f g:  fzf :GFiles
" f h:  fzf :History

" o - open/options
" o -:  open dirvish
" o t:  open terminal
" o q:  open quickfix list
" o l:  open location list
" o r:  open registries
" o s:  open saved sessions?
" o d:  toggle DelimitMateSwitch?
" o g:  toggle GoldenRatioResize? (https://github.com/roman/golden-ratio)

" v - vi
" v v:     open vimrc/init.vim
" v r:     reload vimrc
" v ucig:  plug update/clean/install/upgrade
" v h:     startify/home
" v m:     fzf :Maps
" v s:     save session?

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
" q q:  quit
" q s:  save session and quit

" g - git
" g gs:  :Git (status)
" g p:   :Git push

