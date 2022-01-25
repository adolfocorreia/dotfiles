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

" Disable external providers support to improve startup time.
let g:loaded_python_provider = 0
let g:loaded_python3_provider = 0
let g:loaded_ruby_provider = 0
let g:loaded_node_provider = 0
let g:loaded_perl_provider = 0

" Disable netrw.
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1



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


""" Dependency plugins """

" Common lua functions.
Plug 'nvim-lua/plenary.nvim'


""" Useful keybingings """

" Make repeat command (.) plugin compatible.
Plug 'tpope/vim-repeat'

" Jump to any forward (s__) or backward (S__) location specified by two characters.
" In case of multiple targets, a third character (label) can be used.
Plug 'ggandor/lightspeed.nvim'

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

" TODO: evaluate this better
" Insert and delete brackets, parenthesis and quotes in pairs.
Plug 'windwp/nvim-autopairs'
" Note: nvim-autopairs backspace map is incompatible with vim-visual-multi
" Evaluate 'LunarWatcher/auto-pairs' since it might be compatible

" Reopen files at last edit position.
Plug 'farmergreg/vim-lastplace'

" TODO: add key mappings
" Whitespace highlighting and removal.
Plug 'ntpeters/vim-better-whitespace'


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

" TODO: evaluate treesitter text objects plugin.


""" Language support """

" Syntax highlighting, indentation, folding and more using ASTs.
Plug 'nvim-treesitter/nvim-treesitter', {'do': 'TSUpdate'}

" TODO: evaluate if sleuth is really necessary with treesitter
" TODO: consider using expandtab, tabstop, softtabstop, shiftwidth explicitly
" Automatic tab/indenting configuration.
Plug 'tpope/vim-sleuth'

" LSP configuration.
Plug 'neovim/nvim-lspconfig'

" LSP completion.
Plug 'ms-jpq/coq_nvim', {'branch': 'coq'}

" TODO: evaluate null-ls plugin
" Inject LSP diagnostics, code actions and more from non-LSP tools.
Plug 'jose-elias-alvarez/null-ls.nvim'


" Language plugins.
" Reference: https://github.com/sheerun/vim-polyglot#language-packs

" Julia. LaTeX to Unicode substitutions.
Plug 'JuliaEditorSupport/julia-vim'


""" Terminal and file management support """

" Send code to REPL: send motion in normal mode (gr_) or visual mode (gr),
" send line (grr) and send file (grR).
Plug 'kassio/neoterm'

" Vinegar-like path navigator. Use - to open, gq to quit and g? for help.
" Go to parent directory (-), reload (R), open file at cursor (i) or selected
" (I), show file info (K), preview file at cursor (p), next (C-n), previous
" (C-p) and go to home (~).
Plug 'justinmk/vim-dirvish'
" File manipulation commands for dirvish. Create file (a), directory (A),
" delete (dd), rename (r), yank (yy), copy (pp) and move (PP).
Plug 'roginfarrer/vim-dirvish-dovish'


""" Git integration """

" TODO: evaluate neogit
" Git support (:Git).
Plug 'tpope/vim-fugitive'
" Plug 'TimUntersberger/neogit'

" Show a git diff in the sign column.
Plug 'lewis6991/gitsigns.nvim'


""" Search commands """

" TODO: evaluate telescope
" Fuzzy find :Files, :GFiles (git files), :Buffers, :Colors, :Lines, :Marks,
" :Windows, :History (old files), :History: (commands), :History/ (search),
" :Commits, :Commands, :Maps.
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-rooter'


""" Windows and themes """

" TODO: evaluate dashboard-nvim
" Show start screen.
Plug 'mhinz/vim-startify'

" Display popup with key bindings.
Plug 'folke/which-key.nvim'

" Status line.
Plug 'nvim-lualine/lualine.nvim'

" Add icons.
Plug 'kyazdani42/nvim-web-devicons'

" Delete buffers without losing window layout.
Plug 'famiu/bufdelete.nvim'

" List for showing diagnostics, references, search results, quickfix and
" location lists.
Plug 'folke/trouble.nvim'

" Color highlighter.
Plug 'norcalli/nvim-colorizer.lua'

" Treesitter supported colorschemes:
" - https://github.com/nvim-treesitter/nvim-treesitter/wiki/Colorschemes
" - https://github.com/rockerBOO/awesome-neovim#tree-sitter-supported-colorscheme

" Tokyo Night
Plug 'folke/tokyonight.nvim'


""" Neovim management """

" Startup profiling.
Plug 'dstein64/vim-startuptime'


" Initialize plugin system.
call plug#end()



"""""" Theme settings """"""

" Enable 24-bit RGB colors in terminal mode.
set termguicolors

" Set theme properties.
let g:tokyonight_style = 'storm'
let g:tokyonight_lualine_bold = 1

" Load default color scheme.
colorscheme tokyonight


"""""" Misc settings """"""

" Use ':set option?' to check current option value.
" Use ':verbose set option?' to check where it was set.

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
set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
set foldnestmax=4
set foldlevel=2

" Keep 5 lines above or below the cursor when scrolling.
set scrolloff=5

" Keep 5 columns to the left or to the right of the cursor.
set sidescrolloff=5

" TODO: organize commands below as functions (in lua?)
" Highlight line and column under cursor. It helps with navigation.
" set cursorline
" set cursorcolumn
let cul_cuc_blacklist = ['terminal']
autocmd vimrc BufWinEnter * if index(cul_cuc_blacklist, &buftype) < 0 | setlocal cul   | setlocal cuc   | endif
autocmd vimrc WinEnter    * if index(cul_cuc_blacklist, &buftype) < 0 | setlocal cul   | setlocal cuc   | endif
autocmd vimrc WinLeave    * if index(cul_cuc_blacklist, &buftype) < 0 | setlocal nocul | setlocal nocuc | endif

" Highlight column 80. It helps identifying long lines.
" set colorcolumn=80
let cc_blacklist = ['help', 'nofile', 'nowrite', 'terminal']
autocmd vimrc BufWinEnter * if index(cc_blacklist, &buftype) < 0 | setlocal colorcolumn+=80
autocmd vimrc WinEnter    * if index(cc_blacklist, &buftype) < 0 | setlocal colorcolumn+=80
autocmd vimrc WinLeave    * if index(cc_blacklist, &buftype) < 0 | setlocal colorcolumn-=80

" Show the line number relative to the cursor in front of each line and the
" absolute line number for the one with the cursor.
" set number
" set relativenumber
let nu_rnu_blacklist = ['help', 'nofile', 'nowrite', 'terminal']
autocmd vimrc BufWinEnter * if index(nu_rnu_blacklist, &buftype) < 0 | setlocal nu   | setlocal rnu   | endif
autocmd vimrc WinEnter    * if index(nu_rnu_blacklist, &buftype) < 0 | setlocal nu   | setlocal rnu   | endif
autocmd vimrc WinLeave    * if index(nu_rnu_blacklist, &buftype) < 0 | setlocal nonu | setlocal nornu | endif

" Disable numbering and cursor highlighting in terminal buffers.
autocmd vimrc TermOpen * setlocal nonumber norelativenumber nocursorline nocursorcolumn


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

" Highlight yanked region.
autocmd vimrc TextYankPost * silent! lua vim.highlight.on_yank{timeout=500}



"""""" Plugin settings """"""

" Disable quick-scope highlighting for certain buffers and file types.
let g:qs_buftype_blacklist = ['terminal', 'nofile', 'help']
let g:qs_filetype_blacklist = ['startify', 'fugitive']

" Add underline to quick-scope highlighted characters.
" References:
" - https://github.com/unblevable/quick-scope
" - https://stackoverflow.com/questions/18774910/how-to-partially-link-highlighting-groups
execute 'highlight QuickScopePrimary gui=underline' .
  \' guifg='   . synIDattr(synIDtrans(hlID('ErrorMsg')), 'fg', 'gui')
execute 'highlight QuickScopeSecondary gui=underline' .
  \' guifg='   . synIDattr(synIDtrans(hlID('WarningMsg')), 'fg', 'gui')

" Visual Multi plugin key mappings.
let g:VM_maps = {}
let g:VM_maps['Find Under']         = '<M-d>'
let g:VM_maps['Find Subword Under'] = '<M-d>'

" vim-better-whitespace settings.
execute 'highlight ExtraWhitespace' .
  \' guibg=' . synIDattr(synIDtrans(hlID('Error')), 'fg', 'gui')

" neoterm settings.
let g:neoterm_default_mod = 'vertical'
let g:neoterm_direct_open_repl = 1
let g:neoterm_repl_python = ['ipython --profile=vi']
let g:neoterm_repl_enable_ipython_paste_magic = 1
" let g:neoterm_bracketed_paste = 1
if g:os ==# 'Linux'
  let g:neoterm_shell = 'bash'
endif

" Load lua plugins' settings.
execute 'luafile ' . stdpath('config') . '/config.lua'



"""""" Key mappings """"""

" - Used keys reference: :help index
" - Unused keys reference: https://vim.fandom.com/wiki/Unused_keys
" - Prefer non recursive maps (_noremap)
" - Plugin maps (<Plug>) must be recursive


" Auto center on matched string.
noremap n nzzzv
noremap N Nzzzv


" Use @p to paste with a space before the inserted text.
let @p="a \<Esc>p"


" Add big j/k jumps to jumplist.
nnoremap <expr> j (v:count >= 10 ? "m'" . v:count : "") . 'j'
nnoremap <expr> k (v:count >= 10 ? "m'" . v:count : "") . 'k'


" Insert blank line above or below (in insert mode).
inoremap <C-j> <C-o>o
inoremap <C-k> <C-o>O


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


" Map vim-easy-align to gl (since ga is already used).
nmap gl <Plug>(EasyAlign)
xmap gl <Plug>(EasyAlign)


" Map neoterm commands.
xmap gr <Plug>(neoterm-repl-send)
nmap gr <Plug>(neoterm-repl-send)
nmap grr <Plug>(neoterm-repl-send-line)
nmap <silent> grR :TREPLSendFile<CR>


" Map vim-better-whitespace commands.
nnoremap <silent> ]w :NextTrailingWhitespace<CR>
nnoremap <silent> [w :PrevTrailingWhitespace<CR>


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
" o s:  open saved sessions? - Startify
" o d:  toggle DelimitMateSwitch?
" o g:  toggle GoldenRatioResize? (https://github.com/roman/golden-ratio)

" v - vi
" v s:     save session? - Startify

" l - LSP
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

