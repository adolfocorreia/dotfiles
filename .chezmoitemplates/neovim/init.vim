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
let g:loaded_netrwSettings = 1
let g:loaded_netrwFileHandlers = 1



"""""" Plugins """"""

" Install vim-plug (if not present).
" Reference: https://github.com/junegunn/vim-plug/wiki/tips
let data_dir = stdpath('data') . '/site'
if empty(glob(data_dir . '/autoload/plug.vim'))
  let url = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  silent execute '!curl -fLo ' . data_dir . '/autoload/plug.vim --create-dirs ' . url
  autocmd vimrc VimEnter * PlugInstall --sync | source $MYVIMRC
endif


" TODO: convert to packer
" Installation directory for vim-plug plugins.
call plug#begin(stdpath('data') . '/plugged')


""" Dependency plugins """

" Common lua functions.
Plug 'nvim-lua/plenary.nvim'

" Custom user text objects.
Plug 'kana/vim-textobj-user'


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

" TODO: evaluate this better
" Comment out lines (gcc) or comment out with motions (gc_) or selections (gc).
" Use gb_ for block comments.
Plug 'numToStr/Comment.nvim'

" Useful [_, ]_ keybindings: b (change buffers), Space (add blank lines),
" e (exchange line), navigate quickfix (q/Q) and location (l/L) lists;
" Paste after (]p) or before ([p) linewise.
" Toggle common options: _oh (hlsearch), _oi (ignorecase), _ol (list tabs and
" trailing spaces), _on (number), _or (relativenumber), _ov (virtualedit),
" _ow (wrap), _ox (cursorline and cursorcolumn), _oz (spell).
" Also _od for :diffthis and :diffoff.
Plug 'tpope/vim-unimpaired'

" Text exchange operator: cx_, cxx (current line), X (in visual mode),
" cxc (clear pending exchanges).
Plug 'tommcdo/vim-exchange'

" Coerce text cases with: crs (snake_case), crm (MixedCase), crc (camelCase),
" cru (UPPER_CASE), cr- (dash-case), cr. (dot.case), cr<space> (space case),
" crt (Title Case).
" Search and replace words with with variants like plural and case. Use e.g.
" :%S/facilit{y,ies}/building{,s}/gc
Plug 'tpope/vim-abolish'

" Emacs keybindings in insert and command modes:
" C-b, C-f: back/forward character
" M-b, M-f: back/forward word
" C-a, C-e: beginning/end of line
" M-n, M-p: down/up line
" C-d, M-d: delete character/word
Plug 'tpope/vim-rsi'

" TODO: evaluate this and svermeulen/vim-subversive better
" Replace text object (<M-s>), line (<M-s><M-s>) or to end of line (<M-S) with
" contents of the unnamed register "" (e.g. <M-s>iw replaces the word under
" the cursor with the current yank).
Plug 'gbprod/substitute.nvim'

" TODO: evaluate monaqa/dial.nvim
" Use C-a/C-x to increment/decrement dates, times, roman numerals and ordinals
" (e.g. 1st, 2nd, 3rd). For letters of the alphabet, use linewise visual
" selection on empty lines.
Plug 'tpope/vim-speeddating'


""" Editing helps """

" Sublime Text-like multiple cursor editing.
" To activate, select words with M-d or create cursors vertically with C-j/C-k.
" Use C-h/C-l to add characters to the selection and o to go to the other side
" of the selection. Use n/N to get more occurrences and [/] to navigate
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

" TODO: evaluate https://github.com/ethanholz/nvim-lastplace
" Reopen files at last edit position.
Plug 'farmergreg/vim-lastplace'

" TODO: add key mappings
" Whitespace highlighting and removal.
Plug 'ntpeters/vim-better-whitespace'


""" Custom motions and text objects """

" CamelCase and snake_case motions (M-w, M-b, M-e).
Plug 'chaoren/vim-wordmotion'

" Text object (iv) for variable segments in camelCase or snake_case words.
Plug 'Julian/vim-textobj-variable-segment'

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

" Indentation level text object: ii (indentation level), ai (ii and line
" above), aI (ii with lines above/below).
Plug 'michaeljsmith/vim-indent-object'

" Language syntax text objects: functions (af/if).
Plug 'nvim-treesitter/nvim-treesitter-textobjects'


""" Language support """

" Syntax highlighting, indentation, folding and more using ASTs.
Plug 'nvim-treesitter/nvim-treesitter'

" TODO: evaluate this better
" Show context of currently visible buffer contents.
Plug 'romgrk/nvim-treesitter-context'

" TODO: evaluate this better
" Show current scope in status line.
Plug 'SmiteshP/nvim-gps'

" TODO: evaluate if sleuth is really necessary with treesitter
" TODO: consider using expandtab, tabstop, softtabstop, shiftwidth explicitly
" Automatic tab/indenting configuration.
Plug 'tpope/vim-sleuth'

" TODO: read :h lsp
" LSP configuration.
Plug 'neovim/nvim-lspconfig'

" Completion.
Plug 'hrsh7th/nvim-cmp'
Plug 'onsails/lspkind-nvim'

" Completion sources.
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-nvim-lua'
Plug 'hrsh7th/cmp-path'
Plug 'lukas-reineke/cmp-rg'
Plug 'saadparwaiz1/cmp_luasnip'
if g:os ==# 'Windows'
  Plug 'tzachar/cmp-tabnine', { 'do': 'powershell ./install.ps1' }
else
  Plug 'tzachar/cmp-tabnine', { 'do': './install.sh' }
endif

" Snippets.
" TODO: evaluate this better
Plug 'L3MON4D3/LuaSnip'
Plug 'rafamadriz/friendly-snippets'

" TODO: evaluate null-ls plugin
" Inject LSP diagnostics, code actions and more from non-LSP tools.
Plug 'jose-elias-alvarez/null-ls.nvim'


" Language plugins.
" Reference: https://github.com/sheerun/vim-polyglot#language-packs

" TODO: Evaluate zeavim.vim, vim-dasht and vim-devdocs
Plug 'KabbAmine/zeavim.vim'

" Julia. LaTeX to Unicode substitutions.
Plug 'JuliaEditorSupport/julia-vim'

" Markdown previewer. Start preview with :MarkdownPreview command.
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install' }


""" Terminal and file management support """

" Send code to REPL: send motion in normal mode (gr_) or visual mode (gr),
" send line (grr) and send file (grR).
Plug 'kassio/neoterm'

" File manager for Neovim with a directory buffer that allows file manipulation
" by editing text. Save buffer to modify filesystem. Use <CR> to open file or
" directory, gh to toggle hidden files and - to open parent directory.
Plug 'elihunter173/dirbuf.nvim'

" Shell commands :Delete, :Move, :Rename, :Mkdir, :Chmod, :Wall (save all).
Plug 'tpope/vim-eunuch'

" TODO: evaluate this better
" Project management.
Plug 'ahmedkhalf/project.nvim'


""" Git integration """

" TODO: evaluate neogit
" Git support (:Git).
Plug 'tpope/vim-fugitive'
" Plug 'TimUntersberger/neogit'

" Show a git diff in the sign column.
Plug 'lewis6991/gitsigns.nvim'

" Tab page interface for cycling through diffs
Plug 'sindrets/diffview.nvim'


""" Search commands """

" Extendable fuzzy finder.
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-file-browser.nvim'
Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }


""" Windows and themes """

" TODO: evaluate goolord/alpha-nvim
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

" TODO: evaluate this better
" Indent guides.
Plug 'lukas-reineke/indent-blankline.nvim'

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

" Improve startup time for Neovim.
Plug 'lewis6991/impatient.nvim'

" TODO: evaluate this
" REPL and debug console for nvim lua.
" Plug 'bfredl/nvim-luadev'
" Plug 'rafcamlet/nvim-luapad'

" Lua development setup for init.lua and plugins.
Plug 'folke/lua-dev.nvim'


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
execute 'highlight Folded guibg=' . synIDattr(synIDtrans(hlID('Normal')), 'bg', 'gui')



"""""" Misc settings """"""

" Use ':set option?' to check current option value.
" Use ':verbose set option?' to check where it was set.

" Set blinking cursor in normal mode.
if g:os ==# 'Windows'
  set guicursor=n-v-c-sm:block-blinkwait500-blinkon200-blinkoff150,i-ci-ve:ver25,r-cr-o:hor20
endif

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
set foldlevelstart=99

" Keep 5 lines above or below the cursor when scrolling.
set scrolloff=5

" Keep 5 columns to the left or to the right of the cursor.
set sidescrolloff=5

" Highlight line and column under cursor. It helps with navigation.
set cursorline
set cursorcolumn

" Highlight column 90. It helps identifying long lines.
set colorcolumn=90

" Show the line number relative to the cursor in front of each line and the
" absolute line number for the one with the cursor.
set number
set relativenumber

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

" Use popup menu for completion (one or more matches) and do not select any
" match at first.
set completeopt=menu,menuone,noselect

" Always open diff windows vertically
set diffopt+=vertical

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

" Autobalance windows in each tab on Neovim resize.
autocmd vimrc VimResized * tabdo wincmd =

" Use q to close some support windows.
autocmd vimrc FileType help,juliadoc,qf nnoremap <silent> <buffer> q :close<CR>

" Send help windows to the right.
autocmd vimrc FileType help,juliadoc setlocal bufhidden=unload | wincmd L

" Avoid cursor movement when using operators (e.g. gc_, gr_).
" Save view when setting the operatorfunc option and restore it when cursor is moved.
" Reference: https://vimways.org/2019/making-things-flow
function! SaveViewBeforeOperator() abort
  if match(v:option_new, 'Comment') >= 0 || match(v:option_new, 'neoterm') >= 0
    let w:operatorfunc_view = winsaveview()
    autocmd vimrc CursorMoved * ++once call RestoreViewAfterOperator()
  endif
endfunction
function! RestoreViewAfterOperator() abort
  call winrestview(w:operatorfunc_view)
  unlet w:operatorfunc_view
endfunction
augroup OperatorFuncSteadyView
  autocmd!
  autocmd OptionSet operatorfunc call SaveViewBeforeOperator()
augroup END

" Avoid cursor movement when yanking text.
" Save view on CursorMoved and restore after yank operation.
" Reference: https://github.com/svban/YankAssassin.vim
function! SaveViewOnCursorMove() abort
  let w:pre_yank_view = winsaveview()
endfunction
function! RestoreViewAfterYank() abort
  if v:event.operator=='y' && exists('w:pre_yank_view')
    call winrestview(w:pre_yank_view)
  endif
endfunction
augroup YankSteadyView
  autocmd!
  autocmd CursorMoved * call SaveViewOnCursorMove()
  autocmd TextYankPost * call RestoreViewAfterYank()
augroup END

"""""" Plugin settings """"""

" TODO: make global lists of special (non-code) buffer and file types
" Disable quick-scope highlighting for certain buffers and file types.
let g:qs_buftype_blacklist = ['terminal', 'nofile', 'help']
let g:qs_filetype_blacklist = ['startify', 'fugitive', 'dirbuf']

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
let g:VM_maps['Select Cursor Down'] = '<C-j>'
let g:VM_maps['Select Cursor Up']   = '<C-k>'
let g:VM_maps['Select l']           = '<C-l>'
let g:VM_maps['Select h']           = '<C-h>'

" TODO: improve and convert this to lua
" Disable nvim-autopairs when entering in Visual Multi mode.
" References:
" - *vm-functions*
" - https://github.com/windwp/nvim-autopairs/blob/master/lua/nvim-autopairs.lua
" - https://vi.stackexchange.com/questions/7734/how-to-save-and-restore-a-mapping
let g:VM_bs_map = {}
function! DisableAutopairsMappings() abort
  if mapcheck('<BS>', 'i') !=# ''
    let g:VM_bs_map = maparg('<BS>', 'i', 0 , 1)
    if g:VM_bs_map.buffer == 0 || g:VM_bs_map.expr == 0
      throw 'Unexpected mapping options!'
    endif
    lua require('nvim-autopairs').disable()
    iunmap <buffer> <expr> <BS>
  endif
endfunction
function! EnableAutopairsMappings() abort
  if g:VM_bs_map != {}
    execute 'inoremap <buffer> <expr> <BS> ' . g:VM_bs_map.rhs
    lua require('nvim-autopairs').enable()
    let g:VM_bs_map = {}
  endif
endfunction
autocmd vimrc User visual_multi_start call DisableAutopairsMappings()
autocmd vimrc User visual_multi_exit  call EnableAutopairsMappings()

" vim-better-whitespace settings.
execute 'highlight ExtraWhitespace' .
  \' guibg=' . synIDattr(synIDtrans(hlID('Error')), 'fg', 'gui')
" Deactivate '<Leader>s' key mapping
let g:better_whitespace_operator = ''

" vim-wordmotion settings.
let g:wordmotion_nomap = 1

" zeavim settings.
let g:zv_disable_mapping = 1

" startify settings.
" TODO: evaluate this better
let g:startify_bookmarks = [
  \ expand(stdpath('config') . '/init.vim'),
  \ expand(stdpath('config') . '/config.lua'),
  \ ]
let g:startify_fortune_use_unicode = 1
let g:startify_session_persistence = 1

" neoterm settings.
let g:neoterm_default_mod = 'vertical'
let g:neoterm_direct_open_repl = 1
let g:neoterm_repl_python = ['ipython --profile=vi']
let g:neoterm_repl_enable_ipython_paste_magic = 1
autocmd vimrc FileType python let g:neoterm_bracketed_paste = 0
autocmd vimrc FileType julia  let g:neoterm_bracketed_paste = 1
if g:os ==# 'Linux'
  let g:neoterm_shell = 'bash'
endif

" TODO: make global lists of special (non-code) buffer and file types
" indent_blankline.nvim settings
let g:indent_blankline_buftype_exclude = ['help', 'nofile', 'nowrite', 'terminal']
let g:indent_blankline_filetype_exclude = ['', 'lspinfo', 'checkhealth', 'help', 'startify']
let g:indent_blankline_use_treesitter = v:true

" Load impatient.nvim.
lua require('impatient').enable_profile()

" Load lua plugins' settings.
execute 'luafile ' . stdpath('config') . '/config.lua'



"""""" Key mappings """"""

" - Used keys reference: :help index
" - Unused keys reference:
"   - https://vim.fandom.com/wiki/Unused_keys
"   - https://skippi.medium.com/ideas-for-non-leader-vim-mappings-fd32a2769c87
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


" Disable C-q (tmux prefix).
noremap  <C-q> <Nop>
lnoremap <C-q> <Nop>
tnoremap <C-q> <Nop>


" Save buffer with C-s.
nnoremap <silent> <C-s> :update<CR>
inoremap <silent> <C-s> <C-o>:update<CR>


" Clear last search highlighting (Esc is not mapped to anything in normal mode).
nnoremap <silent> <Esc> :noh<CR><Esc>


" Map substitute.nvim commands.
nnoremap <M-s>      <Cmd>lua require('substitute').operator()<CR>
nnoremap <M-s><M-s> <Cmd>lua require('substitute').line()<CR>
nnoremap <M-S>      <Cmd>lua require('substitute').eol()<CR>
xnoremap <M-s>      <Cmd>lua require('substitute').visual()<CR>


" Map vim-easy-align to gl (since ga is already used).
nmap gl <Plug>(EasyAlign)
xmap gl <Plug>(EasyAlign)


" Map neoterm commands.
xmap gr  <Plug>(neoterm-repl-send)
nmap gr  <Plug>(neoterm-repl-send)
nmap grr <Plug>(neoterm-repl-send-line)
nmap <silent> grR :TREPLSendFile<CR>


" Map LSP commands.
nnoremap <silent> ]d <Cmd>lua vim.diagnostic.goto_next()<CR>
nnoremap <silent> [d <Cmd>lua vim.diagnostic.goto_prev()<CR>


" Map vim-better-whitespace commands.
nnoremap <silent> ]w :NextTrailingWhitespace<CR>
nnoremap <silent> [w :PrevTrailingWhitespace<CR>


" Map vim-wordmotion commands.
nmap <M-w> <Plug>WordMotion_w
nmap <M-b> <Plug>WordMotion_b
nmap <M-e> <Plug>WordMotion_e


" Map <Tab> to = in fugitive.
autocmd vimrc FileType fugitive nmap <buffer> <Tab> =


" Terminal escaping mapping.
tnoremap <C-s> <C-\><C-n>


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

tnoremap <M-Left>  <C-\><C-n><C-w>h
tnoremap <M-Down>  <C-\><C-n><C-w>j
tnoremap <M-Up>    <C-\><C-n><C-w>k
tnoremap <M-Right> <C-\><C-n><C-w>l
inoremap <M-Left>  <C-\><C-n><C-w>h
inoremap <M-Down>  <C-\><C-n><C-w>j
inoremap <M-Up>    <C-\><C-n><C-w>k
inoremap <M-Right> <C-\><C-n><C-w>l
nnoremap <M-Left>  <C-w>h
nnoremap <M-Down>  <C-w>j
nnoremap <M-Up>    <C-w>k
nnoremap <M-Right> <C-w>l

