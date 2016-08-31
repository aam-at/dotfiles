" detect OS {{{
    let s:is_windows = has('win32') || has('win64')
    let s:is_cygwin = has('win32unix')
    let s:is_macvim = has('gui_macvim')
"}}}

" my settings {
    " initialize default settings
    let s:my_settings = {}
    let s:my_settings.cache_dir = '~/.config/nvim/.cache'
    let s:my_settings.default_indent = 4
    let s:my_settings.max_column = 120
    let s:my_settings.autocomplete_method = 'neocomplcache'
    let s:my_settings.enable_cursorcolumn = 0
    let s:my_settings.colorscheme = 'solarized'
    let s:my_settings.background = 'light'
    if has('lua')
        let s:my_settings.autocomplete_method = 'neocomplete'
    elseif filereadable(expand("~/.nvim/bundle/YouCompleteMe/python/ycm_core.*"))
        let s:my_settings.autocomplete_method = 'ycm'
    endif

    let s:my_settings.plugin_groups = []
    call add(s:my_settings.plugin_groups, 'core')
    call add(s:my_settings.plugin_groups, 'editing')
    call add(s:my_settings.plugin_groups, 'navigation')
    call add(s:my_settings.plugin_groups, 'programming')
    call add(s:my_settings.plugin_groups, 'autocomplete')
    call add(s:my_settings.plugin_groups, 'python')
    call add(s:my_settings.plugin_groups, 'lua')
    call add(s:my_settings.plugin_groups, 'latex')
    call add(s:my_settings.plugin_groups, 'scm')
    call add(s:my_settings.plugin_groups, 'unite')
    call add(s:my_settings.plugin_groups, 'indents')
    call add(s:my_settings.plugin_groups, 'writing')
    call add(s:my_settings.plugin_groups, 'misc')
    if s:is_windows
        call add(s:my_settings.plugin_groups, 'windows')
    endif
" }

" setup & neobundle {
    if has('nvim')
        set clipboard+=unnamedplus
    endif
    if has('vim_starting')
        if &compatible
            set nocompatible               " Be iMproved
        endif

        " set the runtime path to include NeoBundle and initialize
        set runtimepath+=~/.config/nvim/bundle/neobundle.vim/
    endif
    if s:is_windows
        set rtp+=~/.config/nvim
    endif
    call neobundle#begin(expand('~/.config/nvim/bundle/'))
    " Let NeoBundle manage NeoBundle
    " Required:
    NeoBundleFetch 'Shougo/neobundle.vim'
" }

" functions {
    function! s:get_cache_dir(suffix) "{{{
        return resolve(expand(s:my_settings.cache_dir . '/' . a:suffix))
    endfunction "}}}

    function! Source(begin, end) " {
        let lines = getline(a:begin, a:end)
        for line in lines
        execute line
        endfor
    endfunction " }

    function! Preserve(command) " {
        " preparation: save last search, and cursor position.
        let _s=@/
        let l = line(".")
        let c = col(".")
        " do the business:
        execute a:command
        " clean up: restore previous search history, and cursor position
        let @/=_s
        call cursor(l, c)
    endfunction " }

    function! StripTrailingWhitespace() " {
        call Preserve("%s/\\s\\+$//e")
    endfunction " }

    function! EnsureExists(path) " {
        if !isdirectory(expand(a:path))
            call mkdir(expand(a:path))
        endif
    endfunction " }

    function! CloseWindowOrKillBuffer() " {
        let number_of_windows_to_this_buffer = len(filter(range(1, winnr('$')), "winbufnr(v:val) == bufnr('%')"))

        " never bdelete a nerd tree
        if matchstr(expand("%"), 'NERD') == 'NERD'
            wincmd c
            return
        endif

        if number_of_windows_to_this_buffer > 1
            wincmd c
        else
            bdelete
        endif
    endfunction " }
" }

"""""""""""""""""
"               "
" BASE SETTINGS "
"               "
"""""""""""""""""
" base configuration {{{

    set timeoutlen=300                                  "mapping timeout
    set ttimeoutlen=50                                  "keycode timeout
  
    set history=1000                                    "number of command lines to remember
    set undolevels=1000
    set ttyfast                                         "assume fast terminal connection
    set viewoptions=folds,options,cursor,unix,slash     "unix/windows compatibility
    set encoding=utf-8                                  "set encoding for text
    if exists('$TMUX')
        set clipboard=
    else
        set clipboard=unnamed                           "sync with OS clipboard
    endif
    set hidden                                          "allow buffer switching without saving
    set autoread                                        "auto reload if file saved externally
    set fileformats+=mac                                "add mac to auto-detection of file format line endings
    set nrformats-=octal                                "always assume decimal numbers
    set showcmd
    set tags=tags;/
    set showfulltag
    set modeline
    set modelines=5

    if s:is_windows && !s:is_cygwin
        " ensure correct shell in gvim
        set shell=c:\windows\system32\cmd.exe
    endif

    if $SHELL =~ '/fish$'
        " VIM expects to be run from a POSIX shell.
        set shell=sh
    endif

    set noshelltemp                                     "use pipes

    " whitespace
    set backspace=indent,eol,start                      "allow backspacing everything in insert mode
    set autoindent                                      "automatically indent to match adjacent lines
    set expandtab                                       "spaces instead of tabs
    set smarttab                                        "use shiftwidth to enter tabs
    let &tabstop=s:my_settings.default_indent           "number of spaces per tab for display
    let &softtabstop=s:my_settings.default_indent       "number of spaces per tab in insert mode
    let &shiftwidth=s:my_settings.default_indent        "number of spaces when indenting
    set list                                            "highlight whitespace
    set listchars=tab:▸\ ,trail:•,extends:»,precedes:«
    set shiftround
    set linebreak
    let &showbreak='↪ '

    set scrolloff=1                                     "always show content after scroll
    set scrolljump=5                                    "minimum number of lines to scroll
    set display+=lastline
    set wildmenu                                        "show list for autocomplete
    set wildmode=list:full
    set wildignorecase

    set splitbelow
    set splitright

    " disable sounds
    set noerrorbells
    set novisualbell
    set t_vb=

    " searching
    set hlsearch                                        "highlight searches
    set incsearch                                       "incremental searching
    set ignorecase                                      "ignore case for searching
    set smartcase                                       "do case-sensitive if there's a capital letter
    if executable('ag')
        set grepprg=ag\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow
        set grepformat=%f:%l:%c:%m
    elseif executable('pt')
        set grepprg=pt\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow
        set grepformat=%f:%l:%c:%m
    elseif executable('ack')
        set grepprg=ack\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow\ $*
        set grepformat=%f:%l:%c:%m
    endif


    " vim file/folder management {{{
        " persistent undo
        if exists('+undofile')
            set undofile
            let &undodir = s:get_cache_dir('undo')
        endif

        " backups
        set backup
        let &backupdir = s:get_cache_dir('backup')
        set backupskip=/tmp/*,/private/tmp/*"

        " swap files
        let &directory = s:get_cache_dir('swap')
        set noswapfile
        set writebackup

        call EnsureExists(s:my_settings.cache_dir)
        call EnsureExists(&undodir)
        call EnsureExists(&backupdir)
        call EnsureExists(&directory)
    "}}}

    let mapleader = ","
    let maplocalleader = "\\"
    let g:mapleader = ","
"}}}

" python configuration {{{
    let g:python_host_prog = '/home/amatyasko/.pyenv/versions/neovim2/bin/python'
    let g:python3_host_prog = '/home/amatyasko/.pyenv/versions/neovim3/bin/python'
" }}}

" ui configuration {{{
    set textwidth=79
    set nowrap                                          "don't automatically wrap on load
    set formatoptions-=t                                "don't automatically wrap text when typing
    " set termguicolors
    " :let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1              " change cursor shape (does not work with guake)

    set ruler
    set showmatch                                       "automatically highlight matching braces/brackets/etc.
    set matchtime=2                                     "tens of a second to show matching parentheses
    set number
    set lazyredraw
    set laststatus=2
    set noshowmode
    set foldenable                                      "enable folds by default
    set foldmethod=syntax                               "fold via syntax of files
    set foldlevelstart=99                               "open all folds by default
    let g:xml_syntax_folding=1                          "enable xml folding

    set cursorline
    autocmd WinLeave * setlocal nocursorline
    autocmd WinEnter * setlocal cursorline
    let &colorcolumn=s:my_settings.max_column
    highlight ColorColumn ctermbg=233
    if s:my_settings.enable_cursorcolumn
        set cursorcolumn
        autocmd WinLeave * setlocal nocursorcolumn
        autocmd WinEnter * setlocal cursorcolumn
    endif

    if has('conceal')
        set conceallevel=1
        set listchars+=conceal:Δ
    endif
    set gfn=Sauce\ Code\ Powerline
    " set gfn=Menlo\ For\ Powerline

    if has('gui_running')
        " open maximized
        set lines=999 columns=9999
        if s:is_windows
            autocmd GUIEnter * simalt ~x
        endif

        set guioptions+=t                                 "tear off menu items
        set guioptions-=T                                 "toolbar icons
        set guioptions-=r
        set guioptions-=L

        if s:is_macvim
            "set gfn=Ubuntu_Mono:h14
            set transparency=2
        endif

        if s:is_windows
            set gfn=Hack
        endif

        if has('gui_gtk')
            "set gfn=Ubuntu\ Mono\ 11
        endif
    else
        if $COLORTERM == 'gnome-terminal'
            set t_Co=256 "why you no tell me correct colors?!?!
        endif
        if $TERM_PROGRAM == 'iTerm.app'
            " different cursors for insert vs normal mode
            if exists('$TMUX')
                let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
                let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
            else
                let &t_SI = "\<Esc>]50;CursorShape=1\x7"
                let &t_EI = "\<Esc>]50;CursorShape=0\x7"
            endif
        endif
    endif
"}}}


""""""""""""""""""""""""""
"                        "
" PACKAGE MANAGEMENT     "
"                        "
"""""""""""""""""""""""""" {{{

if count(s:my_settings.plugin_groups, 'core') "{{{
    NeoBundle 'bling/vim-airline' "{{{
        let g:airline#extensions#tabline#enabled = 1
        let g:airline#extensions#tabline#left_sep = ' '
        let g:airline#extensions#tabline#left_alt_sep = '¦'
        let g:airline#extensions#tabline#buffer_idx_mode = 1
        nmap <leader>1 <Plug>AirlineSelectTab1
        nmap <leader>2 <Plug>AirlineSelectTab2
        nmap <leader>3 <Plug>AirlineSelectTab3
        nmap <leader>4 <Plug>AirlineSelectTab4
        nmap <leader>5 <Plug>AirlineSelectTab5
        nmap <leader>6 <Plug>AirlineSelectTab6
        nmap <leader>7 <Plug>AirlineSelectTab7
        nmap <leader>8 <Plug>AirlineSelectTab8
        nmap <leader>9 <Plug>AirlineSelectTab9
        let g:airline_powerline_fonts = 1
        if !exists('g:airline_symbols')
            let g:airline_symbols = {}
        endif

        " unicode symbols
        let g:airline_left_sep = '»'
        let g:airline_left_sep = '▶'
        let g:airline_right_sep = '«'
        let g:airline_right_sep = '◀'
        let g:airline_symbols.linenr = '␊'
        let g:airline_symbols.linenr = '␤'
        let g:airline_symbols.linenr = '¶'
        let g:airline_symbols.branch = '⎇'
        let g:airline_symbols.paste = 'ρ'
        let g:airline_symbols.paste = 'Þ'
        let g:airline_symbols.paste = '∥'
        let g:airline_symbols.whitespace = 'Ξ'
    "}}}
    " Better numbers for vim
    NeoBundle 'myusuf3/numbers.vim'
    NeoBundle 'tpope/vim-surround'
    NeoBundle 'tpope/vim-repeat'
    NeoBundle 'tpope/vim-dispatch'
    NeoBundle 'tpope/vim-eunuch'
    NeoBundle 'tpope/vim-unimpaired' "{{{
        nmap <c-up> [e
        nmap <c-down> ]e
        vmap <c-up> [egv
        vmap <c-down> ]egv
    "}}}
    NeoBundle 'Shougo/vimproc.vim', {
        \   'build': {
        \   'mac': 'make -f make_mac.mak',
        \   'unix': 'make -f make_unix.mak',
        \   'cygwin': 'make -f make_cygwin.mak',
        \   'windows': '"C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\bin\nmake.exe" make_msvc32.mak',
        \ },
    \ }
    NeoBundle 'Konfekt/FastFold'
endif "}}}

if count(s:my_settings.plugin_groups, 'editing') "{{{
    NeoBundleLazy 'editorconfig/editorconfig-vim', {'autoload':{'insert':1}}
    NeoBundle 'tpope/vim-endwise'
    NeoBundle 'tpope/vim-speeddating'
    NeoBundle 'thinca/vim-visualstar'
    NeoBundle 'tomtom/tcomment_vim'
    NeoBundle 'terryma/vim-expand-region'
    NeoBundle 'terryma/vim-multiple-cursors'
    NeoBundle 'chrisbra/NrrwRgn'
    NeoBundleLazy 'godlygeek/tabular', {'autoload':{'commands':'Tabularize'}} "{{{
        nmap <Leader>a& :Tabularize /&<CR>
        vmap <Leader>a& :Tabularize /&<CR>
        nmap <Leader>a= :Tabularize /=<CR>
        vmap <Leader>a= :Tabularize /=<CR>
        nmap <Leader>a: :Tabularize /:<CR>
        vmap <Leader>a: :Tabularize /:<CR>
        nmap <Leader>a:: :Tabularize /:\zs<CR>
        vmap <Leader>a:: :Tabularize /:\zs<CR>
        nmap <Leader>a, :Tabularize /,<CR>
        vmap <Leader>a, :Tabularize /,<CR>
        nmap <Leader>a<Bar> :Tabularize /<Bar><CR>
        vmap <Leader>a<Bar> :Tabularize /<Bar><CR>
    "}}}
    NeoBundle 'jiangmiao/auto-pairs'
    NeoBundle 'justinmk/vim-sneak' "{{{
        let g:sneak#streak = 1
    "}}}
endif "}}}

if count(s:my_settings.plugin_groups, 'navigation') "{{{
    NeoBundle 'mileszs/ack.vim' "{{{
        if executable('ag')
            let g:ackprg = "ag --nogroup --column --smart-case --follow"
        endif
    "}}}
    " ag can be installed on windows with https://chocolatey.org/
    " pt written in go. works everywhere.
    " TODO: select the one which is fastest.
    NeoBundle 'rking/ag.vim' "{{{
        if !executable('ag') && executable('pt')
            let g:ag_prg = "pt --nogroup --column --smart-case --follow"
        endif
    "}}}
    " Undo window
    NeoBundleLazy 'simnalamburt/vim-mundo', {'autoload':{'commands': 'GundoToggle'}} "{{{
        " f3 toggles the Gundo plugin window
        nnoremap <silent> <F3> :GundoToggle<CR>
        let g:gundo_width=80
        let g:gundo_right = 1
    "}}}
    NeoBundleLazy 'EasyGrep', {'autoload':{'commands':'GrepOptions'}} "{{{
        let g:EasyGrepRecursive=1
        let g:EasyGrepAllOptionsInExplorer=1
        let g:EasyGrepCommand=1
        nnoremap <leader>vo :GrepOptions<cr>
    "}}}
    NeoBundle 'ctrlpvim/ctrlp.vim', { 'depends': 'tacahiroy/ctrlp-funky' } "{{{
        let g:ctrlp_working_path_mode = 0
        let g:ctrlp_clear_cache_on_exit=1
        let g:ctrlp_max_height=20
        let g:ctrlp_show_hidden=0
        let g:ctrlp_follow_symlinks=1
        let g:ctrlp_max_files=20000
        let g:ctrlp_cache_dir=s:get_cache_dir('ctrlp')
        let g:ctrlp_reuse_window='startify'
        let g:ctrlp_extensions=['funky']
        let g:ctrlp_custom_ignore = {
                    \ 'dir': '\v[\/]\.(git|hg|svn|idea)$',
                    \ 'file': '\v\.(DS_Store|exe|so|dll|pyc)$'
                    \ }

        if executable('ag')
            " Faster indexing of files; requires having ag (AKA the_silver_searcher)
            " installed.
            let g:ctrlp_user_command='ag %s -l --nocolor -g ""'
        endif

        nmap \ [ctrlp]
        nnoremap [ctrlp] <nop>

        nnoremap [ctrlp]t :CtrlPBufTag<cr>
        nnoremap [ctrlp]T :CtrlPTag<cr>
        nnoremap [ctrlp]l :CtrlPLine<cr>
        nnoremap [ctrlp]o :CtrlPFunky<cr>
        nnoremap [ctrlp]b :CtrlPBuffer<cr>
    "}}}
    NeoBundleLazy 'scrooloose/nerdtree', {'autoload':{'commands':['NERDTreeToggle','NERDTreeFind']}} "{{{
        let NERDTreeShowHidden=1
        let NERDTreeQuitOnOpen=0
        let NERDTreeShowLineNumbers=0
        let NERDTreeChDirMode=0
        let NERDTreeShowBookmarks=1
        let NERDTreeIgnore=['\.git','\.hg']
        let NERDTreeBookmarksFile=s:get_cache_dir('NERDTreeBookmarks')
        " NERDTree key bindings
        nmap <silent> <F5> :NERDTreeToggle<CR>
    "}}}
    NeoBundle 'jistr/vim-nerdtree-tabs'
    NeoBundle 'Xuyuanp/nerdtree-git-plugin'
    NeoBundle 'tpope/vim-vinegar'
    NeoBundle 'easymotion/vim-easymotion' "{{{
        " Easymotion colors for light colors
        hi link EasyMotionTarget ErrorMsg
        hi link EasyMotionTarget2First Search
        hi link EasyMotionTarget2Second Search
        hi link EasyMotionShade  Comment
    "}}}"
    NeoBundle 'MattesGroeger/vim-bookmarks'
endif "}}}


if count(s:my_settings.plugin_groups, 'programming') "{{{
    NeoBundle 'scrooloose/syntastic' "{{{
        set statusline+=%#warningmsg#
        set statusline+=%{SyntasticStatuslineFlag()}
        set statusline+=%*

        let g:syntastic_error_symbol = '✗'
        let g:syntastic_warning_symbol = '∆'
        let g:syntastic_style_error_symbol = '✠'
        let g:syntastic_style_warning_symbol = '≈'

        let g:syntastic_always_populate_loc_list = 1
        let g:syntastic_auto_loc_list = 1
        let g:syntastic_check_on_open = 1
        let g:syntastic_check_on_wq = 0
        let g:syntastic_loc_list_height = 7

        let g:syntastic_python_checkers = ['flake8', 'pep8']
        " avoid conflicts with python mode
        " Dysable syntastic for python. Currently use syntastic (some problems
        " with quickfix window in python mode)
        let g:syntastic_mode_map = { 'passive_filetypes': ['python'] }
        let g:syntastic_ignore_files = ['\.py$'] 
    "}}}
    " Commenting code
    NeoBundle 'scrooloose/nerdcommenter'
    NeoBundle 'kien/rainbow_parentheses.vim'
    NeoBundleLazy 'majutsushi/tagbar' , {'autoload':{'commands':'TagbarToggle'}} "{{{
        nnoremap <silent> <F4> :TagbarToggle<CR>
    "}}}
    " Supertab plugin
    NeoBundle 'ervandew/supertab' "{{{
        au FileType python set omnifunc=pythoncomplete#Complete
        let g:SuperTabDefaultCompletionType = "context"
        set completeopt=menuone,longest,preview
    "}}}
    " Protobuf highlighting
    NeoBundle 'uarun/vim-protobuf'
endif "}}}

if count(s:my_settings.plugin_groups, 'autocomplete') "{{{
    NeoBundle 'honza/vim-snippets'
    if s:my_settings.autocomplete_method == 'ycm' "{{{
      NeoBundle 'Valloric/YouCompleteMe' "{{{
        let g:ycm_complete_in_comments_and_strings=1
        let g:ycm_key_list_select_completion=['<C-n>', '<Down>']
        let g:ycm_key_list_previous_completion=['<C-p>', '<Up>']
        let g:ycm_filetype_specific_completion_to_disable = { 'python' : 1 }
        let g:ycm_filetype_blacklist={'unite': 1, 'python' : 1}
    " }
      "}}}
      NeoBundle 'SirVer/ultisnips' "{{{
        let g:UltiSnipsExpandTrigger="<tab>"
        let g:UltiSnipsJumpForwardTrigger="<tab>"
        let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
        let g:UltiSnipsSnippetsDir='~/.config/nvim/snippets'
      "}}}
    else
      NeoBundle 'Shougo/neosnippet-snippets'
      NeoBundle 'Shougo/neosnippet.vim' "{{{
        let g:neosnippet#snippets_directory='~/.config/nvim/bundle/vim-snippets/snippets,~/.config/nvim/snippets'
        let g:neosnippet#enable_snipmate_compatibility=1

        imap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : (pumvisible() ? "\<C-n>" : "\<TAB>")
        smap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
        imap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""
        smap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""
      "}}}
      NeoBundle 'Shougo/deoplete.nvim' "{{{
        let g:deoplete#enable_at_startup = 1   "enable deoplete at vim startup
        let g:deoplete#enable_ignore_case = 1  "let matcher ignore case
        let g:deoplete#enable_smart_case = 1   "smart case
        inoremap <expr><C-h> deolete#mappings#smart_close_popup()."\<C-h>"
        inoremap <expr><BS> deoplete#mappings#smart_close_popup()."\<C-h>"
      "}}}
    endif "}}}
endif "}}}

if count(s:my_settings.plugin_groups, 'python') "{{{
    " NeoBundle 'nvie/vim-flake8' - same functionality to syntastic
    NeoBundleLazy 'klen/python-mode', {'autoload':{'filetypes':['python']}} "{{{
        " Activate rope
        " Keys:
        " K             Show python docs
        " <Ctrl-Space>  Rope autocomplete
        " <Ctrl-c>g     Rope goto definition
        " <Ctrl-c>d     Rope show documentation
        " <Ctrl-c>f     Rope find occurrences
        " <Leader>b     Set, unset breakpoint (g:pymode_breakpoint enabled)
        " [[            Jump on previous class or function (normal, visual, operator modes)
        " ]]            Jump on next class or function (normal, visual, operator modes)
        " [M            Jump on previous class or method (normal, visual, operator modes)
        " ]M            Jump on next class or method (normal, visual, operator modes)
        let g:pymode_rope = 0 " use jedi plugin for autocompletion

        " Documentation
        let g:pymode_doc = 1
        let g:pymode_doc_key = 'K'

        "Linting
        let g:pymode_lint = 1
        let g:pymode_lint_checker = "pyflakes,pep8"
        " Auto check on save
        let g:pymode_lint_write = 0

        "Quickfix window
        let g:pymode_quickfix_minheight = 3
        let g:pymode_quickfix_maxheight = 6
        let g:pymode_lint_cwindow = 1

        " Support virtualenv
        let g:pymode_virtualenv = 1

        " Enable breakpoints plugin
        let g:pymode_breakpoint = 1
        let g:pymode_breakpoint_bind = '<leader>b'

        " syntax highlighting
        let g:pymode_syntax = 1
        let g:pymode_syntax_all = 1
        let g:pymode_syntax_indent_errors = g:pymode_syntax_all
        let g:pymode_syntax_space_errors = g:pymode_syntax_all

        " Don't autofold code
        let g:pymode_folding = 0
    "}}}
    NeoBundleLazy 'python-rope/ropevim', {'autoload':{'filetypes':['python']}}
    NeoBundle 'davidhalter/jedi-vim' "{{{
        let g:jedi#auto_vim_configuration = 0
        let g:jedi#popup_on_dot = 0
        let g:jedi#goto_command = "<leader>d"
        let g:jedi#goto_assignments_command = "<leader>g"
        let g:jedi#goto_definitions_command = ""
        let g:jedi#documentation_command = "K"
        let g:jedi#usages_command = "<leader>z"
        let g:jedi#completions_command = "<C-Space>"
        " disable <leader>r. Use rope-vim for refactoring
        let g:jedi#rename_command = ""
    "}}}
    NeoBundleLazy 'heavenshell/vim-pydocstring', {'autoload':{'filetypes':['python']}} "{{{
        nmap <silent> <C-_> <Plug>(pydocstring)
    "}}}
    NeoBundleLazy 'vim-scripts/python_match.vim', {'autoload':{'filetypes':['python']}}
    NeoBundleLazy 'mindriot101/vim-yapf', {'autoload':{'filetypes':['python']}} "{{{
        nmap <leader>= :call Yapf()<cr>
    "}}}
    NeoBundleLazy 'lambdalisue/vim-pyenv', {
            \ 'depends': ['davidhalter/jedi-vim'],
            \ 'autoload': {
            \   'filetypes': ['python', 'python3'],
            \ }}
endif "}}}

if count(s:my_settings.plugin_groups, 'lua') "{{{
    NeoBundle 'xolox/vim-misc'
    NeoBundleLazy 'xolox/vim-lua-inspect', {'depends': 'xolox/vim-misc', 'autoload':{'filetypes':['lua']}}
    NeoBundleLazy 'xolox/vim-lua-ftplugin', {'depends': 'xolox/vim-misc', 'autoload':{'filetypes':['lua']}}
    " NeoBundle 'WolfgangMehner/lua-support'
endif "}}}

if count(s:my_settings.plugin_groups, 'latex') "{{{
    NeoBundleLazy 'LaTeX-Box-Team/LaTeX-Box', {'autoload':{'filetypes':['tex']}}
    NeoBundleLazy 'lervag/vimtex', {'autoload':{'filetypes':['tex']}}
    if !s:is_windows
        NeoBundleLazy 'xuhdev/vim-latex-live-preview', {'autoload':{'filetypes':['tex']}}
    endif
endif "}}}

if count(s:my_settings.plugin_groups, 'scm') "{{{
    NeoBundle 'mhinz/vim-signify' "{{{
        let g:signify_update_on_bufenter=0
    "}}}
    if executable('hg')
        NeoBundle 'bitbucket:ludovicchabant/vim-lawrencium'
    endif
    NeoBundle 'tpope/vim-fugitive' "{{{
        nnoremap <silent> <leader>gs :Gstatus<CR>
        nnoremap <silent> <leader>gd :Gdiff<CR>
        nnoremap <silent> <leader>gc :Gcommit<CR>
        nnoremap <silent> <leader>gb :Gblame<CR>
        nnoremap <silent> <leader>gl :Glog<CR>
        nnoremap <silent> <leader>gp :Gpush<CR>
        nnoremap <silent> <leader>gf :Gpull<CR>
        nnoremap <silent> <leader>gw :Gwrite<CR>
        nnoremap <silent> <leader>gr :Gremove<CR>
        autocmd BufReadPost fugitive://* set bufhidden=delete
    "}}}
    NeoBundleLazy 'gregsexton/gitv', {'depends':['tpope/vim-fugitive'], 'autoload':{'commands':'Gitv'}} "{{{
        nnoremap <silent> <leader>gv :Gitv<CR>
        nnoremap <silent> <leader>gV :Gitv!<CR>
    "}}}
endif "}}}

if count(s:my_settings.plugin_groups, 'unite') "{{{
    NeoBundle 'Shougo/unite.vim' "{{{
    let bundle = neobundle#get('unite.vim')
    function! bundle.hooks.on_source(bundle)
        call unite#filters#matcher_default#use(['matcher_fuzzy'])
        call unite#filters#sorter_default#use(['sorter_rank'])
        call unite#custom#profile('default', 'context', {
                    \ 'start_insert': 1
                    \ })
    endfunction

    let g:unite_data_directory=s:get_cache_dir('unite')
    let g:unite_source_history_yank_enable=1
    let g:unite_source_rec_max_cache_files=5000

    if executable('ag')
        let g:unite_source_grep_command='ag'
        let g:unite_source_grep_default_opts='--nocolor --nogroup -S'
        let g:unite_source_grep_recursive_opt=''
    elseif executable('pt')
        " Use pt in unite grep source.
        " https://github.com/monochromegane/the_platinum_searcher
        let g:unite_source_grep_command = 'pt'
        let g:unite_source_grep_default_opts='--nocolor --nogroup -S'
        let g:unite_source_grep_recursive_opt = ''
    elseif executable('ack')
        let g:unite_source_grep_command='ack'
        let g:unite_source_grep_default_opts='--no-heading --no-color -C4'
        let g:unite_source_grep_recursive_opt=''
    endif

    function! s:unite_settings()
        nmap <buffer> Q <plug>(unite_exit)
        nmap <buffer> <esc> <plug>(unite_exit)
        imap <buffer> <esc> <plug>(unite_exit)
        " Play nice with supertab
        let b:SuperTabDisabled=1
        " Enable navigation with control-j and control-k in insert mode
        imap <buffer> <C-j>   <Plug>(unite_select_next_line)
        imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
    endfunction
    autocmd FileType unite call s:unite_settings()

    nmap <space> [unite]
    nnoremap [unite] <nop>

    if s:is_windows
        nnoremap <silent> [unite]<space> :<C-u>Unite -toggle -auto-resize -buffer-name=mixed file_rec:! buffer file_mru bookmark<cr><c-u>
        nnoremap <silent> [unite]f :<C-u>Unite -toggle -auto-resize -buffer-name=files file_rec:!<cr><c-u>
    else
        nnoremap <silent> [unite]<space> :<C-u>Unite -toggle -auto-resize -buffer-name=mixed file_rec/async:! buffer file_mru bookmark<cr><c-u>
        nnoremap <silent> [unite]f :<C-u>Unite -toggle -auto-resize -buffer-name=files file_rec/async:!<cr><c-u>
    endif
    nnoremap <silent> [unite]e :<C-u>Unite -buffer-name=recent file_mru<cr>
    nnoremap <silent> [unite]y :<C-u>Unite -buffer-name=yanks history/yank<cr>
    nnoremap <silent> [unite]l :<C-u>Unite -auto-resize -buffer-name=line line<cr>
    nnoremap <silent> [unite]b :<C-u>Unite -auto-resize -buffer-name=buffers buffer file_mru<cr>
    nnoremap <silent> [unite]/ :<C-u>Unite -no-quit -buffer-name=search grep:.<cr>
    nnoremap <silent> [unite]m :<C-u>Unite -auto-resize -buffer-name=mappings mapping<cr>
    nnoremap <silent> [unite]s :<C-u>Unite -quick-match buffer<cr>
    "}}}
    NeoBundleLazy 'Shougo/neomru.vim', {'autoload':{'unite_sources':'file_mru'}}
    NeoBundleLazy 'osyo-manga/unite-airline_themes', {'autoload':{'unite_sources':'airline_themes'}} "{{{
        nnoremap <silent> [unite]a :<C-u>Unite -winheight=10 -auto-preview -buffer-name=airline_themes airline_themes<cr>
    "}}}
    NeoBundleLazy 'ujihisa/unite-colorscheme', {'autoload':{'unite_sources':'colorscheme'}} "{{{
        nnoremap <silent> [unite]c :<C-u>Unite -winheight=10 -auto-preview -buffer-name=colorschemes colorscheme<cr>
    "}}}
    NeoBundleLazy 'tsukkee/unite-tag', {'autoload':{'unite_sources':['tag','tag/file']}} "{{{
        nnoremap <silent> [unite]t :<C-u>Unite -auto-resize -buffer-name=tag tag tag/file<cr>
    "}}}
    NeoBundleLazy 'Shougo/unite-outline', {'autoload':{'unite_sources':'outline'}} "{{{
        nnoremap <silent> [unite]o :<C-u>Unite -auto-resize -buffer-name=outline outline<cr>
    "}}}
    NeoBundleLazy 'Shougo/unite-help', {'autoload':{'unite_sources':'help'}} "{{{
        nnoremap <silent> [unite]h :<C-u>Unite -auto-resize -buffer-name=help help<cr>
    "}}}
    NeoBundleLazy 'Shougo/junkfile.vim', {'autoload':{'commands':'JunkfileOpen','unite_sources':['junkfile','junkfile/new']}} "{{{
        let g:junkfile#directory=s:get_cache_dir('junk')
        nnoremap <silent> [unite]j :<C-u>Unite -auto-resize -buffer-name=junk junkfile junkfile/new<cr>
    "}}}
endif "}}}

if count(s:my_settings.plugin_groups, 'indents') "{{{
    NeoBundle 'nathanaelkane/vim-indent-guides' "{{{
        let g:indent_guides_start_level=1
        let g:indent_guides_guide_size=1
        let g:indent_guides_enable_on_vim_startup=0
        let g:indent_guides_color_change_percent=3
        if !has('gui_running')
        let g:indent_guides_auto_colors=0
        function! s:indent_set_console_colors()
            hi IndentGuidesOdd ctermbg=235
            hi IndentGuidesEven ctermbg=236
        endfunction
        autocmd VimEnter,Colorscheme * call s:indent_set_console_colors()
        endif
    "}}}
endif "}}}

if count(s:my_settings.plugin_groups, 'writing') "{{{
    NeoBundle 'kana/vim-textobj-user'
    NeoBundle 'kana/vim-textobj-indent'
    NeoBundle 'kana/vim-textobj-entire'
    NeoBundle 'lucapette/vim-textobj-underscore'
    NeoBundle 'reedes/vim-textobj-sentence' "{{{
        augroup textobj_sentence
            autocmd!
            autocmd FileType markdown call textobj#sentence#init()
            autocmd FileType textile call textobj#sentence#init()
            autocmd FileType text call textobj#sentence#init()
        augroup END
    "}}}
    NeoBundle 'reedes/vim-textobj-quote' "{{{
        augroup textobj_quote
            autocmd!
            autocmd FileType markdown call textobj#quote#init()
            autocmd FileType textile call textobj#quote#init()
            autocmd FileType text call textobj#quote#init({'educate': 0})
        augroup END
    "}}}
    NeoBundle 'reedes/vim-wordy', {'autoload': {'filetypes': ['txt', 'tex']}}
    NeoBundle 'reedes/vim-litecorrect' "{{{
        augroup litecorrect
            autocmd!
            autocmd FileType markdown,mkd call litecorrect#init()
            autocmd FileType textile      call litecorrect#init()
            autocmd FileType text         call litecorrect#init()
        augroup END
    "}}}
    NeoBundle 'reedes/vim-pencil' "{{{
        augroup pencil
            autocmd!
            autocmd FileType markdown,mkd call pencil#init()
            autocmd FileType texfile      call pencil#init()
            autocmd FileType text         call pencil#init()
        augroup END
    "}}}
    NeoBundle 'panozzaj/vim-autocorrect' "{{{
        augroup autocorrect
            autocmd filetype markdown,mkd call AutoCorrect()
            autocmd filetype text         call AutoCorrect()
            autocmd filetype texfile      call AutoCorrect()
        augroup END
    "}}}
    NeoBundleLazy 'junegunn/goyo.vim', {'autoload':{'commands':'Goyo'}}
    NeoBundleLazy 'junegunn/limelight.vim', {'autoload':{'commands':'Limelight'}} "{{{
        let g:limelight_conceal_ctermfg = 'gray'
        let g:limelight_conceal_ctermfg = 240
        autocmd! User GoyoEnter Limelight
        autocmd! User GoyoLeave Limelight!
    " }}}
endif "}}}


if count(s:my_settings.plugin_groups, 'misc') "{{{
    if exists('$TMUX')
      NeoBundle 'christoomey/vim-tmux-navigator'
      NeoBundle 'benmills/vimux'
      NeoBundle 'julienr/vim-cellmode', {'autoload':{'filetypes':['python']}}
    endif
    if executable('task')
        NeoBundle 'blindFS/vim-taskwarrior' "{{{
            let g:task_log_directory   = '~/.task'
        "}}}
        NeoBundle 'Spirotot/taskwiki', {'depends' : 'vimwiki/vimwiki'}
        NeoBundle 'powerman/vim-plugin-AnsiEsc'
    endif
    NeoBundle 'wakatime/vim-wakatime'
    NeoBundleLazy 'tpope/vim-scriptease', {'autoload':{'filetypes':['vim']}}
    NeoBundleLazy 'plasticboy/vim-markdown', {'autoload':{'filetypes':['markdown']}}
    NeoBundleLazy 'guns/xterm-color-table.vim', {'autoload':{'commands':'XtermColorTable'}}
    NeoBundle 'chrisbra/vim_faq'
    " Notes {{{
        " If I'm lazy to wait for emacs launch, for quick edit
        NeoBundle 'jceb/vim-orgmode'
        NeoBundle 'vimwiki/vimwiki' "{{{
            " Taskwarior backend for notes
            let g:vimwiki_list = [{'path': '~/Dropbox/Notes/markdown/',
                        \ 'syntax': 'markdown', 'ext': '.md'}]
            " autocmd! BufRead,BufNewFile */vimwiki/*        set filetype=vimwiki
            autocmd BufNewFile,BufReadPost *.md set filetype=markdown
        "}}}
    "}}}
    NeoBundle 'powerline/fonts'
    NeoBundle 'bufkill.vim'
    NeoBundle 'mhinz/vim-startify' "{{{
        let g:startify_session_dir = s:get_cache_dir('sessions')
        let g:startify_change_to_vcs_root = 1
        let g:startify_show_sessions = 1
        nnoremap <F1> :Startify<cr>
    "}}}
    NeoBundleLazy 'mattn/gist-vim', { 'depends': 'mattn/webapi-vim', 'autoload': { 'commands': 'Gist' } } "{{{
        let g:gist_post_private=1
        let g:gist_show_privates=1
    "}}}
    NeoBundleLazy 'Shougo/vimshell.vim', {'autoload':{'commands':[ 'VimShell', 'VimShellInteractive' ]}} "{{{
        if s:is_macvim
            let g:vimshell_editor_command='mvim'
        else
            let g:vimshell_editor_command='vim'
        endif
        let g:vimshell_right_prompt='getcwd()'
        let g:vimshell_data_directory=s:get_cache_dir('vimshell')
        let g:vimshell_vimshrc_path='~/.config/nvim/vimshrc'

        nnoremap <localleader>c :VimShell -split<cr>
        nnoremap <localleader>cc :VimShell -split<cr>
        nnoremap <localleader>cn :VimShellInteractive node<cr>
        nnoremap <localleader>cl :VimShellInteractive lua<cr>
        nnoremap <localleader>cr :VimShellInteractive irb<cr>
        nnoremap <localleader>cp :VimShellInteractive python<cr>
    "}}}
    NeoBundleLazy 'zhaocai/GoldenView.Vim', {'autoload':{'mappings':['<Plug>ToggleGoldenViewAutoResize']}} "{{{
        let g:goldenview__enable_default_mapping=0
        nmap <F8> <Plug>ToggleGoldenViewAutoResize
    "}}}
    NeoBundleLazy 'krisajenkins/vim-pipe'
endif "}}}

if count(s:my_settings.plugin_groups, 'windows') "{{{
    if has('win32') && !has('win64')
        NeoBundle 'derekmcloughlin/gvimfullscreen_win32' "{{{
            let $GVIMFS=substitute(expand("$HOME/.config/nvim/bundle/gvimfullscreen_win32/gvimfullscreen.dll"), '\\', '\\\\', 'g')
            map <F11> <Esc>:call libcallnr($GVIMFS, "ToggleFullScreen", 0)<CR>
        "}}}
    else
        NeoBundle 'xqin/gvimfullscreen' "{{{
            let $GVIMFS=substitute(expand("$HOME/.config/nvim/bundle/gvimfullscreen/gvimfullscreen.dll.x64"), '\\', '\\\\', 'g')
            map <F11> <Esc>:call libcallnr($GVIMFS, "ToggleFullScreen", 0)<CR>
        "}}}
    endif
endif "}}}

" }}}

" mappings {{{
    " Learn vim hard way
    noremap <Up> <NOP>
    noremap <Down> <NOP>
    noremap <Left> <NOP>
    noremap <Right> <NOP>
    inoremap <Up> <NOP>
    inoremap <Down> <NOP>
    inoremap <Left> <NOP>
    inoremap <Right> <NOP>

    " formatting shortcuts
    nmap <leader>fef :call Preserve("normal gg=G")<CR>
    nmap <leader>f$ :call StripTrailingWhitespace()<CR>
    vmap <leader>s :sort<cr>

    " Shorcuts for quick save and quit
    nnoremap <leader>w :w<cr>
    noremap <Leader>q :q<cr>
    noremap <Leader>Q :qa<cr>

    " toggle paste
    map <F6> :set invpaste<CR>:set paste?<CR>

    " remap arrow keys
    nnoremap <left> :bprev<CR>
    nnoremap <right> :bnext<CR>
    nnoremap <up> :tabnext<CR>
    nnoremap <down> :tabprev<CR>

    " smash escape
    inoremap jk <esc>
    inoremap kj <esc>

    " change cursor position in insert mode
    inoremap <C-h> <left>
    inoremap <C-l> <right>
    inoremap <C-u> <C-g>u<C-u>

    nnoremap j gj
    nnoremap k gk

    " shortcuts for windows {{{
        nnoremap <leader>v <C-w>v<C-w>l
        nnoremap <leader>s <C-w>s
        nnoremap <leader>vsa :vert sba<cr>
        " bind Ctrl+<movement> keys to move around the windows, instead of using Ctrl+w + <movement>
        " Every unnecessary keystroke that can be saved is good for your health :)
        nnoremap <C-h> <C-w>h
        nnoremap <C-j> <C-w>j
        nnoremap <C-k> <C-w>k
        nnoremap <C-l> <C-w>l
    "}}}

    " Tab shorcuts
    map <Leader>n gT
    map <Leader>m gt
    map <leader>tn :tabnew<CR>
    map <leader>tc :tabclose<CR>

    " Better navigating through omnicomplete option list
    " See http://stackoverflow.com/questions/2170023/how-to-map-keys-for-popup-menu-in-vim
    function! OmniPopup(action)
        if pumvisible()
            if a:action == 'j'
                return "\<C-N>"
            elseif a:action == 'k'
                return "\<C-P>"
            endif
        endif
        return a:action
    endfunction

    inoremap <silent><C-j> <C-R>=OmniPopup('j')<CR>
    inoremap <silent><C-k> <C-R>=OmniPopup('k')<CR>
    inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
    function! s:my_cr_function()
        " return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
        " For no inserting <CR> key.
        return pumvisible() ? "\<C-y>" : "\<CR>"
    endfunction

    "inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
    inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
        \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

    inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
        \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>


"}}}

" autocmd {{{
    autocmd! bufwritepost .vimrc source %
    " go back to previous position of cursor if any
    autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \  exe 'normal! g`"zvzz' |
        \ endif

    autocmd FileType js,scss,css autocmd BufWritePre <buffer> call StripTrailingWhitespace()
    autocmd FileType css,scss setlocal foldmethod=marker foldmarker={,}
    autocmd FileType css,scss nnoremap <silent> <leader>S vi{:sort<CR>
    autocmd FileType python setlocal foldmethod=indent
    autocmd FileType markdown setlocal nolist
    autocmd FileType vim setlocal fdm=indent keywordprg=:help
"}}}

" color schemes {
    NeoBundle 'vim-scripts/Lucius'
    NeoBundle 'altercation/vim-colors-solarized' "{{{
        let g:solarized_termcolors=256
        let g:solarized_termtrans=1
        " if has('gui_running')
        "     let g:solarized_termcolors=256
        " else
        "     " set t_Co=16
        "     let g:solarized_termcolors=16
        " endif
    "}}}
    NeoBundle 'flazz/vim-colorschemes'
    NeoBundle 'nanotech/jellybeans.vim'
    NeoBundle 'tomasr/molokai'
    NeoBundle 'chriskempson/vim-tomorrow-theme'
    NeoBundle 'saghul/vim-colortoggle' "{{{
        let g:default_background_type = s:my_settings.background
    "}}}
" }

" finish loading {
    if exists('g:dotvim_settings.disabled_plugins')
        for plugin in g:dotvim_settings.disabled_plugins
            exec 'NeoBundleDisable '.plugin
        endfor
    endif

    call neobundle#end()         " required
    filetype plugin indent on    " required
    " To ignore plugin indent changes, instead use:
    " filetype plugin on
    "
    " Brief help
    " :NeoBundleList - list configured bundles
    " :NeoBundleInstall(!) - install (update) bundles
    " :NeoBundleClean(!) - confirm (or auto-approve) removal of unused bundles
    "
    " Refer to :help neobundle for more examples and for a full list of commands.
    " enable syntax highlighting
    syntax enable

    " Use 256 colours (Use this setting only if your terminal supports 256 colours)
    set t_Co=256
    let &background=s:my_settings.background
    exec 'colorscheme '.s:my_settings.colorscheme

    NeoBundleCheck
" }
