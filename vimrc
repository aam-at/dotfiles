" detect OS {
    let s:is_windows = has('win32') || has('win64')
    let s:is_cygwin = has('win32unix')
    let s:is_macvim = has('gui_macvim')
" }

" my settings {

    " initialize default settings
    let s:my_settings = {}
    let s:my_settings.cache_dir = '~/.vim/.cache'
    let s:my_settings.default_indent = 4
    let s:my_settings.max_column = 120
    let s:my_settings.autocomplete_method = 'neocomplcache'
    let s:my_settings.enable_cursorcolumn = 0
    let s:my_settings.colorscheme = 'lucius'
    let s:my_settings.background = 'light'
    if has('lua')
        let s:my_settings.autocomplete_method = 'neocomplete'
    elseif filereadable(expand("~/.vim/bundle/YouCompleteMe/python/ycm_core.*"))
        let s:my_settings.autocomplete_method = 'ycm'
    endif

    let s:my_settings.plugin_groups = ['core', 'python', 'programming', 'misc', 'windows']
    let s:my_settings.plugin_groups = []
    call add(s:my_settings.plugin_groups, 'core')
    call add(s:my_settings.plugin_groups, 'python')
    call add(s:my_settings.plugin_groups, 'scm')
    call add(s:my_settings.plugin_groups, 'editing')
    call add(s:my_settings.plugin_groups, 'indents')
    call add(s:my_settings.plugin_groups, 'navigation')
    call add(s:my_settings.plugin_groups, 'unite')
    call add(s:my_settings.plugin_groups, 'autocomplete')
    " call add(s:settings.plugin_groups, 'textobj')
    call add(s:my_settings.plugin_groups, 'misc')
    if s:is_windows
        call add(s:my_settings.plugin_groups, 'windows')
    endif
" }

" setup & neobundle {
    set nocompatible              " be iMproved, required
    filetype off                  " required
    if s:is_windows
        set rtp+=~/.vim
    endif
    " set the runtime path to include NeoBundle and initialize
    set rtp+=~/.vim/bundle/neobundle.vim
    call neobundle#begin(expand('~/.vim/bundle/'))
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


"""""""""""""""""""""""""
"
" PACKAGE MANAGEMENT
" 
"""""""""""""""""""""""""

" Core plugin
NeoBundle 'Shougo/vimproc.vim', {
            \ 'build' : {
            \     'windows' : 'tools\\update-dll-mingw',
            \     'cygwin' : 'make -f make_cygwin.mak',
            \     'mac' : 'make',
            \     'linux' : 'make',
            \     'unix' : 'gmake',
            \    },
            \ }
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/unite-outline'
NeoBundleLazy 'Shougo/vimshell.vim', {'autoload':{'commands':[ 'VimShell', 'VimShellInteractive' ]}} " {
    let g:vimshell_editor_command='mvim'
    let g:vimshell_right_prompt='getcwd()'
    let g:vimshell_data_directory=s:get_cache_dir('vimshell')
    let g:vimshell_vimshrc_path='~/.vim/vimshrc'

    nnoremap <leader>c :VimShell -split<cr>
    nnoremap <leader>cc :VimShell -split<cr>
    nnoremap <leader>cn :VimShellInteractive node<cr>
    nnoremap <leader>cl :VimShellInteractive lua<cr>
    nnoremap <leader>cr :VimShellInteractive irb<cr>
    nnoremap <leader>cp :VimShellInteractive python<cr>
" }

NeoBundle 'xolox/vim-session'

" NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'vim-scripts/Lucius'

" Togglable panels
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'Xuyuanp/nerdtree-git-plugin'
NeoBundle 'tpope/vim-vinegar'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'powerline/fonts'
NeoBundle 'bling/vim-airline'
NeoBundle 'vim-scripts/taglist.vim'
NeoBundle 'majutsushi/tagbar'

" Search in files {
    NeoBundle 'mileszs/ack.vim'
    NeoBundle 'rking/ag.vim'
    NeoBundle 'kien/ctrlp.vim'
    NeoBundle 'tacahiroy/ctrlp-funky'
" }

" Git wrapper for vim
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'gregsexton/gitv'

" Supertab plugin
NeoBundle 'ervandew/supertab'

" Better numbers for vim
NeoBundle 'myusuf3/numbers.vim'

" Vim Tmux navigation
NeoBundle 'christoomey/vim-tmux-navigator'

" Notes management {

    " NeoBundle 'vimwiki/vimwiki'
    " Vim + Evernote
    NeoBundle 'xolox/vim-notes'
    NeoBundle 'neilagabriel/vim-geeknote'

" }

" Surround plugin
NeoBundle 'tpope/vim-surround'

" Easymotion
NeoBundle 'easymotion/vim-easymotion'

" Plugins for writing {

    NeoBundle 'kana/vim-textobj-user'
    NeoBundle 'kana/vim-textobj-indent'
    NeoBundle 'reedes/vim-litecorrect'
    NeoBundle 'reedes/vim-textobj-sentence'
    NeoBundle 'reedes/vim-textobj-quote'
    NeoBundle 'reedes/vim-wordy'

" }

" Plugins for programming {

    " Commenting code
    NeoBundle 'scrooloose/nerdcommenter'

    " Undo window
    NeoBundle 'simnalamburt/vim-mundo'
    " Snippets
    NeoBundle 'MarcWeber/vim-addon-mw-utils'
    NeoBundle 'tomtom/tlib_vim'
    " Completion
    NeoBundle 'Shougo/neocomplete.vim'
    " Snippets for necomplete
    NeoBundle 'Shougo/neosnippet'
    NeoBundle 'Shougo/neosnippet-snippets'
    NeoBundle 'scrooloose/syntastic'
    NeoBundle 'godlygeek/tabular'
    NeoBundle 'kien/rainbow_parentheses.vim'

    " Python plugins {

        " NeoBundle 'nvie/vim-flake8' - same functionality to syntastic
        NeoBundle 'klen/python-mode'
        NeoBundle 'python-rope/ropevim'
        NeoBundle 'davidhalter/jedi-vim'
        NeoBundle 'heavenshell/vim-pydocstring'
        NeoBundle 'vim-scripts/python_match.vim'

    " }
    
    " Lua plugins {

        NeoBundle 'xolox/vim-misc'
        NeoBundle 'xolox/vim-lua-inspect'
        NeoBundle 'xolox/vim-lua-ftplugin'
        " NeoBundle 'WolfgangMehner/lua-support'
    
    " }
    
    " Misc plugins {
        NeoBundle 'plasticboy/vim-markdown'
    "}
    
    " Utility functions for vim programming
    NeoBundle 'L9'

" }



"""""""""""""""""""""""""
"
" SETTINGS & KEYBINDINGS
"
"""""""""""""""""""""""""

autocmd! bufwritepost .vimrc source %
" set guifont=Source\ Code\ Pro\ 12
" Python config
" enable syntax highlighting
set shell=/bin/bash

" TABs setting {

    " set tabs to have 4 spaces
    set tabstop=4
    set softtabstop=4
    " when using the >> or << commands, shift lines by 4 spaces
    set shiftwidth=4
    set shiftround
    " expand tabs into spaces
    set expandtab

" }

" allow backspacing over everything in insert mode
set backspace=indent,eol,start 
" indent when moving to the next line while writing code
set autoindent
" show a visual line under the cursor's current line
set cursorline
" show the matching part of the pair for [] {} and ()
set showmatch
set ruler

" Line numbers and text length {
    " show line numbers: better numbers plugin
    set number
    " set relativenumber
    set textwidth=79
    set nowrap  " don't automatically wrap on load
    set formatoptions-=t   " don't automatically wrap text when typing
    set colorcolumn=80
    highlight ColorColumn ctermbg=233
    " set foldmethod=indent
    " set foldlevel=99
"}

set list
set listchars=tab:▸\                " ┐
set listchars+=trail:·              " │ Use custom symbols to
" set listchars+=eol:↴              " │ represent invisible characters ¶
set listchars+=nbsp:_               " |
set listchars+=extends:»,precedes:« " ┘

" backup/persistance settings {

    set undodir=~/.vim/tmp/undo//
    set backupdir=~/.vim/tmp/backup//
    set directory=~/.vim/tmp/swap//
    set backupskip=/tmp/*,/private/tmp/*"
    set backup
    set writebackup
    " Disable swap files
    set noswapfile
    " persist (g)undo tree between sessions
    set undofile
    set history=700
    set undolevels=700

" }


" Easymotion colors for light colors {
    hi link EasyMotionTarget ErrorMsg
    hi link EasyMotionTarget2First Search
    hi link EasyMotionTarget2Second Search
    hi link EasyMotionShade  Comment
"}

" Rebind <Leader> key
" I like to have it here becuase it is easier to reach than the default and
" it is next to ``m`` and ``n`` which I use for navigating between tabs.
let mapleader = ","

" Learn vim hard way
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP> 
inoremap <Up> <NOP> 
inoremap <Down> <NOP> 
inoremap <Left> <NOP> 
inoremap <Right> <NOP> 

" This makes j and k work on "screen lines" instead of on "file lines"; now, when
" we have a long line that wraps to multiple screen lines, j and k behave as we
" expect them to.
nnoremap j gj
nnoremap k gk

" This is quit all
noremap <Leader>q :q<cr>
noremap <Leader>Q :qa<cr>

" bind Ctrl+<movement> keys to move around the windows, instead of using Ctrl+w + <movement>
" Every unnecessary keystroke that can be saved is good for your health :)
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

" Tab navigation
map <Leader>n gT
map <Leader>m gt

    
" Session management {

    let g:session_directory = "~/.vim/sessions"
    let g:session_autoload = "no"
    let g:session_autosave = "no"
    let g:session_command_aliases = 1
    nnoremap <leader>so :OpenSession
    nnoremap <leader>ss :SaveSession
    nnoremap <leader>sd :DeleteSession<CR>
    nnoremap <leader>sc :CloseSession<CR>

" }

" NERDTree key bindings
nmap <silent> <F5> :NERDTreeToggle<CR>

" Tag bar key binding
nmap <silent> <F4> :Tagbar<CR>

" Search plugins {

    " Ag plugin
    " let g:ag_prg="ag --column --smart-case"

    " CtrlP plugin {

        " let g:ctrlp_map = '<leader>t'

        " Use Vim's cwd
        let g:ctrlp_working_path_mode = 0
        let g:ctrlp_match_window = 'bottom,order:btt,min:1,max:15'

        " Faster indexing of files; requires having ag (AKA the_silver_searcher)
        " installed.
        let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden
            \ --ignore .git
            \ --ignore .svn
            \ --ignore .hg
            \ --ignore .DS_Store
            \ --ignore "**/*.pyc"
            \ -g ""'
    " }

    " CtrlP funky plugin
    nnoremap <Leader>fu :CtrlPFunky<Cr>
    " narrow the list down with a word under cursor
    nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>

" }

" Notes management {

    " let g:vimwiki_list = [{'path': '~/my_wiki/', 
    "                   \ 'syntax': 'markdown', 'ext': '.md'}]
    " vim-geeknote settings
    let g:GeeknoteFormat="markdown"
    let g:notes_directories = ['~/Dropbox/Notes']  

" }


" Writing plugins settings {

    " TextObj Sentence {
            augroup textobj_sentence
              autocmd!
              autocmd FileType markdown call textobj#sentence#init()
              autocmd FileType textile call textobj#sentence#init()
              autocmd FileType text call textobj#sentence#init()
            augroup END
    " }

    " TextObj Quote {
            augroup textobj_quote
                autocmd!
                autocmd FileType markdown call textobj#quote#init()
                autocmd FileType textile call textobj#quote#init()
                autocmd FileType text call textobj#quote#init({'educate': 0})
            augroup END
    " }

"}

"General programming plugins settings {

    " Neocomplete settings {

        " make neocomplcache use jedi#completions omini function for python scripts
        if !exists('g:neocomplcache_omni_functions')
            let g:neocomplcache_omni_functions = {}
            let g:neocomplcache_omni_functions['python'] = 'jedi#completions'
        endif
        " make Vim call omni function when below patterns matchs
        let g:neocomplcache_force_omni_patterns = {}
        let g:neocomplcache_force_omni_patterns.python = '[^. \t]\.\w*' 

    " }

    " YouCompleteMe plugin {
        let g:ycm_filetype_specific_completion_to_disable = { 'python' : 1 }
        let g:ycm_filetype_blacklist = { 'python' : 1 }
    " }

    " Syntactic plugin {

        set statusline+=%#warningmsg#
        set statusline+=%{SyntasticStatuslineFlag()}
        set statusline+=%*

        let g:syntastic_error_symbol = '✗'
        let g:syntastic_warning_symbol = '⚠'

        let g:syntastic_always_populate_loc_list = 1
        let g:syntastic_auto_loc_list = 1
        let g:syntastic_check_on_open = 1
        let g:syntastic_check_on_wq = 0
        let g:syntastic_loc_list_height = 7
        
        let g:syntastic_python_checkers = ['flake8', 'pep8']
        " avoid conflicts with python mode
        " Dysable syntastic for python. Currently use syntastic (some problems
        " with quickfix window in python mode)
        " let g:syntastic_mode_map = { 'passive_filetypes': ['python'] }
        " let g:syntastic_ignore_files = ['\.py$'] 

    " }

    " Gundo plugin {

        " f3 toggles the Gundo plugin window
        nnoremap <silent> <F3> :GundoToggle<CR>
        let g:gundo_width=80
        let g:gundo_right = 1

    " }
"}

" Python specific plugins settings {
    
    " Pymode plugin {
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
    " }

    " Jedi plugin {
        let g:jedi#popup_on_dot = 0
        let g:jedi#goto_command = "<leader>d"
        let g:jedi#goto_assignments_command = "<leader>g"
        let g:jedi#goto_definitions_command = ""
        let g:jedi#documentation_command = "K"
        let g:jedi#usages_command = "<leader>z"
        let g:jedi#completions_command = "<C-Space>"
        " disable <leader>r. Use rope-vim for refactoring
        let g:jedi#rename_command = ""
    " }

    " Supertab plugin {
        au FileType python set omnifunc=pythoncomplete#Complete
        let g:SuperTabDefaultCompletionType = "context"
        set completeopt=menuone,longest,preview
    " }

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
    inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
    inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
        \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

    inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
        \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>

    " Remap pydocstring
    nmap <silent> <C-_> <Plug>(pydocstring) 

" }

" Powerline/Airline plugin {

    set encoding=utf-8 " Necessary to show Unicode glyphs
    let g:airline_powerline_fonts = 1
    " let g:Powerline_symbols = 'fancy'
    " set rtp+=$HOME/.local/lib/python2.7/site-packages/powerline/bindings/vim/
    set laststatus=2
    set guifont=Sauce\ Code\ Powerline
    " set guifont=Menlo\ For\ Powerline
    " 
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

" }

" fresh ubuntu installation {
    " sudo apt-get update
    " sudo apt-get upgrade -y
    " sudo apt-get install vim-gtk cmake build-essential silversearcher-i fish -y
    " install vundle
    " git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
    " vim +PluginInstall
    " Compile and configure vim to use anaconda python2.7
    " sudo apt-get install liblua5.2-dev ruby-dev ctags
    " pipi install ropevim
    " ./configure --with-features=huge --enable-rubyinterp \
    "             --enable-pythoninterp --with-python-config-dir=$HOME/anaconda/lib/python2.7/config \
    "             --enable-gui=gtk --enable-cscope \
    "             --enable-luainterp --with-luajit --with-lua-prefix=$HOME/local/lua_packages/torch/install/bin/th \
    "             --enable-fail-if-missing --prefix=$HOME/opt/vim
    " make install
    " sudo update-alternatives --install /usr/bin/editor editor /usr/local/bin/vim 0
    " http://stackoverflow.com/questions/26956933/how-to-make-vim74-compile-with-python
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
    "filetype plugin on
    "
    " Brief help
    " :NeoBundleList - list configured bundles
    " :NeoBundleInstall(!) - install (update) bundles
    " :NeoBundleClean(!) - confirm (or auto-approve) removal of unused bundles
    "
    " Refer to :help neobundle for more examples and for a full list of commands.
    " Put your non-Plugin stuff after this line
      syntax enable

    " Use 256 colours (Use this setting only if your terminal supports 256 colours)
    set t_Co=256
    let &background=s:my_settings.background
    exec 'colorscheme '.s:my_settings.colorscheme

    NeoBundleCheck
" }
