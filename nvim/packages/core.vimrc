" VIM Setup {{{

  " base options {{{
    scriptencoding utf-8
    set encoding=utf-8                                  "set encoding for text
    set title                       " set the terminal title to the current file
    if exists('$TMUX')
        set clipboard=
    else
        set clipboard=unnamed                           "sync with OS clipboard
    endif
    set hidden                                          "allow buffer switching without saving
    set fileformats+=mac                                "add mac to auto-detection of file format line endings
    set nrformats-=octal                                "always assume decimal numbers
    set tags=tags;/
    set showfulltag
    set showcmd                     " shows partial commands
    set autoread                                        "auto reload if file saved externally
    set timeoutlen=300                                  "mapping timeout
    set ttimeoutlen=50                                  "keycode timeout
    set ttyfast                                         "assume fast terminal connection
    set virtualedit=all             " to edit where there is no actual character

    if g:is_windows && !g:is_cygwin
      " ensure correct shell in gvim
      set shell=c:\windows\system32\cmd.exe
    endif

    if $SHELL =~ '/fish$'
      " VIM expects to be run from a POSIX shell.
      set shell=sh
    endif

    if exists('$TMUX')
      set clipboard=
    else
      set clipboard=unnamed                           "sync with OS clipboard
    endif

    " Never goback to compatible mode
    nnoremap Q <nop>
  "}}}

  " ui configuration {{{
    set viewoptions=folds,options,cursor,unix,slash     "unix/windows compatibility
    set ls=2                        " status line always visible
    set go-=T                       " hide the toolbar
    set go-=m                       " hide the menu
    " The next two lines are quite tricky, but in Gvim, if you don't do this, if you
    " only hide all the scrollbars, the vertical scrollbar is showed anyway
    set go+=rRlLbh                  " show all the scrollbars
    set go-=rRlLbh                  " hide all the scrollbars
    set fillchars+=vert:│           " better looking for windows separator
    
    set ruler                       " sets a permanent rule
    set number
    set lazyredraw                  " only redraws if it is needed
    set laststatus=2
    set noshowmode
    set modeline
    set modelines=5
    
    set scrolloff=1                                     "always show content after scroll
    set scrolljump=5                                    "minimum number of lines to scroll
    set display+=lastline
    
    set splitbelow
    set splitright
    
    " disable sounds
    set noerrorbells
    set novisualbell
    set t_vb=
    " set termguicolors
    " :let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1              " change cursor shape (does not work with guake)

    set cursorline
    autocmd WinLeave * setlocal nocursorline
    autocmd WinEnter * setlocal cursorline
    let &colorcolumn=g:my_settings.max_column
    highlight ColorColumn ctermbg=233
    if g:my_settings.enable_cursorcolumn
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
      if g:is_windows
        autocmd GUIEnter * simalt ~x
      endif

      set guioptions+=t                                 "tear off menu items
      set guioptions-=T                                 "toolbar icons
      set guioptions-=r
      set guioptions-=L

      if g:is_macvim
          "set gfn=Ubuntu_Mono:h14
        set transparency=2
      endif

      if g:is_windows
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
  
  " tabs, space and wrapping {{{
    set backspace=indent,eol,start                      "allow backspacing everything in insert mode
    set autoindent                                      "automatically indent to match adjacent lines
    set expandtab                                       "spaces instead of tabs
    set smarttab                                        "use shiftwidth to enter tabs
    let &tabstop=g:my_settings.default_indent           "number of spaces per tab for display
    let &softtabstop=g:my_settings.default_indent       "number of spaces per tab in insert mode
    let &shiftwidth=g:my_settings.default_indent        "number of spaces when indenting
    set list                                            "highlight whitespace
    set listchars=tab:▸\ ,trail:•,extends:»,precedes:«
    set shiftround
    set linebreak
    let &showbreak='↪ '

    " set formatoptions=qrn1ct
    set textwidth=80
    set colorcolumn=81
    set nowrap                                          "don't automatically wrap on load
    set formatoptions-=t                                "don't automatically wrap text when typing

    function! ToggleWrap()
      let s:nowrap_cc_bg = [22, '#005f00']
      redir => s:curr_cc_hi
      silent hi ColorColumn
      redir END
      let s:curr_cc_ctermbg = matchstr(s:curr_cc_hi, 'ctermbg=\zs.\{-}\s\ze\1')
      let s:curr_cc_guibg = matchstr(s:curr_cc_hi, 'guibg=\zs.\{-}\_$\ze\1')
      if s:curr_cc_ctermbg != s:nowrap_cc_bg[0]
        let g:curr_cc_ctermbg = s:curr_cc_ctermbg
      endif
      if s:curr_cc_guibg != s:nowrap_cc_bg[1]
        let g:curr_cc_guibg = s:curr_cc_guibg
      endif
      if &textwidth == 80
        set textwidth=0
        exec 'hi ColorColumn ctermbg='.s:nowrap_cc_bg[0].
                    \' guibg='.s:nowrap_cc_bg[1]
      elseif &textwidth == 0
        set textwidth=80
        exec 'hi ColorColumn ctermbg='.g:curr_cc_ctermbg.
                    \' guibg='.g:curr_cc_guibg
      endif
    endfunction

    nmap <silent><Leader>ew :call ToggleWrap()<CR>
  "}}}

  " folding {{{
    set foldenable                                      "enable folds by default
    set foldmethod=syntax                               "fold via syntax of files
    set foldlevelstart=99                               "open all folds by default
    let g:xml_syntax_folding=1                          "enable xml folding
  "}}}
  
  " searching {{{
    set hlsearch                                      "highlight searches
    set incsearch                                     "incremental searching
    set showmatch                                     "show pairs match
    set matchtime=2                                   "tens of a second to show matching parentheses
    set ignorecase                                    "ignore case for searching
    set smartcase                                     "do case-sensitive if there's a capital letter
    set ignorecase                                    "ignore case letters
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
  " }}}
  
  " History and permanent undo levels {{{
    set history=1000                                  "number of command lines to remember
    set undolevels=1000
    set undofile
    set undoreload=1000
  "}}}

  " backups {{{
    set viminfo+=n$NVIM_HOME/.cache/viminfo
    " persistent undo
    if exists('+undofile')
        set undofile
        let &undodir = GetCacheDir('undo')
    endif

    " backups
    set backup
    let &backupdir = GetCacheDir('backup')
    set backupskip=/tmp/*,/private/tmp/*"

    " swap files
    set noswapfile
    let &directory = GetCacheDir('swap')
    set writebackup

    call EnsureExists(g:my_settings.cache_dir)
    call EnsureExists(&undodir)
    call EnsureExists(&backupdir)
    call EnsureExists(&directory)
  " }}}

  " wildmenu {{{
    set wildmenu                        " Command line autocompletion
    set wildignorecase
    set wildmode=list:longest,full      " Shows all the options
    set wildignore+=*.sw?                            " Vim swap files
    set wildignore+=*.bak,*.?~,*.??~,*.???~,*.~      " Backup files
    set wildignore+=*.luac                           " Lua byte code
    set wildignore+=*.jar                            " java archives
    set wildignore+=*.pyc                            " Python byte code
    set wildignore+=*.stats                          " Pylint stats
  " }}}

  " colorscheme {{{
    syntax enable                  " enable the syntax highlight
    set background=dark            " set a dark background
    set t_Co=256                   " 256 colors for the terminal
    if has('gui_running')
      colorscheme molokai
    else
      colorscheme molokai256
    endif
  " }}}

  " resize the divisions if the Vim window size changes {{{
    au VimResized * exe "normal! \<c-w>="
  "}}}

  " New windows {{{
    nnoremap <Leader>v <C-w>v
    nnoremap <Leader>h <C-w>s
  " }}}

  " Fast window moves {{{
    nnoremap <C-h> <C-w>h
    nnoremap <C-j> <C-w>j
    nnoremap <C-k> <C-w>k
    nnoremap <C-l> <C-w>l
  " }}}

  " Fast window & buffer close and kill {{{
    nnoremap <Leader>k <C-w>c
    nnoremap <silent><Leader>K :bd<CR>
  " }}}

  " Autoload configuration when this file changes ($MYVIMRC) {{{
    autocmd! BufWritePost vimrc source %
    autocmd! BufWritePost *.vimrc source ~/.vimrc
  " }}}

  " Toggle the Quickfix window {{{
    function! s:QuickfixToggle()
      for i in range(1, winnr('$'))
        let bnum = winbufnr(i)
        if getbufvar(bnum, '&buftype') == 'quickfix'
          cclose
          lclose
          return
        endif
      endfor
      copen
    endfunction
    command! ToggleQuickfix call <SID>QuickfixToggle()
    nnoremap <silent> <Leader>q :ToggleQuickfix<CR>
  " }}}

  " Quick exiting without save {{{
    nnoremap <Leader>`` :qa!<CR>
  "}}}

  " Execution permissions by default to shebang (#!) files {{{
  augroup shebang_chmod
    autocmd!
    autocmd BufNewFile  * let b:brand_new_file = 1
    autocmd BufWritePost * unlet! b:brand_new_file
    autocmd BufWritePre *
          \ if exists('b:brand_new_file') |
          \   if getline(1) =~ '^#!' |
          \     let b:chmod_post = '+x' |
          \   endif |
          \ endif
    autocmd BufWritePost,FileWritePost *
          \ if exists('b:chmod_post') && executable('chmod') |
          \   silent! execute '!chmod '.b:chmod_post.' "<afile>"' |
          \   unlet b:chmod_post |
          \ endif
  augroup END

  " }}}

  " Load matchit by default {{{
    if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
      runtime! macros/matchit.vim
    endif
  " }}}

  " Make the Y behavior similar to D & C {{{
    nnoremap Y y$
  " }}}

  " mappings {{{
    " Learn vim hard way "{{{
      noremap <Up> <NOP>
      noremap <Down> <NOP>
      noremap <Left> <NOP>
      noremap <Right> <NOP>
      inoremap <Up> <NOP>
      inoremap <Down> <NOP>
      inoremap <Left> <NOP>
      inoremap <Right> <NOP>
    "}}}

    " formatting shortcuts {{{
      nmap <leader>fef :call Preserve("normal gg=G")<CR>
      nmap <leader>f$ :call StripTrailingWhitespace()<CR>
      vmap <leader>s :sort<cr>
    "}}}

    " shorcuts for quick save and quit {{{
      nnoremap <leader>w :w<cr>
      noremap <Leader>q :q<cr>
      noremap <Leader>Q :qa<cr>
    "}}}

    " toggle paste
    map <F6> :set invpaste<CR>:set paste?<CR>

    " remap arrow keys {{{
      nnoremap <left> :bprev<CR>
      nnoremap <right> :bnext<CR>
      nnoremap <up> :tabnext<CR>
      nnoremap <down> :tabprev<CR>
    "}}}

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

"}}}

call neobundle#append()
" plugins {{{
  " better numbers for vim
  NeoBundle 'myusuf3/numbers.vim'
  " fancy start screen for vim
  NeoBundle 'mhinz/vim-startify' "{{{
      let g:startify_session_dir = GetCacheDir('sessions')
      let g:startify_change_to_vcs_root = 1
      let g:startify_show_sessions = 1
      nnoremap <F1> :Startify<cr>
  "}}}
"}}}
call neobundle#end()
