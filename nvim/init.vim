" detect OS {{{
    let g:is_windows = has('win32') || has('win64')
    let g:is_cygwin = has('win32unix')
    let g:is_macvim = has('gui_macvim')
"}}}

" my settings {
    " initialize default settings
    let $NVIM_HOME=$HOME."/.config/nvim"
    let g:my_settings = {}
    let g:my_settings.cache_dir = $NVIM_HOME. '/.cache'
    let g:my_settings.default_indent = 4
    let g:my_settings.max_column = 120
    let g:my_settings.autocomplete_method = 'neocomplcache'
    let g:my_settings.enable_cursorcolumn = 0
    let g:my_settings.colorscheme = 'solarized'
    let g:my_settings.background = 'dark'
    if has('lua')
        let g:my_settings.autocomplete_method = 'neocomplete'
    elseif filereadable($NVIM_HOME."./bundle/YouCompleteMe/python/ycm_core.*")
        let g:my_settings.autocomplete_method = 'ycm'
    endif

    let g:my_packages = []
    call add(g:my_packages, 'core')
    call add(g:my_packages, 'navigation')
    call add(g:my_packages, 'vim')
    call add(g:my_packages, 'dev')
    call add(g:my_packages, 'autocomplete')
    call add(g:my_packages, 'python')
    call add(g:my_packages, 'lua')
    call add(g:my_packages, 'git')
    call add(g:my_packages, 'editing')
    call add(g:my_packages, 'writing')
    call add(g:my_packages, 'latex')
    call add(g:my_packages, 'markdown')
    call add(g:my_packages, 'neobundle')
    call add(g:my_packages, 'tools')
    if g:is_windows
        call add(g:my_packages, 'windows')
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
        exec 'set runtimepath+='.$NVIM_HOME.'/bundle/neobundle.vim'
    endif
    if g:is_windows
        set rtp+=~/.config/nvim
    endif
    call neobundle#begin($NVIM_HOME.'/bundle/')
    " Let NeoBundle manage NeoBundle
    " Required:
    NeoBundleFetch 'Shougo/neobundle.vim'
" }

" base configuration {{{
  set noshelltemp                                     "use pipes

  let mapleader = ","
  let g:mapleader = ","
  let maplocalleader = "\\"
  let g:maplocalleader = "\\"
  nmap <space> [unite]
  nnoremap [unite] <nop>
  nmap <LocalLeader> [menu]
  nnoremap [menu] <nop>

  " python configuration {{{
      let g:python_host_prog = $HOME.'/.pyenv/versions/neovim2/bin/python'
      let g:python3_host_prog = $HOME.'/.pyenv/versions/neovim3/bin/python'
  " }}}
"}}}


""""""""""""""""""""""""""
"                        "
"     BASE PACKAGES      "
"                        "
"""""""""""""""""""""""""" {{{

" vimproc to asynchronously run commands (NeoBundle, Unite)
NeoBundle 'Shougo/vimproc', {
    \ 'build' : {
    \     'windows': '"C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\bin\nmake.exe" make_msvc32.mak',
    \     'cygwin' : 'make -f make_cygwin.mak',
    \     'mac' : 'make -f make_mac.mak',
    \     'unix' : 'make -f make_unix.mak',
    \    },
  \ }

" unite. The interface to rule almost everything
NeoBundle 'Shougo/unite.vim' "{{{
  let g:unite_source_menu_menus = {}
  " menus menu
  nnoremap <silent> <LocalLeader>u :<C-u>Unite -silent -winheight=20 menu<cr>

  let bundle = neobundle#get('unite.vim')
  function! bundle.hooks.on_source(bundle)
    call unite#filters#matcher_default#use(['matcher_fuzzy'])
    call unite#filters#sorter_default#use(['sorter_rank'])
    call unite#custom#profile('default', 'context', {
                \ 'start_insert': 1
                \ })
  endfunction

  let g:unite_data_directory=utils#GetCacheDir('unite')
  let g:unite_source_history_yank_enable=1
  let g:unite_source_rec_max_cache_files=5000

  if executable('ag')
    let g:unite_source_grep_command='ag'
    let g:unite_source_grep_default_opts='--nocolor --nogroup -s'
    let g:unite_source_grep_recursive_opt=''
  elseif executable('pt')
    " use pt in unite grep source.
    " https://github.com/monochromegane/the_platinum_searcher
    let g:unite_source_grep_command = 'pt'
    let g:unite_source_grep_default_opts='--nocolor --nogroup -s'
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

  nnoremap <silent> [unite]l :<C-u>Unite -auto-resize -buffer-name=line line<cr>
  nnoremap <silent> [unite]/ :<C-u>Unite -no-quit -buffer-name=search grep:.<cr>
  nnoremap <silent> [unite]m :<C-u>Unite -auto-resize -buffer-name=mappings mapping<cr>
  nnoremap <silent> [unite]s :<C-u>Unite -quick-match buffer<cr>
  nnoremap <silent> [unite]T :<C-u>Unite -auto-resize -buffer-name=tabs tab<CR>
"}}}

" Unite sources
NeoBundle 'Shougo/neomru.vim', {'autoload':{'unite_sources':
            \['file_mru', 'directory_mru']}} "{{{
  let g:neomru#file_mru_path = g:my_settings.cache_dir.'/neomru/file'
  let g:neomru#directory_mru_path = g:my_settings.cache_dir.'/neomru/directory'

  if g:is_windows
    nnoremap <silent> [unite]<space> :<C-u>Unite -toggle -auto-resize -buffer-name=mixed file_rec:! buffer file_mru bookmark<cr><c-u>
    nnoremap <silent> [unite]f :<C-u>Unite -toggle -auto-resize -buffer-name=files file_rec:!<cr><c-u>
  else
    nnoremap <silent> [unite]<space> :<C-u>Unite -toggle -auto-resize -buffer-name=mixed file_rec/async:! buffer file_mru bookmark<cr><c-u>
    nnoremap <silent> [unite]f :<C-u>Unite -toggle -auto-resize -buffer-name=files file_rec/async:!<cr><c-u>
  endif
  nnoremap <silent> [unite]b :<C-u>Unite -auto-resize -buffer-name=buffers buffer file_mru<cr>
  nnoremap <silent> [unite]e :<C-u>Unite -buffer-name=recent file_mru<cr>
"}}}
NeoBundle 'Shougo/unite-outline', {'autoload':{'unite_sources':'outline'}} "{{{
  nnoremap <silent> [unite]o :<C-u>Unite -auto-resize -buffer-name=outline outline<cr>
"}}}
NeoBundle 'Shougo/neoyank.vim', {'autoload':{'unite_sources':'history_yank'}} "{{{
  nnoremap <silent> [unite]y :<C-u>Unite -buffer-name=yanks history/yank<cr>
"}}}
NeoBundle 'Shougo/unite-help', {'autoload':{'unite_sources':'help'}} "{{{
  nnoremap <silent> [unite]h :<C-u>Unite -auto-resize -buffer-name=help help<cr>
"}}}
NeoBundle 'tsukkee/unite-tag', {'autoload':{'unite_sources':['tag','tag/file']}} "{{{
  nnoremap <silent> [unite]t :<C-u>Unite -auto-resize -buffer-name=tag tag tag/file<cr>
"}}}
NeoBundle 'ujihisa/unite-colorscheme', {'autoload':{'unite_sources':'colorscheme'}} "{{{
  nnoremap <silent> [unite]c :<C-u>Unite -winheight=10 -auto-preview -buffer-name=colorschemes colorscheme<cr>
"}}}
NeoBundle 'osyo-manga/unite-airline_themes', {'autoload':{'unite_sources':'airline_themes'}} "{{{
  nnoremap <silent> [unite]a :<C-u>Unite -winheight=10 -auto-preview -buffer-name=airline_themes airline_themes<cr>
"}}}
" junk files
NeoBundle 'Shougo/junkfile.vim', {'autoload':{'unite_sources':'junkfile'}} "{{{
  let g:junkfile#directory=utils#GetCacheDir('junk')
  nnoremap <silent> [unite]j :<C-u>Unite -auto-resize -buffer-name=junk junkfile<cr>
"}}}
NeoBundle 'ujihisa/unite-locate', {'autoload':{'unite_sources':'locate'}}
NeoBundle 'thinca/vim-unite-history', { 'autoload' : { 'unite_sources' :
            \ ['history/command', 'history/search']}}
NeoBundle 'osyo-manga/unite-filetype', { 'autoload' : {'unite_sources' :
            \ 'filetype', }}
NeoBundle 'osyo-manga/unite-quickfix', {'autoload':{'unite_sources':
            \ ['quickfix', 'location_list']}}
NeoBundle 'osyo-manga/unite-fold', {'autoload':{'unite_sources':'fold'}}
NeoBundle 'tacroe/unite-mark', {'autoload':{'unite_sources':'mark'}}

" color schemes {{{
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
    let g:default_background_type = g:my_settings.background
  "}}}

  " dark themes
  " improved terminal version of molokai, almost identical to the GUI one
  NeoBundle 'joedicastro/vim-molokai256'

  NeoBundle 'tomasr/molokai'
  NeoBundleLazy 'sjl/badwolf', { 'autoload' : { 'unite_sources' : 'colorscheme', }}
  NeoBundleLazy 'nielsmadan/harlequin', { 'autoload' : { 'unite_sources' : 'colorscheme', }}

  " light themes
  NeoBundleLazy 'vim-scripts/summerfruit256.vim', { 'autoload' : { 'unite_sources' : 'colorscheme', }}
  NeoBundleLazy 'joedicastro/vim-github256', { 'autoload' : { 'unite_sources' : 'colorscheme', }}

  " make terminal themes from GUI themes
  NeoBundleLazy 'godlygeek/csapprox', { 'autoload' : { 'commands' : ['CSApprox', 'CSApproxSnapshot']}}
"}}}

" GUI {{{
  NeoBundle 'bling/vim-airline' "{{{
    set noshowmode

    let g:airline_theme='powerlineish'
    let g:airline_powerline_fonts=1
    let g:airline#extensions#branch#enabled=1
    let g:airline#extensions#whitespace#enabled = 1
    let g:airline#extensions#hunks#non_zero_only = 1
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
  NeoBundle 'vim-airline/vim-airline-themes'
  NeoBundleLazy 'zhaocai/GoldenView.Vim', {'autoload':{'mappings':['<Plug>ToggleGoldenViewAutoResize']}} "{{{
      let g:goldenview__enable_default_mapping=0
      nmap <F8> <Plug>ToggleGoldenViewAutoResize
  "}}}
"}}}

" finish loading {{{
  if exists('g:my_settings.disabled_plugins')
    for plugin in g:my_settings.disabled_plugins
        exec 'NeoBundleDisable '.plugin
    endfor
  endif
  call neobundle#end()         " required

  " load packages {{{
    exec ':so ' $NVIM_HOME."/autoload/helperfuncs.vim"
    for package in g:my_packages
      let package_path = $NVIM_HOME."/packages/" . package . ".vimrc"
      if filereadable(package_path)
        exec ':so ' package_path
      endif
    endfor
  "}}}
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
  let &background=g:my_settings.background
  exec 'colorscheme '.g:my_settings.colorscheme

  NeoBundleCheck
"}}}
