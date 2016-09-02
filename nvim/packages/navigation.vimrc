" vim setup {{{

  " save as root
  cmap w!! w !sudo tee % >/dev/null<CR>:e!<CR><CR>

  " quick saving
  nmap <silent> <Leader>w :update<CR>

  " use ranger as a file explorer {{{
  fun! RangerChooser()
    exec "silent !ranger --choosefile=/tmp/chosenfile " . expand("%:p:h")
    if filereadable('/tmp/chosenfile')
      exec 'edit ' . system('cat /tmp/chosenfile')
      call system('rm /tmp/chosenfile')
    endif
    redraw!
  endfun
  map <Leader>x :call RangerChooser()<CR>
  "}}}
"}}}

call neobundle#append()
" plugins {{{
  NeoBundle 'mileszs/ack.vim' "{{{
    if executable('ag')
      let g:ackprg = "ag --nogroup --column --smart-case --follow"
    endif
  "}}}
  " ag can be installed on windows with https://chocolatey.org/
  " pt written in go. works everywhere.
  NeoBundle 'rking/ag.vim' "{{{
    if !executable('ag') && executable('pt')
      let g:ag_prg = "pt --nogroup --column --smart-case --follow"
    endif
  "}}}
  " undo window
  NeoBundleLazy 'simnalamburt/vim-mundo', {'autoload':{'commands': 'GundoToggle'}} "{{{
    " f3 toggles the Gundo plugin window
    nnoremap <silent> <F3> :MundoToggle<CR>
    let g:mundo_width=80
    let g:mundo_right = 1
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
    let g:ctrlp_cache_dir=GetCacheDir('ctrlp')
    let g:ctrlp_reuse_window='startify'
    let g:ctrlp_extensions=['funky']
    let g:ctrlp_custom_ignore = {
                \ 'dir': '\v[\/]\.(git|hg|svn|idea)$',
                \ 'file': '\v\.(DS_Store|exe|so|dll|pyc)$'
                \ }

    if executable('ag')
      " faster indexing of files; requires having ag (AKA the_silver_searcher)
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
    let NERDTreeBookmarksFile=GetCacheDir('NERDTreeBookmarks')
    " NERDTree key bindings
    nmap <silent> <F5> :NERDTreeToggle<CR>
  "}}}
  NeoBundle 'jistr/vim-nerdtree-tabs'
  NeoBundle 'Xuyuanp/nerdtree-git-plugin'
  NeoBundle 'tpope/vim-vinegar'
  NeoBundle 'MattesGroeger/vim-bookmarks'
  " file explorer (needed where ranger is not available)
  NeoBundleLazy 'Shougo/vimfiler', {'autoload' : { 'commands' : ['VimFiler']}} "{{{
    nnoremap <silent><Leader>X :VimFiler<CR>
    let g:vimfiler_as_default_explorer = 1
    let g:vimfiler_tree_leaf_icon = '├'
    let g:vimfiler_tree_opened_icon = '┐'
    let g:vimfiler_tree_closed_icon = '─'
    let g:vimfiler_file_icon = '┄'
    let g:vimfiler_marked_file_icon = '✓'
    let g:vimfiler_readonly_file_icon = '✗'
    let g:vimfiler_force_overwrite_statusline = 0
    let g:vimfiler_time_format = '%d-%m-%Y %H:%M:%S'
    let g:vimfiler_data_directory = GetCacheDir('vimfiler')
  "}}}
  " a diff tool for directories
  NeoBundleLazy 'joedicastro/DirDiff.vim', { 'autoload': { 'commands' : 'DirDiff'}}

"}}}
call neobundle#end()

" unite menu {{{
  let g:unite_source_menu_menus.files = {
      \ 'description' : '          files & dirs
      \                                          ⌘ [space]o',
    \}
  let g:unite_source_menu_menus.files.command_candidates = [
      \['▷ open file                                                  ⌘ <Leader>o',
          \'Unite -start-insert file'],
      \['▷ open more recently used files                              ⌘ <Leader>m',
          \'Unite file_mru'],
      \['▷ open file with recursive search                            ⌘ <Leader>O',
          \'Unite -start-insert file_rec/async'],
      \['▷ edit new file',
          \'Unite file/new'],
      \['▷ search directory',
          \'Unite directory'],
      \['▷ search recently used directories',
          \'Unite directory_mru'],
      \['▷ search directory with recursive search',
          \'Unite directory_rec/async'],
      \['▷ make new directory',
          \'Unite directory/new'],
      \['▷ change working directory',
          \'Unite -default-action=lcd directory'],
      \['▷ know current working directory',
          \'Unite output:pwd'],
      \['▷ junk files                                                 ⌘ <Leader>d',
          \'Unite junkfile/new junkfile'],
      \['▷ save as root                                               ⌘ :w!!',
          \'exe "write !sudo tee % >/dev/null"'],
      \['▷ quick save                                                 ⌘ <Leader>w',
          \'normal <Leader>w'],
      \['▷ open ranger                                                ⌘ <Leader>x',
          \'call RangerChooser()'],
      \['▷ open vimfiler                                              ⌘ <Leader>X',
          \'VimFiler'],
    \]

  let g:unite_source_menu_menus.files.command_candidates = helperfuncs#unite_menu_gen(g:unite_source_menu_menus.files.command_candidates, [])

  nnoremap <silent>[menu]o :Unite -silent -winheight=17 -start-insert
              \ menu:files<CR>
"}}}
