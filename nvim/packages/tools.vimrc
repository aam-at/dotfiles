call neobundle#append()
" plugins {{{
  " open a url into the browser or another files with an external app
  NeoBundle 'vim-scripts/utl.vim' "{{{
    map <Leader>j :Utl <CR><Bar>:redraw!<CR>
    " For mac only
    let g:utl_cfg_hdl_scm_http_system = "silent !firefox %u &"
    let g:utl_cfg_hdl_mt_application_pdf = 'silent :!zathura %p &'
    let g:utl_cfg_hdl_mt_image_jpeg = 'silent :!sxiv %p &'
    let g:utl_cfg_hdl_mt_image_gif = 'silent :!sxiv %p &'
    let g:utl_cfg_hdl_mt_image_png = 'silent :!sxiv %p &'
  "}}}

  " tmux {{{
  if exists('$TMUX')
    NeoBundle 'christoomey/vim-tmux-navigator'
    " easily interacts with Tmux from Vim
    NeoBundle 'benmills/vimux'
    NeoBundle 'julienr/vim-cellmode', {'autoload':{'filetypes':['python']}}
    " tmux config file syntax
    NeoBundleLazy 'vimez/vim-tmux', { 'autoload' : { 'filetypes' : 'conf'}}
  endif
  "}}}
  "task {{{
  if executable('task')
      NeoBundle 'blindFS/vim-taskwarrior' "{{{
          let g:task_log_directory   = '~/.task'
      "}}}
      NeoBundle 'Spirotot/taskwiki', {'depends' : 'vimwiki/vimwiki'}
      NeoBundle 'powerman/vim-plugin-AnsiEsc'
  endif
  "}}}
  NeoBundle 'wakatime/vim-wakatime'
  " a vim plugin for vim plugins
  NeoBundleLazy 'tpope/vim-scriptease', {'autoload':{'filetypes':['vim']}}
  NeoBundleLazy 'guns/xterm-color-table.vim', {'autoload':{'commands':'XtermColorTable'}}
  NeoBundle 'bufkill.vim'
  NeoBundleLazy 'krisajenkins/vim-pipe'
  NeoBundleLazy 'Shougo/vimshell.vim', {'autoload':{'commands':[ 'VimShell', 'VimShellInteractive' ]}} "{{{
    if g:is_macvim
        let g:vimshell_editor_command='mvim'
    else
        let g:vimshell_editor_command='vim'
    endif
    let g:vimshell_right_prompt='getcwd()'
    let g:vimshell_data_directory=utils#GetCacheDir('vimshell')
    let g:vimshell_vimshrc_path='~/.config/nvim/vimshrc'

    nnoremap <localleader>c :VimShell -split<cr>
    nnoremap <localleader>cc :VimShell -split<cr>
    nnoremap <localleader>cn :VimShellInteractive node<cr>
    nnoremap <localleader>cl :VimShellInteractive lua<cr>
    nnoremap <localleader>cr :VimShellInteractive irb<cr>
    nnoremap <localleader>cp :VimShellInteractive python<cr>
  "}}}
  " if I'm lazy to wait for emacs launch, for quick edit
  NeoBundle 'jceb/vim-orgmode'
  " vim faq just for fun
  NeoBundle 'chrisbra/vim_faq'
  " download powerline fonts
  NeoBundle 'powerline/fonts'
"}}}
call neobundle#end()

" unite menu {{{
  let g:unite_source_menu_menus.tools = {
      \ 'description' : '          other tools
      \                                           ⌘ [space]t',
    \}
  let g:unite_source_menu_menus.tools.command_candidates = [
      \['▷ run with python2 in tmux panel             (vimux)         ⌘ <Leader>rr',
          \'normal ,rr'],
      \['▷ run with python3 in tmux panel             (vimux)         ⌘ <Leader>r3',
          \'normal ,r3'],
      \['▷ run with python2 & time in tmux panel      (vimux)         ⌘ <Leader>rt',
          \'normal ,rt'],
      \['▷ run with pypy & time in tmux panel         (vimux)         ⌘ <Leader>rp',
          \'normal ,rp'],
      \['▷ command prompt to run in a tmux panel      (vimux)         ⌘ <Leader>rc',
          \'VimuxPromptCommand'],
      \['▷ repeat last command                        (vimux)         ⌘ <Leader>rl',
          \'VimuxRunLastCommand'],
      \['▷ stop command execution in tmux panel       (vimux)         ⌘ <Leader>rs',
          \'VimuxInterruptRunner'],
      \['▷ inspect tmux panel                         (vimux)         ⌘ <Leader>ri',
          \'VimuxInspectRunner'],
      \['▷ close tmux panel                           (vimux)         ⌘ <Leader>rq',
          \'VimuxCloseRunner'],
      \['▷ open link                                                  ⌘ <Leader>j',
          \'normal ,j'],
    \]
  let g:unite_source_menu_menus.tools.command_candidates = helperfuncs#unite_menu_gen(g:unite_source_menu_menus.tools.command_candidates, [])

  nnoremap <silent>[menu]t :Unite -silent menu:tools<CR>
"}}}
