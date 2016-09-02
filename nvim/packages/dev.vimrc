call neobundle#append()
" plugins {{{
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
  " commenting code
  NeoBundle 'scrooloose/nerdcommenter'
  NeoBundle 'kien/rainbow_parentheses.vim'
  NeoBundleLazy 'majutsushi/tagbar' , {'autoload':{'commands':'TagbarToggle'}} "{{{
    nnoremap <silent> <F4> :TagbarToggle<CR>
  "}}}
  " supertab plugin
  NeoBundle 'ervandew/supertab' "{{{
    au FileType python set omnifunc=pythoncomplete#Complete
    let g:SuperTabDefaultCompletionType = "context"
    set completeopt=menuone,longest,preview
  "}}}
  " protobuf highlighting
  NeoBundle 'uarun/vim-protobuf'
  " display indent levels in code
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
"}}}
call neobundle#end()
