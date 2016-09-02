call neobundle#append()
" plugins {{{
  NeoBundleLazy 'lambdalisue/vim-pyenv', {
    \ 'depends': ['davidhalter/jedi-vim'],
    \ 'autoload': {
    \   'filetypes': ['python', 'python3'],
  \ }}

  NeoBundleLazy 'klen/python-mode', {'autoload':{'filetypes':['python']}} "{{{
    " activate rope
    " keys:
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

    " documentation
    let g:pymode_doc = 1
    let g:pymode_doc_key = 'K'

    " linting
    let g:pymode_lint = 1
    let g:pymode_lint_checker = ['pylint', 'pep8', 'pyflakes']
    let g:pymode_lint_ignore = ''
    let g:pymode_lint_write = 0 " auto check on save

    " quickfix window
    let g:pymode_quickfix_minheight = 3
    let g:pymode_quickfix_maxheight = 6
    let g:pymode_lint_cwindow = 1

    " support virtualenv
    let g:pymode_virtualenv = 1

    " enable breakpoints plugin
    let g:pymode_breakpoint = 1
    let g:pymode_breakpoint_bind = '<leader>b'

    " syntax highlighting
    let g:pymode_syntax = 1
    let g:pymode_syntax_all = 1
    let g:pymode_syntax_indent_errors = g:pymode_syntax_all
    let g:pymode_syntax_space_errors = g:pymode_syntax_all

    " don't autofold code
    let g:pymode_folding = 0

    nmap <silent><Leader>n :PymodeLint<CR>
    nmap <silent><Leader>N :PymodeLintAuto<CR>
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
  " show indent lines
  NeoBundleLazy 'Yggdroot/indentLine', {'autoload': {'filetypes': ['python']}} "{{{
    let g:indentLine_enabled = 0
    let g:indentLine_char = '┊'
    let g:indentLine_color_term = 239

    map <silent> <Leader>L :IndentLinesToggle<CR>
  "}}}
  " vimux {{{
    let g:VimuxUseNearestPane = 1

    map <Leader>rr :call VimuxRunCommand('clear;cd '.expand("%:p:h") .' ;python2 '.bufname("%"))<CR>
    map <Leader>r3 :call VimuxRunCommand('clear;cd '.expand("%:p:h") .' ;python3 '.bufname("%"))<CR>
    map <Leader>rt :call VimuxRunCommand('clear;cd '.expand("%:p:h") .' ;time python2 '.bufname("%"))<CR>
    map <Leader>rp :call VimuxRunCommand('clear;cd '.expand("%:p:h") .' ;time pypy '.bufname("%"))<CR>

    map <Leader>rc :VimuxPromptCommand<CR>
    map <Leader>rl :VimuxRunLastCommand<CR>
    map <Leader>rs :VimuxInterruptRunner<CR>
    map <Leader>ri :VimuxInspectRunner<CR>
    map <Leader>rq :VimuxCloseRunner<CR>
  " }}}
" }}}
call neobundle#end()

" unite menu {{{
  let g:unite_source_menu_menus.python = {
    \ 'description' : '         python tools
    \                                          ⌘ [space]p',
  \}
  let g:unite_source_menu_menus.python.command_candidates = [
    \['▷ run python code                            (pymode)        ⌘ <Leader>r',
        \'PymodeRun'],
    \['▷ show docs for the current word             (pymode)        ⌘ K',
        \'normal K'],
    \['▷ insert a breakpoint                        (pymode)        ⌘ <Leader>B',
        \'normal <Leader>B'],
    \['▷ pylint check                               (pymode)        ⌘ <Leader>n',
        \'PymodeLint'],
    \['▷ pylint auto correct                        (pymode)        ⌘ <Leader>N',
        \'PymodeLintAuto'],
    \['▷ go to definition                           (pymode-rope)   ⌘ C-C g',
        \'call pymode#rope#goto_definition()'],
    \['▷ find where a function is used              (pymode-rope)   ⌘ C-C f',
        \'call pymode#rope#find_it()'],
    \['▷ show docs for current word                 (pymode-rope)   ⌘ C-C d',
        \'call pymode#rope#show_doc()'],
    \['▷ reorganize imports                         (pymode-rope)   ⌘ C-C r o',
        \'call pymode#rope#organize_imports()'],
    \['▷ refactorize - rename                       (pymode-rope)   ⌘ C-C r r',
        \'call pymode#rope#rename()'],
    \['▷ refactorize - inline                       (pymode-rope)   ⌘ C-C r i',
        \'call pymode#rope#inline()'],
    \['▷ refactorize - move                         (pymode-rope)   ⌘ C-C r v',
        \'call pymode#rope#move()'],
    \['▷ refactorize - use function                 (pymode-rope)   ⌘ C-C r u',
        \'call pymode#rope#use_function()'],
    \['▷ refactorize - change signature             (pymode-rope)   ⌘ C-C r s',
        \'call pymode#rope#signature()'],
    \['▷ refactorize - rename current module        (pymode-rope)   ⌘ C-C r 1 r',
        \'PymodeRopeRenameModule'],
    \['▷ refactorize - module to package            (pymode-rope)   ⌘ C-C r 1 p',
        \'PymodeRopeModuleToPackage'],
    \['▷ list virtualenvs                           (virtualenv)',
        \'Unite output:VirtualEnvList'],
    \['▷ activate virtualenv                        (virtualenv)',
        \'VirtualEnvActivate'],
    \['▷ deactivate virtualenv                      (virtualenv)',
        \'VirtualEnvDeactivate'],
    \['▷ run coverage2                              (coveragepy)',
        \'call system("coverage2 run ".bufname("%")) | Coveragepy report'],
    \['▷ run coverage3                              (coveragepy)',
        \'call system("coverage3 run ".bufname("%")) | Coveragepy report'],
    \['▷ toggle coverage report                     (coveragepy)',
        \'Coveragepy session'],
    \['▷ toggle coverage marks                      (coveragepy)',
        \'Coveragepy show'],
    \]
    let g:unite_source_menu_menus.python.command_candidates = helperfuncs#unite_menu_gen(g:unite_source_menu_menus.python.command_candidates, [])

    nnoremap <silent>[menu]p :Unite -silent -winheight=42 menu:python<CR>
"}}}
