" cut/paste {{{
  " to/from the clipboard
  map <Leader>y "*y
  map <Leader>p "*p

  " toggle paste mode
  map <Leader>P :set invpaste<CR>
"}}}

" toggle the search results highlighting {{{
  map <silent><Leader>eq :set invhlsearch<CR>
"}}}

" Show hidden chars {{{
  nmap <Leader>eh :set list!<CR>
  set listchars=tab:→\ ,eol:↵,trail:·,extends:↷,precedes:↶
" }}}

" text statistics {{{
  " get the total of lines, words, chars and bytes (and for the current position)
  map <Leader>es g<C-G>

  " get the word frequency in the text
  function! WordFrequency() range
    let all = split(join(getline(a:firstline, a:lastline)), '\A\+')
    let frequencies = {}
    for word in all
      let frequencies[word] = get(frequencies, word, 0) + 1
    endfor
    let lst = []
    for [key,value] in items(frequencies)
      call add(lst, value."\t".key."\n")
    endfor
    call sort(lst)
    echo join(lst)
  endfunction

  command! -range=% WordFrequency <line1>,<line2>call WordFrequency()
  map <Leader>ef :Unite output:WordFrequency<CR>
" }}}

call neobundle#append()
" plugins {{{

  " Autocompletion of (, [, {, ', ", ...
  NeoBundle 'delimitMate.vim' "{{{
    let delimitMate_expand_space = 1
  "}}}
  " smart and fast date changer
  NeoBundle 'tpope/vim-speeddating'
  " to surround vim objects with a pair of identical chars
  NeoBundle 'tpope/vim-surround'
  " extend repetitions by the 'dot' key
  NeoBundle 'tpope/vim-repeat'
  " toggle comments
  NeoBundle 'tpope/vim-commentary'
  " reveals all the character info, Unicode included
  NeoBundle 'tpope/vim-characterize'
  NeoBundle 'tpope/vim-endwise'
  NeoBundle 'tpope/vim-dispatch'
  NeoBundle 'tpope/vim-eunuch'
  NeoBundle 'tpope/vim-unimpaired' "{{{
      nmap <c-up> [e
      nmap <c-down> ]e
      vmap <c-up> [egv
      vmap <c-down> ]egv
  "}}}
  NeoBundle 'easymotion/vim-easymotion' "{{{
      " Easymotion colors for light colors
      hi link EasyMotionTarget ErrorMsg
      hi link EasyMotionTarget2First Search
      hi link EasyMotionTarget2Second Search
      hi link EasyMotionShade  Comment
  "}}}"
  " jump with two characters
  NeoBundle 'justinmk/vim-sneak' "{{{
      let g:sneak#streak = 1
  "}}}
  
  " smart digraphs insertion
  NeoBundle 'Rykka/easydigraph.vim' "{{{
    let g:EasyDigraph_nmap = '<Leader>dd'
  "}}}
  " to insert lorem ipsum blocks
  NeoBundleLazy 'vim-scripts/loremipsum', { 'autoload' : { 'commands' : 'Loremipsum'}}
  " transpose lines and text blocks
  NeoBundleLazy 'salsifis/vim-transpose', { 'autoload' : { 'commands' : 'Transpose'}}
  " easy exchange for two motions
  NeoBundle 'tommcdo/vim-exchange'
  " marks admin
  NeoBundle 'kshenoy/vim-signature'
  
  NeoBundleLazy 'editorconfig/editorconfig-vim', {'autoload':{'insert':1}}
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

  NeoBundle 'Konfekt/FastFold'
  " text-objects {{{
    NeoBundle 'vim-scripts/argtextobj.vim' " aa, ia
    NeoBundle 'kana/vim-textobj-user'
    NeoBundle 'kana/vim-textobj-lastpat' " a/, i/, a?, i?
    NeoBundle 'kana/vim-textobj-line' " al, il
    NeoBundle 'kana/vim-textobj-indent' " ai, ii, aI, iI
    NeoBundle 'kana/vim-textobj-entire' " ae, ie
    NeoBundle 'lucapette/vim-textobj-underscore' " a_, i_
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
  "}}}
"}}}
call neobundle#end()

" unite menu {{{
  let g:unite_source_menu_menus.text = {
      \ 'description' : '           text editing
      \                                          ⌘ [space]e',
    \}
  let g:unite_source_menu_menus.text.command_candidates = [
      \['▷ toggle search results highlight                            ⌘ <Leader>eq',
      \'set invhlsearch'],
      \['▷ toggle line numbers                                        ⌘ <Leader>l',
      \'call ToggleRelativeAbsoluteNumber()'],
      \['▷ toggle wrapping                                            ⌘ <Leader>ew',
      \'call ToggleWrap()'],
      \['▷ show hidden chars                                          ⌘ <Leader>eh',
      \'set list!'],
      \['▷ toggle fold                                                ⌘ /',
      \'normal za'],
      \['▷ open all folds                                             ⌘ zR',
      \'normal zR'],
      \['▷ close all folds                                            ⌘ zM',
      \'normal zM'],
      \['▷ copy to the clipboard                                      ⌘ <Leader>y',
      \'normal <Leader>y'],
      \['▷ paste from the clipboard                                   ⌘ <Leader>p',
      \'normal <Leader>p'],
      \['▷ toggle paste mode                                          ⌘ <Leader>P',
      \'normal <Leader>P'],
      \['▷ remove trailing whitespaces                                ⌘ <Leader>et',
      \'normal <Leader>et'],
      \['▷ text statistics                                            ⌘ <Leader>es',
      \'Unite output:normal\ ,es -no-cursor-line'],
      \['▷ show word frequency                                        ⌘ <Leader>ef',
      \'Unite output:WordFrequency'],
      \['▷ show available digraphs',
      \'digraphs'],
      \['▷ insert lorem ipsum text',
      \'exe "Loremipsum" input("numero de palabras: ")'],
      \['▷ show current char info                                     ⌘ ga',
      \'normal ga'],
    \]
  let g:unite_source_menu_menus.text.command_candidates = helperfuncs#unite_menu_gen(g:unite_source_menu_menus.text.command_candidates, [])

  nnoremap <silent> <LocalLeader>e :Unite -silent -winheight=20 menu:text <CR>
"}}}
