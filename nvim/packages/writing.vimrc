" vim setup {{{
  " turn on the spell checking and set the English language
  nmap <Leader>se :setlocal spell spelllang=en<CR>
  " turn off the spell checking
  nmap <Leader>so :setlocal nospell <CR>
  " jump to the next bad spell word
  nmap <Leader>sn ]s
  " suggest words
  nmap <Leader>sp z=
  " jump to the next bad spell word and suggests a correct one
  nmap <Leader>sc ]sz=
  " add word to the dictionary
  nmap <Leader>sa zg
"}}}

call neobundle#append()
" plugins {{{
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
  NeoBundle 'maksimr/vim-translator' "{{{
    " Selected words and press 'T' to translate
    let g:goog_user_conf = {'langpair': 'en|ru', 'v_key': 'T'}
  "}}}
"}}}
call neobundle#end()

" unite menu {{{
  let g:unite_source_menu_menus.spelling = {
      \ 'description' : '       spell checking
      \                                        ⌘ [space]s',
    \}
  let g:unite_source_menu_menus.spelling.command_candidates = [
      \['▷ spell checking in English                                  ⌘ <Leader>se',
      \'setlocal spell spelllang=en'],
      \['▷ turn off spell checking                                    ⌘ <Leader>so',
      \'setlocal nospell'],
      \['▷ jumps to next bad spell word and show suggestions          ⌘ <Leader>sc',
      \'normal <Leader>sc'],
      \['▷ jumps to next bad spell word                               ⌘ <Leader>sn',
      \'normal <Leader>sn'],
      \['▷ suggestions                                                ⌘ <Leader>sp',
      \'normal <Leader>sp'],
      \['▷ add word to dictionary                                     ⌘ <Leader>sa',
      \'normal <Leader>sa'],
    \]
  let g:unite_source_menu_menus.spelling.command_candidates = helperfuncs#unite_menu_gen(g:unite_source_menu_menus.spelling.command_candidates, [])

  nnoremap <silent>[menu]s :Unite -silent menu:spelling<CR>
"}}}
