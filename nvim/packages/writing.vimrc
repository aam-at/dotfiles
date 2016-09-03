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
  NeoBundle 'reedes/vim-wordy', {'autoload': {'filetypes': ['txt', 'tex', 'org']}}
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
  NeoBundleLazy 'dbmrq/vim-ditto', {'autoload': {'commands': ['ToggleDitto']}}
    let g:ditto_mode = "file"
    let g:ditto_file = utils#GetCacheDir('ditto')
    au FileType markdown,text,tex DittoOn  " Turn on Ditto's autocmds

    nmap <leader>di <Plug>ToggleDitto      " Turn it on and off

    nmap =d <Plug>DittoNext                " Jump to the next word
    nmap -d <Plug>DittoPrev                " Jump to the previous word
    nmap +d <Plug>DittoGood                " Ignore the word under the cursor
    nmap _d <Plug>DittoBad                 " Stop ignoring the word under the cursor
    nmap ]d <Plug>DittoMore                " Show the next matches
    nmap [d <Plug>DittoLess                " Show the previous matches
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
  let g:unite_source_menu_menus.writing = {
      \ 'description' : '       spelling and writing
      \                                        ⌘ [space]s',
    \}
  let g:unite_source_menu_menus.writing.command_candidates = [
      \['▷ writeroom for vim                                          ⌘ <Leader>sW',
      \'Goyo'],
      \['▷ ditto to detect repeated words                             ⌘ <Leader>di',
      \'ToggleDitto'],
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
  let g:unite_source_menu_menus.writing.command_candidates = helperfuncs#unite_menu_gen(g:unite_source_menu_menus.writing.command_candidates, [])

  nnoremap <silent> <LocalLeader>s :<C-u>Unite -silent menu:writing<CR>
"}}}
