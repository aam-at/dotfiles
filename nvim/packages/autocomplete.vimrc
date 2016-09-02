call neobundle#append()
" plugins {{{
  NeoBundle 'honza/vim-snippets'
  if g:my_settings.autocomplete_method == 'ycm' "{{{
    NeoBundle 'Valloric/YouCompleteMe' "{{{
      let g:ycm_complete_in_comments_and_strings=1
      let g:ycm_key_list_select_completion=['<C-n>', '<Down>']
      let g:ycm_key_list_previous_completion=['<C-p>', '<Up>']
      let g:ycm_filetype_specific_completion_to_disable = { 'python' : 1 }
      let g:ycm_filetype_blacklist={'unite': 1, 'python' : 1}
  " }
    "}}}
    NeoBundle 'SirVer/ultisnips' "{{{
      let g:UltiSnipsExpandTrigger="<tab>"
      let g:UltiSnipsJumpForwardTrigger="<tab>"
      let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
      let g:UltiSnipsSnippetsDir='~/.config/nvim/snippets'
    "}}}
  else
    NeoBundle 'Shougo/neosnippet-snippets'
    NeoBundle 'Shougo/neosnippet.vim' "{{{
      let g:neosnippet#snippets_directory='~/.config/nvim/bundle/vim-snippets/snippets,~/.config/nvim/snippets'
      let g:neosnippet#enable_snipmate_compatibility=1

      imap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : (pumvisible() ? "\<C-n>" : "\<TAB>")
      smap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
      imap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""
      smap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""
    "}}}
    NeoBundle 'Shougo/deoplete.nvim' "{{{
      let g:deoplete#enable_at_startup = 1   "enable deoplete at vim startup
      let g:deoplete#enable_ignore_case = 1  "let matcher ignore case
      let g:deoplete#enable_smart_case = 1   "smart case
      inoremap <expr><C-h> deolete#mappings#smart_close_popup()."\<C-h>"
      inoremap <expr><BS> deoplete#mappings#smart_close_popup()."\<C-h>"
    "}}}
  endif "}}}
"}}}
call neobundle#end()
