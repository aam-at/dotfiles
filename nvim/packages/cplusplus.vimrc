call neobundle#append()
" plugins {{{
  " Generate config for YCMD
  NeoBundle 'rdnetto/YCM-Generator'
  NeoBundle 'octol/vim-cpp-enhanced-highlight' "{
    let g:cpp_class_scope_highlight = 1
    let g:cpp_member_variable_highlight = 1
    let g:cpp_class_decl_highlight = 1
    let g:cpp_concepts_highlight = 1
    let g:cpp_experimental_simple_template_highlight = 1
    " faster highlighting for templates
    " let g:cpp_experimental_template_highlight = 1
  "}
" }}}
call neobundle#end()
