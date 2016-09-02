call neobundle#append()
" plugins {{{
  NeoBundleLazy 'LaTeX-Box-Team/LaTeX-Box', {'autoload':{'filetypes':['tex']}}
  NeoBundleLazy 'lervag/vimtex', {'autoload':{'filetypes':['tex']}}
  if !g:is_windows
      NeoBundleLazy 'xuhdev/vim-latex-live-preview', {'autoload':{'filetypes':['tex']}}
  endif
"}}}
call neobundle#end()
