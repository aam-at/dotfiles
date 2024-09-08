func! myspacevim#before() abort
  set smartcase
  set ignorecase
endf

func! myspacevim#after() abort
  set background=dark
  lua << EOF
  -- execute myspacevim_after.lua
  require('myspacevim_after')
EOF
endf
