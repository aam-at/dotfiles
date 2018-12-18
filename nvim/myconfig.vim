func! myconfig#before() abort
  set smartcase
  set ignorecase
  let g:python_host_prog = $HOME.'/.pyenv/versions/neovim2/bin/python'
  let g:python3_host_prog = $HOME.'/.pyenv/versions/neovim3/bin/python3'
endf
