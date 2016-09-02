call neobundle#append()
" plugins {{{
  NeoBundle 'xolox/vim-misc'
  NeoBundleLazy 'xolox/vim-lua-inspect', {'depends': 'xolox/vim-misc', 'autoload':{'filetypes':['lua']}}
  NeoBundleLazy 'xolox/vim-lua-ftplugin', {'depends': 'xolox/vim-misc', 'autoload':{'filetypes':['lua']}}
  " NeoBundle 'WolfgangMehner/lua-support'
"}}}
call neobundle#end()
