call neobundle#append()
" plugins {{{
  if has('win32') && !has('win64')
      NeoBundle 'derekmcloughlin/gvimfullscreen_win32' "{{{
        let $GVIMFS=substitute($NVIM_HOME"./bundle/gvimfullscreen_win32/gvimfullscreen.dll", '\\', '\\\\', 'g')
        map <F11> <Esc>:call libcallnr($GVIMFS, "ToggleFullScreen", 0)<CR>
    "}}}
  else
    NeoBundle 'xqin/gvimfullscreen' "{{{
        let $GVIMFS=substitute($NVIM_HOME"./bundle/gvimfullscreen/gvimfullscreen.dll.x64", '\\', '\\\\', 'g')
        map <F11> <Esc>:call libcallnr($GVIMFS, "ToggleFullScreen", 0)<CR>
    "}}}
  endif
"}}}
call neobundle#end()
