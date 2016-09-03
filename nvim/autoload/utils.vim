" functions {{{
  function! utils#GetCacheDir(suffix) "{
    return resolve(expand(g:my_settings.cache_dir . '/' . a:suffix))
  endfunction "}

  function! utils#Source(begin, end) "{
    let lines = getline(a:begin, a:end)
    for line in lines
      execute line
    endfor
  endfunction "}

  function! utils#Preserve(command) "{
    " preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " do the business:
    execute a:command
    " clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
  endfunction "}

  function! utils#StripTrailingWhitespace() "{
    call utils#Preserve("%s/\\s\\+$//e")
  endfunction "}

  function! utils#EnsureExists(path) "{
    if !isdirectory(expand(a:path))
      call mkdir(expand(a:path))
    endif
  endfunction "}

  function! utils#CloseWindowOrKillBuffer()" {
    let number_of_windows_to_this_buffer = len(filter(range(1, winnr('$')), "winbufnr(v:val) == bufnr('%')"))

    " never bdelete a nerd tree
    if matchstr(expand("%"), 'NERD') == 'NERD'
      wincmd c
      return
    endif

    if number_of_windows_to_this_buffer > 1
      wincmd c
    else
      bdelete
    endif
  endfunction "}
" }}}
