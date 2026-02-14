" setlocal ft=markdown

syntax match TodoStateTODO  /^TODO\>/
syntax match TodoStateDOING /^DOING\>/
syntax match TodoStateDONE  /^DONE\>/
highlight link TodoStateTODO  Error
highlight link TodoStateDOING WarningMsg
highlight link TodoStateDONE  Todo

syntax match TodoOwner /@\w\+/
highlight link TodoOwner Keyword

syntax match TodoOwnerYou /@axvr/
highlight link TodoOwnerYou Changed

syntax match TodoGroup /^===.*/ contains=TodoGroupTitle
syntax match TodoGroupTitle /^=*\s\zs.*/ contained
highlight link TodoGroupTitle Title
highlight link TodoGroup SpecialComment

syntax match TodoTag /#\w\+/
highlight link TodoTag Function

" function! s:auto_tab()
"     let ln = getline('.')[: getcharpos('.')[2] - 1]
"     if ln =~# '^\(TODO\|DOING\|DONE\)\(\t@\w\+\)\{-,1}$'
"         exec "normal! a\<tab>"
"     else
"         exec "normal! a\<space>"
"     endif
" endfunction

" inoremap <Space> <C-o>:call <SID>auto_tab()<CR>

set tabstop=8 noexpandtab softtabstop=0
