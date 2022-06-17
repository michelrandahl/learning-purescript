let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/Code/learning-purescript/take2/chapter18
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
argglobal
%argdel
edit src/Main.purs
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
split
1wincmd k
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe '1resize ' . ((&lines * 40 + 31) / 62)
exe '2resize ' . ((&lines * 19 + 31) / 62)
argglobal
balt src/Main.purs
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 97 - ((37 * winheight(0) + 20) / 40)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 97
normal! 0
wincmd w
argglobal
if bufexists("term://~/Code/learning-purescript/take2/chapter18//383500:/nix/store/sa9h3gslr4jq0s2z22pyibwv5i7bj4ls-bash-interactive-5.1-p8/bin/bash") | buffer term://~/Code/learning-purescript/take2/chapter18//383500:/nix/store/sa9h3gslr4jq0s2z22pyibwv5i7bj4ls-bash-interactive-5.1-p8/bin/bash | else | edit term://~/Code/learning-purescript/take2/chapter18//383500:/nix/store/sa9h3gslr4jq0s2z22pyibwv5i7bj4ls-bash-interactive-5.1-p8/bin/bash | endif
if &buftype ==# 'terminal'
  silent file term://~/Code/learning-purescript/take2/chapter18//383500:/nix/store/sa9h3gslr4jq0s2z22pyibwv5i7bj4ls-bash-interactive-5.1-p8/bin/bash
endif
balt src/Main.purs
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 238 - ((18 * winheight(0) + 9) / 19)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 238
normal! 062|
wincmd w
exe '1resize ' . ((&lines * 40 + 31) / 62)
exe '2resize ' . ((&lines * 19 + 31) / 62)
tabnext 1
badd +96 src/Main.purs
badd +0 term://~/Code/learning-purescript/take2/chapter18//383500:/nix/store/sa9h3gslr4jq0s2z22pyibwv5i7bj4ls-bash-interactive-5.1-p8/bin/bash
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToOF
let &winminheight = s:save_winminheight
let &winminwidth = s:save_winminwidth
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
