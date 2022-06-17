let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/Code/learning-purescript/take2/chapter7
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
argglobal
%argdel
edit src/Ch7a.purs
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
split
wincmd _ | wincmd |
split
2wincmd k
wincmd w
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
exe '1resize ' . ((&lines * 15 + 29) / 58)
exe '2resize ' . ((&lines * 16 + 29) / 58)
exe '3resize ' . ((&lines * 23 + 29) / 58)
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
let s:l = 1 - ((0 * winheight(0) + 7) / 15)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 017|
wincmd w
argglobal
if bufexists("src/Ch7b.purs") | buffer src/Ch7b.purs | else | edit src/Ch7b.purs | endif
if &buftype ==# 'terminal'
  silent file src/Ch7b.purs
endif
balt src/Ch7a.purs
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
let s:l = 6 - ((5 * winheight(0) + 8) / 16)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 6
normal! 0
wincmd w
argglobal
if bufexists("term://~/Code/learning-purescript/take2/chapter7//1145836:/nix/store/90y23lrznwmkdnczk1dzdsq4m35zj8ww-bash-interactive-5.1-p8/bin/bash") | buffer term://~/Code/learning-purescript/take2/chapter7//1145836:/nix/store/90y23lrznwmkdnczk1dzdsq4m35zj8ww-bash-interactive-5.1-p8/bin/bash | else | edit term://~/Code/learning-purescript/take2/chapter7//1145836:/nix/store/90y23lrznwmkdnczk1dzdsq4m35zj8ww-bash-interactive-5.1-p8/bin/bash | endif
if &buftype ==# 'terminal'
  silent file term://~/Code/learning-purescript/take2/chapter7//1145836:/nix/store/90y23lrznwmkdnczk1dzdsq4m35zj8ww-bash-interactive-5.1-p8/bin/bash
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
let s:l = 425 - ((22 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 425
normal! 0
wincmd w
2wincmd w
exe '1resize ' . ((&lines * 15 + 29) / 58)
exe '2resize ' . ((&lines * 16 + 29) / 58)
exe '3resize ' . ((&lines * 23 + 29) / 58)
tabnext 1
badd +8 src/Main.purs
badd +4 term://~/Code/learning-purescript/take2/chapter7//1145836:/nix/store/90y23lrznwmkdnczk1dzdsq4m35zj8ww-bash-interactive-5.1-p8/bin/bash
badd +1 src/Ch7a.purs
badd +19 spago.dhall
badd +9 src/Ch7b.purs
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
