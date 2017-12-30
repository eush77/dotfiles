set shell=bash
set textwidth=72
set smarttab expandtab shiftwidth=4 softtabstop=4
set hlsearch incsearch ignorecase scrolloff=6
set wildmenu
cabbrev p <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'N' : 'p')<CR>
nmap s :exec "normal i".nr2char(getchar())."\e"<CR>
nmap S :exec "normal a".nr2char(getchar())."\e"<CR>
nmap <Space> :nohlsearch<Bar>echo<CR>
nmap <Tab> :nohlsearch<Bar>echo<CR>
map <c-a> ^
map <c-e> $

" Syntax highlighting.
augroup filetype
    au! BufRead,BufNewFile *.ll set filetype=llvm
augroup END
augroup filetype
    au! BufRead,BufNewFile *.fish set filetype=fish
augroup END
