set -gx EDITOR 'emacs -nw'
set -gx LESSOPEN '| src-hilite-lesspipe.sh %s'
set -gx LESS '-R'
set -gx MP '/mnt/mp'

alias l 'ls -CF -BX --group-directories-first -v'
alias la 'ls -alhFX --group-directories-first'
alias lc 'ls -1 -BX -v --group-directories-first'
alias g git
alias emacs 'emacs -nw'
