set -gx CXX 'colorg++'
set -gx EDITOR 'emacs -nw'
set -gx LESSOPEN '| src-hilite-lesspipe.sh %s'
set -gx LESS '-R'
set -gx MP '/mnt/mp'
set -gx MP2 '/mnt/mp2'

alias l 'ls -CF -BX --group-directories-first -v'
alias la 'ls -alhFX --group-directories-first'
alias lc 'ls -1 -BX -v --group-directories-first'
alias g git
alias gs 'g s'
alias emacs 'emacs -nw'
alias matlab 'matlab -nodesktop -nosplash'
alias nodejs 'echo -n'
alias re 'grep -P'
alias se 'sed -r'

set -gx LESS_TERMCAP_mb (printf '\e[01;31m')
set -gx LESS_TERMCAP_md (printf '\e[01;31m')
set -gx LESS_TERMCAP_me (printf '\e[0m')
set -gx LESS_TERMCAP_se (printf '\e[0m')
set -gx LESS_TERMCAP_so (printf '\e[01;44;33m')
set -gx LESS_TERMCAP_ue (printf '\e[0m')
set -gx LESS_TERMCAP_us (printf '\e[01;32m')
