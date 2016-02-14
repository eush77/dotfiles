set -gx EDITOR 'emacs -nw'
set -gx PAGER 'less'
set -gx LESS '--ignore-case --RAW-CONTROL-CHARS --quit-if-one-screen --no-init --jump-target=6'
set -gx LESSOPEN '| emacs --batch -u eush77 -l /usr/local/bin/e2ansi-cat %s 2>/dev/null'
set -gx MP '/mnt/mp'

alias l 'ls -CF -BX --group-directories-first -v'
alias la 'ls -alhFX --group-directories-first'
alias lc 'ls -1 -BX -v --group-directories-first'
alias gg 'git grep'
alias e editor
alias v pager
alias less 'less -+FX'

abbr -a g git
abbr -a n npm

alias emacs 'emacs -nw'
alias matlab 'matlab -nodesktop -nosplash'

set -gx LESS_TERMCAP_mb (printf '\e[01;31m')
set -gx LESS_TERMCAP_md (printf '\e[01;31m')
set -gx LESS_TERMCAP_me (printf '\e[0m')
set -gx LESS_TERMCAP_se (printf '\e[0m')
set -gx LESS_TERMCAP_so (printf '\e[01;44;33m')
set -gx LESS_TERMCAP_ue (printf '\e[0m')
set -gx LESS_TERMCAP_us (printf '\e[01;32m')

# Kill background jobs on exit.
function _killbg --on-process-exit %self
        for pid in (jobs --pid)
                kill $pid
        end
end

# Hook up direnv.
eval (direnv hook fish)
