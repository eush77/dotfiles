##### LET THE CARNAGE BEGIN #####

#============================ Requirements =================================
# Grc              : coloring aliases & configuration files
# Source-highlight : syntax highlight
#===========================================================================

#======================== Environment variables ============================
MP=/mnt/mp
HISTCONTROL=ignoreboth
HISTIGNORE='l:la:lc:ll:fg:fg *:bg:bg *:cd:cd *:dirs:dirs *:jobs:jobs *:pushd:pushd *:popd:popd *:history:history *'
HISTSIZE=10000
HISTTIMEFORMAT='(%d.%m|%R)  '
shopt -s histappend cmdhist checkjobs
PROMPT_COMMAND='history -a'
PS1='[\#]\w\$ '
export WWW_HOME=http://www.duckduckgo.com
export LESS='-MR'
export LESSOPEN='| src-hilite-lesspipe.sh "%s"'
export EDITOR='emacs -nw'
#===========================================================================

#========================== Startup execution ==============================
#===========================================================================

#============================== Aliases ====================================
alias bc='bc -q'
alias ready="zenity --info --text='Ready, Master!' --title=''"
alias cd=cd-go
alias g=git
alias sudo='sudo ' # It pushes the rest of aliases through "sudo"
#alias bfdev='wine ~/.wine/drive_c/Program\ Files/bfdev/bfdev.exe'
#alias bfc='~/.wine/drive_c/Program\ Files/bfc/run'
alias dirs='dirs -v'
alias catll='cat >/dev/null'
alias img='gpicview'
alias du='du -h'
alias df='df -h'
alias l='ls -CF -BX --group-directories-first -v'
alias la='ls -alhFX --group-directories-first'
alias lc='ls -1 -BX -v --group-directories-first'
alias lv='l -v'
alias emacs='emacs -nw'
alias objdump='grc objdump'
alias strace='grc strace -o /dev/stdout'
alias tcpdump='grc tcpdump'
alias diff='grc diff'
alias hilite='source-highlight -fesc -i'
#===========================================================================

#======================= Unordered stuff ===================================
set +H
setxkbmap -model pc104 -layout "us,ru" -variant ",winkeys" -option "grp:caps_toggle,grp:win_switch,grp_led:caps,compose:menu,numpad:microsoft"
bind 'set completion-ignore-case on'
#===========================================================================

#=================== Less as a colored man pager ===========================
export LESS_TERMCAP_mb=$'\e[01;31m'
export LESS_TERMCAP_md=$'\e[01;31m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;44;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[01;32m'
#===========================================================================

#================================= Functions ===============================
function cdkb {
    if [[ $# -gt 1 ]]; then
        echo -en '\e[31mInvalid call parameters list!\e[0m Lol, be more accurate another time.'
        exit
    fi
    LIST=$(find -maxdepth 1 -type d |sed -r "s/ /$(echo -en '\001')/g")
    if [[ $# -eq 0 ]]; then
        IDX=0
        for I in ${LIST}; do
            I=$(sed "s/$(echo -en '\001')/ /g" <<<"$I")
            echo -en "[$((++IDX))] $I? \e[36my/n\e[0m: "
            read
            if [[ ${REPLY} == 'y' ]]; then
                echo -n "Diving into \"$I\"... "
                cd "$I"
                echo
                break
            fi
        done
    else
        IDX=$1
        for I in ${LIST}; do
            I=$(sed "s/$(echo -en '\001')/ /g" <<<"$I")
            if [[ 0 -eq $((--IDX)) ]]; then
                echo -n "Diving into \"$I\"... "
                cd "$I"
                echo
                break
            fi
        done
    fi
}

function cd-go {
    local ERROR='\e[31m[cd-go]\e[0m'
    if [[ $# -gt 1 ]]; then
        echo -e "$ERROR Too many arguments to \"cd\"!"
        return 1
    elif [[ $# -eq 0 ]]; then
        builtin cd
    # From now on, the one and only argument given is "$1"
    elif [[ -z "$1" ]]; then
        echo -e "$ERROR The only argument is empty!"
        return 2
    elif [[ "${1::1}" == "-" || -d "$1" ]]; then
        builtin cd "$1"
    else
        echo -en "Directory \"$1\" does not exist. \e[34mCreate one?\e[0m "
        read
        if [[ "${REPLY::1}" == "y" ]]; then
            mkdir -p "$1"
            builtin cd "$1"
        else
            echo "Cancelled."
        fi
    fi
}

function timer {
    local S=0 M=0
    echo -n "0:00"
    while sleep 1; do
        if [[ $((++S)) -eq 60 ]]; then
            : $((++M, S = 0))
        fi
        printf "\r%u:%02u" $M $S
    done
}
#===========================================================================

##### HAPPY ENDING #####
