# -*- mode: shell-script; indent-tabs-mode: t; tab-width: 4; -*-#

#======================== Environment Variables ============================
export EDITOR=vi
export LESS='--ignore-case --RAW-CONTROL-CHARS --quit-if-one-screen
             --no-init --jump-target=6'
export LESS_TERMCAP_mb=$'\e[01;31m'
export LESS_TERMCAP_md=$'\e[01;31m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;44;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[01;32m'
export PAGER=less
#===========================================================================

# If not running interactively, don't go any further.
[[ $- != *i* ]] && return

#============================ Shell Settings ===============================
FIGNORE='~'
HISTCONTROL=ignoreboth
HISTFILESIZE=20000
HISTIGNORE='l:la:lc:ll:fg:fg *:bg:bg *:cd:cd *:dirs:dirs *:jobs:jobs
            *:pushd:pushd *:popd:popd *:history:history *'
HISTSIZE=15000
HISTTIMEFORMAT='(%d.%m|%R)  '
IGNOREEOF=0
PROMPT_COMMAND='history -a; [[ "${_LAST_PWD:=$PWD}" != "$PWD" ]] && l; _LAST_PWD="$PWD"'
PS1='!\! ${PS1_SHLVL}\[\e[36m\]\u@${PS1_HOSTNAME}\[\e[0m\]:\[\e[36m\]\[\e[36m\]\w\[\e[0m\]> '
PS1_HOSTNAME="${HOSTNAME:0:2}"

SHLVL_BASE=1
if [[ "$SHLVL" -gt "$SHLVL_BASE" ]]; then
	PS1_SHLVL="(+$((SHLVL - SHLVL_BASE))) "
else
	PS1_SHLVL=""
fi

shopt -s \
	  autocd \
      cdspell \
      checkjobs \
      checkwinsize \
      cmdhist \
      direxpand \
      dirspell \
	  extglob \
	  failglob \
	  globstar \
	  gnu_errfmt \
	  histappend \
	  histreedit \
	  histverify \
	  huponexit \
	  lastpipe \
	  lithist \
	  no_empty_cmd_completion \
	  nocaseglob
#===========================================================================

#============================ Readline =====================================
bind 'set completion-ignore-case on'
bind 'set convert-meta on'
bind 'set match-hidden-files off'

bind 'Meta-n: menu-complete'
bind 'Meta-p: menu-complete-backward'
bind 'SPACE: magic-space'
#===========================================================================

#============================== Aliases ====================================
alias bc='bc -q'
alias catll='cat >/dev/null'
alias df='df -h'
alias dirs='dirs -v'
alias du='du -h'
alias emacs='emacs -nw'
alias g=git
alias hilite='source-highlight -fesc -i'
alias l='ls -CF -BX --group-directories-first -v'
alias la='ls -alhFX --group-directories-first'
alias lc='ls -1 -BX -v --group-directories-first'
alias ready="zenity --info --text='Ready, Master!' --title=''"
alias sudo='sudo '  # Expand aliases under `sudo'.
#===========================================================================

#================================= Functions ===============================
# Simple message formatter.
function _fmt {
	local TYPE="$1"
	local TEXT="$2"

	case "$TYPE" in
		error)
			local TYPE_COLOR='01;31'
			;;
		usage)
			local TYPE_COLOR='01;30'
			;;
		*)
			local TYPE_COLOR=37
			;;
	esac

	printf '\e[01m%s:\e[%sm %s:\e[0m %s\n' \
		   "${FUNCNAME[1]}" "$TYPE_COLOR" "${TYPE^?}" "$TEXT"
}

# Traverse directories that perhaps can't be entirely printed or completed on
# a terminal.
function cdkb {
	if [[ "$#" -gt 1 ]]; then
		_fmt usage 'cdkb [arg]'
		return 1
	fi
	local LIST=$(find -maxdepth 1 -type d |
					 sed -r "s/ /$(echo -en '\001')/g")
	if [[ "$#" -eq 0 ]]; then
		local IDX=0
		for I in ${LIST}; do
			local I=$(sed "s/$(echo -en '\001')/ /g" <<<"$I")
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
		local IDX="$1"
		for I in ${LIST}; do
			local I=$(sed "s/$(echo -en '\001')/ /g" <<<"$I")
			if [[ 0 -eq $((--IDX)) ]]; then
				echo -n "Diving into \"$I\"... "
				cd "$I"
				echo
				break
			fi
		done
	fi
}

# Create a directory if it doesn't exist.
function to {
	if [[ "$#" -ne 1 ]]; then
		_fmt usage 'to <dir>'
		return 1
	fi

	local DIR="$1"

	# Do not respect CDPATH.
	if ! grep -q '^/' <<<"$DIR"; then
		DIR="./$DIR"
	fi

	mkdir -p "$DIR" && cd "$DIR"
}

# Simple timer with precision of one second.
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

#======================= System Setup ======================================
# setxkbmap -model pc104 \
# 		  -layout 'us,ru' \
# 		  -variant ',winkeys' \
# 		  -option 'grp:caps_toggle' \
# 		  -option 'grp:win_switch' \
# 		  -option 'grp_led:caps' \
# 		  -option 'compose:menu' \
# 		  -option 'numpad:microsoft'
#===========================================================================

#======================= Host Config =======================================
[[ -r .bashrc.host ]] && source .bashrc.host
#===========================================================================
