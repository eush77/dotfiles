# -*- mode: shell-script; indent-tabs-mode: t; tab-width: 4; -*-#

#======================== Environment Variables ============================
export EDITOR=mg
export LESS='--ignore-case --RAW-CONTROL-CHARS --quit-if-one-screen --no-init --jump-target=6'
export LESS_TERMCAP_mb=$'\e[01;31m'
export LESS_TERMCAP_md=$'\e[01;31m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;44;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[01;32m'
export PAGER=less
export WWW_HOME='https://ddg.gg/'

eval "$(dircolors)"
#===========================================================================

# If not running interactively, don't go any further.
[[ $- != *i* ]] && return

#============================ Shell Settings ===============================
FIGNORE='~'
HISTCONTROL=ignoreboth
HISTFILESIZE=20000
HISTIGNORE='l:la:lc:ll:fg:fg *:bg:bg *:cd:cd *:pushd:pushd *:popd:popd *'
HISTSIZE=15000
HISTTIMEFORMAT='(%d.%m|%R)  '
IGNOREEOF=0
PROMPT_COMMAND='history -a; [[ "${_LAST_PWD:=$PWD}" != "$PWD" ]] && l; _LAST_PWD="$PWD"'
PS1='\[$((PS1_STATUS=$?))\r\]!\! ${PS1_SHLVL}$(__git_ps1 "(%s) ")\[\e[36m\]${PS1_HOSTID}\[\e[0m\]${PS1_HOSTID_SUFFIX}\[\e[36m\]\[\e[36m\]$(__ps1_paths)\[\e[31m\]$(__ps1_failure)\[\e[0m\]$(__ps1_success) '

function __ps1_failure {
	[[ "$PS1_STATUS" -ne 0 ]] && echo '>'
}

function __ps1_success {
	[[ "$PS1_STATUS" -eq 0 ]] && echo '>'
}

SHLVL_BASE="$((${TMUX:+1}+1))"
if [[ "$SHLVL" -gt "$SHLVL_BASE" ]]; then
	PS1_SHLVL="(+$((SHLVL - SHLVL_BASE))) "
else
	PS1_SHLVL=
fi

if [[ -n "$SSH_CONNECTION" ]]; then
	PS1_HOSTID="$USER@${HOSTNAME:0:2}"
	PS1_HOSTID_SUFFIX=:
else
	PS1_HOSTID=
	PS1_HOSTID_SUFFIX=
fi

if type -P git >/dev/null; then
	GIT_PS1_SHOWDIRTYSTATE=1
	GIT_PS1_SHOWUNTRACKEDFILES=1
	GIT_PS1_SHOWUPSTREAM=verbose
	GIT_PS1_STATESEPARATOR=

	source /usr/share/git/git-prompt.sh
else
	alias __git_ps1=true
fi

function __ps1_paths {
	builtin dirs -p | sed '2,$ { s:.*/:: }' | tac | paste -sd,
}

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

bind -x '"\M-i": READLINE_LINE="i $READLINE_LINE"; let READLINE_POINT+=2'
bind -x '"\M-p": READLINE_LINE+=" | $PAGER"'
bind -x '"\M-s": READLINE_LINE="sudo $READLINE_LINE"; let READLINE_POINT+=5'

# Commacd
bind '"\C-]": ",,, "'
#===========================================================================

#============================== Aliases ====================================
# Include Grc aliases.
[[ -r /etc/profile.d/grc.bashrc ]] && source /etc/profile.d/grc.bashrc

alias bc='bc -q'
alias catll='cat >/dev/null'
alias cp='cp --reflink=auto'
alias df='df -h'
alias diff='diff --color=auto'
alias dirs='dirs -v'
alias du='du -h'
alias emacs='emacs -nw'
alias g=git
alias gg='git grep'
alias grep='grep --color=auto'
alias hilite='source-highlight -fesc -i'
alias l='ls -v --classify --group-directories-first --ignore-backups'
alias la='ls -lv --all --classify --group-directories-first --human-readable'
alias lc='ls -1v --group-directories-first --ignore-backups'
alias ls='ls --color=auto'
alias pacaur='pacaur --color=auto'
alias pacman='pacman --color=auto'
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

# i <topic> [<index term>] - Display documentation.
function i {
	local topic="$1"
	local term="$2"

	if [[ "$(info --where "$topic")" = "*manpages*" ]]; then
		if [[ "$COLUMNS" -gt 78 ]]; then
			local -x MANWIDTH=78
		fi

		if [[ -n "$term" ]]; then
			local -x MANPAGER="$PAGER --pattern='$term'"
		fi

		man "$topic"
	else
		local args=()

		if [[ -n "$term" ]]; then
			args+=("--index-search=$term")
		fi

		info "$topic" "${args[@]}"
	fi
}
#===========================================================================

#======================= System Setup ======================================
# Show battery status if running on a TTY.
[[ -z "$DISPLAY" ]] &&
	[[ "$(</sys/class/power_supply/BAT1/status)" = "Discharging" ]] &&
	acpi --battery

# Hook up Commacd.
[[ -r /usr/share/commacd/commacd.bash ]] &&
	source /usr/share/commacd/commacd.bash

# Hook up Z.
[[ -r /usr/share/z/z.sh ]] && source /usr/share/z/z.sh
#===========================================================================

#======================= Host Config =======================================
[[ -r ~/.bashrc.host ]] && source ~/.bashrc.host
#===========================================================================

true
