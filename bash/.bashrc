# -*- mode: shell-script; indent-tabs-mode: t; tab-width: 4; -*-#

#
# Environment
#

export EDITOR=mg
export LESSOPEN="| source-highlight --out-format esc -i %s"
export PAGER="less --quit-if-one-screen"
export PLS="less -N +%d"
export W3MMAN_W3M='w3m -o confirm_qq=false'
export WWW_HOME='https://google.com/'

# If not running interactively, don't go any further.
[[ $- != *i* ]] && return

# LESS_TERMCAP variables can't be put in a `.lesskey' file in Less 487.
# https://unix.stackexchange.com/questions/328597/how-to-make-lesskey-terminal-independent
export LESS_TERMCAP_md="$(tput bold; tput setaf 6)"
export LESS_TERMCAP_me="$(tput sgr0)"
export LESS_TERMCAP_so="$(tput bold; tput setaf 3; tput setab 4)"
export LESS_TERMCAP_se="$(tput sgr0)"
export LESS_TERMCAP_us="$(tput setaf 3)"
export LESS_TERMCAP_ue="$(tput sgr0)"

eval "$(dircolors)"

#
# Shell Settings
#

FIGNORE='~'
HISTCONTROL=ignoreboth
HISTFILESIZE=20000
HISTIGNORE='l:la:lc:ll:fg:fg *:bg:bg *:cd:cd *:pushd:popd:popd *'
HISTSIZE=15000
HISTTIMEFORMAT='(%d.%m|%R)  '
IGNOREEOF=0
PROMPT_COMMAND='history -a; __prompt_listing'
PROMPT_LISTING_LIMIT=20
PS1='\[$((PS1_STATUS=$?))\r\]!\! ${PS1_SHLVL}$(__git_ps1 "(%s) ")\[\e[36m\]${PS1_HOSTID}\[\e[0m\]${PS1_HOSTID_SUFFIX}\[\e[36m\]\[\e[36m\]$(__ps1_paths)\[\e[31m\]$(__ps1_failure)\[\e[0m\]$(__ps1_success) '

# Calculate SHLVL nesting level indicator for PS1.
if [[ -n "$DISPLAY" ]]; then
	# Terminal emulators in desktop sessions start at SHLVL=2.
	SHLVL_BASE=2
else
	SHLVL_BASE=1
fi
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

alias __git_ps1=true  # default value, redefined later

shopt -s \
	  autocd \
	  cdspell \
	  checkjobs \
	  checkwinsize \
	  cmdhist \
	  direxpand \
	  dirspell \
	  extglob \
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

# Load bash-completion, if available.
[[ -r /usr/share/bash-completion/bash_completion ]] &&
	source /usr/share/bash-completion/bash_completion

#
# Readline
#

bind 'set colored-stats on'
bind 'set completion-ignore-case on'
bind 'set convert-meta on'
bind 'set match-hidden-files off'

bind 'Meta-n: menu-complete'
bind 'Meta-p: menu-complete-backward'
bind 'SPACE: magic-space'

bind '"\C-h": "\C-apls -a -- \C-m"'
bind '"\C-v": "\C-e |& $PAGER\C-m"'
bind -x '"\M-i": READLINE_LINE="i $READLINE_LINE"; let READLINE_POINT+=2'
bind -x '"\M-p": READLINE_LINE="\$PAGER $READLINE_LINE"; let READLINE_POINT+=7'
bind -x '"\M-s": READLINE_LINE="sudo $READLINE_LINE"; let READLINE_POINT+=5'

# Commacd
bind '"\C-]": ",,, "'

#
# Aliases
#

# Include Grc aliases.
[[ -r /etc/profile.d/grc.bashrc ]] && source /etc/profile.d/grc.bashrc
[[ "$(type -t make)" = alias ]] && unalias make  # Preserve CMake coloring

alias bc='bc -q'
alias catll='cat >/dev/null'
alias cp='cp --reflink=auto'
alias df='df -h'
alias diff='diff --color=auto'
alias dirs='dirs -v'
alias du='du -h'
alias emacs='emacs -nw'
alias grep='grep --color=auto'
alias hilite='source-highlight -fesc -i'
alias l='ls -v --classify --group-directories-first --ignore-backups'
alias la='ls -lv --all --classify --group-directories-first --human-readable'
alias lc='ls -1v --group-directories-first --ignore-backups'
alias ls='ls --color=auto'
alias pacaur='pacaur --color=auto'
alias pacman='pacman --color=auto'
alias rgrep='pls -a -- grep --color=always --line-number --recursive --with-filename $RGREPFLAGS'
alias ready="zenity --info --text='Ready, Master!' --title=''"
alias reinit='source ~/.bashrc'
alias sudo='sudo '  # Expand aliases under `sudo'.

# Mplayer profiles.
alias mplayer.fs='mplayer -profile fs'
alias mplayer.top='mplayer -profile top'
alias mplayer.bottom='mplayer -profile bottom'
alias mplayer.tl='mplayer -profile tl'
alias mplayer.tr='mplayer -profile tr'
alias mplayer.bl='mplayer -profile bl'
alias mplayer.br='mplayer -profile br'

#
# Functions
#

function e {
	# Use indirection to allow EDITOR to be redefined by the host.
	$EDITOR "$@"
}

# List files on directory change.
function __prompt_listing {
	[[ "${_LAST_PWD:=$PWD}" != "$PWD" ]] &&
		[[ "$(find -maxdepth 1 | wc -l)" -le "$PROMPT_LISTING_LIMIT" ]] &&
		l
	_LAST_PWD="$PWD"
}

# This function is called from PS1 to print a prompt sigil after a failed
# command.
function __ps1_failure {
	[[ "$PS1_STATUS" -ne 0 ]] && echo '>'
}

# This function is called from PS1 to print a prompt sigil after a succeeded
# command.
function __ps1_success {
	[[ "$PS1_STATUS" -eq 0 ]] && echo '>'
}

# This function is called from PS1 to print the directory stack.
function __ps1_paths {
	builtin dirs -p | sed '2,$ { s:.*/:: }' | tac | paste -sd,
}

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

# cdtree [<tree>] - Change current directory to the corresponding directory in
# the other directory tree.
#
# Paths to directory trees must be specified in the array CDTREE.
function cdtree {
	local tree

	# Determine the current directory tree.
	local treeindex=0
	for tree in "${CDTREE[@]}"; do
		if [[ "$PWD" = "$tree"/* ]]; then
			local thistree="$tree"
			break
		else
			(( ++treeindex ))
		fi
	done
	if [[ ! -v thistree ]]; then
		_fmt error 'Not in a directory tree'
		return 1
	fi

	# Check if the other tree is specified explicitly as a function parameter,
	# or else choose the next tree in the list.
	local othertree
	if [[ -n "$1" ]]; then
		if [[ -d "$1" ]]; then
			othertree="$1"
		else
			# Try to resolve the argument against paths in CDTREE.
			for tree in "${CDTREE[@]}"; do
				if [[ "$tree" = */"$1" ]]; then
					othertree="$tree"
					break
				fi
			done
			if [[ -z "$othertree" ]]; then
				_fmt error "Directory tree not found: $1"
				return 1
			fi
		fi
	else
		othertree="${CDTREE[$(((treeindex + 1) % ${#CDTREE[@]}))]}"
	fi

	# Change to a directory in the other tree. Traverse up any subdirectories
	# that exist in one subtree but not the other.
	local otherdir="$othertree${PWD#$thistree}"
	while ! cd "$otherdir" 2>/dev/null; do
		otherdir="${otherdir%/*}"
	done
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

# Man wrapper that sets MANWIDTH dynamically.
function man {
	if [[ "$COLUMNS" -gt 78 ]]; then
		local -x MANWIDTH=78
	fi

	command man "$@"
}

# i [-s | --select] <topic> [<index term>] - Display documentation.
#
# Chooses between Info and W3MMAN.
#
# With `--select', asks for the manual to display, otherwise selects the first
# found.
function i {
	local opt="$(getopt -n 'i' -o 's' -l 'select' -- "$@")"
	if [[ "$?" -ne 0 ]]; then
		return 1
	fi

	local select topic term

	for opt in $(eval printf '%s\\n' "$opt"); do
		case "$opt" in
			--select | -s)
				select=1
				;;
			--)
				;;
			*)
				if [[ ! -v topic ]]; then
					topic="$opt"
				elif [[ ! -v term ]]; then
					term="$opt"
				else
					_fmt error 'i <topic> [<term>]'
					return 2
				fi
		esac
	done

	local docfiles
	mapfile -t docfiles < <(cat <(cd /; info --where --all "$topic" |
									  sed '/^\*manpages\*$/ d') \
								<(man --where --all "$topic" 2>/dev/null))

	local docfile
	case "${#docfiles[@]}" in
		0)
			_fmt error "Not found"
			return 1
			;;
		1)
			docfile="$docfiles"
			;;
		*)
			if [[ -v select ]]; then
				local PS3='Select documentation: '
				select sel in "${docfiles[@]}"; do
					if [[ -n "$sel" ]]; then
						docfile="$sel"
						break
					fi
				done
			else
				docfile="${docfiles[0]}"
			fi
			;;
	esac

	if [[ "$(basename "$docfile")" = *.info?(.gz) ]]; then
		local args=()

		if [[ -n "$term" ]]; then
			args+=("--index-search=$term")
		fi

		info --usage "$topic" "${args[@]}"
	elif [[ -z "$term" ]]; then
		w3mman -l "$docfile"
	else
		MANPAGER="$PAGER --pattern='$term'" man "$topic"
	fi
}

# youtube-mw <filename> - Mark the video watched on YouTube, assuming it has
# been downloaded with YouTube-dl (and not renamed).
function youtube-mw {
	local id="$(grep --perl-regexp --only-matching '(?<=-)[\w-]{11}(?=\.)' \
		  		<<<"$1")"
	[[ -n "$id" ]] &&
		_fmt note "Marking watched $id"
		youtube-dl --simulate --mark-watched \
				   --username="$YOUTUBE_USERNAME" -- "$id"
}

#
# Utilities
#

: acpi :

# Show battery status if running on a TTY.
[[ -z "$DISPLAY" ]] &&
	[[ -e /sys/class/power_supply/BAT1/status ]] &&
	[[ "$(</sys/class/power_supply/BAT1/status)" = "Discharging" ]] &&
	acpi --battery

: commacd :

[[ -r /usr/share/commacd/commacd.bash ]] &&
	source /usr/share/commacd/commacd.bash

: direnv :

[[ "$(type -t direnv)" = "file" ]] &&
	eval "$(direnv hook bash)"

: fzf :

[[ -x "$(type -P fzf)" ]] && {
	export FZF_DEFAULT_OPTS;
	printf -v FZF_DEFAULT_OPTS "%s " \
	       --bind=ctrl-k:kill-line \
	       --color=dark \
	       --info=inline \
	       --height=40% \
	       --layout=reverse;
	[[ -r ~/.fzf.bash ]] &&
		source ~/.fzf.bash
	[[ -r /usr/share/fzf/completion.bash ]] &&
		source /usr/share/fzf/completion.bash
	[[ -r /usr/share/fzf/key-bindings.bash ]] &&
		source /usr/share/fzf/key-bindings.bash
	bind '"\C-l": "\C-e |& fzf\C-m"';
	bind '"\C-t": transpose-chars';
	bind '"\M-c": "\C-e \C-a\C-k `__fzf_cd__`\C-m\C-y\C-b\C-d"'
	bind -x '"\M-v": fzf-file-widget';

	# browse [dir] [query] - Browse files with Fzf
	[[ -x "$(type -P ctags)" ]] && function browse {
		local DIR="${1:-$PWD}"
		( cd "$DIR" &&
		  fzf --bind="alt-n:preview-page-down" \
		      --bind="alt-p:preview-page-up" \
		      --bind="alt-v:page-up" \
		      --bind="change:top" \
		      --bind="ctrl-c:cancel" \
		      --bind="ctrl-j:jump" \
		      --bind="ctrl-v:page-down" \
		      --bind="return:execute(less {})" \
		      --height=100% \
		      --layout=reverse-list \
		      --preview="ctags -x {} | ifne -n cat {}" \
		      --preview-window=:60% \
		      --query="$2" )
	}
}

: git :

[[ -x "$(type -P git)" ]] && {
	alias g=git

	if [[ -x "$(type -P fzf)" ]]; then
		function gg {
			git grep "$@" |
			fzf |
			grep --perl-regexp --only-matching '.*:\d+(?=:)' | {
				IFS=: read -a PT;
				case "${#PT[@]}" in
					2) $PAGER -N +"${PT[1]}" "${PT[0]}" ;;
					3) git -c pager.show="$PAGER -N +${PT[2]}" \
					   show "${PT[0]}:${PT[1]}" ;;
				esac;
			}
		}
	else
		alias gg="git grep"
	fi

	# Define completions.
	[[ -r "/usr/share/bash-completion/completions/git" ]] &&
		source "/usr/share/bash-completion/completions/git" &&
		__git_complete g __git_main &&
		__git_complete gg _git_grep

	if [[ -r "/usr/share/git/git-prompt.sh" ]]; then
		GIT_PS1="/usr/share/git/git-prompt.sh"
	elif [[ -r "/usr/lib/git-core/git-sh-prompt" ]]; then
		GIT_PS1="/usr/lib/git-core/git-sh-prompt"
	fi

	# Define prompt function.
	if [[ -v GIT_PS1 ]]; then
		GIT_PS1_SHOWDIRTYSTATE=1
		GIT_PS1_SHOWUNTRACKEDFILES=1
		GIT_PS1_SHOWUPSTREAM=verbose
		GIT_PS1_STATESEPARATOR=

		unalias __git_ps1
		source "${GIT_PS1}"
	fi

	# Fzf widget for inserting files from Git status.
	[[ -x "$(type -P fzf)" ]] && {
		function __fzf_git_status__ {
			local FILES=$(git status --porcelain | fzf --multi | cut -c4- | while read -r FILE; do
				printf '%q ' "$FILE"
			done)
			READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$FILES${READLINE_LINE:$READLINE_POINT}"
			READLINE_POINT=$(( READLINE_POINT + ${#FILES} ))
		}

		bind -x '"\M-g": __fzf_git_status__'
	}
}

: pkgfile :

[[ -r "/usr/share/doc/pkgfile/command-not-found.bash" ]] &&
	source "/usr/share/doc/pkgfile/command-not-found.bash"

: z :

[[ -r /usr/share/z/z.sh ]] && source /usr/share/z/z.sh
[[ -r /etc/profile.d/z.sh ]] && source /etc/profile.d/z.sh

: z + fzf :

[[ (-r /usr/share/z/z.sh || -r /etc/profile.d/z.sh) && -x $(type -P fzf) ]] && {
	function __fzf_z__ {
		local line
		line=$(z -l $@ | tac | fzf --no-sort) && printf "cd %q\n" "${line##+([0-9])+( )}"
	}
	bind '"\M-z": "\C-e \C-a\C-k `__fzf_z__`\C-m\C-y\C-b\C-d"'
}

#
# Host Config
#

[[ -r ~/.bashrc.host ]] && source ~/.bashrc.host
true
