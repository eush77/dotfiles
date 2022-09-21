# -*- mode: shell-script; indent-tabs-mode: t; tab-width: 4; -*-#

LESSOPEN="| git highlight %s"

# Set up a `diff-highlight` command. The variable is referenced in the Git
# config.
if [[ -x "/usr/share/git/diff-highlight/diff-highlight" ]]
then
	export DIFF_HIGHLIGHT="/usr/share/git/diff-highlight/diff-highlight"
elif [[ -r "/usr/share/doc/git/contrib/diff-highlight/diff-highlight" ]]
then
	export DIFF_HIGHLIGHT="perl /usr/share/doc/git/contrib/diff-highlight/diff-highlight"
else
	export DIFF_HIGHLIGHT="cat"
fi

# Define completions.
[[ -r "/usr/share/bash-completion/completions/git" ]] &&
	source "/usr/share/bash-completion/completions/git"

#
# PS1
#

if [[ -r "/usr/share/git/git-prompt.sh" ]]
then
	GIT_PS1="/usr/share/git/git-prompt.sh"
elif [[ -r "/usr/lib/git-core/git-sh-prompt" ]]
then
	GIT_PS1="/usr/lib/git-core/git-sh-prompt"
fi

# Define prompt function.
if [[ -v GIT_PS1 ]]
then
	GIT_PS1_SHOWDIRTYSTATE=1
	GIT_PS1_SHOWUNTRACKEDFILES=1
	GIT_PS1_SHOWUPSTREAM=verbose
	GIT_PS1_STATESEPARATOR=

	source "${GIT_PS1}"
	PS1=${PS1/$\{PS1_EXT\}/$\{PS1_EXT\}$\(__git_ps1 \"(%s) \"\)}
fi

#
# Git
#

alias g=git

type __git_complete > /dev/null && __git_complete g __git_main

#
# Git Grep
#

if type -P fzf > /dev/null
then
	function gg {
		{
			git grep "$@";
			echo EOF;
		} | fzf |
			grep --perl-regexp --only-matching '.*:\d+(?=:)' | {
			IFS=: read -ra PT;
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

type __git_complete > /dev/null && __git_complete gg _git_grep

#
# Fzf Widget
#

function __my_git_widget_select__ {
	local BINDINGS HEADER MAIN MAIN_PRI

	while IFS=$'\t' read KEY DESCRIPTION COMMAND PRI
	do
		[[ "$PRI" -eq 0 ]] && continue

		[[ "$PRI" -gt "$MAIN_PRI" ]] && {
			MAIN=$COMMAND
			MAIN_PRI=$PRI
		}

		BINDINGS+=("--bind=alt-$KEY:clear-query+reload:$COMMAND")
		HEADER+="${HEADER:+$'\t'}[M-$KEY]: $DESCRIPTION"
	done <<-EOF
		b	branch	git branch --all --color=always	1
		r	commit	git log -10 --color=always --decorate --oneline ${1:+${1@Q}} --	$([[ -n "$1" ]] && echo 3 || echo 1)
		s	status	git -c color.status=always status --short	$([[ -n "$(git status --porcelain | head -1)" ]] && echo 2 || echo 0)
	EOF

	eval "$MAIN" |
		fzf --ansi --header="$HEADER" --header-first --multi "${BINDINGS[@]}" |
		while read -r FIRST SECOND _
		do
			if [[ "${#FIRST}" -gt 2 ]]
			then
				git name-rev --always --name-only "$FIRST"
			else
				printf "$SECOND\n"
			fi
		done |
		xargs -r echo
}

function __my_git_widget__ {
	local FRONT=${READLINE_LINE:0:$READLINE_POINT}
	local BACK=${READLINE_LINE:$READLINE_POINT}

	local ARG=${FRONT##* }
	local SUB=$(__my_git_widget_select__ "$ARG")

    if [[ -n "$SUB" ]]
	then
		READLINE_LINE="${FRONT:0:${#FRONT}-${#ARG}}$SUB$BACK"
		READLINE_POINT=$((READLINE_POINT - ${#ARG} + ${#SUB}))
	fi
}

bind -x '"\M-g": __my_git_widget__'
