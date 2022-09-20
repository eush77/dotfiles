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

function __my_git_M_g__ {
	local ROOT=$(git root)
	local FILES=$(git status --porcelain | fzf --multi | cut -c4- | while read -r FILE
				  do
					  printf '%q ' $(realpath --relative-to="$PWD" "$ROOT/$FILE")
				  done)
	READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$FILES${READLINE_LINE:$READLINE_POINT}"
	READLINE_POINT=$(( READLINE_POINT + ${#FILES} ))
}

bind -x '"\M-g": __my_git_M_g__'
