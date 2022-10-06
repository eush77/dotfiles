# -*- mode: shell-script; indent-tabs-mode: t; tab-width: 4; -*-#

[[ -r /usr/share/fzf/completion.bash ]] &&
	source /usr/share/fzf/completion.bash
[[ -r /usr/share/fzf/key-bindings.bash ]] &&
	source /usr/share/fzf/key-bindings.bash
[[ -r /usr/share/doc/fzf/examples/completion.bash ]] &&
	source /usr/share/doc/fzf/examples/completion.bash
[[ -r /usr/share/doc/fzf/examples/key-bindings.bash ]] &&
	source /usr/share/doc/fzf/examples/key-bindings.bash

export FZF_DEFAULT_OPTS;
printf -v FZF_DEFAULT_OPTS "%s " \
	   --bind=alt-\\\<:first \
	   --bind=alt-\\\>:last \
	   --bind=alt-i:toggle-all \
	   --bind=alt-n:preview-page-down \
	   --bind=alt-p:preview-page-up \
	   --bind=alt-v:page-up \
	   --bind=btab:jump-accept \
	   --bind=ctrl-k:kill-line \
	   --bind=ctrl-v:page-down \
	   --color=dark \
	   --info=inline \
	   --height=40% \
	   --layout=reverse

bind '"\C-l": "\C-e |& fzf\C-m"'
bind -r '\C-t'
bind '"\C-t": transpose-chars'

#
# File Widget
#

function __my_file_select__ {
	local PROMPT=${1/#$HOME\//\~/}

	if [[ "$PROMPT" != "/" ]]
	then
		PROMPT+="/"
	fi

	while read -r FILE
	do
		if [[ -d "$1/$FILE" ]]
		then
			printf '\033[36m%s\033[m\n' "$FILE"
		else
			echo "$FILE"
		fi
	done | fzf --ansi \
			   --no-info \
			   --preview="[[ -d \"$1\"/{} ]] && ls --almost-all -C --color \"$1\"/{} || \
						  source-highlight --out-format=esc -i \"$1\"/{} 2> /dev/null || cat \"$1\"/{}" \
			   --preview-window=,$((COLUMNS - ${#PROMPT} - 20)) \
			   --prompt="$PROMPT" \
			   "${@:2}" |
		xargs -r printf '%s/%s' "$1" |
		xargs -r realpath
}

function __my_format_file_name__ {
	local FILE=$(realpath --relative-base="$PWD" "$1")
	echo "${FILE/#$HOME\//\~/}"
}

function __my_directory_widget__ {
	local PREV DIR=$PWD

	until [[ -z "$DIR" || "$DIR" = "$PREV" ]]
	do
		PREV=$DIR
		DIR=$({
				 echo .
				 echo ..
				 find -L "$DIR" -mindepth 1 -maxdepth 1 -type d ! -name '.*' -printf '%P\n' 2> /dev/null
				 find -L "$DIR" -mindepth 1 -maxdepth 1 -type d -name '.*' -printf '%P\n' 2> /dev/null
				 find -L "$DIR" -mindepth 2 -type d \( -path '*/.*' -prune -o -printf '%P\n' \) 2> /dev/null
			 } | __my_file_select__ "$DIR" --bind=backward-eof:first+down+accept)
	done

	[[ -n "$DIR" ]] || return

	if [[ ! -v READLINE_LINE ]]
	then
		echo "$DIR"
		return
	fi

	DIR=$(__my_format_file_name__ "$DIR")

	READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$DIR${READLINE_LINE:$READLINE_POINT}"
	READLINE_POINT=$((READLINE_POINT + ${#DIR}))
}

function __my_file_widget__ {
	local FILE=$PWD

	while [[ -d "$FILE" ]]
	do
		FILE=$({
				  echo ..
				  find -L "$FILE" -mindepth 1 -maxdepth 1 -type d ! -name '.*' -printf '%P\n' 2> /dev/null
				  find -L "$FILE" -mindepth 1 -maxdepth 1 -type d -name '.*' -printf '%P\n' 2> /dev/null
				  find -L "$FILE" -mindepth 1 -maxdepth 1 -type f ! -name '*~' -printf '%P\n' 2> /dev/null
				  find -L "$FILE" -mindepth 2 \( -name '*~' -o -path '*/.*' \) -prune -o -printf '%P\n' 2> /dev/null
			  } | __my_file_select__ "$FILE" --bind=backward-eof:first+accept)
	done

	FILE=$(__my_format_file_name__ "$FILE")

	READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$FILE${READLINE_LINE:$READLINE_POINT}"
	READLINE_POINT=$((READLINE_POINT + ${#FILE}))
}

bind '"\M-c": "\C-e \C-a\C-k `__my_directory_widget__`\C-m\C-y\C-b\C-d"'
bind -x '"\M-C": __my_directory_widget__'
bind -x '"\M-v": __my_file_widget__'

#
# rgrep
#

function rgrep {
	{
		grep --color=always --line-number --recursive --with-filename $RGREPFLAGS "$@"
		printf '\r\e[38;5;110m>%s' $'\u25A0' >&2
	} | fzf --ansi |
		if IFS=: read -r FILE LINE _  && [[ "$FILE" != EOF ]]
		then
			$PAGER -N +"$LINE" "$FILE"
		fi
}
