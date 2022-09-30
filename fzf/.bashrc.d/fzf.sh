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
	   --bind=alt-i:toggle-all \
	   --bind=alt-n:preview-page-down \
	   --bind=alt-p:preview-page-up \
	   --bind=alt-v:page-up \
	   --bind=ctrl-k:kill-line \
	   --bind=ctrl-v:page-down \
	   --bind=tab:jump-accept \
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

function __my_file_widget_select__ {
	local PROMPT=$(realpath "$1")

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
	done | fzf --ansi --prompt="$PROMPT" "${@:2}" |
		xargs -r printf '%s/%s' "$1" |
		xargs -r realpath
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
			 } | __my_file_widget_select__ "$DIR" \
										   --bind=backward-eof:first+down+accept \
										   --no-info \
										   --preview="ls --almost-all -C --color \"$DIR\"/{}" \
										   --preview-window=,$((COLUMNS - ${#DIR} - 20)))
	done

	[[ -n "$DIR" ]] &&
		realpath --relative-to="$PWD" "$DIR"
}

function __my_file_widget__ {
	local FILE=.

	while [[ -d "$FILE" ]]
	do
		FILE=$({
				  echo ..
				  find -L "$FILE" -mindepth 1 -maxdepth 1 -type d ! -name '.*' -printf '%P\n' 2> /dev/null
				  find -L "$FILE" -mindepth 1 -maxdepth 1 -type d -name '.*' -printf '%P\n' 2> /dev/null
				  find -L "$FILE" -mindepth 1 -maxdepth 1 -type f ! -name '*~' -printf '%P\n' 2> /dev/null
				  find -L "$FILE" -mindepth 2 \( -name '*~' -o -path '*/.*' \) -prune -o -printf '%P\n' 2> /dev/null
			  } | __my_file_widget_select__ "$FILE" --bind=backward-eof:first+accept)
	done

	READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$FILE${READLINE_LINE:$READLINE_POINT}"
	READLINE_POINT=$((READLINE_POINT + ${#FILE}))
}


bind '"\M-c": "\C-e \C-a\C-k `__my_directory_widget__`\C-m\C-y\C-b\C-d"'
bind -x '"\M-v": __my_file_widget__'

#
# rgrep
#

function rgrep {
	{
		grep --color=always --line-number --recursive --with-filename $RGREPFLAGS "$@"
		echo EOF
	} | fzf --ansi |
		if IFS=: read -r FILE LINE _  && [[ "$FILE" != EOF ]]
		then
			$PAGER -N +"$LINE" "$FILE"
		fi
}
