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
bind '"\M-c": "\C-e \C-a\C-k `__fzf_cd__`\C-m\C-y\C-b\C-d"'

#
# File Widget
#

function __my_file_widget_select__ {
	{
		echo ..
		find -L "$1" -mindepth 1 -maxdepth 1 -type d ! -name '.*' -printf '%P\n'
		find -L "$1" -mindepth 1 -maxdepth 1 -type f ! -name '*~' -printf '%P\n'
		find -L "$1" -mindepth 2 \( -name '*~' -o -path '*/.git/*' -o -path '*/.repo/*' \) -prune -o -printf '%P\n'
	} | while read -r FILE
	do
		if [[ -d "$1/$FILE" ]]
		then
			printf '\033[36m%s\033[m\n' "$FILE"
		else
			echo "$FILE"
		fi
	done | fzf --ansi --bind=backward-eof:first+accept --prompt="$(realpath "$1")/" |
		xargs -r printf '%s/%s' "$1" |
		xargs -r realpath --relative-to="$PWD"
}

function __my_file_widget__ {
	local FILE=.

	while [[ -d "$FILE" ]]
	do
		FILE=$(__my_file_widget_select__ "$FILE")
	done

	READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$FILE${READLINE_LINE:$READLINE_POINT}"
	READLINE_POINT=$((READLINE_POINT + ${#FILE}))
}

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
