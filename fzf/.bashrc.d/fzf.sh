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
	   --bind=ctrl-k:kill-line \
	   --color=dark \
	   --info=inline \
	   --height=40% \
	   --layout=reverse

bind '"\C-l": "\C-e |& fzf\C-m"'
bind -r '\C-t'
bind '"\C-t": transpose-chars'
bind '"\M-c": "\C-e \C-a\C-k `__fzf_cd__`\C-m\C-y\C-b\C-d"'
bind -x '"\M-v": fzf-file-widget'

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
