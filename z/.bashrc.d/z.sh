# -*- mode: shell-script; indent-tabs-mode: t; tab-width: 4; -*-#

[[ -r /usr/share/z/z.sh ]] && source /usr/share/z/z.sh
[[ -r /etc/profile.d/z.sh ]] && source /etc/profile.d/z.sh

function __my_z_widget__ {
	z -l "$@" |
		tac |
		awk '{ print $2 }' |
		__my_file_select__ / --no-sort --preview-window=,50% |
		xargs -r printf 'cd %q\n'
}

type -t z > /dev/null &&
	bind '"\M-z": "\C-e \C-a\C-k `__my_z_widget__`\C-m\C-y\C-b\C-d"'
