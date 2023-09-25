# -*- mode: shell-script; indent-tabs-mode: t; tab-width: 4; -*-#

function __my_bd_select__ {
	local DIR=$PWD

	until [[ "$DIR" = "/" ]]
	do
		DIR=$(dirname "$DIR")
		echo "$DIR"
	done | __my_file_select__ / --preview-window=,$((COLUMNS - ${#PWD}))
}

function __my_bd__ {
	if [[ "$#" -ne 0 ]]
	then
		. bd -si "$@"
		return
	fi

	local DIR=$(__my_bd_select__)

	if [[ -n "$DIR" ]]
	then
		cd "$DIR"
	fi
}

[[ -x "$(type -P bd)" ]] &&
	alias bd=__my_bd__
