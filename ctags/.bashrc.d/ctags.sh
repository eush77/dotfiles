# -*- mode: shell-script; indent-tabs-mode: t; tab-width: 4; -*-#

function tt {
	if [[ "$1" = "--help" ]]
	then
		echo "Usage: tt <query> [<file>|<dir>|<ctags-flags>]..."
		echo
		echo "Interactively select a tag by QUERY and jump to tag definition."
		echo "If no FILE or DIR given, search tags in the current directory."
		return
	fi

	local QUERY=$1
	shift

	local -a CTAGS_FLAGS
	local -a FILES

	for ARG in "$@"
	do
		if [[ "$ARG" = -* ]]
		then
			CTAGS_FLAGS=("${CTAGS_FLAGS[@]}" "$ARG")
		elif [[ -f "$ARG" ]]
		then
			FILES=("${FILES[@]}" "$ARG")
		elif [[ -d "$ARG" ]]
		then
			FILES=("${FILES[@]}" "$ARG"/**)
		fi
	done

	if [[ "${#FILES}" -eq 0 ]]
	then
		FILES=($(find -type f))
	fi

	ctags --fields='{kind}{input}{line}{scope}{name}{signature}' --output-format=json "${CTAGS_FLAGS[@]}" "${FILES[@]}" |
		jq -r 'select(.kind != "namespace") |
		       "'$'\e[90m''\(.path):\(.line):'$'\e[m''\t\(if .scope then "\(.scope)::" else "\(.kind) " end
			    )\(.name)\(.signature // "")"' |
		sort |
		fzf -0 --ansi --query="'$QUERY" |
		if IFS=: read -r FILE LINE _
		then
			$PAGER -N +"$LINE" "$FILE"
		fi
}
