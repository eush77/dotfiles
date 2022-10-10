# -*- mode: shell-script; indent-tabs-mode: t; tab-width: 4; -*-#

function tt {
	local QUERY=$1
	shift

	local -a FILES

	if [[ "$#" -eq 0 ]]
	then
		FILES=($(find -type f))
	fi

	for FILE in "$@"
	do
		if [[ -f "$FILE" ]]
		then
			FILES=("${FILES[@]}" "$FILE")
		elif [[ -d "$FILE" ]]
		then
			FILES=("${FILES[@]}" "$FILE"/**)
		fi
	done

	ctags --fields='{kind}{input}{line}{scope}{name}{signature}' --output-format=json "${FILES[@]}" |
		jq -r 'select(.kind != "class" and .kind != "namespace") |
		       "'$'\e[90m''\(.path):\(.line):'$'\e[m''\t\(if .scope then "\(.scope)::" else "\(.kind) " end)\(.name)\(.signature // "")"' |
		sort |
		fzf -0 --ansi --query="'$QUERY" |
		if IFS=: read -r FILE LINE _
		then
			$PAGER -N +"$LINE" "$FILE"
		fi
}
