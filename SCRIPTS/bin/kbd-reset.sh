#!/bin/bash
# -*- mode: shell-script; indent-tabs-mode: t; tab-width: 4; -*-#
set -euo pipefail
IFS=$'\n'
export LC_COLLATE=C

#> kbd-reset.sh
#>
#> Generate a reset keymap file that assigns Control and Meta keysyms missing
#> in the default keymap `defkeymap.map.gz'. The resulting keymap is most
#> convenient as a starting point for any customizations.

cat - <(join <(dumpkeys -l |
				   grep -Eo '\b(Meta|Control)_\w+' |
				   sed -r 's:(Meta_)?(Control_)?(.*):\3 &\1 \2 :; s:Meta_ : alt:; s:Control_ : control:' |
				   sort) \
			 <(dumpkeys -f |
				   awk '/^keycode/ {
					   if ($4 && $4 != "nul" && $4 != "VoidSymbol") print $4, $1, $2, $3;
					   if ($5 && $5 != "nul" && $5 != "VoidSymbol") print $5, "shift", $1, $2, $3;
				   }' |
				   sed 's:^+::' |
				   sort) \
			 -o 1.3,1.4,2.2,2.3,2.4,2.5,1.2) <<-EOF
	# Generated from $(basename "$0") on $(date).

	include "defkeymap.map.gz"

EOF
