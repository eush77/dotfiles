# `M-l` is bound to `__fish_list_current_token` by default. Rebind it to `M-m`
# and restore the usual readline binding.
bind \em __fish_list_current_token
bind \el downcase-word

# Paginate-and-execute combo.
bind \e\n "__fish_paginate; and commandline --function execute"

# Multiline editing.
bind \ej "commandline -i \ \\\\\n"
