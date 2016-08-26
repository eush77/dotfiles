set -g fish_prompt_color_path 423189
set -g fish_prompt_color_equivalent_path brown

function fish_prompt --description='Fancy prompt'
  set -l exit_code $status

  set_color yellow

  if set --query CMD_DURATION
    printf '\a> %s\n' $CMD_DURATION
  end

  if test $exit_code -ne 0
    printf '> code %s\n' $exit_code
  end

  if test $TMUX
    set text_color $fish_prompt_color_equivalent_path
  else
    set text_color $fish_prompt_color_path
  end
  set_color $text_color

  if test $USER = root
    set_color red
    printf '[#] '
    set_color $text_color
  end

  if test $SHLVL -gt 1
    printf '(+%d) ' (math $SHLVL - 1)
  end

  if fish_prompt_git_info $text_color
    printf ' '
  end
  set_color $text_color

  # Print directory stack.
  if begin; count $dirstack >/dev/null; end
    printf "%s\n" $dirstack |tac |xargs -n1 basename |tr "\n" ","
  end

  set -l path (prompt_pwd)

  # Special-case /tmp/tmpdir.
  if begin; printf '%s' (pwd) |grep -q '^/tmp/tmpdir/'; end
    set path (printf '%s' $path |sed 's:/t/t/:\#:')
  end

  printf '%s' $path
  set_color normal
  printf ' ❩  '

  # TODO: This should be in another function.
  if test (count $fish_with_command) -gt 0
      commandline -r "$fish_with_command "
  end
end
