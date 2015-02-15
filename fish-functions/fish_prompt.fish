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

  if test $SHLVL -gt 1
    printf '(+%d) ' (math $SHLVL - 1)
  end

  if fish_prompt_git_info $text_color
    printf ' '
  end
  set_color $text_color

  printf (prompt_pwd)
  set_color normal
  printf ' ❩  '
end
