set -g fish_prompt_color_path 423189
set -g fish_prompt_color_equivalent_path brown

function fish_prompt --description='Fancy prompt'
         if test $TMUX
            set_color $fish_prompt_color_equivalent_path
         else
            set_color $fish_prompt_color_path
         end
         printf (prompt_pwd)
         set_color normal
         printf ' ❩  '
end
