set -g fish_prompt_color_session 4d1618
set -g fish_prompt_color_path 423189

function fish_prompt --description='Fancy prompt'
         set_color $fish_prompt_color_session
         if test $SHLVL -ne 1
            printf '<%d> ' $SHLVL
         end
         printf $fish_session_name
         set_color normal
         printf ' '
         set_color $fish_prompt_color_path
         printf (prompt_pwd)
         set_color normal
         printf ' ❩  '
end
