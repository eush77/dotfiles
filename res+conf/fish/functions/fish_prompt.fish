set -gx fish_prompt_word_generator ~/.config/fish/functions/fish_prompt.d/word
set -g fish_prompt_color_session_name 4d1618
set -g fish_prompt_color_path 423189

function fish_prompt --description='Fancy prompt'
         if not set --query fish_prompt_session_name
            set -g fish_prompt_session_name (eval $fish_prompt_word_generator)
         end
         set_color $fish_prompt_color_session_name
         echo -n $fish_prompt_session_name
         set_color normal
         echo -n ' '
         set_color $fish_prompt_color_path
         echo -n (prompt_pwd)
         set_color normal
         echo ' ❩  '
end

function prompt_title --description='Name this shell session'
         set -g fish_prompt_session_name $argv
end
