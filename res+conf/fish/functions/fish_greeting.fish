set -gx fish_session_word_generator ~/.config/fish/functions/fish_greeting.d/word
set -gx fish_greeting_color_session 89272b

function fish_greeting --description 'Configured Fish greeting'
         printf 'Welcome to Fish. '
         if test $SHLVL -eq 1
            set -gx fish_session_name (eval $fish_session_word_generator)
            printf "Session name: "
            set_color $fish_greeting_color_session
            printf $fish_session_name
            set_color normal
            printf ".\nDon't hesitate to change it via fish_session.\n"
         else
            printf "This shell is $SHLVL shells deep.\n"
         end
end
