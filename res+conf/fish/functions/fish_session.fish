function fish_session -a name --description='Name this shell session'
         if test $name
            set -g fish_session_name $name
         else
            read --command=$fish_session_name fish_session_name
         end
end
