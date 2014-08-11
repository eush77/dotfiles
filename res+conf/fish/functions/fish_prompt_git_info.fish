function fish_prompt_git_info --argument-names text_color --description='Include Git info in the current prompt'
         if git status >/dev/null ^&1
            printf '['

            set branch (git rev-parse --abbrev-ref HEAD)

            if test (count (git status --short)) -eq 0
               set_color green
               printf '%s' $branch
            else
               # Dirty state.
               set_color yellow
               printf '%s*' $branch
            end

            set_color $text_color
            printf ']'
         end
end
