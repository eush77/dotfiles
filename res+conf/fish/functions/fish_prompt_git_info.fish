set -g fish_prompt_git_info_clean green
set -g fish_prompt_git_info_equivalent_clean green
set -g fish_prompt_git_info_dirty yellow
set -g fish_prompt_git_info_equivalent_dirty red

function fish_prompt_git_info --argument-names text_color --description='Include Git info in the current prompt'
         if test $TMUX
            set clean_color $fish_prompt_git_info_equivalent_clean
            set dirty_color $fish_prompt_git_info_equivalent_dirty
         else
            set clean_color $fish_prompt_git_info_clean
            set dirty_color $fish_prompt_git_info_dirty
         end

         if git status >/dev/null ^&1
            printf '['

            set branch (git rev-parse --abbrev-ref HEAD)

            if test (count (git status --short)) -eq 0
               set_color $clean_color
               printf '%s' $branch
            else
               # Dirty state.
               set_color $dirty_color
               printf '%s*' $branch
            end

            set_color $text_color
            printf ']'
         end
end
