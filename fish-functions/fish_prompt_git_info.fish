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

            set git_status (git status --short)
            set branch (git rev-parse --abbrev-ref HEAD ^/dev/null)

            if test $status -ne 0
               set branch '??'
               set branch_color $dirty_color
            else if test (count $git_status) -ne 0
               set branch $branch"*"
               if printf '%s\n' $git_status |grep '^??' >/dev/null
                  # Untracked files.
                  set branch $branch"*"
               end
               set branch_color $dirty_color
            else
               set branch_color $clean_color
            end

            printf '['
            set_color $branch_color
            printf '%s' $branch
            set_color $text_color
            printf ']'
         end
end
