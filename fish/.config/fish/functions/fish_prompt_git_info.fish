set -g fish_prompt_git_info_clean green
set -g fish_prompt_git_info_equivalent_clean green
set -g fish_prompt_git_info_dirty yellow
set -g fish_prompt_git_info_equivalent_dirty red

function fish_prompt_git_info -a text_color --description='Include Git info in the current prompt'
  if test $TMUX
    set clean_color $fish_prompt_git_info_equivalent_clean
    set dirty_color $fish_prompt_git_info_equivalent_dirty
  else
    set clean_color $fish_prompt_git_info_clean
    set dirty_color $fish_prompt_git_info_dirty
  end

  set git_status (git status --short ^/dev/null)

  if test $status -eq 0
    set branch (git rev-parse --abbrev-ref HEAD ^/dev/null)

    if test $status -ne 0
      # Not initialized.
      set branch '??'
      set branch_color $dirty_color
    else if test (count $git_status) -ne 0
      if printf '%s\n' $git_status |grep '^??' >/dev/null
        # Untracked files.
        set branch $branch"**"
      else if begin; printf '%s\n' $git_status |grep '^.[^ ]' >/dev/null; end
      # Changes not staged for commit.
      set branch $branch"*"
      else
        # Changes to be committed.
        set branch $branch"="
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
