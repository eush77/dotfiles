# Name must match what `fish_prompt` expects.
set -g fish_with_command

function with -a cmd --description "Add a prefix to each subsequent command"
    set fish_with_command $fish_with_command $cmd
end

function without --description "Don't prefix subsequent commands"
    set -l len (count $fish_with_command)
    if test $len -le 1
        set fish_with_command
    else
        set fish_with_command $fish_with_command[1..(math $len - 1)]
    end
end
