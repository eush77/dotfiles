# Note that the shell will attempt to change directory without requiring cd if the
# name of a directory is provided (starting with '.', '/' or '~', or ending with '/').
#                                                                                 man cd

function cd --argument-names dir --description='Change (and possibly create) working directory'
         if begin test (count $argv) -eq 1
                  and test ! -e $dir; end
            echo -en "Directory \"$dir\" does not exist. "
            set_color cyan
            echo -n 'Create? '
            read -l reply
            if test (echo $reply |sed -r 's|^\s*(\S).*$|\l\1|') = y
               mkdir $dir
               builtin cd $dir
            end
         else
            builtin cd $argv
         end
end
