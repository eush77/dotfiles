function goto --description "Print path to GO TO"
    read --prompt="" --command=\"(pwd)\" --shell --local dir
    eval builtin cd $dir
end
