function to -a dir --description="mkdir + cd"
    mkdir -p $dir

    if begin; printf "%s" $dir |grep '^[/.]' >/dev/null; end
        cd $dir
    else
        cd ./$dir
    end
end
