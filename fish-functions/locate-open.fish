function locate-open --description "locate(1) a file and open(1) it"
    locate $argv | percol | tee /dev/stderr | xargs xdg-open ^/dev/null
end
