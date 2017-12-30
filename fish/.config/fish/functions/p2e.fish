function p2e --description "Pipe-to-Emacs utility"
     set -l tmp (tempfile --prefix=emacs)
     cat >$tmp
     emacsclient --no-wait $tmp
     emacsclient --eval --suppress-output "(with-current-buffer (get-file-buffer \"$tmp\") (set-visited-file-name nil) (rename-buffer \"*piped*\" t) (set-buffer-modified-p t))"
     rm $tmp
end
