function to -a dir --description="cd that can create things"
  function _to_mkdir_cd -a path
    mkdir -p $path
    cd $path
  end

  # Handle $HOME and relative/absolute paths first.
  # In these cases we can't safely unpack (and don't have to).
  if test (count $argv) -eq 0
    cd $argv
  else if begin; printf "%s" $dir |grep '^[/.]' >/dev/null; end
    _to_mkdir_cd $dir
  else
    set -l head (printf "%s" $dir |sed 's:/.*::')
    set -l tail (printf "%s" $dir |sed 's:^[^/]*/*::')

    # The first component might exist, in which case we cd straight there.
    # This also resolves $head using $CDPATH.
    # As an interesting corner case, -/whatever is resolved via $dirprev/$dirnext.
    cd $head ^/dev/null; or _to_mkdir_cd $head

    if test "$tail" != ""
      _to_mkdir_cd $tail
    end
  end

  functions -e _to_mkdir_cd
end
