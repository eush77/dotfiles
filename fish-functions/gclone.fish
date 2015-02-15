function gclone --description "git-clone + cd"
  function _gclone_repo_address -a repo
    if not printf "%s" $repo |grep :// >/dev/null
      printf "https://github.com/%s" $repo
    else
      printf "%s" $repo
    end
  end

  if test (count $argv) -eq 1
    set argv[1] (_gclone_repo_address $argv[1])
  end
  functions -e _gclone_repo_address

  set -l stdout (mktemp -u)
  mkfifo $stdout

  git clone $argv --progress 2>|tee $stdout &
  set -l dir (cat $stdout |head -1 |grep -o "'.*'" |tr -d "'")
  rm $stdout

  cd $dir
end
