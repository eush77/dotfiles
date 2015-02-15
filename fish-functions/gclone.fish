function gclone --description "git-clone + cd"
  set -l stdout (mktemp -u)
  mkfifo $stdout

  git clone $argv --progress 2>|tee $stdout &
  set -l dir (cat $stdout |head -1 |grep -o "'.*'" |tr -d "'")
  rm $stdout

  cd $dir
end
