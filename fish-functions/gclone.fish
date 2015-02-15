function gclone -a repo --description "git-clone + cd"
  if not printf "%s" $repo |grep :// >/dev/null
    set repo (printf "https://github.com/%s" $repo)
  end
  set -l dir (printf "%s" $repo |sed -r "s:.*/::;s:\.git\$::")

  git clone $repo; and cd $dir
end
