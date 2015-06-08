function gclone --description "git-clone + cd"
  eval set opt (getopt -o '' -l http -- $argv)

  set -l use_http 0

  while count $opt >/dev/null
    switch $opt[1]
      case --http
        set use_http 1
      case --
        set -e opt[1]
        break
    end
    set -e opt[1]
  end

  set -l repo $opt[1]

  if not printf "%s" $repo |grep -q :
    set -l repo_format
    if test $use_http -eq 1
      set repo_format "https://github.com/%s"
    else
      set repo_format "git@github.com:%s.git"
    end
    set repo (printf $repo_format $repo)
  end
  set -l dir (printf "%s" $repo |sed -r "s|.*[/:]||;s|\.git\$||")

  git clone $repo; and cd $dir
end
