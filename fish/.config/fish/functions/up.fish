function up -a depth --description "Go up several directories"
  if test (count $argv) -eq 0
    set depth 1
  end
  for i in (seq $depth)
    cd ..
  end
end
