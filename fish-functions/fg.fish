function fg --description 'Send job to foreground'
  if test (jobs |wc -l) -ne 0
    builtin fg $argv
  end
  return 0
end
