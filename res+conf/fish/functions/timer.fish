function timer --description 'Simple stopwatch'
         set -l sec 0
         set -l min 0
         function tick -a min sec
                  printf '\r%u:%02u' $min $sec
         end
         while begin tick $min $sec; and sleep 1; end
               set -l sec (math $sec+1)
               if test $sec -eq 60
                  set -l sec 0
                  set -l min (math $min+1)
               end
         end
end
