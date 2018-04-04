(with-eval-after-load "hippie-exp"
  (custom-set hippie-expand-try-functions-list
              '(try-expand-all-abbrevs
                try-expand-list
                try-expand-list-all-buffers
                try-expand-line
                try-expand-line-all-buffers
                try-expand-dabbrev-visible
                try-expand-dabbrev
                try-expand-dabbrev-all-buffers
                try-complete-file-name-partially
                try-complete-file-name
                try-complete-lisp-symbol-partially
                try-expand-dabbrev-from-kill
                try-expand-whole-kill)))
