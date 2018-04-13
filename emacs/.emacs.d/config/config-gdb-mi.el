(custom-set gdb-many-windows t)

(defun my-gdb-setup-windows--dedicate-comint-window ()
  "Make GUD Comint window dedicated so that `display-buffer'
won't use it for source buffers."
  (set-window-dedicated-p (get-buffer-window gud-comint-buffer) t))
(advice-add 'gdb-setup-windows
            :after #'my-gdb-setup-windows--dedicate-comint-window)
