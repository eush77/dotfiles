;;; async-shell-command

(custom-set-variables '(async-shell-command-buffer 'new-buffer))

(defun my-async-shell-command--no-window (func &rest args)
  "Don't display output buffer."
  (let ((display-buffer-overriding-action '(display-buffer-no-window)))
    (apply func args)))

(advice-add 'async-shell-command
            :around #'my-async-shell-command--no-window)
