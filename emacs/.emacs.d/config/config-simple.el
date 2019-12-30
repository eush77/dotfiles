;;; async-shell-command

(custom-set-variables '(async-shell-command-buffer 'new-buffer))

(defun my-async-shell-command--no-window (func &rest args)
  "Don't display output buffer."
  (let ((display-buffer-overriding-action '(display-buffer-no-window)))
    (apply func args)))

(advice-add 'async-shell-command
            :around #'my-async-shell-command--no-window)

;;;###autoload
(defcustom my-shell-command-kill-buffer-when-terminated t
  "If non-nil, output buffers of terminated shell commands are
killed."
  :type 'boolean
  :group 'my)

(define-advice shell-command-sentinel (:after (process signal) my-kill-buffer)
  "Kill buffer when its process is terminated."
  (when (and my-shell-command-kill-buffer-when-terminated
             (memq (process-status process) '(exit signal)))
    (kill-buffer (process-buffer process))))
