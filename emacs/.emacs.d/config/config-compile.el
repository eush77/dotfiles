(custom-set compilation-auto-jump-to-first-error t)
(custom-set compilation-scroll-output t)

;;;###autoload
(defun my-recompile ()
  "Save current buffer, recompile, then switch to the compilation
buffer."
  (interactive)
  (require 'compile)
  (unless (compilation-buffer-p (current-buffer))
    (save-buffer))
  (recompile)
  (unless (compilation-buffer-p (current-buffer))
    (switch-to-buffer-other-frame compilation-last-buffer)))
