(custom-set compilation-auto-jump-to-first-error t)
(custom-set compilation-scroll-output t)

(defun my-compilation-save-buffers-predicate ()
  "Limit buffers that are saved before compiling to those derived
from `prog-mode'."
  (derived-mode-p 'prog-mode))
(custom-set compilation-save-buffers-predicate
            #'my-compilation-save-buffers-predicate)

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
