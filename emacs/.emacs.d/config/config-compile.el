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
  (when (my-compilation-save-buffers-predicate)
    (save-buffer))
  (when compilation-last-buffer
    (when-let ((compilation-window
                (get-buffer-window compilation-last-buffer t)))
      (select-frame-set-input-focus (window-frame compilation-window))
      (select-window compilation-window)))
  (recompile)
  (select-window (get-buffer-window compilation-last-buffer t)))

(define-key compilation-mode-map (kbd "c") #'compile)
