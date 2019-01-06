(require 'dash)

;;; Basic setup

(custom-set compilation-scroll-output t)

(defun my-compilation-save-buffers-predicate ()
  "Limit buffers that are saved before compiling to those derived
from `prog-mode'."
  (derived-mode-p 'prog-mode))
(custom-set compilation-save-buffers-predicate
            #'my-compilation-save-buffers-predicate)

;;; Commands

(defun my-compilation-other-buffer ()
  "Switch to other `compilation-mode' buffer."
  (interactive)
  (let* ((compilation-buffers
          (--filter (and (eq (buffer-local-value 'major-mode it)
                             'compilation-mode)
                         (not (string= (buffer-name it) "*Compile-Log*")))
                    (buffer-list)))
         (other-buffer (or (cadr (memq (current-buffer) compilation-buffers))
                          (car compilation-buffers))))
    (when (eq other-buffer (current-buffer))
      (user-error "No other compilation buffer"))
    (switch-to-buffer other-buffer)))

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

;;; Keymap

(define-key compilation-mode-map (kbd "c") #'compile)
(define-key compilation-mode-map (kbd "h") #'my-compilation-other-buffer)
