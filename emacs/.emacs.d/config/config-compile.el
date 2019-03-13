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

;;;###autoload
(defcustom my-compilation-ignored-buffers '("*Compile-Log*"
                                            "*Org-Babel Error Output*"
                                            "*Org PDF LaTeX Output*")
  "Names of buffers ignored by `my-compilation-buffers'."
  :type '(repeat string)
  :group 'my)

(defun my-compilation-buffers ()
  "Get all compilation buffers except `my-compilation-ignored-buffers'."
  (--filter (and (eq (buffer-local-value 'major-mode it) 'compilation-mode)
                 (not (member (buffer-name it)
                              my-compilation-ignored-buffers)))
            (buffer-list)))

(defun my-compilation-other-buffer ()
  "Switch to other `compilation-mode' buffer."
  (interactive)
  (let* ((buffers
          ;; Reverse compilation buffers to cycle through all of them - after
          ;; each buffer switch the new buffer moves to the front of the list
          (reverse (my-compilation-buffers)))
         (other-buffer (or (cadr (memq (current-buffer) buffers))
                           (car buffers))))
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
