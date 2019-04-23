(require 'dash)

;;; ANSI Control Sequences

(defun my-compilation-filter-ansi-color (&rest _)
  (save-excursion
    (let ((inhibit-read-only t))
      (perform-replace ansi-color-control-seq-regexp ""
                       nil t nil nil nil (point-min) (point-max)))))

(advice-add 'compilation-handle-exit
            :after #'my-compilation-filter-ansi-color)

;;; Commands

(defun my-compilation-save-buffers-predicate ()
  "Limit buffers that are saved before compiling to those derived
from `prog-mode'."
  (derived-mode-p 'prog-mode))

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

(defun my-compile-in-compilation-buffer ()
  "Like `compile', but command defaults to the last command."
  (interactive)
  (let ((command
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (save-excursion (goto-char (point-min))
                           (beginning-of-line 4)
                           (buffer-substring (point) (line-end-position))))))
    (compile (compilation-read-command command))))

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

;;; Custom Setup

(custom-set-variables
 '(compilation-save-buffers-predicate
   #'my-compilation-save-buffers-predicate)
 '(compilation-scroll-output t))

;;; Keymap

(define-key compilation-mode-map (kbd "c") #'my-compile-in-compilation-buffer)
(define-key compilation-mode-map (kbd "h") #'my-compilation-other-buffer)
(define-key compilation-shell-minor-mode-map (kbd "C-M-n") nil)
(define-key compilation-shell-minor-mode-map (kbd "C-M-p") nil)
