(require 'dash)

;;; ANSI Control Sequences

(defun my-compilation-filter-ansi-color (&rest _)
  (save-excursion
    (let ((inhibit-read-only t))
      (perform-replace ansi-color-control-seq-regexp ""
                       nil t nil nil nil (point-min) (point-max)))))

(advice-add 'compilation-handle-exit
            :after #'my-compilation-filter-ansi-color)

;;; Buffer Saving

(defvar org-directory)

(defun my-compilation-save-buffers-predicate ()
  "Non-nil if the current buffer is saved before compiling."
  (and (derived-mode-p 'bibtex-mode 'prog-mode 'text-mode)
       (not (string-equal
             (directory-file-name (file-name-directory buffer-file-name))
             (directory-file-name (expand-file-name org-directory))))))

(custom-set-variables
 '(compilation-save-buffers-predicate
   #'my-compilation-save-buffers-predicate))

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

;;; Display-Buffer

(define-advice compilation-start
    (:around (func &rest args) my-display-buffer)
  "Display output in the same window."
  (let ((display-buffer-overriding-action
         '(display-buffer-same-window)))
    (apply func args)))

;;; History

(defvar-local my-compile-history nil
  "Buffer-local compilation history variable.")

(defvar my-compile-history-counter 0
  "Number of local compilation history variables created.")

(defun my-compile-history--format-docstring ()
  "Format documentation string for local history variable."
  (cl-assert (eq major-mode 'compilation-mode))
  (format "Compilation history for buffer %s." (buffer-name)))

(defun my-compile-reset-history ()
  "Reset compilation history for the current buffer."
  (interactive)
  (cl-assert (eq major-mode 'compilation-mode))
  (setq my-compile-history
        (intern (format "my-compile-history-%d" my-compile-history-counter)))
  (setq my-compile-history-counter (+ my-compile-history-counter 1))
  (eval `(defvar ,my-compile-history nil
           ,(my-compile-history--format-docstring))))

(defun my-compile-delete-history ()
  "Delete compilation history variable for the current buffer."
  (when (eq major-mode 'compilation-mode)
    (makunbound my-compile-history)
    (setf (documentation-property my-compile-history 'variable-documentation)
          nil)))

(defun my-compilation-mode--init-compile-history (func &rest args)
  "Initialize compilation history for the current buffer.

Preserve existing `my-compile-history' or create a new one."
  (let ((history my-compile-history))
    (apply func args)
    (if history
        (setq my-compile-history history)
      (my-compile-reset-history))))

(defun my-compilation-read-command (command)
  "Read compilation command from the minibuffer.

COMMAND is the default command."
  (read-shell-command "Compile command: " command
                      (if (equal (car (symbol-value my-compile-history))
                                 command)
                          (cons my-compile-history 1)
                        my-compile-history)))

(define-advice compile (:around (func command &rest rest) my-add-to-history)
  "Add COMMAND to compilation history."
  (with-current-buffer (apply func command rest)
    (cl-assert (eq major-mode 'compilation-mode))
    (unless (equal (car (symbol-value my-compile-history)) command)
      (push command (symbol-value my-compile-history)))))

(defun my-rename-buffer--compile-history-docstring (&rest _)
  "Update documentation string for local history variable."
  (when (eq major-mode 'compilation-mode)
    (setf (documentation-property my-compile-history 'variable-documentation)
          (my-compile-history--format-docstring))))

(add-hook 'kill-buffer-hook #'my-compile-delete-history)
(advice-add 'compilation-mode
            :around #'my-compilation-mode--init-compile-history)
(advice-add 'compilation-read-command
            :override #'my-compilation-read-command)
(advice-add 'rename-buffer
            :after #'my-rename-buffer--compile-history-docstring)

;;; Keymap

(define-key compilation-mode-map (kbd "c") #'my-compile-in-compilation-buffer)
(define-key compilation-mode-map (kbd "h") #'my-compilation-other-buffer)
(define-key compilation-shell-minor-mode-map (kbd "C-M-n") nil)
(define-key compilation-shell-minor-mode-map (kbd "C-M-p") nil)

;;; Recompile

(defun my-recompile-window ()
  "Get compilation window displayed in an active frame."
  (--find (with-current-buffer (window-buffer it)
            (derived-mode-p 'compilation-mode))
          (my-active-windows)))

;;;###autoload
(defun my-recompile (&optional select-p)
  "Recompile in an active compilation window.

With prefix argument SELECT-P, select the window as well."
  (interactive "P")
  (let ((window (my-recompile-window)))
    (unless window
      (user-error "No active compilation window"))
    (when (my-compilation-save-buffers-predicate)
      (save-buffer))
    (with-selected-frame (window-frame window)
      (with-selected-window window
        (with-current-buffer (window-buffer window)
          (recompile))))
    (when select-p
      (select-window window))))

;;; Scrolling

(custom-set-variables
 '(compilation-scroll-output t))
