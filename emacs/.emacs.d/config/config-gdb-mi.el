(custom-set gdb-many-windows t)

(defun my-gdb-setup-windows--dedicate-comint-window ()
  "Make GUD Comint window dedicated so that `display-buffer'
won't use it for source buffers."
  (set-window-dedicated-p (get-buffer-window gud-comint-buffer) t))
(advice-add 'gdb-setup-windows
            :after #'my-gdb-setup-windows--dedicate-comint-window)

(defun my-gdb-eval-in-comint-buffer ()
  "Switch to Gdb Comint buffer and clear the input line."
  (interactive)
  (switch-to-buffer gud-comint-buffer)
  (comint-delete-input))

(defun my-gdb-print-in-comint-buffer ()
  "Switch to Gdb Comint buffer and populate the input line for
printing an expression."
  (interactive)
  (my-gdb-eval-in-comint-buffer)
  (insert "p "))

(defun my-gdb-select-relative-frame (offset)
  "Select stack frame OFFSET lines down the stack from the
current frame."
  (goto-char (marker-position gdb-stack-position))
  (forward-line offset)
  (gdb-select-frame))

(defun my-gdb-frame-up ()
  "Up one frame in Gdb stack buffer."
  (interactive)
  (my-gdb-select-relative-frame 1))

(defun my-gdb-frame-down ()
  "Down one frame in Gdb stack buffer."
  (interactive)
  (my-gdb-select-relative-frame -1))

(defun my-gdb-switch-to-stack-buffer ()
  "Switch to call stack buffer."
  (interactive)
  (switch-to-buffer (gdb-get-buffer 'gdb-stack-buffer)))

(defun my-gdb-mode-hook ()
  "My hook for GDB-MI mode."
  (gud-def my-gud-reverse-cont
           (gdb-gud-context-call "-exec-continue --reverse")
           nil
           "Continue backwards with display.")
  (gud-def my-gud-reverse-finish
           (gdb-gud-context-call "-exec-finish --reverse" nil t)
           nil
           "Finish executing current function backwards.")
  (gud-def my-gud-reverse-next
           (gdb-gud-context-call "-exec-next --reverse" "%p" t)
           nil
           "Step one line backwards (skip functions).")
  (gud-def my-gud-reverse-step
           (gdb-gud-context-call "-exec-step --reverse" "%p" t)
           nil
           "Step one source line backwards with display."))
(add-hook 'gdb-mode-hook #'my-gdb-mode-hook)

(define-key gdb-frames-mode-map (kbd "c") #'gud-cont)
(define-key gdb-frames-mode-map (kbd "C") #'my-gud-reverse-cont)
(define-key gdb-frames-mode-map (kbd "d") #'my-gdb-frame-down)
(define-key gdb-frames-mode-map (kbd "e") #'my-gdb-eval-in-comint-buffer)
(define-key gdb-frames-mode-map (kbd "f") #'gud-finish)
(define-key gdb-frames-mode-map (kbd "F") #'my-gud-reverse-finish)
(define-key gdb-frames-mode-map (kbd "n") #'gud-next)
(define-key gdb-frames-mode-map (kbd "N") #'my-gud-reverse-next)
(define-key gdb-frames-mode-map (kbd "p") #'my-gdb-print-in-comint-buffer)
(define-key gdb-frames-mode-map (kbd "s") #'gud-step)
(define-key gdb-frames-mode-map (kbd "S") #'my-gud-reverse-step)
(define-key gdb-frames-mode-map (kbd "u") #'my-gdb-frame-up)

(define-key gud-mode-map (kbd "C-c s") #'my-gdb-switch-to-stack-buffer)
