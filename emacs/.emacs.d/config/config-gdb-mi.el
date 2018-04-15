(custom-set gdb-many-windows t)

(defun my-gdb-setup-windows--dedicate-comint-window ()
  "Make GUD Comint window dedicated so that `display-buffer'
won't use it for source buffers."
  (set-window-dedicated-p (get-buffer-window gud-comint-buffer) t))
(advice-add 'gdb-setup-windows
            :after #'my-gdb-setup-windows--dedicate-comint-window)

(defun my-gdb-eval-in-comint-buffer ()
  "Switch to Gdb Comint buffer and populate the input line for
evaluating an expression."
  (interactive)
  (switch-to-buffer gud-comint-buffer)
  (comint-delete-input)
  (insert "p "))

(defun my-gdb-select-relative-frame (offset)
  "Select stack frame OFFSET lines down the stack from the
current frame."
  (goto-char (marker-position gdb-stack-position))
  (forward-line offset)
  (gdb-select-frame))

(defun my-gdb-select-next-frame ()
  "Select next frame in Gdb stack buffer."
  (interactive)
  (my-gdb-select-relative-frame 1))

(defun my-gdb-select-previous-frame ()
  "Select previous frame in Gdb stack buffer."
  (interactive)
  (my-gdb-select-relative-frame -1))

(defun my-gdb-switch-to-stack-buffer ()
  "Switch to call stack buffer."
  (interactive)
  (switch-to-buffer (gdb-get-buffer 'gdb-stack-buffer)))

(define-key gdb-frames-mode-map (kbd "e") #'my-gdb-eval-in-comint-buffer)
(define-key gdb-frames-mode-map (kbd "n") #'my-gdb-select-next-frame)
(define-key gdb-frames-mode-map (kbd "p") #'my-gdb-select-previous-frame)

(define-key gud-mode-map (kbd "C-c s") #'my-gdb-switch-to-stack-buffer)
