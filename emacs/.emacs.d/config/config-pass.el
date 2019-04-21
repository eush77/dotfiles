;;; view-mode

(defun my-pass-setup-buffer--disable-view-mode ()
  "Turn off `view-mode' toggled by `view-read-only' somehow."
  (view-mode 0))

(advice-add 'pass-setup-buffer
            :after #'my-pass-setup-buffer--disable-view-mode)
