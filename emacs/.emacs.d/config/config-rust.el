;;; Customizations

(custom-set-variables
 '(rust-format-on-save t)
 '(rust-format-show-buffer nil))

;;; Keymap

(define-key rust-mode-map (kbd "C-M-\\") #'rust-format-buffer)
