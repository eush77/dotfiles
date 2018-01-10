(dolist (hook '(emacs-lisp-mode-hook
                c-mode-hook
                c++-mode-hook))
  (add-hook hook (lambda () (hs-minor-mode t))))
