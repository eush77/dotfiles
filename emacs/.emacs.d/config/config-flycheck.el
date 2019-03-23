(custom-set-variables
 '(flycheck-disabled-checkers '(emacs-lisp-checkdoc))
 '(flycheck-global-modes '(not org-mode)))

(global-flycheck-mode 1)

;;;###autoload (autoload 'my-flycheck-hydra/body "config-flycheck")
(eval `(defhydra my-flycheck-hydra ()
         "Flycheck"
         ("p" flycheck-previous-error "previous error")
         ("n" flycheck-next-error "next error")
         (,(if window-system "<return>" "RET") nil)))

(define-key flycheck-mode-map [remap flycheck-previous-error]
  #'my-flycheck-hydra/flycheck-previous-error)
(define-key flycheck-mode-map [remap flycheck-next-error]
  #'my-flycheck-hydra/flycheck-next-error)
