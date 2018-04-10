(direnv-mode 1)

(with-eval-after-load "direnv"
  (custom-set direnv-non-file-modes '(eshell-mode
                                      magit-status-mode))

  (with-eval-after-load "eshell"
    (defun my-direnv-update-directory-environment--eshell (&rest args)
      "Update `eshell-path-env'."
      (setq eshell-path-env (getenv "PATH")))
    (advice-add 'direnv-update-directory-environment
                :after #'my-direnv-update-directory-environment--eshell)))
