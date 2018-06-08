(when (executable-find "direnv")
  (direnv-mode 1))

(custom-set direnv-non-file-modes '(compilation-mode
                                    dired-mode
                                    eshell-mode
                                    magit-status-mode))

(with-eval-after-load "eshell"
  (defun my-eshell--direnv-path-env (&rest args)
    "Update `eshell-path-env'."
    (unless (file-remote-p default-directory)
      (setq eshell-path-env (getenv "PATH"))))
  (advice-add 'eshell
              :after #'my-eshell--direnv-path-env)

  (defun my-direnv-update-directory-environment--eshell (&rest args)
    "Update `eshell-path-env'."
    (setq eshell-path-env (getenv "PATH")))
  (advice-add 'direnv-update-directory-environment
              :after #'my-direnv-update-directory-environment--eshell))
