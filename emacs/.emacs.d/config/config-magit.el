(with-eval-after-load "magit-mode"
  (custom-set git-commit-fill-column 72)

  (define-key magit-mode-map (kbd "C-c f") #'magit-find-file))
