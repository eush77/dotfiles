(with-eval-after-load "markdown-mode"
  (define-key markdown-mode-map (kbd "M-<left>") #'markdown-promote)
  (define-key markdown-mode-map (kbd "M-<right>") #'markdown-demote)
  (define-key markdown-mode-map (kbd "M-<up>") #'markdown-move-up)
  (define-key markdown-mode-map (kbd "M-<down>") #'markdown-move-down)
  (add-hook 'markdown-mode-hook #'auto-fill-mode))
