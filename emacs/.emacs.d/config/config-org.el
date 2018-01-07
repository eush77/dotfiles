(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c C-\\") #'org-toggle-link-display)
  (define-key org-mode-map (kbd "C-c C-r") #'ivy-resume)
  (add-hook 'org-mode-hook #'auto-fill-mode))
