(with-eval-after-load "org"
  (add-hook 'org-mode-hook #'auto-fill-mode)

  (define-key org-mode-map (kbd "C-c j") #'org-promote-subtree)
  (define-key org-mode-map (kbd "C-c k") #'org-demote-subtree)
  (define-key org-mode-map (kbd "C-c n") #'org-move-subtree-down)
  (define-key org-mode-map (kbd "C-c p") #'org-move-subtree-up)
  (define-key org-mode-map (kbd "C-c C-\\") #'org-toggle-link-display)
  (define-key org-mode-map (kbd "C-c C-r") #'ivy-resume))
