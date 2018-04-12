(add-to-list 'package-selected-packages 'markdown-mode)

(define-key markdown-mode-map (kbd "M-<left>") #'markdown-promote)
(define-key markdown-mode-map (kbd "M-<right>") #'markdown-demote)
(define-key markdown-mode-map (kbd "M-<up>") #'markdown-move-up)
(define-key markdown-mode-map (kbd "M-<down>") #'markdown-move-down)
(add-hook 'markdown-mode-hook #'auto-fill-mode)

(with-eval-after-load "smartparens"
  (sp-local-pair 'markdown-mode "`" "`"))
