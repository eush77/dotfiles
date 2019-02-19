;;; Auto-fill

(add-hook 'markdown-mode-hook #'auto-fill-mode)

;;; Keymap

(define-key markdown-mode-map (kbd "M-<left>") #'markdown-promote)
(define-key markdown-mode-map (kbd "M-<right>") #'markdown-demote)
(define-key markdown-mode-map (kbd "M-<up>") #'markdown-move-up)
(define-key markdown-mode-map (kbd "M-<down>") #'markdown-move-down)

;;; Smartparens

(with-eval-after-load "smartparens"
  (sp-local-pair 'markdown-mode "`" "`"))
