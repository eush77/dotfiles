;;; Auto-fill

(add-hook 'markdown-mode-hook #'auto-fill-mode)

;;; Keymap

(define-key markdown-mode-map "\C-c\C-j" #'counsel-outline)
(define-key markdown-mode-map (kbd "M-<left>") #'markdown-promote)
(define-key markdown-mode-map (kbd "M-<right>") #'markdown-demote)
(define-key markdown-mode-map (kbd "M-<up>") #'markdown-move-up)
(define-key markdown-mode-map (kbd "M-<down>") #'markdown-move-down)
(define-key markdown-mode-map (kbd "M-N") #'my-outline-show-next-subtree)
(define-key markdown-mode-map (kbd "M-P") #'my-outline-show-previous-subtree)

;;; Smartparens

(with-eval-after-load "smartparens"
  (sp-local-pair 'markdown-mode "`" "`"))
