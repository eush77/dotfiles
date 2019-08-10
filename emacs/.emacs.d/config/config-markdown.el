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

;;; Typography

(define-advice markdown-fill-paragraph
    (:before (&rest _ignored) my-nbsp-fix)
  "Fix non-breaking spacing."
  (when-let ((nbsp (my-nbsp-get-sequence)))
    (let ((begin (save-excursion
                   (forward-char)
                   (markdown-backward-paragraph)
                   (point)))
          (end (save-excursion
                 (markdown-forward-paragraph)
                 (point))))
      (require 'guess-language)
      (my-nbsp-fix (guess-language-region begin end)
                   nbsp
                   begin
                   end))))
