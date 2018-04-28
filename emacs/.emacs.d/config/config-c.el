(defun my-c-hook ()
  "My hook for C mode."
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'inextern-lang 0))
(add-hook 'c-mode-hook #'my-c-hook)

(define-key c-mode-map (kbd "C-c C-a") #'sp-beginning-of-sexp)
(define-key c-mode-map (kbd "C-c C-b") #'sp-backward-sexp)
(define-key c-mode-map (kbd "C-c C-d") #'sp-kill-sexp)
(define-key c-mode-map (kbd "C-c C-e") #'sp-end-of-sexp)
(define-key c-mode-map (kbd "C-c C-f") #'sp-forward-sexp)
(define-key c-mode-map (kbd "C-c C-u C-M-b") #'sp-backward-barf-sexp)
(define-key c-mode-map (kbd "C-c C-u C-M-f") #'sp-forward-barf-sexp)
(define-key c-mode-map [remap indent-region] #'clang-format-region)
