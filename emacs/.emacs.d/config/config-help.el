(custom-set help-window-select t)

(define-key help-mode-map "j" #'scroll-up-line)
(define-key help-mode-map "k" #'scroll-down-line)
(define-key help-mode-map "n" #'forward-button)
(define-key help-mode-map "p" #'backward-button)
