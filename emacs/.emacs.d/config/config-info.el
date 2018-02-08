(with-eval-after-load "info"
  (define-key Info-mode-map "j" #'scroll-up-line)
  (define-key Info-mode-map "k" #'scroll-down-line)
  (define-key Info-mode-map "n" #'Info-next-reference)
  (define-key Info-mode-map "p" #'Info-prev-reference)
  (define-key Info-mode-map "{" #'Info-prev)
  (define-key Info-mode-map "}" #'Info-next))
