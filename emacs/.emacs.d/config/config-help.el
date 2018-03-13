(custom-set help-window-select t)

(with-eval-after-load "help"
  (define-key help-mode-map "j" #'scroll-up-line)
  (define-key help-mode-map "k" #'scroll-down-line)
  (define-key help-mode-map "n" #'forward-button)
  (define-key help-mode-map "p" #'backward-button))
