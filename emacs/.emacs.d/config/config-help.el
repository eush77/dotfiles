(custom-set help-window-select t)

(with-eval-after-load "help"
  (define-key help-mode-map "n" #'forward-button)
  (define-key help-mode-map "p" #'backward-button))
