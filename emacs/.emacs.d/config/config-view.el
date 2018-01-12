(with-eval-after-load "view"
  (define-key view-mode-map (kbd "j") #'scroll-up-line)
  (define-key view-mode-map (kbd "k") #'scroll-down-line))
