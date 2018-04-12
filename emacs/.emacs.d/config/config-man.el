(custom-set Man-notify-method 'aggressive)
(custom-set Man-switches "-a")

(define-key Man-mode-map (kbd "j") #'scroll-up-line)
(define-key Man-mode-map (kbd "k") #'scroll-down-line)
(define-key Man-mode-map (kbd "l") #'recenter-top-bottom)
(define-key Man-mode-map (kbd "q") #'Man-kill)
