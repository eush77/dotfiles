(custom-set Man-notify-method 'aggressive)
(custom-set Man-switches "-a")

;;; Commands

(defun my-man-pushy ()
  "Like `man', but with `Man-notify-method' set to `pushy'."
  (interactive)
  (let ((Man-notify-method 'pushy))
    (call-interactively #'man)))

;;; Keymap

(define-key Man-mode-map (kbd "j") #'scroll-up-line)
(define-key Man-mode-map (kbd "k") #'scroll-down-line)
(define-key Man-mode-map (kbd "l") #'recenter-top-bottom)
(define-key Man-mode-map (kbd "m") #'my-man-pushy)
(define-key Man-mode-map (kbd "q") #'Man-kill)
