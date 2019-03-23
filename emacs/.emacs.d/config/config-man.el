;;; Notify method

(custom-set-variables '(Man-notify-method 'aggressive))

(defun my-man--pushy-from-man (func &rest args)
  "Use `pushy' as `Man-notify-method' if called from a Man
buffer."
  (if (eq major-mode 'Man-mode)
      (let ((Man-notify-method 'pushy))
        (apply func args))
    (apply func args)))
(advice-add 'man :around #'my-man--pushy-from-man)

;;; Switches

(custom-set-variables '(Man-switches "-a"))

;;; Keymap

(define-key Man-mode-map (kbd "j") #'scroll-up-line)
(define-key Man-mode-map (kbd "k") #'scroll-down-line)
(define-key Man-mode-map (kbd "l") #'recenter-top-bottom)
(define-key Man-mode-map (kbd "q") #'Man-kill)
