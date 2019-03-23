(with-eval-after-load "vc"
  (custom-set-variables '(vc-follow-symlinks t)))

(with-eval-after-load "vc-annotate"
  (defun my-vc-annotate--same-window (func &rest args)
    "Display output in the same window."
    (let ((display-buffer-overriding-action '(display-buffer-same-window)))
      (apply func args)))
  (advice-add 'vc-annotate :around #'my-vc-annotate--same-window)

  (defun my-vc-annotate-magit-show-commit ()
    "Visit commit at line in Magit."
    (interactive)
    (magit-show-commit (car (split-string (thing-at-point 'line) " "))))

  (define-key vc-annotate-mode-map (kbd "m") #'my-vc-annotate-magit-show-commit))
