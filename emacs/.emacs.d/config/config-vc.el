(with-eval-after-load "vc"
  (custom-set vc-follow-symlinks t))

(with-eval-after-load "vc-annotate"
  (defun my-vc-annotate-magit-show-commit ()
    "Visit commit at line in Magit."
    (interactive)
    (magit-show-commit (car (split-string (thing-at-point 'line) " "))))

  (define-key vc-annotate-mode-map (kbd "m") #'my-vc-annotate-magit-show-commit))
