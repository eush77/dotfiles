;;; -*- lexical-binding: t -*-

(with-eval-after-load "magit-mode"
  (custom-set git-commit-fill-column 72)

  (let ((show-and-move
         (lambda (move-next)
           (when (not (magit-section-hidden (magit-current-section)))
             (magit-section-hide (magit-current-section))
             (funcall move-next))
           (magit-section-show (magit-current-section)))))

    (defun my-magit-section-show-next ()
      "If the current section is hidden, show it.
Otherwise hide it, and show the next sibling section."
      (interactive)
      (funcall show-and-move #'magit-section-forward-sibling))

    (defun my-magit-section-show-previous ()
      "If the current section is hidden, show it.
Otherwise hide it, and show the previous sibling section."
      (interactive)
      (funcall show-and-move #'magit-section-backward-sibling)))

  (define-key magit-mode-map (kbd "C-c f") #'magit-find-file)
  (define-key magit-mode-map (kbd "M-N") #'my-magit-section-show-next)
  (define-key magit-mode-map (kbd "M-P") #'my-magit-section-show-previous)

  ; `C-M-i' equals `M-TAB' on TTY.
  (define-key magit-status-mode-map (kbd "C-M-i") #'magit-section-cycle))
