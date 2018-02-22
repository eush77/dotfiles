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
  (define-key magit-mode-map (kbd "M-P") #'my-magit-section-show-previous))

(with-eval-after-load "magit-status"
  ;; `C-M-i' equals `M-TAB' on TTY.
  (define-key magit-status-mode-map (kbd "C-M-i") #'magit-section-cycle))

;;; Add support for `drop' action in interactive rebase.
(with-eval-after-load "git-rebase"
  (let ((update-action-regexp
         (lambda (re)
           (replace-regexp-in-string "\\\\|pick" "\\\\|pick\\\\|drop" re))))

    (defun git-rebase-mode-font-lock-keywords--drop (keywords)
      "Add `drop' action."
      (cons (cons (funcall update-action-regexp (caar keywords))
                  (cdar keywords))
            (cdr keywords)))
    (advice-add 'git-rebase-mode-font-lock-keywords :filter-return
                #'git-rebase-mode-font-lock-keywords--drop)

    (defun my-git-rebase-hook ()
      "My hook for Git Rebase mode."
      (setq git-rebase-line (funcall update-action-regexp git-rebase-line)))
    (add-hook 'git-rebase-mode-hook #'my-git-rebase-hook))

  (defun my-git-rebase-drop ()
    "Drop commit on current line."
    (interactive)
    (git-rebase-set-action "drop"))

  (define-key git-rebase-mode-map (kbd "k") #'my-git-rebase-drop)
  (define-key git-rebase-mode-map (kbd "C-k") #'my-git-rebase-drop))
