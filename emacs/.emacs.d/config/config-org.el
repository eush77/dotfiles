;;; -*- lexical-binding: t -*-

(with-eval-after-load "org"
  (require 'org-depend)
  (custom-set org-todo-keywords
              '((sequence "TODO(!)" "NEXT(!)" "|" "DONE" "DROP")
                (sequence "PLAN" "|" "PASS" "FAIL")
                (sequence "|" "GONE")))
  (custom-set org-todo-keyword-faces
              '(("PLAN" . "yellow")
                ("PASS" . "green")
                ("FAIL" . "red")))
  (custom-set org-agenda-todo-list-sublevels nil)
  (custom-set org-enforce-todo-dependencies t)
  (custom-set org-enforce-todo-checkbox-dependencies t)
  (custom-set org-log-done 'time)
  (custom-set org-log-into-drawer t)

  (add-hook 'org-mode-hook #'auto-fill-mode)

  (defun my-org-todo--circumventing-blocking (func arg)
    "Use completion to determine the new state when circumventing
state blocking with a `\\[universal-argument] \\[universal-argument] \\[universal-argument]'."
    (if (equal arg '(64))
        (let ((org-blocker-hook nil))
          (funcall func '(4)))
      (funcall func arg)))
  (advice-add 'org-todo
              :around #'my-org-todo--circumventing-blocking)

  (let ((show-and-move
         (lambda (move-next)
           (outline-back-to-heading)
           (outline-end-of-heading)
           (when (not (outline-invisible-p))
             (outline-hide-subtree)
             (funcall move-next 1))
           (outline-show-entry)
           (outline-show-children)
           (outline-back-to-heading))))

    (defun my-outline-show-next-subtree ()
      "If the subtree under the current heading is hidden, show it.
Otherwise hide it, and show the next sibling subtree."
      (interactive)
      (funcall show-and-move #'outline-forward-same-level))

    (defun my-outline-show-previous-subtree ()
      "If the subtree under the current heading is hidden, show it.
Otherwise hide it, and show the previous sibling subtree."
      (interactive)
      (funcall show-and-move #'outline-backward-same-level)))

  (define-key org-mode-map (kbd "C-c C-r") #'ivy-resume)

  (define-key org-mode-map (kbd "C-c j") #'org-shiftmetaleft)
  (define-key org-mode-map (kbd "C-c k") #'org-shiftmetaright)
  (define-key org-mode-map (kbd "C-c n") #'org-metadown)
  (define-key org-mode-map (kbd "C-c p") #'org-metaup)
  (define-key org-mode-map (kbd "C-c C-\\") #'org-toggle-link-display)
  (define-key org-mode-map (kbd "M-N") #'my-outline-show-next-subtree)
  (define-key org-mode-map (kbd "M-P") #'my-outline-show-previous-subtree))
