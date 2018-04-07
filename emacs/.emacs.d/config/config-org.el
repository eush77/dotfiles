;;; -*- lexical-binding: t -*-

(with-eval-after-load "org"
  (require 'org-depend)
  (custom-set org-todo-keywords
              '((sequence "TODO(!)" "NEXT(!)" "|" "DONE" "DROP(@)")
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

  (defun my-org-drop-headings (end)
    "Drop headings from point to END."
    (when (< (point) end)
      (when (not (member (org-get-todo-state) org-done-keywords))
        (org-todo "DROP"))
      (org-next-visible-heading 1)
      (my-org-drop-headings end)))

  (defun my-org-drop-subtree ()
    "Drop subtree at point, circumventing any state blocking."
    (interactive)
    (let ((subtree-end (save-excursion (org-end-of-subtree)
                                       (point)))
          (org-blocker-hook nil))
      (save-excursion (my-org-drop-headings subtree-end))
      (outline-hide-subtree)))

  (defun my-org-todo--circumventing-blocking (func arg)
    "Use completion to determine the new state when circumventing
state blocking with a `\\[universal-argument] \\[universal-argument] \\[universal-argument]'.

If the new state is `DROP', drop the whole subtree."
    (if (equal arg '(64))
        (let ((org-blocker-hook nil))
          (funcall func '(4))
          (when (string= (org-get-todo-state) "DROP")
            (my-org-drop-subtree)))
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
