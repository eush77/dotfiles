;;; -*- lexical-binding: t -*-

;;; magit

(with-eval-after-load "magit-mode"
  (put 'magit-clean 'disabled nil)
  (custom-set git-commit-fill-column 72)

  (let ((show-and-move
         (lambda (move-next)
           (when (not (oref (magit-current-section) hidden))
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

;;; magit-diff

(with-eval-after-load "magit-diff"
  ;; `C-M-i' equals `M-TAB' on TTY.
  (define-key magit-revision-mode-map (kbd "C-M-i") #'magit-section-cycle))

;;; magit-dispatch

(defun my-magit-dispatch--disable-file-limit (func &rest args)
  "Disable file limiting if command is run via `magit-dispatch'."
  (if (eq last-command 'magit-dispatch)
      (cl-letf (((symbol-function 'magit-file-relative-name) #'ignore))
        (apply func args))
    (apply func args)))

(with-eval-after-load "magit-diff"
  (advice-add 'magit-diff--initial-value
              :around #'my-magit-dispatch--disable-file-limit))

(with-eval-after-load "magit-log"
  (advice-add 'magit-log--initial-value
              :around #'my-magit-dispatch--disable-file-limit))

;;; magit-files

(with-eval-after-load "magit-files"
  (define-key magit-file-mode-map "\C-cf" 'magit-file-dispatch)
  (define-key magit-file-mode-map "\C-cg" 'magit-dispatch))

;;; magit-log

;;;###autoload
(defcustom my-magit-log-exclude-refs-regexp nil
  "Regexp to filter out refs from `magit-log-all',
`magit-log-branches', and `magit-log-all-branches'."
  :type 'regexp
  :group 'my)

(defun my-magit-log--exclude-refs (func &optional args files)
  "Exclude refs matching `my-magit-log-exclude-refs-regexp'."
  (let ((excluded-refs
         (when my-magit-log-exclude-refs-regexp
           (seq-map (lambda (ref) (concat "^" ref))
                    (seq-filter (lambda (ref)
                                  (string-match-p
                                   my-magit-log-exclude-refs-regexp
                                   ref))
                                (magit-list-refnames))))))
    (funcall func (append excluded-refs args) files)))

(with-eval-after-load "magit-log"
  (advice-add 'magit-log-all :around #'my-magit-log--exclude-refs)
  (advice-add 'magit-log-branches :around #'my-magit-log--exclude-refs)
  (advice-add 'magit-log-all-branches :around #'my-magit-log--exclude-refs))

;;; magit-status

(with-eval-after-load "magit-status"
  ;; `C-M-i' equals `M-TAB' on TTY.
  (define-key magit-status-mode-map (kbd "G") #'magit-list-repositories)
  (define-key magit-status-mode-map (kbd "C-M-i") #'magit-section-cycle))

;;; magit-rebase

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
