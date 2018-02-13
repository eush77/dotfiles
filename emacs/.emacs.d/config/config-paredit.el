;;; -*- lexical-binding: t -*-

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

(with-eval-after-load "paredit"
  (defun my-paredit-forward-duplicate-sexp ()
    "Duplicate current sexp forward."
    (interactive)
    (cond ((looking-back ")") (backward-sexp))
          ((looking-at "(") t)
          (t (backward-up-list)))
    (let ((saved-kill-ring kill-ring))
      (let ((start (point)))
        (forward-sexp 1)
        (kill-ring-save start (point)))
      (newline-and-indent)
      (yank)
      (backward-sexp)
      (setq kill-ring saved-kill-ring)))

  (defun my-paredit-backward-duplicate-sexp ()
    "Duplicate current sexp backward."
    (interactive)
    (cond ((looking-at "(") (forward-sexp))
          ((looking-back ")") t)
          (t (up-list)))
    (let ((saved-kill-ring kill-ring))
      (let ((end (point)))
        (backward-sexp 1)
        (kill-ring-save (point) end))
      (yank)
      (newline-and-indent)
      (backward-sexp)
      (setq kill-ring saved-kill-ring)))

  ;; Solve the conflict with `view-mode' which manifests itself when
  ;; `view-mode' is entered first (because of e.g. the value of
  ;; `view-read-only').
  (let ((paredit-func (lookup-key paredit-mode-map (kbd "DEL"))))
    (when paredit-func
      (define-key paredit-mode-map (kbd "DEL")
        (lambda () (interactive)
          (call-interactively (if view-mode
                                  (lookup-key view-mode-map (kbd "DEL"))
                                  paredit-func))))))

  ;; Redefine keys to solve conflicts with `window-jump', or simply for
  ;; consistency's sake.
  (dolist (keys '(("C-M-n" . "C-c C-n")
                  ("C-M-p" . "C-c C-p")
                  ("C-M-u" . "C-c C-u")))
    (let* ((old-key (car keys))
           (new-key (cdr keys))
           (paredit-func (lookup-key paredit-mode-map (kbd old-key))))
      (when paredit-func
        (define-key paredit-mode-map (kbd old-key) nil)
        (define-key paredit-mode-map (kbd new-key) paredit-func))))

  (define-key paredit-mode-map (kbd "C-c C-b") #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-c C-d") #'paredit-focus-on-defun)
  (define-key paredit-mode-map (kbd "C-c C-f") #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-c C-M-b") #'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-c C-M-f") #'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-c M-n")
    #'my-paredit-forward-duplicate-sexp)
  (define-key paredit-mode-map (kbd "C-c M-p")
    #'my-paredit-backward-duplicate-sexp))
