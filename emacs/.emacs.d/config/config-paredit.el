;;; -*- lexical-binding: t -*-

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

(with-eval-after-load "paredit"
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

  ;; Solve conflicts with global `window-jump' keys.
  (dolist (keys '(("C-M-n" . "C-c C-n") ("C-M-p" . "C-c C-p")))
    (let* ((old-key (car keys))
           (new-key (cdr keys))
           (paredit-func (lookup-key paredit-mode-map (kbd old-key))))
      (when paredit-func
        (define-key paredit-mode-map (kbd old-key) nil)
        (define-key paredit-mode-map (kbd new-key) paredit-func))))

  (define-key paredit-mode-map (kbd "C-c C-b") #'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-c C-f") #'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-c C-M-b") #'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-c C-M-f") #'paredit-forward-barf-sexp))
