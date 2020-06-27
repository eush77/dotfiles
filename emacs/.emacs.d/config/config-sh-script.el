(require 'counsel)

;;; outline

(defun my-sh-outline-level ()
  (let (buffer-invisibility-spec)
    (cond ((looking-at-p "#") 1)
          ((looking-at-p ":") 2))))

(defun my-sh-outline-title ()
  (and (looking-back outline-regexp)
       (or (match-string 1) (match-string 2))))

(defun my-sh-setup-outline ()
  (setq-local outline-regexp "# \\(.*\\)\n#\n\n\\|: \\(.*\\) :\n\n")
  (setq-local outline-level #'my-sh-outline-level)
  (setf (alist-get 'sh-mode counsel-outline-settings)
        '(:outline-title my-sh-outline-title))
  (outline-minor-mode 1))

(add-hook 'sh-mode-hook #'my-sh-setup-outline)
