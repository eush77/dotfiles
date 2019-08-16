;;; ff-get-other-file

(defun my-tex-ff-other-file (file)
  "`ff-other-file-alist' function for TeX files.

Returns the location of an Org file the FILE is exported from."
  (list (concat (file-name-sans-extension file) ".org")))

(defun my-tex-ff-other-file-setup ()
  "Setup for `ff-get-other-file'."
  (setq-local ff-search-directories '("/"))
  (setq-local ff-other-file-alist
              '(("\\.tex\\'" my-tex-ff-other-file))))
(add-hook 'latex-mode-hook #'my-tex-ff-other-file-setup)
(add-hook 'plain-tex-mode-hook #'my-tex-ff-other-file-setup)

;;; Typography

(define-advice LaTeX-fill-paragraph (:before (&rest _ignored) my-nbsp-fix)
  "Fix non-breaking spaces."
  (when-let ((nbsp (my-nbsp-get-sequence)))
    (save-excursion
      (LaTeX-forward-paragraph)
      (let ((end (point)))
        (LaTeX-backward-paragraph)
        (let ((begin (point)))
          (require 'guess-language)
          (my-nbsp-fix (guess-language-region begin end)
                       nbsp
                       begin
                       end))))))

;;; Keymap

(define-key latex-mode-map "\C-c\C-j" #'counsel-outline)
(define-key plain-tex-mode-map "\C-c\C-j" #'counsel-outline)

(key-chord-define latex-mode-map "xw" #'ff-get-other-file)
(key-chord-define plain-tex-mode-map "xw" #'ff-get-other-file)
