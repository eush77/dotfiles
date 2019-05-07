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
(add-hook 'LaTeX-mode-hook #'my-tex-ff-other-file-setup)

;;; Keymap

(define-key latex-mode-map "\C-c\C-j" #'counsel-outline)
(define-key LaTeX-mode-map "\C-c\C-j" #'counsel-outline)

(key-chord-define latex-mode-map "xw" #'ff-get-other-file)
(key-chord-define LaTeX-mode-map "xw" #'ff-get-other-file)
