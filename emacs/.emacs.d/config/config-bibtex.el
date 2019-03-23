(add-hook 'bibtex-mode-hook #'hs-minor-mode)

;;; Reformat setup

(custom-set-variables
 '(bibtex-align-at-equal-sign t)
 '(bibtex-comma-after-last-field t)
 '(bibtex-entry-format t))

;;; Keymap

(define-key bibtex-mode-map [remap indent-region] #'bibtex-reformat)
