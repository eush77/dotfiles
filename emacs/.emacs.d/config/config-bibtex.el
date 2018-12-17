;;; Reformat setup

(custom-set bibtex-entry-format t)
(custom-set bibtex-align-at-equal-sign t)
(custom-set bibtex-comma-after-last-field t)

;;; Keymap

(define-key bibtex-mode-map [remap indent-region] #'bibtex-reformat)
