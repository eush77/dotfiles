;;; Continuous Mode

(custom-set doc-view-continuous t)

;;; Terminal Display

(when (and (not (display-graphic-p))
           (executable-find doc-view-pdftotext-program))
  (advice-add 'doc-view-initiate-display :override
              #'doc-view-open-text))

;;; Keymap

(define-key doc-view-mode-map (kbd "k")
  #'doc-view-previous-line-or-previous-page)
(define-key doc-view-mode-map (kbd "j")
  #'doc-view-next-line-or-next-page)
