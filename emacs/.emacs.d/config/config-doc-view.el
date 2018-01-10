(with-eval-after-load "doc-view"
  (custom-set doc-view-continuous t)

  (define-key doc-view-mode-map (kbd "k")
    #'doc-view-previous-line-or-previous-page)
  (define-key doc-view-mode-map (kbd "j")
    #'doc-view-next-line-or-next-page))
