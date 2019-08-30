(pdf-tools-install)

(with-eval-after-load "pdf-tools"
  (add-to-list 'pdf-tools-enabled-modes
               'pdf-view-midnight-minor-mode)

  ;; `pdf-isearch-minor-mode' integrates `pdf-tools' with `isearch'.
  (define-key pdf-view-mode-map (kbd "C-r") #'isearch-backward)
  (define-key pdf-view-mode-map (kbd "C-s") #'isearch-forward)

  (define-key pdf-view-mode-map (kbd "j") #'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") #'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "h") #'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "l") #'image-forward-hscroll))
