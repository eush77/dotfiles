(autoload 'pdf-view-mode "pdf-tools" nil t)
(add-to-list 'auto-mode-alist '("\\.pdf$" . pdf-view-mode))

(with-eval-after-load "pdf-tools"
  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

  (define-key pdf-view-mode-map (kbd "j") #'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") #'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "h") #'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "l") #'image-forward-hscroll))
