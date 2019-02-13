(with-eval-after-load "erc-track"
  (add-to-list 'erc-track-exclude-types "JOIN")
  (add-to-list 'erc-track-exclude-types "PART")
  (add-to-list 'erc-track-exclude-types "QUIT")

  (define-key erc-track-minor-mode-map (kbd "C-c C-@") nil)
  (define-key erc-track-minor-mode-map (kbd "C-c C-SPC") nil)
  (define-key erc-track-minor-mode-map (kbd "C-x M-5")
    #'erc-track-switch-buffer))
