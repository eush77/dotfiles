(require 'dired-x)

(with-eval-after-load "dired"
  (custom-set dired-dwim-target t)
  (add-hook 'dired-after-readin-hook #'dired-hide-details-mode)

  (define-key dired-mode-map (kbd "C-M-p") #'window-jump-up)
  (define-key dired-mode-map (kbd "C-M-n") #'window-jump-down)

  (define-key dired-mode-map (kbd "SPC") #'dired-up-directory)
  (define-key dired-mode-map (kbd "C-S-p") #'dired-prev-subdir)
  (define-key dired-mode-map (kbd "C-S-n") #'dired-next-subdir)
  (define-key dired-mode-map (kbd "c") #'dired-kill-subdir)
  (define-key dired-mode-map (kbd "z") #'dired-hide-subdir))
