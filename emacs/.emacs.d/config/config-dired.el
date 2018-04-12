(require 'dired-x)

(with-eval-after-load "dired"
  (custom-set dired-listing-switches "-l --almost-all --group-directories-first --human-readable")
  (custom-set dired-dwim-target t)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'dired-hide-dotfiles-mode)

  (defun my-dired-browse-file ()
    "Browse a file with `browse-url'."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (browse-url file)))

  (define-key dired-mode-map (kbd "C-M-p") #'window-jump-up)
  (define-key dired-mode-map (kbd "C-M-n") #'window-jump-down)

  (define-key dired-mode-map (kbd ".") #'dired-hide-dotfiles-mode)
  (define-key dired-mode-map (kbd "SPC") #'dired-up-directory)
  (define-key dired-mode-map (kbd "C-S-p") #'dired-prev-subdir)
  (define-key dired-mode-map (kbd "C-S-n") #'dired-next-subdir)
  (define-key dired-mode-map (kbd "b") #'my-dired-browse-file)
  (define-key dired-mode-map (kbd "c") #'dired-kill-subdir)
  (define-key dired-mode-map (kbd "z") #'dired-hide-subdir))
