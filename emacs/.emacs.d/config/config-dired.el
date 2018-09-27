(require 'dired-imenu)

(custom-set dired-dwim-target t)
(custom-set dired-listing-switches
            (concat "-lv --group-directories-first --human-readable"
                    (if window-system " --all" " --almost-all")))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-hide-dotfiles-mode)

(defun my-dired-browse-file ()
  "Browse a file with `browse-url'."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (browse-url file)))

(defun my-dired-mouse-find-file (event)
  "Like `dired-mouse-find-file-other-window', but find file in
the same window."
  (interactive "e")
  (cl-letf (((symbol-function 'dired-other-window) #'dired)
            ((symbol-function 'find-file-other-window) #'find-file))
    (funcall #'dired-mouse-find-file-other-window event)))

(defun my-dired-toggle-dwim-target ()
  "Toggle `dired-dwim-target'."
  (interactive)
  (setq dired-dwim-target (not dired-dwim-target))
  (message "Dired DWIM target %s"
           (if dired-dwim-target "enabled" "disabled")))

(define-key dired-mode-map (kbd "C-M-p") #'window-jump-up)
(define-key dired-mode-map (kbd "C-M-n") #'window-jump-down)

(define-key dired-mode-map (kbd ".") #'dired-hide-dotfiles-mode)
(define-key dired-mode-map (kbd "SPC") #'dired-up-directory)
(define-key dired-mode-map (kbd "C-x C-y") #'my-dired-toggle-dwim-target)
(define-key dired-mode-map (kbd "M-p") #'dired-prev-subdir)
(define-key dired-mode-map (kbd "M-n") #'dired-next-subdir)
(define-key dired-mode-map (kbd "b") #'my-dired-browse-file)
(define-key dired-mode-map (kbd "c") #'dired-kill-subdir)
(define-key dired-mode-map (kbd "r") #'dired-do-query-replace-regexp)
(define-key dired-mode-map (kbd "z") #'dired-hide-subdir)
(define-key dired-mode-map [mouse-2] #'my-dired-mouse-find-file)
