(defalias 'browse-web #'w3m)

(custom-set browse-url-browser-function #'w3m-browse-url)
(custom-set browse-url-generic-program nil)
(custom-set browse-url-new-window-flag t)

(with-eval-after-load "w3m"
  (custom-set w3m-fill-column 78)
  (custom-set w3m-default-display-inline-images t)
  (custom-set w3m-add-referer nil)
  (custom-set w3m-make-new-session t)
  (custom-set w3m-home-page "about:")

  (add-hook 'w3m-mode-hook #'w3m-lnum-mode)

  (when window-system
    (add-hook 'w3m-mode-hook
              (lambda ()
                (face-remap-add-relative 'default :family "Droid Serif")
                (text-scale-adjust 1))))

  (defadvice w3m-canonicalize-url (before w3m-canonicalize-domain-name activate)
    (when (string-match "^[a-zA-Z0-9.-]+\\.[a-zA-Z0-9.-]+$" (ad-get-arg 0))
      (ad-set-arg 0 (concat "http://" (ad-get-arg 0)))))

  (defun completing-read--w3m-switch-buffer (args)
    "Wipe out initial input and position populated by `w3m-switch-buffer'."
    (if (and (eq major-mode 'w3m-mode)
             (stringp (nth 4 args))
             (string-match "\*w3m\*" (nth 4 args)))
        (butlast args 3)
      args))
  (advice-add 'completing-read :filter-args
              #'completing-read--w3m-switch-buffer)

  ;; Follow symbolic links in `w3m-bookmark-file' when checking the file's
  ;; modification time.
  (defun w3m-bookmark-file-modtime--chase-links (func &rest args)
    "Follow symbolic links in `w3m-bookmark-file'."
    (let ((w3m-bookmark-file (file-chase-links w3m-bookmark-file)))
      (apply func args)))
  (advice-add 'w3m-bookmark-file-modtime
              :around #'w3m-bookmark-file-modtime--chase-links)

  (defun my-w3m-pocket-add-current-url ()
    "Add current URL to Pocket."
    (interactive)
    (require 'pocket-lib)
    (when (pocket-lib-add-urls w3m-current-url)
      (message "Added: %s" w3m-current-url)))

  (defun my-w3m-lnum-pocket-link-action (info)
    "`w3m-lnum-universal' action for adding a link to Pocket."
    (let ((url (car info)))
      (when (pocket-lib-add-urls url)
        (message "Added: %s" url))))

  (setq w3m-lnum-actions-link-alist (butlast w3m-lnum-actions-link-alist))

  (setq w3m-lnum-actions-link-alist
        (append w3m-lnum-actions-link-alist
                `((?P ,#'my-w3m-lnum-pocket-link-action "Add to Pocket"))))

  (define-key w3m-mode-map (kbd "b") #'w3m-view-previous-page)
  (define-key w3m-mode-map (kbd "B") #'w3m-view-next-page)
  (define-key w3m-mode-map (kbd "M-c") #'w3m-cookie)
  (define-key w3m-mode-map (kbd "f") #'w3m-lnum-follow)
  (define-key w3m-mode-map (kbd "F") #'w3m-lnum-goto)
  (define-key w3m-mode-map (kbd "h") #'w3m-history)
  (define-key w3m-mode-map (kbd "j") #'scroll-up-line)
  (define-key w3m-mode-map (kbd "k") #'scroll-down-line)
  (define-key w3m-mode-map (kbd "l") #'recenter)
  (define-key w3m-mode-map (kbd "m") #'w3m-lnum-external-view-this-url)
  (define-key w3m-mode-map (kbd "M") #'w3m-external-view-current-url)
  (define-key w3m-mode-map (kbd "n") #'w3m-next-anchor)
  (define-key w3m-mode-map (kbd "C-c M-n") #'w3m-tab-move-right)
  (define-key w3m-mode-map (kbd "p") #'w3m-previous-anchor)
  (define-key w3m-mode-map (kbd "P") #'my-w3m-pocket-add-current-url)
  (define-key w3m-mode-map (kbd "C-c M-p") #'w3m-tab-move-left)
  (define-key w3m-mode-map (kbd "w") #'w3m-lnum-universal))
