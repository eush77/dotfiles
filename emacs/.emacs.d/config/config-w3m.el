;;; browse-web

(defalias 'browse-web #'w3m)

;;; browse-url

(custom-set browse-url-browser-function #'w3m-browse-url)
(custom-set browse-url-generic-program nil)
(custom-set browse-url-new-window-flag t)

;;; completing-read

(defun my-completing-read--w3m-switch-buffer (args)
  "Wipe out initial input and position populated by `w3m-switch-buffer'."
  (if (and (eq major-mode 'w3m-mode)
           (stringp (nth 4 args))
           (string-match "\*w3m\*" (nth 4 args)))
      (butlast args 3)
    args))

(defun my-completing-read--w3m-session-save (func &rest args)
  "Wipe out initial input inserted via `minibuffer-setup-hook'."
  (if (eq major-mode 'w3m-mode)
      (let ((minibuffer-setup-hook nil))
        (apply func args))
    (apply func args)))

(with-eval-after-load "w3m"
  (advice-add 'completing-read :filter-args
              #'my-completing-read--w3m-switch-buffer)
  (advice-add 'completing-read :around
              #'my-completing-read--w3m-session-save))

;;; font

(defun my-w3m-setup-font ()
  "Set up the font for `w3m-mode'."
  (face-remap-add-relative 'default :family "Droid Serif")
  (text-scale-adjust 1))

(with-eval-after-load "w3m"
  (when window-system
    (add-hook 'w3m-mode-hook #'my-w3m-setup-font)))

;;; pocket-lib

(defun my-w3m-pocket-add-url (&optional this-url)
  "Add current URL to Pocket. If THIS-URL is not nil, add URL at
point instead."
  (interactive "P")
  (require 'pocket-lib)
  (let ((url (if this-url (w3m-anchor) w3m-current-url)))
    (when (pocket-lib-add-urls url)
      (message "Added: %s" url))))

(defun my-w3m-lnum-pocket-link-action (info)
  "`w3m-lnum-universal' action for adding a link to Pocket."
  (let ((url (car info)))
    (when (pocket-lib-add-urls url)
      (message "Added: %s" url))))

(with-eval-after-load "w3m-lnum"
  (add-to-list 'w3m-lnum-actions-link-alist
               `(?P ,#'my-w3m-lnum-pocket-link-action "Add to Pocket")
               t))

;;; w3m-bookmark-file

(defun w3m-bookmark-file-modtime--chase-links (func &rest args)
  "Follow symbolic links in `w3m-bookmark-file'."
  (let ((w3m-bookmark-file (file-chase-links w3m-bookmark-file)))
    (apply func args)))

(with-eval-after-load "w3m"
  ;; Follow symbolic links in `w3m-bookmark-file' when checking the file's
  ;; modification time.
  (advice-add 'w3m-bookmark-file-modtime
              :around #'w3m-bookmark-file-modtime--chase-links))

;;; w3m-canonicalize-url

(defun my-w3m-canonicalize-url--domain-name (args)
  "Auto-prepend \"http://\" to domain-looking names."
  (if (string-match-p "^[a-zA-Z0-9.-]+\\.[a-zA-Z0-9.-]+$" (car args))
      (cons (concat "http://" (car args)) (cdr args))
    args))

(with-eval-after-load "w3m"
  (advice-add 'w3m-canonicalize-url
              :filter-args #'my-w3m-canonicalize-url--domain-name))

;;; w3m-mode-map

(with-eval-after-load "w3m"
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
  (define-key w3m-mode-map (kbd "P") #'my-w3m-pocket-add-url)
  (define-key w3m-mode-map (kbd "C-c M-p") #'w3m-tab-move-left)
  (define-key w3m-mode-map (kbd "w") #'w3m-lnum-universal))

;;; w3m

(with-eval-after-load "w3m"
  (custom-set w3m-add-referer nil)
  (custom-set w3m-default-display-inline-images t)
  (custom-set w3m-fill-column 78)
  (custom-set w3m-home-page "about:")
  (custom-set w3m-make-new-session t)

  (add-hook 'w3m-mode-hook #'w3m-lnum-mode))

;;; w3m-filter

(defun my-w3m-filter--catch-errors (func &rest args)
  (condition-case nil
      (funcall func args)
    (error)))

(with-eval-after-load "w3m-filter"
  (advice-add 'w3m-filter :around #'my-w3m-filter--catch-errors))
