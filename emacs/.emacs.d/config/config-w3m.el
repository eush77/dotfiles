(require 'dash)
(require 'dash-functional)

;;; browse-web

(defalias 'browse-web #'w3m)

;;; browse-url

(custom-set-variables
 '(browse-url-browser-function #'w3m-browse-url)
 '(browse-url-generic-program nil)
 '(browse-url-new-window-flag t))

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

;;;###autoload
(defcustom my-w3m-default-face-remapping-specs nil
  "Face remapping specs for the default face in W3m buffers."
  :type '(plist))

(defun my-w3m-remap-default-face ()
  "Apply `my-w3m-default-face-remapping-specs' to default face."
  (when my-w3m-default-face-remapping-specs
    (apply #'face-remap-add-relative 'default
           my-w3m-default-face-remapping-specs)))

(with-eval-after-load "w3m"
  (when (display-graphic-p)
    (add-hook 'w3m-mode-hook #'my-w3m-remap-default-face)))

;;; org-capture

(defun my-w3m-org-capture-link-action (info)
  "`w3m-lnum-universal' action for capturing url in Org."
  (goto-char (cadr info))
  (org-capture nil "u"))

(with-eval-after-load "w3m-lnum"
  (add-to-list 'w3m-lnum-actions-link-alist
               '(?c my-w3m-org-capture-link-action "Capture")
               t))

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

;;; my-w3m-anchor-text

;;;###autoload
(defun my-w3m-anchor-text ()
  "Get anchor anchor at point."
  (when (w3m-anchor)
    (let ((begin (previous-single-property-change (+ (point) 1) 'face))
          (end (next-single-property-change (point) 'face)))
      (buffer-substring-no-properties begin end ))))

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

(define-advice w3m-canonicalize-url (:filter-args (args) my-domain-name)
  "Auto-prepend \"http://\" to domain-looking names."
  (pcase-let ((`(,url ,feeling-searchy) args))
    (if (string-match-p "^[a-zA-Z0-9.-]+\\.[a-zA-Z0-9.-]+\\(?:/.*\\)?$" url)
        (cons (concat "http://" url) feeling-searchy)
      args)))

(define-advice w3m-canonicalize-url (:filter-args (args) my-feeling-searchy)
  "Return search url for uncanonicalized query to `w3m-goto-url'."
  (if (eq (nth 1 (backtrace-frame 7)) 'w3m-goto-url)
      (list (car args) t)
    args))

(define-advice w3m-string-match-url-components (:filter-args (args) my-spaces)
  "Don't match strings with spaces as URLs."
  (if (string-match-p " " (car args))
      '("")
    args))

;;; w3m-mode-map

(with-eval-after-load "w3m"
  (define-key w3m-mode-map (kbd "b") #'w3m-view-previous-page)
  (define-key w3m-mode-map (kbd "B") #'w3m-view-next-page)
  (define-key w3m-mode-map (kbd "M-c") #'w3m-cookie)
  (define-key w3m-mode-map (kbd "f") #'w3m-lnum-follow)
  (define-key w3m-mode-map (kbd "F") (lambda ()
                                       (interactive)
                                       (w3m-lnum-follow 4)))
  (define-key w3m-mode-map (kbd "h") #'w3m-history)
  (define-key w3m-mode-map (kbd "j") #'scroll-up-line)
  (define-key w3m-mode-map (kbd "J") #'w3m-lnum-goto)
  (define-key w3m-mode-map (kbd "k") #'scroll-down-line)
  (define-key w3m-mode-map (kbd "l") #'recenter)
  (define-key w3m-mode-map (kbd "m") #'w3m-lnum-external-view-this-url)
  (define-key w3m-mode-map (kbd "M") #'w3m-external-view-current-url)
  (define-key w3m-mode-map (kbd "n") #'w3m-next-anchor)
  (define-key w3m-mode-map (kbd "C-c M-n") #'w3m-tab-move-right)
  (define-key w3m-mode-map (kbd "p") #'w3m-previous-anchor)
  (define-key w3m-mode-map (kbd "P") #'my-w3m-pocket-add-url)
  (define-key w3m-mode-map (kbd "C-c M-p") #'w3m-tab-move-left)
  (define-key w3m-mode-map (kbd "r") #'w3m-reload-this-page)
  (define-key w3m-mode-map (kbd "R") #'w3m-redisplay-this-page)
  (define-key w3m-mode-map (kbd "q") #'my-w3m-close-window)
  (define-key w3m-mode-map (kbd "Q") #'w3m-close-window)
  (define-key w3m-mode-map (kbd "w") #'w3m-lnum-universal))

;;; w3m

(with-eval-after-load "w3m"
  (custom-set-variables
   '(w3m-add-referer nil)
   '(w3m-default-display-inline-images t)
   '(w3m-fill-column 78)
   '(w3m-home-page "about:")
   '(w3m-make-new-session t)
   '(w3m-pop-up-windows nil))

  (add-hook 'w3m-mode-hook #'w3m-lnum-mode))

;;; w3m-alive-p

(defun my-w3m-alive-p--latter-buffer (func &rest args)
  "Prefer latter buffers in the order of buffer names."
  (cl-letf* ((buffers (reverse (w3m-list-buffers)))
             ((symbol-function 'w3m-list-buffers) (-const buffers)))
    (apply func args)))

(advice-add 'w3m-alive-p :around #'my-w3m-alive-p--latter-buffer)

;;; w3m-close-window

(defun my-w3m-close-window ()
  "Version of `w3m-close-window' that doesn't change window
configuration and doesn't close buffers in other windows."
  (interactive)
  (w3m-history-store-position)
  (if (--all? (eq (buffer-local-value 'major-mode it)
                  'w3m-mode)
              (buffer-list))
      (message "All buffers are W3m buffers!")
    (while (eq (buffer-local-value 'major-mode (window-buffer)) 'w3m-mode)
      (bury-buffer (window-buffer))
      (set-window-buffer (selected-window) (other-buffer)))))

;;; w3m-download

(defun my-w3m-download--default-directory (func &rest args)
  "Save to `default-directory' if called outside of `w3m-mode'."
  (if (derived-mode-p 'w3m-mode)
      (apply func args)
    (let ((w3m-default-save-directory default-directory))
      (apply func args))))

(advice-add 'w3m-download :around #'my-w3m-download--default-directory)

;;; w3m-filter

(defun my-w3m-filter--catch-errors (func &rest args)
  (with-demoted-errors "Error in w3m-filter: %S"
    (apply func args)))

(with-eval-after-load "w3m-filter"
  (advice-add 'w3m-filter :around #'my-w3m-filter--catch-errors))

;;; w3m-select-buffer

(defun my-w3m-select-buffer-delete-buffer--show-this-line (&rest _)
  "Show the buffer on the current menu line."
  (w3m-select-buffer-show-this-line))

(defun my-w3m-select-buffer-quit ()
  "Quit the buffers selection without changing the shown buffer."
  (interactive)
  (and (get-buffer-window w3m-select-buffer-name)
       (delete-windows-on w3m-select-buffer-name)))

(defun my-w3m-select-buffer-print-this-url ()
  "Display the url of the current menu line in the echo area and
put it into ‘kill-ring’."
  (interactive)
  (with-current-buffer (w3m-select-buffer-current-buffer)
    (w3m-print-current-url)))

(with-eval-after-load "w3m"
  (advice-add 'w3m-select-buffer-delete-buffer
              :after #'my-w3m-select-buffer-delete-buffer--show-this-line)
  (advice-add 'w3m-select-buffer-quit
              :override #'my-w3m-select-buffer-quit)

  (define-key w3m-select-buffer-mode-map (kbd "C-n") #'next-line)
  (define-key w3m-select-buffer-mode-map (kbd "C-p") #'previous-line)
  (define-key w3m-select-buffer-mode-map (kbd "u")
    #'my-w3m-select-buffer-print-this-url))

;;; w3m-uri-replace

(with-eval-after-load "w3m"
  (add-to-list 'w3m-uri-replace-alist
               '("\\`\\(https://github\\.com/[^/]+/[^/]+\\)/blob/"
                 w3m-pattern-uri-replace
                 "\\1/raw/")))
