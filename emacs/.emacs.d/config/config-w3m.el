(custom-set browse-url-browser-function #'w3m-browse-url)
(custom-set browse-url-generic-program nil)
(custom-set browse-url-new-window-flag t)

(with-eval-after-load "w3m"
  (custom-set w3m-fill-column 78)
  (custom-set w3m-default-display-inline-images t)
  (custom-set w3m-add-referer nil)
  (custom-set w3m-make-new-session t)
  (custom-set w3m-new-session-in-background t)
  (custom-set w3m-home-page "about:")

  (add-hook 'w3m-mode-hook #'w3m-lnum-mode)

  (add-hook 'w3m-mode-hook
            (lambda ()
              (face-remap-add-relative 'default :family "Droid Serif")
              (text-scale-adjust 1)))

  (defadvice w3m-canonicalize-url (before w3m-canonicalize-domain-name activate)
    (when (string-match "^[a-zA-Z0-9.-]+\\.[a-zA-Z0-9.-]+$" (ad-get-arg 0))
      (ad-set-arg 0 (concat "http://" (ad-get-arg 0)))))

  ;; Wipe out the initial-input and the position in the list.
  ;; A hack against `w3m-switch-buffer' populating the initial input.
  (defadvice completing-read (before completing-read-ido-position activate)
    (when (eq major-mode 'w3m-mode)
      (ad-set-arg 4 "")
      (when (listp (ad-get-arg 5))
        (ad-set-arg 5 (car (ad-get-arg 5))))))

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
  (define-key w3m-mode-map (kbd "C-c M-p") #'w3m-tab-move-left))
