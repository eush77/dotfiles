(with-eval-after-load "w3m"
  (custom-set w3m-fill-column 78)
  (custom-set w3m-default-display-inline-images t)
  (custom-set w3m-add-referer nil)
  (custom-set browse-url-generic-program nil)
  (custom-set w3m-make-new-session nil)
  (custom-set w3m-new-session-in-background nil)

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

  (add-hook 'w3m-mode-hook #'w3m-lnum-mode))
