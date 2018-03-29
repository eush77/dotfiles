(global-color-identifiers-mode 1)

(with-eval-after-load "color-identifiers-mode"
  (defvar my-color-identifiers-face-remapping-cookies #s(hash-table test eq)
    "Alist associated buffers with face remapping cookies.")

  (defun my-color-identifiers-face-remapping-specs ()
    "Compute face remapping to disable colorful syntax
highlighting in the current buffer."
    (let ((foreground (face-foreground 'default)))
      (mapcar
       (lambda (spec)
         (cons (car spec) (plist-put (cdr spec) :foreground foreground)))
       '((font-lock-builtin-face :slant normal :weight bold)
         (font-lock-comment-delimiter-face :slant italic :weight normal)
         (font-lock-comment-face :slant italic :weight normal)
         (font-lock-constant-face :slant normal :weight normal)
         (font-lock-doc-face :slant italic :weight normal)
         (font-lock-function-name-face :slant normal :weight normal)
         (font-lock-keyword-face :slant normal :weight bold)
         (font-lock-preprocessor-face :slant normal :weight bold)
         (font-lock-string-face :slant normal :weight normal)
         (font-lock-type-face :slant normal :weight normal)
         (font-lock-variable-name-face :slant normal :weight normal)
         (font-lock-warning-face :slant normal :weight normal)))))

  (defun my-color-identifiers-map-faces-hook ()
    "Adjust buffer faces on theme or major mode change."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (assoc major-mode color-identifiers:modes-alist)
          (let ((entry (gethash buffer
                                my-color-identifiers-face-remapping-cookies)))
            (when (or (not entry) (not (eq major-mode (car entry))))
              (when (and entry (not (eq major-mode (car entry))))
                (mapc #'face-remap-remove-relative (cdr entry)))
              (let ((cookies (mapcar
                              (apply-partially #'apply
                                               #'face-remap-add-relative)
                              (my-color-identifiers-face-remapping-specs))))
                (puthash buffer
                         (cons major-mode cookies)
                         my-color-identifiers-face-remapping-cookies))))))))

  (defun my-color-identifiers-unmap-faces-hook ()
    "Remove face adjustments on theme change."
    (maphash (lambda (buffer entry)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (mapc #'face-remap-remove-relative (cdr entry)))))
             my-color-identifiers-face-remapping-cookies)
    (clrhash my-color-identifiers-face-remapping-cookies))

  (add-hook 'after-change-major-mode-hook #'my-color-identifiers-map-faces-hook)

  (defun load-theme--color-identifiers (&rest args)
    "Update face adjustments for `color-identifiers-mode'."
    (my-color-identifiers-unmap-faces-hook)
    (my-color-identifiers-map-faces-hook))
  (advice-add 'load-theme :after #'load-theme--color-identifiers))
