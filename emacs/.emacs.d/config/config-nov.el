(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; font

;;;###autoload
(defcustom my-nov-default-face-remapping-specs nil
  "Face remapping specs for the default face in Nov buffers."
  :type '(plist))

(defun my-nov-remap-default-face ()
  "Apply `my-nov-default-face-remapping-specs' to the default face."
  (when my-nov-default-face-remapping-specs
    (apply #'face-remap-add-relative 'default
           my-nov-default-face-remapping-specs)))

(with-eval-after-load "nov"
  (custom-set-variables '(nov-variable-pitch nil))
  (when (display-graphic-p)
    (add-hook 'nov-mode-hook #'my-nov-remap-default-face)))

;; nov-mode-map

(with-eval-after-load "nov"
  (define-key nov-mode-map (kbd "j") #'scroll-up-line)
  (define-key nov-mode-map (kbd "k") #'scroll-down-line))

;; nov-text-width

(with-eval-after-load "nov"
  (custom-set-variables '(nov-text-width fill-column)))
