;;; ASCII Art Labels

(defun my-swap-buffers-asciiart-label (num)
  "Replaces `swap-buffers-label' using `switch-window-asciiart'."
  (nth (- num 1) switch-window-asciiart))

;; Use ASCII art labels when on a text-only display to make the labels stand
;; out.
(unless (display-graphic-p)
  (add-to-list 'package-selected-packages 'switch-window)
  (package-install-selected-packages)
  (require 'switch-window-asciiart)

  (custom-set-variables
   `(swap-buffers-qwerty-shortcuts
     ,(mapcar (lambda (label)
                (let ((pos (1+ (string-match "(.)" label))))
                  (substring label pos (1+ pos))))
              (butlast switch-window-asciiart))))

  (advice-add 'swap-buffers-label
              :override #'my-swap-buffers-asciiart-label))

;;; swap-buffers-keep-focus

(custom-set-variables '(swap-buffers-keep-focus t))
