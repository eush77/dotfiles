(custom-set swap-buffers-keep-focus t)

(unless window-system
  (require 'switch-window-asciiart)

  (custom-set swap-buffers-qwerty-shortcuts
              (progn (require 'switch-window-asciiart)
                     (mapcar (lambda (label)
                               (let ((pos (1+ (string-match "(.)" label))))
                                 (substring label pos (1+ pos))))
                             (butlast switch-window-asciiart))))

  (defun my-swap-buffers-label--asciiart (num)
    "My replacement for `swap-buffers-label' using
`switch-window-asciiart'."
    (nth (- num 1) switch-window-asciiart))
  (advice-add 'swap-buffers-label
              :override #'my-swap-buffers-label--asciiart))
