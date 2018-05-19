(define-key multitran-mode-map (kbd "l") #'multitran-history-prev)
(define-key multitran-mode-map (kbd "r") #'multitran-history-next)

(defun my-multitran-region (select-langs)
  "Like `multitran', but ask for the word only if the region
isn't active, otherwise use the region."
  (interactive "P")
  (if (use-region-p)
      (let* ((word (buffer-substring (mark) (point)))
             (langs (if select-langs
                        (multitran--read-languages word)
                      multitran-languages)))
        (multitran word langs))
    (call-interactively #'multitran)))
