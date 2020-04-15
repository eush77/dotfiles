(defun my-telega-mode-line-format ()
  "Format mode line for `telega'."
  (let ((mode-line-string
         (concat
          (when telega-use-tracking-for
            (telega-mode-line-tracking))
          (telega-mode-line-unread-unmuted)
          (telega-mode-line-mentions 'messages))))
    (unless (string-empty-p mode-line-string)
      (concat (telega-mode-line-icon) mode-line-string))))

(custom-set-variables
 '(telega-mode-line-string-format
   '((:eval (my-telega-mode-line-format)))))
