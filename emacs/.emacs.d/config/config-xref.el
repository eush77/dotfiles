(custom-set xref-show-xrefs-function #'ivy-xref-show-xrefs)

(defun my-xref-show-pos-in-buf--same-window (func &rest args)
  "Display xref location in the same window. See
`display-buffer-same-window'."
  (let ((display-buffer-overriding-action '(display-buffer-same-window)))
    (apply func args)))
(advice-add 'xref--show-pos-in-buf
            :around #'my-xref-show-pos-in-buf--same-window)
