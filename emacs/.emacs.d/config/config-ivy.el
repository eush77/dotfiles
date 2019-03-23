(ivy-mode 1)

(custom-set-variables
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t))

;; `S-SPC' is not available in TTY.
(define-key ivy-minibuffer-map [remap set-mark-command]
  #'ivy-restrict-to-matches)
