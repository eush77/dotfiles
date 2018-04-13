(ivy-mode 1)

(custom-set ivy-use-selectable-prompt t)
(custom-set ivy-use-virtual-buffers t)

;; `S-SPC' is not available in TTY.
(define-key ivy-minibuffer-map [remap set-mark-command]
  #'ivy-restrict-to-matches)
