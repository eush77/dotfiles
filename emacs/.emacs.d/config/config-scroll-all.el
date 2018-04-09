(with-eval-after-load "scroll-all"
  (defun my-scroll-all-scroll-down-line-all (arg)
    "Scroll text down in all visible windows."
    (interactive "p")
    (scroll-all-function-all #'scroll-down-line arg))

  (defun my-scroll-all-scroll-up-line-all (arg)
    "Scroll text up in all visible windows."
    (interactive "p")
    (scroll-all-function-all #'scroll-up-line arg))

  (defun my-scroll-all-check-to-scroll ()
    "My additional scrolling-command checks for `scroll-all-mode'."
    (cond ((eq this-command 'scroll-down-line)
           (call-interactively #'my-scroll-all-scroll-down-line-all))
          ((eq this-command 'scroll-up-line)
           (call-interactively #'my-scroll-all-scroll-up-line-all))
          ((eq this-command 'w3m-scroll-down-or-previous-url)
           (call-interactively #'scroll-all-page-up-all))
          ((eq this-command 'w3m-scroll-up-or-next-url)
           (call-interactively #'scroll-all-page-down-all))))
  (advice-add 'scroll-all-check-to-scroll
              :after #'my-scroll-all-check-to-scroll))
