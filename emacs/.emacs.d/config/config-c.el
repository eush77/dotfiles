(with-eval-after-load "c-mode"
  (defun my-c-hook ()
    "My hook for C mode."
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'inextern-lang 0))
  (add-hook 'c-mode-hook #'my-c-hook))
