(add-to-list 'package-selected-packages 'forth-mode)

(defun my-forth-hook ()
  "My hook for Forth mode."
  (setq-local comment-start "\\ ")
  (setq-local comment-end ""))
(add-hook 'forth-mode-hook #'my-forth-hook)
