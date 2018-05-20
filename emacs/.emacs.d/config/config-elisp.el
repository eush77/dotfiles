(defun my-lisp-interaction-mode--litable ()
  "Enable Litable mode."
  (litable-mode 1))
(advice-add 'lisp-interaction-mode
            :after #'my-lisp-interaction-mode--litable)
