;;; litable

(defun my-lisp-interaction-mode--litable ()
  "Enable Litable mode."
  (litable-mode 1))
(advice-add 'lisp-interaction-mode
            :after #'my-lisp-interaction-mode--litable)

;;; emacs-lisp-mode-map

(define-key emacs-lisp-mode-map "\C-c\C-j" #'counsel-outline)

;;; outline-minor-mode

(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
