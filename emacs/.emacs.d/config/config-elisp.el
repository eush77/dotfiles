;;; litable

(defun my-lisp-interaction-mode--litable ()
  "Enable Litable mode."
  (litable-mode 1))
(advice-add 'lisp-interaction-mode
            :after #'my-lisp-interaction-mode--litable)

;;; emacs-lisp-macroexpand

(defun my-emacs-lisp-macroexpand--save-excursion (func &rest args)
  "Save excursion."
  (save-excursion (apply func args)))

(advice-add 'emacs-lisp-macroexpand
            :around #'my-emacs-lisp-macroexpand--save-excursion)

;;; emacs-lisp-mode-map

(define-key emacs-lisp-mode-map "\C-c\C-j" #'counsel-outline)
(define-key emacs-lisp-mode-map "\C-c\C-m" #'emacs-lisp-macroexpand)

;;; nameless

(add-hook 'emacs-lisp-mode-hook #'nameless-mode)

;;; outline-minor-mode

(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
