(require 'js2-mode)

(add-to-list 'package-selected-packages 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(define-key js2-mode-map (kbd "C-c C-a") nil)
(define-key js2-mode-map (kbd "C-c C-e") nil)
(define-key js2-mode-map (kbd "C-c C-f") nil)
(define-key js2-mode-map (kbd "C-c C-s") #'nodejs-repl-send-region)
