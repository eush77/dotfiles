(add-to-list 'package-selected-packages 'js2-mode)
(package-install-selected-packages)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
