(key-chord-mode 1)

(key-chord-define-global "x1" #'delete-other-windows)
(key-chord-define-global "x2" #'my-balanced-split-window-vertically)
(key-chord-define-global "x3" #'my-balanced-split-window-horizontally)
(key-chord-define-global "x0" #'my-balanced-delete-window)
(key-chord-define-global "xb" #'ivy-switch-buffer)
(key-chord-define-global "xk" #'kill-buffer)
(key-chord-define-global "xq" #'quit-window)
(key-chord-define-global "xs" #'save-buffer)
(key-chord-define-global "xf" #'counsel-find-file)
(key-chord-define-global "xl" #'counsel-find-library)
(key-chord-define-global "xg" #'counsel-git)
(key-chord-define-global "xv" #'find-alternate-file)
