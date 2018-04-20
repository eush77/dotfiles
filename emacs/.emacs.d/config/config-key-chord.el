(key-chord-mode 1)

;; Buffer operations
(key-chord-define-global "xb" #'counsel-ibuffer)
(key-chord-define-global "xk" #'kill-buffer)
(key-chord-define-global "xs" #'save-buffer)

;; Creating or deleting windows
(key-chord-define-global "x0" #'my-balanced-delete-window)
(key-chord-define-global "x1" #'delete-other-windows)
(key-chord-define-global "x2" #'my-balanced-split-window-vertically)
(key-chord-define-global "x3" #'my-balanced-split-window-horizontally)
(key-chord-define-global "x9" #'switch-window-then-delete)
(key-chord-define-global "xq" #'quit-window)

;; Finding files
(key-chord-define-global "xf" #'find-file)
(key-chord-define-global "xg" #'counsel-git)
(key-chord-define-global "xl" #'find-library)
(key-chord-define-global "xr" #'counsel-recentf)
(key-chord-define-global "xv" #'find-alternate-file)

;; Switching windows
(key-chord-define-global "xi" #'switch-window)
(key-chord-define-global "xo" #'my-switch-to-mru-window)
