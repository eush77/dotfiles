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
(key-chord-define-global "xm" #'my-org-jump)
(key-chord-define-global "xn" #'my-org-notes-jump)
(key-chord-define-global "xr" #'counsel-recentf)
(key-chord-define-global "xv" #'find-alternate-file)

;; Movement
(key-chord-define-global "gb" #'iy-go-to-char-backward)
(key-chord-define-global "gf" #'iy-go-up-to-char)
(key-chord-define-global "gj" #'avy-goto-char-timer)
(key-chord-define-global "gm" #'avy-goto-line)
(key-chord-define-global "gw" #'avy-goto-word-1)

;; Switching windows
(key-chord-define-global "xh" #'my-switch-to-mru-window)
(key-chord-define-global "xj" #'switch-window)
