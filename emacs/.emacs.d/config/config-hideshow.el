(add-hook 'prog-mode-hook #'hs-minor-mode)

(defun my-outline-minor-mode--hideshow (&rest args)
  "Disable `hs-minor-mode' while `outline-minor-mode' is
enabled."
  (if outline-minor-mode
      (when (derived-mode-p 'prog-mode)
        (hs-minor-mode 1))
    (hs-minor-mode 0)))
(advice-add 'outline-minor-mode
            :before #'my-outline-minor-mode--hideshow)
