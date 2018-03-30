(defun my-prog-mode-smartscan-hook ()
  "Enable Smart Scan mode in Prog-mode buffers."
  (smartscan-mode 1))
(add-hook 'prog-mode-hook #'my-prog-mode-smartscan-hook)
