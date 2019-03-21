;;; -*- eval: (outline-minor-mode) -*-

;;; Window Configuration

(defvar my-ediff-startup-window-configurations nil
  "Window configurations saved on entering Ediff.

An alist (frame . wconf) of window configurations per frame.")

(defun my-ediff-save-window-configuration ()
  "Save to `my-ediff-startup-window-configurations'."
  (setf (alist-get (selected-frame)
                   my-ediff-startup-window-configurations)
        (current-window-configuration)))
(add-hook 'ediff-before-setup-hook
          #'my-ediff-save-window-configuration)

(defun my-ediff-restore-window-configuration ()
  "Restore from `my-ediff-startup-window-configurations'."
  (when-let ((window-configuration
              (alist-get (selected-frame)
                         my-ediff-startup-window-configurations)))
    (set-window-configuration window-configuration)
    (setf (alist-get (selected-frame)
                     my-ediff-startup-window-configurations nil t)
          nil)))
(add-hook 'ediff-after-quit-hook-internal
          #'my-ediff-restore-window-configuration)
