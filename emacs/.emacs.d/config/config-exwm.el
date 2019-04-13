;;; -*- lexical-binding: t -*-

;;; exwm-input-global-keys

(defun my-exwm-brightness-down ()
  "Decrease screen brightness."
  (interactive)
  (call-process "xbacklight" nil nil nil "-dec" "10"))

(defun my-exwm-brightness-up ()
  "Increase screen brightness."
  (interactive)
  (call-process "xbacklight" nil nil nil "-inc" "10"))

(defun my-exwm-toggle-touchpad ()
  "Enable or disable touchpad."
  (interactive)
  (let ((state
         (string-to-number
          (shell-command-to-string
           "synclient -l | awk '/TouchpadOff/ { print $3 }'"))))
    (call-process "synclient" nil nil nil
                  (format "TouchpadOff=%d" (- 1 state)))))

(custom-set-variables
 '(exwm-input-global-keys
   `((,(kbd "<XF86MonBrightnessDown>") . my-exwm-brightness-down)
     (,(kbd "<XF86MonBrightnessUp>") . my-exwm-brightness-up)
     (,(kbd "<XF86TouchpadToggle>") . my-exwm-toggle-touchpad)
     (,(kbd "s-r") . exwm-reset)
     ,@(mapcar (lambda (windex)
                 `(,(kbd (format "<f%d>" (+ windex 1)))
                   . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create ,windex))))
               (number-sequence 0 7)))))

;;; exwm-input-simulation-keys

;; Part of `exwm-config-default'
(custom-set-variables
 '(exwm-input-simulation-keys
   '(([?\C-b] . [left])
     ([?\C-f] . [right])
     ([?\C-p] . [up])
     ([?\C-n] . [down])
     ([?\C-a] . [home])
     ([?\C-e] . [end])
     ([?\M-v] . [prior])
     ([?\C-v] . [next])
     ([?\C-d] . [delete])
     ([?\C-k] . [S-end delete]))))

;;; exwm-update-class-hook

(defun my-exwm-update-buffer-name ()
  (exwm-workspace-rename-buffer exwm-class-name))

;; Part of `exwm-config-default'
(add-hook 'exwm-update-class-hook #'my-exwm-update-buffer-name)
