;;; -*- lexical-binding: t -*-

;;; Commands

(defun my-exwm-brightness-change (amount)
  "Change screen brightness by AMOUNT."
  (interactive "nChange screen brightness by [-100..100]: ")
  (call-process "xbacklight" nil nil nil "-inc" (number-to-string amount))
  (message "Screen brightness set to %s%%"
           (string-trim (shell-command-to-string "xbacklight -get"))))

;;;###autoload
(defcustom my-exwm-brightness-change-amount 10
  "The amount to use when raising or lowering screen brightness.

See `my-exwm-brightness-down', `my-exwm-brightness-up'."
  :type 'integer
  :group 'my)

(defun my-exwm-brightness-down ()
  "Decrease screen brightness."
  (interactive)
  (my-exwm-brightness-change (- my-exwm-brightness-change-amount)))

(defun my-exwm-brightness-up ()
  "Increase screen brightness."
  (interactive)
  (my-exwm-brightness-change my-exwm-brightness-change-amount))

(defun my-exwm-toggle-touchpad ()
  "Enable or disable touchpad."
  (interactive)
  (let ((state
         (string-to-number
          (shell-command-to-string
           "synclient -l | awk '/TouchpadOff/ { print $3 }'"))))
    (call-process "synclient" nil nil nil
                  (format "TouchpadOff=%d" (- 1 state)))))

(defun my-exwm-workspace-next (n)
  "Switch to the next N-th workspace."
  (interactive "p")
  (exwm-workspace-switch (mod (+ exwm-workspace-current-index n)
                              (exwm-workspace--count))))

(defun my-exwm-workspace-previous (n)
  "Switch to the previous N-th workspace."
  (interactive "p")
  (my-exwm-workspace-next (- n)))

;;; exwm-input-global-keys

(custom-set-variables
 '(exwm-input-global-keys
   `((,(kbd "<XF86AudioLowerVolume>") . emms-volume-lower)
     (,(kbd "<XF86AudioMute>") . my-emms-volume-mute)
     (,(kbd "<XF86AudioRaiseVolume>") . emms-volume-raise)
     (,(kbd "<XF86MonBrightnessDown>") . my-exwm-brightness-down)
     (,(kbd "<XF86MonBrightnessUp>") . my-exwm-brightness-up)
     (,(kbd "<XF86TouchpadToggle>") . my-exwm-toggle-touchpad)
     (,(kbd "C-M-j") . window-jump-left)
     (,(kbd "C-M-k") . window-jump-right)
     (,(kbd "C-M-n") . window-jump-down)
     (,(kbd "C-M-p") . window-jump-up)
     (,(kbd "s-r") . exwm-reset)
     (,(kbd "s-a") . exwm-workspace-add)
     (,(kbd "s-d") . exwm-workspace-delete)
     (,(kbd "s-n") . my-exwm-workspace-next)
     (,(kbd "s-p") . my-exwm-workspace-previous)
     (,(kbd "s-w") . exwm-workspace-switch)
     ,@(mapcar (lambda (windex)
                 `(,(kbd (format "<f%d>" (+ windex 1)))
                   . (lambda ()
                       (interactive)
                       (exwm-workspace-switch ,windex))))
               (number-sequence 0 7)))))

;;; exwm-input-simulation-keys

(custom-set-variables
 '(exwm-input-simulation-keys
   '(([?\C-a] . [home])
     ([?\C-b] . [left])
     ([?\C-d] . [delete])
     ([?\C-e] . [end])
     ([?\C-f] . [right])
     ([?\C-k] . [S-end delete])
     ([?\C-n] . [down])
     ([?\C-p] . [up])
     ([?\C-v] . [next])
     ([?\C-w] . [?\C-x])
     ([?\C-y] . [?\C-v])
     ([?\M-<] . [home])
     ([?\M->] . [end])
     ([?\M-h] . [?\C-a])
     ([?\M-v] . [prior])
     ([?\M-w] . [?\C-c]))))

;;; exwm-update-class-hook

(defun my-exwm-update-buffer-name ()
  (exwm-workspace-rename-buffer exwm-class-name))

;; Part of `exwm-config-default'
(add-hook 'exwm-update-class-hook #'my-exwm-update-buffer-name)

;;; exwm-workspace

(defun my-exwm-workspace-prompt-for-workspace--switch (func &rest args)
  "Switch between workspaces interactively."
  (if (not (eq this-command 'exwm-workspace-switch))
      (apply func args)
    (cl-letf* (((symbol-function 'next-history-element)
                (lambda ()
                  (interactive)
                  (throw 'reprompt
                         (cons 'switch (mod (- minibuffer-history-position 2)
                                            (exwm-workspace--count))))))
               ((symbol-function 'previous-history-element)
                (lambda ()
                  (interactive)
                  (throw 'reprompt
                         (cons 'switch (mod minibuffer-history-position
                                            (exwm-workspace--count))))))
               (goto-history-element-function
                (symbol-function 'goto-history-element))
               ((symbol-function 'goto-history-element)
                (lambda (n)
                  (interactive "p")
                  (cond
                   ((string-equal (this-command-keys) "\C-a")
                    (throw 'reprompt (cons 'switch 0)))
                   ((string-equal (this-command-keys) "\C-e")
                    (throw 'reprompt (cons 'switch
                                           (1- (exwm-workspace--count)))))
                   (t (funcall goto-history-element-function n)))))
               ((symbol-function 'exwm-workspace--prompt-delete)
                (lambda ()
                  (interactive)
                  (when (< 1 (exwm-workspace--count))
                    (throw 'reprompt 'delete))))
               (exwm-workspace--switch-map
                (let ((keymap-parent exwm-workspace--switch-map)
                      (keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap keymap-parent)
                  (define-key keymap (kbd "n") #'previous-history-element)
                  (define-key keymap (kbd "p") #'next-history-element)
                  (define-key keymap (kbd "s-w") #'previous-history-element)
                  keymap)))
      (catch 'switch
        (while t
          (pcase (catch 'reprompt (apply func args))
            (`(switch . ,n) (exwm-workspace-switch n))
            ('delete (let ((frame (selected-frame)))
                       (other-frame 1)
                       (delete-frame frame)))
            (frame (throw 'switch frame))))))))

(advice-add 'exwm-workspace--prompt-for-workspace
            :around #'my-exwm-workspace-prompt-for-workspace--switch)
