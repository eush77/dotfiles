;;; -*- lexical-binding: t -*-
(require 'counsel)
(require 'dash)

;;; External Commands

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

(defun my-exwm-lock-screen ()
  "Lock screen."
  (interactive)
  (call-process "slock")
  (message "Welcome back"))

(defun my-exwm-toggle-touchpad ()
  "Enable or disable touchpad."
  (interactive)
  (let ((state
         (string-to-number
          (shell-command-to-string
           "synclient -l | awk '/TouchpadOff/ { print $3 }'"))))
    (call-process "synclient" nil nil nil
                  (format "TouchpadOff=%d" (- 1 state)))))

;;; IBuffer

(defun my-counsel-ibuffer-by-exwm-class-name ()
  "`counsel-ibuffer' limited to Exwm buffers of same X class."
  (interactive)
  (require 'ibuffer)
  (cl-letf*
      ((class-name exwm-class-name)
       (get-buffers-function
        (symbol-function 'counsel-ibuffer--get-buffers))
       ((symbol-function 'counsel-ibuffer--get-buffers)
        (lambda ()
          (--filter (with-current-buffer (cdr it)
                      (and (eq major-mode 'exwm-mode)
                           (string-equal exwm-class-name class-name)))
                    (funcall get-buffers-function)))))
    (counsel-ibuffer)))

;;; Key Chords

;; Define keys participating in key chords in `exwm-mode-map' so that they are
;; passed through to Emacs.
(map-keymap
 (lambda (event-type key-chord-map)
   (when (eq event-type 'key-chord)
     (map-keymap
      (lambda (key _)
        (define-key exwm-mode-map (string key)
          (lambda ()
            (interactive)
            (exwm-input--fake-key key))))
      key-chord-map)))
 (current-global-map))

(defun my-exwm-define-key-chords ()
  "Define local key chords for Exwm buffer."
  (key-chord-define-local "XB" #'my-counsel-ibuffer-by-exwm-class-name))

(add-hook 'exwm-mode-hook #'my-exwm-define-key-chords)

;;; XDG Applications

(defvar my-xdg-web-browser-app
  (let ((default-directory "~"))
    (string-trim (shell-command-to-string
                  "xdg-settings get default-web-browser")))
  "App name of the default XDG web browser.")

(defvar my-xdg-web-browser-class-name
  (let* ((desktop-file
          (cdr (assoc my-xdg-web-browser-app
                      (counsel-linux-apps-list-desktop-files))))
         (props (xdg-desktop-read-file desktop-file)))
    (or (gethash "StartupWMClass" props)
        (gethash "Name" props)))
  "X class name of the default XDG web browser.")

;;;###autoload
(defun my-xdg-web-browser-buffer-p (&optional buffer)
  "True if BUFFER is an Exwm buffer of the default XDG web browser.

BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'exwm-mode)
         (string-equal exwm-class-name
                       my-xdg-web-browser-class-name))))

(defun my-xdg-web-browser-buffer ()
  "Get live Exwm buffer of the default XDG web browser, or nil."
  (if (my-xdg-web-browser-buffer-p)
      (current-buffer)
    (seq-find #'my-xdg-web-browser-buffer-p (buffer-list))))

;;;###autoload
(defun my-xdg-web-browser ()
  "Launch or switch to the default XDG web browser."
  (interactive)
  (if-let ((buffer (my-xdg-web-browser-buffer)))
      (if-let ((window (get-buffer-window buffer t)))
          (select-window window)
        (pop-to-buffer-same-window buffer))
    (counsel-linux-app-action-default (cons nil my-xdg-web-browser-app))))

;;;###autoload
(defun my-xdg-web-browser-get-current-url ()
  "Get URL visited in the default XDG web browser, or nil."
  (when-let ((buffer (my-xdg-web-browser-buffer)))
    (with-current-buffer buffer
      (unless (string-equal exwm-class-name "Firefox")
        (error "Unsupported web browser %s" exwm-class-name))
      (with-selected-window (get-buffer-window)
        (exwm-input--fake-key ?\C-l)    ; Focus location bar
        (sit-for 0)
        (exwm-input--fake-key ?\C-c)
        (sit-for .2)
        (let ((url (current-kill 0 t)))
          (with-temp-buffer
            (insert url)
            (thing-at-point 'url)))))))

;;; exwm-input-global-keys

(custom-set-variables
 '(exwm-input-global-keys
   `((,(kbd "<XF86AudioLowerVolume>") . emms-volume-lower)
     (,(kbd "<XF86AudioMute>") . my-emms-volume-mute)
     (,(kbd "<XF86AudioRaiseVolume>") . emms-volume-raise)
     (,(kbd "<XF86MonBrightnessDown>") . my-exwm-brightness-down)
     (,(kbd "<XF86MonBrightnessUp>") . my-exwm-brightness-up)
     (,(kbd "<XF86TouchpadToggle>") . my-exwm-toggle-touchpad)
     (,(kbd "C-g") . keyboard-quit)
     (,(kbd "C-M-j") . window-jump-left)
     (,(kbd "C-M-k") . window-jump-right)
     (,(kbd "C-M-n") . window-jump-down)
     (,(kbd "C-M-p") . window-jump-up)
     (,(kbd "M-`") . my-exwm-workspace-switch-or-next)
     (,(kbd "M-<tab>") . my-switch-window)
     (,(kbd "s-a") . exwm-workspace-add)
     (,(kbd "s-d") . exwm-workspace-delete)
     (,(kbd "s-l") . my-exwm-lock-screen)
     (,(kbd "s-m") . exwm-workspace-move)
     (,(kbd "s-n") . my-exwm-workspace-next)
     (,(kbd "s-p") . my-exwm-workspace-previous)
     (,(kbd "s-r") . exwm-reset)
     (,(kbd "s-s") . exwm-workspace-swap)
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
     ([?\M-<] . [C-home])
     ([?\M->] . [C-end])
     ([?\M-h] . [?\C-a])
     ([?\M-v] . [prior])
     ([?\M-w] . [?\C-c]))))

;;; exwm-update-class-hook

(defun my-exwm-update-buffer-name ()
  (exwm-workspace-rename-buffer exwm-class-name))

;; Part of `exwm-config-default'
(add-hook 'exwm-update-class-hook #'my-exwm-update-buffer-name)

;;; exwm-workspace

(defun my-exwm-workspace-next (n)
  "Switch to the next N-th workspace."
  (interactive "p")
  (exwm-workspace-switch (mod (+ exwm-workspace-current-index n)
                              (exwm-workspace--count))))

(defun my-exwm-workspace-previous (n)
  "Switch to the previous N-th workspace."
  (interactive "p")
  (my-exwm-workspace-next (- n)))

(defun my-exwm-workspace-switch-or-next (&optional force-display-p)
  "Switch to another workspace interactively.

If there is only one workspace or a couple, switch immediately
without displaying the switching interface unless FORCE-DISPLAY-P
is non-nil."
  (interactive "P")
  (cl-case (and (not force-display-p) (length (frame-list)))
    (1 (user-error "No other workspace"))
    (2 (my-exwm-workspace-next 1))
    (otherwise
     (let ((current-prefix-arg))
       (call-interactively 'exwm-workspace-switch)))))

(custom-set-variables
 '(exwm-layout-show-all-buffers t)
 '(exwm-workspace-show-all-buffers t))

(defun my-exwm-workspace-display-current ()
  "Display current workspace index."
  (interactive)
  (let ((message-log-max))
    (message (elt exwm-workspace--switch-history
                  exwm-workspace-current-index))))

(add-hook 'exwm-workspace-switch-hook #'my-exwm-workspace-display-current)
(advice-add 'exwm-workspace-add :after #'my-exwm-workspace-display-current)
(advice-add 'exwm-workspace-delete :after #'my-exwm-workspace-display-current)

(defvar my-exwm-workspace-switch-prompt-method 'workspace
  "Prompt method for `exwm-workspace-switch'.

If symbol `workspace', use the default prompt of
`exwm-workspace--prompt-for-workspace'. If symbol `buffer-names',
use `my-select-frame-by-buffer-names'.")

(defun my-exwm-workspace-switch-toggle-prompt-method ()
  "Toggle `my-exwm-workspace-switch-prompt-method'."
  (interactive)
  (setq my-exwm-workspace-switch-prompt-method
        (cl-case my-exwm-workspace-switch-prompt-method
          (workspace 'buffer-names)
          (buffer-names 'workspace)
          (otherwise (error "Unsupported prompt method")))))

(defun my-exwm-workspace-prompt-for-workspace--switch (func &rest args)
  "Switch between workspaces interactively."
  (if (not (memq this-command '(exwm-workspace-switch
                                my-exwm-workspace-switch-or-next)))
      (apply func args)
    (cl-letf* ((inhibit-message t)      ; my-exwm-workspace-display-current
               ((symbol-function 'next-history-element)
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
               (rethrow-toggle-prompt
                (lambda ()
                  (interactive)
                  (throw 'reprompt 'toggle-prompt)))
               (exwm-workspace--switch-map
                (let ((keymap-parent exwm-workspace--switch-map)
                      (keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap keymap-parent)
                  (define-key keymap (kbd "M-`") rethrow-toggle-prompt)
                  (define-key keymap (kbd "n") #'previous-history-element)
                  (define-key keymap (kbd "p") #'next-history-element)
                  (define-key keymap (kbd "s-w") #'previous-history-element)
                  keymap))
               (minibuffer-local-must-match-map
                (let ((keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap minibuffer-local-must-match-map)
                  (define-key keymap (kbd "M-`") rethrow-toggle-prompt)
                  keymap))
               (ivy-minibuffer-map
                (let ((keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap ivy-minibuffer-map)
                  (define-key keymap (kbd "M-`") rethrow-toggle-prompt)
                  keymap)))
      (catch 'switch
        (while t
          (pcase (catch 'reprompt
                   (cl-case my-exwm-workspace-switch-prompt-method
                     (workspace (apply func args))
                     (buffer-names (my-select-frame-by-buffer-names))
                     (otherwise (error "Unsupported prompt method"))))
            (`(switch . ,n) (exwm-workspace-switch n))
            ('delete (let ((frame (selected-frame)))
                       (other-frame 1)
                       (delete-frame frame)))
            ('toggle-prompt (my-exwm-workspace-switch-toggle-prompt-method))
            (frame (throw 'switch frame))))))))

(advice-add 'exwm-workspace--prompt-for-workspace
            :around #'my-exwm-workspace-prompt-for-workspace--switch)

(defun my-exwm-workspace-update-switch-history (&rest _)
  "Update `exwm-workspace--switch-history'."
  (exwm-workspace--update-switch-history))

(advice-add 'exwm-workspace-add
            :after #'my-exwm-workspace-update-switch-history)
(advice-add 'exwm-workspace-delete
            :after #'my-exwm-workspace-update-switch-history)

;;; exwm-xim

(require 'exwm-xim)

(exwm-xim-enable)

(--each (where-is-internal 'toggle-input-method (current-global-map))
  (when (= (length it) 1)
    (add-to-list 'exwm-input-prefix-keys (elt it 0))))
