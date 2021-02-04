;;; -*- lexical-binding: t -*-
(require 'counsel)
(require 'dash)

;;; applications

;;;###autoload
(defun my-exwm-app-name (app)
  "Get name of an application APP."
  (gethash "Name"
           (xdg-desktop-read-file
            (assoc-default
             app
             (counsel-linux-apps-list-desktop-files)))))

;;;###autoload
(defun my-exwm-app-class-name (app)
  "Get Window Class name of an application APP."
  (--> (xdg-desktop-read-file
        (assoc-default
         app
         (counsel-linux-apps-list-desktop-files)))
       (or (gethash "StartupWMClass" it)
           (gethash "Name" it))))

(defun my-exwm-find-buffer-by-app (app)
  "Find Exwm buffer displaying APP. "
  (my-exwm-find-buffer-by-class-name (my-exwm-app-class-name app)))

(defun my-exwm-find-buffer-by-class-name (class-name)
  "Find Exwm buffer by class name.

The function favors the current buffer and the buffers displayed
in the selected frame."
  (--find (with-current-buffer it
            (and (derived-mode-p 'exwm-mode)
                 (string-equal exwm-class-name class-name)))
          (cons (current-buffer) (buffer-list (selected-frame)))))

;;;###autoload
(defun my-start-app (app &optional class-name)
  "Start the app or switch to a running instance.

CLASS-NAME defaults to the result of calling
`my-exwm-app-class-name' on APP.

If the app is running in the selected window already, restore
the window configuration before the app was opened."
  (unless class-name
    (setq class-name (my-exwm-app-class-name app)))
  (let ((buffer (my-exwm-find-buffer-by-class-name class-name)))
    (cond
     ((and (eq (current-buffer) buffer)
           (boundp 'my-saved-window-configuration))
      (let ((configuration my-saved-window-configuration))
        (makunbound 'my-saved-window-configuration)
        (set-window-configuration configuration)))
     ((eq (current-buffer) buffer)
      (quit-window))
     ((and buffer (not (-> buffer
                           (get-buffer-window t)
                           (window-frame)
                           (eq (selected-frame)))))
      (select-window (get-buffer-window buffer t)))
     (buffer
      (let ((saved-configuration (current-window-configuration)))
        ;; Delete other windows before switching to avoid jitter.
        (delete-other-windows)
        (exwm-workspace-switch-to-buffer buffer)
        (setf (buffer-local-value 'my-saved-window-configuration buffer)
              saved-configuration)))
     (t
      ;; The buffer is created asynchronously, so schedule the rest of
      ;; the function using `exwm-update-class-hook'.
      (setq my-start-app--class-name class-name
            my-start-app--saved-configuration
            (current-window-configuration))
      (add-hook 'exwm-update-class-hook
                #'my-start-app--on-update-class
                t)
      (let ((default-directory "/"))
        (counsel-linux-app-action-default (cons nil app)))))))

(defvar my-start-app--class-name)
(defvar my-start-app--saved-configuration)

(defun my-start-app--on-update-class ()
  (remove-hook 'exwm-update-class-hook 'my-start-app--on-update-class)
  (setf (buffer-local-value
         'my-saved-window-configuration
         (my-exwm-find-buffer-by-class-name my-start-app--class-name))
        my-start-app--saved-configuration)
  (makunbound 'my-start-app--class-name)
  (makunbound 'my-start-app--saved-configuration)
  (when (one-window-p)
    ;; The window may not be fully drawn for some reason, force redisplay to
    ;; fix it.
    (split-window))
  (delete-other-windows))

;;; exwm-edit

;;;###autoload
(defun my-exwm-edit ()
  "Edit text in the current text field."
  (interactive)
  (exwm-input--fake-key ?\C-a)
  (sit-for 0)
  (let ((previous-kill (current-kill 0 t)))
    (exwm-input--fake-key ?\C-c)
    (sit-for .2)
    (let ((current-kill (current-kill 0 t)))
      (kill-new
       (read-string "Input: "
                    (unless (string-equal current-kill previous-kill)
                      current-kill)))))
  (exwm-input--fake-key ?\C-v))

;;; global keys

(defun my-exwm-ctrl-ret (arg)
  "Command associated with C-<return> in Exwm buffers.

Without prefix argument, toggle key-chord state in the current
buffer, equivalent to (my-exwm-key-chord-state (current-buffer)
t).

With prefix argument, call `my-exwm-edit'."
  (declare (interactive-only t))
  (interactive "P")
  (when (derived-mode-p 'exwm-mode)
    (if arg
        (my-exwm-edit)
      (my-exwm-key-chord-state (current-buffer) t))))

(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'emms-volume-lower)
(exwm-input-set-key (kbd "<S-XF86AudioLowerVolume>") #'my-emms-volume-fine-lower)
(exwm-input-set-key (kbd "<XF86AudioMute>") #'my-emms-volume-mute)
(exwm-input-set-key (kbd "<XF86AudioNext>") #'my-media-player-next-track)
(exwm-input-set-key (kbd "<XF86AudioPlay>") #'my-media-player-pause)
(exwm-input-set-key (kbd "<XF86AudioPrev>") #'my-media-player-previous-track)
(exwm-input-set-key (kbd "<XF86AudioStop>") #'my-media-player-next-state)
(exwm-input-set-key (kbd "<XF86Favorites>") #'my-media-player)
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'emms-volume-raise)
(exwm-input-set-key (kbd "<S-XF86AudioRaiseVolume>") #'my-emms-volume-fine-raise)
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'my-screen-brightness-down)
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'my-screen-brightness-up)
(exwm-input-set-key (kbd "<XF86TouchpadToggle>") #'my-touchpad-toggle)
(exwm-input-set-key (kbd "C-<return>") #'my-exwm-ctrl-ret)
(exwm-input-set-key (kbd "C-g") #'keyboard-quit)
(exwm-input-set-key (kbd "C-M-j") #'window-jump-left)
(exwm-input-set-key (kbd "C-M-k") #'window-jump-right)
(exwm-input-set-key (kbd "C-M-n") #'window-jump-down)
(exwm-input-set-key (kbd "C-M-p") #'window-jump-up)
(exwm-input-set-key (kbd "M-`") #'my-exwm-workspace-switch-or-next)
(exwm-input-set-key (kbd "M-<tab>") #'my-switch-window)
(exwm-input-set-key (kbd "s-a") #'exwm-workspace-add)
(exwm-input-set-key (kbd "s-d") #'exwm-workspace-delete)
(exwm-input-set-key (kbd "s-l") #'my-screen-lock)
(exwm-input-set-key (kbd "s-m") #'exwm-workspace-move)
(exwm-input-set-key (kbd "s-n") #'my-exwm-workspace-next)
(exwm-input-set-key (kbd "s-p") #'my-exwm-workspace-previous)
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-s") #'exwm-workspace-swap)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

(dotimes (n 8)
  (exwm-input-set-key (kbd (format "<f%d>" (+ n 1)))
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch n))))

;;; ibuffer

;;;###autoload
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

;;; key chords

(defun my-exwm-input-fake-key-command ()
  "Pass command keys through to the Exwm buffer."
  (interactive)
  (mapc #'exwm-input--fake-key (this-command-keys)))

(defvar my-exwm-key-chord-keys nil
  "List of keys that participare in key chords.")

(map-keymap
 (lambda (event-type key-chord-map)
   (when (eq event-type 'key-chord)
     (map-keymap
      (lambda (key _)
        (add-to-list 'my-exwm-key-chord-keys key))
      key-chord-map)))
 (current-global-map))

(defun my-exwm-key-chord-state (buffer &optional toggle-p)
  "Manage key-chord state in Exwm BUFFER.

If TOGGLE-P is non-nil, switch the state to the opposite.

Return the current state.

The state is stored in the buffer-local variable
`my-exwm-key-chord-state' and is t by default. Calling this
function forces the variable to be created if it's missing."
  (cl-assert (with-current-buffer buffer
               (derived-mode-p 'exwm-mode)))
  (let ((value
         (if (local-variable-p 'my-exwm-key-chord-state buffer)
             (buffer-local-value 'my-exwm-key-chord-state buffer)
           (setf (buffer-local-value 'my-exwm-key-chord-state buffer) t))))
    (when toggle-p
      (setq value (not value))
      (setf (buffer-local-value 'my-exwm-key-chord-state buffer) value)
      (message "Key chords %s in %S"
               (if value "enabled" "disabled")
               buffer)
      (if (eq buffer (current-buffer))
          (my-exwm-apply-key-chord-state buffer)))
    value))

(defun my-exwm-apply-key-chord-state (buffer)
  "Apply key-chord state in Exwm BUFFER."
  (let ((command (and (my-exwm-key-chord-state buffer)
                      'my-exwm-input-fake-key-command)))
    (dolist (key my-exwm-key-chord-keys)
      (define-key exwm-mode-map (string key) command))))

(define-advice exwm-input--set-active-window
    (:after (&optional id) my-exwm-key-chord-state)
  "Apply key-chord state in the current buffer."
  (when id
    (my-exwm-apply-key-chord-state (current-buffer))))

(defun my-exwm-define-key-chords ()
  "Define local key chords for Exwm buffer."
  (key-chord-define-local "XB" #'my-counsel-ibuffer-by-exwm-class-name))

(add-hook 'exwm-mode-hook #'my-exwm-define-key-chords)

;;; media player

(require 'dbus)

;;;###autoload
(defcustom my-media-players
  '(("Yandex Music"
     "ymp.desktop"
     "yandex-music-player"
     "org.mpris.MediaPlayer2.YandexMusic"
     nil)
    ("Spotify"
     "spotify.desktop"
     "Spotify"
     "org.mpris.MediaPlayer2.spotify"
     nil)
    ("Spotifyd"
     "spotify.desktop"
     "Spotify"
     "org.mpris.MediaPlayer2.spotifyd"
     "spotifyd"))
  "Alist of supported media players.

Each entry is a list (NAME APP_NAME WINDOW_CLASS MPRIS SERVICE)
where:

- NAME is a unique name of the player,
- APP_NAME is the base name of its desktop entry file,
- WINDOW_CLASS is the name of its X window class (nil to
  determine automatically),
- MPRIS is the name of its MPRIS D-Bus service, and
- SERVICE is the name of its Systemd service."
  :type `(alist :key-type (string :tag "Name")
                :value-type
                (group (choice
                        :tag "App Name"
                        ,@(--map `(const ,(car it))
                                 (counsel-linux-apps-list-desktop-files)))
                       (choice :tag "Window Class"
                               (const :tag "Auto" nil)
                               string)
                       (string :tag "MPRIS Service")))
  :group 'my)

;;;###autoload
(defcustom my-media-player (caar my-media-players)
  "Currently used media player.

The name must be one of the keys in `my-media-players' alist."
  :type `(radio ,@(--map `(const ,(car it)) my-media-players)
                (string :tag "Other"
                        :validate
                        (lambda (widget)
                          (unless (assoc (widget-value widget)
                                         my-media-players)
                            (widget-put
                             widget :error
                             "No such player defined in `my-media-players'")
                            widget))))
  :group 'my)

;;;###autoload
(defun my-media-player-app ()
  (car (assoc-default my-media-player my-media-players)))

;;;###autoload
(defun my-media-player-class-name ()
  (pcase-let ((`(,app ,class-name . _)
               (assoc-default my-media-player my-media-players)))
    (or class-name (my-exwm-app-class-name app))))

(defun my-media-player-mpris-service ()
  (caddr (assoc-default my-media-player my-media-players)))

(defun my-media-player-systemd-service ()
  (cadddr (assoc-default my-media-player my-media-players)))

(defun my-media-player-call-method (name)
  "Call method NAME on the MediaPlayer2 interface."
  (dbus-call-method :session
                    (my-media-player-mpris-service)
                    "/org/mpris/MediaPlayer2"
                    "org.mpris.MediaPlayer2.Player"
                    name
                    :timeout 1000))

(defun my-media-player-get-property (name)
  "Get property NAME on the MediaPlayer2 interface."
  (dbus-get-property :session
                    (my-media-player-mpris-service)
                    "/org/mpris/MediaPlayer2"
                    "org.mpris.MediaPlayer2.Player"
                    name))

(defun my-media-player-handle-dbus-error (func &rest args)
  "Call FUNC while handling `dbus-error' error signals."
  (condition-case error
      (apply func args)
    (dbus-error
     (if-let* ((service (my-media-player-systemd-service))
               ((yes-or-no-p
                 (format "%s\nRestart %s.service? "
                         (error-message-string error) service))))
         (call-process "systemctl" nil nil nil "restart" "--user" service)
       (signal 'dbus-error (cdr error))))))

(advice-add 'my-media-player-call-method
            :around #'my-media-player-handle-dbus-error)
(advice-add 'my-media-player-get-property
            :around #'my-media-player-handle-dbus-error)

;;;###autoload
(defun my-media-player-next-track ()
  "Switch to the next track."
  (interactive)
  (if (my-media-player-get-property "CanGoNext")
      (my-media-player-call-method "Next")
    (message "%s has no next track" my-media-player)))

;;;###autoload
(defun my-media-player-pause ()
  "Toggle play/pause."
  (interactive)
  (if (my-media-player-get-property "CanPause")
      (my-media-player-call-method "PlayPause")
    (message "%s has no current track" my-media-player)))

;;;###autoload
(defun my-media-player-previous-track ()
  "Switch to the previous track."
  (interactive)
  (if (my-media-player-get-property "CanGoPrevious")
      (my-media-player-call-method "Previous")
    (message "%s has no previous track" my-media-player)))

;;;###autoload
(defun my-media-player-stop ()
  "Stop playback."
  (interactive)
  (if (my-media-player-get-property "CanControl")
      (my-media-player-call-method "Stop")
    (message "%s has no current track" my-media-player)))

(defvar my-media-player-next-state nil
  "Next state to be applied when track changes.

One of symbols `stop-after', `loop-track', or nil.")

(defun my-media-player-next-state (state)
  "Toggle variable `my-media-player-next-state'.

If called interactively, switch to the next state."
  (interactive (list (cl-case my-media-player-next-state
                       (stop-after 'loop-track)
                       (loop-track nil)
                       (t 'stop-after))))
  (setq my-media-player-next-state state)
  (message (cl-case state
             (stop-after "Stopping at next track")
             (loop-track "Looping this track")
             (t "Playing through"))))

(defcustom my-media-player-notifications t
  "Show notifications when tracks change."
  :type 'boolean
  :group 'my)

(defclass my-media-player-track-info ()
  ((title :type string :initarg :title :initform nil)
   (album :type string :initarg :album :initform nil)
   (artist :type string :initarg :artist :initform nil))
  "Track metadata."
  :allow-nil-initform t)

(defun my-media-player-current-track-info ()
  "Create track info for the currently playing track."
  (let ((metadata (my-media-player-get-property "Metadata")))
    (my-media-player-track-info
     :title (caar (assoc-default "xesam:title" metadata))
     :album (caar (assoc-default "xesam:album" metadata))
     :artist (mapconcat #'identity
                        (caar (assoc-default "xesam:artist" metadata))
                        ", "))))

(defvar my-media-player-current-track-info nil
  "Track info for the currently playing track, or last played.")

(defun my-media-player-update-current-track-info ()
  "Update variable `my-media-player-current-track-info'.

Returns t if the update was not a no-op."
  (let* ((track-info (my-media-player-current-track-info))
         (updated-p (not (equal my-media-player-current-track-info
                                track-info))))
    (setq my-media-player-current-track-info track-info)
    updated-p))

(defun my-media-player-notify ()
  "Send current track info as a desktop notification."
  (with-slots (title album artist) my-media-player-current-track-info
    (notifications-notify
     :app-name my-media-player
     :title (if (and artist title)
                (format "%s - %s" artist title)
              (or artist title))
     :body album)))

(defun my-media-player-on-state-change ()
  "Handle the player changing state.

Must be called by the player on pause/unpause, but at least when
the current track changes."
  (pcase (cons my-media-player-next-state
               (my-media-player-update-current-track-info))
    ('(stop-after . t)
     (setq my-media-player-next-state nil)
     (my-media-player-stop))
    ('(loop-track . t)
     (my-media-player-previous-track))
    ((and `(,_ . ,updated-p)
          (guard
           (and my-media-player-notifications
                (or updated-p
                    (string-equal
                     (my-media-player-get-property "PlaybackStatus")
                     "Playing")))))
     (my-media-player-notify))))

;;; notification server

(defun my-notify-server-information (&rest args)
  "GetServerInformation for `my-notify-message'."
  '("Emacs Message" "my" "0.0.1" "1.2"))

(defun my-notify-capabilities (&rest args)
  "GetCapabilities for `my-notify-message'."
  '("body"))

(defun my-notify-message (app-name
                          _replaces_id
                          _app_icon
                          summary
                          body
                          _actions
                          _hints
                          _expire_timeout)
  "Handle D-Bus notification using `message'."
  (if (and body (not (string-empty-p body)))
      (message "%s: %s (%s)" app-name summary body)
    (message "%s: %s" app-name summary)))

(defun my-notify-close (id)
  "CloseNotification for `my-notify-message'."
  nil)

(dbus-register-method :session
                      "org.freedesktop.Notifications"
                      "/org/freedesktop/Notifications"
                      "org.freedesktop.Notifications"
                      "GetCapabilities"
                      #'my-notify-capabilities)
(dbus-register-method :session
                      "org.freedesktop.Notifications"
                      "/org/freedesktop/Notifications"
                      "org.freedesktop.Notifications"
                      "CloseNotification"
                      #'my-notify-close)
(dbus-register-method :session
                      "org.freedesktop.Notifications"
                      "/org/freedesktop/Notifications"
                      "org.freedesktop.Notifications"
                      "Notify"
                      #'my-notify-message)
(dbus-register-method :session
                      "org.freedesktop.Notifications"
                      "/org/freedesktop/Notifications"
                      "org.freedesktop.Notifications"
                      "GetServerInformation"
                      #'my-notify-server-information)

;;; simulation keys

(custom-set-variables
 '(exwm-input-simulation-keys
   '(([?\C-_] . [?\C-z])
     ([?\C-a] . [home])
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

;;; screen brightness

(defvar my-screen-brightness-deferred-p nil)
(defvar my-screen-brightness-deferred-amount 0)

(defun my-screen-brightness-change-sentinel (process _event)
  (setq my-screen-brightness-deferred-p nil)
  (if (zerop my-screen-brightness-deferred-amount)
      (message "Screen brightness set to %s%%"
               (truncate
                (string-to-number
                 (my-local-shell-command-to-string "xbacklight -get"))))
    (my-screen-brightness-change my-screen-brightness-deferred-amount)))

;;;###autoload
(defun my-screen-brightness-change (amount)
  "Change screen brightness by AMOUNT."
  (interactive "nChange screen brightness by [-100..100]: ")
  (if my-screen-brightness-deferred-p
      (setq my-screen-brightness-deferred-amount
            (+ my-screen-brightness-deferred-amount amount))
    (setq my-screen-brightness-deferred-p t
          my-screen-brightness-deferred-amount 0)
    (make-process :name "xbacklight"
                  :command `("xbacklight" "-inc" ,(number-to-string amount))
                  :sentinel #'my-screen-brightness-change-sentinel)))

;;;###autoload
(defcustom my-screen-brightness-change-amount 10
  "The amount to use when raising or lowering screen brightness.

See `my-screen-brightness-down', `my-screen-brightness-up'."
  :type 'integer
  :group 'my)

;;;###autoload
(defun my-screen-brightness-down ()
  "Decrease screen brightness."
  (interactive)
  (my-screen-brightness-change (- my-screen-brightness-change-amount)))

;;;###autoload
(defun my-screen-brightness-up ()
  "Increase screen brightness."
  (interactive)
  (my-screen-brightness-change my-screen-brightness-change-amount))

;;; screen lock

;;;###autoload
(defun my-screen-lock ()
  "Lock screen."
  (interactive)
  (call-process "slock")
  (message "Welcome back"))

;;; suspend

;;;###autoload
(defun my-suspend-system ()
  "Suspend the system."
  (interactive)
  (call-process "systemctl" nil nil nil "suspend"))

;;; touchpad

;;;###autoload
(defun my-touchpad-toggle ()
  "Enable or disable touchpad."
  (interactive)
  (call-process "synclient" nil nil nil
                (format "TouchpadOff=%d"
                         (with-temp-buffer
                          (call-process "synclient" nil t nil "-l")
                          (search-backward "TouchpadOff")
                          (end-of-line)
                          (- 1 (number-at-point))))))

;;; unclutter

(defcustom my-exwm-unclutter-timeout 3
  "Number of seconds before an inactive cursor is hidden.

Used if `unclutter' program is in PATH."
  :type '(choice (const :tag "Off" nil)
                 (integer :tag "Seconds"))
  :group 'my)

(defun my-exwm-unclutter ()
  "Run `unclutter'.

See `my-exwm-unclutter-timeout'."
  (interactive)
  (cl-assert my-exwm-unclutter-timeout)
  (make-process
   :name "unclutter"
   :command `("unclutter" "--timeout"
              ,(number-to-string my-exwm-unclutter-timeout))
   :noquery t))

(and my-exwm-unclutter-timeout
     (executable-find "unclutter")
     (add-hook 'exwm-init-hook #'my-exwm-unclutter))

;;; update-class hook

(defun my-exwm-update-buffer-name ()
  (exwm-workspace-rename-buffer exwm-class-name))

;; Part of `exwm-config-default'
(add-hook 'exwm-update-class-hook #'my-exwm-update-buffer-name)

;;; web browser

;;;###autoload
(defcustom my-web-browser-app
  (string-trim
   (shell-command-to-string "xdg-settings get default-web-browser"))
  "Web browser application."
  :type 'string
  :group 'my)

;;;###autoload
(defun my-web-browser-buffer-p (&optional buffer)
  "True if BUFFER is an Exwm buffer of the web browser.

BUFFER defaults to the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (eq (with-current-buffer buffer
        (my-exwm-find-buffer-by-app my-web-browser-app))
      buffer))

;;;###autoload
(defun my-web-browser-get-current-url ()
  "Get URL visited by the web browser in the current buffer."
  (when (and (my-web-browser-buffer-p)
             (string-match-p "^Firefox" exwm-class-name))
    (with-selected-window (get-buffer-window)
      (exwm-input--fake-key ?\C-l)    ; Focus location bar
      (sit-for 0)
      (exwm-input--fake-key ?\C-c)
      (sleep-for .2)
      (let ((url (current-kill 0 t)))
        (with-temp-buffer
          (insert url)
          (thing-at-point 'url))))))

;;; workspaces

;;;###autoload
(defun my-exwm-workspace-next (n)
  "Switch to the next N-th workspace."
  (interactive "p")
  (exwm-workspace-switch (mod (+ exwm-workspace-current-index n)
                              (exwm-workspace--count))))

;;;###autoload
(defun my-exwm-workspace-previous (n)
  "Switch to the previous N-th workspace."
  (interactive "p")
  (my-exwm-workspace-next (- n)))

;;;###autoload
(defun my-exwm-workspace-switch-or-next (&optional force-display-p)
  "Switch to another workspace interactively.

If there is only one workspace or a couple, switch immediately
without displaying the switching interface unless FORCE-DISPLAY-P
is non-nil."
  (interactive "P")
  (cl-case (and (not force-display-p) (exwm-workspace--count))
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
                  (throw 'reprompt '(toggle-prompt))))
               (rethrow-toggle-prompt-and-advance
                (lambda ()
                  (interactive)
                  (throw 'reprompt '(toggle-prompt . t))))
               (exwm-workspace--switch-map
                (let ((keymap-parent exwm-workspace--switch-map)
                      (keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap keymap-parent)
                  (define-key keymap (kbd "<tab>") rethrow-toggle-prompt)
                  (define-key keymap (kbd "M-`") #'previous-history-element)
                  (define-key keymap (kbd "n") #'previous-history-element)
                  (define-key keymap (kbd "p") #'next-history-element)
                  (define-key keymap (kbd "s-w") #'previous-history-element)
                  keymap))
               (minibuffer-local-must-match-map
                (let ((keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap minibuffer-local-must-match-map)
                  (define-key keymap (kbd "<tab>") rethrow-toggle-prompt)
                  (define-key keymap (kbd "M-`")
                    rethrow-toggle-prompt-and-advance)
                  keymap))
               (ivy-minibuffer-map
                (let ((keymap (make-sparse-keymap)))
                  (set-keymap-parent keymap ivy-minibuffer-map)
                  (define-key keymap (kbd "<tab>") rethrow-toggle-prompt)
                  (define-key keymap (kbd "M-`")
                    rethrow-toggle-prompt-and-advance)
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
            (`(toggle-prompt . ,advance-p)
             (my-exwm-workspace-switch-toggle-prompt-method)
             (when advance-p
               (exwm-workspace-switch (mod (+ exwm-workspace-current-index 1)
                                           (exwm-workspace--count)))))
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

;;; xim

(require 'exwm-xim)

(exwm-xim-enable)

(--each (where-is-internal 'toggle-input-method (current-global-map))
  (when (= (length it) 1)
    (add-to-list 'exwm-input-prefix-keys (elt it 0))))
