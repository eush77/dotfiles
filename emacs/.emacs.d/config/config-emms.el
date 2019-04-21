(require 'emms-volume-pulse)

;;; Starting Up

(emms-all)
(emms-default-players)
(emms-mode-line-disable)
(emms-playing-time-disable-display)

;;; Adding Tracks

(defun my-emms-add-tracks-dwim ()
  "Add tracks at point or completed interactively.

Calls `emms-add-dired' or `emms-add-directory-tree'
(with completion), depending on the mode of the buffer."
  (declare (interactive-only t))
  (interactive)
  (if (eq major-mode 'dired-mode)
      (emms-add-dired)
    (call-interactively #'emms-add-directory-tree)))

;;; Playback Order

(defun my-emms-next--random-playlist (func &rest args)
  "`emms-next' or `emms-random' based on `emms-random-playlist'."
  (if emms-random-playlist
      (emms-random)
    (apply func args)))

(defun my-emms-previous--random-playlist ()
  "Signal an error if `emms-random-playlist' is on."
  (when emms-random-playlist
    (user-error "Can't go back in a random playback")))

(defun my-emms-random--random-playlist ()
  "Turn on `emms-random-playlist'."
  (unless emms-random-playlist
    (emms-toggle-random-playlist)))

(defun my-emms-toggle-looping ()
  "Toggle between looping states.

Switch between looping a single track, a playlist, or stopping at
the end of the playlist.

Resets `emms-random-playlist', modifies `emms-repeat-track'
and/or `emms-repeat-playlist'."
  (declare (interactive-only t))
  (interactive)
  (setq emms-random-playlist nil)
  (setq emms-player-next-function #'emms-next-noerror)
  (cond (emms-repeat-track (setq emms-repeat-track nil)
                           (setq emms-repeat-playlist t)
                           (message "Will repeat the playlist."))
        (emms-repeat-playlist (setq emms-repeat-playlist nil)
                              (message "Will stop at the end of the playlist."))
        (t (setq emms-repeat-track t)
           (message "Will repeat this track."))))

(defun my-emms-toggle-stop-after ()
  "Toggle between stopping after this track or continuing playing
the playlist."
  (declare (interactive-only t))
  (interactive)
  (cond ((not (eq emms-player-next-function 'emms-stop))
         (setq emms-player-next-function #'emms-stop)
         (message "Will stop after this track."))
        (emms-random-playlist
         (setq emms-player-next-function #'emms-random)
         (message "Will play the tracks randomly."))
        (t (setq emms-player-next-function #'emms-next-noerror)
           (message "Will play the track sequentially."))))

(advice-add 'emms-next :around #'my-emms-next--random-playlist)
(advice-add 'emms-previous :before #'my-emms-previous--random-playlist)
(advice-add 'emms-random :after #'my-emms-random--random-playlist)

;;; Playlist Mode

(defun my-emms-playlist-mode-goto-dired-at-point--default-directory ()
  "Visit `emms-source-file-default-directory' in Dired if there
is no track at point."
  (unless (emms-playlist-track-at)
    (dired emms-source-file-default-directory)))
(advice-add 'emms-playlist-mode-goto-dired-at-point
            :before-until
            #'my-emms-playlist-mode-goto-dired-at-point--default-directory)

(with-eval-after-load "emms-playlist-mode"
  (define-key emms-playlist-mode-map (kbd "0") #'emms-volume-raise)
  (define-key emms-playlist-mode-map (kbd "9") #'emms-volume-lower)
  (define-key emms-playlist-mode-map (kbd "SPC") #'emms-pause))

;;; PulseAudio

(defun my-emms-pulse-get-sink ()
  "Get active PulseAudio sink.

Active sink is defined as the last one in the list printed by
`pactl'."
  (with-temp-buffer
    (call-process "pactl" nil t nil "list" "short" "sinks")
    (cadr (split-string (buffer-substring (line-beginning-position 0)
                                          (point-max))))))

(defun my-emms-volume-pulse-get-volume--select-sink ()
  "Select the active `emms-volume-pulse-sink'."
  (setq emms-volume-pulse-sink (my-emms-pulse-get-sink)))

(with-eval-after-load "emms-volume-pulse"
  (advice-add 'emms-volume--pulse-get-volume
              :before #'my-emms-volume-pulse-get-volume--select-sink))

;;; Volume

(defun my-emms-volume-fine-lower ()
  "Like `emms-volume-lower', but with the unit decrement."
  (interactive)
  (let ((emms-volume-change-amount 1))
    (emms-volume-lower)))

(defun my-emms-volume-fine-raise ()
  "Like `emms-volume-raise', but with the unit increment."
  (interactive)
  (let ((emms-volume-change-amount 1))
    (emms-volume-raise)))

(defvar my-emms-volume-mute--last-volume nil
  "Last volume before muting the speaker.")

;;;###autoload
(defun my-emms-volume-mute ()
  "Toggle mute"
  (interactive)
  (let ((volume (emms-volume--pulse-get-volume)))
    (cond ((> volume 0)
           (setq my-emms-volume-mute--last-volume volume)
           (emms-volume-pulse-change (- volume)))
          (my-emms-volume-mute--last-volume
           (emms-volume-pulse-change my-emms-volume-mute--last-volume))
          (t (message "Cannot unmute")))))

;;; Hydra

(defvar my-hydra-emms/hint-line ""
  "First line of `my-hydra-emms/hint' showing track info.")

;;;###autoload
(defface my-hydra-emms-hint-line-artist
  '((t :bold t))
  "The face used to highlight artist in the
`my-hydra-emms/hint-line'."
  :group 'my)

;;;###autoload
(defface my-hydra-emms-hint-line-time
  '((t :foreground "Black" :bold t))
  "The face used to highlight playing time in the
`my-hydra-emms/hint-line'."
  :group 'my)

;;;###autoload
(defface my-hydra-emms-hint-line-title
  '((t :foreground "Green"))
  "The face used to highlight title in the
`my-hydra-emms/hint-line'."
  :group 'my)

;;;###autoload
(defface my-hydra-emms-hint-line-album
  '((t :foreground "Yellow"))
  "The face used to highlight album in the
`my-hydra-emms/hint-line'."
  :group 'my)

(defun my-hydra-emms-format-hint-line (track)
  "Format hint line for `my-hydra-emms/hint-line'."
  (let ((playing-time (format-seconds "%m:%02s" emms-playing-time))
        (total-time (when-let ((total-time
                                (emms-track-get track 'info-playing-time)))
                      (format-seconds "/%m:%02s" total-time)))
        (state (cond ((eq emms-player-next-function 'emms-stop) "Stop After")
                     (emms-random-playlist "Random")
                     (emms-repeat-track "Loop One")
                     (emms-repeat-playlist "Loop")))
        (artist (when-let ((artist (emms-track-get track 'info-artist)))
                  (concat artist " ")))
        (title (emms-track-get track 'info-title))
        (tracknumber (when-let ((tracknumber
                                 (emms-track-get track 'info-tracknumber)))
                       (format "#%d" (string-to-number tracknumber))))
        (album (emms-track-get track 'info-album))
        (year (when-let ((year (emms-track-get track 'info-year)))
                (format " (%s)" year))))
    (if title
        (let* ((left (concat
                      (propertize artist
                                  'face 'my-hydra-emms-hint-line-artist)
                      (propertize (concat "[" playing-time total-time "]"
                                          (and state (concat " " state)))
                                  'face 'my-hydra-emms-hint-line-time)))
               (middle (propertize title
                                   'face 'my-hydra-emms-hint-line-title))
               (right (concat
                       tracknumber
                       (when album
                         (concat " of "
                                 (propertize album
                                             'face
                                             'my-hydra-emms-hint-line-album)))
                       year))
               (space (- (frame-text-cols)
                         (length left)
                         (length middle)
                         (length right)))
               (left-space (/ space 2))
               (right-space (- space left-space)))
          (concat left
                  (make-string left-space ? )
                  middle
                  (make-string right-space ? )
                  right))
      ;; Fall back to simple description or file name.
      (concat (propertize (concat (and state (concat state " "))
                                  "[" playing-time total-time "] ")
                          'face 'my-hydra-emms-hint-line-time)
              (if (and (eq (emms-track-type track) 'file)
                       emms-source-file-default-directory)
                  (file-relative-name (emms-track-name track)
                                      emms-source-file-default-directory)
                (emms-track-simple-description track))))))

(defun my-hydra-emms-update-hint ()
  "Recompute `my-hydra-emms/hint-line' and update
`my-hydra-emms/hint'."
  (setq my-hydra-emms/hint-line (my-hydra-emms-format-hint-line
                                 (emms-playlist-current-selected-track)))
  (when my-hydra-emms/hint-update-timer
    (hydra-show-hint my-hydra-emms/hint 'my-hydra-emms)))

(defvar my-hydra-emms/hint-update-timer nil
  "Timer for `my-hydra-emms-update-hint'.")

(defun my-hydra-emms-start-hint-update-timer ()
  "Set up `my-hydra-emms/hint-update-timer' to call
`my-hydra-emms-update-hint' every second."
  (when my-hydra-emms/hint-update-timer
    (my-hydra-emms-stop-hint-update-timer))
  (setq my-hydra-emms/hint-update-timer
        (run-at-time 0 1 #'my-hydra-emms-update-hint)))

(defun my-hydra-emms-stop-hint-update-timer ()
  "Stop `my-hydra-emms/hint-update-timer' previously started by
`my-hydra-emms-start-hint-update-timer'."
  (when my-hydra-emms/hint-update-timer
    (cancel-timer my-hydra-emms/hint-update-timer)
    (setq my-hydra-emms/hint-update-timer nil)))

(advice-add 'my-emms-toggle-looping :after #'my-hydra-emms-update-hint)
(advice-add 'emms-toggle-random-playlist :after #'my-hydra-emms-update-hint)
(advice-add 'my-emms-toggle-stop-after :after #'my-hydra-emms-update-hint)

;;;###autoload (autoload 'my-hydra-emms/body "config-emms")
(defhydra my-hydra-emms (:body-pre (my-hydra-emms-start-hint-update-timer)
                         :before-exit (my-hydra-emms-stop-hint-update-timer))
  "
%s`my-hydra-emms/hint-line
"
  ("SPC" emms-pause "pause" :column "Playback")
  ("p" emms-previous "previous")
  ("n" emms-next "next")
  ("(" emms-volume-lower "--" :column "Volume Down")
  ("9" my-emms-volume-fine-lower "-")
  ("m" my-emms-volume-mute (if (zerop (emms-volume--pulse-get-volume))
                               "unmute"
                             "mute"))
  (")" emms-volume-raise "++" :column "Volume Up")
  ("0" my-emms-volume-fine-raise "+")
  ("<" emms-seek-backward "backward" :column "Seek")
  (">" emms-seek-forward "forward")
  ("l" my-emms-toggle-looping "looping" :column "Order")
  ("r" emms-toggle-random-playlist "random")
  ("s" my-emms-toggle-stop-after "stop after")
  ("a" my-emms-add-tracks-dwim "add" :column "Playlist")
  ("e" emms "playlist" :exit t)
  ("q" nil "cancel" :column ""))
