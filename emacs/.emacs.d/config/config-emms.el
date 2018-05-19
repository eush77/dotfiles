(emms-all)
(emms-default-players)
(emms-mode-line-disable)
(emms-playing-time-disable-display)

(with-eval-after-load "emms-playlist-mode"
  (define-key emms-playlist-mode-map (kbd "0") #'emms-volume-raise)
  (define-key emms-playlist-mode-map (kbd "9") #'emms-volume-lower)
  (define-key emms-playlist-mode-map (kbd "SPC") #'emms-pause))

(with-eval-after-load "emms-volume-pulse"
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
  (advice-add 'emms-volume--pulse-get-volume
              :before #'my-emms-volume-pulse-get-volume--select-sink))

;;
;; EMMS Hydra
;;

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
        (artist (when-let ((artist (emms-track-get track 'info-artist)))
                  (concat artist " ")))
        (title (emms-track-get track 'info-title))
        (tracknumber (when-let ((tracknumber
                                 (emms-track-get track 'info-tracknumber)))
                       (format "#%d of " (string-to-number tracknumber))))
        (album (emms-track-get track 'info-album))
        (year (when-let ((year (emms-track-get track 'info-year)))
                (format " (%s)" year))))
    (if title
        (let* ((left (concat
                      (propertize artist
                                  'face 'my-hydra-emms-hint-line-artist)
                      (propertize (concat "[" playing-time total-time "]")
                                  'face 'my-hydra-emms-hint-line-time)))
               (middle (propertize title
                                   'face 'my-hydra-emms-hint-line-title))
               (right (concat
                       tracknumber
                       (propertize album 'face 'my-hydra-emms-hint-line-album)
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
      (concat (propertize (concat "[" playing-time total-time "] ")
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
  (hydra-show-hint my-hydra-emms/hint 'my-hydra-emms))

(defvar my-hydra-emms/hint-update-timer nil
  "Timer for `my-hydra-emms-update-hint'.")

(defun my-hydra-emms-start-hint-update-timer ()
  "Set up `my-hydra-emms/hint-update-timer' to call
`my-hydra-emms-update-hint' every second."
  (cl-assert (null my-hydra-emms/hint-update-timer))
  (setq my-hydra-emms/hint-update-timer
        (run-at-time 0 1 #'my-hydra-emms-update-hint)))

(defun my-hydra-emms-stop-hint-update-timer ()
  "Stop `my-hydra-emms/hint-update-timer' previously started by
`my-hydra-emms-start-hint-update-timer'."
  (cl-assert my-hydra-emms/hint-update-timer)
  (cancel-timer my-hydra-emms/hint-update-timer)
  (setq my-hydra-emms/hint-update-timer nil))

(defun my-hydra-emms-add ()
  "Add files using `emms-add-dired' or `emms-add-directory-tree'
(with completion), depending on the mode of the buffer."
  (interactive)
  (message "%s" major-mode)
  (if (eq major-mode 'dired-mode)
      (emms-add-dired)
    (call-interactively #'emms-add-directory-tree)))

;;;###autoload (autoload 'my-hydra-emms/body "config-emms")
(defhydra my-hydra-emms (:body-pre (my-hydra-emms-start-hint-update-timer)
                         :before-exit (my-hydra-emms-stop-hint-update-timer))
  "
%s`my-hydra-emms/hint-line
"
  ("SPC" emms-pause "pause" :column "Playback")
  ("n" emms-next "next")
  ("p" emms-previous "previous")
  ("0" emms-volume-raise "++" :column "Volume")
  ("9" emms-volume-lower "--")
  ("<" emms-seek-backward "backward" :column "Seek")
  (">" emms-seek-forward "forward")
  ("a" my-hydra-emms-add "add" :column "Playlist")
  ("e" emms "playlist" :exit t)
  ("q" nil "cancel" :column ""))
