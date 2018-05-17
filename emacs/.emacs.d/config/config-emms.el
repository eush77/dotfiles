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

(defvar my-hydra-emms/hint-title ""
  "Track title for use in `my-hydra-emms/body' hint.")

(defvar my-hydra-emms/hint-time ""
  "Track playing time for use in `my-hydra-emms/body' hint.")

(defun my-hydra-emms-update-hint ()
  "Set up hint variables for `my-hydra-emms/body'."
  (interactive)
  (let ((description (emms-track-description
                      (emms-playlist-current-selected-track))))
    (setq my-hydra-emms/hint-title
          (if (and (file-name-absolute-p description)
                   emms-source-file-default-directory)
              (file-relative-name description
                                  emms-source-file-default-directory)
            description)))
  (let ((emms-playing-time-display-p t)
        (emms-playing-time-style 'time))
    (emms-playing-time-display)
    (setq my-hydra-emms/hint-time emms-playing-time-string))
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

(defun my-hydra-emms-next ()
  "Wrapper around `emms-next' for `my-hydra-emms/body'."
  (interactive)
  (emms-next)
  (my-hydra-emms-update-hint))

(defun my-hydra-emms-previous ()
  "Wrapper around `emms-previous' for `my-hydra-emms/body'."
  (interactive)
  (emms-previous)
  (my-hydra-emms-update-hint))

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
EMMS [%s`my-hydra-emms/hint-time] %s`my-hydra-emms/hint-title
"
  ("9" emms-volume-lower "volume <")
  ("0" emms-volume-raise "volume >")
  ("SPC" emms-pause "pause")
  ("n" my-hydra-emms-next "next")
  ("p" my-hydra-emms-previous "previous")
  ("a" my-hydra-emms-add "add")
  ("e" emms "playlist" :exit t)
  ("q" nil "cancel"))
