(emms-all)
(emms-default-players)

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
