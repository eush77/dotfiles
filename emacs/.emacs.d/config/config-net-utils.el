(defvar my-ping-history '("google.com")
  "Host history for `my-ping'.")

(defun my-ping (host)
  "Ping HOST."
  (interactive
   (list (let ((machine-at-point (net-utils-machine-at-point)))
           (read-string "Ping host: "
                        (if (string-empty-p machine-at-point)
                            (car my-ping-history)
                          machine-at-point)
                        'my-ping-history))))
  (select-window
   (net-utils-run-simple (concat "*Ping " host "*")
                         ping-program
                         (append ping-program-options (list host)))))

(advice-add 'ping :override #'my-ping)
