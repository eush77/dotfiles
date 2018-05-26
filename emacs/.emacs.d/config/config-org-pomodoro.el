(defun my-org-pomodoro--clock-in-last (func &rest args)
  "Don't clock back break time if called with two prefix arguments,
unless `org-pomodoro-clock-break' is t."
  (cl-letf ((clock-in-last-function (symbol-function 'org-clock-in-last))
            ((symbol-function 'org-clock-in-last)
             (lambda (arg)
               (interactive "P")
               (funcall clock-in-last-function
                        (if (and (equal arg '(16))
                                 (not org-pomodoro-clock-break))
                            0
                          arg)))))
    (apply func args)))
(advice-add 'org-pomodoro
            :around #'my-org-pomodoro--clock-in-last)

(defun my-org-pomodoro-start--adjust-mode-line (func &rest args)
  "My adjustments to the `global-mode-string' when `org-pomodoro'
is running."
  ;; Put `org-mode-line-string' before `org-pomodoro-mode-line'.
  (when (memq 'org-mode-line-string global-mode-string)
    (delq 'org-mode-line-string global-mode-string)
    (push 'org-mode-line-string global-mode-string)))
(advice-add 'org-pomodoro-start
            :after #'my-org-pomodoro-start--adjust-mode-line)
