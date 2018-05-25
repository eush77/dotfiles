(defun my-org-pomodoro-start--adjust-mode-line (func &rest args)
  "My adjustments to the `global-mode-string' when `org-pomodoro'
is running."
  ;; Put `org-mode-line-string' before `org-pomodoro-mode-line'.
  (when (memq 'org-mode-line-string global-mode-string)
    (delq 'org-mode-line-string global-mode-string)
    (push 'org-mode-line-string global-mode-string)))
(advice-add 'org-pomodoro-start
            :after #'my-org-pomodoro-start--adjust-mode-line)
