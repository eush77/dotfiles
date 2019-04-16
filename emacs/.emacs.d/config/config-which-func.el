(defun my-which-function-mode--header-line (&rest _)
  "Display current function in the header line."
  (setq header-line-format
        (and which-function-mode which-func-format)))

(advice-add 'which-function-mode
            :after #'my-which-function-mode--header-line)
