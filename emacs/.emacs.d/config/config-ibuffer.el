;;; ibuffer-make-column-filename-and-process

(defun my-ibuffer-process-command (process)
  "Get command to stand for PROCESS in IBuffer."
  (let ((command (process-command process)))
    (if (and (string-equal (car command) shell-file-name)
             (string-equal (cadr command) shell-command-switch))
        (list (caddr command))
      command)))

(define-advice ibuffer-make-column-filename-and-process
    (:around (func buffer mark) my-process-command)
  "Include process command."
  (let ((string (funcall func buffer mark)))
    (if-let ((process (get-buffer-process buffer)))
        (propertize
         (replace-regexp-in-string
          "^([^)]*)"
          (with-output-to-string
            (princ (my-ibuffer-process-command process)))
          string)
         'font-lock-face 'italic
         'ibuffer-process process)
      string)))

;;; ibuffer-make-column-process

(define-advice ibuffer-make-column-process
    (:around (func buffer mark) my-process-command)
  "Include process command."
  (let ((string (funcall func buffer mark)))
    (if (string-empty-p string)
        ""
      (replace-regexp-in-string "^([^)]*)"
                                (with-output-to-string
                                  (princ (my-ibuffer-process-command
                                          (get-buffer-process buffer))))
                                string))))
