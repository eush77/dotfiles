;;; set-visited-file-name

(defun my-set-visited-file-name--unset (args)
  "Unset visited file if FILENAME names an existing directory."
  (let ((filename (car args))
        (rest (cdr args)))
    (when (and filename
               (file-directory-p filename)
               (yes-or-no-p "Unset visited file? "))
      (setq filename nil))
    (cons filename rest)))

(advice-add 'set-visited-file-name
            :filter-args #'my-set-visited-file-name--unset)
