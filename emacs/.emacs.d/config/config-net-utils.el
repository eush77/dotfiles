(defun my-ping--run-simple (func &rest args)
  "Use `net-utils-run-simple'"
  (cl-letf (((symbol-function 'net-utils-run-program)
             (lambda (name _ program args)
               (net-utils-run-simple (concat "*" name "*")
                                     program
                                     args))))
    (apply func args)))
(advice-add 'ping :around #'my-ping--run-simple)
