(when (executable-find "direnv")
  (direnv-mode 1))

(custom-set-variables
 '(direnv-non-file-modes '(compilation-mode
                           dired-mode
                           eshell-mode
                           magit-status-mode)))

;;; Gdb

(defun gdb@my-direnv (func &rest args)
  "Disable Direnv output."
  (let ((tramp-remote-process-environment
         (append tramp-remote-process-environment
                 '("DIRENV_LOG_FORMAT= "))))
    (apply func args)))

(with-eval-after-load "gdb-mi"
  (advice-add 'gdb :around #'gdb@my-direnv))

;;; Eshell

(with-eval-after-load "eshell"
  (defun my-eshell--direnv-path-env (&rest args)
    "Update `eshell-path-env'."
    (unless (file-remote-p default-directory)
      (setq eshell-path-env (getenv "PATH"))))
  (advice-add 'eshell
              :after #'my-eshell--direnv-path-env)

  (defun my-direnv-update-directory-environment--eshell (&rest args)
    "Update `eshell-path-env'."
    (setq eshell-path-env (getenv "PATH")))
  (advice-add 'direnv-update-directory-environment
              :after #'my-direnv-update-directory-environment--eshell))

;;; Tramp

(defcustom my-direnv-enabled-hosts nil
  "List of remote hosts to use Direnv on.

Each host must have `direnv' executable accessible in the default
environment."
  :type '(repeat string)
  :group 'my)

(defun tramp-handle-start-file-process@my-direnv (args)
  "Enable Direnv for hosts in `my-direnv-enabled-hosts'."
  (pcase-let ((`(,name ,buffer . ,program) args))
    (if (and (stringp (car program))
             (not (--find (eq (second it) 'eshell-gather-process-output)
                          (backtrace-frames))))
        (with-parsed-tramp-file-name (expand-file-name default-directory) nil
          (if (member host my-direnv-enabled-hosts)
              `(,name
                ,buffer
                "direnv"
                "exec"
                ,localname
                ,@program)
            args))
      args)))

(with-eval-after-load "tramp-sh"
  (advice-add 'tramp-handle-start-file-process
              :filter-args #'tramp-handle-start-file-process@my-direnv))
