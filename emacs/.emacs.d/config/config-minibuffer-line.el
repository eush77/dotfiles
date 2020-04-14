(require 'battery)

(add-to-list 'package-selected-packages 'dash)
(add-to-list 'package-selected-packages 'dash-functional)
(package-install-selected-packages)
(require 'dash)
(autoload '-compose "dash-functional")

(minibuffer-line-mode 1)

(defun my-minibuffer-battery ()
  "Format battery status information for display in the
minibuffer line."
  (if (and battery-echo-area-format battery-status-function)
      (let ((status (funcall battery-status-function)))
        (if (string= (cdr (assq ?L status)) "BAT")
            (battery-format (concat "%L %B %p%% (%m min), ")
		            (funcall battery-status-function))
          ""))
    ""))

(defun my-minibuffer-clock ()
  "Formats the current time to include in the minibuffer line.

Uses `bash-fuzzy-clock' [1,2] if it's installed, or else defaults
to exact time.

\[1]: URL `https://sourceforge.net/projects/bashfuzzyclock'
\[2]: URL `https://aur.archlinux.org/packages/bash-fuzzy-clock'"
  (let ((help-echo (format-time-string "%c")))
    (propertize (condition-case nil
                    (string-trim-right
                     (car (process-lines "bash-fuzzy-clock")))
                  (file-error (format-time-string "%R")))
                'help-echo help-echo
                'mouse-face 'highlight)))

(defun my-format-minibuffer-battery (battery width)
  "Format minibuffer battery info to the given width."
  (if (< width (string-width battery))
      (substring battery (- width))
    battery))

(defun my-format-minibuffer-clock (clock width)
  "Format minibuffer clock to the given width."
  (when (stringp clock)
    (setq clock (split-string clock)))
  (let ((str (mapconcat #'identity clock " ")))
    (if (<= (length str) width)
        str
      (my-format-minibuffer-clock (cdr clock) width))))

(defun my-format-minibuffer-global-mode-string ()
  "Format `global-mode-string' for display in the minibuffer."
  (mapconcat #'identity
             (-remove #'string-empty-p
                      (-map (-compose #'string-trim #'format-mode-line)
                            global-mode-string))
             " "))

(defun my-mode-line-escape (mode-line)
  "Escape literal mode line according to `mode-line-format'."
  (replace-regexp-in-string "%" "%%" mode-line))

(custom-set-variables
 '(minibuffer-line-format
   '(:eval (let* ((globals (my-format-minibuffer-global-mode-string))
                  (battery (my-minibuffer-battery))
                  (clock (my-minibuffer-clock))
                  (frame-width
                   (apply #'min
                          (mapcar #'frame-text-cols
                                  (minibuffer-frame-list))))
                  (space-width
                   (max 2 (- frame-width
                             (string-width globals)
                             (string-width battery)
                             (string-width clock))))
                  (battery-width (max 0 (- frame-width
                                           (string-width globals)
                                           space-width
                                           (string-width clock))))
                  (battery
                   (my-format-minibuffer-battery battery
                                                 battery-width))
                  (clock-width (max 0 (- frame-width
                                         (string-width globals)
                                         space-width
                                         (string-width battery))))
                  (clock (my-format-minibuffer-clock clock
                                                     clock-width))
                  (space-width
                   (max 2 (- frame-width
                             (string-width globals)
                             (string-width battery)
                             (string-width clock))))
                  (globals-width (min (length globals)
                                      (- frame-width
                                         space-width
                                         (string-width battery)
                                         (string-width clock)))))
             (my-mode-line-escape
              (concat (substring globals 0 globals-width)
                      (make-string space-width ? )
                      battery
                      clock))))))

(defun my-keyboard-quit--minibuffer-line (func)
  "Update minibuffer line after quit."
  (condition-case nil (funcall func) (quit (minibuffer-line--update))))
(advice-add 'keyboard-quit :around #'my-keyboard-quit--minibuffer-line)

(defun my-minibuffer-line--auto-save-hook ()
  "Update minibuffer line after auto-saving."
  (run-at-time 0 nil (lambda ()
                       (message nil)
                       (minibuffer-line--update))))
(add-to-list 'auto-save-hook #'my-minibuffer-line--auto-save-hook)

(defun my-minibuffer-line-update--original-faces (func &rest args)
  "Don't replace any faces when formatting the mode line."
  (cl-letf* ((format-mode-line-function (symbol-function 'format-mode-line))
             ((symbol-function 'format-mode-line)
              (lambda (format &optional face)
                ;; Ignore FACE.
                (funcall format-mode-line-function format))))
    (apply func args)))
(advice-add 'minibuffer-line--update
            :around #'my-minibuffer-line-update--original-faces)
