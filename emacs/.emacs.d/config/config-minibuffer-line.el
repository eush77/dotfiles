(minibuffer-line-mode 1)

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

(custom-set minibuffer-line-format
            '(:eval (let* ((globals (format-mode-line global-mode-string))
                           (clock (my-minibuffer-clock))
                           (space
                            (make-string
                             (- (apply #'min
                                       (mapcar #'frame-text-cols
                                               (minibuffer-frame-list)))
                                (string-width globals)
                                (string-width clock))
                             ? )))
                      (concat globals space clock))))

(defun keyboard-quit--minibuffer-line (func)
  "Restore minibuffer line after quit."
  (condition-case nil (funcall func) (quit (minibuffer-line--update))))
(advice-add 'keyboard-quit :around #'keyboard-quit--minibuffer-line)
