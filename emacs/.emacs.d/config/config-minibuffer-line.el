(minibuffer-line-mode 1)

(custom-set minibuffer-line-format
            '(:eval (let ((time-string (format-time-string "%R")))
                      (concat (make-string (- (frame-text-cols)
                                              (string-width time-string)
                                              1)
                                           ? )
                              time-string))))

(defun keyboard-quit--minibuffer-line (func)
  "Restore minibuffer line after quit."
  (condition-case nil (funcall func) (quit (minibuffer-line--update))))
(advice-add 'keyboard-quit :around #'keyboard-quit--minibuffer-line)
