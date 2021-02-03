;; Folder Icons

(setf (alist-get "Channels" telega-folder-icons-alist
                 nil nil #'string-equal)
      "📰")
(setf (alist-get "Private" telega-folder-icons-alist
                 nil nil #'string-equal)
      "🫂")
(setf (alist-get "Work" telega-folder-icons-alist
                 nil nil #'string-equal)
      "🕴️")

;; Mode Line Format

(defun my-telega-mode-line-format ()
  "Format mode line for `telega'."
  (let ((mode-line-string
         (concat
          (when telega-use-tracking-for
            (telega-mode-line-tracking))
          (telega-mode-line-unread-unmuted)
          (telega-mode-line-mentions 'messages))))
    (unless (string-empty-p mode-line-string)
      (concat (telega-mode-line-icon) mode-line-string))))

(custom-set-variables
 '(telega-mode-line-string-format
   '((:eval (my-telega-mode-line-format)))))

;; WebPage Mode

(define-key telega-webpage-mode-map (kbd "j") #'scroll-up-line)
(define-key telega-webpage-mode-map (kbd "k") #'scroll-down-line)

;; Misc

(custom-set-variables
 '(telega-chat-folder-format "%I ")
 '(telega-open-file-function 'org-open-file))
