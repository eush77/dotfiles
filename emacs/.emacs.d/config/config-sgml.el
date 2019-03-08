;;; Commands

(defun my-html-browse-file ()
  (interactive)
  "Load the current file in a web browser."
  (if buffer-file-name
      (browse-url buffer-file-name)
    (user-error "Not in a file-visiting buffer")))

;;; html-mode-map

(define-key html-mode-map (kbd "C-c \\") #'my-html-browse-file)
(define-key html-mode-map (kbd "C-c i") #'html-list-item)
(define-key html-mode-map (kbd "C-c l") #'html-name-anchor)
(define-key html-mode-map (kbd "C-c o") #'html-ordered-list)
(define-key html-mode-map (kbd "C-c u") #'html-unordered-list)
