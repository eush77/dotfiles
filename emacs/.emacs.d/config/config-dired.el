(require 'dash)
(require 'dired-imenu)

;;; Basic Setup

(custom-set-variables
 '(dired-dwim-target t)
 '(dired-listing-switches
   (concat "-lv --group-directories-first --human-readable"
           (if (display-mouse-p) " --all" " --almost-all"))))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-hide-dotfiles-mode)
(add-hook 'dired-mode-hook #'toggle-truncate-lines)

;;; Commands

(defun my-dired-browse-file ()
  "Browse a file with `browse-url'."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (browse-url file)))

(defun my-dired-mouse-find-file (event)
  "Like `dired-mouse-find-file-other-window', but find file in
the same window."
  (interactive "e")
  (cl-letf (((symbol-function 'dired-other-window) #'dired)
            ((symbol-function 'find-file-other-window) #'find-file))
    (funcall #'dired-mouse-find-file-other-window event)))

(defun my-dired-other-window ()
  "Switch to other `dired-mode' window."
  (interactive)
  (if-let ((other-window
            (car (--filter (and (not (eq it (selected-window)))
                                (eq (buffer-local-value 'major-mode
                                                        (window-buffer it))
                                    'dired-mode))
                           (window-list)))))
      (select-window other-window)
    (user-error "No other Dired window")))

(defun my-dired-toggle-dwim-target ()
  "Toggle `dired-dwim-target'."
  (interactive)
  (setq dired-dwim-target (not dired-dwim-target))
  (message "Dired DWIM target %s"
           (if dired-dwim-target "enabled" "disabled")))

(defun my-dired-tmpdir ()
  "Create a temporary directory buffer.

The new directory will be deleted when the buffer is killed."
  (interactive)
  (with-current-buffer (dired (make-temp-file "" t))
    (add-hook 'kill-buffer-hook
              (lambda ()
                (unless (file-in-directory-p dired-directory
                                             temporary-file-directory)
                  (error "Not removing a directory not in %s"
                         temporary-file-directory))
                (delete-directory dired-directory t t))
              nil t)))

;;; dired-do-delete

(defun my-dired-do-delete--rmdir (&optional arg)
  "Remove empty directories without asking."
  (unless arg
    (pcase (dired-get-marked-files nil nil nil t)
      (`(,directory)
       (when (and (file-directory-p directory)
                  (equal (directory-files directory) '("." "..")))
         ;; Found an empty directory - delete it
         (delete-directory directory)
         (dired-revert)
         (message "Empty directory \"%s\" deleted"
                  (file-name-nondirectory directory))
         t)))))

(advice-add 'dired-do-delete :before-until #'my-dired-do-delete--rmdir)

;;; dired-filter

(add-hook 'dired-mode-hook #'dired-filter-mode)

(custom-set-variables '(dired-filter-stack nil))

(define-key dired-filter-map (kbd "w") #'dired-filter-pop)

;;; dired-guess-shell-command

(custom-set-variables
 '(dired-guess-shell-alist-user
   `((,(concat "\\." (regexp-opt '("avi" "m4v" "mkv" "mp4" "webm" "wmv")) "\\'")
      "mplayer;")
     (,(concat "\\." (regexp-opt '("doc" "docx" "odt" "xls" "xlsx")) "\\'")
      "libreoffice"))))

;;; find-file

;;;###autoload
(defcustom my-src-directory "~/src"
  "Directory with source files, with the structure mirroring
`my-build-directory'."
  :type 'directory
  :group 'my)

;;;###autoload
(defcustom my-build-directory "~/build"
  "Directory with build files, with the structure mirroring
`my-src-directory'."
  :type 'directory
  :group 'my)

(defun my-punct-delimited-prefixes (str)
  "Given a string STR, return a list of all its
punctuation-delimited prefixes, in order of decreasing length,
including the original string."
  (let ((prefixes)
        (end (string-match "[[:punct:]]" str)))
    (while end
      (push (substring str 0 end) prefixes)
      (setq end (string-match "[[:punct:]]" str (+ end 1))))
    (cons str prefixes)))

(defun my-dired-ff-expand-relative-name (file
                                         source-directory
                                         destination-directory)
  "Extract relative file name of FILE in SOURCE-DIRECTORY and
expand it in DESTINATION-DIRECTORY, returning a list of possible
alternative names."
  (let* ((filename (directory-file-name (file-relative-name file
                                                            source-directory)))
         (dirname (if (file-name-directory filename)
                      (expand-file-name (file-name-directory filename)
                                        destination-directory)
                    destination-directory)))
    (mapcar (lambda (basename)
              (file-name-as-directory (expand-file-name basename dirname)))
            (my-punct-delimited-prefixes (file-name-nondirectory filename)))))

(defun my-dired-ff-other-file (_)
  "`ff-other-file-alist' function for Dired."
  (cond ((file-in-directory-p default-directory my-build-directory)
         (my-dired-ff-expand-relative-name default-directory
                                           my-build-directory
                                           my-src-directory))
        ((file-in-directory-p default-directory my-src-directory)
         (my-dired-ff-expand-relative-name default-directory
                                           my-src-directory
                                           my-build-directory))))

(defun my-dired-ff-mode-hook ()
  (setq-local ff-other-file-alist '(("\\.none" my-dired-ff-other-file)))
  (setq-local ff-search-directories '("/")))

(add-hook 'dired-mode-hook #'my-dired-ff-mode-hook)

;;; htmlize

(define-advice dired-insert-set-properties
    (:after (begin end) my-htmlize-link)
  "Add `htmlize-link' text properties.

This is used by `htmlize' to insert html links to files."
  (save-excursion
    (goto-char begin)
    (while (< (point) end)
      (when-let ((filename-begin (dired-move-to-filename)))
        (let* ((filename-end (dired-move-to-end-of-filename))
               (filename (buffer-substring filename-begin filename-end))
               (directory-p (file-directory-p
                             (expand-file-name filename
                                               (if (consp dired-directory)
                                                   (car dired-directory)
                                                 dired-directory)))))
          (put-text-property filename-begin filename-end 'htmlize-link
                             (url-encode-url
                              (concat filename (and directory-p "/"))))))
      (forward-line 1))))

;;; dired-mode-map

(define-key dired-mode-map (kbd "C-M-n") #'window-jump-down)
(define-key dired-mode-map (kbd "C-M-p") #'window-jump-up)
(define-key dired-mode-map (kbd "C-x C-q") #'wdired-change-to-wdired-mode)

(define-key dired-mode-map (kbd ".") #'dired-hide-dotfiles-mode)
(define-key dired-mode-map (kbd "?") #'counsel-rg)
(define-key dired-mode-map [remap dired-diff] #'ediff-files)
(define-key dired-mode-map (kbd "SPC") #'dired-up-directory)
(define-key dired-mode-map (kbd "C-x C-y") #'my-dired-toggle-dwim-target)
(define-key dired-mode-map (kbd "M-p") #'dired-prev-subdir)
(define-key dired-mode-map (kbd "M-n") #'dired-next-subdir)
(define-key dired-mode-map (kbd "b") #'my-dired-browse-file)
(define-key dired-mode-map (kbd "c") #'dired-kill-subdir)
(define-key dired-mode-map (kbd "h") #'my-dired-other-window)
(define-key dired-mode-map (kbd "r") #'dired-do-query-replace-regexp)
(define-key dired-mode-map (kbd "T") #'my-dired-tmpdir)
(define-key dired-mode-map (kbd "x") nil)
(define-key dired-mode-map (kbd "X") #'dired-do-flagged-delete)
(define-key dired-mode-map (kbd "z") #'dired-hide-subdir)
(define-key dired-mode-map [mouse-2] #'my-dired-mouse-find-file)

(key-chord-define dired-mode-map "xw" #'ff-get-other-file)
