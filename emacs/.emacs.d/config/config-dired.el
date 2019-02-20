(require 'dash)
(require 'dired-imenu)

;;; Basic setup

(custom-set dired-dwim-target t)
(custom-set dired-listing-switches
            (concat "-lv --group-directories-first --human-readable"
                    (if window-system " --all" " --almost-all")))

(add-hook 'dired-mode-hook #'dired-filter-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-hide-dotfiles-mode)

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

;;; `dired-filter'

(custom-set dired-filter-stack nil)

(define-key dired-filter-map (kbd "w") #'dired-filter-pop)

;;; Find-file

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
         (dirname (expand-file-name (file-name-directory filename)
                                    destination-directory)))
    (mapcar (lambda (basename) (expand-file-name basename dirname))
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

;;; Keymap

(define-key dired-mode-map (kbd "C-M-p") #'window-jump-up)
(define-key dired-mode-map (kbd "C-M-n") #'window-jump-down)

(define-key dired-mode-map (kbd ".") #'dired-hide-dotfiles-mode)
(define-key dired-mode-map (kbd "SPC") #'dired-up-directory)
(define-key dired-mode-map (kbd "C-x C-y") #'my-dired-toggle-dwim-target)
(define-key dired-mode-map (kbd "M-p") #'dired-prev-subdir)
(define-key dired-mode-map (kbd "M-n") #'dired-next-subdir)
(define-key dired-mode-map (kbd "b") #'my-dired-browse-file)
(define-key dired-mode-map (kbd "c") #'dired-kill-subdir)
(define-key dired-mode-map (kbd "h") #'my-dired-other-window)
(define-key dired-mode-map (kbd "r") #'dired-do-query-replace-regexp)
(define-key dired-mode-map (kbd "x") nil)
(define-key dired-mode-map (kbd "X") #'dired-do-flagged-delete)
(define-key dired-mode-map (kbd "z") #'dired-hide-subdir)
(define-key dired-mode-map [mouse-2] #'my-dired-mouse-find-file)

(key-chord-define dired-mode-map "xw" #'ff-get-other-file)
