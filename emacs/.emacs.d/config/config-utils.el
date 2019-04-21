;;; Editing

(autoload 'copy-from-above-command "misc" nil t)

(defun my-forward-duplicate-line ()
  "Duplicate current line forward, moving the cursor to the
second copy."
  (interactive)
  (forward-line 1)
  (beginning-of-line)
  (copy-from-above-command)
  (newline)
  (forward-line -1))

(defun my-backward-duplicate-line ()
  "Duplicate current line backward, moving the cursor to the
first copy."
  (interactive)
  (my-forward-duplicate-line)
  (forward-line -1))

;;;###autoload
(defun my-forward-duplicate-sexp-or-line ()
  "Duplicate current sexp forward, unless the point is at the
beginning of line, in which case duplicate the line forward."
  (interactive)
  (if (zerop (current-column))
      (my-forward-duplicate-line)
    (my-sp-forward-duplicate-sexp)))

;;;###autoload
(defun my-backward-duplicate-sexp-or-line ()
  "Duplicate current sexp backward, unless the point is at the
beginning of line, in which case duplicate the line backward."
  (interactive)
  (if (zerop (current-column))
      (my-backward-duplicate-line)
    (my-sp-backward-duplicate-sexp)))

;;;###autoload
(defcustom my-open-line-and-indent t
  "Non-nil indicates that `my-open-next-line' and
`my-open-previous-line' indent inserted lines."
  :type 'boolean
  :group 'my)

;;;###autoload
(defun my-open-next-line (count)
  "Insert COUNT empty lines after the line at point.

If `my-open-line-and-indent' is non-nil, indent the newly inserted
line according to the major mode (with
`indent-according-to-mode').

See URL `http://www.emacswiki.org/emacs/OpenNextLine'."
  (interactive "*p")
  (end-of-line)
  (open-line count)
  (next-line 1)

  (when my-open-line-and-indent
    (indent-according-to-mode)))

;;;###autoload
(defun my-open-previous-line (count)
  "Insert COUNT empty lines before the line at point.

If `my-open-line-and-indent' is non-nil, indent the newly inserted
line according to the major mode (with
`indent-according-to-mode').

See URL `http://www.emacswiki.org/emacs/OpenNextLine'."
  (interactive "*p")
  (beginning-of-line)
  (open-line count)
  (when my-open-line-and-indent
    (indent-according-to-mode)))

;;; Defun motion

;;;###autoload
(defun my-previous-defun ()
  "Move to the previous defun."
  (interactive)
  (forward-char)
  (beginning-of-defun 2))

;;;###autoload
(defun my-next-defun ()
  "Move to the next defun."
  (interactive)
  (forward-char)
  (beginning-of-defun)
  (beginning-of-defun -1))

;;; Finding files

;;;###autoload
(defcustom my-find-directories nil
  "Alist of directories to find with `my-find-directory'."
  :type '(alist :key-type directory :value-type (group (integer :tag "Minimum Depth")
                                                       (integer :tag "Maximum Depth")))
  :group 'my)

(defun my-unique-prefixes (lists)
  "Return an alist mapping each list in LISTS to its minimum
identifying prefix."
  (seq-mapcat (pcase-lambda (`(,head . ,group))
                (cond ((null head)
                       (cl-assert (= (length group) 1)
                                  t
                                  "The lists are not unique")
                       `((,(car group))))
                      ((= (length group) 1) `((,(car group) ,head)))
                      (t (mapcar (pcase-lambda (`(,list . ,prefix))
                                   `((,head . ,list) ,head . ,prefix))
                                 (my-unique-prefixes (mapcar #'cdr group))))))
              (seq-group-by #'car lists)))

;;;###autoload
(defun my-find-directory ()
  "Find a subdirectory of one of the directories in
`my-find-directories'."
  (interactive)
  (let* ((root-names
          (mapcar (pcase-lambda (`(,dirs . ,prefix-dirs))
                    (cons (mapconcat #'identity (reverse dirs) "/")
                          (mapconcat #'identity (reverse prefix-dirs) "/")))
                  (my-unique-prefixes
                   (mapcar (pcase-lambda (`(,directory . _))
                             (reverse (split-string
                                       (expand-file-name directory) "/")))
                           my-find-directories))))
         (subdirectories
         (with-temp-buffer
           (mapc (pcase-lambda (`(,directory ,min-depth ,max-depth))
                   (let ((directory (expand-file-name directory)))
                     ;; TODO: Group find invocations for different starting
                     ;; points by depth.
                     (call-process find-program nil t nil
                                   directory
                                   "-mindepth" (number-to-string min-depth)
                                   "-maxdepth" (number-to-string max-depth)
                                   "-type" "d"
                                   "(" "-name" ".[^.]*" "-prune"
                                   "-o" "-printf" "%H\t/%P\n" ")")))
                 my-find-directories)
           (mapcar (pcase-lambda (`(,root ,subdir))
                     (cons (directory-file-name (concat
                                                 (cdr (assoc root root-names))
                                                 subdir))
                           (concat root subdir)))
                   (seq-partition (split-string (buffer-string)
                                                "[\n\t]" t)
                                  2)))))
    (find-file (cdr (assoc (completing-read "Find directory: "
                                            subdirectories
                                            nil
                                            t)
                 subdirectories)))))

;;;###autoload
(defcustom my-find-next-skipped-extensions completion-ignored-extensions
  "List of extensions to skip over with `my-find-next-file' and
`my-find-previous-file'.

File names ending in any string in this list are skipped over,
unless `buffer-file-name' would be skipped too.")

(defun my-find-next-skipped-file-name-p (filename)
  "True if FILENAME should be skipped according to
`my-find-next-skipped-extensions'."
  (seq-some (lambda (ext) (string-suffix-p ext filename))
            my-find-next-skipped-extensions))

(defun my-next-file-name (n &optional verbose)
  "Get the name of the Nth next file in the directory containing
the currently visited file.

If VERBOSE, message current position in the directory."
  (unless buffer-file-name
    (user-error "No visited file"))
  (let* ((forward-p (>= n 0))
         (all-files (seq-filter #'file-regular-p
                                (directory-files
                                 (file-name-directory buffer-file-name))))
         (files (if (my-find-next-skipped-file-name-p buffer-file-name)
                    all-files
                  (seq-remove #'my-find-next-skipped-file-name-p all-files)))
         (current-next-files (member
                              (file-name-nondirectory buffer-file-name)
                              (if forward-p files (reverse files))))
         (next-files (nthcdr (abs n) current-next-files)))
    (when verbose
      (message "File %d / %d%s"
               (let ((position-next-files
                      (or next-files current-next-files)))
                 (if forward-p
                     (- (length files) (length position-next-files) -1)
                   (length position-next-files)))
               (length files)
               (if next-files ""
                 (propertize (format " (no %s file)"
                                     (if forward-p "next" "previous"))
                             'face 'error))))
    (car next-files)))

;;;###autoload
(defun my-find-next-file (&optional n)
  "Find Nth next file in the directory containing the currently
visited file."
  (interactive "p")
  (when-let ((next-file (my-next-file-name n t)))
    (find-file next-file)))

;;;###autoload
(defun my-find-previous-file (&optional n)
  "Find Nth previous file in the directory containing the
currently visited file."
  (interactive "p")
  (my-find-next-file (- n)))

(defun my-quote-text-string (str)
  "Quote STR if necessary so that it can be inserted into text."
  (if (string-match "^[[:alnum:]]+$" str)
      str
    (format "\"%s\"" (replace-regexp-in-string "\\(\\\\*\\)\"" "\\1\\1\\\\\""
                                               str))))

(ert-deftest my-quote-text-string ()
  (should (string= (my-quote-text-string "alpha") "alpha"))
  (should (string= (my-quote-text-string "alnum12") "alnum12"))
  (should (string= (my-quote-text-string "") "\"\""))
  (should (string= (my-quote-text-string "with spaces") "\"with spaces\""))
  (should (string= (my-quote-text-string "escape \"quotes\"")
                   "\"escape \\\"quotes\\\"\""))
  (should (string= (my-quote-text-string "escape \\\"quotes\\\"")
                   "\"escape \\\\\\\"quotes\\\\\\\"\"")))

;;;###autoload (autoload 'my-find-next-file-hydra/my-find-next-file "config-utils")
;;;###autoload (autoload 'my-find-next-file-hydra/my-find-previous-file "config-utils")
(eval `(defhydra my-find-next-file-hydra
         (:pre (unless (buffer-file-name)
                 (hydra-disable)
                 (user-error "No visited file")))
         "
File in %s(my-quote-text-string
           (file-name-nondirectory
            (directory-file-name
             (file-name-directory (buffer-file-name))))): "
         ("p" my-find-previous-file "previous")
         ("n" my-find-next-file "next")
         (,(if window-system "<return>" "RET") nil)))

;;; Frame switching

(add-to-list 'package-selected-packages 'dash)
(add-to-list 'package-selected-packages 'dash-functional)
(package-install-selected-packages)

(autoload '-compose "dash-functional")
(require 'dash)

;;;###autoload
(defun my-select-frame-by-buffer-names (&optional switch-p)
  "Interactively select a frame from names of its displayed buffers.

The frames ordered from the most recently used to the least
recently used and the most recently used one is preselected.

If SWITCH-P is non-nil, the selected frame is immediately
switched to. If there are only 2 frames total, the other frame is
switched to without prompt."
  (interactive (list t))
  (let* ((frame-list                     ; List of frames in MRU order
          (-distinct (--map (window-frame it)
                            (--mapcat (get-buffer-window-list it nil 'visible)
                                      (buffer-list)))))
         (frame-alist                    ; List of frames with labels
          (--map (cons (mapconcat (-compose #'buffer-name #'window-buffer)
                                  (window-list it)
                                  ", ")
                       it)
                 frame-list))
         (selected-frame
          (if (<= (length frame-alist) 2)
              (next-frame)
            (cdr (assoc (completing-read "Select frame: "
                                         frame-alist
                                         nil
                                         t
                                         nil
                                         nil
                                         ;; Select the other frame
                                         (caadr frame-alist))
                        frame-alist)))))
    (if switch-p
        (select-frame-set-input-focus selected-frame)
      selected-frame)))

;;; Lisp evaluation

;;;###autoload
(defun my-eval-sexp (replace-sexp)
  "Evaluate region if the region is active, otherwise evaluate last sexp.

If REPLACE-SEXP is not nil, replace the sexp with its value.
Otherwise print the value in the echo area."
  (interactive "P")
  (let ((region (if (use-region-p)
                    (sort (list (mark) (point)) #'<)
                  (let ((bounds (save-excursion
                                  (thing-at-point--beginning-of-sexp)
                                  (bounds-of-thing-at-point 'sexp))))
                    (list (car bounds) (cdr bounds)))))
        (output (if replace-sexp (current-buffer) t)))
    (when replace-sexp
      (goto-char (cadr region)))
    (apply #'eval-region (append region (list output)))
    (when replace-sexp
      (when (looking-back "\n")
        (delete-char -1))
      (let ((marker (point-marker)))
        (goto-char (car region))
        (apply #'delete-region region)
        (when (looking-at "\n")
          (delete-char 1))
        (goto-char marker)))))

;;; Rectangles

(defun my-rectangle-set-mark ()
  "Set rectangular mark."
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (rectangle-mark-mode 1)))

;;;###autoload (autoload 'my-hydra-rectangle/body "config-utils")
(defhydra my-hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                              :foreign-keys run
                              :hint nil
                              :post (deactivate-mark))
  "

^Mark         ^ | ^^Insert          ^^ | ^^Replace    ^^ | ^Kill Ring     ^ |
^-------------^ | ^^----------------^^ | ^^-----------^^ | ^--------------^ | ^---------^
_SPC_: set mark | _o__O_: open         | _d__D_: delete  | _M-w_: copy      | _q_: cancel
  _x_: exchange | _n__N_: number lines | _c__C_: clear   | _C-w_: kill      |
^             ^ | ^^                ^^ | _t__T_: type    | _C-y_: yank      |
"
  ("SPC" my-rectangle-set-mark)
  ("x" rectangle-exchange-point-and-mark)

  ("o" open-rectangle)
  ("O" open-rectangle :exit t)
  ("n" rectangle-number-lines)
  ("N" rectangle-number-lines :exit t)

  ("d" delete-rectangle)
  ("D" delete-rectangle :exit t)
  ("c" clear-rectangle)
  ("C" clear-rectangle :exit t)
  ("t" string-rectangle)
  ("T" string-rectangle :exit t)

  ("M-w" copy-rectangle-as-kill)
  ("C-w" kill-rectangle)
  ("C-y" yank-rectangle)

  ("q" nil)

  ("C-@" my-rectangle-set-mark)
  ("C-b" rectangle-backward-char)
  ("C-f" rectangle-forward-char)
  ("C-n" rectangle-next-line)
  ("C-p" rectangle-previous-line))

;;; Shell commands

(defvar my-pinned-shell-command nil
  "Command to execute with `my-shell-command-on-buffer'. If nil,
prompt for the command each time.")

(defvar my-shell-command-on-buffer-history nil
  "Command history of `my-shell-command-on-buffer'.")

(defun my-shell-command-on-buffer (command)
  "Run shell command on the current buffer as input. Use
`my-pinned-shell-command' if set, otherwise prompt for a command.

With `C-u' prefix argument, prompt even if
`my-pinned-shell-command' is set, and ask prompt to set entered
command as a new `my-pinned-shell-command'.

With `C-u C-u' prefix argument, just reset
`my-pinned-shell-command'."
  (interactive
   (list (cond ((equal current-prefix-arg '(16))
                (setq my-pinned-shell-command nil)
                (message "Pinned shell command is cleared")
                "true")
               ((and my-pinned-shell-command (not current-prefix-arg))
                my-pinned-shell-command)
               (t (let ((command (read-from-minibuffer
                                  "Shell command on buffer: " nil nil nil
                                  'my-shell-command-on-buffer-history)))
                    (when (and (equal current-prefix-arg '(4))
                               (y-or-n-p "Pin this command? "))
                      (setq my-pinned-shell-command command))
                    command)))))
  (shell-command-on-region (point-min) (point-max) command))

;;; Virtual desktops

;;;###autoload
(defun my-frame-wm-desktop (frame)
  "Get virtual desktop number of a frame running in a window
system, or nil."
  (when window-system
    (string-to-number
     (or (cadr
          (split-string
           ;; Run `xprop' from a local directory.
           (let ((default-directory "~"))
             (shell-command-to-string
              (format "xprop -id %s _NET_WM_DESKTOP"
                      (alist-get 'outer-window-id
                                 (frame-parameters frame)))))
           "="))
         ""))))

;;; Window sizing

;;;###autoload
(defcustom my-window-size-delta 1
  "Default delta for window sizing commands.

See `my-enlarge-window', `my-enlarge-window-horizontally', and
`my-shrink-window-horizontally'."
  :type 'integer
  :group 'my)

;;;###autoload
(defun my-enlarge-window (delta)
  "Like `enlarge-window', but defaults to `my-window-size-delta'
instead of 1."
  (interactive "P")
  (setq delta (or delta my-window-size-delta))
  (enlarge-window delta))

;;;###autoload
(defun my-shrink-window (delta)
  "Like `shrink-window', but defaults to `my-window-size-delta'
instead of 1."
  (interactive "P")
  (setq delta (or delta my-window-size-delta))
  (shrink-window delta))

;;;###autoload
(defun my-enlarge-window-horizontally (delta)
  "Like `enlarge-window-horizontally', but defaults to
`my-window-size-delta' instead of 1."
  (interactive "P")
  (setq delta (or delta my-window-size-delta))
  (enlarge-window-horizontally delta))

;;;###autoload
(defun my-shrink-window-horizontally (delta)
  "Like `shrink-window-horizontally', but defaults to
`my-window-size-delta' instead of 1."
  (interactive "P")
  (setq delta (or delta my-window-size-delta))
  (shrink-window-horizontally delta))

;;;###autoload (autoload 'my-hydra-window-resize/my-enlarge-window "config-utils")
;;;###autoload (autoload 'my-hydra-window-resize/my-shrink-window "config-utils")
;;;###autoload (autoload 'my-hydra-window-resize/my-enlarge-window-horizontally "config-utils")
;;;###autoload (autoload 'my-hydra-window-resize/my-shrink-window-horizontally "config-utils")
(eval `(defhydra my-hydra-window-resize ()
         "
Resize window
"
         ("^" my-enlarge-window "enlarge" :column "Vertically")
         ("v" my-shrink-window "shrink")
         ("{" my-shrink-window-horizontally "enlarge" :column "Horizontally")
         ("}" my-enlarge-window-horizontally "shrink")
         (,(if window-system "<return>" "RET") nil "cancel" :column "")
         ("q" nil nil)))

;;; Window splitting

;;;###autoload
(defun my-balanced-split-window-vertically ()
  "Split window vertically and maintain balance."
  (interactive)
  (let ((window-min-height window-safe-min-height))
    (split-window-vertically))
  (balance-windows))

;;;###autoload
(defun my-balanced-split-window-horizontally ()
  "Split window horizontally and maintain balance."
  (interactive)
  (let ((window-min-width window-safe-min-width))
    (split-window-horizontally))
  (balance-windows))

;;;###autoload
(defun my-balanced-delete-window ()
  "Delete current window and maintain balance."
  (interactive)
  (delete-window)
  (balance-windows))

;;; Window switching

;;;###autoload
(defun my-switch-to-mru-window ()
  "Switch to most recently used window."
  (interactive)
  (select-window (or (get-mru-window t t t) (minibuffer-window)))
  (select-frame-set-input-focus (window-frame (selected-window))))

;;; XDG Applications

(require 'counsel)

(defvar my-xdg-web-browser-app
  (let ((default-directory "~"))
    (string-trim (shell-command-to-string
                  "xdg-settings get default-web-browser")))
  "App name of the default XDG web browser.")

(defvar my-xdg-web-browser-class-name
  (let* ((desktop-file
          (cdr (assoc my-xdg-web-browser-app
                      (counsel-linux-apps-list-desktop-files))))
         (props (xdg-desktop-read-file desktop-file)))
    (or (gethash "StartupWMClass" props)
        (gethash "Name" props)))
  "X class name of the default XDG web browser.")

(defun my-xdg-web-browser-buffer ()
  "Get live Exwm buffer of the default XDG web browser, or nil."
  (--find (with-current-buffer it
            (and (derived-mode-p 'exwm-mode)
                 (string-equal exwm-class-name
                               my-xdg-web-browser-class-name)))
          (buffer-list)))

;;;###autoload
(defun my-xdg-web-browser ()
  "Launch or switch to the default XDG web browser."
  (interactive)
  (if-let ((buffer (my-xdg-web-browser-buffer)))
      (if-let ((window (get-buffer-window buffer t)))
          (select-window window)
        (pop-to-buffer-same-window buffer))
    (counsel-linux-app-action-default (cons nil my-xdg-web-browser-app))))
