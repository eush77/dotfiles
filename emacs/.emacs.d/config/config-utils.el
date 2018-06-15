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

;;;###autoload
(defun my-find-directory ()
  "Find a subdirectory of one of the directories in
`my-find-directories'."
  (interactive)
  (let ((subdirectories
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
                     (cons (directory-file-name (concat (file-name-nondirectory root)
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

(defun my-next-file-name (n &optional verbose)
  "Get the name of the Nth next file in the directory containing
the currently visited file.

If VERBOSE, message current position in the directory."
  (unless (buffer-file-name)
    (user-error "No visited file"))
  (let* ((forward-p (>= n 0))
         (files (seq-filter #'file-regular-p
                            (directory-files
                             (file-name-directory (buffer-file-name)))))
         (current-next-files (member
                              (file-name-nondirectory (buffer-file-name))
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
(defun my-select-frame ()
  "Interactively select a frame from names of buffers that are
open in it, with frames ordered from the most recently used to
the least recently used and the default selection pointing at the
most recently used other frame.

If there are only 2 frames open, switch to the other frame
immediately."
  (interactive)
  (let* ((frame-list                     ; List of frames in MRU order
          (-distinct (--map (window-frame it)
                            (--mapcat (get-buffer-window-list it nil 'visible)
                                      (buffer-list)))))
         (frame-alist                    ; List of frames with labels
          (--map (cons (mapconcat (-compose #'buffer-name #'window-buffer)
                                  (window-list it)
                                  ", ")
                       it)
                 frame-list)))
    (if (<= (length frame-alist) 2)
        (other-frame 1)
      (select-frame-set-input-focus
       (cdr (assoc (completing-read "Select frame: "
                                    frame-alist
                                    nil
                                    t
                                    nil
                                    nil
                                    ;; Select the other frame
                                    (caadr frame-alist))
                   frame-alist))))))

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

;;;###autoload
(defhydra my-hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                              :foreign-keys run
                              :post (deactivate-mark))
  "
"
  ("SPC" my-rectangle-set-mark "set mark" :column "Mark")
  ("x" rectangle-exchange-point-and-mark "exchange")

  ("o" open-rectangle "open" :column "Insert")
  ("n" rectangle-number-lines "number lines")

  ("d" delete-rectangle "delete" :column "Replace")
  ("c" clear-rectangle "clear")
  ("t" string-rectangle "type")

  ("M-w" copy-rectangle-as-kill "copy" :column "Kill Ring")
  ("C-w" kill-rectangle "kill")
  ("C-y" yank-rectangle "yank")

  ("q" nil "cancel" :column "")

  ("C-@" my-rectangle-set-mark nil)
  ("C-b" rectangle-backward-char nil)
  ("C-f" rectangle-forward-char nil)
  ("C-n" rectangle-next-line nil)
  ("C-p" rectangle-previous-line nil))

;;; Virtual desktops

;;;###autoload
(defun my-frame-wm-desktop (frame)
  "Get virtual desktop number of a frame running in a window
system, or nil."
  (when window-system
    (string-to-number
     (cadr
      (split-string
       (shell-command-to-string
        (format "xprop -id %s _NET_WM_DESKTOP"
                (alist-get 'outer-window-id
                           (frame-parameters frame))))
       "=")))))

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
