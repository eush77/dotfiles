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
  :type '(alist :key-type directory :value-type (integer :tag "Depth"))
  :group 'my)

;;;###autoload
(defun my-find-directory ()
  "Find a subdirectory of one of the directories in
`my-find-directories'."
  (interactive)
  (let ((subdirectories
         (with-temp-buffer
           (mapc (lambda (pair)
                   (let ((directory (expand-file-name (car pair)))
                         (depth (cdr pair)))
                     ;; TODO: Group find invocations for different starting
                     ;; points by depth.
                     (call-process find-program nil t nil
                                   directory
                                   "-mindepth" "1"
                                   "-maxdepth" (number-to-string depth)
                                   "-type" "d"
                                   "(" "-name" ".[^.]*" "-prune"
                                   "-o" "-printf" "%H\t%P\n" ")")))
                 my-find-directories)
           (mapcar (lambda (pair)
                     (seq-let (root subdir) pair
                       (cons subdir (expand-file-name subdir root))))
                   (seq-partition (split-string (buffer-string)
                                                "[\n\t]" t)
                                  2)))))
    (find-file (cdr (assoc (completing-read "Find directory: "
                                            subdirectories
                                            nil
                                            t)
                 subdirectories)))))

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

;;;###autoload
(defhydra my-hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                              :foreign-keys run
                              :post (deactivate-mark))
  "
"
  ("SPC" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)) "reset" :column "Mark")
  ("x" rectangle-exchange-point-and-mark "exchange")

  ("o" open-rectangle "open" :column "Insert")
  ("n" rectangle-number-lines "number lines")

  ("d" delete-rectangle "delete" :column "Replace")
  ("c" clear-rectangle "clear")
  ("t" string-rectangle "type")

  ("q" nil "cancel" :column "")

  ("C-b" rectangle-backward-char nil)
  ("C-f" rectangle-forward-char nil)
  ("C-n" rectangle-next-line nil)
  ("C-p" rectangle-previous-line nil)
  ("C-w" kill-rectangle nil)
  ("C-y" yank-rectangle nil)
  ("M-w" copy-rectangle-as-kill nil))

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
