;;; Buffers

(defun my-quit-buffers-by-mode (mode)
  "Bury buffers by major MODE and quit their windows."
  (interactive
   (list (if current-prefix-arg
             (intern (completing-read
                      "Major mode: "
                      (cl-remove-duplicates
                       (seq-map (apply-partially #'buffer-local-value
                                                 'major-mode)
                                (buffer-list)))))
           major-mode)))
  (dolist (buffer (--filter (eq (buffer-local-value 'major-mode it) mode)
                            (buffer-list)))
    (quit-windows-on buffer)
    (bury-buffer buffer)))

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

;;; Dedicated Windows

(defun my-toggle-window-dedicated-p (&optional window)
  "Toggle dedicated state of the WINDOW.

If WINDOW is nil, use the selected window."
  (interactive)
  (let ((flag (not (window-dedicated-p window))))
    (set-window-dedicated-p window flag)
    (if flag
        (message "Window is strongly dedicated to %S"
                 (window-buffer window))
      (message "Window is not dedicated"))))

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
          (if (and switch-p (<= (length frame-alist) 2))
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

;;; Typography

;;;###autoload
(defcustom my-nbsp-patterns
  '((nil
     ("[[:digit:]]" . "[[:digit:]]")
     ("" . "—"))
    (ru
     ("\\bа")
     ("\\bбез")
     ("\\bбезо")
     ("\\bв")
     ("\\bво")
     ("\\bдля")
     ("\\bдо")
     ("\\bза")
     ("\\bтак" . "и\\b")                ; Before (1)
     ;; Match a word boundary to preserve `и' ending list items.
     ("\\s-и" . "\\b\\|\\[")            ; (1)
     ("\\bиз")
     ("\\bиз-за")
     ("\\bиз-под")
     ("\\bизо")
     ("\\bк")
     ("\\bко")
     ("\\bна")
     ("\\bнад")
     ("\\bне")
     ("\\bни")
     ("\\bо")
     ("\\bоб")
     ("\\bот")
     ("\\bото")
     ("\\bпо")
     ("\\bпод")
     ("\\bподо")
     ("\\bпри")
     ("\\bпро")
     ("\\bради")
     ("\\bс")
     ("\\bсо")
     ("\\bу")
     ("\\bсм\\.")
     ("\\bт\\." . "[а-я]\\.")
     ("" . "бы\\b")
     ("" . "же\\b")
     ("" . "ли\\b"))
    (en
     ("\\ba")
     ("\\ban")
     ("\\bas")
     ("\\bat")
     ("\\bby")
     ("\\bfor")
     ("\\bfrom")
     ("\\bin")
     ("\\bof")
     ("\\bon")
     ("\\bthe")
     ("\\bto")
     ("\\bwith")))
  "Patterns governing the use of non-breaking spaces.

A list of pairs (SYMBOL . REGEXPS) where SYMBOL is either an ISO
639-1 language code or nil (matching every language), and REGEXPS
is a list of (START . END) pairs of regular expressions. END
defaults to the empty string if nil. Whenever START and END match
around a sequence of whitespace-only characters, these characters
are to be replaced with a single non-breaking space.")

(defvar-local my-nbsp-local-patterns nil
  "Same as `my-nbsp-patterns', but buffer-local.")

(defvar-local my-nbsp-sequence nil
  "Non-breaking space sequence to use in the current buffer.")

(defun my-nbsp-get-sequence (&optional default)
  "Get nbsp sequence to use in the current buffer.

If DEFAULT is nil, return nil by default.
If DEFAULT is t, return the unicode string for nbsp.
If DEFAULT is the symbol `ask', read the string from minibuffer. "
  (interactive '(ask))
  (cond
   (my-nbsp-sequence my-nbsp-sequence)
   ((derived-mode-p 'html-mode) "&nbsp;")
   ((derived-mode-p 'tex-mode) "~")
   ((save-excursion (goto-char (point-min))
                    (re-search-forward " " nil t))
    " ")
   (t (cl-case default
        ((nil) nil)
        ((t) " ")
        (ask (read-string "Non-breaking space sequence: " " "))
        (otherwise (error "Unsupported value for DEFAULT"))))))

(defun my-nbsp-fix (langid nbsp &optional start end query-p)
  "Fix non-breaking spacing in the region of the current buffer.

LANGID is an ISO 639-1 language code. When called interactively,
it is guessed from the current buffer or the active region unless
called with a prefix argument, otherwise read from the
minibuffer.

NBSP is a string to insert for non-breaking space. When called
interactively, it is guessed from the contents of the buffer.

START and END specify the region to operate on. When called
interactively, they default to the bounds of the active region.

QUERY-P specifies whether to query the user for each match or
replace silently. `t' when called interactively."
  (interactive
   (pcase-let ((`((,start . ,end))
                (if (region-active-p)
                    (region-bounds)
                  (list (cons (point-min) (point-max))))))
     (list
      (cond (current-prefix-arg
             (intern (completing-read "Language: "
                                      guess-language-languages)))
            (t (require 'guess-language)
               (guess-language-region start end)))
      (call-interactively #'my-nbsp-get-sequence)
      start
      end
      t)))
  (save-excursion
    (perform-replace (mapconcat (pcase-lambda (`(,start . ,end))
                                  (format "\\(?1:%s\\)\\s-+\\(?2:%s\\)"
                                          start
                                          (or end "")))
                                (append (alist-get major-mode my-nbsp-patterns)
                                        (alist-get langid my-nbsp-patterns))
                                "\\|")
                     (concat "\\1" nbsp "\\2")
                     query-p
                     t
                     nil
                     nil
                     nil
                     start
                     end)))

(defun my-nbsp-fix-paragraph (langid nbsp &optional query-p)
  "Fix non-breaking spaces in the current paragraph.

LANGID is an ISO 639-1 language code. When called interactively,
it is guessed from the current paragraph.

NBSP is a string to insert for non-breaking space. When called
interactively, it is guessed from the contents of the buffer.

QUERY-P specifies whether to query the user for each match or
replace silently. `t' when called with a prefix argument."
  (interactive
   (progn (require 'guess-language)
          (list (guess-language-paragraph)
                (call-interactively #'my-nbsp-get-sequence)
                current-prefix-arg)))
  (save-excursion
    (forward-paragraph)
    (let ((end (point)))
      (backward-paragraph)
      (let ((start (point)))
        (my-nbsp-fix langid nbsp start end query-p)))))

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

(defun my-frame-wm-desktop (&optional frame)
  "Get _NET_WM_DESKTOP number of a FRAME or nil.

If FRAME is nil, use selected frame."
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

(defun my-active-frames ()
  "Get all active frames.

First frame in the list is always the selected frame.

Active frames are frames that should be used as targets for
`window-jump', `switch-window', etc.

- When using Exwm, a frame is active iff it has `exwm-active'
  frame parameter.

- When using other window managers, a frame is active iff it has
  the same non-nil _NET_WM_DESKTOP as the selected frame.

- Otherwise, a frame is active iff it is visible."
  (let ((frames
         (cons (selected-frame) (cl-remove (selected-frame) (frame-list)))))
    (cond ((boundp 'exwm-state)
           (--filter (frame-parameter it 'exwm-active) frames))
          (window-system
           ;; Avoid recomputing `selected-wm-desktop' for every frame.
           (let ((selected-wm-desktop (my-frame-wm-desktop)))
             (--filter (= (my-frame-wm-desktop it) selected-wm-desktop)
                       frames)))
          (t (-filter #'frame-visible-p frames)))))

;;;###autoload
(defun my-active-windows ()
  "Get all windows on all active frames.

See `my-active-frames'."
  (-mapcat #'window-list (my-active-frames)))

(defvar my-switch-window-order []
  "All active windows ordered by use time.

See `my-active-windows'.")

(defvar my-switch-window-index 0
  "Index of the current window in `my-switch-window-order'.")

(defun my-switch-window-reorder ()
  "Update `my-switch-window-order' and `my-switch-window-index'."
  (setq my-switch-window-order
        (vconcat (seq-sort-by #'window-use-time #'>
                              (my-active-windows))))
  (setq my-switch-window-index
        (seq-position my-switch-window-order (selected-window))))

(defun my-switch-window-next (n)
  "Switch to the next Nth window in `my-switch-window-order'."
  (interactive "p")
  (if (zerop n)
      (select-window
       (elt my-switch-window-order my-switch-window-index))
    (setq my-switch-window-index
          (mod (+ my-switch-window-index (cl-signum n))
               (length my-switch-window-order)))
    (when (elt my-switch-window-order my-switch-window-index)
      (setq n (- n (cl-signum n))))
    (my-switch-window-next n)))

(defun my-switch-window-nth (n)
  "Switch to N-th window in `my-switch-window-order'."
  (select-window (elt my-switch-window-order n)))

(defvar my-switch-window-overlays []
  "Window-number overlays for `my-switch-window-hydra/body'.")

;;;###autoload
(defface my-switch-window-current
  '((t :foreground "deep sky blue"))
  "Face for the current window."
  :group 'my)

;;;###autoload
(defface my-switch-window-other
  '((t :foreground "orange red"))
  "Face for other windows."
  :group 'my)

;;;###autoload
(defface my-switch-window-overlay-current
  '((t :inherit my-switch-window-current :height 4.0))
  "Face for the current-window overlay."
  :group 'my)

;;;###autoload
(defface my-switch-window-overlay-other
  '((t :inherit my-switch-window-other :height 4.0))
  "Face for `my-switch-window-overlays'."
  :group 'my)

(defun my-switch-window-create-overlay (window index)
  "Create overlay in WINDOW displaying NUMBER."
  (let ((overlay (make-overlay (window-point window)
                               (window-point window)
                               (window-buffer window))))
    (overlay-put overlay 'before-string
                 (propertize (number-to-string index) 'face
                             'my-switch-window-overlay-other))
    (overlay-put overlay 'window window)
    overlay))

(defun my-switch-window-create-overlays ()
  "Set `my-switch-window-overlays' for `my-switch-window-order'."
  (my-switch-window-delete-overlays)
  (setq my-switch-window-overlays
        (vconcat (seq-map-indexed #'my-switch-window-create-overlay
                                  my-switch-window-order))))

(defun my-switch-window-delete-overlays ()
  "Delete `my-switch-window-overlays'."
  (seq-each (lambda (overlay)
              (when overlay
                (delete-overlay overlay)))
            my-switch-window-overlays)
  (setq my-switch-window-overlays []))

(defun my-switch-window-set-overlay-face (face)
  "Set face of the overlay in the selected window."
  (when-let* ((overlay
               (elt my-switch-window-overlays my-switch-window-index))
              (before-string (overlay-get overlay 'before-string)))
    (overlay-put overlay 'before-string
                 (propertize before-string 'face face))))

(define-advice my-switch-window-next (:before (_) other-overlay)
  (my-switch-window-set-overlay-face 'my-switch-window-overlay-other))

(define-advice my-switch-window-next (:after (_) current-overlay)
  (my-switch-window-set-overlay-face 'my-switch-window-overlay-current))

(defun my-switch-window-delete-window ()
  "Delete selected window."
  (interactive)
  (let ((index my-switch-window-index)
        (window (selected-window)))
    (my-switch-window-next 1)
    (if (eq (selected-window) window)
        (message "No other window")
      (delete-overlay (elt my-switch-window-overlays index))
      (aset my-switch-window-order index nil)
      (aset my-switch-window-overlays index nil)
      (delete-window window))))

(defun my-switch-window-split-window (&optional side)
  "Split selected window along SIDE.

See `split-window' for the valid values of SIDE."
  (interactive)
  (let ((window (split-window nil nil side))
        (index (length my-switch-window-order)))
    (setq my-switch-window-order
          (vconcat my-switch-window-order (list window)))
    (setq my-switch-window-overlays
          (vconcat my-switch-window-overlays
                   (list (my-switch-window-create-overlay window index))))))

(defhydra my-switch-window-hydra (:hint nil
                                  :body-pre
                                  (progn
                                    (my-switch-window-reorder)
                                    (my-switch-window-create-overlays)
                                    (my-switch-window-next 1))
                                  :before-exit
                                  (my-switch-window-delete-overlays))
  ("M-<tab>" (my-switch-window-next 1) nil)
  ("M-<iso-lefttab>" (my-switch-window-next -1) nil)
  ("-" #'my-switch-window-delete-window nil)
  ("+" #'my-switch-window-split-window nil)
  ("|" (my-switch-window-split-window t) nil)
  ("<return>" nil))

(setq my-switch-window-hydra/hint
      '(let ((window-heads
              (seq-filter
               #'identity
               (seq-map-indexed
                (lambda (window index)
                  (when window
                    `(,(number-to-string index)
                      (lambda ()
                        (interactive)
                        (hydra-keyboard-quit)
                        (my-switch-window-nth ,index))
                      ,(propertize
                        (buffer-name (window-buffer window)) 'face
                        (if (eq window (selected-window))
                            'my-switch-window-current
                          'my-switch-window-other))
                      :exit t)))
                my-switch-window-order))))
         (dolist (index (number-sequence 0 9))
           (define-key my-switch-window-hydra/keymap (number-to-string index)
             (lambda ()
               (interactive)
               (message "No such window"))))
         (pcase-dolist (`(,key ,cmd) window-heads)
           (define-key my-switch-window-hydra/keymap key cmd))
         (eval (hydra--format nil nil "Switch window [-]"
                              (append my-switch-window-hydra/heads
                                      window-heads)))))

;;;###autoload
(defun my-switch-window (&optional force-display-p)
  "Switch to another active window interactively.

If there is only one active window or a couple, switch
immediately without displaying the switching interface unless
FORCE-DISPLAY-P is non-nil."
  (declare (interactive-only t))
  (interactive "P")
  (cl-case (and (not force-display-p)
                (length (my-active-windows)))
    (1 (message "No other window"))
    (2 (select-window (car (remq (selected-window)
                                 (my-active-windows)))))
    (otherwise (my-switch-window-hydra/body))))
