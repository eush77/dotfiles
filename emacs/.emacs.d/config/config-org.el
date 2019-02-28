;;; -*- lexical-binding: t; eval: (outline-minor-mode) -*-
(require 'org-depend)
(require 'subr-x)

;;; Agenda

(custom-set org-agenda-custom-commands
            `(("p" "Planned tasks"
               ((my-org-agenda-planned-view "Weekly.org")
                (my-org-agenda-planned-view "Monthly.org")
                (my-org-agenda-planned-view "Quarterly.org")
                (my-org-agenda-planned-view "Yearly.org")
                (my-org-agenda-planned-view "5 Years.org")
                (my-org-agenda-planned-view "Life.org"))
               ((org-agenda-prefix-format "  ")))))
(custom-set org-agenda-span 'fortnight)

(defun my-org-agenda-context-filter--set (symbol value)
  "Set VALUE as a context filter."
  (unless (cl-every (lambda (str) (string-match "^[+-]@" str)) value)
    (user-error "Invalid value for a context filter"))
  (set-default symbol value))

;;;###autoload
(defcustom my-org-agenda-context-filter nil
  "Context filter to apply in the `org-todo-list' agenda view.

See `org-agenda-tag-filter-preset'."
  :type '(repeat string)
  :set #'my-org-agenda-context-filter--set
  :group 'my)

(defun my-org-agenda-todo-keyword-filter--set (symbol value)
  "Set VALUE as a TODO keyword filter."
  (unless
      (seq-every-p
       (lambda (str) (string-match "^[+-][[:alnum:]]+$" str))
       value)
    (user-error "Invalid value for a TODO keyword filter"))
  (set-default symbol value))

;;;###autoload
(defcustom my-org-agenda-todo-keyword-filter nil
  "TODO keyword filter to apply in the `org-todo-list' agenda view.

See `org-agenda-regexp-filter-preset'."
  :type '(repeat string)
  :set #'my-org-agenda-todo-keyword-filter--set
  :group 'my)

(defun my-org-agenda-todo-keyword-filter-to-regexp-filter (filter)
  "Turn TODO keyword filter FILTER into a regexp filter.

See `org-agenda-regexp-filter-preset'."
  (mapcar (lambda (el)
            (string-match "^\\([+-]\\)\\(.*\\)$" el)
            (concat (match-string 1 el) "^" (match-string 2 el) " "))
          filter))

(defun my-org-todo-list--filters (func &rest args)
  "Apply context [1] and TODO keyword [2] filters in the
`org-todo-list' agenda view.

\[1]: `my-org-agenda-context-filter'
\[2]: `my-org-agenda-todo-keyword-filter'"
  (let ((org-agenda-tag-filter-preset
         (append my-org-agenda-context-filter
                 org-agenda-tag-filter-preset))
        (org-agenda-regexp-filter-preset
         (append (my-org-agenda-todo-keyword-filter-to-regexp-filter
                  my-org-agenda-todo-keyword-filter)
                 org-agenda-regexp-filter-preset)))
    (apply func args)))
(with-eval-after-load "org-agenda"
  (advice-add 'org-todo-list :around #'my-org-todo-list--filters))

;;;###autoload
(defcustom my-org-plan-directory "~/org/plan"
  "Directory with Org files for the planned agenda view.

See `my-org-agenda-planned-view'."
  :type 'directory
  :group 'my)

(defun my-org-agenda-planned-view (file-name)
  "Show all PLAN, PASS, or FAIL entries that have at least one PLAN sibling,
essentially compiling the list of currently planned items along
with the items completed in the current time period.

If FILE-NAME is not absolute, it is interpreted as relative to
`my-org-plan-directory'. "
  (org-compile-prefix-format t)
  (let* ((file-name (expand-file-name file-name my-org-plan-directory))
         (org-select-this-todo-keyword "PLAN|PASS|FAIL")
         ;; Force `breadcrumbs' property to be computed.
         (org-prefix-format-compiled
          (cons (append (car org-prefix-format-compiled)
                        '((org-prefix-has-breadcrumbs t)))
                (cdr org-prefix-format-compiled)))
         (all-entries (progn
                        (org-check-agenda-file file-name)
                        (org-agenda-get-day-entries
                         file-name
                         (calendar-gregorian-from-absolute (org-today))
                         :todo)))
         ;; Entries grouped by breadcrumbs.
         (all-entry-groups (seq-group-by (lambda (entry)
                                           (get-text-property 0
                                                              'breadcrumbs
                                                              entry))
                                         all-entries))
         ;; Groups of entries with at least one PLAN entry.
         (entry-groups
          (seq-filter (lambda (group)
                        (seq-find (lambda (entry)
                                    (string= "PLAN"
                                             (substring entry
                                                        (string-match
                                                         (get-text-property
                                                          0
                                                          'org-todo-regexp
                                                          entry)
                                                         entry)
                                                        (match-end 0))))
                                  (cdr group)))
                      all-entry-groups))
         (entries (seq-mapcat #'cdr entry-groups))
         (category (if entries
                       (get-text-property 0 'org-category (car entries))
                     (file-name-base file-name)))
         ;; Collect and format all breadcrumbs.
         (breadcrumbs
          (mapconcat (lambda (group)
                       (car (last (split-string (car group) "->" t))))
                     entry-groups
                     ", "))
         (header (org-add-props
                     (concat category
                             ":"
                             (make-string (max 1 (- (window-text-width)
                                                    (length category)
                                                    1 ; Colon
                                                    (length breadcrumbs)))
                                          ? )
                             breadcrumbs)
                     nil
                   'face 'org-agenda-structure)))
    (insert header
            "\n"
            (org-agenda-finalize-entries entries)
            "\n\n")))

;;; Archive

(custom-set org-archive-location "archive/%s::")

(defun my-org-save-archive-buffer ()
  "Save the live archive buffer for the current buffer."
  (interactive)
  (when-let ((buffer (get-file-buffer (org-extract-archive-file))))
    (with-current-buffer buffer
      (save-buffer))))

(defun my-org-add-save-archive-buffer-hook ()
  (add-hook 'after-save-hook #'my-org-save-archive-buffer nil t))

(add-hook 'org-mode-hook #'my-org-add-save-archive-buffer-hook)

;;; Capture

(custom-set org-capture-templates
            `(("n" "New item in the backlog" entry
               (file org-default-notes-file)
               ,(concat "* NEW %?\n"
                        ":LOGBOOK:\n"
                        "- State \"NEW\"        from              %U\n"
                        ":END:\n"))
              ("r" "New item in the backlog (quote region)" entry
               (file org-default-notes-file)
               ,(concat "* NEW %?\n"
                        ":LOGBOOK:\n"
                        "- State \"NEW\"        from              %U\n"
                        ":END:\n"
                        "Captured from %(pcase
                            (with-current-buffer
                                (org-capture-get :original-buffer) major-mode)
                          ('w3m-mode \"%:link\")
                          ('gnus-article-mode \"%:from\")
                          (_ \"%f\")):\n"
                        "#+BEGIN_QUOTE\n"
                        "%i\n"
                        "#+END_QUOTE\n"))
              ("l" "Store link in the backlog" entry
               (file org-default-notes-file)
               ,(concat "* NEW %a\n"
                        ":LOGBOOK:\n"
                        "- State \"NEW\"        from              %U\n"
                        ":END:\n"))))

;;; Clocking

;;;###autoload (autoload 'my-hydra-org-clock/body "config-org")
(defhydra my-hydra-org-clock ()
  "Org Clock"
  ("i" org-clock-in "clock-in" :exit t)
  ("x" org-clock-in-last "clock-in last" :exit t)
  ("p" org-pomodoro "pomodoro" :exit t)
  ("o" org-clock-out "clock-out" :exit t)
  ("c" org-clock-cancel "cancel clock" :exit t)
  ("j" org-clock-goto "goto clock" :exit t)
  ("q" nil "quit"))

;;; Column View

(custom-set org-columns-default-format
            "%32ITEM %TODO %1PRIORITY %4EFFORT{:} %4CLOCKSUM %CATEGORY %TAGS")
(custom-set org-columns-summary-types '(("!min" . my-org-timestamp-summarize-min)))

(defun my-org-timestamp-summarize-min (timestamps &optional format)
  "Summarize TIMESTAMPS by returning the minimum."
  (seq-reduce (lambda (left right)
                (cond ((string-empty-p left) right)
                      ((time-less-p (org-time-string-to-time left)
                                    (org-time-string-to-time right)) left)
                      (t right)))
              timestamps
              ""))

;;; Effort

(custom-set org-global-properties
            '(("EFFORT_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 6:00")))

;;; Export

(custom-set org-export-async-init-file
            (locate-user-emacs-file "ox-async-init.el"))

;;; ff-get-other-file

(defun my-locate-org-file (file)
  "Find FILE in `org-directory'.

If FILE is relative, it is expanded relative to `org-directory'.

If FILE is absolute and not in `org-directory', its root org
suffix is replaced with `org-directory'."
  (if (file-name-absolute-p file)
      (replace-regexp-in-string
       (concat ".*/"
               (regexp-quote (file-name-nondirectory
                              (directory-file-name org-directory)))
               "/")
       (file-name-as-directory org-directory)
       file
       t t)
    (expand-file-name file org-directory)))

(defun my-org-ff-other-file/archive (file)
  "Get the archive location for FILE, or the file from which
entries were archived into FILE most recently.

Returns either a one-element list or an empty list."
  (require 'org-archive)
  (when-let ((buffer (get-file-buffer file)))
    (with-current-buffer buffer
      (let ((archive-file (org-extract-archive-file)))
        (if (file-exists-p archive-file)
            (list archive-file)
          (save-excursion
            (goto-char (point-max))
            (when (search-backward-regexp
                   "^ *:ARCHIVE_FILE: *\\([^ ].*\\)$"
                   nil
                   t)
              (list (my-locate-org-file (match-string 1))))))))))

(defun my-org-ff-other-file/export (file)
  "Return the list of locations of files exported from FILE."
  (mapcar (lambda (ext)
            (concat (file-name-sans-extension file) ext))
          ;; The order of extensions matters for files exported to multiple
          ;; formats
          '(".tex" ".html" ".man" ".md" ".txt")))

(defun my-org-ff-other-file (file)
  "`ff-other-file-alist' function for Org files."
  (append (my-org-ff-other-file/archive file)
          (my-org-ff-other-file/export file)))

(defun my-org-ff-other-file-setup ()
  "Setup for `ff-get-other-file'."
  (setq-local ff-search-directories '("/"))
  (setq-local ff-other-file-alist
              '(("\\.org\\'" my-org-ff-other-file))))
(add-hook 'org-mode-hook #'my-org-ff-other-file-setup)

;;; Finding Files

;;;###autoload
(defcustom my-org-notes-directory "~/notes"
  "Directory with textual notes."
  :type 'directory
  :group 'my)

(defun my-org-jump-less-p (left right)
  "Sorting function for `my-org-jump' and `my-org-notes-jump'.

Top-level files are ordered before nested files. Two top-level
files are ordered alphabetically."
  (unless (string-match "/" left)
    (or (string-match "/" right)
        (< (elt left 0) (elt right 0)))))

;;;###autoload
(defun my-org-jump (&optional directory)
  "Jump to a file in DIRECTORY, which defaults to
`org-directory'."
  (interactive (list org-directory))
  (cl-letf* ((ivy-read-function (symbol-function 'ivy-read))
             ((symbol-function 'ivy-read)
              (lambda (prompt collection &rest args)
                (apply ivy-read-function prompt collection
                       :sort t args)))
             (ivy-sort-functions-alist '((t . my-org-jump-less-p))))
    (counsel-file-jump nil directory)))

;;;###autoload
(defun my-org-notes-jump ()
  "Jump to a file in `my-org-notes-directory'."
  (interactive)
  (my-org-jump my-org-notes-directory))

;;; Formatting

(add-hook 'org-mode-hook #'auto-fill-mode)

(custom-set org-startup-indented t)

(defun my-window-text-width--org-tty (width)
  "Decrement text width when displaying on a terminal in Org
Agenda mode.

By default, `org-agenda-align-tags' puts last characters of
right-aligned strings in the rightmost window column which is the
margin column on a terminal display, and thus the strings get
truncated."
  (if (and (not window-system)
           (eq major-mode 'org-agenda-mode))
      (- width 1)
    width))
(advice-add 'window-text-width
            :filter-return #'my-window-text-width--org-tty)

;;; LaTeX Export

(defun my-org-latex-plain-text--tilde-nbsp (func text info)
  "Use `~' for non-breaking space."
  (mapconcat (lambda (text) (funcall func text info))
             (split-string text "~")
             "~"))
(advice-add 'org-latex-plain-text
            :around #'my-org-latex-plain-text--tilde-nbsp)

(defun my-insert-tilde ()
  "Insert \"~\" at point."
  (interactive)
  (insert "~"))

;;; Lists

(custom-set org-list-allow-alphabetical t)

;;; Logbook

(custom-set org-log-done 'time)
(custom-set org-log-into-drawer t)
(custom-set org-log-refile 'time)

;;; Priority

(custom-set org-default-priority ?C)
(custom-set org-lowest-priority ?D)

;;; Refiling

(custom-set org-refile-targets '((org-files-list :maxlevel . 2)))
(custom-set org-refile-allow-creating-parent-nodes t)
(custom-set org-refile-use-outline-path 'file)
(custom-set org-outline-path-complete-in-steps nil)

(defun my-org-remove-new-keywords ()
  "Remove NEW keyword from entries after refiling.
This may silently modify any entry from the point onward."
  (org-back-to-heading t)
  (perform-replace (concat "^\\(" org-outline-regexp "\\)NEW ")
                   "\\1" nil t nil))

(add-hook 'org-after-refile-insert-hook #'my-org-remove-new-keywords)

;;; Structure Editing

(defun my-org-shiftmetaleft--region (func &rest args)
  "Promote headings in the active region."
  (if (region-active-p)
      (org-map-entries #'org-promote nil 'region)
    (apply func args)))
(advice-add 'org-shiftmetaleft :around #'my-org-shiftmetaleft--region)

(defun my-org-shiftmetaright--region (func &rest args)
  "Demote headings in the active region."
  (if (region-active-p)
      (org-map-entries #'org-demote nil 'region)
    (apply func args)))
(advice-add 'org-shiftmetaright :around #'my-org-shiftmetaright--region)

;;; Structure Templates

(defun my-org-insert-structure-template--upcase (func &rest args)
  "Convert block structure in upper case."
  (let ((begin (point))
        (end (copy-marker (+ (point) 1))))
    (apply func args)
    (upcase-region begin (marker-position end))))
(advice-add 'org-insert-structure-template
            :around #'my-org-insert-structure-template--upcase)

;;; Todo

(custom-set org-todo-keywords
            '((sequence "TODO(!)" "WAIT(@)" "NEXT(!)" "|" "DONE(!)" "DROP(@)")
              (sequence "PLAN" "|" "PASS" "FAIL")))
(custom-set org-todo-keyword-faces
            '(("PLAN" . "yellow")
              ("PASS" . "green")
              ("FAIL" . "red")))
(custom-set org-enforce-todo-checkbox-dependencies t)
(custom-set org-enforce-todo-dependencies t)

(defun my-org-drop-headings (end)
  "Drop headings from point to END."
  (when (< (point) end)
    (when (not (member (org-get-todo-state) org-done-keywords))
      (org-todo "DROP"))
    (org-next-visible-heading 1)
    (my-org-drop-headings end)))

(defun my-org-drop-subtree ()
  "Drop subtree at point, circumventing any state blocking."
  (interactive)
  (let ((subtree-end (save-excursion (org-end-of-subtree)
                                     (point)))
        (org-blocker-hook nil))
    (save-excursion (my-org-drop-headings subtree-end))
    (outline-hide-subtree)))

(defun my-org-todo--circumventing-blocking (func arg)
  "Use completion to determine the new state when circumventing
state blocking with a `\\[universal-argument] \\[universal-argument] \\[universal-argument]'.

If the new state is `DROP', drop the whole subtree."
  (if (equal arg '(64))
      (let ((org-blocker-hook nil))
        (funcall func '(4))
        (when (string= (org-get-todo-state) "DROP")
          (my-org-drop-subtree)))
    (funcall func arg)))
(advice-add 'org-todo
            :around #'my-org-todo--circumventing-blocking)

(defun my-org-todo--skip-wait (func arg)
  "Skip `WAIT' state when cycling through."
  (if-let ((next-state
            (cl-case arg
              ((nil 'right) (cadr (member "WAIT"
                                    (append org-todo-keywords-1
                                            '("")))))
              ('left (cadr (member "WAIT"
                                   (reverse
                                    (cons "" org-todo-keywords-1))))))))
      (let ((org-todo-get-default-hook
             (lambda (new-state _)
               (when (string= new-state "WAIT")
                 next-state))))
        (funcall func arg))
    (funcall func arg)))
(advice-add 'org-todo :around #'my-org-todo--skip-wait)

;;; Visibility

(custom-set org-cycle-global-at-bob t)

(defun my-org-cycle (arg)
  "With single prefix argument ARG, call
`my-outline-cycle-entry'. Otherwise call `org-cycle'."
  (interactive "P")
  (if (equal arg '(4))
      (my-outline-cycle-entry)
    (org-cycle arg)))

;;; Keymap

(key-chord-define org-mode-map "xw" #'ff-get-other-file)

(define-key org-mode-map (kbd "C-`") #'my-insert-tilde)
(define-key org-mode-map (kbd "C-c C-\\") #'org-toggle-link-display)
(define-key org-mode-map (kbd "C-c C-x C-p") #'org-pomodoro)
(define-key org-mode-map (kbd "C-c j") #'org-shiftmetaleft)
(define-key org-mode-map (kbd "C-c k") #'org-shiftmetaright)
(define-key org-mode-map (kbd "C-c n") #'org-metadown)
(define-key org-mode-map (kbd "C-c p") #'org-metaup)
(define-key org-mode-map (kbd "C-c M-,") #'org-insert-structure-template)
(define-key org-mode-map (kbd "C-M-b") #'org-previous-link)
(define-key org-mode-map (kbd "C-M-f") #'org-next-link)
(define-key org-mode-map (kbd "C-x 8 e") #'counsel-org-entity)
(define-key org-mode-map (kbd "M-N") #'my-outline-show-next-subtree)
(define-key org-mode-map (kbd "M-P") #'my-outline-show-previous-subtree)
(define-key org-mode-map (kbd "<tab>") #'my-org-cycle)
(define-key org-mode-map [remap org-goto] #'my-counsel-org-goto)

(defhydra my-org-mark-subtree-hydra ()
  "Mark subtree"
  ("@" org-mark-subtree))
(define-key org-mode-map [remap org-mark-subtree]
  #'my-org-mark-subtree-hydra/org-mark-subtree)

(with-eval-after-load "org-agenda"
  (define-key org-agenda-mode-map (kbd "C-c C-j")
    #'counsel-org-agenda-headlines))

(with-eval-after-load "ox-beamer"
  (define-key org-beamer-mode-map (kbd "C-c C-b") nil)
  (define-key org-beamer-mode-map (kbd "C-c C-x B")
    #'org-beamer-select-environment))
