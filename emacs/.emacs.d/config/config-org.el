;;; -*- lexical-binding: t; eval: (outline-minor-mode) -*-
(require 'dash)
(require 'org-depend)
(require 'subr-x)

;;; Agenda

(defun my-org-agenda-get-batch-view-region ()
  "Get restriction region for the batch view.

Return the current subtree if the subtree is (partially) visible
or the whole buffer otherwise."
  (save-excursion
    (org-back-to-heading)
    (let* ((next-visible-heading
            (save-excursion (org-next-visible-heading 1) (point)))
           (end-of-subtree
            (save-excursion (org-end-of-subtree 1) (point)))
           (subtree-p (< next-visible-heading end-of-subtree)))
      (if subtree-p
          (cons (point) end-of-subtree)
        (cons (point-min) (point-max))))))

(defvar my-org-agenda-goto-hd-point nil
  "Heading to go to after creating an agenda buffer.")

(defun my-org-search-view--go-to-heading (&rest _)
  "Go to heading at `my-org-agenda-goto-hd-point'."
  (when my-org-agenda-goto-hd-point
    (org-agenda-next-item 1)
    (while (/= (org-get-at-bol 'org-hd-marker)
               my-org-agenda-goto-hd-point)
      (org-agenda-next-item 1))))
(advice-add 'org-search-view
            :after #'my-org-search-view--go-to-heading)

(custom-set org-agenda-custom-commands
            `(("b" "Batch view" search "{..}"
               ((org-agenda-restrict
                 (progn (put 'org-agenda-files 'org-restrict
                             (list (buffer-file-name (buffer-base-buffer))))
                        (current-buffer)))
                (org-agenda-restrict-begin
                 (car (my-org-agenda-get-batch-view-region)))
                (org-agenda-restrict-end
                 (cdr (my-org-agenda-get-batch-view-region)))
                (my-org-agenda-goto-hd-point
                 (save-excursion (org-back-to-heading) (point)))))
              ("n" "Next actions" todo "NEXT")
              ("p" "Planned tasks"
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
  (require 'org-archive)
  (when-let ((buffer (get-file-buffer (org-extract-archive-file))))
    (with-current-buffer buffer
      (save-buffer))))

(defun my-org-add-save-archive-buffer-hook ()
  (add-hook 'after-save-hook #'my-org-save-archive-buffer nil t))

(add-hook 'org-mode-hook #'my-org-add-save-archive-buffer-hook)

(defun my-org-archive-all-matches--highlight-line (func &rest args)
  "Highlight line with the current match."
  (require 'hl-line)
  (cl-letf* ((hl-line-mode t)
             (y-or-n-p-function (symbol-function 'y-or-n-p))
             ((symbol-function 'y-or-n-p)
              (lambda (prompt)
                (hl-line-highlight)
                (condition-case err (funcall y-or-n-p-function prompt)
                  ((error quit)
                   (hl-line-unhighlight)
                   (signal (car err) (cdr err)))))))
    (apply func args)))

(advice-add 'org-archive-all-matches
            :around #'my-org-archive-all-matches--highlight-line)

;;; Capture

(defun my-w3m-anchor-text ()
  "Get anchor anchor at point."
  (when (w3m-anchor)
    (let ((begin (previous-single-property-change (+ (point) 1) 'face))
          (end (next-single-property-change (point) 'face)))
      (buffer-substring-no-properties begin end ))))

(defun my-org-capture-region (%f %i %U %:from %:link %:subject)
  "Quote active region"
  (let* ((magit-id (and (eq major-mode 'magit-revision-mode)
                        (save-excursion (goto-char (point-min))
                                        (thing-at-point 'word))))
         (from (pcase major-mode
                 ('w3m-mode %:link)
                 ('gnus-article-mode
                  (format "\"%s\" (by %s)" %:subject %:from))
                 ('magit-revision-mode
                  (magit-rev-format "\"%s\" (%h by %an)" magit-id))
                 (_ (if (string-empty-p %f) (buffer-name) %f))))
         (subject (pcase major-mode
                    ('w3m-mode w3m-current-title)
                    ('gnus-article-mode %:subject)
                    ('magit-revision-mode (magit-rev-format "%s" magit-id)))))
    (concat "* NEW %?" subject "\n"
            ":LOGBOOK:\n"
            "- State \"NEW\"        from              " %U "\n"
            ":END:\n"
            (with-temp-buffer
              (insert "Captured from " from ":\n")
              (fill-region (point-min) (point-max))
              (buffer-string))
            "#+BEGIN_QUOTE\n"
            (string-trim %i) "\n"
            "#+END_QUOTE\n")))

(defun my-org-capture-link (%U)
  "Store link at point / in the active region"
  (if (region-active-p)
      (let ((headings))
        (save-excursion
          (deactivate-mark)
          (save-restriction
            (narrow-to-region (mark) (point))
            (goto-char (point-min))
            (when (w3m-anchor)
              (save-restriction
                (widen)
                (push (my-org-capture-link) headings)))
            (while (w3m-goto-next-anchor)
              (save-restriction
                (widen)
                (push (my-org-capture-link) headings)))))
        (mapconcat #'identity headings ""))
    (let ((link (or (w3m-anchor)
                    (thing-at-point 'url)
                    (buffer-local-value
                     'w3m-current-url
                     (w3m-select-buffer-current-buffer))))
          (description (or (my-w3m-anchor-text)
                           (thing-at-point 'url)
                           (w3m-buffer-title
                            (w3m-select-buffer-current-buffer)))))
      (concat "* NEW " (org-make-link-string link description) "\n"
              ":LOGBOOK:\n"
              "- State \"NEW\"        from              " %U "\n"
              ":END:\n"))))

(defun my-org-capture-link-context ()
  "Context for `my-org-capture-link' template."
  (if (region-active-p)
      (eq major-mode 'w3m-mode)
    (or (eq major-mode 'w3m-select-buffer-mode)
        (w3m-anchor)
        (thing-at-point 'url))))

(custom-set org-capture-templates
            `(("n" "New item" entry
               (file org-default-notes-file)
               ,(concat "* NEW %?\n"
                        ":LOGBOOK:\n"
                        "- State \"NEW\"        from              %U\n"
                        ":END:\n"))
              ("r" ,(documentation 'my-org-capture-region) entry
               (file org-default-notes-file)
                "%(with-current-buffer (org-capture-get :original-buffer)
                   (my-org-capture-region
                    \"%f\" \"%i\" \"%U\" \"%:from\" \"%:link\" \"%:subject\"))")
              ("c" "Store link to the current buffer" entry
               (file org-default-notes-file)
               ,(concat "* NEW [[%(pcase
                            (with-current-buffer
                                (org-capture-get :original-buffer) major-mode)
                             ((or 'w3m-mode 'gnus-article-mode) \"%:link\")
                             (_ \"file:%F\")
                           )][%f%:description]]\n"
                        ":LOGBOOK:\n"
                        "- State \"NEW\"        from              %U\n"
                        ":END:\n"))
              ("u" ,(documentation 'my-org-capture-link) entry
               (file org-default-notes-file)
               "%(with-current-buffer (org-capture-get :original-buffer)
                  (my-org-capture-link \"%U\"))")))

(defun my-org-capture-c-context ()
  (or (memq major-mode '(w3m-mode gnus-article-mode))
      (buffer-file-name)))

(custom-set org-capture-templates-contexts
            '(("c" (my-org-capture-c-context))
              ("r" (region-active-p))
              ("u" (my-org-capture-link-context))))

(defun my-org-capture-refile--save-target-buffer (func &rest args)
  "Save target buffer after refiling an item from it."
  (let ((base (or (buffer-base-buffer) (current-buffer))))
    (apply func args)
    (with-current-buffer base (save-buffer))))
(advice-add 'org-capture-refile
            :around #'my-org-capture-refile--save-target-buffer)

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

;;; Commands

(defun my-org-convert-url-property (type &optional property)
  "Move URL from a headling property to a link in the title and back.

If TYPE is a symbol `link', convert URL property to link.

If TYPE is symbol `property', convert link to a URL property.

If TYPE is symbol `toggle', convert between a property and a
link.

Optional argument PROPERTY specifies the name of property,
defaults to \"URL\"."
  (interactive
   (list 'toggle
         (let ((url-property
                (car (seq-find
                      (lambda (prop)
                        (url-type (url-generic-parse-url (cdr prop))))
                      (org-entry-properties)))))
           (read-string "Property: " (or url-property "URL")))))
  (unless property
    (setq property "URL"))
  (save-excursion
    (org-back-to-heading)
    (let* ((element (org-element-at-point))
           (title (org-element-property :title element))
           (url-prop (org-element-property (intern (concat ":" property))
                                           element))
           (title-context (let ((case-fold-search))
                            (looking-at org-todo-line-regexp)
                            (goto-char (match-beginning 3))
                            (org-element-context)))
           (link (and (eq (org-element-type title-context) 'link)
                      (org-element-property :raw-link title-context)))
           (link-contents
            (and (eq (org-element-type title-context) 'link)
                 (buffer-substring
                  (org-element-property :contents-begin title-context)
                  (org-element-property :contents-end title-context)))))
      (cond ((and url-prop (not link) (memq type '(link toggle)))
             (org-edit-headline (org-make-link-string url-prop title))
             (org-delete-property property))
            ((and (not url-prop) link (memq type '(property toggle)))
             (org-edit-headline link-contents)
             (org-set-property property link))))))

(defun my-org-convert-url-quote ()
  "Move URL from a quote to a link in the title.

Before:

    * Foo
    #+BEGIN_QUOTE
    http://example.com
    #+END_QUOTE

After:

    * [[http://example.com][Foo]]"
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let* ((title (let ((case-fold-search))
                    (looking-at org-todo-line-regexp)
                    (goto-char (match-beginning 3))
                    (org-element-context)))
           (block (progn (org-next-block 1) (org-element-at-point)))
           (url (progn
                  (goto-char (org-element-property :contents-begin block))
                  (thing-at-point 'url))))
      (when (and (not (eq (org-element-type title) 'link))
                 (eq (org-element-type block) 'quote-block)
                 url
                 (progn (end-of-thing 'url)
                        (= (char-after) ?\n)
                        (= (+ (point) 1)
                           (org-element-property :contents-end block))))
        (org-back-to-heading)
        (delete-region (org-element-property :begin block)
                       (org-element-property :end block))
        (org-edit-headline (org-make-link-string
                            url
                            (org-element-property :raw-value title)))))))

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

Org files are ordered before anything else. Top-level files are
ordered before nested files. Top-level org files are ordered
alphabetically."
  (or (and (string= (file-name-extension left) "org")
           (not (string= (file-name-extension right) "org")))
      (and (not (string-match-p "/" left))
           (or (string-match-p "/" right)
               (< (elt left 0) (elt right 0))))))

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

;;;###autoload
(defcustom my-org-cabinet-directory "cabinet"
  "Directory with non-actionable Org files, relative to
`org-directory'."
  :type 'directory
  :group 'my)

(defun my-org-cabinet-files ()
  "Get list of non-actionable Org files stored for the reference
value."
  (directory-files (expand-file-name my-org-cabinet-directory
                                     org-directory)
                   t
                   "\\`[^#]+.org\\'"))

(custom-set org-refile-targets '((org-files-list :maxlevel . 2)
                                 (my-org-cabinet-files :maxlevel . 2)))
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

;;; Reverting

(defvar my-revert-buffer--org-revert-in-progress nil
  "t if in the process of reverting all Org buffers.

This variable is needed to avoid asking to revert buffers
recursively.")

(defun my-revert-buffer--org-revert-all (&rest _)
  "Revert all unmodified Org buffers that are changed on disk."
  (require 'ibuffer)
  (when (and (derived-mode-p 'org-mode)
             (not my-revert-buffer--org-revert-in-progress))
    (let ((my-revert-buffer--org-revert-in-progress t)
          (buffers (--filter (and (with-current-buffer it
                                    (derived-mode-p 'org-mode))
                                  (not (verify-visited-file-modtime it))
                                  (not (buffer-modified-p it)))
                             (buffer-list))))
      (when (and buffers
                 (with-temp-buffer
                   (rename-buffer " *Org Buffers*")
                   (save-window-excursion
                     (display-buffer (current-buffer)
                                     (cons 'display-buffer-at-bottom
                                           '((window-height . 0.15))))
                     (ibuffer-columnize-and-insert-list
                      (mapcar #'buffer-name buffers))
                     (yes-or-no-p "Reread these other Org files as well? "))))
        (dolist (buffer buffers)
          (with-current-buffer buffer
            (revert-buffer t t)))))))
(advice-add 'revert-buffer
            :after #'my-revert-buffer--org-revert-all)

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
            '((type  "TODO(!)" "NEXT(!)" "WAIT(@)" "|" "DONE(!)" "DROP(@)")
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
(define-key org-mode-map (kbd "C-c C-/ C-p") #'my-org-convert-url-property)
(define-key org-mode-map (kbd "C-c C-/ C-q") #'my-org-convert-url-quote)
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

(unless (display-graphic-p)
  (define-key org-mode-map (kbd "C-c DEL C-p") #'my-org-convert-url-property)
  (define-key org-mode-map (kbd "C-c DEL C-q") #'my-org-convert-url-quote))

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
