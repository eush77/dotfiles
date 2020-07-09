;;; -*- lexical-binding: t; eval: (outline-minor-mode) -*-
(add-to-list 'package-selected-packages 'enlive)
(add-to-list 'package-selected-packages 'dash)
(add-to-list 'package-selected-packages 'dash-functional)
(add-to-list 'package-selected-packages 's)
(package-install-selected-packages)

(require 'dash)
(require 'dash-functional)
(require 'enlive)
(require 'org-datetree)
(require 'org-depend)
(require 's)
(require 'subr-x)

;;; Agenda

(custom-set-variables '(org-agenda-window-setup 'current-window))

(defun my-org-agenda-get-batch-view-region ()
  "Get restriction region for the batch view.

If called with an active region, return the active region.

If called with no active region, return the current subtree if
the subtree is (partially) visible or the containing subtree or
the whole buffer otherwise."
  (save-excursion
    (org-back-to-heading)
    (cond
     ;; Return the active region
     ((region-active-p) (cons (region-beginning) (region-end)))
     ;; Return the current subtree
     ((< (save-excursion (org-next-visible-heading 1)
                         (point))
         (save-excursion (org-end-of-subtree 1)
                         (point)))
      (cons (point) (save-excursion
                      (org-end-of-subtree 1)
                      (point))))
     ;; Return the containing subtree
     ((< 1 (org-current-level))
      (progn (outline-up-heading 1)
             (cons (point) (save-excursion
                             (org-end-of-subtree 1)
                             (point)))))
     ;; Return the whole buffer
     (t (cons (point-min) (point-max))))))

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

(custom-set-variables
 '(org-agenda-custom-commands
   `(("b" "Batch view" search "{..}"
      ((org-agenda-restrict
        (progn
          (put 'org-agenda-files 'org-restrict
               (let ((file (buffer-file-name (buffer-base-buffer))))
                 (unless file
                   (user-error
                    "Cannot form batch view for non-file-visiting buffers"))
                 (list file)))
          (current-buffer)))
       (org-agenda-restrict-begin
        (car (my-org-agenda-get-batch-view-region)))
       (org-agenda-restrict-end
        (cdr (my-org-agenda-get-batch-view-region)))
       (my-org-agenda-goto-hd-point
        (unless (region-active-p)
          (save-excursion (org-back-to-heading) (point))))))
     ("n" "Next actions" todo "NEXT")
     ("p" "Planned tasks"
      ((my-org-agenda-planned-view "Weekly.org")
       (my-org-agenda-planned-view "Monthly.org")
       (my-org-agenda-planned-view "Quarterly.org")
       (my-org-agenda-planned-view "Yearly.org")
       (my-org-agenda-planned-view "5 Years.org")
       (my-org-agenda-planned-view "Life.org"))
      ((org-agenda-prefix-format "  "))))))

(defun my-org-agenda-get-restriction-and-command--display-buffer-action
    (func &rest args)
  "Set up `display-buffer' action."
  (cl-letf ((display-buffer-overriding-action '(display-buffer-at-bottom))
            ((symbol-function 'delete-other-windows) #'ignore))
    (apply func args)))
(advice-add 'org-agenda-get-restriction-and-command
            :around
            #'my-org-agenda-get-restriction-and-command--display-buffer-action)

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

(custom-set-variables
 '(org-agenda-bulk-custom-functions
   '((?y (lambda ()
           (let ((org-inhibit-blocking t)
                 (org-inhibit-logging 'note))
             (org-agenda-todo-yesterday
              (car org-done-keywords-for-agenda))))))))

;;; Archive

(custom-set-variables '(org-archive-location "archive/%s::"))

(defun my-org-save-archive-buffer ()
  "Save and kill the live archive buffer for the current buffer."
  (interactive)
  (require 'org-archive)
  (dolist (buffer (seq-remove
                   #'null
                   (seq-map #'get-file-buffer (org-all-archive-files))))
    (with-current-buffer buffer
      (save-buffer)
      (kill-buffer))))

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

(defun my-org-files-list--remove-archive (files)
  "Remove archive files.

Archive files are those matching `org-archive-location'."
  (let* ((archive-file (car (split-string org-archive-location "::")))
         (archive-file-regexp
          (concat
           "\\(?:\\`\\|/\\)"
           (if (string-match "\\`\\(.*[^%]\\(?:%%\\)*\\)?%s\\(.*\\)\\'"
                             archive-file)
               (concat (regexp-quote (substring archive-file
                                                (match-beginning 1)
                                                (match-end 1)))
                       "[^/]+"
                       (regexp-quote (substring archive-file
                                                (match-beginning 2)
                                                (match-end 2))))
             (regexp-quote archive-file))
           "\\'")))
    (seq-remove (apply-partially #'string-match-p archive-file-regexp)
                files)))
(advice-add 'org-files-list
            :filter-return #'my-org-files-list--remove-archive)

;;; Babel

(defun my-org-babel-execute-buffer--ignore-non-executable (func &rest args)
  "Ignore non-executable source blocks."
  (cl-letf* ((execute-src-block (symbol-function 'org-babel-execute-src-block))
             ((symbol-function 'org-babel-execute-src-block)
              (lambda (&rest args)
                (condition-case err
                    (apply execute-src-block args)
                  (error
                   (unless (string-match-p "\\`No org-babel-execute function"
                                           (cadr err))
                     (signal (car err) (cdr err))))))))
    (apply func args)))

(advice-add 'org-babel-execute-buffer
            :around #'my-org-babel-execute-buffer--ignore-non-executable)

;;; Capture

(defvar my-org-capture-position-cursor t
  "Non-nil if capture templates include cursor positioning %?.")

(defvar my-org-capture-timestamp nil
  "Date string inserted as an active time stamp in capture trees.

If non-nil, the string is parsed by `org-read-date' and inserted
as an active time stamp with optional end time in the entry
heading.")

(defun my-org-capture-position-cursor ()
  "Insert cursor-positioning sequence."
  (when my-org-capture-position-cursor
    (insert "%?")))

(defvar org-end-time-was-given)

(defun my-org-capture-insert-timestamp (&optional timestamp)
  "Insert timestamp into the captured headline.

If `my-org-capture-timestamp' is non-nil, insert
`my-org-capture-timestamp'. Otherwise if TIMESTAMP is non-nil,
insert TIMESTAMP."
  (when-let ((timestamp (or my-org-capture-timestamp timestamp)))
    (let ((org-end-time-was-given))
      (org-insert-time-stamp (org-read-date nil t timestamp)
                             t nil nil nil (list org-end-time-was-given))
      (insert " "))))

(defun my-org-capture-set-todo-keyword ()
  "Set todo keyword on the current headline."
  (let ((org-inhibit-blocking t)
        (org-inhibit-logging 'note)
        (org-todo-keywords '((sequence "NEW(!)" "|" "DONE"))))
    (org-mode)
    (org-todo "NEW")
    (org-add-log-note)))

(defun my-org-capture-tree (title)
  "Generate Org capture tree with TITLE."
  (with-temp-buffer
    (org-insert-heading)
    (my-org-capture-insert-timestamp)
    (my-org-capture-position-cursor)
    (insert title)
    (my-org-capture-set-todo-keyword)
    (buffer-string)))

(defvar my-org-capture-extracted-timestamps)
(setf (documentation-property 'my-org-capture-extracted-timestamps
                              'variable-documentation)
      "List of timestamps extracted by `my-org-capture-extract-tree'.

If this variable is bound when evaluating
`my-org-capture-extract-tree', it is set to the list of
timestamps extracted from the argument url.")

(defun my-org-capture-extract-tree (url)
  "Generate Org capture tree extracted from URL, or nil.

If `my-org-capture-extracted-timestamps' is bound, set it to the
list of extracted timestamps."
  (when-let ((el (my-org-extract url)))
    (let ((timestamps (org-element-property :timestamps el)))
      (when (boundp 'my-org-capture-extracted-timestamps)
        (setq my-org-capture-extracted-timestamps timestamps))
      (with-temp-buffer
        (insert (my-org-interpret el))
        (let ((case-fold-search))
          (org-back-to-heading)
          (looking-at org-heading-regexp)
          (goto-char (match-beginning 2)))
        (my-org-capture-insert-timestamp (car timestamps))
        (my-org-capture-position-cursor)
        (my-org-capture-set-todo-keyword)
        (buffer-string)))))

(defun my-org-capture-current-link (type %:link &optional %:description %f %F)
  "Current link"
  (pcase-let
      ((`(,link . ,description)
        (cond ((derived-mode-p 'exwm-mode)
               (if-let ((link (my-xdg-web-browser-get-current-url)))
                   (cons link exwm-title)
                 (let ((message
                        "Couldn't get current url from the web browser"))
                   (message message)
                   (error message))))
              ((or (derived-mode-p 'gnus-article-mode 'w3m-mode) (not %F))
               (cons %:link %:description))
              (t (cons (concat "file:" %F) %f)))))
    (cl-case type
      ('entry
       (or (my-org-capture-extract-tree link)
           (my-org-capture-tree
            ;; If DESCRIPTION is t, insert it as an empty string but in
            ;; brackets.
            (if (eq description t)
                (cl-letf (((symbol-function 'org-string-nw-p)
                           (lambda (_) t)))
                  (org-make-link-string link ""))
              (org-make-link-string link description)))))
      ('item (concat "- " (org-make-link-string link description)))
      (otherwise (user-error "Unsupported entry type")))))

(defun my-org-capture-current-link-context ()
  (or (derived-mode-p 'gnus-article-mode 'w3m-mode)
      (buffer-file-name)
      (and (boundp 'exwm-state) (my-xdg-web-browser-buffer-p))))

(defvar my-org-capture-killed-link-urls nil
  "Urls from the kill ring.")

(defun my-org-capture-killed-link ()
  "Link from the kill ring"
  (let ((link (completing-read "Killed URL: "
                               my-org-capture-killed-link-urls)))
    (or (my-org-capture-extract-tree link)
        (let ((description (read-string "Description: " link)))
          (my-org-capture-tree (org-make-link-string link description))))))

(defun my-org-capture-killed-link-context ()
  (setq my-org-capture-killed-link-urls
        (with-temp-buffer
          (seq-mapcat (lambda (kill)
                        (erase-buffer)
                        (insert (string-trim kill))
                        (when-let ((url (thing-at-point 'url)))
                          (list url)))
                      kill-ring))))

(defun my-org-capture-link ()
  "Link at point"
  (let ((link
         (or (w3m-anchor)
             (thing-at-point 'url)
             (buffer-local-value 'w3m-current-url
                                 (w3m-select-buffer-current-buffer)))))
    (or (my-org-capture-extract-tree link)
        (my-org-capture-tree
         (org-make-link-string
          link
          (or (my-w3m-anchor-text)
              (thing-at-point 'url)
              (w3m-buffer-title (w3m-select-buffer-current-buffer))))))))

(defun my-org-capture-link-context ()
  (or (eq major-mode 'w3m-select-buffer-mode)
      (and (fboundp 'w3m-anchor) (w3m-anchor))
      (thing-at-point 'url)))

(defun my-org-capture-links ()
  "Links in the active region"
  (let ((my-org-capture-position-cursor)
        (trees))
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min))
        (when (w3m-anchor)
          (save-restriction
            (widen)
            (push (my-org-capture-link) trees)))
        (while (w3m-goto-next-anchor)
          (when (w3m-anchor)            ; Skip forms
            (save-excursion
              (save-restriction
                (widen)
                (push (my-org-capture-link) trees)))))))
    (mapconcat #'identity (reverse trees) "")))

(defun my-org-capture-links-context ()
  (and (region-active-p) (eq major-mode 'w3m-mode)))

(defun my-org-capture-list-item-target ()
 (if-let ((marker
           (or (--find
                (let ((marker-buffer (marker-buffer it)))
                  (and (bufferp marker-buffer)
                       (not (eq marker-buffer (current-buffer)))
                       (with-current-buffer marker-buffer
                         (derived-mode-p 'org-mode))))
                global-mark-ring)
               (when-let
                   ((buffer
                     (--find (with-current-buffer it
                               (derived-mode-p 'org-mode))
                             (-map #'window-buffer
                                   (cdr (my-active-windows))))))
                 (with-current-buffer buffer (point-marker))))))
     (progn (set-buffer (marker-buffer marker))
            (goto-char marker))
   (user-error "No other Org buffer found in `global-mark-ring' or `my-active-windows'")))

(defun my-org-capture-new ()
  "New item"
  (my-org-capture-tree ""))

(defun my-org-capture-region (type %i &optional %f %:from %:link %:subject)
  "Active region"
  (let* ((magit-id (and (eq major-mode 'magit-revision-mode)
                        (save-excursion (goto-char (point-min))
                                        (thing-at-point 'word))))
         (body (concat
                (string-trim
                 (if (derived-mode-p 'exwm-mode)
                     (with-selected-window (get-buffer-window)
                       (exwm-input--fake-key ?\C-c)
                       (sleep-for .2)
                       (current-kill 0 t))
                   %i))
                "\n"))
         (from (pcase major-mode
                 ('w3m-mode %:link)
                 ('gnus-article-mode
                  (format "\"%s\" (by %s)" %:subject %:from))
                 ('magit-revision-mode
                  (magit-rev-format "\"%s\" (%h by %an)" magit-id))
                 ('exwm-mode
                  (let ((description (format "\"%s\"" exwm-title)))
                    (if-let ((link (my-xdg-web-browser-get-current-url)))
                        (org-make-link-string link description)
                      description)))
                 (_ (if (string-empty-p %f) (buffer-name) %f))))
         (subject (pcase major-mode
                    ('w3m-mode w3m-current-title)
                    ('gnus-article-mode %:subject)
                    ('magit-revision-mode (magit-rev-format "%s" magit-id))
                    (_ ""))))
    (cl-case type
      ('entry
       (concat (my-org-capture-tree subject)
               (when from
                 (with-temp-buffer
                   (insert "Captured from " from ":\n")
                   (fill-region (point-min) (point-max))
                   (buffer-string)))
               "#+BEGIN_QUOTE\n"
               body
               "#+END_QUOTE\n"))
      ('item
       (replace-regexp-in-string "^" "- "
                                 (string-trim body)))
      (otherwise (user-error "Unsupported entry type")))))

(defun my-org-capture-region-context ()
  (or (region-active-p) (derived-mode-p 'exwm-mode)))

(defun my-org-capture-scratch ()
  "Scratch text"
  (if (region-active-p)
      (buffer-substring (region-beginning) (region-end))
    ""))

(defun my-org-capture-scratch-target ()
  (set-buffer "*scratch*")
  (goto-char (point-max)))

(custom-set-variables
 '(org-capture-templates
   `(("n" ,(documentation 'my-org-capture-new) entry
      (file org-default-notes-file)
      "%(my-org-capture-new)")
     ("r" ,(documentation 'my-org-capture-region) entry
      (file org-default-notes-file)
      "%(with-current-buffer (org-capture-get :original-buffer)
          (my-org-capture-region 'entry \"%i\" \"%f\"
                                 \"%:from\" \"%:link\" \"%:subject\"))")
     ("c" ,(documentation 'my-org-capture-current-link) entry
      (file org-default-notes-file)
      "%(with-current-buffer (org-capture-get :original-buffer)
          (my-org-capture-current-link 'entry \"%:link\" \"%:description\"
                                       \"%f\" \"%F\"))")
     ("k" ,(documentation 'my-org-capture-killed-link) entry
      (file org-default-notes-file)
      "%(my-org-capture-killed-link)")
     ("u" ,(documentation 'my-org-capture-link) entry
      (file org-default-notes-file)
      "%(with-current-buffer (org-capture-get :original-buffer)
          (my-org-capture-link))")
     ("U" ,(documentation 'my-org-capture-links) entry
      (file org-default-notes-file)
      "%(with-current-buffer (org-capture-get :original-buffer)
          (my-org-capture-links))")
     ("s" ,(documentation 'my-org-capture-scratch) plain
      (function my-org-capture-scratch-target)
      "%(with-current-buffer (org-capture-get :original-buffer)
          (my-org-capture-scratch))"
      :empty-lines 1 :immediate-finish t :jump-to-captured t :no-save t)
     ("l" "List item")
     ("lc" ,(documentation 'my-org-capture-current-link) item
      (function my-org-capture-list-item-target)
      "%(with-current-buffer (org-capture-get :original-buffer)
          (my-org-capture-current-link 'item \"%:link\" \"%:description\"
                                       \"%f\" \"%F\"))")
     ("lr" ,(documentation 'my-org-capture-region) item
      (function my-org-capture-list-item-target)
      "%(with-current-buffer (org-capture-get :original-buffer)
          (my-org-capture-region 'item \"%i\" \"%f\"
                                 \"%:from\" \"%:link\" \"%:subject\"))")))
 '(org-capture-templates-contexts
   '(("r" (my-org-capture-region-context))
     ("c" (my-org-capture-current-link-context))
     ("k" (my-org-capture-killed-link-context))
     ("u" (my-org-capture-link-context))
     ("U" (my-org-capture-links-context))
     ("lc" (my-org-capture-current-link-context)))))

(define-advice org-capture-refile
    (:after (&rest args) my-jump-to-refiled)
  "Jump to refiled items."
  (unless (eq (marker-buffer org-capture-last-stored-marker)
              (current-buffer))
    (org-goto-marker-or-bmk org-capture-last-stored-marker)))

(define-advice org-capture-refile
    (:around (func &rest args) my-save-target-buffer)
  "Save target buffer after refiling an item from it."
  (let ((base (or (buffer-base-buffer) (current-buffer))))
    (apply func args)
    (with-current-buffer base (save-buffer))))

(define-advice org-capture-select-template
    (:around (func &rest args) my-display-buffer-action)
  "Set up `display-buffer' action."
  (let ((display-buffer-overriding-action
         '(display-buffer-at-bottom . ((window-height . 0.15)))))
    (apply func args)))

(define-advice org-capture-store-last-position
    (:around (func) my-non-visiting-buffer)
  "Don't bookmark captured position in non-visiting target buffers."
  (if (buffer-file-name (buffer-base-buffer (current-buffer)))
      (funcall func)
    (let ((org-capture-bookmark))
      (funcall func))))

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

(custom-set-variables
 '(org-columns-default-format
   "%32ITEM %TODO %1PRIORITY %4EFFORT{:} %4CLOCKSUM %CATEGORY %TAGS")
 '(org-columns-summary-types
   '(("!min" . my-org-timestamp-summarize-min))))

(defun my-org-timestamp-summarize-min (timestamps &optional format)
  "Summarize TIMESTAMPS by returning the minimum."
  (seq-reduce (lambda (left right)
                (cond ((string-empty-p left) right)
                      ((time-less-p (org-time-string-to-time left)
                                    (org-time-string-to-time right)) left)
                      (t right)))
              timestamps
              ""))

;;; Datetree

(defun my-org-in-datetree-p ()
  "t if point is inside a datetree, nil otherwise."
  (catch 'return
    (save-excursion
      (org-back-to-heading)
      (while (not (string-match-p
                   "\\`20[1-9][0-9]\\'"
                   (org-element-property :raw-value
                                         (org-element-context))))
        (unless (org-up-heading-safe)
          (throw 'return nil)))
      (or (= 1 (org-current-level))
          (progn (org-up-heading-safe)
                 (and (org-element-property :DATE_TREE
                                            (org-element-context))
                      t))))))

(defun my-org-datetree-file-entries ()
  "File top-level entries into the datetree."
  (interactive)
  (require 'hl-line)
  (save-excursion
    (let ((hl-line-mode t)
          (case-fold-search))
      (while (re-search-forward (concat "^\\*\\s *"
                                        org-todo-regexp
                                        "?\\s *"
                                        org-ts-regexp)
                                nil t)
        (let ((timestamp (org-element-context)))
          (cl-assert (eq (org-element-type timestamp) 'timestamp))
          (hl-line-highlight)
          (when (condition-case err (y-or-n-p "File into datetree? ")
                  ((error quit)
                   (hl-line-unhighlight)
                   (signal (car err) (cdr err))))
            (hl-line-unhighlight)
            (org-cut-subtree)
            (org-datetree-file-entry-under
             (current-kill 0)
             (list (org-element-property :month-start timestamp)
                   (org-element-property :day-start timestamp)
                   (org-element-property :year-start timestamp)))
            (org-reveal)))))))

(define-advice org-timestamp-change
    (:around (func n &rest args) my-datetree-file-or-cleanup)
  "File heading or clean up the datetree if change is a no-op."
  (let* ((element (org-element-context))
         (at-timestamp-p (eq (org-element-type element) 'timestamp)))
    (apply func n args)
    (when (and at-timestamp-p
               (zerop n)
               (equal (org-element-context) element))
      (if (my-org-in-datetree-p)
          (when (yes-or-no-p "Clean up the datetree? ")
            (org-datetree-cleanup))
        (when (y-or-n-p "File into datetree? ")
          (org-cut-subtree)
          (org-datetree-file-entry-under
           (current-kill 0)
           (list (org-element-property :month-start element)
                 (org-element-property :day-start element)
                 (org-element-property :year-start element)))
          (org-reveal))))))

;;; Ediff

(defun my-org-setup-ediff-prepare-buffer-hook ()
  "Set up `ediff-prepare-buffer-hook' for Org buffer."
  (add-hook 'ediff-prepare-buffer-hook #'org-show-all nil t))

(add-hook 'org-mode-hook #'my-org-setup-ediff-prepare-buffer-hook)

(defun my-org-mode--in-ediff-session (func &rest args)
  "Show all contents of the buffer if in a Ediff session."
  (let ((in-ediff-p (member 'ediff-diff-status mode-line-format)))
    (apply func args)
    (when in-ediff-p
      (org-show-all))))
(advice-add 'org-mode :around #'my-org-mode--in-ediff-session)

;;; Effort

(custom-set-variables
 '(org-global-properties
   '(("EFFORT_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 6:00"))))

;;; Export

(custom-set-variables
 '(org-export-async-init-file
   (locate-user-emacs-file "ox-async-init.el")))

;;; Extract

(defun my-org-interpret (el)
  "Interpret extracted headline EL as Org syntax.

Combines the following interpreters:
  - `org-element-headline-interpreter',
  - `org-element-planning-interpreter',
  - `org-element-property-drawer-interpreter'."
  (concat
   (org-element-headline-interpreter el nil)
   (let ((planning (org-element-planning-interpreter el nil)))
     (when (not (string-empty-p planning))
       (concat planning "\n")))
   (when-let
       ((props
         (-map (pcase-lambda (`(,prop ,value &rest _))
                 (format "%s: %s\n" prop value))
               (cdr (-partition-before-pred
                     (-andfn #'symbolp
                             (-compose #'s-uppercase-p #'symbol-name))
                     (cadr el))))))
     (concat (org-element-property-drawer-interpreter
              nil (apply #'concat props))
             "\n"))))

(defun my-org-extract-from-url (url &optional title &rest props)
  "Extract headline from a generic URL with optional TITLE.

Insert additional property pairs PROPS into the headline's
property list."
  `(headline (:level 1
              :title ,(if title (org-make-link-string url title) url)
              ,@props)))

(defun my-org-extract-from-artguide (url)
  "Extract headline from an ArtGuide URL."
  (and
   (string-match-p "\\`https?://artguide\\.com/events/" url)
   (let* ((page (enlive-fetch url))
          (title (enlive-text (enlive-query page [.event-info > h1])))
          (deadline
           (pcase-let
               ((`(,day ,month ,year)
                 (-map #'string-to-number
                       (split-string
                        (enlive-text
                         (cadr
                          (enlive-query-all page
                                            [.schedule-date-time > span])))
                        "\\."))))
             `(timestamp (:type active
                          :year-start ,(+ 2000 year)
                          :month-start ,month
                          :day-start ,day))))
          (location
           (mapconcat #'enlive-text
                      (enlive-query-all page [.schedule-place a])
                      ", ")))
     (my-org-extract-from-url url title
                              :tags '("museum")
                              :deadline deadline
                              :LOCATION location))))

(defvar my-org-audible-properties
  '(("By: " . :AUTHOR)
    ("Narrated by: " . :NARRATOR)
    ("Length: " . :DURATION))
  "Mapping from property lines on the Audible website to Org properties.

Each element is a cons cell (PREFIX . PROPERTY).")

(defun my-org-extract-from-audible (url)
  "Extract headline from an Audible URL."
  (and
   (string-match-p "\\`https?://www\\.audible\\.com/pd/" url)
   (let ((retry-count 0))
     (cl-loop
      (when (= retry-count 3)
        (error "Could not scrape %s" url))
      (let* ((hero-content
              (enlive-query (enlive-fetch url) [.hero-content]))
             (title (enlive-text (enlive-query hero-content [h1]))))
        (if (string-empty-p title)
            (progn (sleep-for .1)
                   (cl-incf retry-count))
          (cl-return
           `(headline
             (:level 1
              :AUDIBLE ,url
              :title ,title
              :tags ("audible")
              ,@(--mapcat
                 (pcase-let ((`(,prefix . ,prop)
                              (assoc it my-org-audible-properties
                                     #'string-prefix-p)))
                   (and prop (list prop (s-chop-prefix prefix it))))
                 (-map (-compose #'s-trim
                                 #'s-collapse-whitespace
                                 #'enlive-text)
                       (enlive-query-all
                        hero-content
                        [.bc-col > .bc-row > .bc-text]))))))))))))

(defun my-org-extract-from-garagemca (url)
  "Extract headline from a Garage MCA URL."
  (and
   (string-match-p "\\`https?://garagemca\\.org/" url)
   (let* ((page (enlive-fetch url))
          (page-en
           (enlive-fetch
            (replace-regexp-in-string "/ru/" "/en/" url)))
          (title
           (enlive-text (enlive-query page [.event__header__title])))
          (org-end-time-was-given)
          (start-time
           (org-read-date
            nil t
            (replace-regexp-in-string
             "–" "-"
             (enlive-text
              (--map-when
               (and (enlive-is-element it) (eq (car it) 'comment))
               " "
               (enlive-query page-en [.event__meta__timestamp]))))))
          (timestamp
           (with-temp-buffer
             (org-insert-time-stamp
              start-time t nil nil nil (list org-end-time-was-given))
             (buffer-string))))
     (my-org-extract-from-url url title
                              :timestamps (list timestamp)))))

(defvar my-org-goodreads-genre-limit 3
  "Maximum number of genres inserted into GENRES property.")

(defvar my-org-goodreads-properties
  '(("author" . :AUTHOR)
    ("numberOfPages" . :PAGES)
    ("isbn" . :ISBN)
    ("inLanguage" . :LANGUAGE))
  "Mapping from itemprop html properties on the Goodreads website to Org properties.

Each element is a cons cell (ITEMPROP . PROPERTY).")

(defun my-org-extract-from-goodreads (url)
  "Extract headline from a Goodreads URL."
  (and
   (string-match-p "\\`https?://www\\.goodreads\\.com/book/show/" url)
   (let* ((page (enlive-fetch url))
          (metacol (enlive-query page [:metacol])))
     `(headline
       (:level 1
               :GOODREADS ,url
               :title ,(string-trim
                        (enlive-text
                         (enlive-query metacol [:bookTitle])))
               :tags ("book")
               :GENRES ,(mapconcat
                         #'enlive-text
                         (-take my-org-goodreads-genre-limit
                                (enlive-query-all
                                 page [.left > .bookPageGenreLink]))
                         ", ")
               ,@(let ((series
                        (string-trim
                         (enlive-text
                          (enlive-query metacol [:bookSeries])))))
                   (unless (string-empty-p series)
                     (list :SERIES
                           (replace-regexp-in-string "\\`(\\(.*\\))\\'" "\\1"
                                                     series))))
               ,@(--mapcat
                  (when-let ((prop
                              (alist-get (enlive-attr it 'itemprop)
                                         my-org-goodreads-properties
                                         nil nil #'equal)))
                    (list prop
                          (s-trim (s-collapse-whitespace (enlive-text it)))))
                  (enlive-filter metacol
                                 (lambda (el) (enlive-attr el 'itemprop)))))))))

(defun my-org-extract-from-rambler-kassa (url)
  "Extract headline from a Rambler Kassa URL."
  (when-let*
      ((tag
        (pcase (caddr
                (s-match
                 (concat "\\`https?://kassa\\.rambler\\.ru/"
                         "\\([a-z]+/\\)?\\([a-z]+\\)/[[:digit:]]+")
                 url))
          ("concert" "music")
          ("movie" "cinema")
          ("performance" "play")))
       (page (enlive-fetch url))
       (title (enlive-text (enlive-query page [.item_title])))
       (title2
        (s-trim
         (car
          (s-split "—"
                   (enlive-text (enlive-query page [.item_title2]))))))
       (title (if (string-empty-p title2)
                  title
                (format "%s (%s)" title title2))))
    (my-org-extract-from-url url title
                             :tags (list tag))))

(defcustom my-org-timepad-token nil
  "TimePad API token for `my-org-extract-from-timepad'.

Needs to have at least `view_events' permission.

Get the token from URL `http://dev.timepad.ru/api/oauth/'."
  :group 'my
  :type '(choice (const :tag "Not set" nil) string))

(defun my-org-extract-from-timepad (url)
  "Extract headline from a TimePad URL."
  (when-let*
      ((event-id
        (cadr
         (s-match
          "\\`https?://[^./]+\\.timepad\\.ru/event/\\([[:digit:]]+\\)/"
          url)))
       (url-request-extra-headers
        (cond (my-org-timepad-token
               `(("Authorization" . ,(concat "Bearer " my-org-timepad-token))))
              ((yes-or-no-p "Need a token to access TimePad API. Customize? ")
               (customize-variable 'my-org-timepad-token))))
       (event
        (with-current-buffer
            (url-retrieve-synchronously
             (concat "https://api.timepad.ru/v1/events/" event-id))
          (pcase-let ((`(,status ,status-code ,status-message)
                       (assoc url-http-response-status url-http-codes))
                      (response
                       (json-read-from-string
                        (decode-coding-string
                         (buffer-substring (point) (point-max)) 'utf-8))))
            (if (eq status-code 'OK)
                response
              (error "%s %s: %s"
                     status status-message
                     (alist-get 'message
                                (alist-get 'response_status response)))))))
       (timestamp
        (with-temp-buffer
          (org-insert-time-stamp
           (date-to-time (alist-get 'starts_at event))
           t nil nil nil
           (list
            (format-time-string "%R"
                                (date-to-time
                                 (alist-get 'ends_at event)))))
          (buffer-string)))
       (location
        (concat (alist-get 'name (alist-get 'organization event))
                ", "
                (alist-get 'address (alist-get 'location event)))))
    (my-org-extract-from-url url (alist-get 'name event)
                             :LOCATION location
                             :timestamps (list timestamp))))

(defun my-org-extract-from-tretyakovgallery (url)
  "Extract headline from a Tretyakov Gallery URL."
  (and
   (string-match-p "\\`https?://www\\.tretyakovgallery\\.ru/" url)
   (pcase-let*
       ((page (enlive-fetch url))
        (title
         ;; Strip content rating.
         (replace-regexp-in-string
          "[[:space:]][[:digit:]]\\{1,2\\}\\+\\.?[[:space:]]?$" ""
          (enlive-text (enlive-query page [.header-event__title]))))
        (`(,timestamps . ,tags)
         (if (string-match-p "/cinema/" url)
             (let ((duration
                    ;; Round up to the nearest 10-minutes mark.
                    (* (fceiling
                        (/ (apply
                            #'+
                            (--mapcat
                             (let ((text (enlive-text it)))
                               (and (string-match
                                     "\\b\\([[:digit:]]+\\) мин\\.\\'"
                                     text)
                                    (list (string-to-number
                                           (match-string 1 text)))))
                             (enlive-query-all page [.event-desc__lid])))
                           10.0))
                       10)))
               (cons
                (--map (with-temp-buffer
                         (let ((start-time
                                (org-read-date
                                 t t
                                 (alist-get 'value (cadr it)))))
                           (org-insert-time-stamp
                            start-time t nil nil nil
                            (and (not (zerop duration))
                                 (format-time-string
                                  "-%R"
                                  (time-add start-time
                                            (* duration 60))))))
                         (buffer-string))
                       (enlive-query-all page [.event-schedule-time__input]))
                '("cinema")))
           (pcase-let*
               ((page-en
                 (enlive-fetch
                  (replace-regexp-in-string "\\.ru/" ".ru/en/" url)))
                (type
                 (downcase
                  (enlive-text
                   (enlive-query page-en [.event-info__type]))))
                (`(,duration . ,tags)
                 (pcase type
                   ("concerts and performances" '(120 . ("music")))
                   ("lectures" '(120 . ("lecture")))
                   ("guided tours" '(80 . ("museum" "tour")))
                   (t '(nil . nil))))
                (start-time
                 (org-read-date
                  t t
                  (concat
                   (enlive-text (enlive-query page-en [.event-info__date]))
                   " "
                   (enlive-text (enlive-query page-en [.event-info__time]))))))
             (cons (with-temp-buffer
                     (org-insert-time-stamp
                      start-time t nil nil nil
                      (and duration
                           (format-time-string "-%R"
                                               (time-add start-time
                                                         (* duration 60)))))
                     (list (buffer-string)))
                   tags)))))
     (my-org-extract-from-url url title
                              :tags tags
                              :timestamps timestamps))))

(defcustom my-org-extractors
  '(my-org-extract-from-artguide
    my-org-extract-from-audible
    my-org-extract-from-garagemca
    my-org-extract-from-goodreads
    my-org-extract-from-rambler-kassa
    my-org-extract-from-timepad
    my-org-extract-from-tretyakovgallery)
  "List of Org extractors for URLs.

Each extractor is a function that takes a URL and returns either
an Org headline element extracted from the URL or nil if the
extractor cannot be applied.

If the URL describes an event with one or more potential start
times, the extractor must set `:timestamps' property on the
returned headline with the list of all alternative timestamp
strings."
  :group 'my
  :type '(repeat (function)))

(defun my-org-extract (url)
  "Extract headline from a URL using the defined extractors.

Cycles through `my-org-extractors' until the first extractor that
returns non-nil."
  (--some (funcall it url) my-org-extractors))

;;; Find-Other-File

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
      (or (seq-filter #'file-exists-p (org-all-archive-files))
          (save-excursion
            (goto-char (point-max))
            (when (search-backward-regexp
                   "^ *:ARCHIVE_FILE: *\\([^ ].*\\)$"
                   nil
                   t)
              (list (my-locate-org-file (match-string 1)))))))))

(defun my-org-ff-other-file/export (file)
  "Return the list of locations of files exported from FILE."
  (mapcar (lambda (ext) (concat (file-name-base file) ext))
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

(custom-set-variables '(org-startup-indented t))

(defun my-org-align-tags ()
  "Align all tags in the visible part of the buffer.

Interactive wrapper around `org-align-tags'."
  (interactive)
  (org-align-tags t))

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

(custom-set-variables '(org-list-allow-alphabetical t))

;;; Logbook

(custom-set-variables
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-log-refile 'time))

;;; Mouse

(when (display-mouse-p)
  (require 'org-mouse))

;;; Priority

(custom-set-variables
 '(org-default-priority ?C)
 '(org-lowest-priority ?D))

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

(custom-set-variables
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-allow-creating-parent-nodes t)
 '(org-refile-targets '((org-files-list :maxlevel . 3)))
 '(org-refile-use-outline-path 'file))

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
  (if (region-active-p)
      (let ((begin (copy-marker (region-beginning) t))
            (end (copy-marker (save-excursion
                                (goto-char (region-end))
                                (skip-chars-backward " \r\t\n"
                                                     (region-beginning))
                                (point)))))
        (apply func args)
        (save-excursion
          (goto-char begin)
          (re-search-backward "#\\+begin_")
          (upcase-region (line-beginning-position) (line-end-position))
          (goto-char end)
          (re-search-forward "#\\+end_")
          (upcase-region (line-beginning-position) (line-end-position))))
    (let ((begin (point))
          (end (copy-marker (point) t)))
      (apply func args)
      (upcase-region begin end))))
(advice-add 'org-insert-structure-template
            :around #'my-org-insert-structure-template--upcase)

;;; Todo

(custom-set-variables
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-todo-keywords '((type  "TODO(t!)" "NEXT(n!)" "WAIT(w@)"
                              "|" "DONE(d!)" "DROP(r!)")
                       (sequence "PLAN" "|" "PASS" "FAIL")))
 '(org-todo-keyword-faces '(("PLAN" . "yellow")
                            ("PASS" . "green")
                            ("FAIL" . "red")))
 '(org-use-fast-todo-selection 'expert))

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

(define-advice org-store-log-note (:around (func) my-hide-log-drawer)
  "Hide the log drawer if the previous log drawer is hidden."
  (let ((buffer (marker-buffer org-log-note-marker)))
    (funcall func)
    (with-current-buffer buffer
      (when (org-with-wide-buffer
             (org-back-to-heading)
             (and (re-search-backward
                   (concat "^[[:space:]]*:"
                           (regexp-quote (org-log-into-drawer))
                           ":[[:space:]]*$")
                   nil t)
                  (invisible-p (point))))
        (outline-hide-entry)))))

;;; Typography

(defun my-org-nbsp-fixup-pattern (start end)
  "Protect the pattern against Org inline syntax."
  (cons (if (or (null start) (string-empty-p start))
            "[^*/=]"
          start)
        (if (or (null end) (string-empty-p end))
            "[^*/=]"
          end)))

(setf (alist-get 'org-mode my-nbsp-patterns)
      '(("[^/$]" . "---")))

(setf (alist-get 'org-mode my-nbsp-pattern-fixups)
      #'my-org-nbsp-fixup-pattern)

(defun my-org-region-snap-to-elements (begin end)
  "Extend region bounds to nearest element boundaries.

Returns a cons pair of new bounds."
  (cons (save-excursion
          (goto-char begin)
          (let ((element (org-element-at-point)))
            (if (eq (org-element-type element) 'src-block)
                ;; Don't snap past source blocks.
                begin
              (org-element-property :begin element))))
        (save-excursion
          (goto-char (- end 1))
          (let ((element (org-element-at-point)))
            (cl-case (org-element-type element)
              ;; Don't snap past entire sections.'
              (headline (org-element-property :contents-begin element))
              ;; Don't snap past source blocks.
              (src-block end)
              (t (org-element-property :end element)))))))

(defun my-org-nbsp-fix-element (langid nbsp &optional begin end query-p)
  "Fix non-breaking spaces in the element at point.

LANGID is an ISO 639-1 language code. When called interactively,
it is guessed from the contents of the element.

NBSP is a string to insert for non-breaking space. When called
interactively, it is guessed from the contents of the element.

If BEGIN and END are non-nil, fix spaces in all elements in the
region.

QUERY-P specifies whether to query the user for each match or
replace silently. `t' when called with a prefix argument."
  (interactive
   (pcase-let ((`(,begin . ,end)
                (apply #'my-org-region-snap-to-elements
                       (if (region-active-p)
                           (-cons-to-list (car (region-bounds)))
                         (list (point) (+ (point) 1))))))
     (require 'guess-language)
     (list (guess-language-region begin end)
           (call-interactively #'my-nbsp-get-sequence)
           begin
           end
           current-prefix-arg)))
  (pcase-let ((`(,begin . ,end)
               (my-org-region-snap-to-elements (or begin (point))
                                               (or end (+ (point) 1)))))
    (my-nbsp-fix langid nbsp begin end query-p)))

(define-advice org-fill-paragraph (:before (&rest _ignored) my-nbsp-fix)
  "Fix non-breaking spaces.

Both the language and the nbsp sequence are guessed from the
current buffer. If there is no guess for the nbsp sequence no
operation is performed. Otherwise all matches are replaced
silently."
  (when-let ((nbsp (my-nbsp-get-sequence)))
    (let* ((element (org-element-at-point))
           (begin (org-element-property :begin element))
           (end (org-element-property :end element)))
      (require 'guess-language)
      (my-org-nbsp-fix-element
       (apply #'guess-language-region
              (-cons-to-list (my-org-region-snap-to-elements begin end)))
       nbsp))))

;;; URLs

(defun my-org-convert-url-get-title ()
  (save-excursion
   (org-back-to-heading)
   (let ((case-fold-search))
     (looking-at org-todo-line-regexp)
     (goto-char (match-beginning 3))
     (org-element-context))))

(defun my-org-convert-url-get-link ()
  (let ((title (my-org-convert-url-get-title)))
    (and (eq (org-element-type title) 'link)
         (org-element-property :raw-link title))))

(defun my-org-url-property-name (url)
  "Property name corresponding to a URL."
  (s-join
   "-"
   (reverse
    (s-split "\\."
             (upcase
              (caddr
               (s-match
                "\\`https?://\\(www\\.\\)?\\([^/]+\\)\\(\\.[a-z]+\\)/"
                url)))))))

(defun my-org-convert-url-property (type &optional property)
  "Move URL from a headling property to a link in the title and back.

If TYPE is a symbol `link', convert URL property to link.

If TYPE is symbol `property', convert link to a URL property.

If TYPE is symbol `toggle', convert between a property and a
link.

Optional argument PROPERTY specifies the name of property,
defaults to one given by `my-org-convert-url-property-patterns'
if converting to a property and one of the patterns matches, or
\"URL\" otherwise."
  (interactive
   `(toggle ,(read-string
              "Property: "
              (or (if-let ((link (my-org-convert-url-get-link)))
                      (my-org-url-property-name link)
                    (seq-some
                     (pcase-lambda (`(,property . ,value))
                       (and (url-type (url-generic-parse-url value))
                            property))
                     (org-entry-properties)))
                  "URL"))))
  (let* ((element (save-excursion
                    (org-back-to-heading)
                    (org-element-at-point)))
         (title (my-org-convert-url-get-title))
         (link (my-org-convert-url-get-link)))
    (setq property
          (or property
              (and link (my-org-url-property-name link))
              "URL"))
    (let ((url-prop
           (org-element-property (intern (concat ":" property)) element)))
      (cond ((and url-prop (not link) (memq type '(link toggle)))
             (org-edit-headline
              (org-make-link-string url-prop
                                    (org-element-property :title element)))
             (org-delete-property property))
            ((and (not url-prop) link (memq type '(property toggle)))
             (org-edit-headline
              (buffer-substring
               (org-element-property :contents-begin title)
               (org-element-property :contents-end title)))
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

(defun my-org-pocket-add-url (&optional no-mark-done)
  "Save URL under point to Pocket and mark the entry DONE.

If NO-MARK-DONE is non-nil, don't change the entry state."
  (interactive "P")
  (require 'pocket-lib)
  (let* ((context (org-element-context))
         (url (and (eq (org-element-type context) 'link)
                   (org-element-property :raw-link context))))
    (when (and url (pocket-lib-add-urls url))
      (unless (or no-mark-done (org-entry-is-done-p))
        (org-todo 'done))
      (message "Added: %s" url))))

;;; Visibility

(custom-set-variables '(org-cycle-global-at-bob t))

(defun my-org-cycle (arg)
  "With single prefix argument ARG, call
`my-outline-cycle-entry'. Otherwise call `org-cycle'."
  (interactive "P")
  (if (equal arg '(4))
      (my-outline-cycle-entry)
    (org-cycle arg)))

;;; Web Server

(defun my-ws-send-org-agenda
    (process agenda-command &optional revert-buffers-p &rest agenda-args)
  "Render agenda buffer as html and send to PROCESS.

AGENDA-COMMAND is the agenda function to call. AGENDA-ARGS are
the arguments to this function.

If REVERT-BUFFER-P is non-nil, revert Org buffers without asking."
  (when revert-buffers-p
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (org-revert-all-org-buffers)))
  (with-current-buffer
      (save-window-excursion
        (apply agenda-command agenda-args)
        (org-agenda-redo t)
        (htmlize-buffer))
    (ws-response-header process 200 '("Content-Type" . "text/html"))
    (process-send-region process (point-min) (point-max))
    (kill-buffer)))

(defun my-ws-send-org-checklist (process file &optional revert-p)
  "Render Org file as an html checklist and send to PROCESS.

The checklist includes all headings with a todo keyword. The page
includes an HTML form with a selector consisting of the rest of
the headings and a button that sends a POST request.

If REVERT-P is non-nil, revert that buffer."
  (with-current-buffer
    (cl-letf (((symbol-function 'yes-or-no-p) (-const revert-p)))
      (find-file-noselect (expand-file-name file org-directory)))
    (goto-char (point-max))
    (let ((checklist)
          (form))
      (while (outline-previous-heading)
        (let* ((el (org-element-at-point))
               (title (org-element-property :title el))
               (todo-keyword (org-element-property :todo-keyword el)))
          (if todo-keyword
              (push `(label (input :type "checkbox" ,title) (br))
                    checklist)
            (push `(option :value ,title ,title) form))))
      (ws-response-header process 200 '("Content-Type" . "text/html"))
      (process-send-string
       process
       (xmlgen
        `(html (head (meta :name "viewport"
                           :content "width=device-width, initial-scale=1"))
               (body ,@checklist
                     (hr :style "margin-top: 1.5em;")
                     (form :method "post"
                           (button "+")
                           (select :name "item" ,@form)))))))))

(defun my-ws-send-org-html
    (process org-file &optional revert-buffer-p)
  "Export ORG-FILE as html and send to PROCESS.

If REVERT-BUFFER-P is non-nil, revert Org buffer without asking."
  (setq org-file (expand-file-name org-file org-directory))
  (with-current-buffer
      (let ((revert-without-query
             (if revert-buffer-p
                 (cons (regexp-quote org-file) revert-without-query)
               revert-without-query)))
        (find-file-noselect org-file))
    (with-current-buffer
        (let ((org-export-show-temporary-export-buffer))
          (org-html-export-as-html))
      (ws-response-header process 200 '("Content-Type" . "text/html"))
      (process-send-region process (point-min) (point-max)))))

;;; Keymap

(key-chord-define org-mode-map "xw" #'ff-get-other-file)

(define-key org-mode-map (kbd "M-<tab>") nil)

(define-key org-mode-map (kbd "C-`") #'my-insert-tilde)
(define-key org-mode-map (kbd "C-c C-\\") #'org-toggle-link-display)
(define-key org-mode-map (kbd "C-c C-/ C-p") #'my-org-convert-url-property)
(define-key org-mode-map (kbd "C-c C-/ C-q") #'my-org-convert-url-quote)
(define-key org-mode-map (kbd "C-c C-x C-p") #'org-pomodoro)
(define-key org-mode-map (kbd "C-c C-y") #'org-todo-yesterday)
(define-key org-mode-map (kbd "C-c j") #'org-shiftmetaleft)
(define-key org-mode-map (kbd "C-c k") #'org-shiftmetaright)
(define-key org-mode-map (kbd "C-c n") #'org-metadown)
(define-key org-mode-map (kbd "C-c p") #'org-metaup)
(define-key org-mode-map (kbd "C-c P") #'my-org-pocket-add-url)
(define-key org-mode-map (kbd "C-c M-,") #'org-insert-structure-template)
(define-key org-mode-map (kbd "C-M-b") #'org-previous-link)
(define-key org-mode-map (kbd "C-M-f") #'org-next-link)
(define-key org-mode-map (kbd "C-x 8 e") #'counsel-org-entity)
(define-key org-mode-map (kbd "M-N") #'my-outline-show-next-subtree)
(define-key org-mode-map (kbd "M-Q") #'my-org-nbsp-fix-element)
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
