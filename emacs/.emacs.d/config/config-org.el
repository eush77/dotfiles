;;; -*- lexical-binding: t; eval: (outline-minor-mode) -*-
(add-to-list 'package-selected-packages 'enlive)
(package-install-selected-packages)

(require 'enlive)
(require 'org-depend)
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

(defun my-org-capture-set-todo-keyword ()
  "Set todo keyword on the current headline."
  (let ((org-inhibit-blocking t)
        (org-inhibit-logging 'note)
        (org-todo-keywords '((sequence "NEW(!)" "|" "DONE"))))
    (org-mode)
    (org-todo "NEW")
    (org-add-log-note)))

(defvar my-org-capture-position-cursor t
  "Non-nil if capture templates include cursor positioning %?.")

(defun my-org-capture-tree (title)
  "Generate Org capture tree with TITLE."
  (with-temp-buffer
    (org-insert-heading)
    (insert (if my-org-capture-position-cursor
                (concat "%?" title)
              title))
    (my-org-capture-set-todo-keyword)
    (buffer-string)))

(defun my-org-capture-extract-tree (url)
  "Generate Org capture tree extracted from URL.

Returns nil if there is no extractor for URL."
  (when-let ((insert-fn (my-org-extractor-insert-fn url)))
    (with-temp-buffer
      (funcall insert-fn url)
      (when my-org-capture-position-cursor
        (let ((case-fold-search))
          (org-back-to-heading)
          (looking-at org-heading-regexp)
          (goto-char (match-beginning 2))
          (insert "%?")))
      (my-org-capture-set-todo-keyword)
      (buffer-string))))

(defun my-org-capture-current-link (%:link &optional %:description %f %F)
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
    (or (my-org-capture-extract-tree link)
        (my-org-capture-tree
         ;; If DESCRIPTION is t, insert it as an empty string but in brackets.
         (if (eq description t)
             (cl-letf (((symbol-function 'org-string-nw-p) (lambda (_) t)))
               (org-make-link-string link ""))
           (org-make-link-string link description))))))

(defun my-org-capture-current-link-context ()
  (or (derived-mode-p 'gnus-article-mode 'w3m-mode)
      (buffer-file-name)
      (my-xdg-web-browser-buffer-p)))

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

(defun my-org-capture-new ()
  "New item"
  (my-org-capture-tree ""))

(defun my-org-capture-region (%i &optional %f %:from %:link %:subject)
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
    (concat (my-org-capture-tree subject)
            (when from
              (with-temp-buffer
                (insert "Captured from " from ":\n")
                (fill-region (point-min) (point-max))
                (buffer-string)))
            "#+BEGIN_QUOTE\n"
            body
            "#+END_QUOTE\n")))

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
          (my-org-capture-region \"%i\" \"%f\"
                                 \"%:from\" \"%:link\" \"%:subject\"))")
     ("c" ,(documentation 'my-org-capture-current-link) entry
      (file org-default-notes-file)
      "%(with-current-buffer (org-capture-get :original-buffer)
          (my-org-capture-current-link \"%:link\" \"%:description\"
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
      :empty-lines 1 :immediate-finish t :jump-to-captured t :no-save t)))
 '(org-capture-templates-contexts
   '(("r" (my-org-capture-region-context))
     ("c" (my-org-capture-current-link-context))
     ("k" (my-org-capture-killed-link-context))
     ("u" (my-org-capture-link-context))
     ("U" (my-org-capture-links-context)))))

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

(defvar my-org-audible-properties
  '(("By: " . "AUTHOR")
    ("Narrated by: " . "NARRATOR")
    ("Length: " . "DURATION"))
  "Mapping from property lines on the Audible website to Org properties.

Each element is a cons cell (PREFIX . PROPERTY).")

(defun my-org-audible-insert (url)
  "Insert headline for an Audible URL."
  (interactive "sURL: ")
  (let ((retry-count 0))
    (cl-loop
     (when (= retry-count 3)
       (error "Could not scrape %s" url))
     (let* ((hero-content (enlive-query (enlive-fetch url) [.hero-content]))
            (title (enlive-text (enlive-query hero-content [h1]))))
       (if (string-empty-p title)
           (progn (sleep-for .1)
                  (cl-incf retry-count))
         (org-insert-heading)
         (insert title)
         (org-set-tags ":audible:")
         (org-set-property "AUDIBLE" url)
         (dolist (span (enlive-query-all hero-content [.bc-row > .bc-text]))
           (pcase-let*
               ((text (string-trim (replace-regexp-in-string
                                    "[[:space:]\n]+" " "
                                    (enlive-text span))))
                (`(,property . ,value)
                 (seq-some (pcase-lambda (`(,prefix . ,property))
                             (when (string-prefix-p prefix text)
                               (cons property (string-remove-prefix prefix text))))
                           my-org-audible-properties)))
             (when property
               (org-set-property property value))))
         (cl-return))))))

(defvar my-org-goodreads-genre-limit 3
  "Maximum number of genres inserted into GENRES property.")

(defvar my-org-goodreads-properties
  '(("author" . "AUTHOR")
    ("numberOfPages" . "PAGES")
    ("isbn" . "ISBN")
    ("inLanguage" . "LANGUAGE"))
  "Mapping from itemprop html properties on the Goodreads website to Org properties.

Each element is a cons cell (ITEMPROP . PROPERTY).")

(defun my-org-goodreads-insert (url)
  "Insert headline for a Goodreads URL."
  (interactive "sURL: ")
  (let* ((page (enlive-fetch url))
         (metacol (enlive-get-element-by-id page "metacol")))
    (org-insert-heading)
    (insert (string-trim
             (enlive-text (enlive-get-element-by-id metacol "bookTitle"))))
    (org-set-tags ":book:")
    (org-set-property "GOODREADS" url)
    (let ((series
           (string-trim (enlive-text
                         (enlive-get-element-by-id metacol "bookSeries")))))
      (unless (string-empty-p series)
        (org-set-property
         "SERIES"
         (replace-regexp-in-string "\\`(\\(.*\\))\\'" "\\1" series))))
    (org-set-property
     "GENRES"
     (mapconcat #'enlive-text
                (seq-take (enlive-query-all page
                                            [.left > .bookPageGenreLink])
                          my-org-goodreads-genre-limit)
                ", "))
    (dolist (el (enlive-filter metacol
                               (lambda (el) (enlive-attr el 'itemprop))))
      (when-let ((property (cdr (assoc (enlive-attr el'itemprop)
                                       my-org-goodreads-properties))))
        (org-set-property
         property
         (string-trim (replace-regexp-in-string "[[:space:]\n]+" " "
                                                (enlive-text el))))))))

(defcustom my-org-extractors
  '(("AMAZON" "\\`https?://[^/]*\\.amazon\\.com/" ignore)
    ("AUDIBLE" "\\`https?://www\\.audible\\.com/pd/" my-org-audible-insert)
    ("GOODREADS" "\\`https?://www\\.goodreads\\.com/book/show/"
     my-org-goodreads-insert)
    ("IMDB" "\\`https?://www\\.imdb\\.com/title/" ignore)
    ("MYANIMELIST" "\\`https?://myanimelist\\.net/anime/" ignore))
  "List of extractors configured for URLs.

Each extractor is a triple (NAME URL-REGEXP INSERT-FN), where
URL-REGEXP specifies urls the extractor can operate on, and
INSERT-FN is a function that inserts the title and info from the
url into the current buffer as a headline with properties."
  :group 'my
  :type '(repeat (list (string :tag "Name")
                       (string :tag "URL Regexp")
                       (function :tag "Insert Function"))))

(defun my-org-extractor (url)
  "Get the extractor matching URL."
  (seq-some (pcase-lambda ((and extractor `(_ ,url-regexp _)))
              (and (string-match-p url-regexp url) extractor))
            my-org-extractors))

(defun my-org-extractor-name (url)
  "Get the name of the extractor matching URL."
  (first (my-org-extractor url)))

(defun my-org-extractor-insert-fn (url)
  "Get the insert function of the extractor matching URL."
  (when-let ((insert-fn (third (my-org-extractor url))))
    (unless (eq insert-fn 'ignore)
      insert-fn)))

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
 '(org-refile-targets '((org-files-list :maxlevel . 2)))
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
 '(org-todo-keywords '((type  "TODO(!)" "NEXT(!)" "WAIT(@)"
                              "|" "DONE(!)" "DROP(@)")
                       (sequence "PLAN" "|" "PASS" "FAIL")))
 '(org-todo-keyword-faces '(("PLAN" . "yellow")
                            ("PASS" . "green")
                            ("FAIL" . "red"))))

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
                      (my-org-extractor-name link)
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
              (and link (my-org-extractor-name link))
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
