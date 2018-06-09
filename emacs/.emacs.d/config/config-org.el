;;; -*- lexical-binding: t -*-
(require 'org-depend)

(custom-set org-todo-keywords
            '((sequence "TODO(!)" "WAIT(@)" "NEXT(!)" "|" "DONE(!)" "DROP(@)")
              (sequence "PLAN" "|" "PASS" "FAIL")))
(custom-set org-todo-keyword-faces
            '(("PLAN" . "yellow")
              ("PASS" . "green")
              ("FAIL" . "red")))
(custom-set org-agenda-custom-commands
            `(("p" "Planned tasks"
               ((my-org-agenda-planned-view "Weekly.org")
                (my-org-agenda-planned-view "Monthly.org")
                (my-org-agenda-planned-view "Quarterly.org")
                (my-org-agenda-planned-view "Yearly.org")
                (my-org-agenda-planned-view "5 Years.org")
                (my-org-agenda-planned-view "Life.org"))
               ((org-agenda-prefix-format "  ")))))
(custom-set org-default-priority ?C)
(custom-set org-lowest-priority ?D)
(custom-set org-global-properties '(("EFFORT_ALL" . "0 0:10 0:30 1:00 2:00")))
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
                        "#+END_QUOTE\n"))))
(custom-set org-columns-default-format
            "%32ITEM %TODO %1PRIORITY %4EFFORT{:} %4CLOCKSUM %CATEGORY %TAGS")
(custom-set org-agenda-span 'fortnight)
(custom-set org-agenda-todo-list-sublevels nil)
(custom-set org-archive-location "archive/%s::")
(custom-set org-enforce-todo-dependencies t)
(custom-set org-enforce-todo-checkbox-dependencies t)
(custom-set org-log-done 'time)
(custom-set org-log-into-drawer t)
(custom-set org-log-refile 'time)
(custom-set org-startup-indented t)

(add-hook 'org-mode-hook #'auto-fill-mode)

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

(let ((show-and-move
       (lambda (move-next)
         (outline-back-to-heading)
         (outline-end-of-heading)
         (when (not (outline-invisible-p))
           (outline-hide-subtree)
           (funcall move-next 1))
         (outline-show-entry)
         (outline-show-children)
         (outline-back-to-heading))))

  (defun my-outline-show-next-subtree ()
    "If the subtree under the current heading is hidden, show it.
Otherwise hide it, and show the next sibling subtree."
    (interactive)
    (funcall show-and-move #'outline-forward-same-level))

  (defun my-outline-show-previous-subtree ()
    "If the subtree under the current heading is hidden, show it.
Otherwise hide it, and show the previous sibling subtree."
    (interactive)
    (funcall show-and-move #'outline-backward-same-level)))

(define-key org-mode-map (kbd "C-c C-\\") #'org-toggle-link-display)
(define-key org-mode-map (kbd "C-c C-x C-p") #'org-pomodoro)
(define-key org-mode-map (kbd "C-c j") #'org-shiftmetaleft)
(define-key org-mode-map (kbd "C-c k") #'org-shiftmetaright)
(define-key org-mode-map (kbd "C-c n") #'org-metadown)
(define-key org-mode-map (kbd "C-c p") #'org-metaup)
(define-key org-mode-map (kbd "C-c u") #'org-remove-occur-highlights)
(define-key org-mode-map (kbd "C-M-b") #'org-previous-link)
(define-key org-mode-map (kbd "C-M-f") #'org-next-link)
(define-key org-mode-map (kbd "M-N") #'my-outline-show-next-subtree)
(define-key org-mode-map (kbd "M-P") #'my-outline-show-previous-subtree)
(define-key org-mode-map [remap org-goto] #'my-counsel-org-goto)

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

(with-eval-after-load "org-agenda"
  (defun my-org-todo-list--context-filter (func &rest args)
    "Filter contexts according to `my-org-agenda-context-filter'
in the `org-todo-list' agenda view."
    (let ((org-agenda-tag-filter-preset
           (append my-org-agenda-context-filter
                   org-agenda-tag-filter-preset)))
      (apply func args)))
  (advice-add 'org-todo-list :around #'my-org-todo-list--context-filter)

  (define-key org-agenda-mode-map (kbd "C-c C-j")
    #'counsel-org-agenda-headlines))

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

;;
;; Org Clock Hydra
;;

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
