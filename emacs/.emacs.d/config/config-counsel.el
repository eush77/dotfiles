;;; -*- lexical-binding: t -*-

(counsel-mode 1)

;;; Ibuffer

(defun counsel--ibuffer-get-buffers@my-preselect (candidates)
  "Remove the current buffer from the list of candidates."
  (seq-remove (pcase-lambda (`(_ . ,buffer)) (eq buffer (current-buffer)))
              candidates))
(advice-add 'counsel--ibuffer-get-buffers
            :filter-return #'counsel--ibuffer-get-buffers@my-preselect)

;;;###autoload
(defcustom my-counsel-ibuffer-excluded-buffers
  '("\\`\\*Compile-Log\\*\\'"
    "\\`\\*Flycheck\\b")
  "Regexes of buffer names to exclude from `counsel-ibuffer'."
  :type '(repeat string)
  :group 'my)

(defun counsel--ibuffer-get-buffers@my-exclude (candidates)
  "Exclude `my-counsel-ibuffer-excluded-buffers'"
  (seq-remove (pcase-lambda (`(_ . ,buffer))
                (seq-some (lambda (regexp)
                            (string-match-p regexp
                                            (buffer-name buffer)))
                          my-counsel-ibuffer-excluded-buffers))
              candidates))
(advice-add 'counsel--ibuffer-get-buffers
            :filter-return #'counsel--ibuffer-get-buffers@my-exclude)

(defun my-counsel-ibuffer-visit-buffer--create (name)
  "Create buffer if it doesn't exist."
  (when (and (stringp name) (not (get-buffer name)))
    (when (yes-or-no-p "Buffer does not exist. Create? ")
      (switch-to-buffer name))
    t))

(advice-add 'counsel-ibuffer-visit-buffer
            :before-until #'my-counsel-ibuffer-visit-buffer--create)

(defun my-counsel-ibuffer--small-cases (func &rest args)
  "Don't complete buffer name if there aren't any options."
  (cl-letf* ((ivy-read-function (symbol-function 'ivy-read))
             ((symbol-function 'ivy-read)
              (lambda (prompt collection &rest args)
                (cl-case (length collection)
                  (0 (message "No other buffer"))
                  (1 (counsel-ibuffer-visit-buffer (car collection)))
                  (otherwise (apply ivy-read-function prompt collection args))))))
    (apply func args)))
(advice-add 'counsel-ibuffer :around #'my-counsel-ibuffer--small-cases)

;;;###autoload
(defun my-counsel-ibuffer-by-mode (mode)
  "Like `counsel-ibuffer', but filter by the given major MODE.

If called interactively, select the mode with completion if
passed a prefix argument, or use the major mode of the current
buffer otherwise."
  (interactive
   (list (if current-prefix-arg
             (intern (completing-read
                      "Major mode: "
                      (cl-remove-duplicates
                       (seq-map (apply-partially #'buffer-local-value
                                                 'major-mode)
                                (buffer-list)))))
           major-mode)))
  (require 'ibuffer)
  (require 'dash)
  (cl-letf* ((get-buffers-function
              (symbol-function 'counsel-ibuffer--get-buffers))
             ((symbol-function 'counsel-ibuffer--get-buffers)
              (lambda ()
                (--filter (eq (buffer-local-value 'major-mode (cdr it)) mode)
                          (funcall get-buffers-function)))))
    (counsel-ibuffer)))

;;; Finding files

(custom-set-variables
 '(counsel-file-jump-args '("." "-not" "-path" "'*\/.git*'"))
 '(counsel-find-file-at-point t)
 '(counsel-find-file-ignore-regexp "\\`\\.")
 '(counsel-preselect-current-file t))

;;; Git grep

;;;###autoload
(defun my-counsel-git-grep-at-point (&optional cmd)
  "Grep for the symbol at point or active region in the current
Git repository.

See `counsel-git-grep'."
  (interactive "P")
  (counsel-git-grep
   (if (region-active-p)
       (buffer-substring (region-beginning) (region-end))
     (thing-at-point 'symbol))
   nil
   cmd))

(defun my-counsel-git-grep-add-project (&rest directories)
  "Add configuration for a project with DIRECTORIES to
`counsel-git-grep-projects-alist'. When a new project is used,
Git Grep will be limited to DIRECTORIES."
  (let ((cmd (concat "git --no-pager "
                     "grep --line-number --no-color --ignore-case "
                     "-e \"%s\" -- "
                     (mapconcat #'identity directories " "))))
    (dolist (directory directories)
      (push (cons directory cmd) counsel-git-grep-projects-alist))))

(define-advice counsel-git-grep
    (:before (&optional initial-input initial-directory cmd) my-projects)
  "Use `counsel-git-grep-projects-alist' configuration when the
INITIAL-DIRECTORY resides in a project. With a prefix argument,
force using Git root instead.

If INITIAL-DIRECTORY does not reside in a project, the behavior
is unchanged.

INITIAL-DIRECTORY defaults to `default-directory'."
    (when (and (cl-find-if
                (lambda (x)
                  (string-match (car x)
                                (expand-file-name (or initial-directory
                                                      default-directory))))
                counsel-git-grep-projects-alist)
               (null cmd))
      (setq current-prefix-arg t)))

(define-advice counsel-git-grep (:filter-args (args) my-region)
  "Grep for an active region."
  (if (region-active-p)
      (cons (buffer-substring (mark) (point))
            (cdr args))
    args))

(defun my-counsel-git-grep--rg (func &rest args)
  "Fall back to `counsel-rg' if not in a Git project."
  (if (condition-case nil (counsel-locate-git-root) (error))
      (apply func args)
    (counsel-rg (cadr args))))
(advice-add 'counsel-git-grep
            :around #'my-counsel-git-grep--rg)

(defun my-counsel-git-grep-count-func-default--projects (func &rest args)
  "Dynamically change `default-directory' to the Git root.

This is necessary if `counsel-git-grep-projects-alist' includes
project directories that are not roots of corresponding Git
repositories."
  (let ((default-directory (counsel-locate-git-root)))
    (apply func args)))
(advice-add 'counsel--git-grep-count-func-default
            :around #'my-counsel-git-grep-count-func-default--projects)

;;; Org

(defun my-counsel-org-entity--default-action (func &rest args)
  "Insert Org entity by default."
  (cl-letf* ((ivy-read-function (symbol-function 'ivy-read))
             ((symbol-function 'ivy-read)
              (lambda (prompt collection &rest args)
                (apply ivy-read-function prompt collection
                       :action `(2 . ,(cdr (plist-get args :action)))
                       (org-plist-delete args :action)))))
    (apply func args)))
(advice-add 'counsel-org-entity
            :around #'my-counsel-org-entity--default-action)

;;;###autoload
(defun my-counsel-org-goto (arg)
  "If called without a prefix argument, call `counsel-org-goto'.

If called with a prefix argument, call `counsel-org-goto-all'."
  (interactive "P")
  (if arg
      (counsel-org-goto-all)
    (counsel-org-goto)))

(defun my-counsel-org-goto-action--push-mark (&rest args)
  "`push-mark' before jumping to location."
  (push-mark))
(advice-add 'counsel-org-goto-action
            :before #'my-counsel-org-goto-action--push-mark)

;;; counsel-rg

(defun my-counsel-rg--no-git-root (func &rest args)
  "Always search in `default-directory' by default."
  (cl-letf (((symbol-function 'counsel--git-root) #'ignore))
    (apply func args)))

(advice-add 'counsel-rg :around #'my-counsel-rg--no-git-root)
