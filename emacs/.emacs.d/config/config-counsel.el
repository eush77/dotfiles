(counsel-mode 1)

;;; Ibuffer

(defun my-counsel-ibuffer--preselect (func &rest args)
  "Remove current buffer, effectively preselecting last buffer."
  (require 'ibuffer)
  (cl-assert (eq ibuffer-default-sorting-mode 'recency))
  (cl-letf* ((ivy-read-function (symbol-function 'ivy-read))
             ((symbol-function 'ivy-read)
              (lambda (prompt collection &rest args)
                (apply ivy-read-function prompt (cdr collection) args))))
    (apply func args)))
(advice-add 'counsel-ibuffer :around #'my-counsel-ibuffer--preselect)

;;; Finding files

(custom-set counsel-find-file-at-point t)
(custom-set counsel-preselect-current-file t)

;;; Git grep

;;;###autoload
(defun my-counsel-git-grep-at-point (&optional cmd)
  "Grep for the symbol at point or active region in the current
Git repository.

See `counsel-git-grep'."
  (interactive "P")
  (if (region-active-p)
      (counsel-git-grep cmd (buffer-substring (region-beginning)
                                              (region-end)))
    (counsel-git-grep cmd (thing-at-point 'symbol))))

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

(defun my-counsel-git-grep--projects (args)
  "Use `counsel-git-grep-projects-alist' configuration when the
`default-directory' resides in a project. With a prefix argument,
force using Git root instead.

If `default-directory' does not reside in a project, the behavior
is unchanged."
  (if (cl-find-if
       (lambda (x)
         (string-match (car x) (expand-file-name default-directory)))
       counsel-git-grep-projects-alist)
      (cons (not (car args)) (cdr args))
    args))
(advice-add 'counsel-git-grep
            :filter-args #'my-counsel-git-grep--projects)

(defun my-counsel-git-grep--region (args)
  "Grep for an active region."
  (if (region-active-p)
      (list (car args)
            (buffer-substring (mark) (point)))
    args))
(advice-add 'counsel-git-grep
            :filter-args #'my-counsel-git-grep--region)

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
