(custom-set counsel-find-file-at-point t)
(custom-set counsel-preselect-current-file t)

(defun my-counsel-git-grep-at-point (&optional cmd)
  "Grep for the symbol at point in the current Git repository.

See `counsel-git-grep'."
  (interactive "P")
  (counsel-git-grep cmd (thing-at-point 'symbol)))

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

(defun my-counsel-git-grep-count-func-default--projects (func &rest args)
  "Dynamically change `default-directory' to the Git root.

This is necessary if `counsel-git-grep-projects-alist' includes
project directories that are not roots of corresponding Git
repositories."
  (let ((default-directory (counsel-locate-git-root)))
    (apply func args)))
(advice-add 'counsel--git-grep-count-func-default
            :around #'my-counsel-git-grep-count-func-default--projects)

(defun my-counsel-ibuffer--preselect (func &rest args)
  "Remove current buffer, effectively preselecting last buffer."
  (cl-assert (eq ibuffer-default-sorting-mode 'recency))
  (letf* ((ivy-read-function (symbol-function 'ivy-read))
          ((symbol-function 'ivy-read)
           (lambda (prompt collection &rest args)
             (apply ivy-read-function prompt (cdr collection) args))))
    (apply func args)))
(advice-add 'counsel-ibuffer :around #'my-counsel-ibuffer--preselect)
