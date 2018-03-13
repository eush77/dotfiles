(custom-set counsel-git-grep-skip-counting-lines t) ; Shave off extra latency.

(defun counsel-git-grep--projects (args)
  "Use `counsel-git-grep-projects' configuration when the
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
(advice-add 'counsel-git-grep :filter-args  #'counsel-git-grep--projects)
