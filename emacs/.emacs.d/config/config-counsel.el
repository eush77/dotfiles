(defun counsel-git-grep--projects (args)
  "Force using `counsel-git-grep-projects' configuration when
the `default-directory' resides in a project."
  (if (cl-find-if
       (lambda (x)
         (string-match (car x) (expand-file-name default-directory)))
       counsel-git-grep-projects-alist)
      (cons t (cdr args))
    args))
(advice-add 'counsel-git-grep :filter-args  #'counsel-git-grep--projects)
