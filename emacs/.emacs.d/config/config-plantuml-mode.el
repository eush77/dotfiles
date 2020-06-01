(custom-set-variables
 '(plantuml-default-exec-mode 'jar)
 '(plantuml-indent-level 4))

(define-advice plantuml-init-once (:after (&rest _) my-indent)
  "Remove activation and deactivation from start and end regexps."
  (delete plantuml-indent-regexp-activate-start plantuml-indent-regexp-start)
  (delete plantuml-indent-regexp-activate-end plantuml-indent-regexp-end))
