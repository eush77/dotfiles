(define-advice quit-window (:before (&rest _) my-unfollow)
  "Disable `follow-mode'."
  (follow-mode 0))
