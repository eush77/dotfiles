;;; Saving

(defun my-narrow-reindent--before-save (&rest _)
  "Reindent the buffer if it is narrowed."
  (when (and narrow-reindent-mode (buffer-narrowed-p))
    (narrow-reindent--before-widen)))

(defun my-narrow-reindent--after-save (&rest _)
  "Reindent the buffer if it is narrowed."
  (when (and narrow-reindent-mode (buffer-narrowed-p))
    (narrow-reindent--after-narrow)))

(advice-add 'basic-save-buffer :before #'my-narrow-reindent--before-save)
(advice-add 'basic-save-buffer :after #'my-narrow-reindent--after-save)

;;; Widening

(advice-add 'narrow-reindent-widen :before-while #'buffer-narrowed-p)
