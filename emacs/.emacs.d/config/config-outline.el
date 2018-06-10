;;;###autoload
(defun my-outline-show-next-subtree (count)
  "If the subtree under the current heading is hidden, show it.
Otherwise hide it, and show the next sibling subtree (or subtree
COUNT subtrees away, in either direction)."
  (interactive "p")
  (outline-back-to-heading)
  (outline-end-of-heading)
  (when (not (outline-invisible-p))
    (outline-hide-subtree)
    (let ((move-next (if (> count 0)
                         #'outline-forward-same-level
                       #'outline-backward-same-level))
          (count (abs count)))
      (dotimes (_ count)
        (funcall move-next 1))))
  (outline-show-entry)
  (outline-show-children)
  (outline-back-to-heading))

;;;###autoload
(defun my-outline-show-previous-subtree (count)
  "Like `my-outline-show-next-subtree', but in the opposite
direction. The meaning of COUNT is inverted."
  (interactive "p")
  (my-outline-show-next-subtree (- count)))

(define-key outline-mode-prefix-map (kbd "M-N")
  #'my-outline-show-next-subtree)
(define-key outline-mode-prefix-map (kbd "M-P")
  #'my-outline-show-previous-subtree)
