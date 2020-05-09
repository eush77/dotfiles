;;; Multiline headings

(defun my-outline-next-visible-heading--multiline (arg)
  "Move to the beginning of a multiline heading."
  (when (and (> arg 0) (not (eq (point) (point-max))))
    (forward-line (- (cl-count ?\n (match-string 0))))))
(advice-add 'outline-next-visible-heading
            :after 'my-outline-next-visible-heading--multiline)

;;; Commands

(defun my-outline-entry-invisible-p ()
  "Non-nil if the entry at point is invisible."
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (outline-invisible-p)))

;;;###autoload
(defun my-outline-show-next-subtree (count)
  "If the subtree under the current heading is hidden, show it.
Otherwise hide it, and show the next sibling subtree (or subtree
COUNT subtrees away, in either direction)."
  (interactive "p")
  (when (not (my-outline-entry-invisible-p))
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

(defun my-outline-overlay-start (point)
  "Return the start of an invisible overlay."
  (seq-min (mapcar
            #'overlay-start
            (seq-filter (lambda (ov) (overlay-get ov 'invisible))
                        (overlays-at point)))))

(defun my-outline-subtree-invisible-p ()
  "Non-nil if the subtree at point is (partially) invisible."
  (let* ((end (save-excursion
                (or (progn (condition-case nil
                               (outline-forward-same-level 1)
                             (error (outline-get-next-sibling)))
                           (when (equal (char-before) ?\n)
                             (backward-char))
                           ;; Skip back a top-level overlay.
                           (when (outline-invisible-p (1- (point)))
                             (let ((ov-start
                                    (my-outline-overlay-start (1- (point)))))
                               (when (zerop
                                      (save-excursion (goto-char ov-start)
                                                      (current-column)))
                                 (goto-char ov-start))))
                           (point))
                    (point-max)))))
    (seq-some (lambda (ov) (overlay-get ov 'invisible))
              (overlays-in (point) end))))

;;;###autoload
(defun my-outline-cycle-entry ()
  "Cycle through entry visibility states."
  (interactive)
  (if (my-outline-entry-invisible-p)
      (outline-show-entry)
    (outline-hide-entry)))

;;;###autoload
(defun my-outline-cycle-subtree ()
  "Cycle through subtree visibility states."
  (interactive)
  (cond ((my-outline-entry-invisible-p)
         (outline-show-entry)
         (outline-show-children))
        ((my-outline-subtree-invisible-p)
         (outline-show-subtree))
        (t (outline-hide-subtree))))

;;; Hydra

;;;###autoload (autoload 'my-outline-hydra/body "config-outline")
(defhydra my-outline-hydra (:body-pre (unless (eq major-mode 'outline-mode)
                                        (outline-minor-mode 1))
                            :foreign-keys run)
  "
%s(cond ((eq major-mode 'outline-mode) \"Outline\")
        (outline-minor-mode \"Outline minor\")
        (t \"No Outline\"))
"
  ("u" outline-up-heading "up" :column "Motion")
  ("b" outline-backward-same-level "backward")
  ("p" outline-previous-visible-heading "previous")
  ("n" outline-next-visible-heading "next")
  ("f" outline-forward-same-level "forward")
  ("@" outline-mark-subtree "mark subtree")

  ("e" my-outline-cycle-entry "cycle entry" :column "Local Visibility")
  ("TAB" my-outline-cycle-subtree "cycle subtree")
  ("P" my-outline-show-previous-subtree "show previous")
  ("N" my-outline-show-next-subtree "show next")

  ("l" outline-hide-sublevels "show level" :column "Global Visibility")
  ("t" (progn (outline-show-all) (outline-hide-body)) "show headings")
  ("o" outline-hide-other "hide other")
  ("a" outline-show-all "show all")

  ("^" outline-move-subtree-up "move up" :column "Editing")
  ("<" outline-promote "promote")
  (">" outline-demote "demote")
  ("v" outline-move-subtree-down "move down")
  ("m" outline-insert-heading "insert heading")

  ("`" (progn (outline-minor-mode 0)
              (my-hs-hydra/body)) "-> hideshow" :exit t :column "")
  ("q" nil "cancel"))

;;; Keymap

(define-key outline-minor-mode-map outline-minor-mode-prefix
  #'my-outline-hydra/body)
(define-key outline-minor-mode-map "\C-c\C-j"
  #'counsel-outline)
(define-key outline-mode-prefix-map (kbd "M-N")
  #'my-outline-show-next-subtree)
(define-key outline-mode-prefix-map (kbd "M-P")
  #'my-outline-show-previous-subtree)

;;; indent-for-tab-command integration

(defun my-indent-for-tab-command--outline-minor (func &rest args)
  "Cycle heading at point"
  (let ((point-before (point)))
    (apply func args)
    (when (and outline-minor-mode
               (= point-before (point))
               (outline-on-heading-p))
      (my-outline-cycle-subtree))))
(advice-add 'indent-for-tab-command
            :around #'my-indent-for-tab-command--outline-minor)
