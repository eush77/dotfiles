(add-hook 'prog-mode-hook #'hs-minor-mode)

;;; Commands

(defun my-hs-block-hidden-p ()
  "Non-nil if the block at point is (partially) invisible."
  (let ((end (save-excursion
               (hs-find-block-beginning)
               (hs-looking-at-block-start-p)
               (hs-forward-sexp (match-data t) 1)
               (point))))
    (seq-some (lambda (ov) (overlay-get ov 'invisible))
              (overlays-in (point) end))))

;;;###autoload
(defun my-hs-cycle-block ()
  "Cycle through block visibility states."
  (interactive)
  (cond ((hs-already-hidden-p)
         (hs-show-block)
         (hs-hide-level 1))
        ((my-hs-block-hidden-p)
         (hs-show-block))
        (t (hs-hide-block))))

;;;###autoload
(defun my-hs-show-next-defun (count)
  "If current defun is hidden, show it. Otherwise hide it, and
show the next defun (or defun COUNT defuns away, in either
direction)."
  (interactive "p")
  (when (not (hs-already-hidden-p))
    (hs-hide-block)
    (let ((move-next (if (> count 0)
                         #'my-next-defun
                       #'my-previous-defun))
          (count (abs count)))
      (dotimes (_ count)
        (funcall move-next))))
  (hs-show-block)
  (hs-hide-level 1))

;;; Hydra

;;;###autoload (autoload 'my-hs-hydra/body "config-hideshow")
(defhydra my-hs-hydra (:body-pre (hs-minor-mode 1)
                       :foreign-keys run)
  "
Hideshow
"
  ("p" my-previous-defun "previous defun" :column "Motion")
  ("n" my-next-defun "next defun")

  ("TAB" my-hs-cycle-block "cycle" :column "Local Visibility")
  ("P" (my-hs-show-next-defun -1) "show previous")
  ("N" my-hs-show-next-defun "show next")

  ("l" hs-hide-level "hide level" :column "Global Visibility")
  ("c" hs-hide-initial-comment-block "header comment")
  ("h" hs-hide-all "hide all")
  ("a" hs-show-all "show all")

  ("`" (progn (hs-minor-mode 0)
              (my-outline-hydra/body)) "-> outline minor" :exit t :column "")
  ("q" nil "cancel" :column ""))

;;; Keymap

(with-eval-after-load "hideshow"
  (define-key hs-minor-mode-map (kbd "C-c @") #'my-hs-hydra/body))

;;; CC mode integration

(defun my-c-indent-command--hideshow (func &rest args)
  "Cycle block on ineffective indentation."
  (let ((point-before (point)))
    (apply func args)
    (when (and (= point-before (point))
               (or (looking-back "{") (looking-at "{")))
      (my-hs-cycle-block))))
(advice-add 'c-indent-command :around #'my-c-indent-command--hideshow)

;;; Emacs Lisp mode integration

(defun my-indent-for-tab-command--hideshow (func &rest args)
  "Cycle block on ineffective indentation."
  (let ((point-before (point)))
    (apply func args)
    (when (and (eq major-mode 'emacs-lisp-mode)
               (= point-before (point))
               (or (looking-back "(") (looking-at "(")))
      (my-hs-cycle-block))))
(advice-add 'indent-for-tab-command
            :around #'my-indent-for-tab-command--hideshow)

;;; Outline minor mode compat

(defun my-outline-minor-mode--hideshow (&optional arg)
  "Disable `hs-minor-mode' while `outline-minor-mode' is
enabled."
  (if arg
      (hs-minor-mode 0)
    (when (derived-mode-p 'prog-mode)
      (hs-minor-mode 1))))
(advice-add 'outline-minor-mode
            :before #'my-outline-minor-mode--hideshow)
