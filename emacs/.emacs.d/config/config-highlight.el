;;; Commands

(defun my-hlt-highlight-line ()
  "Highlight current line."
  (interactive)
  (hlt-highlight-region (line-beginning-position) (line-end-position)))

(defun my-hlt-unhighlight-line ()
  "Unhighlight current line."
  (interactive)
  (hlt-unhighlight-region (line-beginning-position) (line-end-position)))

(defun my-hlt-highlight ()
  "Highlight current line or active region."
  (interactive)
  (if (region-active-p)
      (hlt-highlight-region (region-beginning) (region-end))
    (my-hlt-highlight-line)))

(defun my-hlt-unhighlight ()
  "Unhighlight current line or active region."
  (interactive)
  (if (region-active-p)
      (hlt-unhighlight-region (region-beginning) (region-end))
    (my-hlt-unhighlight-line)))

;;; Hydra

(defvar my-hlt-highlight-hydra/mode 'move
  "State of `my-hlt-highlight-hydra/body':
  highlight - highlight lines
  unhighlight - unhighlight lines
  move - just move around
  jump - jump between highlights")

(defun my-hlt-highlight--set-hydra-mode ()
  "Set `my-hlt-highlight-hydra/mode'."
  (setq my-hlt-highlight-hydra/mode 'highlight))
(advice-add 'my-hlt-highlight
            :after #'my-hlt-highlight--set-hydra-mode)

(defun my-hlt-unhighlight--set-hydra-mode ()
  "Set `my-hlt-highlight-hydra/mode'."
  (setq my-hlt-highlight-hydra/mode 'unhighlight))
(advice-add 'my-hlt-unhighlight
            :after #'my-hlt-unhighlight--set-hydra-mode)

(defun my-hlt-move ()
  (interactive)
  "Set `my-hlt-highlight-hydra/mode' to \"just move around\"."
  (setq my-hlt-highlight-hydra/mode 'move))

(defun my-hlt-jump ()
  (interactive)
  "Set `my-hlt-highlight-hydra/mode' to \"jump between
highlights\"."
  (setq my-hlt-highlight-hydra/mode 'jump))

(defun my-hlt-next ()
  "Move to the next line according to
`my-hlt-highlight-hydra/mode'."
  (interactive)
  (if (eq my-hlt-highlight-hydra/mode 'jump)
      (hlt-next-highlight)
    (forward-line))
  (case my-hlt-highlight-hydra/mode
    ('highlight (my-hlt-highlight))
    ('unhighlight (my-hlt-unhighlight))))

(defun my-hlt-previous ()
  "Move to the previous line according to
`my-hlt-highlight-hydra/mode'."
  (interactive)
  (if (eq my-hlt-highlight-hydra/mode 'jump)
      (hlt-previous-highlight)
    (forward-line -1))
  (case my-hlt-highlight-hydra/mode
    ('highlight (my-hlt-highlight))
    ('unhighlight (my-hlt-unhighlight))))

;;;###autoload (autoload 'my-hlt-highlight-hydra/body "config-highlight")
(defhydra my-hlt-highlight-hydra (:foreign-keys run)
  "
%s(case my-hlt-highlight-hydra/mode
    ('highlight \"Highlight\")
    ('unhighlight \"Unhighlight\")
    ('move \"Move\")
    ('jump \"Jump\")) "
  ("h" my-hlt-highlight "highlight")
  ("u" my-hlt-unhighlight "unhighlight")
  ("g" my-hlt-move "move")
  ("j" my-hlt-jump "jump")
  ("n" my-hlt-next "next")
  ("p" my-hlt-previous "previous")
  ("q" nil))

;;; Keymap

(defun my-hlt-jump-next ()
  "Jump to next highlight."
  (interactive)
  (my-hlt-jump)
  (my-hlt-highlight-hydra/my-hlt-next))

(defun my-hlt-jump-previous ()
  "Jump to previous highlight."
  (interactive)
  (my-hlt-jump)
  (my-hlt-highlight-hydra/my-hlt-previous))

(define-key hlt-map (kbd "n") #'my-hlt-jump-next)
(define-key hlt-map (kbd "p") #'my-hlt-jump-previous)
(define-key hlt-map (kbd "hh") #'my-hlt-highlight-hydra/my-hlt-highlight)
(define-key hlt-map (kbd "hl") #'my-hlt-highlight-line)
(define-key hlt-map (kbd "uu") #'my-hlt-highlight-hydra/my-hlt-unhighlight)
(define-key hlt-map (kbd "ul") #'my-hlt-unhighlight-line)
