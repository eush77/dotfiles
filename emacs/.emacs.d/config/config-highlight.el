;;; -*- lexical-binding: t; eval: (outline-minor-mode) -*-

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

(defun my-hlt-get-highlights ()
  "Get list of all `(start . end)' positions of highlights in the
current buffer, in order. "
  (let ((hlt-regions
         ;; All highlight regions (represented as pairs) in reverse order
         (seq-map (lambda (ov)
                    (cons (overlay-start ov) (overlay-end ov)))
                  (seq-sort-by #'overlay-start #'>
                               (seq-filter (lambda (ov)
                                             (eq (overlay-get ov 'face)
                                                 'highlight))
                                           (overlays-in (point-min)
                                                        (point-max)))))))
    ;; Merge adjacent highlight regions
    (seq-reduce
     (lambda (hlts hlt)
       (if (and hlts
                (or (= (cdr hlt) (caar hlts))
                    (and (= (char-after (cdr hlt)) ?\n)
                         (= (+ (cdr hlt) 1) (caar hlts)))))
           (cons (cons (car hlt) (cdar hlts))
                 (cdr hlts))
         (cons hlt hlts)))
     hlt-regions
     nil)))

(defun my-hlt-insert-highlight (buffer hlt)
  "Insert highlight HLT from BUFFER."
  (let ((beg (point)))
    (insert (with-current-buffer buffer
              (format "%s:%d\n%s"
                      (buffer-name)
                      (line-number-at-pos (car hlt))
                      (buffer-substring-no-properties (car hlt)
                                                      (cdr hlt)))))
    (save-excursion
      (goto-char beg)
      (make-text-button
       (point) (line-beginning-position 2)
       'invisible t
       'help-echo (format "mouse-2, RET: Go to highlight at line %d"
                          (line-number-at-pos (car hlt)))
       'action (lambda (_btn)
                 (set-buffer buffer)
                 (goto-char (car hlt))
                 (display-buffer buffer '((display-buffer-same-window))))))))

(defun my-hlt-toggle-highlight-positions ()
  "Hide or show highlight positions in the current *highlights* buffer."
  (interactive)
  (setq buffer-invisibility-spec (not buffer-invisibility-spec))
  (redraw-frame))

(defun my-hlt-highlights--filter-buffer-substring (begin end &optional _delete)
  (let ((points))
    (while (< begin end)
      (push end points)
      (setq end (previous-single-char-property-change end 'button)))
    (unless (get-char-property begin 'button)
      (push begin points))
    (mapconcat (lambda (range)
                 (if (= (length range) 2)
                     (buffer-substring (car range) (cadr range))
                   ""))
               (seq-partition points 2)
               "")))

(defun my-hlt-list-highlights ()
  "Display all highlights of the current buffer."
  (interactive)
  (let ((buffer (current-buffer))
        (highlights (my-hlt-get-highlights))
        (point (point))
        ;; Point in the *highlights* buffer
        (hlt-point))
    (with-current-buffer (get-buffer-create "*highlights*")
      (setq buffer-read-only t)

      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "TAB") #'forward-button)
        (define-key map (kbd "<backtab>") #'backward-button)
        (define-key map (kbd "n") #'forward-button)
        (define-key map (kbd "p") #'backward-button)
        (define-key map (kbd "h") #'my-hlt-toggle-highlight-positions)
        (define-key map (kbd "q") #'quit-window)
        (use-local-map map))

      (setq-local filter-buffer-substring-function
                  #'my-hlt-highlights--filter-buffer-substring)

      (let ((buffer-read-only nil))
        (erase-buffer)
        (my-hlt-insert-highlight buffer (car highlights))
        (seq-do (lambda (hlt)
                  (insert "\n\n")
                  (when (and (not hlt-point) (<= point (cdr hlt)))
                    (setq hlt-point (point)))
                  (my-hlt-insert-highlight buffer hlt))
                (cdr highlights)))

      (goto-char hlt-point)
      (display-buffer (current-buffer) '((display-buffer-same-window))))))

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

(define-key hlt-map (kbd "l") #'my-hlt-list-highlights)
(define-key hlt-map (kbd "n") #'my-hlt-jump-next)
(define-key hlt-map (kbd "p") #'my-hlt-jump-previous)
(define-key hlt-map (kbd "hh") #'my-hlt-highlight-hydra/my-hlt-highlight)
(define-key hlt-map (kbd "hl") #'my-hlt-highlight-line)
(define-key hlt-map (kbd "uu") #'my-hlt-highlight-hydra/my-hlt-unhighlight)
(define-key hlt-map (kbd "ul") #'my-hlt-unhighlight-line)
