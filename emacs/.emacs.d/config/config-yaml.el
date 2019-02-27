;;; -*- lexical-binding: t; eval: (outline-minor-mode) -*-

;;; Command

(defun my-yaml-edit-indirect-block-literal ()
  (interactive)
  "Edit the block literal at point indirectly, or do nothing is
point is outside a block literal."
  (yaml-narrow-to-block-literal)
  (when (buffer-narrowed-p)
    (let* ((buffer (current-buffer))
           (display-buffer-overriding-action '(display-buffer-same-window))
           (edit-buffer (edit-indirect-region (point-min) (point-max) t)))
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (with-current-buffer buffer
                    (narrow-reindent-widen)))
                nil
                t))))

;;; Narrowing

(add-hook 'yaml-mode-hook 'narrow-reindent-mode)

(defun my-yaml-narrow-to-block-literal--reindent ()
  "Use `narrow-reindent-to-region'."
  (let ((begin (point-min))
        (end (point-max)))
    (widen)
    (narrow-reindent-to-region begin end)))
(advice-add 'yaml-narrow-to-block-literal
            :after #'my-yaml-narrow-to-block-literal--reindent)

;;; Keymap

(define-key yaml-mode-map (kbd "C-c C-w") #'my-yaml-edit-indirect-block-literal)
(define-key yaml-mode-map (kbd "C-x n b") #'yaml-narrow-to-block-literal)

;;; Units

(defun my-yaml-setup-units ()
  (setq-local page-delimiter "\n---\\( |\\)?\n")
  (setq-local paragraph-start "\\w+:")
  (setq-local paragraph-separate ":$"))

(add-hook 'yaml-mode-hook #'my-yaml-setup-units)
