;; Interface & startup.
(custom-set frame-title-format "%b @ emacs")
(custom-set initial-scratch-message nil)
(blink-cursor-mode 0)
(column-number-mode 1)
(display-time-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(size-indication-mode 1)
(tool-bar-mode 0)

;; Backups.
(custom-set backup-directory-alist '(("." . "~/.emacs.d/backups/")))

;; Delete trailing space on save.
(add-hook 'before-save-hook
          (lambda ()
            (unless (eq major-mode 'diff-mode)
	      (delete-trailing-whitespace))))

;; Commenting style.
(custom-set comment-style 'extra-line)

;; Recursive minibuffer - use minibuffer while in minibuffer.
(custom-set enable-recursive-minibuffers t)

;; Paragraph filling.
(custom-set fill-column 78)

;; Indentation.
(custom-set indent-tabs-mode nil)

;; `quoted-insert' radix.
(custom-set read-quoted-char-radix 16)

;; Set a minimal scroll margin for `recenter-top-bottom'.
(custom-set scroll-margin 1)

;; Fix sentence boundaries (used for e.g. navigation commands
;; `backward-sentence' and `forward-sentence').
(custom-set sentence-end-double-space nil)

;; Ignore case when sorting lines.
(custom-set sort-fold-case t)

;; View mode.
(custom-set view-read-only t)

;; Enable basic modes.
(ivy-mode 1)
(pending-delete-mode 1)
(show-paren-mode 1)
(custom-set ivy-use-selectable-prompt t)
(custom-set ivy-use-virtual-buffers t)
