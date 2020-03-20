;; Interface & startup
(custom-set-variables
 '(frame-title-format "%b @ emacs")
 '(initial-scratch-message nil))
(blink-cursor-mode 0)
(column-number-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(size-indication-mode 1)
(tool-bar-mode 0)

;; Backups
(custom-set-variables
 '(backup-directory-alist '(("." . "~/.emacs.d/backups/"))))

;; Delete trailing space on save.
(add-hook 'before-save-hook
          (lambda ()
            (unless (eq major-mode 'diff-mode)
              (delete-trailing-whitespace))))

;; Commenting style
(custom-set-variables
 '(comment-multi-line t)
 '(comment-style 'extra-line))

;; Input method
(custom-set-variables '(default-input-method "russian-computer"))

;; Recursive minibuffer - use minibuffer while in minibuffer.
(custom-set-variables '(enable-recursive-minibuffers t))

;; Do not expand HTTP URLs with `expand-file-name'. This is needed for opening
;; a URL with `emacsclient'.
(defun expand-file-name--http-url (func name &rest rest)
  "Do not expand HTTP URLs."
  (if (string-match "^https?://" name)
      name
    (apply func name rest)))
(advice-add 'expand-file-name :around #'expand-file-name--http-url)

;; Paragraph filling
(custom-set-variables '(fill-column 78))

;; Advice `find-file' to use `browse-url' to open HTTP URLs.
(defun find-file--browse-url (func filename &rest rest)
  "Open HTTP URLs with `browse-url'."
  (if (string-match "^https?://" filename)
      (browse-url filename)
    (apply func filename rest)))
(defun find-file-noselect--browse-url (func filename &rest rest)
  "Open HTTP URLs with `browse-url'."
  (if (string-match "^https?://" filename)
      (progn (browse-url filename) (current-buffer))
    (apply func filename rest)))
(advice-add 'find-file :around #'find-file--browse-url)
(advice-add 'find-file-noselect :around #'find-file-noselect--browse-url)

;; Advice `frame-text-cols' to deduct 1 when running in a terminal, since the
;; rightmost column is the margin column on a terminal.
(unless window-system
  (advice-add 'frame-text-cols :filter-return #'1-))

;; Indentation
(custom-set-variables '(indent-tabs-mode nil))

;; Enable narrowing.
(put 'narrow-to-region 'disabled nil)

;; `quoted-insert' radix
(custom-set-variables '(read-quoted-char-radix 16))

;; Enable region case conversion commands.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Set initial input in `rename-buffer'
(define-advice rename-buffer
    (:around (func name &optional unique) my-initial-input)
  "Set initial input in the minibuffer."
  (interactive
   (list (read-from-minibuffer
          "Rename buffer: "
          (if (string-match-p "\\`\\*.*\\*\\'" (buffer-name))
              "<"
            (buffer-name)))))
  (funcall func name unique))

;; Set a minimal scroll margin for `recenter-top-bottom'.
(custom-set-variables '(scroll-margin 1))

;; Fix sentence boundaries (used for e.g. navigation commands
;; `backward-sentence' and `forward-sentence').
(custom-set-variables '(sentence-end-double-space nil))

;; Repeatedly pop mark with `C-SPC' after single `C-u C-SPC'.
(custom-set-variables '(set-mark-command-repeat-pop t))

;; Ignore case when sorting lines.
(custom-set-variables '(sort-fold-case t))

;; yes-or-no-p
(defun yay-or-nay-p (prompt)
  "Like `yes-or-no-p', but asks for \"yay\" or \"nay\" instead."
  (catch 'yay-or-nay
    (while (not (pcase (read-string (concat prompt "(yay or nay) ") nil t)
                  ((or "yay" "yes") (throw 'yay-or-nay t))
                  ((or "nay" "no") (throw 'yay-or-nay nil))))
      (message "Please answer yay or nay.")
      (sit-for 0.5))))
(advice-add 'yes-or-no-p :override #'yay-or-nay-p)

;; Enable base modes.
(pending-delete-mode 1)
(show-paren-mode 1)
