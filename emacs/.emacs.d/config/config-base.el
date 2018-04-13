;; Interface & startup.
(custom-set frame-title-format "%b @ emacs")
(custom-set initial-scratch-message nil)
(blink-cursor-mode 0)
(column-number-mode 1)
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
(custom-set comment-multi-line t)
(custom-set comment-style 'extra-line)

;; Input method.
(custom-set default-input-method "russian-computer")

;; Recursive minibuffer - use minibuffer while in minibuffer.
(custom-set enable-recursive-minibuffers t)

;; Do not expand HTTP URLs with `expand-file-name'. This is needed for opening
;; a URL with `emacsclient'.
(defun expand-file-name--http-url (func name &rest rest)
  "Do not expand HTTP URLs."
  (if (string-match "^https?://" name)
      name
    (apply func name rest)))
(advice-add 'expand-file-name :around #'expand-file-name--http-url)

;; Paragraph filling.
(custom-set fill-column 78)

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

;; Indentation.
(custom-set indent-tabs-mode nil)

;; Minibuffer line.
(custom-set minibuffer-line-format
            '(:eval (let ((time-string (format-time-string "%R")))
                      (concat (make-string (- (frame-text-cols)
                                              (string-width time-string)
                                              1)
                                           ? )
                              time-string))))
(defun keyboard-quit--minibuffer-line (func)
  "Restore minibuffer line after quit."
  (condition-case nil (funcall func) (quit (minibuffer-line--update))))
(advice-add 'keyboard-quit :around #'keyboard-quit--minibuffer-line)

;; Mode line.
(custom-set mode-line-format '("%e"
                               mode-line-front-space
                               mode-line-mule-info
                               mode-line-client
                               mode-line-modified
                               mode-line-remote
                               mode-line-frame-identification
                               mode-line-buffer-identification
                               "   "
                               mode-line-position
                               (vc-mode vc-mode)
                               "  "
                               mode-line-modes
                               mode-line-misc-info
                               mode-line-end-spaces))
(custom-set rm-blacklist '(" $"         ; rich-minority-mode
                           " FlyC-"     ; flycheck-mode (no-checker)
                           " Guide"     ; guide-key-mode
                           " ivy"       ; ivy-mode
                           "[ln]"))     ; w3m-lnum-mode
(custom-set sml/mode-width 'right)
(custom-set sml/name-width 20)
(custom-set sml/position-percentage-format "")
(custom-set sml/prefix-face-list '(("" sml/prefix)))
(custom-set sml/replacer-regexp-list
            '(("^~/\\.emacs\\.d/elpa/" ":elpa:")
              ("^~/Dropbox/notes/" ":notes:")
              ("^~/Dropbox/org/" ":org:")
              ("^~/src/" ":src:")
              ("^:src:\\([^/]\\)[^/]*/" ":src/\\1:")
              ("^:src/\\(.\\):\\([^/]+\\)/" ":\\1/\\2:")))
(custom-set sml/size-indication-format "%p of %I ")
(custom-set sml/theme 'respectful)

;; Enable narrowing.
(put 'narrow-to-region 'disabled nil)

;; `quoted-insert' radix.
(custom-set read-quoted-char-radix 16)

;; Enable region case conversion commands.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Set a minimal scroll margin for `recenter-top-bottom'.
(custom-set scroll-margin 1)

;; Fix sentence boundaries (used for e.g. navigation commands
;; `backward-sentence' and `forward-sentence').
(custom-set sentence-end-double-space nil)

;; Repeatedly pop mark with `C-SPC' after single `C-u C-SPC'.
(custom-set set-mark-command-repeat-pop t)

;; Ignore case when sorting lines.
(custom-set sort-fold-case t)

;; View mode.
(custom-set view-read-only t)

;; Enable base modes.
(minibuffer-line-mode 1)
(pending-delete-mode 1)
(show-paren-mode 1)
(sml/setup)                             ; Uses `rich-minority-mode' internally.
