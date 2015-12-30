;;; -*- lexical-binding: t -*-

;; Fish-shell is not POSIX-compliant. Thus Emacs should stick to Bash.
;; https://github.com/lee-dohm/emacs/blob/fee920d6ce0c119cb58a419740bc3baf6170/init.d/shell.el
(setq-default explicit-shell-file-name "/bin/bash")
(setq-default shell-file-name "/bin/bash")

;; FIX SHIFT-UP
;; from http://stackoverflow.com/questions/10871745/shift-up-arrow-doesnt-highlight-text-emacs-iterm2
;; and http://lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00211.html
(if (equal "xterm" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up]))
(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map "\e[1;2A" [S-up]))

(setq frame-title-format "%b @ emacs")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq initial-scratch-message nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 5)
(setq-default c-basic-offset 2)
(setq-default c-default-style "linux")
(setq-default comment-style 'extra-line)
(c-set-offset 'case-label '+)
(c-set-offset 'access-label -1)
(c-set-offset 'innamespace 0)
;; Use C99 one-line comments by default.
(add-hook 'c-mode-hook (lambda ()
                         (setq comment-start "// ")
                         (setq comment-end "")))

(defvar backup-dir "~/.emacs-backups")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
(setq auto-save nil)
(setq default-input-method 'russian-computer)

(add-hook 'emacs-startup-hook 'column-number-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq truncate-partial-width-windows nil)
(setq split-width-threshold 100)


;; Highlights current word on a page. Similar to "C-s C-w".
;; Meant to be used with mouse selects.
(defun highlight-word ()
  (interactive)
  (highlight-phrase (current-word)))

(defun unhighlight ()
  (interactive)
  (hi-lock-mode 0))


;; [etags]
;; Use case-sensitive search.
(setq-default tags-case-fold-search nil)

;; https://stackoverflow.com/questions/12074897/automatically-jump-to-tag-in-emacs
(defun find-tag-no-prompt ()
  "Jump to the tag at point without prompting."
  (interactive)
  (find-tag (find-tag-default)))

(global-set-key [double-mouse-1] 'find-tag-no-prompt)
(global-set-key [mouse-2] 'pop-tag-mark)

;; Meant to be overriden. Makes sense only for Emacs configs.
(global-set-key (kbd "C-x f")
                (lambda () (interactive)
                  (call-interactively 'eval-region)
                  (message "eval-region...ok")
                  (pop-mark)))

;; Bound to backward-kill-word by default.
(global-unset-key (kbd "C-<backspace>"))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("/break\\b[^.]*$" . gdb-script-mode))

;; markdown-mode populates auto-mode-alist entries when loaded.
;; In order to override those entries we need to load it right away.
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

;; [grep-mode]
(require 'grep)
;; Put "cchh" grep files alias before anything else. This will make
;; interactive grep choose it by default for C/C++ files, which is
;; what I usually want.
(let ((cchh (assoc "cchh" grep-files-aliases)))
  (setq grep-files-aliases
        (cons cchh (remove cchh grep-files-aliases))))

;; [vc-git-grep]
;; Advice this function to search recursively.
(defadvice grep-expand-template (around vc-git-grep-recursive activate)
  (if (not (string-match "^git .*grep " (ad-get-arg 0)))
      ad-do-it
    ; else
    (ad-set-arg 0 (replace-regexp-in-string
                   "<F>" "\"[F]<F>[/F]\"" (ad-get-arg 0)))
    ad-do-it
    (setq ad-return-value
          (replace-regexp-in-string
           "\\[F\\].*\\[/F\\]"
           (lambda (tmpl)
             (replace-regexp-in-string
              " +" "\" \""
              (replace-regexp-in-string
               "\"" ""
               (substring tmpl 3 -4))))
           ad-return-value))))

(defun compile-goto-error-no-switch ()
  "Select grep result but don't switch window."
  (interactive)
  (compile-goto-error)
  (other-window -1))

(define-key grep-mode-map (kbd "n") 'compilation-next-error)
(define-key grep-mode-map (kbd "p") 'compilation-previous-error)
(define-key grep-mode-map (kbd "M-n") 'next-error-no-select)
(define-key grep-mode-map (kbd "M-p") 'previous-error-no-select)
(define-key grep-mode-map (kbd "l") 'recenter-top-bottom)
(define-key grep-mode-map (kbd "<return>") 'compile-goto-error-no-switch)


(set 'load-path (cons "~/.emacs.d/modules" load-path))
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))

(pending-delete-mode)
(show-paren-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-highlight-level 3)
 '(js2-include-node-externs t)
 '(js2-indent-switch-body t)
 '(js2-missing-semi-one-line-override t)
 '(js2-skip-preprocessor-directives t)
 '(js2-strict-cond-assign-warning nil)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-trailing-comma-warning nil)
 '(js3-auto-indent-p t)
 '(js3-auto-insert-catch-block nil)
 '(js3-boring-indentation nil)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-enter-indents-newline t)
 '(js3-highlight-level 3)
 '(js3-include-gears-externs nil)
 '(js3-include-rhino-externs nil)
 '(js3-indent-dots t)
 '(js3-max-columns 100)
 '(js3-pretty-vars nil)
 '(js3-strict-cond-assign-warning nil)
 '(js3-strict-missing-semi-warning t)
 '(js3-strict-trailing-comma-warning nil)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js3-jsdoc-html-tag-delimiter-face ((t (:foreground "green"))))
 '(js3-jsdoc-html-tag-name-face ((t (:foreground "green"))))
 '(js3-jsdoc-tag-face ((t (:foreground "green"))))
 '(js3-jsdoc-type-face ((t (:foreground "green"))))
 '(js3-jsdoc-value-face ((t (:foreground "green")))))

(require 'sws-mode)
(require 'jade-mode)
(require 'stylus-mode)

(require 'ido)
(ido-mode t)
(require 'ido-hacks)
(ido-hacks-mode t)

(require 'goto-last-change)

(require 'dired-details)
(dired-details-install)

(require 'hideshow)
(require 'control-lock)

;; Define a minor mode with global always-on-top key bindings.
;; https://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs

(defvar k-minor-mode-map (make-keymap))

(define-key k-minor-mode-map (kbd "C-c C-c") 'comment-dwim)

(define-key k-minor-mode-map (kbd "M-.") 'find-tag-no-prompt)
(define-key k-minor-mode-map (kbd "M-;") 'vc-git-grep)

(define-key k-minor-mode-map (kbd "S-<down>") 'shrink-window)
(define-key k-minor-mode-map (kbd "S-<up>") 'enlarge-window)
(define-key k-minor-mode-map (kbd "S-<left>") 'shrink-window-horizontally)
(define-key k-minor-mode-map (kbd "S-<right>") 'enlarge-window-horizontally)

(define-key k-minor-mode-map (kbd "C-M-j") 'window-jump-left)
(define-key k-minor-mode-map (kbd "C-M-k") 'window-jump-right)
(define-key k-minor-mode-map (kbd "C-M-p") 'window-jump-up)
(define-key k-minor-mode-map (kbd "C-M-n") 'window-jump-down)

;; Preserve input history navigation key bindings.
(defun prefer-history-navigation (navfn histfn)
  (lambda () (interactive)
    (if (memq major-mode '(inferior-emacs-lisp-mode))
        (funcall histfn 1) (funcall navfn))))

(define-key k-minor-mode-map (kbd "M-p")
  (prefer-history-navigation 'scroll-down 'comint-previous-input))
(define-key k-minor-mode-map (kbd "M-n")
  (prefer-history-navigation 'scroll-up 'comint-next-input))

(defun jump-line (count)
  "Jump COUNT lines ahead or back."
  (lambda ()
    (interactive)
    (if (< count 0)
        (dotimes (_ (- count)) (previous-line))
      (dotimes (_ count) (next-line)))))

(define-key k-minor-mode-map (kbd "C-M-c") 'control-lock-toggle)
(define-key k-minor-mode-map (kbd "M-c") 'control-lock-toggle)

(define-key k-minor-mode-map (kbd "M-<right>") 'mc/edit-lines)
(define-key k-minor-mode-map (kbd "M-<down>") 'mc/mark-next-like-this)
(define-key k-minor-mode-map (kbd "M-<up>") 'mc/mark-previous-like-this)
(define-key k-minor-mode-map (kbd "C-M-<down>") 'mc/mark-all-like-this)
(define-key k-minor-mode-map (kbd "C-x M-SPC") 'set-rectangular-region-anchor)

(define-key k-minor-mode-map (kbd "C-S-b") 'backward-sexp)
(define-key k-minor-mode-map (kbd "C-S-f") 'forward-sexp)
(define-key k-minor-mode-map (kbd "M-=") 'er/expand-region)

;; Should be "C-S-`".
(define-key k-minor-mode-map (kbd "C-^") 'not-modified)
(define-key k-minor-mode-map (kbd "C-~") 'not-modified)
(define-key k-minor-mode-map (kbd "M-_") 'goto-last-change)

(defun with-hs (command &rest arguments)
  "Load hs-minor-mode if it isn't loaded yet."
  (lambda ()
    (interactive)
    (if (not hs-minor-mode) (hs-minor-mode t))
    (apply command arguments)))

(define-key k-minor-mode-map (kbd "C-c M-c") (with-hs 'hs-toggle-hiding))
(define-key k-minor-mode-map (kbd "C-c M-l") (with-hs 'hs-hide-level 1))
(define-key k-minor-mode-map (kbd "C-c M-a") (with-hs 'hs-hide-all))
(define-key k-minor-mode-map (kbd "C-c M-q") (with-hs 'hs-show-all))

(defun recentered (command)
  "Retain cursor position at the center of the screen after operation."
  (lambda ()
    (interactive)
    (funcall command)
    (recenter)))

(define-key k-minor-mode-map (kbd "<kp-begin>") 'recenter-top-bottom)
(define-key k-minor-mode-map (kbd "<kp-up>") (recentered 'previous-line))
(define-key k-minor-mode-map (kbd "<kp-down>") (recentered 'next-line))
(define-key k-minor-mode-map (kbd "<kp-prior>") (recentered 'scroll-down))
(define-key k-minor-mode-map (kbd "<kp-next>") (recentered 'scroll-up))
(define-key k-minor-mode-map (kbd "<up>") (recentered 'previous-line))
(define-key k-minor-mode-map (kbd "<down>") (recentered 'next-line))
(define-key k-minor-mode-map (kbd "<prior>") (recentered 'scroll-down))
(define-key k-minor-mode-map (kbd "<next>") (recentered 'scroll-up))

(define-key k-minor-mode-map (kbd "M-v")
  (lambda () (interactive)
    (insert (shell-command-to-string "xsel -b"))))

(define-key k-minor-mode-map (kbd "M-\\")
  (lambda () (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'shell-command-on-region))))

(define-minor-mode k-minor-mode
  "A minor mode so that my key settings work across all different major modes."
  t " +k" 'k-minor-mode-map)

(defvar k-minor-dangerous-mode-map (make-keymap))

(define-key k-minor-dangerous-mode-map (kbd "M-p") 'scroll-down)
(define-key k-minor-dangerous-mode-map (kbd "M-n") 'scroll-up)


;; C-m and C-[ are synonymous to ENTER and ESC.
;; In the terminal it isn't possible to work around it at all.
;; In graphical mode there is a hack to unbind C-m from ENTER
;; (presented below).
;
;; Initially I thought of C-m and C-[, alas there doesn't seem to exist
;; an easy way to unbind C-[ from ESC. Similar twist damages META key,
;; as if it was somehow involved.
(when window-system
  (define-key k-minor-dangerous-mode-map (kbd "C-;") 'backward-paragraph)
  (define-key k-minor-dangerous-mode-map (kbd "C-m") 'forward-paragraph)
  (define-key k-minor-dangerous-mode-map (kbd "<return>") 'newline))

(define-minor-mode k-minor-dangerous-mode
  "Extension to k-minor-mode that should not be applied to all major modes."
  t "" 'k-minor-dangerous-mode-map)

;; Preserve minibuffer history ring.
(add-hook 'minibuffer-setup-hook (apply-partially 'k-minor-mode 0))

(mapc
 (lambda (hook)
   (add-hook hook (apply-partially 'k-minor-dangerous-mode 0)))
 '(minibuffer-setup-hook
   dired-mode-hook
   custom-mode-hook
   ack-mode-hook
   grep-mode-hook
   help-mode-hook
   ielm-mode-hook
   package-menu-mode-hook
   ibuffer-mode-hook
   w3m-mode-hook
   eshell-mode-hook))

(k-minor-mode 1)
(k-minor-dangerous-mode 1)

(require 'dired)
(define-key dired-mode-map (kbd "SPC") 'dired-up-directory)

(defun balanced (command)
  "Retain window balance after operation."
  (lambda ()
    (interactive)
    (funcall command)
    (balance-windows)))

(defun safe-delete-frame () (interactive)
  (if (yes-or-no-p "Delete this frame? ")
      (delete-frame)))

(require 'key-chord)
(key-chord-mode 1)
;; Windows, frames, and buffers.
(key-chord-define-global "x1" 'delete-other-windows)
(key-chord-define-global "x2" (balanced 'split-window-below))
(key-chord-define-global "x3" (balanced 'split-window-right))
(key-chord-define-global "x0" (balanced 'delete-window))
(key-chord-define-global "5o" 'other-frame)
(key-chord-define-global "52" 'make-frame-command)
(key-chord-define-global "50" 'safe-delete-frame)
(key-chord-define-global "xb" 'ido-switch-buffer)
(key-chord-define-global "xk" 'ido-kill-buffer)
(key-chord-define-global "xs" 'save-buffer)
(key-chord-define-global "xf" 'ido-find-file)
(key-chord-define-global "xv" 'ido-find-alternate-file)

;; http://stackoverflow.com/a/12934513/2424184
(require 'font-lock)
(defun --copy-face (new-face face)
  "Define NEW-FACE from existing FACE."
  (copy-face face new-face)
  (eval `(defvar ,new-face nil))
  (set new-face new-face))
(--copy-face 'font-lock-label-face  ; labels, case, public, private, proteced, namespace-tags
         'font-lock-keyword-face)
(--copy-face 'font-lock-doc-markup-face ; comment markups such as Javadoc-tags
         'font-lock-doc-face)
(--copy-face 'font-lock-doc-string-face ; comment markups
         'font-lock-comment-face)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(add-hook 'c++-mode-hook
      '(lambda()
        (font-lock-add-keywords
         nil '(;; complete some fundamental keywords
           ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
           ;; add the new C++11 keywords
           ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
           ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
           ;; PREPROCESSOR_CONSTANT
           ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
           ;; hexadecimal numbers
           ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
           ;; integer/float/scientific numbers
           ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
           ;; user-types (customize!)
           ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|type\\|ptr\\)\\>" . font-lock-type-face)
           ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
           ))
        ) t)

(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

(when window-system
  (require 'solarized)
  (load-theme 'solarized-dark)
  (add-to-list 'default-frame-alist '(font . "Monoid 10"))
  (setq x-pointer-shape x-pointer-arrow))
