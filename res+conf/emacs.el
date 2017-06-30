;;; -*- lexical-binding: t -*-

;; Fish-shell is not POSIX-compliant. Thus Emacs should stick to Bash.
;; https://github.com/lee-dohm/emacs/blob/fee920d6ce0c119cb58a419740bc3baf6170/init.d/shell.el

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq custom-file "~/.emacs-custom")
(load custom-file)

(defmacro custom-set (var value)
  "Set VAR to VALUE using `custom-set-variables`.

  See [1] and [2].

[1]: https://emacs.stackexchange.com/questions/102/advantages-of-setting-variables-with-setq-instead-of-custom-el
[2]: https://stackoverflow.com/questions/2079095/how-to-modularize-an-emacs-configuration
"
  (custom-set-variables
   `(,var ,value nil nil
          ,(format "!!! CAREFUL: CUSTOM-SET IN %s !!!" load-file-name))))

;; Fix sentence-navigation commands (backward-sentence, forward-sentence).
(custom-set sentence-end-double-space nil)

(setq-default explicit-shell-file-name "/bin/bash")
;; Upd: use fish for access to $PATH and custom functions.
(setq-default shell-file-name "/usr/bin/fish")

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
(setq-default fill-column 78)
(setq-default indent-tabs-mode nil)
(setq-default read-quoted-char-radix 16)
(setq-default tab-width 5)
(setq-default c-basic-offset 4)
(setq-default c-default-style "linux")
(setq comment-style 'extra-line)
(c-set-offset 'case-label '+)
(c-set-offset 'access-label -1)
(c-set-offset 'innamespace 0)
(c-set-offset 'inline-open 0)
;; Use C99 one-line comments by default.
;; (add-hook 'c-mode-hook (lambda ()
;;                          (setq comment-start "// ")
;;                          (setq comment-end "")))

(add-hook 'c++-mode-hook (lambda () (setq c-basic-offset 2)))

;; Backups and auto-save.
(push (cons "." "~/.emacs.d/backups/") backup-directory-alist)
;(push (list "." "~/.emacs.d/backups/" t) auto-save-file-name-transforms)

;; Manual indentation
;; (defun dotab nil (interactive) (insert "    "))
;; (global-set-key (kbd "C-q") 'dotab)
;; (defun untab nil (interactive) (delete-backward-char 4))
;; (global-set-key (kbd "M-q") 'untab)
(add-hook 'emacs-startup-hook 'column-number-mode)
(add-hook 'before-save-hook
          (lambda ()
            (if (not (eq major-mode 'diff-mode))
                (delete-trailing-whitespace))))

(setq truncate-partial-width-windows nil)
(setq split-width-threshold 80)

(blink-cursor-mode -1)
;; (add-hook 'post-command-hook
;;           (lambda ()
;;             (setq cursor-type 'hbar)
;;             (set-cursor-color "yellow")))


;; Highlights current word on a page. Similar to "C-s C-w".
;; Meant to be used with mouse selects.
(defun highlight-word ()
  (interactive)
  (highlight-phrase (current-word)))

(defun unhighlight ()
  (interactive)
  (hi-lock-mode 0))


;; [macros]
(fset 'i (lambda (&optional arg) "Keyboard macro." (interactive "p")
           (kmacro-exec-ring-item
            (quote ("LLVMIntType()" 0 "%d")) arg)))
(fset 'pi (lambda (&optional arg) "Keyboard macro." (interactive "p")
            (kmacro-exec-ring-item
             (quote ("LLVMPointerType(LLVMIntType(), 0)"
                     0 "%d")) arg)))
(fset 'ci (lambda (&optional arg) "Keyboard macro." (interactive "p")
            (kmacro-exec-ring-item
             (quote ("LLVMConstInt(LLVMIntType(), "
                     0 "%d")) arg)))
(fset 'cn (lambda (&optional arg) "Keyboard macro." (interactive "p")
            (kmacro-exec-ring-item
             (quote ("LLVMConstNull(LLVMIntType())"
                     0 "%d")) arg)))
(fset 'c (lambda (&optional arg) "Keyboard macro." (interactive "p")
           (kmacro-exec-ring-item (quote ("/*  */" 0 "%d")) arg)))
(fset 'type (lambda (&optional arg) "Keyboard macro." (interactive "p")
              (kmacro-exec-ring-item (quote ("LLVMTypeRef" 0 "%d")) arg)))
(fset 'value (lambda (&optional arg) "Keyboard macro." (interactive "p")
               (kmacro-exec-ring-item (quote ("LLVMValueRef" 0 "%d")) arg)))
(fset 'b (lambda (&optional arg) "Keyboard macro." (interactive "p")
           (kmacro-exec-ring-item (quote ("LLVMBuild(builder, " 0 "%d")) arg)))
(fset 'l (lambda (&optional arg) "Keyboard macro." (interactive "p")
           (kmacro-exec-ring-item (quote ("LLVM" 0 "%d")) arg)))
(fset 'bb (lambda (&optional arg) "Keyboard macro." (interactive "p")
            (kmacro-exec-ring-item (quote ([134217848 99 return 134217834 98 108 111 99 107 134217834 167772192 backspace 134217839 134217848 108 return 80 111 115 134217775 40 134217775 44 32 98 108 111 99 107 95 98 98 41 59 134217826 134217826 67108896 134217830 M-up] 0 "%d")) arg)))
(fset 'bi (lambda (&optional arg) "Keyboard macro." (interactive "p")
            (kmacro-exec-ring-item (quote ([16 134217837 67108896 14 5 134217847 134217839 25 2 2 2 67108896 18 34 6 M-up] 0 "%d")) arg)))
(fset 'btt (lambda (&optional arg) "Keyboard macro." (interactive "p")
             (kmacro-exec-ring-item (quote ([?\C-e ?\C-\M-b ?\C-f ?, ?\C-b ?\C-  ?\C-e ?\M-% ?, return ?, ?  ?  ?\; ?\C-q ?\C-j ?  ?  ?d ?y ?n ?a ?m ?i ?c return ?! ?\C-e ?\C-b ?  ?  ?\; ?\C-j ?\C-e ?\C-\M-b ?\C-f ?\C-k ?\C-n ?\C-e] 0 "%d")) arg)))



;; [etags]
;; Use case-sensitive search.
(setq-default tags-case-fold-search nil)

;; https://stackoverflow.com/questions/12074897/automatically-jump-to-tag-in-emacs
(defun find-tag-no-prompt ()
  "Jump to the tag at point without prompting."
  (interactive)
  (find-tag (find-tag-default)))

(defun find-tag-no-prompt-next ()
  "Jump to the next tag in the current search."
  (interactive)
  (find-tag nil t))

;(global-set-key [mouse-1]
(global-set-key [double-mouse-1] 'find-tag-no-prompt)
(global-set-key [mouse-2] 'pop-tag-mark)
(global-set-key [drag-mouse-1] (lambda (click) (interactive "e")
                                 (mouse-set-secondary click)
                                 (message "up-mouse-1")))
(global-set-key [drag-mouse-1] 'mouse-set-secondary)


;; Meant to be overriden. Makes sense only for Emacs configs.
(global-set-key (kbd "C-x f")
                (lambda () (interactive)
                  (call-interactively 'eval-region)
                  (message "eval-region...ok")
                  (pop-mark)))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Compilation: save current buffer, recompile, then switch to results.
(global-set-key
 (kbd "<f7>")
 (lambda () (interactive)
   "Recompile and switch to compilation buffer."
   (unless (compilation-buffer-p (current-buffer))
     (save-buffer))
   (recompile)
   (unless (compilation-buffer-p (current-buffer))
     (switch-to-buffer-other-frame compilation-last-buffer))))

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("/break\\b[^.]*$" . gdb-script-mode))
(add-to-list 'auto-mode-alist '("\\.pdf$" . pdf-view-mode))


;; [pdf-view-mode]
(require 'pdf-tools)
(add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
(define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
(define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
(define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
(define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)


;; [image-mode]
(define-key image-mode-map (kbd "j") 'image-next-line)
(define-key image-mode-map (kbd "k") 'image-previous-line)
(define-key image-mode-map (kbd "h") 'image-backward-hscroll)
(define-key image-mode-map (kbd "l") 'image-forward-hscroll)


;; [Info-mode]
(define-key Info-mode-map (kbd "j") 'scroll-up-line)
(define-key Info-mode-map (kbd "k") 'scroll-down-line)


;; [magit]
(custom-set magit-repository-directories
            '(("/home/eush/dev/postgres_project/llvm_jit" . 0)
              ("/home/eush/dev/postgres_project/llvmmix" . 0)
              ("/home/eush/dev/postgres_project/postgres" . 0)))

;; [gnus]

;; [flycheck]
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
(add-hook 'after-init-hook #'global-flycheck-mode)


(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(require 'color-identifiers-mode)

;; Semantic highlighting - highlight identifiers, not syntax keywords.
(add-hook 'after-init-hook 'global-color-identifiers-mode)

;; Disable colorful syntax highlighting in color-identifiers-enabled buffers.
(let ((face-remapping-specs
       (lambda ()
         (let ((foreground (face-foreground 'default)))
           (mapcar
            (lambda (spec)
              (cons (car spec) (plist-put (cdr spec) :foreground foreground)))
            '((font-lock-builtin-face :slant normal :weight bold)
              (font-lock-comment-delimiter-face :slant italic :weight normal)
              (font-lock-comment-face :slant italic :weight normal)
              (font-lock-constant-face :slant normal :weight normal)
              (font-lock-doc-face :slant italic :weight normal)
              (font-lock-function-name-face :slant normal :weight normal)
              (font-lock-keyword-face :slant normal :weight bold)
              (font-lock-preprocessor-face :slant normal :weight bold)
              (font-lock-string-face :slant normal :weight normal)
              (font-lock-type-face :slant normal :weight normal)
              (font-lock-variable-name-face :slant normal :weight normal)
              (font-lock-warning-face :slant normal :weight normal))))))
      (per-buffer-face-remapping-cookies #s(hash-table test eq)))
  (dolist (hook '(after-load-theme-hook after-change-major-mode-hook))
    (add-hook hook
              (lambda ()
                (dolist (buffer (buffer-list))
                  (with-current-buffer buffer
                    (when (assoc major-mode color-identifiers:modes-alist)
                      (let ((entry
                             (gethash buffer
                                      per-buffer-face-remapping-cookies)))
                        (when (or (not entry) (not (eq major-mode (car entry))))
                          (when (and entry (not (eq major-mode (car entry))))
                            (mapc 'face-remap-remove-relative (cdr entry)))
                          (let ((cookies
                                 (mapcar (apply-partially
                                          'apply
                                          'face-remap-add-relative)
                                         (funcall face-remapping-specs))))
                            (puthash buffer (cons major-mode cookies)
                                     per-buffer-face-remapping-cookies))))))))))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (maphash (lambda (buffer entry)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (mapc 'face-remap-remove-relative (cdr entry)))))
                       per-buffer-face-remapping-cookies)
              (clrhash per-buffer-face-remapping-cookies))))


;; [grep-mode]
;; [compilation-mode]
(require 'grep)
;; Put "cchh" grep files alias before anything else. This will make
;; interactive grep choose it by default for C/C++ files, which is
;; what I ~~usually want~~ used to want.
;(let ((cchh (assoc "cchh" grep-files-aliases)))
;;  (setq grep-files-aliases
;;        (cons cchh (remove cchh grep-files-aliases))))

(defun compile-goto-error-no-switch ()
  "Select grep result but don't switch window."
  (interactive)
  (compile-goto-error)
  (other-window -1))

;; `grep-mode' and `compilation-mode' share most of keybindings.
(mapc
 (lambda (map)
   (define-key map (kbd "n") 'compilation-next-error)
   (define-key map (kbd "p") 'compilation-previous-error)
   (define-key map (kbd "M-n") 'next-error-no-select)
   (define-key map (kbd "M-p") 'previous-error-no-select)
   (define-key map (kbd "l") 'recenter-top-bottom))
 (list grep-mode-map
       compilation-mode-map))
(define-key grep-mode-map (kbd "<return>") 'compile-goto-error)
(define-key compilation-mode-map (kbd "<return>") 'compile-goto-error)

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

;; [helm]
;; Use case insensitive search.
(require 'helm-grep)
(setq-default helm-grep-git-grep-command
              (replace-regexp-in-string "\\bgrep\\b"
                                        "grep --ignore-case"
                                        helm-grep-git-grep-command))

;; [helm]
;; Use git-grep search over anything else.
(require 'helm-files)
(define-key helm-find-files-map (kbd "C-s") 'helm-ff-run-git-grep)
(define-key helm-find-files-map (kbd "C-/") 'helm-ff-run-find-sh-command)

(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)

;; [whitespace-mode]
;; (require 'whitespace)
;; (setq whitespace-line-column 78)
;; (setq whitespace-style '(face lines-tail))
;; (add-hook 'c-mode-hook (apply-partially 'whitespace-mode t))


(set 'load-path (cons "~/.emacs.d/modules" load-path))
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))

(pending-delete-mode)
(show-paren-mode)

;; (require 'less-mode)
;; (require 'sws-mode)

(require 'ido)
(ido-mode t)
(require 'ido-hacks)
(ido-hacks-mode t)
(require 'helm-config)
(require 'helm-ls-git)

(require 'goto-last-change)
(require 'haste)

(require 'dired)
(define-key dired-mode-map (kbd "SPC") 'dired-up-directory)

(require 'dired-details)
(dired-details-install)
(setq dired-dwim-target t)

;; [latex-mode]
(eval-after-load 'latex-mode
  '(define-key latex-mode-map (kbd "C-<return>") nil))

(require 'hideshow)
(require 'control-lock)

;; Change default zap-to-char behavior.
(require 'misc)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Vi-style open-line commands.
;;
;; http://www.emacswiki.org/emacs/OpenNextLine
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(defvar newline-and-indent t)

;; [sdcv]
(global-set-key (kbd "C-c M-s") 'sdcv-search-pointer+)
(global-set-key (kbd "C-c M-d") 'sdcv-search-input)
(setq sdcv-word-pronounce-command "true")

;; Define a minor mode with global always-on-top key bindings.
;; https://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs

(defvar k-minor-mode-map (make-keymap))

(define-key k-minor-mode-map (kbd "C-c C-c")
  (lambda () (interactive)
    (if (eq major-mode 'message-mode)
        (message-send-and-exit)
        (call-interactively #'comment-dwim))))

;; Probably S-M-SPC in window mode.
(define-key k-minor-mode-map (kbd "M-SPC") 'cycle-spacing)

(define-key k-minor-mode-map (kbd "M-,") 'find-tag)
(define-key k-minor-mode-map (kbd "M-.") 'find-tag-no-prompt)
(define-key k-minor-mode-map (kbd "C-M-.") 'find-tag-no-prompt-next)
(define-key k-minor-mode-map (kbd "M-*") 'pop-tag-mark)
(define-key k-minor-mode-map (kbd "M-;") 'vc-git-grep)

(define-key k-minor-mode-map (kbd "S-<down>") 'shrink-window)
(define-key k-minor-mode-map (kbd "S-<up>") 'enlarge-window)
(define-key k-minor-mode-map (kbd "S-<left>") 'shrink-window-horizontally)
(define-key k-minor-mode-map (kbd "S-<right>") 'enlarge-window-horizontally)

(define-key k-minor-mode-map (kbd "C-M-j") 'window-jump-left)
(define-key k-minor-mode-map (kbd "C-M-k") 'window-jump-right)
(define-key k-minor-mode-map (kbd "C-M-p") 'window-jump-up)
(define-key k-minor-mode-map (kbd "C-M-n") 'window-jump-down)

(setq eshell-prompt-function
      (lambda nil
        (concat (eshell/basename (eshell/pwd)) " $ ")))

(defun jump-line (count)
  "Jump COUNT lines ahead or back."
  (lambda ()
    (interactive)
    (if (< count 0)
        (dotimes (_ (- count)) (previous-line))
      (dotimes (_ count) (next-line)))))

(defvar jump-line-count 10)
;(define-key k-minor-mode-map (kbd "C-q") (jump-line (- jump-line-count)))
;(define-key k-minor-mode-map (kbd "C-v") (jump-line jump-line-count))

(define-key k-minor-mode-map (kbd "M-c") 'control-lock-toggle)

(define-key k-minor-mode-map (kbd "M-<right>") 'mc/edit-lines)
(define-key k-minor-mode-map (kbd "M-<down>") 'mc/mark-next-like-this)
(define-key k-minor-mode-map (kbd "M-<up>") 'mc/mark-previous-like-this)
(define-key k-minor-mode-map (kbd "C-M-<down>") 'mc/mark-all-like-this)
(define-key k-minor-mode-map (kbd "C-x M-SPC") 'set-rectangular-region-anchor)

(define-key k-minor-mode-map (kbd "C-c p") 'backward-paragraph)
(define-key k-minor-mode-map (kbd "C-c n") 'forward-paragraph)
(define-key k-minor-mode-map (kbd "C-S-b") 'backward-sexp)
(define-key k-minor-mode-map (kbd "C-S-f") 'forward-sexp)
(define-key k-minor-mode-map (kbd "M-=") 'er/expand-region)

(define-key k-minor-mode-map (kbd "M-_") 'goto-last-change)
(define-key k-minor-mode-map (kbd "C-o") 'open-previous-line)
(define-key k-minor-mode-map (kbd "M-o") 'open-next-line)

(defun duplicate-line-down ()
  "Duplicate line and move cursor to the second copy."
  (interactive)
  (next-line 1)
  (beginning-of-line)
  (copy-from-above-command)
  (newline)
  (previous-line))

(defun duplicate-line-up ()
  "Duplicate line and move cursor to the first copy."
  (interactive)
  (duplicate-line-down)
  (previous-line 1))

(define-key k-minor-mode-map (kbd "C-c M-p") 'duplicate-line-up)
(define-key k-minor-mode-map (kbd "C-c M-n") 'duplicate-line-down)

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
(define-key k-minor-mode-map (kbd "C-x g") 'magit-status)

(define-minor-mode k-minor-mode
  "A minor mode so that my key settings work across all different major modes."
  t " +k" 'k-minor-mode-map)

(defvar k-minor-dangerous-mode-map (make-keymap))

(define-key k-minor-dangerous-mode-map (kbd "M-p") 'scroll-down)
(define-key k-minor-dangerous-mode-map (kbd "M-n") 'scroll-up)

(define-key k-minor-dangerous-mode-map (kbd "M-v")
  (lambda () (interactive)
    (insert (shell-command-to-string "xsel -b"))))

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

(mapc
 (lambda (hook)
   (add-hook hook (apply-partially 'k-minor-dangerous-mode 0)))
 '(ack-mode-hook
   blackbox-mode-hook
   compilation-mode-hook
   cscope-list-entry-hook
   Custom-mode-hook
   dired-mode-hook
   doc-view-mode-hook
   eshell-mode-hook
   flycheck-error-list-mode-hook
   gnus-article-mode-hook
   gnus-group-mode-hook
   gnus-summary-mode-hook
   grep-mode-hook
   help-mode-hook
   ibuffer-mode-hook
   ielm-mode-hook
   Info-mode-hook
   isearch-mode-hook
   magit-blame-mode-hook
   magit-diff-mode-hook
   magit-log-mode-hook
   magit-popup-mode-hook
   magit-refs-mode-hook
   magit-repolist-mode-hook
   magit-revision-mode-hook
   magit-status-mode-hook
   Man-mode-hook
   minibuffer-setup-hook
   package-menu-mode-hook
   shell-mode-hook
   vc-git-log-view-mode-hook
   w3m-mode-hook))

(setq-default k-minor-dangerous-mode-stack nil)

(add-hook 'isearch-mode-hook
          (lambda ()
            (push (if k-minor-dangerous-mode 1 0) k-minor-dangerous-mode-stack)
            (k-minor-dangerous-mode 0)))

(add-hook 'isearch-mode-end-hook
          (lambda ()
            (k-minor-dangerous-mode (pop k-minor-dangerous-mode-stack))))

(k-minor-mode 1)
(k-minor-dangerous-mode 1)

;;
;; Keep windows balanced.
;;

(defun safe-delete-frame () (interactive)
  (if (yes-or-no-p "Delete this frame? ")
      (delete-frame)))

(defun balanced (fun)
  "Apply FUN and balances windows afterwards."
  (lambda () (interactive)
    (funcall fun)
    (balance-windows)))

(require 'key-chord)
(key-chord-mode 1)
;; Windows, frames, and buffers.
(key-chord-define-global "x1" #'delete-other-windows)
(key-chord-define-global "x2" (balanced #'split-window-below))
(key-chord-define-global "x3" (balanced #'split-window-right))
(key-chord-define-global "x0" (balanced #'delete-window))
(key-chord-define-global "5o" #'other-frame)
(key-chord-define-global "52" #'make-frame-command)
(key-chord-define-global "50" #'safe-delete-frame)
(key-chord-define-global "xb" #'ido-switch-buffer)
(key-chord-define-global "xk" #'ido-kill-buffer)
(key-chord-define-global "xs" #'save-buffer)
(key-chord-define-global "xf" #'helm-find-files)
(key-chord-define-global "xg" #'helm-ls-git-ls)
(key-chord-define-global "xv" #'ido-find-alternate-file)

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

;; (defadvice load-theme (before theme-dont-propagate activate)
;;   (mapc #'disable-theme custom-enabled-themes))

(when window-system
  (load-theme 'sanityinc-solarized-dark)
  (add-to-list 'default-frame-alist '(font . "Terminus 10"))
  (setq x-pointer-shape x-pointer-arrow))
(put 'narrow-to-region 'disabled nil)
