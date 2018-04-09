;; Case conversion.
(global-set-key (kbd "C-x M-c") #'capitalize-region)
(global-set-key (kbd "C-x M-l") #'downcase-region)
(global-set-key (kbd "C-x M-u") #'upcase-region)

;; Counsel overloads.
(define-key read-expression-map (kbd "C-r") #'counsel-expression-history)
(global-set-key [remap describe-function] #'counsel-describe-function)
(global-set-key [remap describe-variable] #'counsel-describe-variable)
(global-set-key [remap execute-extended-command] #'counsel-M-x)
(global-set-key [remap info-lookup-symbol] #'counsel-info-lookup-symbol)
(global-set-key [remap insert-char] #'counsel-unicode-char)

;; Expand-region.
(global-set-key (kbd "M-=") #'er/expand-region)
(global-set-key (kbd "M-+") #'er/mark-sentence)

;; Frames.
(global-set-key (kbd "M-`") #'my-select-frame-by-open-buffers)

;; Git grep.
(global-set-key (kbd "C-x v /") #'counsel-git-grep)
(global-set-key (kbd "C-x v ?") #'my-counsel-git-grep-at-point)

;; Global shortcuts.
(global-set-key (kbd "C-x M-1") #'eshell)
(global-set-key (kbd "C-x M-2") #'gnus)
(global-set-key (kbd "C-x M-3") #'browse-web)
(global-set-key (kbd "C-x M-4") #'pocket-reader)

;; Goto-last-change.
(global-set-key (kbd "M-_") #'goto-last-change)

;; Hippie-expand.
(global-set-key [remap dabbrev-expand] #'hippie-expand)

;; IBuffer.
(global-set-key [remap list-buffers] #'ibuffer)

;; Ivy.
(global-set-key (kbd "C-x C-r") #'ivy-resume)
(define-key ivy-minibuffer-map (kbd "C-w") #'ivy-yank-word)

;; Keyboard-escape-quit.
;; `C-[ C-[ C-[' is too easy to hit by an accident.
(global-unset-key (kbd "C-[ C-[ C-["))
(global-set-key (kbd "M-C-]") #'keyboard-escape-quit)

;; Line editing.
(global-set-key (kbd "M-o") #'my-open-next-line)
(global-set-key (kbd "C-o") #'my-open-previous-line)

;; Magit.
(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-x M-g") #'magit-list-repositories)

;; Man.
(global-set-key (kbd "C-h M") #'man)

;; Movement.
;; Unbinding `C-m' from `RET' does not work in a terminal.
(when window-system
  (define-key prog-mode-map (kbd "C-;") #'backward-paragraph)
  (define-key prog-mode-map (kbd "C-m") #'forward-paragraph)
  (define-key prog-mode-map (kbd "<return>") #'newline))

;; Multiple cursors.
(global-set-key (kbd "C-M-<down>") #'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<up>") #'mc/mark-previous-like-this)

;; Org.
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)

;; Rotate.
(global-set-key (kbd "C-c r r") #'rotate-window)
(global-set-key (kbd "C-c r eh") #'rotate:even-horizontal)
(global-set-key (kbd "C-c r ev") #'rotate:even-vertical)
(global-set-key (kbd "C-c r h") #'rotate:main-horizontal)
(global-set-key (kbd "C-c r m") #'rotate:main-vertical)
(global-set-key (kbd "C-c r t") #'rotate:tiled)

;; Scroll lock.
(global-set-key (kbd "<Scroll_Lock>") #'scroll-lock-mode)

;; Scrolling.
(global-set-key (kbd "M-p") #'scroll-down)
(global-set-key (kbd "M-n") #'scroll-up)

;; Smartparens.
(global-set-key (kbd "C-c C-a") #'sp-beginning-of-sexp)
(global-set-key (kbd "C-c C-b") #'sp-backward-sexp)
(global-set-key (kbd "C-c C-d") #'sp-kill-sexp)
(global-set-key (kbd "C-c C-e") #'sp-end-of-sexp)
(global-set-key (kbd "C-c C-f") #'sp-forward-sexp)
(global-set-key (kbd "C-c C-M-b") #'sp-backward-slurp-sexp)
(global-set-key (kbd "C-c C-M-f") #'sp-forward-slurp-sexp)
(global-set-key (kbd "C-c C-M-j") #'sp-join-sexp)
(global-set-key (kbd "C-c C-M-r") #'sp-raise-sexp)
(global-set-key (kbd "C-c C-M-s") #'sp-split-sexp)
(global-set-key (kbd "C-c C-M-t") #'sp-splice-sexp)
(global-set-key (kbd "C-c C-SPC") #'sp-mark-sexp)
(global-set-key (kbd "C-c C-u C-M-b") #'sp-backward-barf-sexp)
(global-set-key (kbd "C-c C-u C-M-f") #'sp-forward-barf-sexp)
(global-set-key (kbd "C-c M-n") #'my-forward-duplicate-sexp-or-line)
(global-set-key (kbd "C-c M-p") #'my-backward-duplicate-sexp-or-line)
(global-set-key (kbd "C-M-b") #'sp-previous-sexp)
(global-set-key (kbd "C-M-d") #'sp-down-sexp)
(global-set-key (kbd "C-M-f") #'sp-next-sexp)
(global-set-key (kbd "C-M-r") #'sp-backward-down-sexp)
(global-set-key (kbd "C-M-u") #'sp-backward-up-sexp)
(global-set-key (kbd "C-M-y") #'sp-up-sexp)
(global-set-key (kbd "M-(") #'my-sp-wrap-with-pair)

;; Swap buffers.
(global-set-key (kbd "C-c b") #'swap-buffers)

;; Swiper.
;; Complementary to `isearch-backward' on C-r.
(global-set-key (kbd "C-s") #'swiper)
(global-set-key (kbd "C-S-s") #'swiper-all)
(global-set-key (kbd "C-M-s") #'swiper-multi)

;; View mode.
(global-set-key (kbd "<pause>") #'view-mode)
(when (not window-system)
  (define-key key-translation-map (kbd "M-[ P") (kbd "<pause>")))

;; Window jump.
(global-set-key (kbd "C-M-j") #'window-jump-left)
(global-set-key (kbd "C-M-k") #'window-jump-right)
(global-set-key (kbd "C-M-p") #'window-jump-up)
(global-set-key (kbd "C-M-n") #'window-jump-down)

;; Window sizing.
(global-set-key [remap enlarge-window] #'my-enlarge-window)
(global-set-key [remap enlarge-window-horizontally]
                #'my-enlarge-window-horizontally)
(global-set-key [remap shrink-window-horizontally]
                #'my-shrink-window-horizontally)

;; Whitespace.
(global-set-key [remap just-one-space] #'cycle-spacing)

;; Zap-to-char behavior.
(global-set-key [remap zap-to-char] #'zap-up-to-char)
