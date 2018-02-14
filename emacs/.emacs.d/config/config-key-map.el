;; Browse-web.
(global-set-key (kbd "C-x M-3") #'browse-web)

;; Counsel overloads.
(define-key read-expression-map (kbd "C-r") #'counsel-expression-history)
(global-set-key [remap describe-function] #'counsel-describe-function)
(global-set-key [remap describe-variable] #'counsel-describe-variable)
(global-set-key [remap execute-extended-command] #'counsel-M-x)
(global-set-key [remap info-lookup-symbol] #'counsel-info-lookup-symbol)
(global-set-key [remap insert-char] #'counsel-unicode-char)

;; Expand-region.
(global-set-key (kbd "M-=") #'er/expand-region)

;; Frames.
(global-set-key (kbd "M-`") #'other-frame)

;; Git grep.
(global-set-key (kbd "C-x v /") #'counsel-git-grep)

;; Goto-last-change.
(global-set-key (kbd "M-_") #'goto-last-change)

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
(global-set-key (kbd "C-c M-n") #'my-duplicate-line-down)
(global-set-key (kbd "C-c M-p") #'my-duplicate-line-up)
(global-set-key (kbd "M-o") #'my-open-next-line)
(global-set-key (kbd "C-o") #'my-open-previous-line)

;; Magit.
(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-x M-g") #'magit-list-repositories)

;; Movement.
;; Unbinding `C-m' from `RET' does not work in a terminal.
(when window-system
  (define-key prog-mode-map (kbd "C-;") #'backward-paragraph)
  (define-key prog-mode-map (kbd "C-m") #'forward-paragraph)
  (define-key prog-mode-map (kbd "<return>") #'newline))

;; Multiple cursors.
(global-set-key (kbd "C-M-<down>") #'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<up>") #'mc/mark-previous-like-this)

;; Scroll lock.
(global-set-key (kbd "<Scroll_Lock>") #'scroll-lock-mode)

;; Scrolling.
(global-set-key (kbd "M-p") #'scroll-down)
(global-set-key (kbd "M-n") #'scroll-up)

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
