;; Counsel overloads.
(define-key read-expression-map (kbd "C-r") #'counsel-expression-history)
(my-global-redefine-key 'describe-function #'counsel-describe-function)
(my-global-redefine-key 'describe-variable #'counsel-describe-variable)
(my-global-redefine-key 'execute-extended-command #'counsel-M-x)
(my-global-redefine-key 'info-lookup-symbol #'counsel-info-lookup-symbol)
(my-global-redefine-key 'insert-char #'counsel-unicode-char)

;; Expand-region.
(global-set-key (kbd "M-=") #'er/expand-region)

;; Frames.
(global-set-key (kbd "M-`") #'other-frame)

;; IBuffer.
(my-global-redefine-key 'list-buffers #'ibuffer)

;; Ivy.
(global-set-key (kbd "C-c C-r") #'ivy-resume)
(define-key ivy-minibuffer-map (kbd "C-w") #'ivy-yank-word)

;; Line editing.
(global-set-key (kbd "C-c M-n") #'my-duplicate-line-down)
(global-set-key (kbd "C-c M-p") #'my-duplicate-line-up)
(global-set-key (kbd "M-o") #'my-open-next-line)
(global-set-key (kbd "C-o") #'my-open-previous-line)

;; Magit.
(global-set-key (kbd "C-x g") #'magit-status)

;; Movement.
;; Unbinding `C-m' from `RET' does not work in a terminal.
(when window-system
  (define-key prog-mode-map (kbd "C-;") #'backward-paragraph)
  (define-key prog-mode-map (kbd "C-m") #'forward-paragraph)
  (define-key prog-mode-map (kbd "<return>") #'newline))

;; Multiple cursors.
(global-set-key (kbd "C-M-<down>") #'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<up>") #'mc/mark-previous-like-this)

;; Scrolling.
(global-set-key (kbd "M-p") #'scroll-down)
(global-set-key (kbd "M-n") #'scroll-up)

;; Swiper.
;; Complementary to `isearch-backward' on C-r.
(global-set-key (kbd "C-s") #'swiper)
(global-set-key (kbd "C-S-s") #'swiper-all)
(global-set-key (kbd "C-M-s") #'swiper-multi)

;; Window jump.
(global-set-key (kbd "C-M-j") #'window-jump-left)
(global-set-key (kbd "C-M-k") #'window-jump-right)
(global-set-key (kbd "C-M-p") #'window-jump-up)
(global-set-key (kbd "C-M-n") #'window-jump-down)

;; Window sizing.
(my-global-redefine-key 'enlarge-window #'my-enlarge-window)
(my-global-redefine-key 'enlarge-window-horizontally
			#'my-enlarge-window-horizontally)
(my-global-redefine-key 'shrink-window-horizontally
			#'my-shrink-window-horizontally)

;; Whitespace.
(my-global-redefine-key 'just-one-space #'cycle-spacing)

;; Zap-to-char behavior.
(my-global-redefine-key 'zap-to-char #'zap-up-to-char)
