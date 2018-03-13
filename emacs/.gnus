;; Mail server settings.
(load-file "~/.gnus-mail")

(setq gnus-gcc-mark-as-read t
      gnus-treat-display-smileys nil
      gnus-use-full-window nil
      mm-text-html-renderer 'w3m)

(gnus-add-configuration
 '(article (horizontal 1.0
                       (vertical 0.3
                                 (summary 1.0 point))
                       (article 1.0))))

(define-key gnus-article-mode-map (kbd "j") #'scroll-up-line)
(define-key gnus-article-mode-map (kbd "k") #'scroll-down-line)
(define-key gnus-summary-mode-map (kbd "C-M-j") #'window-jump-left)
(define-key gnus-summary-mode-map (kbd "C-M-k") #'window-jump-right)
(define-key gnus-summary-mode-map (kbd "C-M-p") #'window-jump-up)
(define-key gnus-summary-mode-map (kbd "C-M-n") #'window-jump-down)

(gnus-demon-add-handler 'gnus-demon-scan-news 10 nil)
(when window-system
  (gnus-desktop-notify-mode))
