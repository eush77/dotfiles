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

(with-eval-after-load "gnus-sum"
  (define-key gnus-summary-mode-map (kbd "C-M-j") #'window-jump-left)
  (define-key gnus-summary-mode-map (kbd "C-M-k") #'window-jump-right)
  (define-key gnus-summary-mode-map (kbd "C-M-p") #'window-jump-up)
  (define-key gnus-summary-mode-map (kbd "C-M-n") #'window-jump-down))

(with-eval-after-load "gnus-art"
  (defun my-gnus-article-add-link ()
    "Add link at point to Pocket."
    (interactive)
    (require 'pocket-lib)
    (let* ((url (w3m-anchor)))
      (when (pocket-lib-add-urls url)
        (message "Added: %s" url))))

  (define-key gnus-article-mode-map (kbd "j") #'scroll-up-line)
  (define-key gnus-article-mode-map (kbd "k") #'scroll-down-line)
  (define-key gnus-article-mode-map (kbd "s") #'my-gnus-article-add-link))

(gnus-demon-add-handler 'gnus-demon-scan-news 10 nil)
(when window-system
  (gnus-desktop-notify-mode))
