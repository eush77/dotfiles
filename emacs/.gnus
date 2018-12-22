;;; Mail servers

(load-file "~/.gnus-mail")

;;; General configuration

(setq gnus-gcc-mark-as-read t
      gnus-treat-display-smileys nil
      gnus-use-full-window nil
      mm-text-html-renderer 'w3m)

(gnus-demon-add-handler 'gnus-demon-scan-news 10 nil)
(gnus-select-account-enable)

(when window-system
  (gnus-desktop-notify-mode))

;;; Window configuration

(gnus-add-configuration
 '(article (horizontal 1.0
                       (vertical 0.3
                                 (summary 1.0 point))
                       (article 1.0))))

;;; Commands

(defun my-gnus-article-add-link ()
    "Add link at point to Pocket."
    (interactive)
    (require 'pocket-lib)
    (let* ((url (w3m-anchor)))
      (when (pocket-lib-add-urls url)
        (message "Added: %s" url))))

(defun my-gnus-article-show-summary ()
  "Same as `gnus-article-show-summary' but supports article
window configurations with no summary window."
  (interactive)
  (if (eq (caadr (assq 'article gnus-buffer-configuration))
          'article)
      (gnus-configure-windows 'summary)
    (gnus-article-show-summary)))

(defun my-gnus-delete-article-window ()
  "Delete Article window."
  (interactive)
  (when (and (gnus-buffer-live-p gnus-article-buffer)
             (not (one-window-p)))
    (when-let (article-window (get-buffer-window gnus-article-buffer))
      ;; Switch away from the article window
      (when (eq (selected-window) article-window)
        (if-let (((gnus-buffer-live-p gnus-summary-buffer))
                 (summary-window (get-buffer-window gnus-summary-buffer)))
            (select-window summary-window)
          (other-window 1)))
      (delete-window article-window))))

;;; Summary

(add-hook 'gnus-exit-group-hook #'my-gnus-delete-article-window)

(with-eval-after-load "gnus-sum"
  (define-key gnus-summary-mode-map (kbd "H") #'my-gnus-delete-article-window)
  (define-key gnus-summary-mode-map (kbd "C-M-j") #'window-jump-left)
  (define-key gnus-summary-mode-map (kbd "C-M-k") #'window-jump-right)
  (define-key gnus-summary-mode-map (kbd "C-M-p") #'window-jump-up)
  (define-key gnus-summary-mode-map (kbd "C-M-n") #'window-jump-down))

;;; Article

(with-eval-after-load "gnus-art"
  (define-key gnus-article-mode-map (kbd "H") #'my-gnus-delete-article-window)
  (define-key gnus-article-mode-map (kbd "h") #'my-gnus-article-show-summary)
  (define-key gnus-article-mode-map (kbd "j") #'scroll-up-line)
  (define-key gnus-article-mode-map (kbd "k") #'scroll-down-line)
  (define-key gnus-article-mode-map (kbd "s") #'my-gnus-article-add-link))

;;; Load host file

(let ((host-file "~/.gnus-host"))
  (when (file-exists-p host-file)
    (load-file host-file)))
