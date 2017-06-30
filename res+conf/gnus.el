;; Mail server settings.
(load-relative ".gnus-mail")

(setq gnus-gcc-mark-as-read t
      gnus-use-full-window nil
      mm-text-html-renderer 'w3m)

(gnus-demon-add-handler 'gnus-demon-scan-news 10 nil)
(require 'gnus-desktop-notify)
(gnus-desktop-notify-mode)
