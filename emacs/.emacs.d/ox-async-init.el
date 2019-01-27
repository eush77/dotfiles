;; Init file for asynchronous Org export. See `org-export-async-init-file'.

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(require 'org)
(require 'ox)
(require 'ox-beamer)

(setq backup-inhibited t)
(setq org-export-async-debug nil)
