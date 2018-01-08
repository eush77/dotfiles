;;; -*- lexical-binding: t -*-
(package-initialize)
(require 'misc)

;; Load basic utilities.
(push (expand-file-name "config" user-emacs-directory) load-path)
(load "config-utils")

;; Custom-set macro - use instead of `setq' for customization variables.
(defmacro custom-set (var value)
  "Set VAR to VALUE using `custom-set-variables`.

Unlike `setq', this honors customization hooks (see Info
node `(elisp) Variable Definitions').

See also URL
`https://emacs.stackexchange.com/questions/102/advantages-of-setting-variables-with-setq-instead-of-custom-el'
and URL
`https://stackoverflow.com/questions/2079095/how-to-modularize-an-emacs-configuration'."
  (custom-set-variables
   `(,var ,value nil nil
          ,(format "!!! CAREFUL: CUSTOM-SET IN %s !!!" load-file-name))))

;; Customization file.
(custom-set custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)

;; Package archives.
(custom-set package-archives
	    '(("gnu" . "http://elpa.gnu.org/packages/")
	      ("melpa" . "http://melpa.org/packages/")))

;; Check and install missing packages.
(my-install-packages 'counsel
                     'expand-region
                     'gnus-desktop-notify
                     'ivy
                     'key-chord
                     'magit
                     'multiple-cursors
                     'org
                     'swiper
                     'w3m
                     'window-jump)

;; The rest of the config is split into separate files.
;;
;; Not all of the configuration is included here - see the config directory
;; for more configuration (typically requiring additional packages installed)
;; and load corresponding entries in the host file.
(load "config-base")
(load "config-c++")
(load "config-dired")
(load "config-key-chord")
(load "config-key-map")
(load "config-markdown")
(load "config-org")
(load "config-shr")

;; Host file - tweak things if necessary.
(let ((host-file (expand-file-name "host.el" user-emacs-directory)))
  (when (file-exists-p host-file)
    (load-file host-file)))

(server-start)
