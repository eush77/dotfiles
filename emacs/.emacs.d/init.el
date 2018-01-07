;;; -*- lexical-binding: t -*-
(package-initialize)
(require 'cl-lib)
(require 'misc)

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

;; Package archives.
(custom-set package-archives
	    '(("gnu" . "http://elpa.gnu.org/packages/")
	      ("melpa" . "http://melpa.org/packages/")))

;; Check and install missing packages.
(let* ((wanted-packages '(counsel
                          expand-region
                          gnus-desktop-notify
			  ivy
                          js2-mode
			  key-chord
                          magit
			  multiple-cursors
			  org
			  swiper
			  w3m
			  window-jump))
       (missing-packages (cl-remove-if #'package-installed-p
				       wanted-packages)))
  (when (and missing-packages
	     (yes-or-no-p (format "Missing packages: %s. Install? "
				  missing-packages)))
    (package-refresh-contents)
    (mapc #'package-install missing-packages)))

;; Customization file.
(custom-set custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)

;; Host file - keep it to a minimum.
(let ((host-file (expand-file-name "host.el" user-emacs-directory)))
  (when (file-exists-p host-file)
    (load-file host-file)))

;; The rest of the config is split into separate files.
(push (expand-file-name "config" user-emacs-directory) load-path)
(load "config-utils")
(load "config-base")
(load "config-c++")
(load "config-dired")
(load "config-js2")
(load "config-key-chord")
(load "config-key-map")
(load "config-markdown")
(load "config-org")
(load "config-shr")
