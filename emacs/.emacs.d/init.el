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
	      ("melpa" . "http://melpa.org/packages/")
              ("org" . "https://orgmode.org/elpa/")))

;; Check and install missing packages.
(my-install-packages 'clang-format
                     'color-identifiers-mode
                     'counsel
                     'dired-hide-dotfiles
                     'direnv
                     'eshell-z
                     'expand-region
                     'flycheck
                     'goto-last-change
                     'guide-key
                     'ivy
                     'ivy-xref
                     'key-chord
                     'magit
                     'minibuffer-line
                     'multiple-cursors
                     'org-plus-contrib
                     'pass
                     'pocket-reader
                     'smart-mode-line
                     'smartparens
                     'smartscan
                     'swiper
                     'w3m
                     'window-jump)

(when window-system (my-install-packages 'gnus-desktop-notify
                                         'pdf-tools))

;; The rest of the config is split into separate files.
;;
;; Not all of the configuration is included here - see the config directory
;; for more configuration (typically requiring additional packages installed)
;; and load corresponding entries in the host file.
(load "config-base")
(load "config-calendar")
(load "config-c++")
(load "config-color-identifiers-mode")
(load "config-counsel")
(load "config-dired")
(load "config-eshell")
(load "config-flycheck")
(load "config-guide-key")
(load "config-help")
(load "config-hideshow")
(load "config-hippie-expand")
(load "config-info")
(load "config-key-chord")
(load "config-key-map")
(load "config-magit")
(load "config-man")
(load "config-markdown")
(load "config-org")
(load "config-pocket-reader")
(load "config-shr")
(load "config-smartparens")
(load "config-smartscan")
(load "config-vc")
(load "config-view")
(load "config-w3m")
(load "config-window-jump")

(when window-system
  (load "config-doc-view")
  (load "config-pdf-tools"))

;; Host file - tweak things if necessary.
(let ((host-file (expand-file-name "host.el" user-emacs-directory)))
  (when (file-exists-p host-file)
    (load-file host-file)))

(server-start)
(pinentry-start)
(direnv-mode)
(setenv "EDITOR" "emacsclient")
