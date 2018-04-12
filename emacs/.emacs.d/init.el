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
(add-to-list 'package-selected-packages 'clang-format)
(add-to-list 'package-selected-packages 'color-identifiers-mode)
(add-to-list 'package-selected-packages 'counsel)
(add-to-list 'package-selected-packages 'dired-hide-dotfiles)
(add-to-list 'package-selected-packages 'direnv)
(add-to-list 'package-selected-packages 'eshell-z)
(add-to-list 'package-selected-packages 'expand-region)
(add-to-list 'package-selected-packages 'flycheck)
(add-to-list 'package-selected-packages 'gnus-select-account)
(add-to-list 'package-selected-packages 'goto-last-change)
(add-to-list 'package-selected-packages 'guide-key)
(add-to-list 'package-selected-packages 'ivy)
(add-to-list 'package-selected-packages 'ivy-xref)
(add-to-list 'package-selected-packages 'key-chord)
(add-to-list 'package-selected-packages 'magit)
(add-to-list 'package-selected-packages 'minibuffer-line)
(add-to-list 'package-selected-packages 'multiple-cursors)
(add-to-list 'package-selected-packages 'org-plus-contrib)
(add-to-list 'package-selected-packages 'pass)
(add-to-list 'package-selected-packages 'pocket-reader)
(add-to-list 'package-selected-packages 'rotate)
(add-to-list 'package-selected-packages 'smart-mode-line)
(add-to-list 'package-selected-packages 'smartparens)
(add-to-list 'package-selected-packages 'smartscan)
(add-to-list 'package-selected-packages 'swap-buffers)
(add-to-list 'package-selected-packages 'swiper)
(add-to-list 'package-selected-packages 'w3m)
(add-to-list 'package-selected-packages 'window-jump)

(when window-system
  (add-to-list 'package-selected-packages 'gnus-desktop-notify)
  (add-to-list 'package-selected-packages 'pdf-tools))
(package-install-selected-packages)

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
(load "config-direnv")
(load "config-eshell")
(load "config-ffap")
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
(load "config-scroll-all")
(load "config-shr")
(load "config-smartparens")
(load "config-smartscan")
(load "config-swap-buffers")
(load "config-vc")
(load "config-view")
(load "config-w3m")
(load "config-window-jump")
(load "config-xref")

(when window-system
  (load "config-doc-view")
  (load "config-pdf-tools"))

;; Host file - tweak things if necessary.
(let ((host-file (expand-file-name "host.el" user-emacs-directory)))
  (when (file-exists-p host-file)
    (load-file host-file)))

(server-start)
(pinentry-start)
(setenv "EDITOR" "emacsclient")
