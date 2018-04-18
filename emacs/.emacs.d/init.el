(package-initialize)

;; Setup config load path and autoloads.
(push (expand-file-name "config" user-emacs-directory) load-path)
(load "config-autoloads")

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

;; My customization group.
(defgroup my
  '((my-eshell-prompt-length custom-variable)
    (my-eshell-prompt-sigil-failure custom-face)
    (my-open-line-and-indent custom-variable)
    (my-window-size-delta custom-variable))
  "Settings for my personal configuration."
  :prefix "my-"
  :link `(file-link ,(expand-file-name "init.el" user-emacs-directory)))

;; Package archives.
(custom-set package-archives
	    '(("gnu" . "http://elpa.gnu.org/packages/")
	      ("melpa" . "http://melpa.org/packages/")
              ("org" . "https://orgmode.org/elpa/")))
(package-refresh-contents)

;; Check and install missing packages.
(add-to-list 'package-selected-packages 'clang-format)
(add-to-list 'package-selected-packages 'color-identifiers-mode)
(add-to-list 'package-selected-packages 'counsel)
(add-to-list 'package-selected-packages 'dired-hide-dotfiles)
(add-to-list 'package-selected-packages 'direnv)
(add-to-list 'package-selected-packages 'emms)
(add-to-list 'package-selected-packages 'eshell-z)
(add-to-list 'package-selected-packages 'expand-region)
(add-to-list 'package-selected-packages 'flycheck)
(add-to-list 'package-selected-packages 'gnus-select-account)
(add-to-list 'package-selected-packages 'goto-last-change)
(add-to-list 'package-selected-packages 'guide-key)
(add-to-list 'package-selected-packages 'highlight)
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
(load "config-color-identifiers-mode")
(load "config-counsel")
(load "config-direnv")
(load "config-emms")
(load "config-flycheck")
(load "config-guide-key")
(load "config-hideshow")
(load "config-ivy")
(load "config-key-chord")
(load "config-key-map")
(load "config-magit")
(load "config-minibuffer-line")
(load "config-mode-line")
(load "config-smartparens")
(load "config-smartscan")
(load "config-vc")
(load "config-w3m")
(with-eval-after-load "calendar" (load "config-calendar"))
(with-eval-after-load "cc-mode" (load "config-c++"))
(with-eval-after-load "compile" (load "config-compile"))
(with-eval-after-load "custom" (load "config-custom"))
(with-eval-after-load "dired" (load "config-dired"))
(with-eval-after-load "eshell" (load "config-eshell"))
(with-eval-after-load "ffap" (load "config-ffap"))
(with-eval-after-load "gdb-mi" (load "config-gdb-mi"))
(with-eval-after-load "help" (load "config-help"))
(with-eval-after-load "highlight" (load "config-highlight"))
(with-eval-after-load "hippie-exp" (load "config-hippie-expand"))
(with-eval-after-load "info" (load "config-info"))
(with-eval-after-load "man" (load "config-man"))
(with-eval-after-load "markdown-mode" (load "config-markdown"))
(with-eval-after-load "org" (load "config-org"))
(with-eval-after-load "pocket-reader" (load "config-pocket-reader"))
(with-eval-after-load "scroll-all" (load "config-scroll-all"))
(with-eval-after-load "shr" (load "config-shr"))
(with-eval-after-load "swap-buffers" (load "config-swap-buffers"))
(with-eval-after-load "view" (load "config-view"))
(with-eval-after-load "window-jump" (load "config-window-jump"))
(with-eval-after-load "xref" (load "config-xref"))

(when window-system
  (load "config-pdf-tools")
  (with-eval-after-load "doc-view" (load "config-doc-view")))

;; Host file - tweak things if necessary.
(let ((host-file (expand-file-name "host.el" user-emacs-directory)))
  (when (file-exists-p host-file)
    (load-file host-file)))

(server-start)
(pinentry-start)
(setenv "EDITOR" "emacsclient")
