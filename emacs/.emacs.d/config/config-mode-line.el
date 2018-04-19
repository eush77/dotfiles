(sml/setup)

(custom-set mode-line-format '("%e"
                               mode-line-front-space
                               mode-line-mule-info
                               mode-line-client
                               mode-line-modified
                               mode-line-remote
                               mode-line-frame-identification
                               mode-line-buffer-identification
                               "   "
                               mode-line-position
                               (vc-mode vc-mode)
                               "  "
                               mode-line-modes
                               mode-line-misc-info
                               mode-line-end-spaces))

(custom-set rm-blacklist '(" $"         ; rich-minority-mode
                           " counsel"   ; counsel-mode
                           " FlyC-"     ; flycheck-mode (no-checker)
                           " Guide"     ; guide-key-mode
                           " ivy"       ; ivy-mode
                           "[ln]"))     ; w3m-lnum-mode

(custom-set sml/mode-width 'right)
(custom-set sml/name-width 20)
(custom-set sml/position-percentage-format "")
(custom-set sml/prefix-face-list '(("" sml/prefix)))
(custom-set sml/replacer-regexp-list
            '(("^~/\\.emacs\\.d/elpa/" ":elpa:")
              ("^~/Dropbox/notes/" ":notes:")
              ("^~/Dropbox/org/" ":org:")
              ("^~/src/" ":src:")
              ("^:src:\\([^/]\\)[^/]*/" ":src/\\1:")
              ("^:src/\\(.\\):\\([^/]+\\)/" ":\\1/\\2:")))
(custom-set sml/size-indication-format "%p of %I ")
(custom-set sml/theme 'respectful)
