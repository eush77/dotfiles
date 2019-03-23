(require 'org)

(sml/setup)

(custom-set-variables
 '(mode-line-format '("%e"
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
                      mode-line-end-spaces))

 '(rm-blacklist '(" $"         ; rich-minority-mode
                  " counsel"   ; counsel-mode
                  " FlyC-"     ; flycheck-mode (no-checker)
                  " Golden"    ; golden-ratio-mode
                  " Guide"     ; guide-key-mode
                  " ivy"       ; ivy-mode
                  "[ln]"))     ; w3m-lnum-mode

 '(sml/mode-width 'right)
 '(sml/name-width 20)
 '(sml/position-percentage-format "")
 '(sml/prefix-face-list '(("" sml/prefix)))
 '(sml/replacer-regexp-list
   `(("^~/\\.emacs\\.d/elpa/" ":elpa:")
     (,(concat "^" my-org-notes-directory "/") ":notes:")
     (,(concat "^" org-directory "/") ":org:")
     ("^~/src/" ":src:")
     ("^:src:\\([^/]\\)[^/]*/" ":src/\\1:")
     ("^:src/\\(.\\):\\([^/]+\\)/" ":\\1/\\2:")))
 '(sml/size-indication-format "%p of %I ")
 '(sml/theme 'respectful))
