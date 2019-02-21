;;; Hydra

;;;###autoload (autoload 'my-hs-hydra/body "config-hideshow")
(defhydra my-smerge-hydra ()
  "SMerge"
  ("n" smerge-next "next")
  ("p" smerge-prev "previous")
  ("u" smerge-keep-upper "keep upper")
  ("l" smerge-keep-lower "keep lower"))

;;; Keymap

(define-key smerge-mode-map (kbd "C-c ^ ^") #'my-smerge-hydra/body)

;; Local Variables:
;; after-save-hook: (lambda nil (message "Saved"))
;; End:
