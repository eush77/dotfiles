(with-eval-after-load "cc-mode"
  (defun my-c++-lineup-lambda (langelem)
    "Line up a C++ lambda argument."
    (save-excursion
      (back-to-indentation)
      (backward-up-list)
      (when (looking-back ")\s*\\(mutable\s*\\)?")
        0)))

  (defun my-c++-hook ()
    "My hook for C++ mode."
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'inextern-lang 0)
    (c-set-offset 'innamespace '-)
    (c-set-offset 'arglist-cont-nonempty
                  (list #'my-c++-lineup-lambda #'c-lineup-arglist)))
  (add-hook 'c++-mode-hook #'my-c++-hook)

  (define-key c++-mode-map [remap indent-region] #'clang-format-region))
