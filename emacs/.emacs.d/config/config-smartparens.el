(smartparens-global-strict-mode 1)

(with-eval-after-load "smartparens"
  (require 'smartparens-config)
  (custom-set sp-highlight-pair-overlay nil)

  (defun my-sp-wrap-with-pair (&optional arg)
    "Wrap the following expression in parentheses.

See `sp-wrap-with-pair', ‘sp-select-next-thing’."
    (interactive "P")
    (sp-wrap-with-pair "("))

  ;; Use asymmetric single quotes.
  (sp-pair "`" "'")

  (sp-local-pair 'c++-mode "<" ">"))
