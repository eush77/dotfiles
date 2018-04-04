(smartparens-global-strict-mode 1)

(with-eval-after-load "smartparens"
  (require 'smartparens-config)
  (custom-set sp-highlight-pair-overlay nil)

  (defun my-hippie-expand--smartparens (&rest args)
    "Fix Smartparens' integration with Hippie-Expand. See [1].

\[1]: URL `https://github.com/Fuco1/smartparens/issues/479'"
    (when smartparens-mode
      (cond ((and (memq (nth he-num hippie-expand-try-functions-list)
                        '(try-expand-list try-expand-list-all-buffers))
                  (= (char-before he-string-end) 41)) ; ?)
             (delete-char -1))
            ((equal he-string-beg he-string-end)
             (delete-char 1)))))
  (advice-add 'hippie-expand :after #'my-hippie-expand--smartparens)

  (defun my-set-mark-command--sp-mark-sexp (arg)
    "Continue marking sexps with `sp-mark-sexp'."
    (when (and transient-mark-mode mark-active
               (or (eq last-command 'sp-mark-sexp)
                   (and (eq last-command 'set-mark-command)
                        (/= (mark) (point)))))
      (sp-mark-sexp arg t)
      t))
  (advice-add 'set-mark-command
              :before-until #'my-set-mark-command--sp-mark-sexp)

  (defun my-sp-kill-hybrid-sexp--kill-line (arg)
    "If called with the prefix argument (but not raw prefix `C-u
C-u'), call `kill-line', otherwise proceed to
`sp-kill-hybrid-sexp'."
    (when (and arg (/= (prefix-numeric-value arg) 16))
      (kill-line arg)
      t))
  (advice-add 'sp-kill-hybrid-sexp
              :before-until #'my-sp-kill-hybrid-sexp--kill-line)

  (defun my-sp-wrap-with-pair (&optional arg)
    "Wrap the following expression in parentheses.

See `sp-wrap-with-pair', ‘sp-select-next-thing’."
    (interactive "P")
    (sp-wrap-with-pair "("))

  (defun my-sp-point-at-apostrophe-p (id action context)
    "True if single quote is an apostrophe.

Like `sp-point-after-word-p', but checks that the inserted symbol
is a single quote character and works for `wrap' action, which is
needed when apostrophe would be considered a close bracked."
    (looking-back "\\w'" nil))

  (defun my-sp-c++-point-at-arrow-operator-p (id action context)
    "True if angle bracket is part of the arrow operator."
    (looking-back "->" nil))

  (defun my-sp-c++-point-at-comparison-operator-p (id action context)
    "True if angle bracket is part of the comparison operator."
    (and (string= id "<")
         (looking-back " ." nil)
         (not (looking-back "\\(template\\|#include\\) <" nil))))

  (defun my-sp-c++-point-at-shift-operator-p (id action context)
    "True if angle bracket is part of the comparison operator."
    (looking-back "<<\\|>>" nil))

  ;; Use asymmetric single quotes.
  (sp-pair "`" "'" :unless '(my-sp-point-at-apostrophe-p))

  ;; C++ angle brackets are overloaded for different things. Disable
  ;; strictness checks (by not listing the `navigate' action) and add some
  ;; filters.
  (sp-local-pair 'c++-mode "<" ">"
                 :actions '(insert wrap autoskip)
                 :unless '(my-sp-c++-point-at-arrow-operator-p
                           my-sp-c++-point-at-comparison-operator-p
                           my-sp-c++-point-at-shift-operator-p)))
