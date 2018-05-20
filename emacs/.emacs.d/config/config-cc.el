;;
;; C
;;

(defun my-c-hook ()
  "My hook for C mode."
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'inextern-lang 0))
(add-hook 'c-mode-hook #'my-c-hook)

(define-key c-mode-map (kbd "C-c C-a") #'sp-beginning-of-sexp)
(define-key c-mode-map (kbd "C-c C-b") #'sp-backward-sexp)
(define-key c-mode-map (kbd "C-c C-d") #'sp-kill-sexp)
(define-key c-mode-map (kbd "C-c C-e") #'sp-end-of-sexp)
(define-key c-mode-map (kbd "C-c C-f") #'sp-forward-sexp)
(define-key c-mode-map (kbd "C-c C-u C-M-b") #'sp-backward-barf-sexp)
(define-key c-mode-map (kbd "C-c C-u C-M-f") #'sp-forward-barf-sexp)
(define-key c-mode-map [remap indent-region] #'clang-format-region)

;;
;; C++
;;

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

(define-key c++-mode-map (kbd "C-c C-a") #'sp-beginning-of-sexp)
(define-key c++-mode-map (kbd "C-c C-b") #'sp-backward-sexp)
(define-key c++-mode-map (kbd "C-c C-d") #'sp-kill-sexp)
(define-key c++-mode-map (kbd "C-c C-e") #'sp-end-of-sexp)
(define-key c++-mode-map (kbd "C-c C-f") #'sp-forward-sexp)
(define-key c++-mode-map (kbd "C-c C-u C-M-b") #'sp-backward-barf-sexp)
(define-key c++-mode-map (kbd "C-c C-u C-M-f") #'sp-forward-barf-sexp)
(define-key c++-mode-map [remap indent-region] #'clang-format-region)

(with-eval-after-load "smartparens"
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

  ;; C++ angle brackets are overloaded for different things. Disable
  ;; strictness checks (by not listing the `navigate' action) and add some
  ;; filters.
  (sp-local-pair 'c++-mode "<" ">"
                 :actions '(insert wrap autoskip)
                 :unless '(my-sp-c++-point-at-arrow-operator-p
                           my-sp-c++-point-at-comparison-operator-p
                           my-sp-c++-point-at-shift-operator-p))

  (defvar my-sp-c++-restrict-to-blocks-mode-map
    (let ((map (make-sparse-keymap)))
      (dolist (cmd (cl-union (mapcar #'cdr sp-paredit-bindings)
                             (mapcar #'cdr sp-smartparens-bindings)))
        (eval `(define-key map [remap ,cmd]
                 (sp-restrict-to-object-interactive
                  'sp-prefix-pair-object
                  (sp-restrict-to-pairs-interactive "{" ',cmd)))))
      map)
    "Keymap for `my-sp-c++-restrict-to-blocks-mode'.")

  (define-minor-mode my-sp-c++-restrict-to-blocks-mode
    "Minor mode for restricting Smartparens commands to curly
blocks."
    :lighter "SP/{}"
    :keymap 'my-sp-c++-restrict-to-blocks-mode-map))
