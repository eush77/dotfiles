(set-keymap-parent c-mode-base-map prog-mode-map)

;;; Common

(defvar my-c-outline-section-regexp
  "//+\\( +[^[:space:]].*\n//\n\n\\|[^/[:alnum:][:blank:]\n\^M]\\)"
  "Regexp to match the beginning of a major section.")

(defvar my-c-outline-heading-regexp
  "[^}/ \t\n\^M]\\|[ \t]+\\([^} \t\n\^M]\\|}[ \t]*[^/\t \n\^M]\\)\\|//+.*[^/\t \n\^M]"
  "Regexp to match the beginning of a heading.")

(defun my-c-outline-level ()
  "Return heading level for Outline minor mode in C modes."
  (let (buffer-invisibility-spec)
    (cond ((looking-at my-c-outline-section-regexp) 1)
          ((looking-at "//+ ")
           (let ((prefix-length (length (match-string 0))))
             (+ 2 (save-excursion
                    (forward-char prefix-length)
                    (skip-chars-forward "\t ")
                    (- (current-column) prefix-length)))))
          (t (+ 2 (save-excursion
                    (skip-chars-forward "\t ")
                    (current-column)))))))

(defun my-c-common-init--outline (&optional mode)
  "Tweak configuration for Outline minor mode."
  (setq outline-regexp (concat my-c-outline-section-regexp
                               "\\|"
                               my-c-outline-heading-regexp))
  (setq outline-level #'my-c-outline-level))
(advice-add 'c-common-init :after #'my-c-common-init--outline)

(key-chord-define c-mode-base-map "xw" #'ff-find-other-file)

;;; C

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

;;; C++

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
