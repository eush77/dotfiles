;;; llvm-mode

(with-eval-after-load "llvm-mode"
  (defun my-llvm-indent-line ()
    "Indent current line."
    (let ((offset (max 0 (- (current-column)
                            (save-excursion (back-to-indentation)
                                            (current-column)))))
          (indent-level
           (cond ((comment-only-p (line-beginning-position)
                                  (line-end-position))
                  (save-excursion
                    (if (/= 0 (forward-line 1))
                        0
                      (back-to-indentation)
                      (current-column))))
                 ((save-excursion
                    (back-to-indentation)
                    (looking-at (mapconcat #'identity
                                           '("!"
                                             ".*:$"
                                             "@"
                                             "attributes "
                                             "declare "
                                             "define "
                                             "source_filename "
                                             "target "
                                             "}")
                                           "\\|")))
                  0)
                 (t 2))))
      (indent-line-to indent-level)
      (move-to-column (+ indent-level offset))))

  (defun my-llvm-mode-hook ()
    "My hook for LLVM mode."
    (setq-local indent-line-function #'my-llvm-indent-line))
  (add-hook 'llvm-mode-hook #'my-llvm-mode-hook))

;;; tablegen-mode

(with-eval-after-load "tablegen-mode"
  ;; Create TableGen mode keymap.
  ;; Based on the code in LLVM trunk - seems like a bug fix.
  (when (not tablegen-mode-map)
    (setq tablegen-mode-map (make-sparse-keymap)))

  (define-key tablegen-mode-map [remap indent-region] #'clang-format-region)
  (key-chord-define tablegen-mode-map "xw" #'ff-find-other-file)

  (with-eval-after-load "smartparens"
    (sp-local-pair 'tablegen-mode "<" ">")))

;;; Support functions

(defun my-llvm-ff-other-file (file)
  "`ff-other-file-alist' function for LLVM source and generated
files."
  (unless (string-match
           (concat "/\\(include/llvm\\|lib\\)/\\(.*\\)\\(/[^/]+\\)"
                   "\\(\\.cpp\\|\\.h\\|\\.td\\|\\.gen\\|\\.inc\\)$")
           file)
    (user-error "No other file"))
  (let ((dir (match-string 2 file))
        (base (match-string 3 file))
        (ext (match-string 4 file)))
    (pcase ext
      ((pred (string= ".cpp")) (list (concat dir base ".h")
                                     (concat dir ".h")))
      ((pred (string= ".h")) (list (concat dir base ".cpp")
                                   (concat dir base base ".cpp")))
      ((pred (string= ".td"))
       (list (concat dir base ".gen")
             ;; Target TableGen descriptions
             (concat dir
                     (replace-regexp-in-string
                      (concat "^/" (regexp-quote
                                    (file-name-nondirectory dir)))
                      "\\&Gen"
                      base)
                     ".inc")))
      ((pred (string= ".gen")) (list (concat dir base ".td")))
      ((pred (string= ".inc"))
       (list (concat dir
                     (replace-regexp-in-string
                      (format "^\\(/%s\\)Gen"
                              (regexp-quote (file-name-nondirectory dir)))
                      "\\1"
                      base)
                     ".td"))))))
