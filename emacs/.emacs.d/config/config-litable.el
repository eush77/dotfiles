(custom-set litable-result-format "=> %s")

;;; Customize display

(advice-add 'litable--print-input :override #'list)

(defun my-litable-find-function-subs-arguments--top-level-only
    (form &optional depth)
  (or (null depth) (zerop depth)))
(advice-add 'litable-find-function-subs-arguments
            :before-while
            #'my-litable-find-function-subs-arguments--top-level-only)

;;; Remove impure functions

(setq litable-pure-functions-list
      (seq-difference litable-pure-functions-list
                      '(LaTeX-back-to-indentation
                        LaTeX-find-matching-begin
                        LaTeX-mark-environment
                        TeX-check-files
                        TeX-fold-mode
                        TeX-normal-mode
                        activate-mark
                        back-to-indentation
                        backward-char
                        backward-list
                        backward-sexp
                        backward-up-list
                        backward-word
                        beginning-of-buffer
                        beginning-of-defun
                        beginning-of-line
                        beginning-of-thing
                        c-end-of-defun
                        check-parens
                        completing-read
                        decf
                        dired-next-line
                        down-list
                        end-of-defun
                        end-of-line
                        end-of-thing
                        error
                        forward-char
                        forward-line
                        forward-list
                        forward-sexp
                        goto-char
                        goto-line
                        make-marker
                        make-overlay
                        message
                        move-beginning-of-line
                        move-end-of-line
                        move-overlay
                        overlay-put
                        paredit-backward-up
                        princ
                        print
                        re-search-backward
                        re-search-forward
                        remove-overlays
                        save-current-buffer
                        set
                        set-buffer
                        setf
                        setq
                        set-buffer-modified-p
                        signal
                        skip-chars-backward
                        skip-chars-forward
                        warn
                        y-or-n-p
                        yes-or-no-p)))

;;; Add pure functions

(mapatoms (lambda (sym)
              (when (string-match "^c[ad]*r$" (symbol-name sym))
                (add-to-list 'litable-pure-functions-list sym))))

(setq litable-pure-functions-list
      (cl-union litable-pure-functions-list
                '(/=
                  <
                  <=
                  =
                  >
                  >=
                  alist-get
                  butlast
                  byte-to-string
                  char-width
                  cl-union
                  eql
                  last
                  make-vector
                  mapcar
                  mapconcat
                  nthcdr
                  prin1-to-string
                  remove
                  seq-concatenate
                  seq-contains
                  seq-count
                  seq-difference
                  seq-drop
                  seq-drop-while
                  seq-elt
                  seq-empty-p
                  seq-every-p
                  seq-filter
                  seq-find
                  seq-group-by
                  seq-intersection
                  seq-into
                  seq-length
                  seq-let
                  seq-map
                  seq-mapcat
                  seq-mapn
                  seq-max
                  seq-min
                  seq-partition
                  seq-position
                  seq-reduce
                  seq-remove
                  seq-some
                  seq-sort
                  seq-subseq
                  seq-take
                  seq-take-while
                  seq-uniq
                  seqp
                  sequencep
                  string-width
                  truncate-string-to-width
                  vconcat
                  vectorp)))
