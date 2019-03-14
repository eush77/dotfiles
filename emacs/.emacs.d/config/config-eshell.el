;;; -*- lexical-binding: t -*-
(require 'eshell-z)

;;; Basic setup

(custom-set eshell-prefer-lisp-functions t)
(custom-set eshell-ls-initial-args '("--classify"
                                     "--color=auto"
                                     "--group-directories-first"
                                     "-v"))

;; Advice `eshell-dirs-substitute-cd' to list directory contents.
(defun eshell-dirs-substitute-cd--ls (func &rest args)
  "List directory contents after executing it in the command position.

Like setting `eshell-list-files-after-cd', but applies only when
`eshell/cd' is called implicitly. It also works correctly when
`eshell/ls' launches an external process (in which case
`eshell-list-files-after-cd' would show its output after the
prompt)."
  (let* ((cd (catch 'eshell-replace-command (apply func args)))
         ;; The resulting command will be destructively modified by Eshell,
         ;; so don't `quote' it.
         (cd+ls (list 'progn
                      (list 'eshell-commands cd)
                      (list 'eshell-named-command "ls"))))
    (throw 'eshell-replace-command cd+ls)))
(advice-add 'eshell-dirs-substitute-cd
            :around #'eshell-dirs-substitute-cd--ls)

;;; Prompt

;; My Eshell prompt has a fixed length.
;;
;; It is composed of "leads" - nondirectory parts of the full name of the
;; current working directory. The last lead is always displayed in full or
;; truncated to fill all the available space, even if to the detriment of
;; other leads.

;;;###autoload
(defcustom my-eshell-prompt-length 20
  "Length of the Eshell prompt.

The prompt string computed by `my-eshell-prompt-function' will be
reduced until it fits and then aligned to be exactly this
length."
  :type 'integer
  :group 'my)

;;;###autoload
(defface my-eshell-prompt-sigil-failure
  '((default :weight bold)
    (((class color) (background light)) :foreground "Pink")
    (((class color) (background dark))  :foreground "Red"))
  "The face used to highlight sigil when
`eshell-last-command-status' is not 0."
  :group 'my)

(defun my-eshell-prompt-sigil-function ()
  "Return trailing sigil for my Eshell prompt."
  (propertize (format " %s%s "
                      (if (zerop (user-uid)) "#" "$")
                      (if eshell-dirstack
                          (number-to-string (length eshell-dirstack))
                        ""))
              'font-lock-face
              (if (zerop eshell-last-command-status)
                  'eshell-prompt
                'my-eshell-prompt-sigil-failure)))

(defun my-eshell-prompt/lead-min-length (lead)
  "Minimum length a lead can be displayed in.
This is usually 1, except when the string is empty."
  (min 1 (length lead)))

(ert-deftest my-eshell-prompt/lead-min-length ()
  (should (= (my-eshell-prompt/lead-min-length "abc") 1))
  (should (= (my-eshell-prompt/lead-min-length "") 0)))

(defun my-eshell-prompt/leads-min-length (leads)
  "Minimum length a sequence of leads can be displayed in.
Each lead can be shortened to `my-eshell-prompt/lead-min-length'
characters and each lead should be followed by slash. "
  (apply #'+
         (length leads)
         (mapcar #'my-eshell-prompt/lead-min-length leads)))

(ert-deftest my-eshell-prompt/leads-min-length ()
  (should (= (my-eshell-prompt/leads-min-length '("abc")) 2))
  (should (= (my-eshell-prompt/leads-min-length '("abc" "abc")) 4))
  (should (= (my-eshell-prompt/leads-min-length '("" "abc")) 3)))

(defun my-eshell-prompt/update-min-length (min-length op lead)
  "Update computed MIN-LENGTH.

New LEAD is added if OP is '+ and subtracted if it is '-."
  (let* ((update-op (pcase op
                      ('+ #'+)
                      ('- #'-)))
         (updated-min-length
          (funcall update-op
                   min-length
                   (+ (my-eshell-prompt/lead-min-length lead) 1))))
    (cl-assert (>= updated-min-length 0))
    updated-min-length))

(ert-deftest my-eshell-prompt/update-min-length ()
  (should (= (my-eshell-prompt/update-min-length 4 '+ "abc") 6))
  (should (= (my-eshell-prompt/update-min-length 4 '+ "") 5))
  (should (= (my-eshell-prompt/update-min-length 4 '- "abc") 2))
  (should (= (my-eshell-prompt/update-min-length 4 '- "") 3)))

(defun my-eshell-prompt/truncate-leads (leads length)
  "Truncate leads until the length exceeds the minimum length
for the sequence. Leads are truncate from the leftmost lead to
the right."
  (letrec ((truncate
            (lambda (leads min-length)
              (if (<= min-length length)
                  leads               ; `leads' fit into the length.
                (funcall truncate
                         (cdr leads)
                         (my-eshell-prompt/update-min-length
                          min-length
                          '-
                          (car leads)))))))
    (funcall truncate leads (my-eshell-prompt/leads-min-length leads))))

(ert-deftest my-eshell-prompt/truncate-leads ()
  (should (equal (my-eshell-prompt/truncate-leads '("abc") 2) '("abc")))
  (should (equal (my-eshell-prompt/truncate-leads '("abc") 1) '()))
  (should (equal (my-eshell-prompt/truncate-leads '("abc" "xyz") 4)
                 '("abc" "xyz")))
  (should (equal (my-eshell-prompt/truncate-leads '("abc" "xyz") 3)
                 '("xyz")))
  (should (equal (my-eshell-prompt/truncate-leads '("" "abc" "xyz") 5)
                 '("" "abc" "xyz")))
  (should (equal (my-eshell-prompt/truncate-leads '("" "abc" "xyz") 4)
                 '("abc" "xyz")))
  (should (equal (my-eshell-prompt/truncate-leads '("" "abc" "xyz") 2)
                 '("xyz")))
  (should (equal (my-eshell-prompt/truncate-leads '("" "abc" "xyz") 1)
                 '())))

(defun my-eshell-prompt/format-lead (lead length)
  "Format LEAD to exactly LENGTH characters."
  (cond ((<= (length lead) length)
         (concat (make-string (- length (length lead)) ? ) lead))
        ((<= length 1) (substring lead 0 length))
        (t (concat (substring lead 0 (- length 1)) "~"))))

(ert-deftest my-eshell-prompt/format-lead ()
  (should (string= (my-eshell-prompt/format-lead "abc" 3) "abc"))
  (should (string= (my-eshell-prompt/format-lead "abc" 8) "     abc"))
  (should (string= (my-eshell-prompt/format-lead "abc" 2) "a~"))
  (should (string= (my-eshell-prompt/format-lead "abc" 1) "a"))
  (should (string= (my-eshell-prompt/format-lead "abc" 0) "")))

(defun my-eshell-prompt/format-leads (leads length)
  "Format LEADS to exactly LENGTH characters.

This function does not handle the last lead specially, but
instead (conceptually) shortens all leads and then expands all
leads starting from the rightmost lead to fill available width."
  (letrec ((truncated-leads (my-eshell-prompt/truncate-leads leads length))
           (format-leads
            (lambda (leads min-length length)
              ;; Take reversed list of truncated leads and expand all leads
              ;; starting from the rightmost to fill available width.
              (pcase leads
                (`(,lead . ,rest)
                 (let* ((rest-min-length
                         (my-eshell-prompt/update-min-length min-length
                                                             '-
                                                             lead))
                        (lead-length (min (length lead)
                                          (- length 1 rest-min-length))))
                   (concat (funcall format-leads
                                    rest
                                    rest-min-length
                                    (- length lead-length 1))
                           (my-eshell-prompt/format-lead lead lead-length)
                           "/")))
                (t (make-string length ? ))))))
    (funcall format-leads
             (reverse truncated-leads)
             (my-eshell-prompt/leads-min-length truncated-leads)
             length)))

(ert-deftest my-eshell-prompt/format-leads ()
  (should (string= (my-eshell-prompt/format-leads '("abc" "xyz") 8)
                   "abc/xyz/"))
  (should (string= (my-eshell-prompt/format-leads '("abc" "xyz") 7)
                   "a~/xyz/"))
  (should (string= (my-eshell-prompt/format-leads '("abc" "xyz") 6)
                   "a/xyz/"))
  (should (string= (my-eshell-prompt/format-leads '("abc" "xyz") 5)
                   "a/x~/"))
  (should (string= (my-eshell-prompt/format-leads '("abc" "xyz") 4)
                   "a/x/"))
  (should (string= (my-eshell-prompt/format-leads '("abc" "xyz") 3)
                   "x~/"))
  (should (string= (my-eshell-prompt/format-leads '("abc" "xyz") 2)
                   "x/"))
  (should (string= (my-eshell-prompt/format-leads '("abc" "xyz") 1)
                   " "))
  (should (string= (my-eshell-prompt/format-leads '("abc" "xyz") 0)
                   "")))

(defun my-eshell-prompt-leads-function ()
  "Return my Eshell prompt string for `default-directory', not
including the sigil."
  (let* ((directory (abbreviate-file-name
                     (directory-file-name default-directory)))
         (last-lead (file-name-nondirectory directory))
         (leads (butlast (split-string directory "/")))
         (sigil (my-eshell-prompt-sigil-function)))
    (propertize
     (cond ((< my-eshell-prompt-length (length sigil))
            (error "`my-eshell-prompt-length' is too small."))
           ((<= my-eshell-prompt-length (+ (length last-lead)
                                           (length sigil)))
            (my-eshell-prompt/format-lead last-lead
                                          (- my-eshell-prompt-length
                                             (length sigil))))
           (t (concat (my-eshell-prompt/format-leads
                       leads
                       (- my-eshell-prompt-length
                          (length last-lead)
                          (length sigil)))
                      last-lead)))
     'font-lock-face 'eshell-prompt)))

(defun my-eshell-prompt-function ()
  "Return my Eshell prompt propertized string for `default-directory'."
  (let ((prompt (concat (my-eshell-prompt-leads-function)
                        (my-eshell-prompt-sigil-function))))
    (add-text-properties
     0
     (length prompt)
     '(read-only t
                 front-sticky (font-lock-face read-only)
                 rear-nonsticky (font-lock-face read-only))
     prompt)
    prompt))

;; Set up the prompt.
;; It is already propertized, so turn off built-in highlighting.
(custom-set eshell-highlight-prompt nil)
(custom-set eshell-prompt-function #'my-eshell-prompt-function)
(custom-set eshell-prompt-regexp "^[^#$]* [#$][[:digit:]]* ")

;;; Commands

(defun my-eshell-insert-sudo ()
  "Prefix current command with prefix."
  (interactive)
  (eshell-bol)
  (insert "sudo "))

(defun eshell/gdb ()
  "Debug previous command in `gdb'."
  (pcase-let* ((`(,program . ,args)
                (split-string-and-unquote (eshell-previous-input-string 1)))
               (bin (if (file-executable-p program)
                        (file-truename program)
                      (executable-find program)))
               (cmd (combine-and-quote-strings (cons bin args))))
    (gdb (concat "gdb -i=mi -cd " default-directory " --args " cmd))))

(defun my-eshell-edit-indirect-output ()
  "Edit the last output block output in a separate buffer with
`edit-indirect-region'."
  (interactive)
  (let ((display-buffer-overriding-action '(display-buffer-same-window)))
    (edit-indirect-region (eshell-beginning-of-output)
                          (eshell-end-of-output)
                          t)))

;;; Completion

(defun pcomplete/eshell-mode/pushd ()
  (pcomplete-here
   (completion-table-merge
    (seq-map-indexed (lambda (_ index) (format "+%d" (+ index 1)))
                     eshell-dirstack)
    (pcomplete-dirs))))

;;; eshell/dirs

(defun my-eshell-dirs (&optional if-verbose)
  "Like `eshell/dirs', but prints the list vertically and
numbered."
  (when (or (not if-verbose) eshell-dirtrack-verbose)
    (mapconcat #'identity
               (seq-map-indexed (lambda (directory index)
                                  (concat (number-to-string index)
                                          "\t"
                                          directory))
                                (cons (eshell/pwd) eshell-dirstack))
               "\n")))
(advice-add 'eshell/dirs :override #'my-eshell-dirs)

;;; Keymap

;; `eshell-mode-map' is local to the buffer, so set up key bindings in a hook.

(defun my-eshell-mode-hook ()
  "My hook for Eshell mode."
  (define-key eshell-mode-map (kbd "C-c C-h") #'counsel-esh-history)
  (define-key eshell-mode-map (kbd "C-c C-l") #'eshell/clear)
  (define-key eshell-mode-map (kbd "C-c C-q") #'eshell-life-is-too-much)
  (define-key eshell-mode-map (kbd "C-c C-w")
    #'my-eshell-edit-indirect-output)
  (define-key eshell-mode-map (kbd "C-c s") #'my-eshell-insert-sudo))
(add-hook 'eshell-mode-hook #'my-eshell-mode-hook)
