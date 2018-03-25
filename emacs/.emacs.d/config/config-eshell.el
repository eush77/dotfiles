;;; -*- lexical-binding: t -*-

(with-eval-after-load "eshell"
  (require 'eshell-z)

  (defvar my-eshell-prompt-length 20
    "Length of the Eshell prompt string (as computed by
`my-eshell-prompt-function'). The prompt string will be reduced
until it fits and then aligned to be exactly this length.")

  (defun my-eshell-prompt-sigil-function ()
    "Return trailing sigil for my Eshell prompt."
    (if (zerop (user-uid)) " # " " $ "))

  ;; My Eshell prompt has a fixed length.
  ;;
  ;; It is composed of "leads" - nondirectory parts of the full name of the
  ;; current working directory. The last lead is always displayed in full or
  ;; truncated to fill all the available space, even if to the detriment of
  ;; other leads.

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
      (assert (>= updated-min-length 0))
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

  (defun my-eshell-prompt-function ()
    "Return my Eshell prompt string for `default-directory'."
    (let* ((directory (abbreviate-file-name
                       (directory-file-name default-directory)))
           (last-lead (file-name-nondirectory directory))
           (leads (butlast (split-string directory "/")))
           (sigil (my-eshell-prompt-sigil-function)))
      (cond ((< my-eshell-prompt-length (length sigil))
             (error "`my-eshell-prompt-length' is too small."))
            ((<= my-eshell-prompt-length (+ (length last-lead)
                                            (length sigil)))
             (concat (my-eshell-prompt/format-lead last-lead
                                                   (- my-eshell-prompt-length
                                                      (length sigil)))
                     sigil))
            (t (concat (my-eshell-prompt/format-leads
                        leads
                        (- my-eshell-prompt-length
                           (length last-lead)
                           (length sigil)))
                       last-lead
                       sigil)))))

  (custom-set eshell-prompt-function #'my-eshell-prompt-function))
