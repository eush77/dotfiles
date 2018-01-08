(require 'cl-lib)

;;
;; Package installation.
;;
(defun my-install-packages (&rest packages)
  "Confirm and install missing PACKAGES."
  (let ((missing-packages (cl-remove-if #'package-installed-p packages)))
    (when (and missing-packages
               (yes-or-no-p (format "Missing packages: %s. Install? "
                                    missing-packages)))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))

;;
;; This is used for redefining some bound command to something else, staying
;; away from an actual key sequence which is subject to change.
;;
(defun my-global-redefine-key (old-command new-command)
  "Set the key currently bound to OLD-COMMAND to NEW-COMMAND."
  (let ((key (where-is-internal old-command
				(current-global-map)
				t)))
    (global-set-key key new-command)))

;;
;; Interactive commands and custom variables follow.
;;

(defun my-duplicate-line-down ()
  "Duplicate line and move cursor to the second copy."
  (interactive)
  (next-line 1)
  (beginning-of-line)
  (copy-from-above-command)
  (newline)
  (previous-line))

(defun my-duplicate-line-up ()
  "Duplicate line and move cursor to the first copy."
  (interactive)
  (my-duplicate-line-down)
  (previous-line 1))

(defcustom my-open-line-and-indent t
  "Non-nil indicates that `my-open-next-line' and
`my-open-previous-line' indent inserted lines.")

(defun my-open-next-line (count)
  "Insert COUNT empty lines after the line at point.

If `my-open-line-and-indent' is non-nil, indent the newly inserted
line according to the major mode (with
`indent-according-to-mode').

See URL `http://www.emacswiki.org/emacs/OpenNextLine'."
  (interactive "*p")
  (end-of-line)
  (open-line count)
  (next-line 1)

  (when my-open-line-and-indent
    (indent-according-to-mode)))

(defun my-open-previous-line (count)
  "Insert COUNT empty lines before the line at point.

If `my-open-line-and-indent' is non-nil, indent the newly inserted
line according to the major mode (with
`indent-according-to-mode').

See URL `http://www.emacswiki.org/emacs/OpenNextLine'."
  (interactive "*p")
  (beginning-of-line)
  (open-line count)
  (when my-open-line-and-indent
    (indent-according-to-mode)))

(defcustom my-window-size-delta 1
  "Default delta for `my-window-*' family of commands.

See `my-enlarge-window', `my-enlarge-window-horizontally', and
`my-shrink-window-horizontally'.")

(defun my-enlarge-window (delta)
  "Like `enlarge-window', but defaults to `my-window-size-delta'
instead of 1."
  (interactive "P")
  (setq delta (or delta my-window-size-delta))
  (enlarge-window delta))

(defun my-enlarge-window-horizontally (delta)
  "Like `enlarge-window-horizontally', but defaults to
`my-window-size-delta' instead of 1."
  (interactive "P")
  (setq delta (or delta my-window-size-delta))
  (enlarge-window-horizontally delta))

(defun my-shrink-window-horizontally (delta)
  "Like `shrink-window-horizontally', but defaults to
`my-window-size-delta' instead of 1."
  (interactive "P")
  (setq delta (or delta my-window-size-delta))
  (shrink-window-horizontally delta))
