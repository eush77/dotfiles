(add-to-list 'package-selected-packages 'dash)
(add-to-list 'package-selected-packages 'dash-functional)
(package-install-selected-packages)
(require 'dash)
(require 'dash-functional)              ; `-compose'

;;
;; Frame switching.
;;

;;;###autoload
(defun my-select-frame ()
  "Interactively select a frame from names of buffers that are
open in it, with frames ordered from the most recently used to
the least recently used and the default selection pointing at the
most recently used other frame.

If there are only 2 frames open, switch to the other frame
immediately."
  (interactive)
  (let* ((frame-list                     ; List of frames in MRU order
          (-distinct (--map (window-frame it)
                            (--mapcat (get-buffer-window-list it nil 'visible)
                                      (buffer-list)))))
         (frame-alist                    ; List of frames with labels
          (--map (cons (mapconcat (-compose #'buffer-name #'window-buffer)
                                  (window-list it)
                                  ", ")
                       it)
                 frame-list)))
    (if (<= (length frame-alist) 2)
        (other-frame 1)
      (select-frame (cdr (assoc (completing-read "Select frame: "
                                                 frame-alist
                                                 nil
                                                 t
                                                 nil
                                                 nil
                                                 ;; Select the other frame
                                                 (caadr frame-alist))
                                frame-alist))))))

;;
;; Editing.
;;

(defun my-forward-duplicate-line ()
  "Duplicate current line forward, moving the cursor to the
second copy."
  (interactive)
  (forward-line 1)
  (beginning-of-line)
  (copy-from-above-command)
  (newline)
  (forward-line -1))

(defun my-backward-duplicate-line ()
  "Duplicate current line backward, moving the cursor to the
first copy."
  (interactive)
  (my-forward-duplicate-line)
  (forward-line -1))

;;;###autoload
(defun my-forward-duplicate-sexp-or-line ()
  "Duplicate current sexp forward, unless the point is at the
beginning of line, in which case duplicate the line forward."
  (interactive)
  (if (zerop (current-column))
      (my-forward-duplicate-line)
    (my-sp-forward-duplicate-sexp)))

;;;###autoload
(defun my-backward-duplicate-sexp-or-line ()
  "Duplicate current sexp backward, unless the point is at the
beginning of line, in which case duplicate the line backward."
  (interactive)
  (if (zerop (current-column))
      (my-backward-duplicate-line)
    (my-sp-backward-duplicate-sexp)))

(defcustom my-open-line-and-indent t
  "Non-nil indicates that `my-open-next-line' and
`my-open-previous-line' indent inserted lines.")

;;;###autoload
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

;;;###autoload
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

;;
;; Window sizing.
;;

(defcustom my-window-size-delta 1
  "Default delta for `my-window-*' family of commands.

See `my-enlarge-window', `my-enlarge-window-horizontally', and
`my-shrink-window-horizontally'.")

;;;###autoload
(defun my-enlarge-window (delta)
  "Like `enlarge-window', but defaults to `my-window-size-delta'
instead of 1."
  (interactive "P")
  (setq delta (or delta my-window-size-delta))
  (enlarge-window delta))

;;;###autoload
(defun my-enlarge-window-horizontally (delta)
  "Like `enlarge-window-horizontally', but defaults to
`my-window-size-delta' instead of 1."
  (interactive "P")
  (setq delta (or delta my-window-size-delta))
  (enlarge-window-horizontally delta))

;;;###autoload
(defun my-shrink-window-horizontally (delta)
  "Like `shrink-window-horizontally', but defaults to
`my-window-size-delta' instead of 1."
  (interactive "P")
  (setq delta (or delta my-window-size-delta))
  (shrink-window-horizontally delta))

;;
;; Window splitting.
;;

;;;###autoload
(defun my-balanced-split-window-vertically ()
  "Split window vertically and maintain balance."
  (interactive)
  (let ((window-min-height window-safe-min-height))
    (split-window-vertically))
  (balance-windows))

;;;###autoload
(defun my-balanced-split-window-horizontally ()
  "Split window horizontally and maintain balance."
  (interactive)
  (let ((window-min-width window-safe-min-width))
    (split-window-horizontally))
  (balance-windows))

;;;###autoload
(defun my-balanced-delete-window ()
  "Delete current window and maintain balance."
  (interactive)
  (delete-window)
  (balance-windows))
