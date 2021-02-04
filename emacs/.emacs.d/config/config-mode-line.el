;;; -*- lexical-binding: t -*-
(require 'org)

(sml/setup)

;;; Custom Setup

(custom-set-variables
 '(mode-line-format '("%e"
                      mode-line-front-space
                      mode-line-mule-info
                      mode-line-client
                      mode-line-modified
                      mode-line-remote
                      mode-line-frame-identification
                      mode-line-buffer-identification
                      "   "
                      mode-line-position
                      (vc-mode vc-mode)
                      "  "
                      mode-line-modes
                      mode-line-end-spaces))

 '(rm-blacklist '(" $"         ; rich-minority-mode
                  " counsel"   ; counsel-mode
                  " FlyC-"     ; flycheck-mode (no-checker)
                  " Golden"    ; golden-ratio-mode
                  " Guide"     ; guide-key-mode
                  " ivy"       ; ivy-mode
                  "[ln]"))     ; w3m-lnum-mode

 '(sml/mode-width 'right)
 '(sml/name-width 20)
 '(sml/position-percentage-format "")
 '(sml/prefix-face-list '(("" sml/prefix)))
 '(sml/replacer-regexp-list
   `(("^~/\\.emacs\\.d/elpa/" ":elpa:")
     (,(concat "^" my-org-notes-directory "/") ":notes:")
     (,(concat "^" org-directory "/") ":org:")
     ("^~/src/" ":src:")
     ("^:src:\\([^/]\\)[^/]*/" ":src/\\1:")
     ("^:src/\\(.\\):\\([^/]+\\)/" ":\\1/\\2:")))
 '(sml/size-indication-format "%p of %I ")
 '(sml/theme 'respectful))

;;; Dedicated Window Identification

(defvar-local my-sml-dedicated-window-identification-face-remap-cookie nil)

(define-advice sml/generate-buffer-identification
    (:after (&rest _) my-dedicated-window)
  "Update face remapping in dedicated windows."
  (when my-sml-dedicated-window-identification-face-remap-cookie
    (face-remap-remove-relative
     my-sml-dedicated-window-identification-face-remap-cookie)
    (setq my-sml-dedicated-window-identification-face-remap-cookie nil))
  (when (window-dedicated-p)
    (setq my-sml-dedicated-window-identification-face-remap-cookie
          (face-remap-add-relative 'sml/filename :overline t :underline t))))

(define-advice set-window-dedicated-p
    (:after (window _) sml/generate-buffer-identification)
  "Regenerate buffer identification for SML mode line.

See `sml/generate-buffer-identification'."
  (when (listp mode-line-buffer-identification) ; Fix for transient
    (if window
        (with-selected-window window
          (sml/generate-buffer-identification))
      (sml/generate-buffer-identification))))

;;; mode-line-buffer-identification-keymap

(define-advice sml/generate-buffer-identification
    (:after (&rest _) my-help-echo)
  "Update `help-echo' property."
  (put-text-property
   0 (length sml/buffer-identification)
   'help-echo
   (format "%s

mouse-1: Switch buffer / quit minibuffer
mouse-2: Toggle one window
mouse-3: %s
mouse-6/mouse-7: Switch workspace
mouse-8/mouse-9: Restore window configuration"
           (or (buffer-file-name) (buffer-name))
           (if (and (derived-mode-p 'exwm-mode)
                    exwm--floating-frame)
               "Start moveresize"
             "Toggle window dedicated"))
   sml/buffer-identification))

(defun my-mode-line-switch-buffer-or-quit ()
  "Switch current buffer or quit if minibuffer is active."
  (interactive "@")
  (if (active-minibuffer-window)
      (ivy-quit-and-run)
    (call-interactively 'counsel-ibuffer)))

(defun my-mode-line-toggle-one-window ()
  "Delete other windows, or undo if no other windows."
  (interactive "@")
  (if (one-window-p t)
      (while (one-window-p t)
        (winner-undo))
    (delete-other-windows)))

(defun my-mode-line-toggle-dedicated-or-moveresize ()
  "Toggle dedicated state of selected window or start moveresize.

Start moveresize operation if on a floating frame, otherwise
toggle dedicated."
  (interactive "@")
  (if (and (derived-mode-p 'exwm-mode)
           exwm--floating-frame)
      (exwm-floating--start-moveresize exwm--id)
    (my-toggle-window-dedicated-p)))

(define-key mode-line-buffer-identification-keymap
  [mode-line mouse-1] #'my-mode-line-switch-buffer-or-quit)
(define-key mode-line-buffer-identification-keymap
  [mode-line mouse-2] #'my-mode-line-toggle-one-window)
(define-key mode-line-buffer-identification-keymap
  [mode-line mouse-3] #'my-mode-line-toggle-dedicated-or-moveresize)
(define-key mode-line-buffer-identification-keymap
  [mode-line mouse-6] #'my-exwm-workspace-previous)
(define-key mode-line-buffer-identification-keymap
  [mode-line mouse-7] #'my-exwm-workspace-next)
(define-key mode-line-buffer-identification-keymap
  [mode-line mouse-8] #'winner-undo)
(define-key mode-line-buffer-identification-keymap
  [mode-line mouse-9] #'winner-redo)
