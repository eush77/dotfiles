(use-package dash :ensure t :defer t)
(use-package hydra :ensure t :defer t)

(use-package ace-window
  :ensure t
  :init (ace-window-display-mode)
  :bind ("C-<tab>" . ace-window)
  :custom
  (aw-char-position 'left)
  (aw-swap-invert t)
  :custom-face
  (aw-leading-char-face ((t (:foreground "dark turquoise" :height 5.0)))))

(use-package avy
  :ensure t
  :defer t)

(use-package calendar
  :defer t
  :custom (calendar-week-start-day 1))

(use-package color-identifiers-mode
  :ensure t
  :init (global-color-identifiers-mode)
  :hook (color-identifiers-mode
         . (lambda ()
             (pcase-dolist (`(,names . ,props) 
                            '(((comment-delimiter doc negation-char type variable-name warning) :inherit default)
                              ((comment function-name string) :inherit default :slant italic)
                              ((builtin constant keyword preprocessor) :inherit default :weight bold)))
               (dolist (name names)
                 (face-remap-add-relative (intern (format "font-lock-%s-face" name)) (list props)))))))

(use-package compile
  :bind (:map compilation-mode-map ("c" . compile))
  :custom (compilation-max-output-line-length nil))

(use-package consult
  :ensure t
  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap isearch-forward] . consult-line))
  :custom (consult-point-placement 'match-end)
  :config
  (consult-customize consult-buffer :keymap
                     (let ((map (make-sparse-keymap)))
                       (define-key map (kbd "M-k")
                                   (lambda ()
                                     "Kill current buffers."
                                     (interactive)
                                     ;; (if-let (candidates (embark-selected-candidates))
                                     ;;     )
                                     ))
                       map))
  (define-advice consult-line (:before (&rest args) find-next-word)
    (when (looking-at-p "[[:space:]]")
      (forward-whitespace 1))
    (setq consult-line@@next-word
          (let ((begin (point))
                (end (line-end-position)))
            (forward-word)
            (when (<= (point) end)
              (replace-regexp-in-string "[[:space:]]" "\\\\\\&"
                                        (buffer-substring begin (point)))))))
  (defun consult-line@@next-word (candidate)
    (-some-> (object-intervals candidate)
      (->> (--filter (when-let (face (plist-get (caddr it) 'face))
                       (if (listp face)
                       (seq-intersection orderless-match-faces face)
                       (seq-contains-p orderless-match-faces face))))
           (--reduce (and acc (= (cadr acc) (car it)) it)))
      (cadr)
      (->> (substring candidate))
      (--> (with-temp-buffer
             (insert it)
             (goto-char (point-min))
             (forward-word)
             (buffer-substring (point-min) (point))))
      (->> (replace-regexp-in-string "[[:space:]]" "\\\\\\&"))))
  (consult-customize consult-line :keymap
                     (let ((map (make-sparse-keymap)))
                       (define-key map (kbd "C-w")
                                   (lambda ()
                                     "Yank next word into the minibuffer."
                                     (interactive)
                                     (cond ((string-empty-p (minibuffer-contents-no-properties))
                                            (when consult-line@@next-word
                                              (insert consult-line@@next-word)))
                                           ((and (memq 'orderless completion-styles)
                                                 (null (cdr (orderless-escapable-split-on-space
                                                             (minibuffer-contents-no-properties)))))
                                            (when-let (next-word
                                                       (consult-line@@next-word
                                                        (replace-regexp-in-string "[^[:print:]]" "" 
                                                                                  (vertico--candidate t))))
                                              (if (= (point) (point-max))
                                                  (insert next-word)
                                                (save-excursion
                                                  (goto-char (point-max))
                                                  (insert next-word))))))))
                       map)))

(use-package consult-ls-git
  :ensure t
  :defer t)

(use-package corfu
  :ensure t
  :init (global-corfu-mode))

(use-package cursor-sensor
  :hook (minibuffer-setup . cursor-intangible-mode)
  :init (setq minibuffer-prompt-properties
              (plist-put minibuffer-prompt-properties 'cursor-intangible t)))

(use-package dabbrev
  :defer t
  :custom (dabbrev-case-fold-search nil))

(use-package delsel
  :init (pending-delete-mode))

(use-package dired
  :custom (dired-dwim-target t)
  :bind (:map dired-mode-map
              ("SPC" . dired-up-directory)
              ("C-M-p")
              ("C-M-n")))

(use-package embark
  :ensure t
  :bind (("C-/" . embark-act))
  :custom (embark-quit-after-action nil))

(use-package embark-consult
  :ensure t
  :defer t)

(use-package expand-region
  :ensure t
  :bind ("M-=" . er/expand-region))

(use-package frame
  :bind ("M-`" . other-frame))

(use-package guide-key
  :ensure t
  :init (guide-key-mode)
  :custom
  (guide-key/guide-key-sequence t)
  (guide-key/highlight-command-regexp "rectangle"))

(use-package help
  :defer t
  :custom (help-window-select 'other))

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand))

(use-package imenu
  :bind ("M-i" . imenu))

(use-package key-chord
  :ensure t
  :init (key-chord-mode)
  :config
  (key-chord-define-global "gj" #'avy-goto-char-timer)
  (key-chord-define-global "x0" #'delete-window)
  (key-chord-define-global "x1" #'delete-other-windows)
  (key-chord-define-global "x2" #'split-window-below)
  (key-chord-define-global "x3" #'split-window-right)
  (key-chord-define-global "xb" #'switch-to-buffer)
  (key-chord-define-global
   "XB" (lambda (mode) "Switch to buffer in the given major mode."
          (interactive (list (if current-prefix-arg
                         (-> (buffer-list)
                             (->> (--map (buffer-local-value 'major-mode it)))
                             (-uniq)
                             (--> (completing-read "Major mode: " it nil t))
                             (intern))
                       major-mode)))
          (cl-letf* ((buffer-names (->> (buffer-list)
                                        (--filter (eq (buffer-local-value 'major-mode it) mode))
                                        (-map #'buffer-name)))
                     (filter-name (lambda (name)
                                    (member (if (consp name) (car name) name) buffer-names)))
                     (function (symbol-function 'completion-table-with-predicate))
                     ((symbol-function 'completion-table-with-predicate)
                      (lambda (table pred1 &rest args)
                        (apply function table (-andfn pred1 filter-name) args))))
            (call-interactively #'switch-to-buffer))))
  (key-chord-define-global "xf" #'find-file)
  (key-chord-define-global "xg" #'consult-ls-git-ls-files)
  (key-chord-define-global "xh" #'consult-ls-git-ls-status)
  (key-chord-define-global "xk" #'kill-buffer)
  (key-chord-define-global "xl" #'find-library)
  (key-chord-define-global "xq" #'quit-window)
  (key-chord-define-global "xr" #'recentf)
  (key-chord-define-global "xs" #'save-buffer)
  (key-chord-define-global "xv" #'find-alternate-file)
  (key-chord-define-global "xw" #'ff-get-other-file))

(use-package lisp
  :bind ("M-(" . insert-parentheses))

(use-package magit
  :ensure t
  :defer t
  :custom (magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  :config
  (setf (alist-get 'commit git-rebase-line-regexps)
        (replace-regexp-in-string "\\\\(\\?1:" "\\\\(?1:drop\\\\|"
                                  (alist-get 'commit git-rebase-line-regexps)))
  (define-advice git-rebase-kill-line (:override () drop)
    "Drop commit on current line."
    (git-rebase-set-action "drop")))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package menu-bar
  :init (menu-bar-mode -1))

(use-package minibuffer
  :defer t
  :custom (completion-styles '(orderless partial-completion basic)))

(use-package misc
  :bind (("C-c M-n" . duplicate-dwim)
         ("C-c M-p" . (lambda () "duplicate-dwim backwards."
                        (interactive)
                        (let ((duplicate-line-final-position 0))
                          (call-interactively duplicate-dwim))))
         ([remap zap-to-char] . zap-up-to-char))
  :custom (duplicate-line-final-position 1))

(use-package multiple-cursors
  :ensure t
  :bind (("C-M-<down>" . mc/mark-next-like-this)
         ("C-M-<up>" . mc/mark-previous-like-this)))

(use-package orderless
  :ensure t
  :defer t)

(use-package org
  :hook (org-mode . org-indent-mode))

(use-package repeat
  :bind ("C-." . repeat))

(use-package replace
  :bind ("C-r" . query-replace-regexp))

(use-package savehist
  :init (savehist-mode))

(use-package scroll-bar
  :init (scroll-bar-mode -1))

(use-package simple
  :bind (("C-o" . (lambda () "Open previous line."
                    (interactive)
                    (beginning-of-line)
                    (open-line 1)))
         ("M-o" . (lambda () "Open next line."
                    (interactive)
                    (forward-line)
                    (open-line 1))))
  :init (column-number-mode)
  :custom
  (indent-tabs-mode nil)
  (set-mark-command-repeat-pop t))

(use-package smartparens
  :ensure t
  :bind (:map smartparens-mode-map
              ("C-M-b" . sp-backward-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-f" . sp-next-sexp)
              ("C-M-r" . sp-backward-down-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-y" . sp-up-sexp)
              ("C-c C-M-b" . sp-backward-slurp-sexp)
              ("C-c C-M-f" . sp-forward-slurp-sexp)
              ("C-c C-M-j" . sp-join-sexp)
              ("C-c C-M-r" . sp-raise-sexp)
              ("C-c C-M-s" . sp-split-sexp)
              ("C-c C-M-t" . sp-splice-sexp)
              ("C-c C-SPC" . sp-mark-sexp)
              ("C-c C-a" . sp-beginning-of-sexp)
              ("C-c C-b" . sp-previous-sexp)
              ("C-c C-e" . sp-end-of-sexp)
              ("C-c C-f" . sp-forward-sexp)
              ("C-c C-u C-M-b" . sp-backward-barf-sexp)
              ("C-c C-u C-M-f" . sp-forward-barf-sexp)
              ("C-k" .
               (lambda (arg) "Kill line or hybrid sexp."
                 (interactive "P")
                 (if (and arg (/= (prefix-numeric-value arg) 16)) (kill-line arg) (sp-kill-hybrid-sexp arg))))
              ("C-x np" . sp-narrow-to-sexp))
  :init (smartparens-global-strict-mode))

(use-package smartscan :ensure t :defer t)
(use-package smartscan; cannot hook into prog-mode with :ensure because prog-mode is loaded during package install
  :hook prog-mode)

(use-package sort
  :bind ("M-s M-s" . sort-lines)
  :custom (sort-fold-case t))

(use-package tool-bar
  :init (tool-bar-mode -1))

(use-package vertico
  :ensure t
  :bind (:map vertico-grid-map
              ("C-M-b" . vertico-grid-left)
              ("C-M-f" . vertico-grid-right))
  :init
  (vertico-mode)
  (vertico-grid-mode)
  (vertico-indexed-mode)
  (vertico-multiform-mode)
  :config
  (dotimes (n 10)
    (define-key vertico-map (kbd (format "C-%s" n))
                `(lambda ()
                   (interactive)
                   (setq vertico--index (+ vertico--index ,n))
                   (vertico-exit)))))

(use-package windmove
  :bind (("C-M-j" . windmove-left)
         ("C-M-k" . windmove-right)
         ("C-M-p" . windmove-up)
         ("C-M-n" . windmove-down)))

(use-package winner
  :init
  (winner-mode)
  (defhydra winner ()
    "Winner"
    ("p" winner-undo "undo")
    ("n" winner-redo "redo")
    ("RET" nil))
  :bind ("C-c w" . winner/body))

(get 'winner-undo 'repeat-map)

(use-package xref
  :defer t
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-scope 'frame nil nil "Customized with use-package ace-window")
 '(custom-enabled-themes '(solarized-dark))
 '(custom-safe-themes
   '("8093e0f40c724d32955ae65b7122ff74ce6aa9a86e408712e8dbfb0e325a3ad7" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" default))
 '(enable-recursive-minibuffers t)
 '(enable-remote-dir-locals t)
 '(magit-repository-directories '(("/sshx:Southeast:/home/eush/src/arkcompiler" . 1)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(embark-consult embark orderless consult-ls-git hydra ace-window switch-window multiple-cursors consult color-identifiers-mode docker-tramp corfu marginalia avy guide-key solarized-theme smartscan smartparens expand-region magit key-chord vertico))
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values '((eval c-set-offset 'innamespace 0)))
 '(split-width-threshold 120))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 161 :foundry "outline" :family "Consolas")))))
