;;; erc

(custom-set erc-prompt-for-password nil)

;;; erc-imenu

(defun my-erc-unfill-notice--read-only ()
  "Like `erc-unfill-notice', but works on read-only lines as
well."
  (let ((str ""))
    (re-search-forward (concat "^" (regexp-quote erc-notice-prefix)))
    (setq str (buffer-substring-no-properties (point) (line-end-position)))
    (forward-line 1)
    (while (looking-at "    ")
      (setq str (concat str " "
                        (buffer-substring-no-properties (+ (point) 4)
                                                        (line-end-position))))
      (forward-line 1))
    str))

(with-eval-after-load "erc-imenu"
  (advice-add 'erc-unfill-notice
              :override #'my-erc-unfill-notice--read-only))

;;; erc-log

(add-to-list 'erc-modules 'log)

(with-eval-after-load "erc-log"
  (custom-set erc-log-channels-directory
              (expand-file-name "erc" user-emacs-directory)))

;;; erc-pcomplete

(defun my-pcomplete/erc-mode/complete-command ()
  "Complete nicks first."
  (pcomplete-here
   (append
    (pcomplete-erc-nicks erc-pcomplete-nick-postfix t)
    (pcomplete-erc-commands))))

(with-eval-after-load "erc-pcomplete"
  (custom-set erc-pcomplete-nick-postfix ": ")
  (custom-set erc-pcomplete-order-nickname-completions t)

  (advice-add 'pcomplete/erc-mode/complete-command
              :override #'my-pcomplete/erc-mode/complete-command))

;;; erc-track

(with-eval-after-load "erc-track"
  (custom-set erc-track-enable-keybindings t)

  (add-to-list 'erc-track-exclude-types "JOIN")
  (add-to-list 'erc-track-exclude-types "PART")
  (add-to-list 'erc-track-exclude-types "QUIT")

  (define-key erc-track-minor-mode-map (kbd "C-c C-@") nil)
  (define-key erc-track-minor-mode-map (kbd "C-c C-SPC") nil)
  (define-key erc-track-minor-mode-map (kbd "C-x M-5")
    #'erc-track-switch-buffer))
