;;; Commands

(defun my-edit-indirect-save-parent ()
  "Call `edit-indirect-save', and then save the parent buffer."
  (interactive)
  (edit-indirect-save)
  (with-current-buffer (overlay-buffer edit-indirect--overlay)
    (save-buffer)))

;;; Keymap

(define-key edit-indirect-mode-map (kbd "C-c C-s") #'edit-indirect-save)
(define-key edit-indirect-mode-map (kbd "C-x C-s") #'my-edit-indirect-save-parent)
(key-chord-define edit-indirect-mode-map "xs" #'my-edit-indirect-save-parent)
