(winner-mode 1)

;;;###autoload (autoload 'my-winner-hydra/body "config-winner")
(eval `(defhydra my-winner-hydra ()
         "Winner"
         ("p" winner-undo "undo")
         ("n" winner-redo "redo")
         (,(if window-system "<return>" "RET") nil)))
