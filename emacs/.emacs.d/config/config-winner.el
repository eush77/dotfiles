(winner-mode 1)

;;;###autoload (autoload 'my-winner-hydra/body "config-winner")
(defhydra my-winner-hydra ()
  "Winner"
  ("p" winner-undo "undo")
  ("n" winner-redo "redo")
  ("<return>" nil))
