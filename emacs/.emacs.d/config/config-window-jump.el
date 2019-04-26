(custom-set-variables '(wj-jump-frames (not (null window-system))))

(advice-add 'wj-all-windows :override #'my-active-windows)
