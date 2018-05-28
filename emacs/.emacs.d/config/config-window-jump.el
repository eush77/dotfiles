(add-to-list 'package-selected-packages 'dash)
(package-install-selected-packages)

(custom-set wj-jump-frames (not (null window-system)))

(defun my-wj-all-windows--selected-wm-desktop (windows)
  "Limit results to the selected virtual desktop of a window
system."
  (require 'dash)
  ;; Call `my-frame-wm-desktop' once per frame, not once per window.
  (let* ((wm-desktop-alist (--map (cons it (my-frame-wm-desktop it))
                                  (frame-list)))
         (selected-wm-desktop (alist-get (selected-frame)
                                         wm-desktop-alist)))
    (--filter (= (alist-get (window-frame it) wm-desktop-alist)
                 selected-wm-desktop)
              windows)))
(advice-add 'wj-all-windows
            :filter-return #'my-wj-all-windows--selected-wm-desktop)
