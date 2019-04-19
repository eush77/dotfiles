(add-to-list 'package-selected-packages 'dash)
(package-install-selected-packages)
(require 'dash)

(custom-set-variables '(wj-jump-frames (not (null window-system))))

(defun my-wj-all-windows--selected-wm-desktop (windows)
  "Limit results to the selected virtual desktop of a window
system."
  ;; Call `my-frame-wm-desktop' once per frame, not once per window.
  (let* ((wm-desktop-alist (--map (cons it (my-frame-wm-desktop it))
                                  (frame-list)))
         (selected-wm-desktop (alist-get (selected-frame)
                                         wm-desktop-alist)))
    (--filter (= (alist-get (window-frame it) wm-desktop-alist)
                 selected-wm-desktop)
              windows)))

(defun my-wj-all-windows--exwm-active (windows)
  "Limit to active Exwm workspaces."
  (--filter (frame-parameter (window-frame it) 'exwm-active) windows))

(advice-add 'wj-all-windows
            :filter-return
            (if (boundp 'exwm-state)
                #'my-wj-all-windows--exwm-active
              #'my-wj-all-windows--selected-wm-desktop))
