(pdf-tools-install)

;;; follow-mode

(require 'follow)

(defun follow-adjust-window@my-pdf-view (window)
  "Adjust `pdf-view-mode' windows in `follow-mode'."
  (when (with-current-buffer (window-buffer window)
          (derived-mode-p 'pdf-view-mode))
    (pcase-let* ((page (pdf-view-current-page window))
                 (hscroll (window-hscroll window))
                 (vscroll
                  (window-vscroll window
                                  pdf-view-have-image-mode-pixel-vscroll))
                 (all-followers (follow-all-followers window))
                 (followers (remove window all-followers))
                 (`(,predecessors . ,successors)
                  (follow-split-followers all-followers window)))
      (--each-indexed predecessors
        (pdf-view-goto-page (max 1 (- page 1 it-index)) it))
      (--each-indexed successors
        (pdf-view-goto-page (min (+ page 1 it-index)
                                 (pdf-cache-number-of-pages)) it))
      (--each followers
        (set-window-hscroll it hscroll)
        (set-window-vscroll it vscroll
                            pdf-view-have-image-mode-pixel-vscroll)))
    ;; Disable the default adjustment.
    t))

(defun pdf-view-next-line-or-next-page@my-follow (func &rest args)
  "Adjust page-flipping for the `follow-mode'.

When flipping the page during execution of `func', first find and
select a window displaying the page, and if no such window
exists, skip the number of pages equal to the number of windows
and select either the topmost leftmost window (if flipping
forward) or the rightmost bottom window (if flipping backward).

This advice applies to all scrolling functions that can flip a
page when at the top or bottom of a page, and enables smooth
experience of continuous forward/backward scrolling in
`follow-mode'."
  (if (not follow-mode)
      (apply func args)
    (cl-letf*
        ((pdf-view-next-page-function
          (symbol-function 'pdf-view-next-page))
         ((symbol-function 'pdf-view-next-page)
          (lambda (&optional n)
            (pcase-let*
                ((forwardp (not (equal n -1)))
                 (all-followers (follow-all-followers))
                 (`(,predecessors . ,successors)
                  (follow-split-followers all-followers)))
              (if-let ((next-window
                        (car (if forwardp successors predecessors))))
                  (select-window next-window)
                (select-window (car (if forwardp
                                        all-followers
                                      (last all-followers))))
                (funcall pdf-view-next-page-function
                         (if forwardp
                             (length all-followers)
                           (- (length all-followers)))))))))
      (apply func args))))

(defun pdf-view-next-page-command@my-follow (func &optional n)
  "Scroll all followers in `follow-mode'.

If selected window is the topmost and leftmost window of the
group, scale the scroll amount by the number of followers."
  (funcall func
           (if (and follow-mode
                    (eq (car (follow-all-followers)) (selected-window)))
               (* (or n 1) (length (follow-all-followers)))
             n)))

(with-eval-after-load "pdf-view"
  (advice-add 'follow-adjust-window
              :before-until #'follow-adjust-window@my-pdf-view)
  (advice-add 'pdf-view-next-line-or-next-page
              :around #'pdf-view-next-line-or-next-page@my-follow)
  (advice-add 'pdf-view-next-page-command
              :around #'pdf-view-next-page-command@my-follow)
  (advice-add 'pdf-view-previous-line-or-previous-page
              :around #'pdf-view-next-line-or-next-page@my-follow)
  (advice-add 'pdf-view-scroll-down-or-previous-page
              :around #'pdf-view-next-line-or-next-page@my-follow)
  (advice-add 'pdf-view-scroll-up-or-next-page
              :around #'pdf-view-next-line-or-next-page@my-follow))

;;; midnight-minor-mode

(define-advice pdf-view-midnight-minor-mode
    (:before (&optional arg) my-recompute-colors)
  "Recompute `pdf-view-midnight-colors'."
  (setq pdf-view-midnight-colors
        (cons (face-attribute 'default :foreground)
              (face-attribute 'default :background))))

;;; keymap

(with-eval-after-load "pdf-tools"
  (add-to-list 'pdf-tools-enabled-modes
               'pdf-view-midnight-minor-mode)

  ;; `pdf-isearch-minor-mode' integrates `pdf-tools' with `isearch'.
  (define-key pdf-view-mode-map (kbd "C-r") #'isearch-backward)
  (define-key pdf-view-mode-map (kbd "C-s") #'isearch-forward)

  (define-key pdf-view-mode-map (kbd "j") #'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") #'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "h") #'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "l") #'image-forward-hscroll))

(with-eval-after-load "pdf-outline"
  (define-key pdf-outline-buffer-mode-map (kbd "O")
    #'pdf-outline-follow-link)
  (define-key pdf-outline-buffer-mode-map (kbd "RET")
    #'pdf-outline-follow-link-and-quit))
