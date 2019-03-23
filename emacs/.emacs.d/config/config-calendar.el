(custom-set-variables
 '(calendar-holidays
   (append holiday-local-holidays
           holiday-other-holidays
           holiday-solar-holidays))
 '(calendar-intermonth-text
   '(propertize (format "%2d"
                        (car (calendar-iso-from-absolute
                              (calendar-absolute-from-gregorian
                               (list month day year)))))
                'font-lock-face 'font-lock-function-name-face))
 '(calendar-week-start-day 1)
 '(holiday-local-holidays
   (progn (require 'russian-holidays) russian-holidays)))

(add-hook 'calendar-today-visible-hook #'calendar-mark-today)

(defun my-calendar--display-at-bottom (func &rest args)
  "Display at the bottom of the frame."
  (let ((display-buffer-overriding-action '(display-buffer-at-bottom)))
    (apply func args)))
(advice-add 'calendar :around #'my-calendar--display-at-bottom)
