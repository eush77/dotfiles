(with-eval-after-load "calendar"
  (custom-set calendar-week-start-day 1)
  (custom-set calendar-intermonth-text
              '(propertize
                (format "%2d"
                        (car
                         (calendar-iso-from-absolute
                          (calendar-absolute-from-gregorian
                           (list month day year)))))
                'font-lock-face 'font-lock-function-name-face)))
