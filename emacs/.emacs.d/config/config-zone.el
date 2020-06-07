(zone-select-add-program 'zone-pgm-rainbow)
(zone-select-add-program 'zone-pgm-sl)

(zone-when-idle 600)

(define-advice zone (:before-until () my-exwm)
  "Don't zone out of buffers in certain modes."
  (derived-mode-p 'exwm-mode 'pdf-view-mode))
