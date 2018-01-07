(with-eval-after-load "shr"
  (custom-set shr-width fill-column)

  ;; Eliminate grey background issue when using a dark theme.
  (custom-set shr-color-visible-luminance-min 80))
