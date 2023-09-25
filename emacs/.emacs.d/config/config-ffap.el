
;;; ffap-c-path

;;;###autoload
(defun my-gcc-include-paths (lang)
  "Get the list of system include paths for language LANG, as
returned by the GCC compiler."
  ;; (split-string
  ;;  (with-temp-buffer
  ;;    (when (zerop (ignore-errors
  ;;                   (call-process "gcc" nil t nil
  ;;                                 "-E" "-Wp,-v" (concat "-x" lang)
  ;;                                 "/dev/null")))
  ;;      (keep-lines "^\s*/" (point-min) (point-max))
  ;;      (buffer-string))))
  )

(custom-set-variables
 '(ffap-c-path (my-gcc-include-paths "c"))
 '(ffap-c++-path (my-gcc-include-paths "c++")))

;;; ffap-machine-at-point

(define-advice ffap-machine-at-point (:before-while () my-dired)
  "Return nil in `dired-mode'.

File names rarely refer to machines but regularly look like they
do."
  (not (derived-mode-p 'dired-mode)))
