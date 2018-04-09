(with-eval-after-load "ffap"
  (defun my-gcc-include-paths (lang)
    "Get the list of system include paths for language LANG, as
returned by the GCC compiler."
    (split-string
     (with-temp-buffer
       (when (zerop (ignore-errors
                      (call-process "gcc" nil t nil
                                    "-E" "-Wp,-v" (concat "-x" lang)
                                    "/dev/null")))
         (keep-lines "^\s*/" (point-min) (point-max))
         (buffer-string)))))

  (custom-set ffap-c-path (my-gcc-include-paths "c"))
  (custom-set ffap-c++-path (my-gcc-include-paths "c++")))
