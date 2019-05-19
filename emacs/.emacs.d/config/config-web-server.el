(define-advice ws-response-header (:filter-args (args) my-content-type)
  "Add charset=utf-8 to the Content-Type header."
  (pcase args
    (`(,process ,code . ,headers)
     `(,process
       ,code
       . ,(seq-map
           (pcase-lambda (`(,header . ,value))
             (cons header
                   (if (and (let ((case-fold-search t))
                              (string-match-p "Content-Type" header))
                            (member value '("text/html" "text/plain")))
                       (concat value "; charset=utf-8")
                     value)))
           headers)))))
