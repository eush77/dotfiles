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

(defun my-ws-send-directory-list (process directory)
  "Send a listing of files in DIRECTORY to PROCESS.

Like `ws-send-directory-list', but render a Dired buffer
instead."
  (with-current-buffer
      (save-window-excursion
        (dired directory)
        (dired-revert)
        (htmlize-buffer))
    (ws-response-header process 200 '("Content-Type" . "text/html"))
    (process-send-region process (point-min) (point-max))
    (kill-buffer)))
