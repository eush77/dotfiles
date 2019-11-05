(add-to-list 'package-selected-packages 'javap-mode)
(package-install-selected-packages)

(require 'javap-mode)

(defvar my-javap-temporary-directory
  (make-temp-file "javap-classes-" t)
  "Directory for disassembly of Java class files.")

(with-eval-after-load "arc-mode"
  (define-advice archive-extract (:after (&rest _) my-javap-mode)
    "Enable javap-mode for class files."
    (let ((file-name
           (expand-file-name (file-name-nondirectory buffer-file-name)
                             my-javap-temporary-directory)))
      (when (string= (file-name-extension file-name) "class")
        (set-buffer-file-coding-system 'binary)
        (write-file file-name)
        (javap-buffer)))))
