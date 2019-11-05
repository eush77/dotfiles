(with-eval-after-load "smartparens"
  (sp-local-pair 'rst-mode "`" "`"))

(cl-assert (null fill-paragraph-function))
(setq-local fill-paragraph-function
            (lambda (_)
              (call-interactively 'my-nbsp-fix-paragraph)))
