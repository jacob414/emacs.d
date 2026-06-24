;;; visual-tests.el --- Tests for visual configuration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'visual)

(ert-deftest my-solarized-aligns-current-line-with-modeline-accent ()
  "Solarized uses its light modeline accent for the current line."
  (let ((my/hl-line-bg "gray35")
        applied-background)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'load-theme) #'ignore)
              ((symbol-function 'custom-set-faces) #'ignore)
              ((symbol-function 'my/apply-hl-line-face)
               (lambda ()
                 (setq applied-background my/hl-line-bg))))
      (my-solarized)
      (should (equal my/hl-line-bg "#d1cdc1"))
      (should (equal applied-background "#d1cdc1")))))

(provide 'visual-tests)

;;; visual-tests.el ends here
