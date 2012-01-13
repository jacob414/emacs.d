(message "Generic setup..")

;; Zenburn (http://slinky.imukuppi.org/zenburnpage/)
(require 'zenburn)
(zenburn)

;; Font tweaks
(custom-set-faces
 '(highlight-current-line-face ((t (:background "gray35"))))
 '(linum ((t (:inherit (default shadow) :height 0.8))))
)

(provide 'generic)
