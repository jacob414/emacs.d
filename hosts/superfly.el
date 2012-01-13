;; Zenburn (http://slinky.imukuppi.org/zenburnpage/)
(require 'zenburn)
(zenburn)

; Final typeface adjustment
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 110 :family "Mensch"))))
 '(highlight-current-line-face ((t (:background "gray35"))))
 '(linum ((t (:inherit (shadow default) :height 0.8)))))

(server-start)

(provide 'superfly)
