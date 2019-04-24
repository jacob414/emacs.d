(add-to-list 'custom-theme-load-path
             (concat emacs-dir "/site-lisp/zenburn"))
(require 'zenburn-theme)
(load-theme 'zenburn t)
(custom-set-faces
 '(cursor ((t (:background "red" :foreground "red"))))
 '(highlight-current-line-face ((t (:background "gray35"))) ) )

(provide 'my-zenburn)
