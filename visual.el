(defun my-zenburn ()
   "Apply Zenburn with my settings."
   (interactive)
   (add-to-list 'custom-theme-load-path
                (concat emacs-dir "/site-lisp/zenburn"))
   (require 'zenburn-theme)
   (load-theme 'zenburn t)
   (custom-set-faces
    '(cursor ((t (:background "red") )) )
    '(highlight-current-line-face ((t (:background "gray35"))))
    '(org-checkbox ((t (:foreground nil :inherit org-todo))))
    )
)

(defun my-solarized ()
  "Apply Solarized light with my settings."
  (interactive)
  (load-theme 'solarized-light t)
   (custom-set-faces
    '(cursor ((t (:background "red" :foreground "red"))))
    '(highlight-current-line-face ((t (:background "moccasin")))
                                  '(mode-line ((t (:background "Black" :foreground "#a4a097" :inverse-video t :box nil :underline nil :slant normal :weight normal)))) )
    '(mode-line ((t (:background "Black" :foreground "#a4a097" :inverse-video t :box nil :underline nil :slant normal :weight normal))))
    '(mode-line-highlight ((t (:box (:line-width 2 :color "#d1cdc1" :style released-button)))))
    '(mode-line-inactive ((t (:background "Brown" :foreground "#d1cdc1" :inverse-video t :box nil :underline nil :slant normal :weight normal))))
    '(org-checkbox ((t (:background "#d6d0be" :foreground "#805c64" :box (:line-width 1 :style released-button)))))
    '(org-link ((t (:foreground "#21867a" :underline t))))
    )
  )

(defun my-solarized-boot ()
  "Apply Solarized light at startup time."
  (interactive)
  ;; Hmm..?
  (add-to-list 'custom-theme-load-path (concat emacs-dir "/site-lisp/solarized"))
  (my-solarized) )

;; If modern, we'd like our color theme early.
(my-zenburn)

(provide 'visual)
