(message "Medea specific..")

;; Fullscreen editing
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)


(setq browse-url-browser-function 'browse-url-generic
              browse-url-generic-program "chromium-browser")

(server-start)

(require 'edit-server)
(edit-server-start)

(custom-set-faces
 '(default ((t (:height 90
                :family "Monospace-8"))))
 '(bold ((t (:weight ultra-bold))))
 '(bold-italic ((t (:slant italic :weight ultra-bold))))
 '(cursor ((t (:background "#FF0000"))))
 '(highlight-current-line-face ((t (:background "gray90"))))
 '(linum ((t (:inherit (shadow default) :height 0.9))))
 '(yas/field-highlight-face ((t (:background "gray35" :underline t))))
 '(region ((t (:background "lightblue")))) )

(set-face-background 'show-paren-match-face "gray85")
(set-face-attribute 'show-paren-match-face nil
       :weight 'bold :underline nil :overline nil :slant 'normal)

;; -------------------------- zenburn ------------------------------------------

(defun my-zenburn ()
  (interactive)
  (require 'zenburn)
  (color-theme-zenburn)
  (custom-set-faces
   '(highlight-current-line-face ((t (:background "gray35"))))
   '(rst-level-face-base-light 38)
   '(cursor ((t (:background "#FF0000")))) )  )

;; -----------------------------------------------------------------------------

(my-zenburn)

(defun my-large-font ()
  (interactive)
  (set-face-attribute 'default nil :font "Monospace-8")
)

(defun bigscreen ()
  (interactive)
  (split-window-horizontally)
  (my-zenburn)
  (fullscreen)
  (fullscreen)
  (my-large-font)
  (balance-windows)
)

(global-set-key (kbd "C-c <up>") 'bigscreen)

(load "gnuserv")
(gnuserv-start)

(my-zenburn)
(fullscreen)
(split-window-horizontally)

(provide 'medea)
