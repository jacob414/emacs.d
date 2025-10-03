(defun my-zenburn ()
  "Apply Zenburn with my settings. Uses MELPA zenburn-theme if available."
  (interactive)
  (when (require 'zenburn-theme nil 'noerror)
    (load-theme 'zenburn t)
    (custom-set-faces
     '(cursor ((t (:background "red"))))
     '(highlight-current-line-face ((t (:background "gray35"))))
     '(org-checkbox ((t (:foreground nil :inherit org-todo)))))))

(defun my-solarized ()
  "Apply Solarized light with my settings using MELPA solarized-theme."
  (interactive)
  (when (require 'solarized-theme nil 'noerror)
    (load-theme 'solarized-light t)
    (custom-set-faces
     '(cursor ((t (:background "red" :foreground "red"))))
     '(highlight-current-line-face ((t (:background "moccasin"))))
     '(mode-line ((t (:background "Black" :foreground "#a4a097" :inverse-video t :box nil :underline nil :slant normal :weight normal))))
     '(mode-line-highlight ((t (:box (:line-width 2 :color "#d1cdc1" :style released-button)))))
     '(mode-line-inactive ((t (:background "Brown" :foreground "#d1cdc1" :inverse-video t :box nil :underline nil :slant normal :weight normal))))
     '(org-checkbox ((t (:background "#d6d0be" :foreground "#805c64" :box (:line-width 1 :style released-button)))))
     '(org-link ((t (:foreground "#21867a" :underline t)))))))

;; my-solarized-boot no longer needed; themes load from MELPA now

;; Theme is now applied from init during startup after packages are available.

;; Shrink the font size used in the line-number gutter
(defvar my/line-number-scale 0.80
  "Scale factor applied to both line-number faces relative to `default'.")

(defun my/set-line-number-faces ()
  (let* ((base (face-attribute 'default :height nil 'default))
         (scale (if (numberp my/line-number-scale) my/line-number-scale 1.0))
         (h (if (integerp base)
                (max 1 (truncate (* base scale)))
              100)))
    (when (facep 'line-number)
      (set-face-attribute 'line-number nil :height h))
    (when (facep 'line-number-current-line)
      ;; Ensure current line uses the exact same height
      (set-face-attribute 'line-number-current-line nil :height h))))

(column-number-mode t)
(global-display-line-numbers-mode t)

(provide 'visual)
