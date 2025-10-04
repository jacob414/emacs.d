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

;; Show line numbers globally
(global-display-line-numbers-mode t)

;; Keep the line number gutter compact; allow growth only when needed
(setq-default display-line-numbers-width 3)
(when (boundp 'display-line-numbers-width-start)
  (setq display-line-numbers-width-start 3))
;; Let Emacs grow/shrink the gutter as needed; set to t to avoid shrink
(setq display-line-numbers-grow-only nil)

;; Line-number face sizing (same size for current and non-current lines)
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
      (set-face-attribute 'line-number-current-line nil :height h))))

;; Highlight the current line using built-in `hl-line`
(defvar my/hl-line-bg "gray35"
  "Background color used for `hl-line`.")

(defun my/apply-hl-line-face ()
  (when (facep 'hl-line)
    (set-face-attribute 'hl-line nil :background my/hl-line-bg :inherit nil)))

(global-hl-line-mode 1)
(add-hook 'after-init-hook #'my/apply-hl-line-face)
(when (fboundp 'advice-add)
  (advice-add 'load-theme :after (lambda (&rest _) (my/apply-hl-line-face))))

;; Reduce horizontal padding inside the line-number gutter by using
;; fractional-width spaces on both sides of the digits. Target ~4px
;; on each side (tweakable via `my/line-number-side-padding-px').
(defvar my/line-number-side-padding-px 2
  "Approximate pixel padding to apply on each side of line numbers.")

(defun my/line-number--pad-columns ()
  "Compute padding in columns to approximate `my/line-number-side-padding-px'."
  (let* ((cw (float (frame-char-width)))
         (px (float my/line-number-side-padding-px)))
    (max 0.0 (/ px cw))))

(defun my/display-line-number-format (line &rest _)
  "Custom display-line-number formatter with reduced side padding.
LINE is the absolute line number; ignore other args if Emacs passes them."
  (let* ((pad (my/line-number--pad-columns))
         (ls (propertize " " 'display `(space :width ,pad)))
         (rs (propertize " " 'display `(space :width ,pad))))
    (concat ls (format "%d" line) rs)))
(setq display-line-numbers-format #'my/display-line-number-format)

(provide 'visual)
