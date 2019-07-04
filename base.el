(defun my-zenburn ()
   "Apply Zenburn with my settings."
   (interactive)
   (add-to-list 'custom-theme-load-path
                (concat emacs-dir "/site-lisp/zenburn"))
   (require 'zenburn-theme)
   (load-theme 'zenburn t)
   (custom-set-faces
    '(cursor ((t (:background "red" :foreground "red"))))
    '(highlight-current-line-face ((t (:background "gray35"))) ) )
  )

(defun my-solarized ()
  "Apply Solarized light with my settings."
  (interactive)
  (load-theme 'solarized-light t)
   (custom-set-faces
    '(cursor ((t (:background "red" :foreground "red"))))
    '(highlight-current-line-face ((t (:background "#F0DFAF")))
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
(when modern  (my-zenburn))

;; Basic settings
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(when (fboundp 'set-fringe-mode) (set-fringe-mode 0))

(column-number-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq font-lock-maximum-decoration t)
(setq ring-bell-function (lambda nil nil))
(setq display-time-24hr-format t)
(display-time)
(setq tramp-default-method "ssh")

;; Disable auto-save
(setq auto-save-default nil)
(setq auto-save-interval 0)
(setq auto-save-timeout 0)

;; Rectangle selections (via CUA)
(when (fboundp 'cua-mode)
  (setq cua-enable-cua-keys nil)
  (cua-mode t) )

;; Parenthesis balance
(show-paren-mode t)
(setq show-paren-style 'expression)
;;(set-face-foreground 'show-paren-mismatch-face "red")

;; y/n keypresses instead of spelled out 'yes'/'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; yasnippet
(defun my-yasnippet ()
  "yasnippet"
  (setq yas-snippet-dirs
        '("~/src/mine/emacs.d/snippet"  ;; personal snippets
          ))
  (require 'yasnippet)
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)

  (require 'warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  )

(provide 'base)
