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

;; Disable backups and lockfiles
(setq make-backup-files nil)   ;; no file~ backups
(setq create-lockfiles nil)    ;; no .#lock files

;; Additional security settings
(setq auth-source-save-behavior nil)  ; Don't save auth info automatically
(setq password-cache-expiry 300)       ; Password cache expires after 5 minutes
(setq epg-gpg-program "gpg2")         ; Use GPG2 if available
(setq epa-file-encrypt-to nil)         ; Don't encrypt to default key
(setq epa-file-select-keys nil)        ; Ask for keys when encrypting

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
